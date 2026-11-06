# =============================================================================
# 00_datos.R  —  Carga y preparación de los datos (Censo Penitenciario 2022, INEC)
# Fuente: microdatos depurados por persona -> base resumida por centro (CPL).
# Entrada : datos/salida_cp2022/cp2022_base_limpia.rds  (genera el script de depuración)
# Objetos : micro, micro29, base, d, etiquetas, vars, esperado, n_centros
# Uso     : source(here::here("R", "00_datos.R"))
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(here)
})

ruta_base <- here("datos", "salida_cp2022", "cp2022_base_limpia.rds")
if (!file.exists(ruta_base)) {
  stop("No existe ", ruta_base, ". Ejecute primero el script de depuración.", call. = FALSE)
}
micro <- readRDS(ruta_base)

# --- Verificación de columnas requeridas ------------------------------------
col_motivos <- setdiff(grep("^F1_S6P02__", names(micro), value = TRUE), "F1_S6P02__99")
col_discap  <- paste0("F1_S2_P070", 1:6)
col_sexual  <- c("F1_S4P16__11", "F1_S4P16__12")
requeridas  <- c("cpl_cod", "cpl_nombre", "ind_inseguro_cpl", "ind_recibe_visitas",
                 "ind_sin_sentencia", "ind_actividad_ocup", "F1_S2_P02", "F1_S2_P11",
                 col_discap, col_sexual)
faltan <- setdiff(requeridas, names(micro))
if (length(faltan) > 0 || length(col_motivos) == 0) {
  stop("Faltan columnas en la base: ", paste(c(faltan, if (!length(col_motivos)) "F1_S6P02__*"),
                                            collapse = ", "), call. = FALSE)
}

# --- Variables a nivel individual -------------------------------------------
micro <- micro |>
  mutate(
    n_motivos = rowSums(pick(all_of(col_motivos)), na.rm = TRUE),
    minoria   = if_else(is.na(F1_S2_P11), NA_real_, as.numeric(F1_S2_P11 %in% 1:5)),
    sexual    = if_else(is.na(F1_S4P16__11) & is.na(F1_S4P16__12), NA_real_,
                        as.numeric(coalesce(F1_S4P16__11, 0) == 1 | coalesce(F1_S4P16__12, 0) == 1)),
    across(all_of(col_discap), ~ as.numeric(.x > 1), .names = "dif_{.col}"),
    sexo = case_when(as.character(F1_S2_P02) %in% c("1", "Hombre") ~ "Hombre",
                     as.character(F1_S2_P02) %in% c("2", "Mujer")  ~ "Mujer",
                     TRUE ~ NA_character_)
  )

# --- Base por centro ---------------------------------------------------------
base <- micro |>
  group_by(cpl_cod, cpl = cpl_nombre) |>
  summarise(
    n_personas          = n(),
    n_resp              = sum(!is.na(ind_inseguro_cpl)),
    inseguros           = sum(ind_inseguro_cpl == 1, na.rm = TRUE),
    p_inseguro          = mean(ind_inseguro_cpl, na.rm = TRUE),
    sin_visitas         = 1 - mean(ind_recibe_visitas, na.rm = TRUE),
    procesados          = mean(ind_sin_sentencia, na.rm = TRUE),
    motivos_por_persona = sum(n_motivos) / n(),
    talleres            = mean(ind_actividad_ocup, na.rm = TRUE),
    minoria_etnica      = mean(minoria, na.rm = TRUE),
    p_delito_sexual     = mean(sexual, na.rm = TRUE),
    discapacidad        = rowMeans(across(starts_with("dif_"), ~ mean(.x, na.rm = TRUE))),
    .groups = "drop"
  ) |>
  filter(n_personas >= 50) |>
  mutate(ln_tam  = log(n_personas),
         es_loja = cpl_cod == 20,
         cpl_corto = str_squish(str_replace(
           cpl, "^Centro de (Privaci[oó]n( Provisional)? de Libertad|Rehabilitaci[oó]n Social) ", "")))

stopifnot("Debe existir exactamente un CPL Loja (cpl_cod == 20)" = sum(base$es_loja) == 1)

# Etiquetas, orden de la matriz y signo esperado (fijado antes de calcular, según la revisión)
etiquetas <- c(
  p_inseguro          = "Inseguridad",
  sin_visitas         = "Sin visitas",
  ln_tam              = "ln(Tamaño)",
  procesados          = "Procesados",
  motivos_por_persona = "Discriminación",
  talleres            = "Talleres",
  minoria_etnica      = "Minoría étnica",
  p_delito_sexual     = "Delito sexual",
  discapacidad        = "Discapacidad"
)
vars <- names(etiquetas)
esperado <- c(sin_visitas = "+", ln_tam = "+", procesados = "+", motivos_por_persona = "+",
              talleres = "−")          # variables de composición: sin signo esperado

d <- base |> select(cpl, cpl_cod, cpl_corto, es_loja, n_personas, n_resp, inseguros, all_of(vars))
stopifnot(!anyNA(d[vars]))
n_centros <- nrow(d)

# Personas de los centros analizados (para las gráficas por sexo y por motivo)
micro29 <- micro |> semi_join(base, by = "cpl_cod")
