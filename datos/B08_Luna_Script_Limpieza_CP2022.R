# =============================================================================
# B08 - CENSO PENITENCIARIO 2022 (INEC, Ecuador)
# Script reproducible: importación, diagnóstico, limpieza, variables derivadas,
# indicadores territoriales y diccionario de variables.
#
# Autora : Luna  |  Club de Economía Aplicada - Carrera de Economía, UNL
# Insumo : cp2022_F.sav (Base_de_datos_CP_2022_v2, INEC)
# Salidas: carpeta `salida_cp2022/` (CSV UTF-8, RDS, Excel, bitácora)
#
# Cómo usar: 1) ajuste `ruta_sav`; 2) ejecute el script completo (source).
# Paquetes : haven, dplyr, tidyr, stringr, readr, openxlsx
#   install.packages(c("haven","dplyr","tidyr","stringr","readr","openxlsx"))
# =============================================================================

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr)
  library(stringr); library(readr); library(openxlsx)
})

# ---- 0. PARÁMETROS -----------------------------------------------------------
# Ruta de la base: puede ser la carpeta que contiene el .sav o el propio archivo .sav.
# En R se escriben las rutas con barras normales "/" (no "\").
ruta_sav        <- Sys.getenv("CP2022_SAV", unset =
  "C:/Users/Usuario/OneDrive/Documents/CICLO/CICLO 6/Club CEA/Base_de_datos_CP_2022_v2")
dir_salida      <- "salida_cp2022"
solo_completas  <- TRUE     # conservar solo entrevistas con resultado "Completa"
pob_prov_csv    <- "poblacion_provincial_2022.csv"  # OPCIONAL: columnas prov_cod, poblacion
                                                    # (Censo 2022) para tasas por 100 000 hab.
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)
if (!l10n_info()[["UTF-8"]])
  warning("Su sesión de R no usa UTF-8: las tildes pueden exportarse mal. ",
          "En RStudio use Tools > Global Options > Code > Saving > Default text encoding = UTF-8 ",
          "y ejecute con source('archivo.R', encoding = 'UTF-8').")

bitacora <- character()
log_msg <- function(...) {
  m <- paste0(...)
  message(m); bitacora <<- c(bitacora, m)
}

# ---- 1. IMPORTACIÓN (conserva etiquetas de variables y de valores) -----------
# Si la ruta no existe, se busca un .sav con "cp2022" en el nombre en la carpeta
# de trabajo, Descargas y Documentos; si no aparece, se abre un selector de archivos.
if (nzchar(ruta_sav) && dir.exists(ruta_sav)) {          # se indicó una carpeta: buscar el .sav dentro
  dentro <- list.files(ruta_sav, pattern = "\\.sav$", full.names = TRUE,
                       recursive = TRUE, ignore.case = TRUE)
  if (length(dentro) == 0) stop("No hay archivos .sav dentro de: ", ruta_sav)
  if (length(dentro) > 1) {
    pref <- dentro[grepl("cp2022", basename(dentro), ignore.case = TRUE)]
    dentro <- if (length(pref)) pref else dentro
  }
  ruta_sav <- dentro[1]
  message("Se usará la base: ", ruta_sav)
}
if (!nzchar(ruta_sav) || !file.exists(ruta_sav)) {
  carpetas <- unique(c(getwd(), "~/Downloads", "~/Descargas", "~/Documents", "~/Documentos",
                       "~/Desktop", "~/Escritorio"))
  candidatos <- list.files(carpetas[dir.exists(carpetas)], pattern = "cp2022.*\\.sav$",
                           full.names = TRUE, ignore.case = TRUE)
  if (length(candidatos)) {
    ruta_sav <- candidatos[1]
    message("Se encontró la base en: ", ruta_sav)
  } else if (interactive()) {
    message("No se encontró la base automáticamente: elija el archivo .sav en la ventana.")
    ruta_sav <- file.choose()
  } else {
    stop("No se encontró el archivo .sav. Defina `ruta_sav` con la ruta completa.")
  }
}
stopifnot(file.exists(ruta_sav))
raw <- read_sav(ruta_sav)
log_msg("1. Base importada: ", nrow(raw), " filas x ", ncol(raw), " columnas.")

# Metadatos originales (se capturan ANTES de quitar las etiquetas)
etq_var <- vapply(raw, function(x) {
  a <- attr(x, "label", exact = TRUE)
  if (is.null(a)) NA_character_ else as.character(a)
}, character(1))
etq_val <- lapply(raw, function(x) attr(x, "labels", exact = TRUE))

# ---- 2. DIAGNÓSTICO INICIAL --------------------------------------------------
diagnostico <- tibble(
  filas                 = nrow(raw),
  columnas              = ncol(raw),
  columnas_numericas    = sum(vapply(raw, is.numeric, logical(1))),
  columnas_texto        = sum(vapply(raw, is.character, logical(1))),
  ids_duplicados        = sum(duplicated(raw$INTERVIEW__ID)),
  codigos_ppl_repetidos = sum(duplicated(raw$F1_S1_CPPL)),
  columnas_100_nulas    = sum(colSums(!is.na(raw)) == 0),
  columnas_mas_90_nulas = sum(colMeans(is.na(raw)) > 0.90)
)
print(as.data.frame(t(diagnostico)))
log_msg("2. Llave única = INTERVIEW__ID (", diagnostico$ids_duplicados,
        " duplicados). F1_S1_CPPL NO es llave: tiene ",
        diagnostico$codigos_ppl_repetidos, " repetidos.")
print(table(Resultado = as_factor(raw$F1_SF_RES_LEV), useNA = "ifany"))

# ---- 3. FILTRO DE ENTREVISTAS VÁLIDAS ----------------------------------------
datos <- raw
if (solo_completas) {
  n0 <- nrow(datos)
  datos <- filter(datos, F1_SF_RES_LEV == 1)
  log_msg("3. Entrevistas no completas excluidas: ", n0 - nrow(datos),
          " (quedan ", nrow(datos), ").")
}

# Quitar etiquetas para operar con números/textos simples
datos <- datos |>
  zap_labels() |> zap_label() |> zap_formats() |> zap_widths() |>
  mutate(across(where(is.numeric), as.numeric)) |>
  mutate(across(where(is.character), ~ na_if(str_squish(.x), "")))

# ---- 4. CÓDIGOS "NO SABE / NO RESPONDE" -> NA --------------------------------
# 4a. Regla general: toda categoría cuya etiqueta empiece por "No sabe" o
#     "No responde" (cubre 8/9, 98/99, 998/999, 9888/9999, según la variable).
patron_nsnr <- "(?i)^\\s*no\\s+(sabe|responde)"
nulos_antes <- colSums(is.na(datos))
for (v in intersect(names(etq_val), names(datos))) {
  lv <- etq_val[[v]]
  if (is.null(lv) || !is.numeric(datos[[v]])) next
  cods <- unname(lv[str_detect(names(lv), patron_nsnr)])
  if (length(cods)) datos[[v]][datos[[v]] %in% cods] <- NA_real_
}
# 4b. Reglas explícitas para variables numéricas sin etiquetas de NS/NR
datos <- datos |>
  mutate(
    F1_S2_P03 = replace(F1_S2_P03, F1_S2_P03 >= 998, NA),   # edad: 998/999
    F1_S1_I04 = na_if(F1_S1_I04, 999)                       # zona: 999 = sin dato
  )
vars_anio <- c("F1_S2_P0403", "F1_S2_P0803A", "F1_S2_P3503",
               "F1_S4P0903", "F1_S4P1103", "F1_S4P1402")
datos[vars_anio] <- lapply(datos[vars_anio], \(x) replace(x, x >= 9000, NA))

nsnr_log <- tibble(variable = names(datos),
                   nuevos_na = as.integer(colSums(is.na(datos)) - nulos_antes[names(datos)])) |>
  filter(nuevos_na > 0) |> arrange(desc(nuevos_na))
log_msg("4. Códigos NS/NR convertidos a NA en ", nrow(nsnr_log),
        " variables (", sum(nsnr_log$nuevos_na), " celdas).")

# ---- 5. COLUMNAS: vacías y textos abiertos -----------------------------------
# No se eliminan las variables con muchos nulos por "salto de pregunta"
# (p. ej. embarazo, estado procesal): su NA es estructural, no un error.
cols_vacias <- names(datos)[colSums(!is.na(datos)) == 0]
cols_texto  <- setdiff(names(datos)[vapply(datos, is.character, logical(1))],
                       c("INTERVIEW__ID", "F1_S1_I12"))
textos_abiertos <- datos |> select(INTERVIEW__ID, any_of(cols_texto))
datos <- datos |> select(-any_of(c(cols_vacias, cols_texto)))
log_msg("5. Columnas 100% vacías eliminadas: ", length(cols_vacias),
        " | textos abiertos separados: ", length(cols_texto), ".")

# ---- 6. VARIABLES DERIVADAS Y ETIQUETAS --------------------------------------
dic_nombres <- function(var) { lv <- etq_val[[var]]; setNames(names(lv), as.character(unname(lv))) }
nom_prov <- dic_nombres("F1_S4P0221")                       # 24 provincias (DPA)
nom_cant <- c(dic_nombres("F1_S4P0222"), dic_nombres("F1_S1_I02"))
nom_cant <- nom_cant[!duplicated(names(nom_cant))]
nombre_de <- function(cod, dic) unname(dic[as.character(cod)])

factor_etq <- function(x, var) {
  lv <- etq_val[[var]]
  droplevels(factor(x, levels = unname(lv), labels = names(lv)))
}

datos <- datos |>
  mutate(
    # Identificación territorial del CPL (códigos DPA INEC)
    prov_cpl_cod = F1_S1_I01, prov_cpl = nombre_de(F1_S1_I01, nom_prov),
    cant_cpl_cod = F1_S1_I02, cant_cpl = nombre_de(F1_S1_I02, nom_cant),
    cpl_cod      = F1_S1_I11, cpl_nombre = F1_S1_I12,
    # Perfil
    sexo      = factor(F1_S2_P02, 1:2, c("Hombre", "Mujer")),
    edad      = F1_S2_P03,
    grupo_edad = cut(edad, c(17, 24, 34, 44, 54, Inf),
                     labels = c("18-24", "25-34", "35-44", "45-54", "55 y más")),
    # Hacinamiento (personas con las que comparte celda)
    pers_celda = F1_S5P01,
    hacin_cat  = cut(pers_celda, c(-1, 0, 4, 9, Inf),
                     labels = c("Ninguna", "1-4", "5-9", "10 o más")),
    # Fechas
    anio_detencion = ifelse(F1_S4P0903 >= 1980 & F1_S4P0903 <= 2022, F1_S4P0903, NA),
    anios_desde_detencion = 2022 - anio_detencion,
    fecha_entrevista = as.Date(ISOdate(2022, F1_SF_F_LGBTI_MES, F1_SF_F_LGBTI_DIA)),
    # Residencia previa a la detención (se imputa el cantón del CPL cuando
    # la persona declaró vivir "en esta ciudad o parroquia rural")
    prov_resid_cod = case_when(F1_S4P02 == 1 ~ F1_S1_I01, F1_S4P02 == 2 ~ F1_S4P0221),
    cant_resid_cod = case_when(F1_S4P02 == 1 ~ F1_S1_I02, F1_S4P02 == 2 ~ F1_S4P0222),
    prov_resid = nombre_de(prov_resid_cod, nom_prov),
    cant_resid = nombre_de(cant_resid_cod, nom_cant),
    # Delitos (solo sentenciados: S4P16 es NA para quienes no lo son)
    n_delitos      = ifelse(F1_S4P12 == 1,
                            rowSums(pick(starts_with("F1_S4P16__") &
                                           !ends_with(c("__98", "__99"))), na.rm = TRUE), NA),
    delito_robo      = F1_S4P16__1,
    delito_drogas    = F1_S4P16__2,
    delito_homicidio = F1_S4P16__5
  )

# Columnas con etiqueta de texto para variables categóricas clave
vars_cat <- c("F1_S1_P10", "F1_S2_P02", "F1_S2_P08", "F1_S2_P11", "F1_S2_P17",
              "F1_S2_P31", "F1_S4P01", "F1_S4P02", "F1_S4P07", "F1_S4P12",
              "F1_S4P15", "F1_S4P21", "F1_S5P47", "F1_S5P48", "F1_S6P01")
for (v in vars_cat) datos[[paste0(v, "_lbl")]] <- factor_etq(datos[[v]], v)
log_msg("6. Variables derivadas y etiquetas creadas.")

# ---- 7. INDICADORES A NIVEL PERSONA (0/1/NA) ---------------------------------
# Cada indicador es un binario; su promedio por territorio * 100 = porcentaje.
# Los NA (NS/NR o salto de pregunta) quedan fuera del denominador.
cualquiera <- function(...) {
  m <- cbind(...)
  ifelse(rowSums(!is.na(m)) == 0, NA, as.numeric(rowSums(m == 1, na.rm = TRUE) > 0))
}
datos <- datos |>
  mutate(
    ind_mujer             = as.numeric(F1_S2_P02 == 2),
    ind_sin_sentencia     = as.numeric(F1_S4P12 == 2),                 # procesados
    ind_mas_2_anios       = as.numeric(F1_S4P01 == 5),                 # >2 años privado de libertad
    ind_hacinamiento5     = as.numeric(pers_celda >= 5),               # comparte celda con >=5
    ind_inseguro_celda    = as.numeric(F1_S5P47 == 2),
    ind_inseguro_cpl      = as.numeric(F1_S5P48 == 2),
    ind_programa_educ     = as.numeric(F1_S5P10 == 1),
    ind_actividad_ocup    = cualquiera(F1_S5P1501, F1_S5P1502, F1_S5P1503, F1_S5P1504),
    ind_extranjero_nac    = as.numeric(F1_S2_P08 == 3),                # nació en otro país
    ind_resid_otra_prov   = as.numeric(prov_resid_cod != prov_cpl_cod),
    ind_reincidencia      = as.numeric(F1_S6P01 == 1),
    ind_consumo_30d       = cualquiera(F1_S5P3103, F1_S5P3104, F1_S5P3105,
                                       F1_S5P3106, F1_S5P3107, F1_S5P3108, F1_S5P3109),
    ind_intento_suicidio  = as.numeric(F1_S5P33 == 1),
    ind_recibe_visitas    = as.numeric(F1_S5P42 == 1),
    ind_delito_drogas     = delito_drogas,
    ind_delito_robo       = delito_robo,
    ind_delito_homicidio  = delito_homicidio
  )

catalogo_ind <- tribble(
  ~indicador,             ~descripcion,
  "ind_mujer",            "% de mujeres entre las PPL",
  "ind_sin_sentencia",    "% de PPL procesadas (sin sentencia) sobre sentenciadas + procesadas",
  "ind_mas_2_anios",      "% con más de 2 años privadas de libertad",
  "ind_hacinamiento5",    "% que comparte celda con 5 o más personas",
  "ind_inseguro_celda",   "% que se siente inseguro/a en su celda",
  "ind_inseguro_cpl",     "% que se siente inseguro/a en el CPL",
  "ind_programa_educ",    "% que participa en el programa educativo del CPL",
  "ind_actividad_ocup",   "% que participa en alguna actividad ocupacional",
  "ind_extranjero_nac",   "% nacida en otro país",
  "ind_resid_otra_prov",  "% cuya residencia previa a la detención era otra provincia",
  "ind_reincidencia",     "% con sentencia previa (reincidencia)",
  "ind_consumo_30d",      "% que consumió alguna sustancia ilícita en los últimos 30 días",
  "ind_intento_suicidio", "% que declara haber intentado quitarse la vida",
  "ind_recibe_visitas",   "% que ha recibido visitas de familiares o amigos",
  "ind_delito_drogas",    "% de sentenciados/as por tráfico ilícito de sustancias",
  "ind_delito_robo",      "% de sentenciados/as por robo",
  "ind_delito_homicidio", "% de sentenciados/as por homicidio/asesinato"
)
ind_vars <- catalogo_ind$indicador

# ---- 8. AGREGACIÓN TERRITORIAL -----------------------------------------------
resumir <- function(d, nivel, cod = NULL, nom = NULL, por_sexo = FALSE) {
  d <- d |> mutate(.cod = if (is.null(cod)) "000" else as.character(.data[[cod]]),
                   .nom = if (is.null(nom)) "Ecuador" else as.character(.data[[nom]]),
                   .sexo = if (por_sexo) as.character(sexo) else "Total")
  d |>
    group_by(.cod, .nom, .sexo) |>
    summarise(
      personas = n(),
      across(all_of(ind_vars),
             list(valor = ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE) * 100,
                  n     = ~ sum(!is.na(.x))),
             .names = "{.col}@@{.fn}"),
      pers_celda_mediana = median(pers_celda, na.rm = TRUE),
      .groups = "drop") |>
    pivot_longer(-c(.cod, .nom, .sexo, personas, pers_celda_mediana),
                 names_to = c("indicador", ".stat"), names_sep = "@@") |>
    pivot_wider(names_from = .stat, values_from = value) |>
    transmute(nivel = nivel, codigo = .cod, nombre = .nom, sexo = .sexo,
              indicador, valor, n_validos = n, personas, pers_celda_mediana)
}

indicadores <- bind_rows(
  resumir(datos, "Nacional"),
  resumir(datos, "Nacional", por_sexo = TRUE),
  resumir(datos, "Provincia", "prov_cpl_cod", "prov_cpl"),
  resumir(datos, "Provincia", "prov_cpl_cod", "prov_cpl", por_sexo = TRUE),
  resumir(datos, "Cantón",    "cant_cpl_cod", "cant_cpl"),
  resumir(datos, "Centro (CPL)", "cpl_cod", "cpl_nombre")
) |>
  left_join(catalogo_ind, by = "indicador") |>
  # Con pocos casos válidos el porcentaje es poco confiable: se marca (n < 30)
  mutate(poco_confiable = n_validos < 30)

# Tasa de PPL por 100 000 habitantes (requiere población del Censo 2022)
pers_prov <- datos |> count(prov_cpl_cod, prov_cpl, name = "personas")
if (file.exists(pob_prov_csv)) {
  pob <- read_csv(pob_prov_csv, show_col_types = FALSE)
  pers_prov <- pers_prov |>
    left_join(pob, by = c("prov_cpl_cod" = "prov_cod")) |>
    mutate(tasa_100mil = personas / poblacion * 1e5)
  log_msg("8. Tasas por 100 000 hab. calculadas con ", pob_prov_csv)
} else {
  log_msg("8. (Opcional) Coloque ", pob_prov_csv, " para calcular tasas por 100 000 hab.")
}

# Matriz origen-destino: provincia de residencia previa -> provincia del CPL
flujos <- datos |>
  filter(!is.na(prov_resid_cod)) |>
  count(prov_resid_cod, prov_resid, prov_cpl_cod, prov_cpl, name = "personas")

# ---- 9. DICCIONARIO DE VARIABLES (anexo del informe) -------------------------
modulo_de <- function(v) case_when(
  v == "INTERVIEW__ID"   ~ "ID Identificador",
  str_detect(v, "^F1_S1") ~ "S1 Identificación y ubicación del CPL",
  str_detect(v, "^F1_S2") ~ "S2 Características de la persona",
  str_detect(v, "^F1_S3") ~ "S3 Hijos/as",
  str_detect(v, "^F1_S4") ~ "S4 Situación jurídica y detención",
  str_detect(v, "^F1_S5") ~ "S5 Condiciones de vida y tratamiento en el CPL",
  str_detect(v, "^F1_S6") ~ "S6 Antecedentes de privación de libertad",
  str_detect(v, "^F1_SF") ~ "SF Cierre de la entrevista",
  TRUE ~ "Otras")

vars_orig <- names(raw)
diccionario <- tibble(
  variable   = vars_orig,
  etiqueta   = unname(etq_var[vars_orig]),
  modulo     = modulo_de(vars_orig),
  tipo_spss  = vapply(raw, \(x) if (is.character(x)) "texto" else "numérica", character(1)),
  n_validos  = as.integer(colSums(!is.na(raw))),
  pct_nulos_originales = round(100 * colMeans(is.na(raw)), 1),
  n_categorias_etiquetadas = vapply(etq_val, \(l) length(l), integer(1)),
  codigos_nsnr = vapply(etq_val, function(l) {
    if (is.null(l)) return("")
    paste(unname(l)[str_detect(names(l), patron_nsnr)], collapse = ", ")
  }, character(1)),
  en_base_limpia = vars_orig %in% names(datos)
)
valores <- bind_rows(lapply(vars_orig, function(v) {
  lv <- etq_val[[v]]
  if (is.null(lv)) return(NULL)
  tibble(variable = v, codigo = as.numeric(unname(lv)), etiqueta = names(lv))
}))

# ---- 10. VALIDACIONES --------------------------------------------------------
stopifnot(
  !anyDuplicated(datos$INTERVIEW__ID),
  all(datos$edad >= 18 & datos$edad <= 100, na.rm = TRUE),
  all(datos$anio_detencion <= 2022, na.rm = TRUE),
  !any(datos$pers_celda >= 998, na.rm = TRUE),
  all(datos$ind_hacinamiento5 %in% c(0, 1, NA))
)
log_msg("10. Validaciones superadas. Base limpia: ", nrow(datos), " filas x ",
        ncol(datos), " columnas.")
print(table(datos$sexo, useNA = "ifany"))

# ---- 11. EXPORTACIÓN ---------------------------------------------------------
# write_excel_csv guarda UTF-8 con BOM: Excel y Power BI leen bien las tildes.
write_excel_csv(datos,           file.path(dir_salida, "cp2022_base_limpia.csv"))
write_excel_csv(textos_abiertos, file.path(dir_salida, "cp2022_textos_abiertos.csv"))
write_excel_csv(indicadores,     file.path(dir_salida, "cp2022_indicadores_territoriales.csv"))
write_excel_csv(pers_prov,       file.path(dir_salida, "cp2022_ppl_por_provincia.csv"))
write_excel_csv(flujos,          file.path(dir_salida, "cp2022_flujos_residencia_cpl.csv"))
write_excel_csv(nsnr_log,        file.path(dir_salida, "cp2022_log_nsnr.csv"))
saveRDS(datos, file.path(dir_salida, "cp2022_base_limpia.rds"))

wb <- createWorkbook()
for (h in c("Diccionario", "Valores", "Indicadores")) addWorksheet(wb, h)
writeDataTable(wb, "Diccionario", diccionario, tableStyle = "TableStyleMedium2")
writeDataTable(wb, "Valores", valores, tableStyle = "TableStyleMedium2")
writeDataTable(wb, "Indicadores", catalogo_ind, tableStyle = "TableStyleMedium2")
setColWidths(wb, "Diccionario", cols = 1:9, widths = c(22, 60, 40, 12, 11, 14, 14, 14, 14))
setColWidths(wb, "Valores", cols = 1:3, widths = c(22, 10, 80))
setColWidths(wb, "Indicadores", cols = 1:2, widths = c(24, 80))
freezePane(wb, "Diccionario", firstRow = TRUE)
saveWorkbook(wb, file.path(dir_salida, "B08_Luna_Diccionario.xlsx"), overwrite = TRUE)

writeLines(c(paste("Bitácora de limpieza -", Sys.time()), bitacora),
           file.path(dir_salida, "bitacora_limpieza.txt"), useBytes = TRUE)
message("Listo. Archivos en: ", normalizePath(dir_salida))
