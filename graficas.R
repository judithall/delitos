library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(here)

options(OutDec = ",")

# ==============================================================================
# 0. CARGAR Y PROCESAR MICRODATOS Y BASE POR CENTRO
# ==============================================================================
ruta_base <- here("datos", "salida_cp2022", "cp2022_base_limpia.rds")

if (!file.exists(ruta_base)) {
  stop("No existe ", ruta_base, ". Ejecute primero el script de depuración.")
}

micro <- readRDS(ruta_base)

# Columnas de motivos de discriminación (excluyendo '99')
col_motivos <- setdiff(grep("^F1_S6P02__", names(micro), value = TRUE), "F1_S6P02__99")
col_discap  <- paste0("F1_S2_P070", 1:6)

# Mutate a nivel individual
micro <- micro |>
  mutate(
    n_motivos = rowSums(pick(all_of(col_motivos)), na.rm = TRUE),
    minoria   = if_else(is.na(F1_S2_P11), NA_real_, as.numeric(F1_S2_P11 %in% 1:5)),
    sexual    = if_else(is.na(F1_S4P16__11) & is.na(F1_S4P16__12), NA_real_,
                        as.numeric(coalesce(F1_S4P16__11, 0) == 1 |
                                     coalesce(F1_S4P16__12, 0) == 1)),
    across(all_of(col_discap), ~ as.numeric(.x > 1), .names = "dif_{.col}")
  )

# Base agrupada por CPL (para gráficas 1 y 2)
base <- micro |>
  group_by(cpl_cod, cpl = cpl_nombre) |>
  summarise(
    n_personas          = n(),
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
         es_loja = cpl_cod == 20)

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
d <- base |> select(cpl, es_loja, all_of(vars))

# Crear carpeta para exportar imágenes si no existe
dir.create(here("figuras"), showWarnings = FALSE, recursive = TRUE)
media_inseguridad <- mean(d$p_inseguro, na.rm = TRUE)

# ==============================================================================
# GRAFICA 1: Ranking de inseguridad percibida por CPL (Objetivo 1)
# ==============================================================================
p1 <- d |>
  mutate(cpl = reorder(cpl, p_inseguro)) |>
  ggplot(aes(x = p_inseguro, y = cpl, fill = es_loja)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_vline(xintercept = media_inseguridad, linetype = "dashed", color = "#D95F02", linewidth = 0.8) +
  geom_text(aes(label = percent(p_inseguro, accuracy = 0.1, decimal.mark = ",")), hjust = -0.15, size = 3.2) +
  annotate("text", x = media_inseguridad + 0.02, y = 3, 
           label = paste0("Promedio: ", percent(media_inseguridad, accuracy = 0.1, decimal.mark = ",")),
           color = "#D95F02", fontface = "bold", hjust = 0, size = 3.5) +
  scale_x_continuous(labels = percent_format(decimal.mark = ","), limits = c(0, max(d$p_inseguro) * 1.15), expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "#B2182B", "FALSE" = "#2B5C8F")) +
  labs(
    title = "Distribución de la inseguridad percibida por CPL",
    subtitle = "Proporción de personas privadas de la libertad que declaran sentirse inseguras (N = 29 centros)",
    x = "Proporción de inseguridad percibida", y = NULL,
    caption = "Fuente: INEC, Censo Penitenciario 2022."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

ggsave(here("figuras", "01_ranking_inseguridad_cpl.png"), plot = p1, width = 8.5, height = 9, dpi = 300)

# ==============================================================================
# GRAFICA 2: Densidad de la inseguridad percibida (Objetivo 1)
# ==============================================================================
p2 <- ggplot(d, aes(x = p_inseguro)) +
  geom_histogram(aes(y = after_stat(density)), bins = 8, fill = "#2B5C8F", color = "white", alpha = 0.7) +
  geom_density(color = "#B2182B", linewidth = 1) +
  geom_vline(xintercept = media_inseguridad, linetype = "dashed", color = "#D95F02", linewidth = 0.8) +
  scale_x_continuous(labels = percent_format(decimal.mark = ",")) +
  labs(
    title = "Distribución de la densidad de la inseguridad percibida",
    subtitle = "Variación del nivel de inseguridad reportado entre los 29 CPL",
    x = "Proporción de personas que se sienten inseguras", y = "Densidad",
    caption = "Fuente: INEC, Censo Penitenciario 2022."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank())

ggsave(here("figuras", "02_densidad_inseguridad.png"), plot = p2, width = 7, height = 4.5, dpi = 300)

# ==============================================================================
# GRAFICA 3: Inseguridad percibida según género
# ==============================================================================
df_genero <- micro |>
  filter(!is.na(F1_S2_P02), !is.na(ind_inseguro_cpl)) |>
  mutate(sexo = case_when(
    F1_S2_P02 %in% c(1, "1", "Hombre") ~ "Hombre",
    F1_S2_P02 %in% c(2, "2", "Mujer") ~ "Mujer",
    TRUE ~ as.character(F1_S2_P02)
  )) |>
  group_by(sexo) |>
  summarise(
    total = n(),
    p_inseguro = mean(ind_inseguro_cpl, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(df_genero, aes(x = sexo, y = p_inseguro, fill = sexo)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = percent(p_inseguro, accuracy = 0.1, decimal.mark = ",")), vjust = -0.5, fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format(decimal.mark = ","), limits = c(0, max(df_genero$p_inseguro) * 1.2), expand = c(0, 0)) +
  scale_fill_manual(values = c("Hombre" = "#2B5C8F", "Mujer" = "#E7298A")) +
  labs(
    title = "Percepción de inseguridad según género",
    subtitle = "Proporción de personas privadas de libertad que se sienten inseguras por sexo",
    x = "Género / Sexo", y = "Proporción que se siente insegura",
    caption = "Fuente: INEC, Censo Penitenciario 2022."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

ggsave(here("figuras", "03_inseguridad_por_genero.png"), plot = p3, width = 6, height = 5, dpi = 300)

# ==============================================================================
# GRAFICA 4: Tipos de discriminación o maltrato más frecuentes en el CPL
# ==============================================================================
n_total_personas <- nrow(micro)

# Mapeo de nombres descriptivos para cada motivo de discriminación F1_S6P02
etiquetas_motivos <- c(
  "F1_S6P02__1"  = "Etnia / Raza",
  "F1_S6P02__2"  = "Edad",
  "F1_S6P02__3"  = "Orientación sexual / Identidad género",
  "F1_S6P02__4"  = "Nacionalidad / Origen",
  "F1_S6P02__5"  = "Religión / Creencias",
  "F1_S6P02__6"  = "Discapacidad / Salud",
  "F1_S6P02__7"  = "Delito imputado / Condena",
  "F1_S6P02__8"  = "Condición socioeconómica",
  "F1_S6P02__9"  = "Pertenencia a banda / Grupo",
  "F1_S6P02__10" = "Aspecto físico / Tatuajes"
)

# Filtrar las columnas que realmente existan en la base
col_existentes <- intersect(names(etiquetas_motivos), names(micro))

df_motivos <- micro |>
  summarise(across(all_of(col_existentes), ~ sum(.x == 1, na.rm = TRUE))) |>
  pivot_longer(cols = everything(), names_to = "cod_motivo", values_to = "casos") |>
  mutate(
    motivo = coalesce(etiquetas_motivos[cod_motivo], cod_motivo),
    prop = casos / n_total_personas,
    motivo = reorder(motivo, prop)
  )

p4 <- ggplot(df_motivos, aes(x = prop, y = motivo)) +
  geom_col(fill = "#7570B3", width = 0.65) +
  geom_text(aes(label = percent(prop, accuracy = 0.1, decimal.mark = ",")), hjust = -0.15, size = 3.3) +
  scale_x_continuous(labels = percent_format(decimal.mark = ","), limits = c(0, max(df_motivos$prop) * 1.2), expand = c(0, 0)) +
  labs(
    title = "Motivos de discriminación o maltrato reportados en el CPL",
    subtitle = "Proporción del total de personas entrevistadas que reporta cada motivo",
    x = "Proporción de personas", y = NULL,
    caption = "Fuente: INEC, Censo Penitenciario 2022."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13), panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

ggsave(here("figuras", "04_motivos_discriminacion.png"), plot = p4, width = 8, height = 5.5, dpi = 300)

message("¡Las 4 gráficas han sido generadas y guardadas exitosamente en la carpeta 'figuras/'!")
