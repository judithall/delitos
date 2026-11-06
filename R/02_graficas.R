# =============================================================================
# 02_graficas.R  —  Gráficas del capítulo de metodología (objetivo 1 y correlación)
# Cada función devuelve un objeto ggplot. graficas.R las guarda como PNG.
# =============================================================================
suppressPackageStartupMessages({library(ggplot2); library(scales); library(dplyr); library(tidyr); library(purrr); library(stringr)})

col_centro <- "#2B5C8F"; col_loja <- "#B2182B"; col_prom <- "#D95F02"
col_mot <- "#7570B3";    col_muj <- "#E7298A"
pct <- function(x, k = 0.1) percent(x, accuracy = k, decimal.mark = ",")

tema_inseg <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", size = base_size + 2),
          plot.title.position = "plot",
          plot.subtitle = element_text(colour = "grey30"),
          plot.caption = element_text(colour = "grey35", hjust = 0),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}
pie_fuente <- "Fuente: INEC, Censo Penitenciario 2022."
fmt_eje <- function(x) sub("^-", "−", format(x, nsmall = 1, decimal.mark = ",", trim = TRUE))   # ejes: −0,5; 0,0; 0,5
fmt_miles <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)

# ---- Figura: ranking de inseguridad por centro, con IC 95 % de Wilson ----------
g_ranking <- function(d) {
  m <- mean(d$p_inseguro)
  dd <- d |>
    mutate(lo = wilson(inseguros, n_resp)$lo, hi = wilson(inseguros, n_resp)$hi,
           cpl_corto = reorder(cpl_corto, p_inseguro))
  ggplot(dd, aes(p_inseguro, cpl_corto, colour = es_loja)) +
    geom_vline(xintercept = m, linetype = "dashed", colour = col_prom, linewidth = 0.7) +
    geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.8, alpha = 0.7) +
    geom_point(aes(size = n_resp)) +
    geom_text(aes(x = hi, label = pct(p_inseguro)), hjust = -0.25, size = 3, show.legend = FALSE) +
    annotate("text", x = m, y = 1, hjust = -0.1, vjust = -1.2, colour = col_prom, fontface = "bold",
             size = 3.4, label = paste0("Promedio de centros: ", pct(m))) +
    scale_colour_manual(values = c(`FALSE` = col_centro, `TRUE` = col_loja), guide = "none") +
    scale_size_area(max_size = 5, name = "Personas que responden", labels = label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(labels = percent_format(decimal.mark = ",", accuracy = 1),
                       expand = expansion(mult = c(0.01, 0.12))) +
    labs(title = "Inseguridad percibida por centro de privación de libertad",
         subtitle = paste0("Proporción que se siente insegura, con intervalo de confianza del 95 % (N = ", nrow(d), " centros)"),
         x = "Proporción que se siente insegura", y = NULL,
         caption = paste0("En rojo, CPL Loja N.° 1. Intervalos de Wilson. ", pie_fuente)) +
    tema_inseg() + theme(panel.grid.major.y = element_blank())
}

# ---- Figura: distribución entre centros (histograma + densidad + media y mediana) --
g_densidad <- function(d) {
  m <- mean(d$p_inseguro); md <- median(d$p_inseguro); l <- d$p_inseguro[d$es_loja]
  ggplot(d, aes(p_inseguro)) +
    geom_histogram(aes(y = after_stat(density)), bins = 8, fill = col_centro, colour = "white", alpha = 0.7) +
    geom_density(colour = col_loja, linewidth = 1) +
    geom_rug(aes(colour = es_loja), sides = "b", linewidth = 0.8, show.legend = FALSE) +
    geom_vline(xintercept = m,  linetype = "dashed", colour = col_prom, linewidth = 0.8) +
    geom_vline(xintercept = md, linetype = "dotted", colour = "grey20", linewidth = 0.8) +
    annotate("text", x = m,  y = Inf, label = paste0("Media: ", pct(m)),    colour = col_prom, hjust = -0.1, vjust = 1.6, size = 3.4, fontface = "bold") +
    annotate("text", x = md, y = Inf, label = paste0("Mediana: ", pct(md)), colour = "grey20", hjust = 1.1,  vjust = 1.6, size = 3.4, fontface = "bold") +
    annotate("text", x = l, y = 0.35, label = "Loja N.° 1", colour = col_loja, hjust = 1.1, size = 3.4) +
    scale_colour_manual(values = c(`FALSE` = "grey30", `TRUE` = col_loja)) +
    scale_x_continuous(labels = percent_format(decimal.mark = ",", accuracy = 1)) +
    labs(title = "Distribución de la inseguridad percibida entre centros",
         subtitle = paste0("Histograma y densidad de la proporción que se siente insegura (N = ", nrow(d), " centros)"),
         x = "Proporción que se siente insegura", y = "Densidad", caption = pie_fuente) +
    tema_inseg()
}

# ---- Figura: inseguridad según sexo (personas de los centros analizados), IC 95 % --
g_sexo <- function(micro29) {
  ds <- micro29 |> filter(!is.na(sexo), !is.na(ind_inseguro_cpl)) |>
    group_by(sexo) |> summarise(n = n(), x = sum(ind_inseguro_cpl == 1), p = x / n, .groups = "drop") |>
    mutate(lo = wilson(x, n)$lo, hi = wilson(x, n)$hi)
  ggplot(ds, aes(sexo, p, fill = sexo)) +
    geom_col(width = 0.5, show.legend = FALSE) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.7) +
    geom_text(aes(y = hi, label = paste0(pct(p), "\n(n = ", fmt_miles(n), ")")),
              vjust = -0.3, size = 3.6, fontface = "bold") +
    scale_fill_manual(values = c(Hombre = col_centro, Mujer = col_muj)) +
    scale_y_continuous(labels = percent_format(decimal.mark = ",", accuracy = 1),
                       limits = c(0, max(ds$hi) * 1.3), expand = c(0, 0)) +
    labs(title = "Inseguridad percibida según sexo",
         subtitle = "Proporción que se siente insegura, con intervalo de confianza del 95 %",
         x = NULL, y = "Proporción que se siente insegura",
         caption = paste0("Personas de los ", n_distinct(micro29$cpl_cod), " centros analizados. ", pie_fuente)) +
    tema_inseg() + theme(panel.grid.major.x = element_blank())
}

# ---- Figura: motivos de discriminación o maltrato reportados --------------------
# Las etiquetas siguen su mapeo del cuestionario (verifique contra el diccionario del INEC).
etiquetas_motivos <- c(
  "F1_S6P02__1"  = "Etnia / Raza",
  "F1_S6P02__2"  = "Edad",
  "F1_S6P02__3"  = "Orientación sexual / Identidad de género",
  "F1_S6P02__4"  = "Nacionalidad / Origen",
  "F1_S6P02__5"  = "Religión / Creencias",
  "F1_S6P02__6"  = "Discapacidad / Salud",
  "F1_S6P02__7"  = "Delito imputado / Condena",
  "F1_S6P02__8"  = "Condición socioeconómica",
  "F1_S6P02__9"  = "Pertenencia a banda / Grupo",
  "F1_S6P02__10" = "Aspecto físico / Tatuajes"
)
g_motivos <- function(micro29) {
  cols <- intersect(names(etiquetas_motivos), names(micro29)); n <- nrow(micro29)
  dm <- micro29 |> summarise(across(all_of(cols), ~ sum(.x == 1, na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "cod", values_to = "casos") |>
    mutate(motivo = coalesce(etiquetas_motivos[cod], cod), prop = casos / n,
           motivo = reorder(motivo, prop))
  ggplot(dm, aes(prop, motivo)) +
    geom_col(fill = col_mot, width = 0.65) +
    geom_text(aes(label = paste0(pct(prop), "  (", fmt_miles(casos), ")")),
              hjust = -0.1, size = 3.2) +
    scale_x_continuous(labels = percent_format(decimal.mark = ",", accuracy = 1),
                       limits = c(0, max(dm$prop) * 1.3), expand = c(0, 0)) +
    labs(title = "Motivos de discriminación o maltrato reportados",
         subtitle = "Proporción de personas entrevistadas que marca cada motivo (puede marcar varios)",
         x = "Proporción de personas", y = NULL,
         caption = paste0("Entre paréntesis, número de personas. ", pie_fuente)) +
    tema_inseg() + theme(panel.grid.major.y = element_blank())
}

# ---- Figura: correlaciones con la inseguridad (Pearson, IC 95 %, sensibilidad) ---
g_forest <- function(con_dep, n) {
  rc <- r_critico(n)
  dd <- con_dep |>
    mutate(sig = case_when(p_holm < 0.05 ~ "Significativa tras Holm",
                           p < 0.05 ~ "Solo sin ajuste", TRUE ~ "No significativa"),
           sig = factor(sig, levels = c("Significativa tras Holm", "Solo sin ajuste", "No significativa")),
           variable = reorder(paste0(variable, " (", esperado, ")"), r))
  ggplot(dd, aes(y = variable)) +
    geom_vline(xintercept = 0, colour = "grey40") +
    geom_vline(xintercept = c(-rc, rc), linetype = "dotted", colour = "grey55") +
    geom_segment(aes(x = r_min, xend = r_max, yend = variable), colour = "grey70", linewidth = 3.2, alpha = 0.7) +
    geom_linerange(aes(xmin = lo, xmax = hi, colour = sig), linewidth = 0.9) +
    geom_point(aes(x = r, colour = sig), size = 3) +
    geom_point(aes(x = r_sin_loja), shape = 23, fill = "white", colour = "black", size = 2.6) +
    scale_colour_manual(values = c("Significativa tras Holm" = "#B2182B", "Solo sin ajuste" = "#D95F02", "No significativa" = col_centro),
                        name = NULL, drop = FALSE) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5), labels = fmt_eje) +
    labs(title = "Correlación de cada variable con la inseguridad percibida",
         subtitle = "Pearson con IC 95 %. Barra gris: rango al excluir cada centro. Rombo: sin Loja",
         x = "Coeficiente de correlación de Pearson (r)", y = NULL,
         caption = str_wrap(paste0("Entre paréntesis, signo esperado. Líneas punteadas: |r| mínimo significativo con N = ", n,
                          " (", fmt_n(rc), "). Holm sobre ocho pruebas. ", pie_fuente), 105)) +
    tema_inseg() + theme(panel.grid.major.y = element_blank())
}

# ---- Figura: dispersión de la inseguridad frente a cada variable -----------------
g_dispersion <- function(d, con_dep, etiquetas) {
  expl <- con_dep$var
  largo <- d |> select(cpl_corto, es_loja, n_resp, p_inseguro, all_of(expl)) |>
    pivot_longer(all_of(expl), names_to = "var", values_to = "x") |>
    mutate(variable = factor(etiquetas[var], levels = etiquetas[expl]))
  lab <- con_dep |> mutate(variable = factor(etiquetas[var], levels = etiquetas[expl]),
                           txt = paste0("ρ = ", fmt_r(rho), " | r = ", fmt_r(r)))
  ggplot(largo, aes(x, p_inseguro)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "grey35", linewidth = 0.6) +
    geom_point(aes(size = n_resp, colour = es_loja), alpha = 0.8) +
    geom_text(data = lab, aes(x = -Inf, y = Inf, label = txt), hjust = -0.05, vjust = 1.4, size = 3, inherit.aes = FALSE) +
    scale_colour_manual(values = c(`FALSE` = "grey50", `TRUE` = col_loja), guide = "none") +
    scale_size_area(max_size = 4.5, guide = "none") +
    scale_y_continuous(labels = percent_format(decimal.mark = ",", accuracy = 1), expand = expansion(mult = c(0.05, 0.15))) +
    facet_wrap(~ variable, scales = "free_x", ncol = 4) +
    labs(title = "Inseguridad percibida frente a cada variable explicativa",
         subtitle = "Cada punto es un centro; el tamaño indica el número de respondientes",
         x = NULL, y = "Proporción que se siente insegura",
         caption = paste0("Punto rojo: CPL Loja N.° 1. ", pie_fuente)) +
    tema_inseg(10)
}

# ---- Figura: matriz de correlaciones (Pearson abajo, Spearman arriba) ------------
g_matriz <- function(d, vars, etiquetas, cor_res) {
  n_v <- length(vars)
  M <- cor_res$matriz
  g <- expand_grid(i = seq_len(n_v), j = seq_len(n_v)) |>
    rowwise() |>
    mutate(r = {
      if (i == j) NA_real_ else {
        v1 <- vars[min(i, j)]; v2 <- vars[max(i, j)]
        k <- which(M$v1 == v1 & M$v2 == v2)
        if (i > j) M$pearson_r[k] else M$spearman_r[k]
      }},
      p = {
        if (i == j) NA_real_ else {
          v1 <- vars[min(i, j)]; v2 <- vars[max(i, j)]
          k <- which(M$v1 == v1 & M$v2 == v2)
          if (i > j) M$pearson_p[k] else M$spearman_p[k]
        }}) |>
    ungroup() |>
    mutate(txt = ifelse(is.na(r), "", paste0(fmt_r(r), estrellas(p))),
           fila = factor(etiquetas[vars[i]], levels = rev(unname(etiquetas))),
           col  = factor(etiquetas[vars[j]], levels = unname(etiquetas)),
           claro = !is.na(r) & abs(r) > 0.6)
  ggplot(g, aes(col, fila, fill = r)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = txt, colour = claro), size = 3.1, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "white")) +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", limits = c(-1, 1),
                         na.value = "grey92", name = "r", labels = fmt_eje) +
    labs(title = paste0("Matriz de correlaciones entre centros (N = ", nrow(d), ")"),
         subtitle = "Pearson bajo la diagonal; Spearman sobre la diagonal",
         x = NULL, y = NULL,
         caption = paste0("* p < ,05; ** p < ,01; *** p < ,001, sin ajuste por comparaciones múltiples. ", pie_fuente)) +
    tema_inseg() + theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank(),
                         legend.position = "right")
}
