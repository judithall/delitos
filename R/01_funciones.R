# =============================================================================
# 01_funciones.R  —  Funciones estadísticas y de formato (correlaciones)
# =============================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(tibble); library(purrr)})

# --- Formato en español: sin cero inicial en r, coma decimal, signo menos tipográfico
fmt_r <- function(r) {
  x <- sprintf("%.2f", r)
  x <- sub("^-0\\.00$", "0.00", x)
  x <- sub("^(-?)0\\.", "\\1,", x)
  sub("^-", "−", x)
}
fmt_p <- function(p) ifelse(p < 0.001, "< ,001", sub("^0\\.", ",", sprintf("%.3f", p)))
fmt_n <- function(x, k = 2) format(round(x, k), nsmall = k, decimal.mark = ",")
estrellas <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")

# --- Intervalo de Wilson para una proporción
wilson <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n; den <- 1 + z^2 / n
  centro <- (p + z^2 / (2 * n)) / den
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  tibble(lo = centro - hw, hi = centro + hw)
}

# --- Correlación de un par, con IC 95 %:
#     Pearson -> transformación z de Fisher (Fisher, 1921)
#     Spearman -> error estándar de Bonett y Wright (2000): sqrt((1 + rho^2/2)/(n - 3))
cor_par <- function(x, y, metodo = c("pearson", "spearman")) {
  metodo <- match.arg(metodo)
  ct <- suppressWarnings(cor.test(x, y, method = metodo, exact = FALSE))
  r  <- unname(ct$estimate); n <- length(x)
  se <- if (metodo == "spearman") sqrt((1 + r^2 / 2) / (n - 3)) else 1 / sqrt(n - 3)
  ic <- tanh(atanh(r) + c(-1, 1) * qnorm(0.975) * se)
  tibble(r = r, p = ct$p.value, lo = ic[1], hi = ic[2])
}

# --- Valor absoluto mínimo de r para ser significativo (bilateral, alfa = 0,05)
r_critico <- function(n, alfa = 0.05) { t <- qt(1 - alfa / 2, n - 2); t / sqrt(n - 2 + t^2) }

# --- Todas las correlaciones del análisis ----------------------------------
# Devuelve: $matriz (pares, Pearson y Spearman) y $con_dep (variable dependiente, con Holm,
# sensibilidad sin Loja y rango jackknife) y la decisión para la Hipótesis 3.
calcular_correlaciones <- function(d, vars, etiquetas, esperado, dep = "p_inseguro") {
  pares <- as_tibble(t(combn(vars, 2)), .name_repair = ~ c("v1", "v2"))
  matriz <- pares |>
    mutate(pearson  = map2(v1, v2, ~ cor_par(d[[.x]], d[[.y]], "pearson")),
           spearman = map2(v1, v2, ~ cor_par(d[[.x]], d[[.y]], "spearman"))) |>
    unnest(c(pearson, spearman), names_sep = "_")

  explicativas <- setdiff(vars, dep)
  con_dep <- map_dfr(explicativas, function(v) {
    p <- cor_par(d[[v]], d[[dep]], "pearson")
    s <- cor_par(d[[v]], d[[dep]], "spearman")
    r_jack <- map_dbl(seq_len(nrow(d)), ~ cor(d[[v]][-.x], d[[dep]][-.x]))
    tibble(var = v, variable = etiquetas[[v]],
           esperado = if (v %in% names(esperado)) esperado[[v]] else "—",
           r = p$r, lo = p$lo, hi = p$hi, p = p$p,
           rho = s$r, rho_lo = s$lo, rho_hi = s$hi, p_rho = s$p,
           r_sin_loja = cor(d[[v]][!d$es_loja], d[[dep]][!d$es_loja]),
           r_min = min(r_jack), r_max = max(r_jack))
  }) |>
    mutate(p_holm     = p.adjust(p, "holm"),
           p_holm_rho = p.adjust(p_rho, "holm"),
           signo_ok   = case_when(esperado == "—" ~ NA,
                                  esperado == "+" ~ r > 0, esperado == "−" ~ r < 0),
           decision   = case_when(p_holm < 0.05 ~ "Significativa (Holm)",
                                  p < 0.05 ~ "Solo sin ajuste", TRUE ~ "No significativa"),
           # Hipótesis 3: r > 0 y p ajustado por Holm < 0,05 (solo la variable Discriminación)
           h3 = if_else(var == "motivos_por_persona",
                        if_else(r > 0 & p_holm < 0.05, "Se rechaza H0", "No se rechaza H0"), NA_character_))
  list(matriz = matriz, con_dep = con_dep)
}

# --- Tabla de la matriz: Pearson bajo la diagonal, Spearman sobre la diagonal
tabla_matriz <- function(d, vars, etiquetas, cor_res) {
  n_v <- length(vars)
  M <- matrix("", n_v, n_v, dimnames = list(NULL, seq_len(n_v)))
  for (k in seq_len(nrow(cor_res$matriz))) {
    i <- match(cor_res$matriz$v1[k], vars); j <- match(cor_res$matriz$v2[k], vars)
    M[j, i] <- paste0(fmt_r(cor_res$matriz$pearson_r[k]),  estrellas(cor_res$matriz$pearson_p[k]))
    M[i, j] <- paste0(fmt_r(cor_res$matriz$spearman_r[k]), estrellas(cor_res$matriz$spearman_p[k]))
  }
  diag(M) <- "—"
  tibble(Variable = paste0(seq_len(n_v), ". ", unname(etiquetas)),
         M  = map_chr(vars, ~ fmt_n(mean(d[[.x]]))),
         DE = map_chr(vars, ~ fmt_n(sd(d[[.x]])))) |>
    bind_cols(as_tibble(M, .name_repair = "minimal"))
}
