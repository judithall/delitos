formula_logit_fraccional <- p_inseguro ~ ln_tam + sin_visitas + talleres + procesados

estimar_logit_fraccional <- function(datos, ponderado = FALSE, tipo_vcov = "HC3") {
  tipo_vcov <- match.arg(tipo_vcov, c("HC0", "HC1", "HC2", "HC3"))
  requeridas <- c(all.vars(formula_logit_fraccional), if (ponderado) "n_resp")
  faltan <- setdiff(requeridas, names(datos))
  if (length(faltan)) {
    stop("Faltan variables para el modelo: ", paste(faltan, collapse = ", "), call. = FALSE)
  }
  if (!nrow(datos) || anyNA(datos[requeridas])) {
    stop("El modelo requiere observaciones completas y al menos un centro.", call. = FALSE)
  }
  if (any(datos$p_inseguro < 0 | datos$p_inseguro > 1)) {
    stop("La variable dependiente debe estar entre 0 y 1.", call. = FALSE)
  }
  if (ponderado && any(datos$n_resp <= 0)) {
    stop("Las ponderaciones n_resp deben ser positivas.", call. = FALSE)
  }

  formula_ajuste <- formula_logit_fraccional
  environment(formula_ajuste) <- environment()
  ponderacion <- if (ponderado) datos$n_resp else NULL
  ajuste <- stats::glm(
    formula_ajuste,
    data = datos,
    family = stats::quasibinomial(link = "logit"),
    weights = ponderacion
  )
  if (!isTRUE(ajuste$converged) || anyNA(stats::coef(ajuste))) {
    stop("El logit fraccional no convergió o produjo coeficientes no estimables.",
         call. = FALSE)
  }

  vcov_robusta <- sandwich::vcovHC(ajuste, type = tipo_vcov)
  if (any(!is.finite(vcov_robusta)) || any(diag(vcov_robusta) <= 0)) {
    stop("La matriz de covarianza robusta contiene valores no finitos o varianzas no positivas.",
         call. = FALSE)
  }
  estimaciones <- stats::coef(ajuste)
  errores <- sqrt(diag(vcov_robusta))
  grados_libertad <- stats::df.residual(ajuste)
  if (grados_libertad <= 0) {
    stop("No hay grados de libertad residuales para la inferencia.", call. = FALSE)
  }
  estadisticos <- estimaciones / errores
  critico <- stats::qt(0.975, df = grados_libertad)

  tabla <- data.frame(
    variable = names(estimaciones),
    estimacion = unname(estimaciones),
    ee_robusto = unname(errores),
    estadistico = unname(estadisticos),
    gl = grados_libertad,
    p = 2 * stats::pt(-abs(estadisticos), df = grados_libertad),
    ic_inferior = unname(estimaciones - critico * errores),
    ic_superior = unname(estimaciones + critico * errores),
    row.names = NULL
  )

  list(
    modelo = ajuste,
    vcov = vcov_robusta,
    resultados = tabla,
    ponderado = ponderado,
    tipo_vcov = tipo_vcov
  )
}

sensibilidad_leave_one_out <- function(datos) {
  especificaciones <- list(
    "Sin ponderar" = FALSE,
    "Ponderado por n_resp" = TRUE
  )
  salida <- lapply(names(especificaciones), function(nombre) {
    ponderado <- especificaciones[[nombre]]
    do.call(rbind, lapply(seq_len(nrow(datos)), function(i) {
      ajuste <- estimar_logit_fraccional(datos[-i, , drop = FALSE],
                                         ponderado = ponderado)
      cbind(
        modelo = nombre,
        centro_excluido = as.character(datos$cpl_corto[i]),
        ajuste$resultados,
        row.names = NULL
      )
    }))
  })
  do.call(rbind, salida)
}

diagnosticos_influencia <- function(modelos, datos) {
  do.call(rbind, lapply(names(modelos), function(nombre) {
    ajuste <- modelos[[nombre]]$modelo
    cook <- stats::cooks.distance(ajuste)
    leverage <- stats::hatvalues(ajuste)
    n <- stats::nobs(ajuste)
    k <- ajuste$rank
    data.frame(
      modelo = nombre,
      centro_max_cook = as.character(datos$cpl_corto[which.max(cook)]),
      cook_max = max(cook),
      cook_umbral_4_n = 4 / n,
      centros_cook_sobre_umbral = sum(cook > 4 / n),
      centro_max_leverage = as.character(datos$cpl_corto[which.max(leverage)]),
      leverage_max = max(leverage),
      leverage_umbral_2k_n = 2 * k / n,
      centros_leverage_sobre_umbral = sum(leverage > 2 * k / n),
      row.names = NULL
    )
  }))
}
