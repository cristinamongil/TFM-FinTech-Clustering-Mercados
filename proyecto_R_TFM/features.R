# features.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Variables que describen el comportamiento de cada indice y que se
# emplean como entradas del clustering (Word 3.5, Tabla 3.4):
#   - rentabilidad anualizada
#   - volatilidad realizada anualizada (ventana movil)
#   - drawdown maximo
#   - correlacion media movil con el resto de mercados
# Ademas resume estas variables dentro de cada fase, construye el conjunto
# por indice-crisis-fase, y lo estandariza (z-score) dentro de cada
# conjunto que se agrupa. Incluye comprobaciones para evitar valores no
# finitos y escalas degeneradas.

# ---------------------------------------------------------------------
# indicesVentanaMovil()
# Para cada fecha del panel devuelve las posiciones de las observaciones
# comprendidas en los tres meses naturales que terminan en esa fecha
# (ventana retrospectiva: solo informacion anterior o igual a t). Como el
# panel recoge las fechas comunes de los diecinueve mercados, el numero de
# observaciones de cada ventana puede variar.
# Entradas: fechas (vector Date del panel), meses (config)
# Salida:   lista con las posiciones de cada ventana
# ---------------------------------------------------------------------
indicesVentanaMovil <- function(fechas, meses = MESES_VENTANA_MOVIL) {
  fechas <- as.Date(fechas)
  lapply(seq_along(fechas), function(t) {
    inicio <- seq(fechas[t], by = paste0("-", meses, " months"), length.out = 2)[2]
    which(fechas >= inicio & fechas <= fechas[t])
  })
}

# ---------------------------------------------------------------------
# tamanoVentanaMovil()
# Diagnostico descriptivo del numero de fechas comunes que contienen las
# ventanas de tres meses. Se calcula a partir de la primera fecha con
# ventana completa, para no contar las ventanas truncadas del arranque de
# la muestra.
# Entradas: fechas (vector Date del panel), meses (config)
# Salida:   data.frame estadistico, n_fechas_comunes
# ---------------------------------------------------------------------
tamanoVentanaMovil <- function(fechas, meses = MESES_VENTANA_MOVIL) {
  fechas <- as.Date(fechas)
  desde  <- seq(min(fechas), by = paste0(meses, " months"), length.out = 2)[2]
  n <- vapply(indicesVentanaMovil(fechas, meses), length, integer(1))[fechas >= desde]
  data.frame(
    estadistico = c("media", "mediana", "minimo", "p25", "p75", "maximo"),
    n_fechas_comunes = c(round(mean(n), 2), stats::median(n), min(n),
                         as.numeric(stats::quantile(n, 0.25)),
                         as.numeric(stats::quantile(n, 0.75)), max(n)),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------
# volatilidadRealizada()
# Desviacion tipica de los rendimientos dentro de la ventana movil de tres
# meses que termina en cada fecha, anualizada. Devuelve NA cuando la
# ventana no alcanza el minimo de observaciones exigido.
# Entradas: rendimientos (vector), ventanas_idx (indicesVentanaMovil),
#           factor_anual y min_obs (config)
# Salida:   vector de volatilidad movil anualizada
# ---------------------------------------------------------------------
volatilidadRealizada <- function(rendimientos, ventanas_idx,
                                 factor_anual = FACTOR_ANUAL,
                                 min_obs = MIN_OBS_VENTANA) {
  n <- length(rendimientos)
  sd_movil <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    ii <- ventanas_idx[[t]]
    if (length(ii) >= min_obs) {
      sd_movil[t] <- stats::sd(rendimientos[ii], na.rm = TRUE)
    }
  }
  sd_movil * sqrt(factor_anual)
}

# ---------------------------------------------------------------------
# caidaMaxima()
# Drawdown maximo del tramo: mayor caida desde un maximo previo dentro de
# la fase. Devuelve un valor negativo o cero (p.ej. -0.35).
# Entradas: precios
# ---------------------------------------------------------------------
caidaMaxima <- function(precios) {
  precios <- as.numeric(precios)
  precios <- precios[is.finite(precios)]
  if (length(precios) < 2) return(NA_real_)
  min(precios / cummax(precios) - 1)
}

# ---------------------------------------------------------------------
# correlacionMediaMovil()
# Para cada fecha, correlacion media por pares de cada indice con el resto
# dentro de la ventana movil de tres meses que termina en esa fecha. Todos
# los mercados emplean exactamente las mismas fechas en cada ventana,
# porque el panel esta formado por fechas comunes de negociacion.
# Entradas: panel_rend (matriz fechas x indices), ventanas_idx, min_obs
# Salida:   matriz fechas x indices con la correlacion media de cada indice
# ---------------------------------------------------------------------
correlacionMediaMovil <- function(panel_rend, ventanas_idx,
                                  min_obs = MIN_OBS_VENTANA) {
  panel_rend <- as.matrix(panel_rend)
  n <- nrow(panel_rend); k <- ncol(panel_rend)
  salida <- matrix(NA_real_, nrow = n, ncol = k,
                   dimnames = list(NULL, colnames(panel_rend)))
  if (k < 2) return(salida)
  for (t in seq_len(n)) {
    ii <- ventanas_idx[[t]]
    if (length(ii) < min_obs) next
    bloque <- panel_rend[ii, , drop = FALSE]
    m <- suppressWarnings(stats::cor(bloque))
    diag(m) <- NA
    salida[t, ] <- colMeans(m, na.rm = TRUE)
  }
  salida
}

# ---------------------------------------------------------------------
# resumenFase()
# Resume una serie movil dentro de una fase por su media (config).
# Ignora los valores no finitos.
# ---------------------------------------------------------------------
resumenFase <- function(valores, metodo = RESUMEN_VENTANA) {
  valores <- valores[is.finite(valores)]
  if (length(valores) == 0) return(NA_real_)
  if (metodo == "media") mean(valores) else max(valores)
}

# ---------------------------------------------------------------------
# calcularVariablesComportamiento()
# Para cada indice, crisis y fase calcula las cuatro variables. La
# rentabilidad anualizada y el drawdown se calculan sobre la fase completa;
# la volatilidad y la correlacion se calculan sobre ventana movil fija en
# toda la muestra y se resumen por su media dentro de la fase.
# Entradas: panel_precios (Fecha + indices), ventanas (definirVentanas)
# Salida:   data.frame indice, crisis, fase + las cuatro variables
# ---------------------------------------------------------------------
calcularVariablesComportamiento <- function(panel_precios, ventanas) {
  fechas  <- panel_precios$Fecha
  indices <- setdiff(names(panel_precios), "Fecha")
  precios <- as.matrix(panel_precios[, indices, drop = FALSE])

  # Rendimientos logaritmicos (primera fila NA para conservar la dimension).
  rend <- rbind(NA, apply(precios, 2, function(col) diff(log(col))))

  # Series moviles calculadas una sola vez sobre toda la muestra con la
  # ventana retrospectiva de tres meses naturales.
  ventanas_idx <- indicesVentanaMovil(fechas)
  vol_movil  <- apply(rend, 2, function(r) volatilidadRealizada(r, ventanas_idx))
  corr_movil <- correlacionMediaMovil(rend, ventanas_idx)

  filas <- list()
  for (i in seq_len(nrow(ventanas))) {
    v <- ventanas[i, ]
    en_fase <- fechas >= v$fecha_inicio & fechas <= v$fecha_fin
    for (idx in indices) {
      p_fase <- precios[en_fase, idx]
      r_fase <- rendimientosLog(p_fase)
      filas[[length(filas) + 1]] <- data.frame(
        indice            = idx,
        crisis            = v$crisis,
        fase              = v$fase,
        rent_anual        = rendimientoAnualizado(r_fase),
        volatilidad       = resumenFase(vol_movil[en_fase, idx]),
        drawdown          = caidaMaxima(p_fase),
        correlacion_media = resumenFase(corr_movil[en_fase, idx]),
        stringsAsFactors  = FALSE)
    }
  }
  do.call(rbind, filas)
}

# ---------------------------------------------------------------------
# estandarizarVariables()
# Estandariza (z-score) las variables DENTRO de cada conjunto que se
# agrupa (por crisis y fase), para que todas pesen por igual. Comprueba
# que ninguna columna tenga escala degenerada (varianza nula) ni valores
# no finitos: si una columna es constante, su z-score se fija a 0 en lugar
# de generar NaN.
# ---------------------------------------------------------------------
estandarizarVariables <- function(variables_df, columnas = COLS_VARIABLES) {
  partes <- split(variables_df, list(variables_df$crisis, variables_df$fase), drop = TRUE)
  estandar <- lapply(partes, function(bloque) {
    for (col in columnas) {
      x <- bloque[[col]]
      mu <- mean(x, na.rm = TRUE)
      s  <- stats::sd(x, na.rm = TRUE)
      if (is.na(s) || s == 0) {
        bloque[[col]] <- rep(0, length(x))   # escala degenerada -> sin aportacion
      } else {
        bloque[[col]] <- (x - mu) / s
      }
    }
    bloque
  })
  res <- do.call(rbind, estandar)
  rownames(res) <- NULL
  res
}

# ---------------------------------------------------------------------
# matrizCluster()
# Extrae, para un subconjunto ya estandarizado (una crisis y una fase), la
# matriz numerica de variables con los indices como nombres de fila, lista
# para kmeans y silhouette. Elimina filas con valores no finitos.
# Entradas: variables_std, crisis, fase, columnas
# Salida:   matriz indices x variables
# ---------------------------------------------------------------------
matrizCluster <- function(variables_std, crisis, fase, columnas = COLS_VARIABLES) {
  bloque <- variables_std[variables_std$crisis == crisis & variables_std$fase == fase, ]
  bloque <- bloque[stats::complete.cases(bloque[, columnas]), ]
  m <- as.matrix(bloque[, columnas])
  rownames(m) <- bloque$indice
  m[apply(m, 1, function(fila) all(is.finite(fila))), , drop = FALSE]
}
