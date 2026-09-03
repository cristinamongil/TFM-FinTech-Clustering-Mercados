# crisisWindows.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Definicion de las crisis y sus ventanas temporales (Word 3.3). La fase
# aguda se delimita del maximo previo (pico) al minimo (valle) sobre una
# serie de referencia construida como la media equiponderada de los
# indices en precios normalizados. Las fases anterior y posterior son de
# seis meses.

# ---------------------------------------------------------------------
# construirReferencia()
# Serie de referencia: media equiponderada de los indices en precios
# normalizados a base 100. Sirve para datar picos y valles.
# Entradas: panel_precios (Fecha + columnas de indices)
# Salida:   data.frame Fecha, Referencia
# ---------------------------------------------------------------------
construirReferencia <- function(panel_precios) {
  fechas <- panel_precios$Fecha
  mat <- as.matrix(panel_precios[, setdiff(names(panel_precios), "Fecha"), drop = FALSE])
  norm <- apply(mat, 2, function(col) precioRelativo(col, base = 100))
  data.frame(Fecha = fechas, Referencia = rowMeans(norm, na.rm = TRUE))
}

# ---------------------------------------------------------------------
# picoAValle()
# Localiza la fecha del maximo (pico) y del minimo (valle) de la serie de
# referencia dentro de los intervalos de busqueda de una crisis.
# Entradas: referencia (Fecha, Referencia), busqueda_pico, busqueda_valle
# Salida:   list(fecha_pico, fecha_valle)
# ---------------------------------------------------------------------
picoAValle <- function(referencia, busqueda_pico, busqueda_valle) {
  sub_pico  <- referencia[referencia$Fecha >= as.Date(busqueda_pico[1]) &
                          referencia$Fecha <= as.Date(busqueda_pico[2]), ]
  sub_valle <- referencia[referencia$Fecha >= as.Date(busqueda_valle[1]) &
                          referencia$Fecha <= as.Date(busqueda_valle[2]), ]
  if (nrow(sub_pico) == 0 || nrow(sub_valle) == 0) {
    stop("No hay datos de la serie de referencia en los intervalos de busqueda de la crisis.")
  }
  list(fecha_pico  = sub_pico$Fecha[which.max(sub_pico$Referencia)],
       fecha_valle = sub_valle$Fecha[which.min(sub_valle$Referencia)])
}

# ---------------------------------------------------------------------
# definirVentanas()
# Fechas de las tres fases de cada crisis: antes (seis meses hasta el
# pico), durante (pico a valle) y despues (seis meses desde el valle).
# Entradas: referencia, crisis (config), meses_antes, meses_despues
# Salida:   data.frame crisis, fase, fecha_inicio, fecha_fin, fecha_pico,
#           fecha_valle
# ---------------------------------------------------------------------
definirVentanas <- function(referencia, crisis = CRISIS,
                            meses_antes = MESES_ANTES, meses_despues = MESES_DESPUES) {
  filas <- list()
  for (nombre in names(crisis)) {
    cr <- crisis[[nombre]]
    pv <- picoAValle(referencia, cr$busqueda_pico, cr$busqueda_valle)
    pico <- pv$fecha_pico; valle <- pv$fecha_valle
    inicio_antes  <- seq(pico,  by = paste0("-", meses_antes, " months"), length.out = 2)[2]
    fin_despues   <- seq(valle, by = paste0(meses_despues, " months"), length.out = 2)[2]
    filas[[nombre]] <- rbind(
      data.frame(crisis = nombre, fase = "antes",   fecha_inicio = inicio_antes, fecha_fin = pico,
                 fecha_pico = pico, fecha_valle = valle, stringsAsFactors = FALSE),
      data.frame(crisis = nombre, fase = "durante", fecha_inicio = pico, fecha_fin = valle,
                 fecha_pico = pico, fecha_valle = valle, stringsAsFactors = FALSE),
      data.frame(crisis = nombre, fase = "despues", fecha_inicio = valle, fecha_fin = fin_despues,
                 fecha_pico = pico, fecha_valle = valle, stringsAsFactors = FALSE)
    )
  }
  res <- do.call(rbind, filas)
  rownames(res) <- NULL
  res
}
