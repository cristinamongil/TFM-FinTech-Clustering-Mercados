# financialFuns.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Funciones financieras generales y reutilizables: rentabilidades
# logaritmicas, rentabilidad acumulada y anualizada, y precios
# normalizados. Base de todas las variables de comportamiento. No dependen
# de objetos globales creados en la consola; validan sus entradas.

# ---------------------------------------------------------------------
# rendimientosLog()
# Rentabilidad logaritmica diaria de una serie de precios.
# Entradas: precios (vector numerico ordenado por fecha)
# Salida:   vector de rendimientos de longitud n-1 (vacio si n < 2)
# ---------------------------------------------------------------------
rendimientosLog <- function(precios) {
  precios <- as.numeric(precios)
  if (length(precios) < 2) return(numeric(0))
  diff(log(precios))
}

# ---------------------------------------------------------------------
# rendimientoAcumulado()
# Rentabilidad total acumulada del tramo (precio final frente a inicial).
# Entradas: precios
# Salida:   escalar (NA si hay menos de dos precios validos)
# ---------------------------------------------------------------------
rendimientoAcumulado <- function(precios) {
  precios <- as.numeric(precios)
  precios <- precios[is.finite(precios)]
  if (length(precios) < 2) return(NA_real_)
  precios[length(precios)] / precios[1] - 1
}

# ---------------------------------------------------------------------
# rendimientoAnualizado()
# Rentabilidad anualizada a partir de la rentabilidad media diaria del
# tramo (Word, Tabla 3.4). Robusta a la longitud de la fase.
# Entradas: rendimientos (log diarios), factor_anual (config)
# Salida:   escalar (NA si no hay rendimientos validos)
# ---------------------------------------------------------------------
rendimientoAnualizado <- function(rendimientos, factor_anual = FACTOR_ANUAL) {
  rendimientos <- rendimientos[is.finite(rendimientos)]
  if (length(rendimientos) == 0) return(NA_real_)
  mean(rendimientos) * factor_anual
}

# ---------------------------------------------------------------------
# precioRelativo()
# Precio normalizado a una base comun (100 por defecto) en la primera
# observacion valida. Sirve para comparar series y para construir la serie
# de referencia equiponderada.
# Entradas: precios, base
# Salida:   vector normalizado (misma longitud que precios)
# ---------------------------------------------------------------------
precioRelativo <- function(precios, base = 100) {
  precios <- as.numeric(precios)
  primero <- precios[which(is.finite(precios))[1]]
  if (is.na(primero) || primero == 0) return(rep(NA_real_, length(precios)))
  precios / primero * base
}
