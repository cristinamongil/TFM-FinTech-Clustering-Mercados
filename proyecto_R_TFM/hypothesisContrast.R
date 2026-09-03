# hypothesisContrast.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Contraste de hipotesis (Word 3.9), la compuerta del diseno. Las crisis
# de 2008 y 2022 predicen un eje separador concreto y se contrastan por el
# acuerdo entre la particion obtenida y la esperada mediante el indice de
# Rand ajustado. La COVID-19 predice baja separabilidad y se contrasta con
# indicadores de separabilidad (silueta baja y correlaciones altas
# generalizadas). Las reglas de decision se fijan de antemano en config.R.

# ---------------------------------------------------------------------
# randAjustado()
# Indice de Rand ajustado entre dos particiones de un mismo conjunto,
# corrigiendo el acuerdo esperable por azar (Hubert y Arabie, 1985).
# Implementado en R base para no depender de paquetes externos.
# Entradas: a, b (vectores de etiquetas del mismo elemento y longitud)
# Salida:   escalar (1 = acuerdo total; ~0 = independencia)
# ---------------------------------------------------------------------
randAjustado <- function(a, b) {
  if (length(a) != length(b) || length(a) < 2) return(NA_real_)
  tabla <- table(a, b)
  suma_ij <- sum(choose(tabla, 2))
  a_i <- sum(choose(rowSums(tabla), 2))
  b_j <- sum(choose(colSums(tabla), 2))
  n2  <- choose(length(a), 2)
  esperado <- a_i * b_j / n2
  maximo   <- (a_i + b_j) / 2
  if (maximo - esperado == 0) return(0)
  (suma_ij - esperado) / (maximo - esperado)
}

# ---------------------------------------------------------------------
# contrastarSeparabilidad()
# Contraste de la hipotesis de la COVID-19: se espera baja separabilidad.
# Combina la silueta media de la particion con la correlacion media
# generalizada entre mercados en la fase.
# Entradas: m (matriz estandarizada indices x variables), etiquetas,
#           corr_media (correlacion media generalizada de la fase, opcional)
# Salida:   list(silueta_media, corr_media, veredicto)
# ---------------------------------------------------------------------
contrastarSeparabilidad <- function(m, etiquetas, corr_media = NA_real_,
                                    umbral_silueta = UMBRAL_SILUETA_BAJA) {
  sil <- siluetaMedia(m, etiquetas)
  veredicto <- if (!is.na(sil) && sil < umbral_silueta) {
    "baja separabilidad (hipotesis respaldada)"
  } else {
    "separabilidad apreciable (hipotesis no confirmada)"
  }
  list(silueta_media = sil, corr_media = corr_media, veredicto = veredicto)
}
