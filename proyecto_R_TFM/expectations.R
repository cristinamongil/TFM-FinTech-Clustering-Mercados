# expectations.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Evaluacion de las expectativas (memoria 3.7 y 3.9). Las expectativas se
# formulan antes de observar los grupos y se evaluan de forma descriptiva,
# sin contraste estadistico formal: en la COVID-19 se observa la
# separabilidad de la particion (silueta media) y el reparto de los
# mercados entre los grupos; en 2008 y 2022, la composicion de cada grupo
# por nivel de desarrollo y region (structuralProfile.R). Este modulo
# contiene tambien el indice de Rand ajustado, que compara las particiones
# de crisis distintas (memoria 3.11).

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
# evaluarSeparabilidad()
# Resumen descriptivo de la expectativa de la COVID-19 (baja separacion
# entre los grupos): silueta media de la particion y tamano de cada grupo.
# No aplica umbrales: la valoracion se realiza en la memoria (4.4).
# Entradas: m (matriz estandarizada indices x variables), etiquetas
# Salida:   list(silueta_media, tamanos)
# ---------------------------------------------------------------------
evaluarSeparabilidad <- function(m, etiquetas) {
  list(silueta_media = siluetaMedia(m, etiquetas),
       tamanos = paste(sort(as.integer(table(etiquetas)), decreasing = TRUE), collapse = "-"))
}
