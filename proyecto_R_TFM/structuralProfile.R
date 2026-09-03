# structuralProfile.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Perfil estructural externo y caracterizacion de los grupos (memoria 3.10).
# El perfil recoge dos variables estructurales de cada mercado, el nivel de
# desarrollo y la region geografica, que NO intervienen en el agrupamiento y
# sirven para caracterizar los grupos sin circularidad. Son datos EXTERNOS
# (Tabla 3.6) tomados de fuentes oficiales y gratuitas: la clasificacion de
# mercados de FTSE Russell y el esquema M49 de Naciones Unidas. Se
# cumplimentan en profiles/perfil_estructural.csv. Si el archivo no esta
# disponible, las funciones lo comunican y el analisis continua con la parte
# que no depende de el.

# Columnas esperadas del perfil (ademas de 'indice').
COLUMNAS_PERFIL <- c("desarrollo", "region")

# ---------------------------------------------------------------------
# plantillaPerfil()
# Devuelve una plantilla vacia del perfil con una fila por indice y las
# columnas esperadas sin cumplimentar. Sirve para generar el CSV que la
# autora debe rellenar con fuentes externas.
# ---------------------------------------------------------------------
plantillaPerfil <- function(info = INFO_INDICES) {
  plantilla <- data.frame(indice = info$ticker, nombre = info$nombre,
                          desarrollo = info$desarrollo, stringsAsFactors = FALSE)
  for (col in setdiff(COLUMNAS_PERFIL, "desarrollo")) plantilla[[col]] <- NA_character_
  plantilla
}

# ---------------------------------------------------------------------
# cargarPerfil()
# Lee el CSV del perfil desde profiles/. Devuelve NULL (con aviso) si el
# archivo no existe o esta vacio, y avisa si faltan columnas o si todos los
# valores sectoriales estan sin cumplimentar.
# Entradas: ruta
# Salida:   data.frame del perfil o NULL
# ---------------------------------------------------------------------
cargarPerfil <- function(ruta = file.path(RUTA_PERFILES, ARCHIVO_PERFIL)) {
  if (!file.exists(ruta)) {
    message("Perfil estructural no encontrado en ", ruta,
            ". La caracterizacion estructural de los grupos queda pendiente ",
            "hasta cumplimentarlo.")
    return(NULL)
  }
  perfil <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(perfil) || nrow(perfil) == 0 || !"indice" %in% names(perfil)) {
    message("El perfil estructural esta vacio o sin la columna 'indice'.")
    return(NULL)
  }
  faltan <- setdiff(COLUMNAS_PERFIL, names(perfil))
  if (length(faltan) > 0) {
    message("El perfil no contiene las columnas: ", paste(faltan, collapse = ", "))
  }
  presentes <- intersect(COLUMNAS_PERFIL, names(perfil))
  if (length(presentes) > 0 &&
      all(vapply(perfil[presentes], function(x) all(is.na(x) | x == ""), logical(1)))) {
    message("El perfil existe pero las variables estructurales estan sin cumplimentar.")
  }
  perfil
}

# ---------------------------------------------------------------------
# perfilDisponible()
# Comprueba si el perfil contiene datos utilizables en una variable
# estructural (al menos dos valores cumplimentados).
# ---------------------------------------------------------------------
perfilDisponible <- function(perfil, variable = "desarrollo") {
  if (is.null(perfil) || !variable %in% names(perfil)) return(FALSE)
  sum(!is.na(perfil[[variable]]) & perfil[[variable]] != "") >= 2
}

# ---------------------------------------------------------------------
# cruzarPerfilClusters()
# Cruza las etiquetas de cluster de una crisis con el perfil estructural:
# por cluster, resume el numero de indices y el reparto por nivel de
# desarrollo.
# Entradas: etiquetas (vector nombrado por indice), perfil
# Salida:   data.frame por cluster o NULL si no hay perfil
# ---------------------------------------------------------------------
cruzarPerfilClusters <- function(etiquetas, perfil) {
  if (is.null(perfil)) return(NULL)
  df <- data.frame(indice = names(etiquetas), cluster = as.integer(etiquetas),
                   stringsAsFactors = FALSE)
  df <- merge(df, perfil, by = "indice", all.x = TRUE)
  agregado <- lapply(split(df, df$cluster), function(g) {
    data.frame(
      cluster        = g$cluster[1],
      n_indices      = nrow(g),
      n_desarrollado = sum(g$desarrollo == "Desarrollado", na.rm = TRUE),
      n_emergente    = sum(g$desarrollo == "Emergente", na.rm = TRUE),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, agregado)
}

# ---------------------------------------------------------------------
# composicionEstructural()
# Tabla de contingencia entre los grupos de una crisis y una variable
# estructural del perfil (desarrollo o region). Devuelve los recuentos por
# grupo y categoria, con el total de cada grupo, en formato largo para
# facilitar tanto su exportacion como su representacion grafica.
# Entradas: etiquetas (vector nombrado por indice), perfil, variable
# Salida:   data.frame (cluster, categoria, n, total_cluster) o NULL
# ---------------------------------------------------------------------
composicionEstructural <- function(etiquetas, perfil, variable = "desarrollo") {
  if (is.null(perfil) || !variable %in% names(perfil)) return(NULL)
  df <- data.frame(indice = names(etiquetas), cluster = as.integer(etiquetas),
                   stringsAsFactors = FALSE)
  df <- merge(df, perfil[, c("indice", variable)], by = "indice", all.x = TRUE)
  df <- df[!is.na(df[[variable]]) & df[[variable]] != "", ]
  if (nrow(df) == 0) return(NULL)
  tabla <- as.data.frame(table(cluster = df$cluster, categoria = df[[variable]]),
                         stringsAsFactors = FALSE)
  names(tabla)[names(tabla) == "Freq"] <- "n"
  tabla$cluster <- as.integer(as.character(tabla$cluster))
  totales <- tapply(tabla$n, tabla$cluster, sum)
  tabla$total_cluster <- as.integer(totales[as.character(tabla$cluster)])
  tabla <- tabla[order(tabla$cluster, tabla$categoria), ]
  rownames(tabla) <- NULL
  tabla
}
