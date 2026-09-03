# exportResults.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Capa de salida. Crea los directorios de resultados, normaliza los
# nombres de archivo y guarda tablas (CSV) y figuras (PNG) con nombres
# trazables y coherentes con el Word (p.ej. tabla_3_1_indices.csv,
# figura_3_4_codo_silueta.png).

# ---------------------------------------------------------------------
# asegurarDirectorios()
# Crea, si no existen, los directorios de datos y de resultados.
# ---------------------------------------------------------------------
asegurarDirectorios <- function() {
  for (d in c(RUTA_BASEDATA, RUTA_DATOS, RUTA_PERFILES, RUTA_TABLAS, RUTA_FIGURAS)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# nombreArchivoSeguro()
# Normaliza un nombre de archivo (minusculas, sin espacios ni acentos).
# ---------------------------------------------------------------------
nombreArchivoSeguro <- function(nombre) {
  nombre <- tolower(nombre)
  nombre <- chartr("aeiouñ", "aeioun", nombre)
  nombre <- gsub("[^a-z0-9._-]+", "_", nombre)
  nombre
}

# ---------------------------------------------------------------------
# guardarTabla()
# Guarda un data.frame como CSV en resultados/tablas con nombre trazable.
# Entradas: x (data.frame), nombre (sin extension), ruta
# Salida:   ruta del archivo escrito (invisible)
# ---------------------------------------------------------------------
guardarTabla <- function(x, nombre, ruta = RUTA_TABLAS) {
  if (!dir.exists(ruta)) dir.create(ruta, recursive = TRUE)
  archivo <- file.path(ruta, paste0(nombreArchivoSeguro(nombre), ".csv"))
  write.csv(x, archivo, row.names = FALSE)
  message("  tabla guardada: ", archivo)
  invisible(archivo)
}

# ---------------------------------------------------------------------
# guardarFigura()
# Guarda un objeto ggplot como PNG en resultados/figuras.
# Entradas: p (ggplot), nombre (sin extension), ancho, alto, ruta
# Salida:   ruta del archivo escrito (invisible)
# ---------------------------------------------------------------------
guardarFigura <- function(p, nombre, ancho = 8, alto = 5, ruta = RUTA_FIGURAS) {
  if (!dir.exists(ruta)) dir.create(ruta, recursive = TRUE)
  archivo <- file.path(ruta, paste0(nombreArchivoSeguro(nombre), ".png"))
  ggplot2::ggsave(archivo, plot = p, width = ancho, height = alto, dpi = 150)
  message("  figura guardada: ", archivo)
  invisible(archivo)
}
