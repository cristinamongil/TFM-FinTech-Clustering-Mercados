# init.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Punto de entrada del proyecto. Carga las librerias necesarias
# (instalando solo las que falten), carga la configuracion y hace
# source() de los modulos de funciones. No ejecuta el analisis: deja el
# entorno preparado para main_TFM.R.

# --- Comprobacion e instalacion controlada de paquetes --------------
# Solo se instala lo que falte y se avisa por consola. El paquete
# "cluster" viene con R base y no requiere instalacion.
.paquetes <- c("quantmod", "zoo", "cluster", "ggplot2", "reshape2")
.faltan <- .paquetes[!vapply(.paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(.faltan) > 0) {
  message("Instalando paquetes que faltan: ", paste(.faltan, collapse = ", "))
  install.packages(.faltan, repos = "https://cloud.r-project.org")
}
rm(.paquetes, .faltan)

suppressPackageStartupMessages({
  library(quantmod)   # descarga desde Yahoo Finance (metodo actual)
  library(zoo)        # relleno de huecos y utilidades de series
  library(cluster)    # coeficiente de silueta
  library(ggplot2)    # figuras
  library(reshape2)   # formato largo para ggplot2
})

# --- Configuracion global -------------------------------------------
source("config.R")

# --- Modulos de funciones -------------------------------------------
# Nucleo (siguiendo la filosofia del proyecto de clase).
source("ExtraccionDatos.R")   # descarga, cache, limpieza, alineacion y disponibilidad
source("financialFuns.R")     # rentabilidades y precios relativos
source("features.R")          # variables de comportamiento y estandarizacion

# Modulos auxiliares, uno por bloque metodologico del capitulo 3.
source("crisisWindows.R")     # 3.3  definicion de ventanas
source("clustering.R")        # 3.8  seleccion de k y K-Means
source("hypothesisContrast.R")# 3.9  contraste de hipotesis
source("structuralProfile.R")   # 3.10 perfil estructural externo y caracterizacion
source("complementary.R")     # 3.11 analisis complementarios
source("plots.R")             # figuras
source("exportResults.R")     # guardado trazable de tablas y figuras

# --- Directorios de salida ------------------------------------------
asegurarDirectorios()

message("init.R cargado: entorno preparado para ejecutar main_TFM.R.")
