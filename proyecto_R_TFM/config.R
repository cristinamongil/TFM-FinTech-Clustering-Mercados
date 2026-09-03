# config.R
# TFM - Analitica de datos en series temporales financieras
# Comportamiento de los mercados financieros ante crisis de distinta naturaleza
#
# Autora: Cristina Mongil de la Cal
#
# Parametros globales del estudio. Todas las decisiones metodologicas
# cerradas se centralizan aqui. Este archivo NO ejecuta calculos: solo
# declara valores. La correspondencia con el Word (capitulo 3) se indica
# en cada bloque.

# --- Rutas del proyecto (relativas a la raiz del .Rproj) -------------
RUTA_BASEDATA <- "basedata"            # datos crudos descargados de Yahoo (un CSV por ticker)
RUTA_DATOS    <- "data"                # objetos intermedios procesados (.rds)
RUTA_PERFILES <- "profiles"            # perfil estructural externo (CSV cumplimentado a mano)
RUTA_TABLAS   <- "resultados/tablas"   # tablas de salida
RUTA_FIGURAS  <- "resultados/figuras"  # figuras de salida

# Nombre del CSV de perfil estructural externo (Tabla 3.6). Es un dato
# EXTERNO que no se descarga de Yahoo: nivel de desarrollo (FTSE Russell)
# y region geografica (esquema M49 de Naciones Unidas).
ARCHIVO_PERFIL <- "perfil_estructural.csv"

# --- Universo de analisis: 19 indices bursatiles (Tabla 3.1) --------
# Se toman en moneda local. El orden define el orden en el panel.
TICKERS_INDICES <- c(
  "^GSPC", "^GSPTSE",
  "^GDAXI", "^FCHI", "^FTSE", "^IBEX", "FTSEMIB.MI", "^SSMI", "^AEX",
  "^N225", "^HSI", "^KS11", "^AXJO",
  "^BVSP", "^BSESN", "^MXX", "000001.SS", "^TWII", "^JKSE"
)

# Nombres descriptivos, pais/mercado y nivel de desarrollo (Tabla 3.1).
# Se usan para etiquetar tablas y figuras y para el perfil interpretativo.
INFO_INDICES <- data.frame(
  ticker = TICKERS_INDICES,
  nombre = c(
    "S&P 500", "S&P/TSX",
    "DAX", "CAC 40", "FTSE 100", "IBEX 35", "FTSE MIB", "SMI", "AEX",
    "Nikkei 225", "Hang Seng", "KOSPI", "S&P/ASX 200",
    "Bovespa", "Sensex", "IPC", "Shanghai Composite", "TAIEX", "Yakarta Composite"
  ),
  pais = c(
    "EE. UU.", "Canada",
    "Alemania", "Francia", "Reino Unido", "Espana", "Italia", "Suiza", "Paises Bajos",
    "Japon", "Hong Kong", "Corea del Sur", "Australia",
    "Brasil", "India", "Mexico", "China", "Taiwan", "Indonesia"
  ),
  bloque = c(
    "America", "America",
    "Europa", "Europa", "Europa", "Europa", "Europa", "Europa", "Europa",
    "Asia-Pacifico", "Asia-Pacifico", "Asia-Pacifico", "Asia-Pacifico",
    "Emergente", "Emergente", "Emergente", "Emergente", "Emergente", "Emergente"
  ),
  desarrollo = c(
    "Desarrollado", "Desarrollado",
    "Desarrollado", "Desarrollado", "Desarrollado", "Desarrollado", "Desarrollado",
    "Desarrollado", "Desarrollado",
    "Desarrollado", "Desarrollado", "Desarrollado", "Desarrollado",
    "Emergente", "Emergente", "Emergente", "Emergente", "Emergente", "Emergente"
  ),
  stringsAsFactors = FALSE
)

# Nota metodologica (Word 3.2 y observacion de la autora): el ticker del
# DAX (^GDAXI) y el del Bovespa (^BVSP) corresponden a indices de RETORNO
# TOTAL; los otros 17 son indices de PRECIOS. De momento se conserva la
# especificacion original. Cambiar cualquier ticker se hace SOLO aqui, en
# TICKERS_INDICES e INFO_INDICES, sin tocar el resto del codigo.

# --- Activos de contexto (Tabla 3.2; fuera del clustering) ----------
# Solo intervienen en la interpretacion economica, no en el agrupamiento.
TICKERS_CONTEXTO     <- c("CL=F", "GC=F", "TLT")
INFO_CONTEXTO <- data.frame(
  ticker = TICKERS_CONTEXTO,
  nombre = c("Petroleo (WTI)", "Oro", "Deuda EE. UU. (TLT)"),
  papel  = c("Precio de la energia (crisis 2022)",
             "Activo refugio ante incertidumbre",
             "Refugio de renta fija (2008 y COVID-19)"),
  stringsAsFactors = FALSE
)
ANCLAS_EN_CLUSTERING <- FALSE           # los activos de contexto NO se agrupan

# Regla de conjunto comun (Word 3.2): si un indice no cubre 2008 se retira
# del estudio COMPLETO, no solo de esa crisis. Atane en particular al
# indice italiano, cuya cobertura debe comprobarse.
FECHA_MINIMA_COMUN <- "2008-01-01"
TICKER_EN_RIESGO   <- "FTSEMIB.MI"

# Si algun indice no cubre 2008, excluirlo altera la muestra de 19 indices
# definida en el Word. Con FALSE, el analisis se DETIENE y pide una
# decision expresa; con TRUE, retira los indices afectados y lo documenta.
PERMITIR_EXCLUSION_AUTOMATICA <- FALSE

# --- Fechas de descarga (Word 3.2: 2007-2023) -----------------------
DESCARGA_INICIO <- "2007-01-01"
DESCARGA_FIN    <- "2023-12-31"

# --- Crisis y ventanas temporales (Word 3.3; Tabla 3.3) -------------
# Cada crisis se divide en tres fases: antes, durante (aguda) y despues.
# "durante" = del maximo previo (pico) al minimo (valle) sobre la serie de
# referencia. "antes"/"despues" = 6 meses. Los intervalos de busqueda del
# pico y del valle son amplios; las fechas exactas las fija el analisis.
MESES_ANTES   <- 6L
MESES_DESPUES <- 6L

CRISIS <- list(
  crisis2008 = list(
    etiqueta       = "Crisis financiera global (2008)",
    busqueda_pico  = c("2007-06-01", "2007-12-31"),
    busqueda_valle = c("2008-09-01", "2009-06-30")
  ),
  covid = list(
    etiqueta       = "Pandemia COVID-19 (2020)",
    busqueda_pico  = c("2020-01-01", "2020-02-29"),
    busqueda_valle = c("2020-03-01", "2020-04-30")
  ),
  crisis2022 = list(
    etiqueta       = "Crisis geopolitica-energetica-inflacionaria (2022)",
    busqueda_pico  = c("2021-12-01", "2022-02-28"),
    busqueda_valle = c("2022-09-01", "2022-11-30")
  )
)

# Serie de referencia para datar picos y valles (Word 3.3):
# media equiponderada de los indices en precios normalizados a base 100.
METODO_REFERENCIA <- "media_equiponderada_normalizada"

# --- Variables de comportamiento (Word 3.5; Tabla 3.4) --------------
# Cuatro variables por indice, crisis y fase: rentabilidad anualizada,
# volatilidad realizada anualizada, drawdown maximo y correlacion media.
MESES_VENTANA_MOVIL <- 3L   # ventana movil retrospectiva de tres meses naturales
MIN_OBS_VENTANA     <- 10L  # minimo de fechas comunes exigido dentro de una ventana
RESUMEN_VENTANA <- "media"  # resumen de las series moviles dentro de cada fase
FACTOR_ANUAL    <- 252L     # dias de negociacion para anualizar
COLS_VARIABLES  <- c("rent_anual", "volatilidad", "drawdown", "correlacion_media")

# --- Clustering y seleccion de k (Word 3.8) -------------------------
RANGO_K           <- 2:8            # valores de k explorados por codo y silueta
K_COMUN           <- NA_integer_    # se CALCULA en main_TFM.R (bloque 10); no se fija a mano
SEMILLA           <- 123L           # reproducibilidad
KMEANS_NREINICIOS <- 50L            # nstart de kmeans (multiples reinicios)

# Regla para proponer el k comun a partir del codo y la silueta:
# se elige el k que maximiza la silueta media promediada entre las tres
# crisis dentro de RANGO_K. main_TFM.R lo propone y la autora lo confirma
# a la vista de la Figura 3.4.
CRITERIO_K_COMUN <- "silueta_media_maxima"

# --- Contraste de hipotesis (memoria 3.9) ---------------------------
# 2008 y 2022 se evaluan de forma descriptiva, observando la composicion
# estructural de cada grupo; la COVID, por separabilidad. El Rand ajustado
# se reserva para comparar particiones ENTRE crisis (analisis 3.11).
UMBRAL_SILUETA_BAJA <- 0.25  # silueta media por debajo: baja separabilidad (COVID)

# --- Analisis complementarios (Word 3.11) ---------------------------
# Reaccion y recuperacion (descriptivo): umbral de caida y porcentaje de
# recuperacion sobre el drawdown maximo de cada mercado.
UMBRAL_CAIDA        <- -0.10   # -10 % respecto al maximo previo
PCT_RECUPERACION    <- 0.50    # se considera recuperado al recobrar el 50 % de la caida
