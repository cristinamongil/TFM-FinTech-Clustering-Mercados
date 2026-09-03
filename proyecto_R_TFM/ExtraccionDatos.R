# ExtraccionDatos.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Capa de extraccion y preparacion inicial de los datos (equivalente a la
# parte de extraccion del proyecto de clase, adaptada al TFM). Descarga
# desde Yahoo Finance con quantmod, cachea en basedata/ un CSV auditable
# por ticker, limpia las series, comprueba su disponibilidad y construye
# el panel comun de precios alineado en fechas de negociacion.

# ---------------------------------------------------------------------
# nombreArchivoTicker()
# Convierte un ticker de Yahoo en un nombre de archivo seguro para el
# sistema operativo (elimina "^" y sustituye "=" y "." por "_").
# Entradas: ticker (p.ej. "^GSPC", "CL=F", "000001.SS")
# Salida:   cadena segura (p.ej. "GSPC", "CL_F", "000001_SS")
# ---------------------------------------------------------------------
nombreArchivoTicker <- function(ticker) {
  nombre <- gsub("\\^", "", ticker)
  nombre <- gsub("[=.]", "_", nombre)
  nombre
}

# ---------------------------------------------------------------------
# tablaEquivalenciaTickers()
# Tabla de equivalencia entre el ticker original de Yahoo y el nombre de
# archivo empleado en basedata/. Permite auditar la descarga cuando el
# ticker contiene simbolos como "^" o "=".
# Entradas: tickers (vector)
# Salida:   data.frame ticker, archivo
# ---------------------------------------------------------------------
tablaEquivalenciaTickers <- function(tickers) {
  data.frame(
    ticker  = tickers,
    archivo = paste0(nombreArchivoTicker(tickers), ".csv"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# descargarActivo()
# Devuelve la serie de un ticker. Si existe un CSV valido en basedata/ que
# cubre las fechas pedidas y no se fuerza el refresco, lo reutiliza; en
# caso contrario descarga con quantmod y lo cachea. El CSV guarda las
# columnas originales (Open, High, Low, Close, Adj.Close, Volume) para
# poder auditar la descarga; el analisis usa el cierre ajustado.
# Entradas: ticker, desde, hasta (config), refrescar (logico)
# Salida:   data.frame con Fecha y las columnas OHLC + Adj.Close + Volume,
#           o NULL si la descarga falla.
# ---------------------------------------------------------------------
descargarActivo <- function(ticker, desde = DESCARGA_INICIO,
                            hasta = DESCARGA_FIN, refrescar = FALSE) {
  if (!dir.exists(RUTA_BASEDATA)) dir.create(RUTA_BASEDATA, recursive = TRUE)
  ruta <- file.path(RUTA_BASEDATA, paste0(nombreArchivoTicker(ticker), ".csv"))

  # Reutilizar copia local si es valida y cubre el periodo pedido.
  if (file.exists(ruta) && !refrescar) {
    serie <- tryCatch({
      s <- read.csv(ruta, stringsAsFactors = FALSE)
      s$Fecha <- as.Date(s$Fecha)
      s
    }, error = function(e) NULL)
    if (!is.null(serie) && nrow(serie) > 0 &&
        min(serie$Fecha) <= as.Date(desde) + 7 &&
        max(serie$Fecha) >= as.Date(hasta) - 7) {
      message("  [cache] ", ticker, " (", nrow(serie), " obs)")
      return(serie)
    }
    message("  [refresco] copia local de ", ticker, " incompleta; se vuelve a descargar")
  }

  # Descarga con quantmod (metodo actual y funcional).
  crudo <- tryCatch(
    quantmod::getSymbols(ticker, src = "yahoo", from = desde, to = hasta,
                         auto.assign = FALSE),
    error = function(e) { message("  [error] ", ticker, ": ", conditionMessage(e)); NULL }
  )
  if (is.null(crudo) || nrow(crudo) == 0) {
    warning("Sin datos para ", ticker)
    return(NULL)
  }

  serie <- data.frame(
    Fecha     = as.Date(zoo::index(crudo)),
    Open      = as.numeric(quantmod::Op(crudo)),
    High      = as.numeric(quantmod::Hi(crudo)),
    Low       = as.numeric(quantmod::Lo(crudo)),
    Close     = as.numeric(quantmod::Cl(crudo)),
    Adj.Close = as.numeric(quantmod::Ad(crudo)),
    Volume    = as.numeric(quantmod::Vo(crudo)),
    stringsAsFactors = FALSE
  )
  write.csv(serie, ruta, row.names = FALSE)
  message("  [descarga] ", ticker, " (", nrow(serie), " obs) -> ", basename(ruta))
  serie
}

# ---------------------------------------------------------------------
# descargarUniverso()
# Descarga (o reutiliza) todos los tickers de una lista y guarda ademas la
# tabla de equivalencia ticker-archivo en basedata/.
# Entradas: tickers, refrescar
# Salida:   lista nombrada por ticker de data.frames (o NULL por ticker)
# ---------------------------------------------------------------------
descargarUniverso <- function(tickers, refrescar = FALSE) {
  series <- list()
  for (tk in tickers) series[[tk]] <- descargarActivo(tk, refrescar = refrescar)
  if (!dir.exists(RUTA_BASEDATA)) dir.create(RUTA_BASEDATA, recursive = TRUE)
  write.csv(tablaEquivalenciaTickers(tickers),
            file.path(RUTA_BASEDATA, "_equivalencia_tickers.csv"), row.names = FALSE)
  series
}

# ---------------------------------------------------------------------
# limpiarSerie()
# Ordena por fecha, elimina valores ausentes y ceros espurios, quita fechas
# duplicadas y devuelve una serie con columnas Fecha y Precio (cierre
# ajustado). De forma opcional rellena huecos hacia delante (locf).
# Entradas: serie (salida de descargarActivo), relleno c("locf","ninguno")
# Salida:   data.frame Fecha, Precio
# ---------------------------------------------------------------------
limpiarSerie <- function(serie, relleno = "locf") {
  if (is.null(serie) || nrow(serie) == 0) return(NULL)
  precio <- if ("Adj.Close" %in% names(serie)) serie$Adj.Close else serie$Cierre
  s <- data.frame(Fecha = as.Date(serie$Fecha), Precio = as.numeric(precio))
  s <- s[order(s$Fecha), ]
  s <- s[!duplicated(s$Fecha), ]
  s <- s[!is.na(s$Precio) & s$Precio > 0, ]
  if (relleno == "locf" && nrow(s) > 0) {
    s$Precio <- zoo::na.locf(s$Precio, na.rm = FALSE)
  }
  s[stats::complete.cases(s), ]
}

# ---------------------------------------------------------------------
# recortarPorFechas()
# Subconjunto de una serie limpia entre dos fechas (ambas incluidas).
# ---------------------------------------------------------------------
recortarPorFechas <- function(serie, inicio, fin) {
  inicio <- as.Date(inicio); fin <- as.Date(fin)
  serie[serie$Fecha >= inicio & serie$Fecha <= fin, ]
}

# ---------------------------------------------------------------------
# comprobarDisponibilidad()
# Para cada indice, primera y ultima fecha, numero de observaciones, si
# cubre la fecha minima comun (2008) y si cubre el inicio de todas las
# ventanas de crisis. Base de la regla de conjunto comun.
# Entradas: lista_series (limpias), fecha_minima, ventanas (opcional)
# Salida:   data.frame ticker, primera_fecha, ultima_fecha, n_obs,
#           cubre_2008, cubre_ventanas
# ---------------------------------------------------------------------
comprobarDisponibilidad <- function(lista_series, fecha_minima = FECHA_MINIMA_COMUN,
                                    ventanas = NULL) {
  fecha_minima <- as.Date(fecha_minima)
  inicio_ventanas <- if (!is.null(ventanas)) min(ventanas$fecha_inicio) else NA
  filas <- lapply(names(lista_series), function(tk) {
    s <- lista_series[[tk]]
    if (is.null(s) || nrow(s) == 0) {
      data.frame(ticker = tk, primera_fecha = as.Date(NA), ultima_fecha = as.Date(NA),
                 n_obs = 0L, cubre_2008 = FALSE, cubre_ventanas = FALSE,
                 stringsAsFactors = FALSE)
    } else {
      data.frame(
        ticker = tk, primera_fecha = min(s$Fecha), ultima_fecha = max(s$Fecha),
        n_obs = nrow(s), cubre_2008 = min(s$Fecha) <= fecha_minima,
        cubre_ventanas = if (is.na(inicio_ventanas)) NA else min(s$Fecha) <= inicio_ventanas,
        stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, filas)
}

# ---------------------------------------------------------------------
# construirPanelPrecios()
# Panel ancho de precios sobre fechas COMUNES de negociacion (interseccion
# de calendarios). Imprescindible para calcular correlaciones coherentes
# entre mercados con festivos nacionales distintos.
# Entradas: lista_series (limpias, nombradas por ticker),
#           metodo c("interseccion","union_locf")
# Salida:   data.frame Fecha + una columna de precios por ticker
# ---------------------------------------------------------------------
construirPanelPrecios <- function(lista_series, metodo = "interseccion") {
  lista_series <- lista_series[!vapply(lista_series, is.null, logical(1))]
  fechas <- lapply(lista_series, function(s) s$Fecha)
  if (metodo == "interseccion") {
    comunes <- Reduce(intersect, fechas)
  } else {
    comunes <- Reduce(union, fechas)
  }
  comunes <- as.Date(sort(unique(comunes)), origin = "1970-01-01")
  panel <- data.frame(Fecha = comunes)
  for (tk in names(lista_series)) {
    s <- lista_series[[tk]]
    valores <- s$Precio[match(comunes, s$Fecha)]
    if (metodo == "union_locf") valores <- zoo::na.locf(valores, na.rm = FALSE)
    panel[[tk]] <- valores
  }
  panel[stats::complete.cases(panel), ]
}
