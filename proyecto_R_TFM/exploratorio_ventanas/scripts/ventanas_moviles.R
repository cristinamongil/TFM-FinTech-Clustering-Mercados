# ---------------------------------------------------------------------
# exploratorio_ventanas/scripts/ventanas_moviles.R
#
# SIMULACION EXPLORATORIA. No forma parte del analisis oficial del TFM.
#
# Objetivo: comprobar de forma descriptiva y EX POST si, dentro de cada
# crisis, algunos mercados modifican su comportamiento antes que otros.
#
# El script NO escribe en ninguna carpeta del pipeline original. Todas sus
# salidas van a exploratorio_ventanas/. Solo LEE data/panel_precios.rds y
# data/ventanas.rds, que produce el pipeline principal.
# ---------------------------------------------------------------------

RAIZ <- getwd()


SALIDA <- file.path(RAIZ, "exploratorio_ventanas")

dir.create(file.path(SALIDA, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SALIDA, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SALIDA, "outputs"), recursive = TRUE, showWarnings = FALSE)

# --- Parametros ------------------------------------------------------
VENTANA_PRINCIPAL <- 63L
VENTANAS_SENS     <- c(42L, 63L, 84L)
PERSISTENCIAS     <- c(5L, 10L)
K_REGIMENES       <- 3L
FACTOR_ANUAL      <- 252L
SEMILLA_BASE      <- 123L
NREINICIOS        <- 50L

# --- Lectura de insumos del pipeline principal (solo lectura) --------
panel    <- readRDS(file.path(RAIZ, "data", "panel_precios.rds"))
ventanas <- readRDS(file.path(RAIZ, "data", "ventanas.rds"))

fechas  <- as.Date(panel$Fecha)
indices <- setdiff(names(panel), "Fecha")
precios <- as.matrix(panel[, indices, drop = FALSE])
n_t <- nrow(precios); n_i <- ncol(precios)

# Rendimientos logaritmicos (primera fila NA, como en el pipeline).
rend <- rbind(rep(NA_real_, n_i),
              apply(precios, 2, function(col) diff(log(col))))
colnames(rend) <- indices

# --- Calendarios propios de cada mercado (solo lectura de basedata/) --
# El panel comun solo conserva las fechas en que negocian los 19 mercados
# a la vez (~154 sesiones/anio frente a ~252 de una bolsa individual). Para
# poder expresar el desfase tambien en sesiones del propio mercado y en
# dias naturales, se leen los calendarios crudos.
nombreArchivo <- function(tk) gsub("[=.]", "_", gsub("\\^", "", tk))
CALENDARIOS <- setNames(lapply(indices, function(tk) {
  as.Date(read.csv(file.path(RAIZ, "basedata", paste0(nombreArchivo(tk), ".csv")),
                   stringsAsFactors = FALSE)$Fecha)
}), indices)

# --- Variables dentro de una ventana movil retrospectiva -------------
# Para cada fecha t y cada mercado i, la ventana son las w sesiones que
# terminan en t (informacion <= t).
#   rent_anual   = suma de rendimientos log de la ventana / w * 252
#   volatilidad  = sd de esos rendimientos * sqrt(252)
#   drawdown     = min(P / cummax(P) - 1) sobre los precios de la ventana
#   correlacion  = media de las correlaciones por pares de i con el resto,
#                  calculadas sobre los rendimientos de la ventana
construirVariablesMoviles <- function(w) {
  rent <- vol <- dd <- corr <- matrix(NA_real_, n_t, n_i,
                                      dimnames = list(NULL, indices))
  for (t in w:n_t) {
    ii <- (t - w + 1):t
    r  <- rend[ii, , drop = FALSE]
    p  <- precios[ii, , drop = FALSE]
    rent[t, ] <- colSums(r, na.rm = TRUE) / w * FACTOR_ANUAL
    vol[t, ]  <- apply(r, 2, stats::sd, na.rm = TRUE) * sqrt(FACTOR_ANUAL)
    dd[t, ]   <- apply(p, 2, function(x) min(x / cummax(x) - 1))
    m <- suppressWarnings(stats::cor(r, use = "pairwise.complete.obs"))
    diag(m) <- NA
    corr[t, ] <- colMeans(m, na.rm = TRUE)
  }
  list(rent_anual = rent, volatilidad = vol, drawdown = dd, correlacion_media = corr)
}

# --- Rachas: primera entrada sostenida en un regimen -----------------
primeraRachaSostenida <- function(pertenece, persistencia) {
  n <- length(pertenece)
  if (n < persistencia) return(list(pos = NA_integer_, dur = NA_integer_))
  r <- rle(pertenece)
  fin <- cumsum(r$lengths)
  ini <- fin - r$lengths + 1L
  ok <- which(r$values & r$lengths >= persistencia)
  if (!length(ok)) return(list(pos = NA_integer_, dur = NA_integer_))
  list(pos = ini[ok[1]], dur = r$lengths[ok[1]])
}

# --- Analisis para una combinacion (ventana, persistencia) -----------
analizar <- function(w, persistencia, guardar_diagnostico = FALSE) {
  V <- construirVariablesMoviles(w)
  filas <- list(); diag_filas <- list()
  crisis_ids <- unique(ventanas$crisis)

  for (cr in crisis_ids) {
    v   <- ventanas[ventanas$crisis == cr, ]
    ini <- as.Date(v$fecha_inicio[v$fase == "antes"])
    fin <- as.Date(v$fecha_fin[v$fase == "despues"])
    pico  <- as.Date(v$fecha_pico[1])
    valle <- as.Date(v$fecha_valle[1])
    en_periodo <- which(fechas >= ini & fechas <= fin)
    idx_pico   <- which.min(abs(fechas[en_periodo] - pico))
    en_aguda   <- fechas[en_periodo] >= pico & fechas[en_periodo] <= valle

    for (j in seq_along(indices)) {
      X <- cbind(rent_anual        = V$rent_anual[en_periodo, j],
                 volatilidad       = V$volatilidad[en_periodo, j],
                 drawdown          = V$drawdown[en_periodo, j],
                 correlacion_media = V$correlacion_media[en_periodo, j])
      ok <- stats::complete.cases(X) & apply(X, 1, function(z) all(is.finite(z)))
      if (sum(ok) < 30) next
      Z <- scale(X[ok, , drop = FALSE])
      Z[!is.finite(Z)] <- 0
      set.seed(SEMILLA_BASE + 1000L * match(cr, crisis_ids) + j)
      km <- stats::kmeans(Z, centers = K_REGIMENES, nstart = NREINICIOS, iter.max = 100)

      # Identificacion del regimen de mayor tension. Regla determinista
      # basada en los centroides estandarizados: se elige el cluster que
      # maximiza volatilidad y correlacion media y minimiza rentabilidad y
      # drawdown (el drawdown es negativo, de modo que mas negativo = mas
      # tension). El solapamiento con la fase aguda del pipeline principal
      # se calcula y se guarda como comprobacion, no como criterio.
      aguda_ok <- en_aguda[ok]
      prop <- tapply(aguda_ok, km$cluster, mean)
      cen  <- km$centers
      score <- cen[, "volatilidad"] + cen[, "correlacion_media"] -
               cen[, "rent_anual"] - cen[, "drawdown"]
      cand <- which(score == max(score))
      reg  <- if (length(cand) == 1) as.integer(cand) else
              as.integer(cand[which.max(cen[cand, "volatilidad"])])

      pertenece <- km$cluster == reg
      racha <- primeraRachaSostenida(pertenece, persistencia)
      fechas_ok <- fechas[en_periodo][ok]
      if (is.na(racha$pos)) {
        f_cambio <- as.Date(NA); dif <- NA_integer_; dur <- NA_integer_
      } else {
        f_cambio <- fechas_ok[racha$pos]
        dif <- which(fechas[en_periodo] == f_cambio) - idx_pico
        dur <- racha$dur
      }
      filas[[length(filas) + 1]] <- data.frame(
        crisis = cr, indice = indices[j], ventana = w, persistencia = persistencia,
        fecha_cambio = f_cambio,
        sesiones_panel_respecto_pico = dif,
        dias_naturales_respecto_pico = if (is.na(dif)) NA_real_ else as.numeric(f_cambio - pico),
        sesiones_mercado_respecto_pico = if (is.na(dif)) NA_integer_ else {
          cal <- CALENDARIOS[[indices[j]]]
          sum(cal > pico & cal <= f_cambio) - sum(cal > f_cambio & cal <= pico)
        },
        momento = ifelse(is.na(dif), NA_character_,
                         ifelse(dif < 0, "antes", ifelse(dif == 0, "en el pico", "despues"))),
        regimen = reg, duracion_racha = dur, n_ventanas = sum(ok),
        stringsAsFactors = FALSE)

      if (guardar_diagnostico) {
        for (cl in seq_len(K_REGIMENES)) {
          diag_filas[[length(diag_filas) + 1]] <- data.frame(
            crisis = cr, indice = indices[j], cluster = cl,
            es_regimen_agudo = (cl == reg),
            n_ventanas = sum(km$cluster == cl),
            prop_en_fase_aguda = round(as.numeric(prop[as.character(cl)]), 4),
            score_tension = round(as.numeric(score[cl]), 4),
            centro_rent_anual = round(cen[cl, "rent_anual"], 4),
            centro_volatilidad = round(cen[cl, "volatilidad"], 4),
            centro_drawdown = round(cen[cl, "drawdown"], 4),
            centro_correlacion = round(cen[cl, "correlacion_media"], 4),
            stringsAsFactors = FALSE)
        }
      }
    }
  }
  list(res = do.call(rbind, filas),
       diag = if (guardar_diagnostico) do.call(rbind, diag_filas) else NULL)
}

# --- Ejecucion -------------------------------------------------------
message("Especificacion principal: ventana 63, persistencia 5")
principal <- analizar(VENTANA_PRINCIPAL, 5L, guardar_diagnostico = TRUE)
write.csv(principal$res,
          file.path(SALIDA, "tables", "fechas_cambio_v63_p5.csv"), row.names = FALSE)
write.csv(principal$diag,
          file.path(SALIDA, "tables", "diagnostico_regimenes_v63_p5.csv"), row.names = FALSE)

message("Sensibilidad: ventanas 42/63/84 x persistencia 5/10")
sens <- list()
for (w in VENTANAS_SENS) for (p in PERSISTENCIAS) {
  message("  w=", w, " p=", p)
  sens[[length(sens) + 1]] <- analizar(w, p)$res
}
sens <- do.call(rbind, sens)
write.csv(sens, file.path(SALIDA, "tables", "sensibilidad_ventana_persistencia.csv"),
          row.names = FALSE)

# --- Resumen por crisis (especificacion principal) -------------------
res <- principal$res
resumen <- do.call(rbind, lapply(unique(res$crisis), function(cr) {
  b <- res[res$crisis == cr & !is.na(res$fecha_cambio), ]
  b <- b[order(b$fecha_cambio, b$indice), ]
  data.frame(crisis = cr,
             primero = b$indice[1], fecha_primero = as.character(b$fecha_cambio[1]),
             segundo = b$indice[2], tercero = b$indice[3],
             ultimo = b$indice[nrow(b)], fecha_ultimo = as.character(b$fecha_cambio[nrow(b)]),
             dispersion_sesiones_panel = max(b$sesiones_panel_respecto_pico) - min(b$sesiones_panel_respecto_pico),
             dispersion_dias_naturales = max(b$dias_naturales_respecto_pico) - min(b$dias_naturales_respecto_pico),
             n_mercados_con_fecha = nrow(b),
             n_antes_del_pico = sum(b$sesiones_panel_respecto_pico < 0),
             stringsAsFactors = FALSE)
}))
write.csv(resumen, file.path(SALIDA, "tables", "resumen_por_crisis.csv"), row.names = FALSE)

# --- Figuras timeline ------------------------------------------------
for (cr in unique(res$crisis)) {
  b <- res[res$crisis == cr & !is.na(res$fecha_cambio), ]
  if (!nrow(b)) next
  b <- b[order(b$fecha_cambio, decreasing = TRUE), ]
  v <- ventanas[ventanas$crisis == cr, ]
  pico <- as.Date(v$fecha_pico[1])
  png(file.path(SALIDA, "figures", paste0("timeline_", cr, ".png")),
      width = 1600, height = 1100, res = 150)
  op <- par(mar = c(4.5, 8.5, 3, 1.5))
  rango <- range(c(b$fecha_cambio, pico))
  plot(b$fecha_cambio, seq_len(nrow(b)), type = "n", yaxt = "n",
       xlim = rango, ylim = c(0.5, nrow(b) + 0.5),
       xlab = "Fecha de primera entrada sostenida", ylab = "",
       main = paste0("Cambio de regimen por mercado - ", cr,
                     "\n(ventana 63 sesiones, persistencia 5)"))
  axis(2, at = seq_len(nrow(b)), labels = b$indice, las = 1, cex.axis = 0.8)
  abline(h = seq_len(nrow(b)), col = "grey90")
  abline(v = pico, col = "red", lwd = 2, lty = 2)
  points(b$fecha_cambio, seq_len(nrow(b)), pch = 19, col = "steelblue4", cex = 1.2)
  legend("bottomleft", legend = c("Inicio de la fase aguda (pico)", "Cambio detectado"),
         col = c("red", "steelblue4"), lty = c(2, NA), pch = c(NA, 19), bty = "n", cex = 0.85)
  par(op); dev.off()
}

# --- Registro de parametros -----------------------------------------
write.csv(data.frame(
  parametro = c("ventana_principal", "ventanas_sensibilidad", "persistencias",
                "k_regimenes", "factor_anual", "semilla_base", "nstart",
                "n_indices", "n_fechas_panel", "fecha_ejecucion"),
  valor = c(VENTANA_PRINCIPAL, paste(VENTANAS_SENS, collapse = "/"),
            paste(PERSISTENCIAS, collapse = "/"), K_REGIMENES, FACTOR_ANUAL,
            SEMILLA_BASE, NREINICIOS, n_i, n_t, as.character(Sys.Date())),
  stringsAsFactors = FALSE),
  file.path(SALIDA, "outputs", "parametros_ejecucion.csv"), row.names = FALSE)

message("Listo. Salidas en exploratorio_ventanas/")
