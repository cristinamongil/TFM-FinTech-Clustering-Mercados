# ---------------------------------------------------------------------
# diagnostico_ventana_principal/scripts/diagnostico_ventana.R
#
# DIAGNOSTICO DE SENSIBILIDAD. No forma parte del analisis oficial.
# Solo LEE codigo y objetos del pipeline principal. Escribe unicamente en
# diagnostico_ventana_principal/.
#
# Escenario A (oficial): volatilidad y correlacion con ventana de 63 filas
#   del panel comun de los 19 mercados.
# Escenario B (sensibilidad): identicas en todo salvo que la ventana son
#   TRES MESES NATURALES terminados en cada fecha t.
# ---------------------------------------------------------------------
RAIZ <- getwd()
SAL  <- file.path(RAIZ, "diagnostico_ventana_principal")
dir.create(file.path(SAL, "tables"), recursive = TRUE, showWarnings = FALSE)

for (f in c("config.R", "financialFuns.R", "features.R", "clustering.R",
            "hypothesisContrast.R", "complementary.R")) source(file.path(RAIZ, f))

# Silueta en R base (el proyecto usa cluster::silhouette; aqui no esta
# disponible, de modo que se reimplementa la misma definicion).
siluetaMedia <- function(m, etiquetas) {
  k <- length(unique(etiquetas)); n <- nrow(m)
  if (k < 2 || k >= n) return(NA_real_)
  D <- as.matrix(stats::dist(m))
  s <- numeric(n)
  for (i in seq_len(n)) {
    propio <- etiquetas == etiquetas[i]
    a <- if (sum(propio) > 1) mean(D[i, propio & seq_len(n) != i]) else 0
    b <- min(vapply(setdiff(unique(etiquetas), etiquetas[i]),
                    function(g) mean(D[i, etiquetas == g]), numeric(1)))
    s[i] <- if (max(a, b) == 0) 0 else (b - a) / max(a, b)
  }
  mean(s)
}

panel    <- readRDS(file.path(RAIZ, "data", "panel_precios.rds"))
ventanas <- readRDS(file.path(RAIZ, "data", "ventanas.rds"))
fechas   <- as.Date(panel$Fecha)
indices  <- setdiff(names(panel), "Fecha")
precios  <- as.matrix(panel[, indices, drop = FALSE])
rend     <- rbind(NA, apply(precios, 2, function(col) diff(log(col))))

# --- Escenario B: ventana de tres meses naturales --------------------
inicioVentanaNatural <- function(t) {
  lim <- seq(fechas[t], by = "-3 months", length.out = 2)[2]
  which(fechas >= lim & fechas <= fechas[t])
}
IDX_B <- lapply(seq_along(fechas), inicioVentanaNatural)
n_obs_B <- vapply(IDX_B, length, integer(1))

volatilidadNatural <- function(r) {
  out <- rep(NA_real_, length(r))
  for (t in seq_along(r)) {
    ii <- IDX_B[[t]]
    if (length(ii) >= 5) out[t] <- stats::sd(r[ii], na.rm = TRUE)
  }
  out * sqrt(FACTOR_ANUAL)
}
correlacionNatural <- function(panel_rend) {
  panel_rend <- as.matrix(panel_rend); n <- nrow(panel_rend)
  sal <- matrix(NA_real_, n, ncol(panel_rend), dimnames = list(NULL, colnames(panel_rend)))
  for (t in seq_len(n)) {
    ii <- IDX_B[[t]]
    if (length(ii) < 5) next
    m <- suppressWarnings(stats::cor(panel_rend[ii, , drop = FALSE]))
    diag(m) <- NA
    sal[t, ] <- colMeans(m, na.rm = TRUE)
  }
  sal
}

construirVariables <- function(vol_movil, corr_movil) {
  filas <- list()
  for (i in seq_len(nrow(ventanas))) {
    v <- ventanas[i, ]
    en <- fechas >= v$fecha_inicio & fechas <= v$fecha_fin
    for (idx in indices) {
      p <- precios[en, idx]; r <- rendimientosLog(p)
      filas[[length(filas) + 1]] <- data.frame(
        indice = idx, crisis = v$crisis, fase = v$fase,
        rent_anual = rendimientoAnualizado(r),
        volatilidad = resumenFase(vol_movil[en, idx]),
        drawdown = caidaMaxima(p),
        correlacion_media = resumenFase(corr_movil[en, idx]),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, filas)
}

message("Escenario A (63 filas del panel)")
volA  <- apply(rend, 2, function(r) volatilidadRealizada(r))
corrA <- correlacionMediaMovil(rend)
varA  <- construirVariables(volA, corrA)

message("Escenario B (3 meses naturales)")
volB  <- apply(rend, 2, volatilidadNatural)
corrB <- correlacionNatural(rend)
varB  <- construirVariables(volB, corrB)

stdA <- estandarizarVariables(varA); stdB <- estandarizarVariables(varB)
K <- 3L
crA <- ejecutarKMeans(stdA, k = K, por = "crisis")
crB <- ejecutarKMeans(stdB, k = K, por = "crisis")
cfA <- ejecutarKMeans(stdA, k = K, por = "crisis_fase")
cfB <- ejecutarKMeans(stdB, k = K, por = "crisis_fase")

# --- Tamano de las ventanas del escenario B --------------------------
val <- n_obs_B[n_obs_B > 0 & fechas >= min(as.Date(ventanas$fecha_inicio))]
tam <- data.frame(estadistico = c("media","mediana","minimo","p25","p75","maximo"),
                  observaciones = c(round(mean(val),2), median(val), min(val),
                                    as.numeric(quantile(val,.25)), as.numeric(quantile(val,.75)), max(val)))
write.csv(tam, file.path(SAL,"tables","B_tamano_ventana_3meses.csv"), row.names = FALSE)
print(tam)

# --- Comparacion A vs B por crisis -----------------------------------
info <- data.frame(indice = indices,
  region = c("America","America","Europa","Europa","Europa","Europa","Europa","Europa","Europa",
             "Asia-Pacifico","Asia-Pacifico","Asia-Pacifico","Asia-Pacifico",
             "America","Asia-Pacifico","America","Asia-Pacifico","Asia-Pacifico","Asia-Pacifico"),
  desarrollo = c(rep("Desarrollado",13), rep("Emergente",6)), stringsAsFactors = FALSE)

comp <- list(); cent <- list(); memb <- list()
for (cr in names(crA)) {
  ea <- crA[[cr]]$etiquetas; eb <- crB[[cr]]$etiquetas
  com <- intersect(names(ea), names(eb))
  eb_al <- alinearEtiquetas(ea[com], eb[com])
  mA <- matrizCluster(stdA, cr, "durante"); mB <- matrizCluster(stdB, cr, "durante")
  # grupo de mayor correlacion media (variable estandarizada)
  gA <- which.max(crA[[cr]]$centroides[,"correlacion_media"])
  gB <- which.max(crB[[cr]]$centroides[,"correlacion_media"])
  sincA <- names(ea)[ea == gA]; sincB <- names(eb)[eb == gB]
  f <- function(v) {
    s <- info[info$indice %in% v, ]
    c(n = nrow(s), des = sum(s$desarrollo == "Desarrollado"),
      eur = sum(s$region == "Europa"))
  }
  a1 <- f(sincA); b1 <- f(sincB)
  comp[[length(comp)+1]] <- data.frame(crisis = cr,
    n_mercados = length(com),
    n_mismo_grupo = sum(eb_al[com] == ea[com]),
    pct_mismo_grupo = round(100*sum(eb_al[com]==ea[com])/length(com),1),
    rand_ajustado_A_vs_B = round(randAjustado(ea[com], eb[com]), 3),
    silueta_A = round(crA[[cr]]$silueta,3), silueta_B = round(crB[[cr]]$silueta,3),
    grupo_max_corr_A = gA, grupo_max_corr_B = gB,
    sinc_A_n = a1["n"], sinc_A_desarrollados = a1["des"], sinc_A_europeos = a1["eur"],
    sinc_B_n = b1["n"], sinc_B_desarrollados = b1["des"], sinc_B_europeos = b1["eur"],
    stringsAsFactors = FALSE)
  for (esc in c("A","B")) {
    cc <- if (esc=="A") crA[[cr]]$centroides else crB[[cr]]$centroides
    d <- as.data.frame(round(cc,3)); d$cluster <- seq_len(nrow(d)); d$escenario <- esc; d$crisis <- cr
    cent[[length(cent)+1]] <- d[,c("crisis","escenario","cluster",COLS_VARIABLES)]
  }
  memb[[length(memb)+1]] <- data.frame(crisis = cr, indice = com,
    grupo_A = as.integer(ea[com]), grupo_B_alineado = as.integer(eb_al[com]),
    coincide = eb_al[com] == ea[com],
    region = info$region[match(com, info$indice)],
    desarrollo = info$desarrollo[match(com, info$indice)], stringsAsFactors = FALSE)
}
comp <- do.call(rbind, comp); rownames(comp) <- NULL
write.csv(comp, file.path(SAL,"tables","comparacion_A_vs_B_fase_aguda.csv"), row.names=FALSE)
write.csv(do.call(rbind,cent), file.path(SAL,"tables","centroides_A_vs_B.csv"), row.names=FALSE)
write.csv(do.call(rbind,memb), file.path(SAL,"tables","pertenencia_A_vs_B.csv"), row.names=FALSE)
print(comp[,c("crisis","n_mismo_grupo","pct_mismo_grupo","rand_ajustado_A_vs_B","silueta_A","silueta_B")])

# --- Rand entre crisis ------------------------------------------------
rA <- cambioEntreParticiones(crA); rB <- cambioEntreParticiones(crB)
entre <- merge(rA, rB, by = c("crisis_a","crisis_b"), suffixes = c("_A","_B"))
write.csv(entre, file.path(SAL,"tables","rand_entre_crisis_A_vs_B.csv"), row.names=FALSE)
print(entre)

# --- Migraciones entre fases -----------------------------------------
migA <- migracionEntreFases(cfA); migB <- migracionEntreFases(cfB)
resumenMig <- function(m, esc) {
  do.call(rbind, lapply(unique(m$crisis), function(cr) {
    b <- m[m$crisis == cr & !is.na(m$n_cambios), ]
    data.frame(escenario = esc, crisis = cr, n = nrow(b),
      sin_cambios = sum(b$n_cambios == 0),
      pct_sin_cambios = round(100*sum(b$n_cambios==0)/nrow(b),1), stringsAsFactors = FALSE)
  }))
}
mig <- rbind(resumenMig(migA,"A"), resumenMig(migB,"B"))
mm <- merge(migA[,c("crisis","indice","n_cambios")], migB[,c("crisis","indice","n_cambios")],
            by=c("crisis","indice"), suffixes=c("_A","_B"))
dif <- aggregate(cbind(cambia_clasificacion = mm$n_cambios_A != mm$n_cambios_B) ~ crisis, data = mm, FUN = sum)
mig <- merge(mig, dif, by = "crisis", all.x = TRUE)
write.csv(mig, file.path(SAL,"tables","migraciones_A_vs_B.csv"), row.names=FALSE)
write.csv(mm, file.path(SAL,"tables","migraciones_detalle_A_vs_B.csv"), row.names=FALSE)
print(mig)

# --- Arrastre entre fases (escenario A) ------------------------------
arr <- list()
for (cr in unique(ventanas$crisis)) {
  vv <- ventanas[ventanas$crisis == cr, ]
  ini_crisis <- as.Date(vv$fecha_inicio[vv$fase == "antes"])
  for (fa in c("antes","durante","despues")) {
    v <- vv[vv$fase == fa, ]
    en <- which(fechas >= v$fecha_inicio & fechas <= v$fecha_fin)
    prop <- vapply(en, function(t) {
      ii <- (t - N_VENTANA_MOVIL + 1):t
      ii <- ii[ii >= 1]
      mean(fechas[ii] >= v$fecha_inicio & fechas[ii] <= v$fecha_fin)
    }, numeric(1))
    arr[[length(arr)+1]] <- data.frame(crisis = cr, fase = fa,
      n_fechas_fase = length(en), n_valores_moviles = length(en),
      pct_medio_propia_fase = round(100*mean(prop),1),
      pct_medio_anterior_a_la_fase = round(100*(1-mean(prop)),1),
      pct_min_propia_fase = round(100*min(prop),1),
      pct_max_propia_fase = round(100*max(prop),1),
      dias_naturales_fase = as.numeric(as.Date(v$fecha_fin) - as.Date(v$fecha_inicio)),
      stringsAsFactors = FALSE)
  }
}
arr <- do.call(rbind, arr)
write.csv(arr, file.path(SAL,"tables","arrastre_entre_fases_escenarioA.csv"), row.names=FALSE)
print(arr)
message("Diagnostico terminado.")
