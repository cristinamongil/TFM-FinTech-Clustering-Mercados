# seedCheck.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Comprobacion de robustez de la inicializacion de K-Means (Word 3.8).
# El agrupamiento de la fase aguda de cada crisis se repite con semillas
# alternativas a la fijada en config.R (SEMILLA = 123), manteniendo el
# resto de parametros del analisis (mismo k comun, mismo nstart, mismas
# variables estandarizadas). Cada particion alternativa se compara con la
# obtenida con la semilla del analisis mediante el indice de Rand
# ajustado y la suma de cuadrados intragrupo (WSS), para comprobar si
# distintas inicializaciones del conjunto de prototipos alteran los
# grupos o su calidad.
#
# Es autosuficiente: si se ejecuta en una sesion nueva de R (sin haber
# corrido antes init.R o main_TFM.R en esa misma sesion), carga el
# entorno y recalcula K_COMUN exactamente igual que main_TFM.R (bloque
# 10), a partir de data/variables_std.rds. Si esos objetos ya estan en
# el entorno (por haber corrido main_TFM.R justo antes), los reutiliza
# tal cual, sin recalcular nada.
# Genera: resultados/tablas/comprobacion_semillas.csv

if (!exists("RUTA_DATOS")) source("init.R")

SEMILLAS_ALTERNATIVAS <- c(1, 7, 42, 99, 777, 2020, 2024, 55555, 808, 31416)

# ---------------------------------------------------------------------
# ejecutarKMeansSemilla()
# Repite el agrupamiento de una crisis (fase aguda) con una semilla dada,
# manteniendo k y nstart. A diferencia de ejecutarKMeans(), devuelve
# tambien la suma de cuadrados intragrupo (WSS) para poder comparar la
# calidad de cada particion, no solo su composicion.
# Entradas: variables_std, crisis, k, semilla, nreinicios
# Salida:   list(etiquetas, wss)
# ---------------------------------------------------------------------
ejecutarKMeansSemilla <- function(variables_std, crisis, k, semilla,
                                   nreinicios = KMEANS_NREINICIOS) {
  m <- matrizCluster(variables_std, crisis, "durante")
  set.seed(semilla)
  km <- stats::kmeans(m, centers = k, nstart = nreinicios)
  list(etiquetas = km$cluster, wss = km$tot.withinss)
}

variables_std <- readRDS(file.path(RUTA_DATOS, "variables_std.rds"))
crisis_nombres <- unique(variables_std$crisis)

if (!exists("K_COMUN") || is.na(K_COMUN)) {
  message("K_COMUN no estaba definido en esta sesion; se recalcula igual ",
          "que en main_TFM.R (bloque 10, silueta media maxima).")
  K_COMUN <- seleccionarK(variables_std)$k_propuesto
  message("K_COMUN recalculado: ", K_COMUN)
}

# Particion de referencia: la empleada en el analisis (SEMILLA = 123).
base <- setNames(
  lapply(crisis_nombres, function(cr) {
    ejecutarKMeansSemilla(variables_std, cr, K_COMUN, SEMILLA)
  }),
  crisis_nombres
)

# Particiones alternativas: mismas crisis, mismo k, semillas distintas.
filas <- list()
for (cr in crisis_nombres) {
  for (s in SEMILLAS_ALTERNATIVAS) {
    alt  <- ejecutarKMeansSemilla(variables_std, cr, K_COMUN, s)
    rand <- randAjustado(base[[cr]]$etiquetas, alt$etiquetas)
    # particion_igual se basa en el Rand ajustado (= 1 solo cuando los
    # grupos son exactamente los mismos), no en si las etiquetas
    # numericas coinciden: K-Means las asigna de forma arbitraria, por lo
    # que dos ejecuciones pueden formar los mismos grupos y llamarlos con
    # numeros distintos (vease el aviso del apartado 3.8 y 3.11 sobre esto).
    filas[[length(filas) + 1]] <- data.frame(
      crisis          = cr,
      semilla         = s,
      rand_ajustado   = round(rand, 4),
      wss_semilla     = round(alt$wss, 3),
      wss_analisis    = round(base[[cr]]$wss, 3),
      particion_igual = isTRUE(all.equal(rand, 1)),
      stringsAsFactors = FALSE
    )
  }
}
comprobacion_semillas <- do.call(rbind, filas)

guardarTabla(comprobacion_semillas, "comprobacion_semillas")

message(
  "Comprobacion de semillas: ", sum(comprobacion_semillas$particion_igual),
  " de ", nrow(comprobacion_semillas),
  " comparaciones con particion identica a la del analisis (semilla ", SEMILLA,
  ", k = ", K_COMUN, ")."
)
