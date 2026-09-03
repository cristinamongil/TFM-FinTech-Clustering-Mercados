# clustering.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Seleccion del numero de grupos y agrupamiento con K-Means (Word 3.8). La
# seleccion de k combina el metodo del codo (suma de cuadrados intragrupo)
# y el coeficiente medio de silueta en las tres crisis, y propone un k
# comun. K-Means se ejecuta con semilla fija y multiples reinicios sobre
# las variables estandarizadas, conservando etiquetas y centroides para
# reproducir los resultados.

# ---------------------------------------------------------------------
# siluetaMedia()
# Coeficiente medio de silueta de una particion sobre una matriz de datos.
# Entradas: m (matriz observaciones x variables), etiquetas (vector)
# Salida:   escalar (NA si k < 2 o k >= n)
# ---------------------------------------------------------------------
siluetaMedia <- function(m, etiquetas) {
  k <- length(unique(etiquetas))
  if (k < 2 || k >= nrow(m)) return(NA_real_)
  sil <- cluster::silhouette(etiquetas, stats::dist(m))
  mean(sil[, "sil_width"])
}

# ---------------------------------------------------------------------
# seleccionarK()
# Para cada crisis (fase aguda) calcula, en el rango de k, la suma de
# cuadrados intragrupo (codo) y la silueta media. Propone un k comun como
# el que maximiza la silueta media promediada entre las tres crisis.
# Entradas: variables_std, rango_k, semilla, nreinicios
# Salida:   list(curvas = data.frame(crisis, k, wss, silueta),
#                 resumen = data.frame(k, silueta_media),
#                 k_propuesto = entero)
# ---------------------------------------------------------------------
seleccionarK <- function(variables_std, rango_k = RANGO_K,
                         semilla = SEMILLA, nreinicios = KMEANS_NREINICIOS) {
  crisis_nombres <- unique(variables_std$crisis)
  curvas <- list()
  for (cr in crisis_nombres) {
    m <- matrizCluster(variables_std, cr, "durante")
    for (k in rango_k) {
      if (k >= nrow(m)) next
      set.seed(semilla)
      km <- stats::kmeans(m, centers = k, nstart = nreinicios)
      curvas[[length(curvas) + 1]] <- data.frame(
        crisis = cr, k = k, wss = km$tot.withinss,
        silueta = siluetaMedia(m, km$cluster), stringsAsFactors = FALSE)
    }
  }
  curvas <- do.call(rbind, curvas)
  resumen <- aggregate(silueta ~ k, data = curvas, FUN = mean, na.rm = TRUE)
  names(resumen)[2] <- "silueta_media"
  k_propuesto <- resumen$k[which.max(resumen$silueta_media)]
  list(curvas = curvas, resumen = resumen, k_propuesto = as.integer(k_propuesto))
}

# ---------------------------------------------------------------------
# ejecutarKMeans()
# Agrupamiento con K-Means por grupos. por = "crisis" agrupa solo la fase
# aguda de cada crisis (analisis principal); por = "crisis_fase" agrupa
# cada combinacion de crisis y fase (soporte de los complementarios).
# Entradas: variables_std, k, por, semilla, nreinicios
# Salida:   lista nombrada por grupo; cada elemento tiene:
#           $crisis, $fase, $etiquetas (vector nombrado por indice),
#           $centroides (matriz k x variables), $silueta (media)
# ---------------------------------------------------------------------
ejecutarKMeans <- function(variables_std, k = K_COMUN, por = "crisis",
                           semilla = SEMILLA, nreinicios = KMEANS_NREINICIOS) {
  if (is.na(k)) stop("k no esta definido. Ejecuta antes seleccionarK() y fija k.")
  combinaciones <- if (por == "crisis") {
    data.frame(crisis = unique(variables_std$crisis), fase = "durante",
               stringsAsFactors = FALSE)
  } else {
    unique(variables_std[, c("crisis", "fase")])
  }
  salida <- list()
  for (i in seq_len(nrow(combinaciones))) {
    cr <- combinaciones$crisis[i]; fa <- combinaciones$fase[i]
    m <- matrizCluster(variables_std, cr, fa)
    if (nrow(m) <= k) {
      warning("Grupo ", cr, "/", fa, " con muy pocas observaciones; se omite.")
      next
    }
    set.seed(semilla)
    km <- stats::kmeans(m, centers = k, nstart = nreinicios)
    etiquetas <- km$cluster; names(etiquetas) <- rownames(m)
    clave <- if (por == "crisis") cr else paste(cr, fa, sep = "_")
    salida[[clave]] <- list(
      crisis = cr, fase = fa, etiquetas = etiquetas,
      centroides = km$centers, silueta = siluetaMedia(m, km$cluster))
  }
  salida
}

# ---------------------------------------------------------------------
# describirCentroides()
# Tabla de centroides de un agrupamiento (coordenadas estandarizadas).
# Entradas: centroides (matriz k x variables)
# Salida:   data.frame cluster + variables
# ---------------------------------------------------------------------
describirCentroides <- function(centroides) {
  df <- as.data.frame(round(centroides, 3))
  df$cluster <- seq_len(nrow(df))
  df[, c("cluster", colnames(centroides))]
}

# ---------------------------------------------------------------------
# alinearEtiquetas()
# Establece la correspondencia entre los grupos de dos particiones de los
# MISMOS mercados atendiendo a la coincidencia de sus miembros: se prueban
# todas las correspondencias posibles entre los k grupos y se elige la que
# deja el mayor numero de mercados en el mismo grupo. Es determinista y no
# utiliza distancias entre centroides, que no serian comparables porque
# cada fase se estandariza por separado. Permite seguir los grupos entre
# fases evitando que una permutacion nominal de las etiquetas de K-Means
# se contabilice como una migracion.
# Entradas: etiquetas_ref, etiquetas_obj (vectores con nombres de indice)
# Salida:   vector de etiquetas objetivo reetiquetadas
# ---------------------------------------------------------------------
alinearEtiquetas <- function(etiquetas_ref, etiquetas_obj) {
  k <- max(c(etiquetas_ref, etiquetas_obj))
  comunes <- intersect(names(etiquetas_ref), names(etiquetas_obj))
  if (length(comunes) == 0) return(etiquetas_obj)
  permutaciones <- permutacionesK(k)
  coincidencias <- vapply(permutaciones, function(p) {
    sum(p[etiquetas_obj[comunes]] == etiquetas_ref[comunes])
  }, numeric(1))
  mapa <- permutaciones[[which.max(coincidencias)]]
  nuevas <- etiquetas_obj
  for (g in seq_len(k)) nuevas[etiquetas_obj == g] <- mapa[g]
  nuevas
}

# ---------------------------------------------------------------------
# permutacionesK()
# Devuelve todas las permutaciones de 1:k como lista de vectores. Con k=3
# son solo 6 correspondencias posibles, de modo que la busqueda es exacta
# y no requiere heuristicas.
# ---------------------------------------------------------------------
permutacionesK <- function(k) {
  if (k == 1) return(list(1L))
  res <- list()
  for (i in seq_len(k)) {
    for (resto in permutacionesK(k - 1)) {
      otros <- setdiff(seq_len(k), i)
      res[[length(res) + 1]] <- as.integer(c(i, otros[resto]))
    }
  }
  res
}
