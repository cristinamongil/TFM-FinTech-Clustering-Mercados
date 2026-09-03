# complementary.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Analisis complementarios que enriquecen las conclusiones sin alterar el
# enfoque exploratorio (Word 3.11, Tabla 3.7): cambio entre particiones,
# migracion entre fases, separabilidad y
# sensibles, reaccion y recuperacion, y comportamiento de los activos de
# contexto (todo ello estrictamente descriptivo).

# ---------------------------------------------------------------------
# cambioEntreParticiones()
# Indice de Rand ajustado entre las particiones de la fase aguda de cada
# par de crisis, sobre los indices comunes. Mide si cada crisis genera un
# patron de agrupacion distinto.
# Entradas: res_crisis (salida de ejecutarKMeans(por="crisis"))
# Salida:   data.frame crisis_a, crisis_b, rand_ajustado, n_comunes
# ---------------------------------------------------------------------
cambioEntreParticiones <- function(res_crisis) {
  nombres <- names(res_crisis)
  filas <- list()
  for (i in seq_along(nombres)) {
    for (j in seq_along(nombres)) {
      if (j <= i) next
      ea <- res_crisis[[i]]$etiquetas; eb <- res_crisis[[j]]$etiquetas
      comunes <- intersect(names(ea), names(eb))
      filas[[length(filas) + 1]] <- data.frame(
        crisis_a = nombres[i], crisis_b = nombres[j],
        rand_ajustado = if (length(comunes) >= 2)
          round(randAjustado(ea[comunes], eb[comunes]), 3) else NA_real_,
        n_comunes = length(comunes), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, filas)
}

# ---------------------------------------------------------------------
# migracionEntreFases()
# Para cada crisis, sigue la etiqueta de cada indice en las tres fases.
# Las etiquetas de "antes" y "despues" se alinean con las de "durante" por
# coincidencia de los mercados que integran cada grupo, para que sean
# comparables entre fases. Devuelve la tabla de
# pertenencia y el numero de cambios de grupo por indice.
# Entradas: res_crisis_fase (salida de ejecutarKMeans(por="crisis_fase"))
# Salida:   data.frame crisis, indice, antes, durante, despues, n_cambios
# ---------------------------------------------------------------------
migracionEntreFases <- function(res_crisis_fase) {
  crisis_nombres <- unique(vapply(res_crisis_fase, function(x) x$crisis, character(1)))
  fases <- c("antes", "durante", "despues")
  filas <- list()
  for (cr in crisis_nombres) {
    ref <- res_crisis_fase[[paste(cr, "durante", sep = "_")]]
    if (is.null(ref)) next
    etq <- list(durante = ref$etiquetas)
    for (fa in c("antes", "despues")) {
      obj <- res_crisis_fase[[paste(cr, fa, sep = "_")]]
      if (is.null(obj)) { etq[[fa]] <- NULL; next }
      nuevas <- alinearEtiquetas(ref$etiquetas, obj$etiquetas)
      names(nuevas) <- names(obj$etiquetas)
      etq[[fa]] <- nuevas
    }
    indices <- names(ref$etiquetas)
    for (idx in indices) {
      valores <- c(antes = NA, durante = NA, despues = NA)
      for (fa in fases) if (!is.null(etq[[fa]]) && idx %in% names(etq[[fa]]))
        valores[fa] <- etq[[fa]][idx]
      presentes <- valores[!is.na(valores)]
      n_cambios <- if (length(presentes) >= 2) sum(diff(as.integer(presentes)) != 0) else NA_integer_
      filas[[length(filas) + 1]] <- data.frame(
        crisis = cr, indice = idx,
        antes = valores["antes"], durante = valores["durante"], despues = valores["despues"],
        n_cambios = n_cambios, stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, filas); rownames(res) <- NULL; res
}

# ---------------------------------------------------------------------
# separabilidadGrupos()
# Silueta media de la particion de la fase aguda de cada crisis. Indica si
# una crisis homogeneiza (silueta baja) o divide (silueta alta) a los
# mercados.
# Entradas: res_crisis
# Salida:   data.frame crisis, silueta_media
# ---------------------------------------------------------------------
separabilidadGrupos <- function(res_crisis) {
  do.call(rbind, lapply(names(res_crisis), function(cr) {
    data.frame(crisis = cr, silueta_media = round(res_crisis[[cr]]$silueta, 3),
               stringsAsFactors = FALSE)
  }))
}

# ---------------------------------------------------------------------
# reaccionRecuperacion()
# Descriptivo, por indice y crisis: fecha en que la caida desde el maximo
# previo supera un umbral, fecha del drawdown maximo, y dias hasta recobrar
# un porcentaje de la caida dentro de la fase posterior. Los mercados que
# no recuperan ese nivel se marcan como no recuperados. No se afirma
# relacion causal entre mercados.
# Entradas: panel_precios, ventanas, umbral_caida, pct_recuperacion
# Salida:   data.frame crisis, indice, fecha_umbral, fecha_min, dd_maximo,
#           dias_recuperacion, recuperado
# ---------------------------------------------------------------------
reaccionRecuperacion <- function(panel_precios, ventanas,
                                 umbral_caida = UMBRAL_CAIDA,
                                 pct_recuperacion = PCT_RECUPERACION) {
  indices <- setdiff(names(panel_precios), "Fecha")
  crisis_nombres <- unique(ventanas$crisis)
  filas <- list()
  for (cr in crisis_nombres) {
    v <- ventanas[ventanas$crisis == cr, ]
    ini_aguda <- v$fecha_inicio[v$fase == "durante"]
    fin_post  <- v$fecha_fin[v$fase == "despues"]
    tramo <- panel_precios[panel_precios$Fecha >= ini_aguda & panel_precios$Fecha <= fin_post, ]
    for (idx in indices) {
      p <- tramo[[idx]]; f <- tramo$Fecha
      if (length(p) < 2) next
      caida <- p / cummax(p) - 1
      i_umbral <- which(caida <= umbral_caida)[1]
      i_min    <- which.min(caida)
      dd_max   <- caida[i_min]
      objetivo <- cummax(p)[i_min] * (1 + dd_max * (1 - pct_recuperacion))
      post_min <- which(seq_along(p) > i_min & p >= objetivo)
      recuperado <- length(post_min) > 0
      dias_rec <- if (recuperado) as.integer(f[post_min[1]] - f[i_min]) else NA_integer_
      filas[[length(filas) + 1]] <- data.frame(
        crisis = cr, indice = idx,
        fecha_umbral = if (!is.na(i_umbral)) f[i_umbral] else as.Date(NA),
        fecha_min = f[i_min], dd_maximo = round(dd_max, 3),
        dias_recuperacion = dias_rec, recuperado = recuperado,
        stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, filas); rownames(res) <- NULL; res
}

# ---------------------------------------------------------------------
# comportamientoContexto()
# Describe el comportamiento de los activos de contexto (oro, petroleo y
# deuda publica estadounidense) dentro de la fase aguda de cada crisis,
# empleando las mismas ventanas oficiales que el resto del analisis. Estos
# activos no participan en el agrupamiento: la tabla sirve unicamente para
# caracterizar el entorno de cada episodio. La volatilidad se calcula como
# la desviacion tipica de los rendimientos diarios de la fase anualizada,
# ya que la fase aguda de la COVID es mas corta que la ventana movil
# empleada para los indices.
# Entradas: precios_contexto (lista de data.frame Fecha/Precio), ventanas
# Salida:   data.frame crisis, activo, n_obs, rent_fase, volatilidad, drawdown
# ---------------------------------------------------------------------
comportamientoContexto <- function(precios_contexto, ventanas,
                                   factor_anual = FACTOR_ANUAL) {
  activos <- names(precios_contexto)
  crisis_nombres <- unique(ventanas$crisis)
  filas <- list()
  for (cr in crisis_nombres) {
    v   <- ventanas[ventanas$crisis == cr, ]
    ini <- v$fecha_inicio[v$fase == "durante"]
    fin <- v$fecha_fin[v$fase == "durante"]
    for (act in activos) {
      serie <- precios_contexto[[act]]
      if (is.null(serie) || nrow(serie) == 0) next
      tramo <- serie[serie$Fecha >= ini & serie$Fecha <= fin, ]
      p <- tramo$Precio
      if (length(p) < 3) next
      rend <- rendimientosLog(p)
      filas[[paste(cr, act)]] <- data.frame(
        crisis      = cr,
        activo      = act,
        n_obs       = length(p),
        rent_fase   = round(p[length(p)] / p[1] - 1, 4),
        volatilidad = round(stats::sd(rend, na.rm = TRUE) * sqrt(factor_anual), 4),
        drawdown    = round(caidaMaxima(p), 4),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, filas)
}
