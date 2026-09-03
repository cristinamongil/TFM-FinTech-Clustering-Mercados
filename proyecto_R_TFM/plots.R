# plots.R
# TFM - Analitica de datos en series temporales financieras
#
# Autora: Cristina Mongil de la Cal
#
# Figuras del proyecto, siguiendo el patron ggplot2 + reshape2 del proyecto
# de clase. Cada funcion devuelve un objeto ggplot que main_TFM.R guarda
# con guardarFigura(). Los nombres de archivo se corresponden con las
# figuras del Word.

# ---------------------------------------------------------------------
# graficarVentanas()
# Serie de referencia con las ventanas de cada crisis sombreadas y los
# picos y valles marcados (Figura 3.3).
# ---------------------------------------------------------------------
graficarVentanas <- function(referencia, ventanas) {
  agudas <- ventanas[ventanas$fase == "durante", ]
  p <- ggplot2::ggplot(referencia, ggplot2::aes(x = Fecha, y = Referencia)) +
    ggplot2::geom_rect(data = agudas, inherit.aes = FALSE,
                       ggplot2::aes(xmin = fecha_inicio, xmax = fecha_fin,
                                    ymin = -Inf, ymax = Inf),
                       fill = "grey80", alpha = 0.5) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(data = agudas, inherit.aes = FALSE,
                        ggplot2::aes(x = fecha_pico, y = NA_real_), na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = "Referencia (base 100)",
                  title = "Serie de referencia y fases agudas de las crisis") +
    ggplot2::theme_minimal()
  p
}

# ---------------------------------------------------------------------
# graficarPreciosRelativos()
# Precios normalizados a base 100 de los indices seleccionados.
# ---------------------------------------------------------------------
graficarPreciosRelativos <- function(panel_precios, indices = NULL) {
  cols <- if (is.null(indices)) setdiff(names(panel_precios), "Fecha") else indices
  df <- data.frame(Fecha = panel_precios$Fecha)
  for (idx in cols) df[[idx]] <- precioRelativo(panel_precios[[idx]], base = 100)
  largo <- reshape2::melt(df, id.vars = "Fecha", variable.name = "indice", value.name = "precio")
  ggplot2::ggplot(largo, ggplot2::aes(x = Fecha, y = precio, colour = indice)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = "Precio relativo (base 100)",
                  title = "Evolucion de los indices (precios normalizados)") +
    ggplot2::theme_minimal()
}

# ---------------------------------------------------------------------
# graficarCodoSilueta()
# Curvas del metodo del codo (WSS) y de la silueta media por crisis, para
# la seleccion del numero de grupos (Figura 3.4).
# Entradas: curvas (salida$curvas de seleccionarK)
# ---------------------------------------------------------------------
graficarCodoSilueta <- function(curvas) {
  largo <- reshape2::melt(curvas, id.vars = c("crisis", "k"),
                          measure.vars = c("wss", "silueta"),
                          variable.name = "metrica", value.name = "valor")
  etiquetas <- c(wss = "Suma de cuadrados intragrupo (codo)",
                 silueta = "Silueta media")
  largo$metrica <- etiquetas[as.character(largo$metrica)]
  ggplot2::ggplot(largo, ggplot2::aes(x = k, y = valor, colour = crisis)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::facet_wrap(~ metrica, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = unique(largo$k)) +
    ggplot2::labs(x = "Numero de grupos (k)", y = NULL,
                  title = "Seleccion del numero de grupos") +
    ggplot2::theme_minimal()
}

# ---------------------------------------------------------------------
# graficarMapaClusters()
# Proyeccion de los indices de una crisis sobre dos variables, coloreados
# por cluster.
# Entradas: m (matriz estandarizada indices x variables), etiquetas,
#           crisis, vars (par de variables a representar)
# ---------------------------------------------------------------------
graficarMapaClusters <- function(m, etiquetas, crisis,
                                 vars = c("volatilidad", "correlacion_media")) {
  df <- data.frame(indice = rownames(m),
                   x = m[, vars[1]], y = m[, vars[2]],
                   cluster = factor(etiquetas), stringsAsFactors = FALSE)
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, colour = cluster, label = indice)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(vjust = -0.7, size = 3, show.legend = FALSE) +
    ggplot2::labs(x = vars[1], y = vars[2],
                  title = paste("Agrupacion de mercados -", crisis)) +
    ggplot2::theme_minimal()
}

# ---------------------------------------------------------------------
# graficarMigracion()
# Barras apiladas al 100 % con el reparto de los mercados segun el numero
# de cambios de grupo (0, 1 o 2) en cada crisis.
# Numero de cambios de grupo entre fases por indice y crisis.
# ---------------------------------------------------------------------
graficarMigracion <- function(tabla_migracion) {
  df <- tabla_migracion[is.finite(tabla_migracion$n_cambios), ]
  resumen <- as.data.frame(table(crisis = df$crisis, cambios = df$n_cambios),
                           stringsAsFactors = FALSE)
  totales <- stats::aggregate(Freq ~ crisis, data = resumen, FUN = sum)
  names(totales)[2] <- "total"
  resumen <- merge(resumen, totales, by = "crisis")
  resumen$pct <- 100 * resumen$Freq / resumen$total
  etiquetas <- c(crisis2008 = "Financiera (2008)",
                 covid = "COVID-19",
                 crisis2022 = "Geopolitico-energetica (2022)")
  resumen$crisis_lab <- factor(etiquetas[resumen$crisis], levels = etiquetas)
  texto_cambios <- c("0 cambios", "1 cambio", "2 cambios")
  resumen$cambios_lab <- factor(texto_cambios[as.integer(resumen$cambios) + 1],
                                levels = texto_cambios)
  ggplot2::ggplot(resumen, ggplot2::aes(x = crisis_lab, y = pct, fill = cambios_lab)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(pct > 0,
                         paste0(Freq, " (", sub(".", ",", sprintf("%.1f", pct), fixed = TRUE),
                                " %)"), "")),
                       position = ggplot2::position_stack(vjust = 0.5), size = 3.2) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_brewer(palette = "Blues", direction = -1) +
    ggplot2::labs(x = NULL, y = "% de los 19 mercados", fill = NULL,
                  title = "Cambios de grupo entre fases, por crisis") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# ---------------------------------------------------------------------
# graficarComposicionEstructural()
# Composicion de cada grupo segun una variable estructural (desarrollo o
# region), con un panel por crisis. Las barras apiladas muestran cuantos
# mercados de cada categoria reune cada grupo, de modo que la lectura de
# la correspondencia entre los grupos y la variable externa es inmediata.
# ---------------------------------------------------------------------
graficarComposicionEstructural <- function(tabla_composicion, variable = "region") {
  df <- tabla_composicion[tabla_composicion$n > 0, ]
  ggplot2::ggplot(df, ggplot2::aes(x = factor(cluster), y = n, fill = categoria)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ crisis) +
    ggplot2::labs(x = "Grupo", y = "Numero de mercados", fill = variable,
                  title = paste("Composicion de los grupos por", variable)) +
    ggplot2::theme_minimal()
}

# ---------------------------------------------------------------------
# graficarContextoCrisis()
# Evolucion de los activos de contexto dentro de la fase aguda de cada
# crisis, normalizada a base 100 en el inicio de la fase para que los tres
# episodios sean comparables entre si. La linea de referencia marca el
# nivel de partida.
# ---------------------------------------------------------------------
graficarContextoCrisis <- function(precios_contexto, ventanas) {
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
      if (nrow(tramo) < 3) next
      filas[[paste(cr, act)]] <- data.frame(
        crisis = cr, activo = act, Fecha = tramo$Fecha,
        nivel  = precioRelativo(tramo$Precio),
        stringsAsFactors = FALSE)
    }
  }
  df <- do.call(rbind, filas)
  ggplot2::ggplot(df, ggplot2::aes(x = Fecha, y = nivel, colour = activo)) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed") +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ crisis, scales = "free_x") +
    ggplot2::labs(x = NULL, y = "Nivel (base 100 al inicio de la fase aguda)",
                  title = "Activos de contexto durante la fase aguda de cada crisis") +
    ggplot2::theme_minimal()
}
