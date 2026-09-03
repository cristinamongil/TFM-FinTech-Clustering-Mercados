# main_TFM.R
# TFM - Analitica de datos en series temporales financieras
# Comportamiento de los mercados financieros ante crisis de distinta naturaleza
#
# Autora: Cristina Mongil de la Cal
# Tutor:  Fernando Fernandez Rebollo
#
# Script principal. Se ejecuta de arriba abajo (Source). Reproduce el flujo
# completo de la metodologia del capitulo 3, desde la obtencion de los datos
# hasta la interpretacion economica y los analisis complementarios. Cada
# bloque indica que hace y que objetos principales genera.

# 1. Preparacion del entorno
# Carga librerias, configuracion y modulos de funciones, y declara las
# hipotesis ANTES de observar los grupos (blinda el contraste). Genera:
# HIPOTESIS.
source("init.R")

HIPOTESIS <- list(
  crisis2008 = list(
    enunciado = "Mayor sincronia en el nucleo de mercados desarrollados.",
    contraste = "composicion_estructural"),
  covid = list(
    enunciado = "Shock sincronizado: baja separabilidad y correlaciones altas generalizadas.",
    contraste = "separabilidad"),
  crisis2022 = list(
    enunciado = "Correspondencia estructural debil: el eje no se alinea con el desarrollo ni la region.",
    contraste = "composicion_estructural")
)
saveRDS(HIPOTESIS, file.path(RUTA_DATOS, "hipotesis.rds"))

# 2. Descarga y almacenamiento de los datos
# Descarga (o reutiliza de basedata/) los 19 indices y los 3 activos de
# contexto desde Yahoo Finance. Genera: series_crudas, CSV en basedata/.
todos_tickers <- c(TICKERS_INDICES, TICKERS_CONTEXTO)
message("Descargando/leyendo ", length(todos_tickers), " activos...")
series_crudas <- descargarUniverso(todos_tickers, refrescar = FALSE)
saveRDS(series_crudas, file.path(RUTA_DATOS, "series_crudas.rds"))

# 3. Comprobacion de disponibilidad historica
# Limpia las series de indices, comprueba que cada uno cubre 2008 y aplica
# la regla de conjunto comun. Si algun indice no cubre 2008 y no se permite
# la exclusion automatica, el analisis se detiene para pedir una decision.
# Genera: tabla de disponibilidad y tickers_validos.
series_indices_limpias <- lapply(series_crudas[TICKERS_INDICES], limpiarSerie)
names(series_indices_limpias) <- TICKERS_INDICES
disponibilidad <- comprobarDisponibilidad(series_indices_limpias)
guardarTabla(disponibilidad, "tabla_disponibilidad_historica")

tickers_validos <- disponibilidad$ticker[disponibilidad$cubre_2008 %in% TRUE]
retirados <- setdiff(TICKERS_INDICES, tickers_validos)
if (length(retirados) > 0) {
  message("Indices que no cubren 2008: ", paste(retirados, collapse = ", "))
  if (!PERMITIR_EXCLUSION_AUTOMATICA) {
    stop("Excluir estos indices altera la muestra de 19 definida en el Word. ",
         "Revisa la tabla de disponibilidad y, si aceptas la exclusion, ",
         "pon PERMITIR_EXCLUSION_AUTOMATICA <- TRUE en config.R.")
  }
  message("Se retiran del estudio completo (PERMITIR_EXCLUSION_AUTOMATICA = TRUE).")
}
saveRDS(tickers_validos, file.path(RUTA_DATOS, "tickers_validos.rds"))

# 4. Limpieza y alineacion de las series
# Construye el panel de precios de los indices validos alineado en fechas
# comunes de negociacion. Los activos de contexto se limpian y se guardan
# aparte (calendario estadounidense). Genera: panel_precios, precios_contexto.
panel_precios <- construirPanelPrecios(series_indices_limpias[tickers_validos],
                                       metodo = "interseccion")
saveRDS(panel_precios, file.path(RUTA_DATOS, "panel_precios.rds"))
message("Panel de indices: ", nrow(panel_precios), " fechas x ",
        ncol(panel_precios) - 1, " indices.")

precios_contexto <- lapply(series_crudas[TICKERS_CONTEXTO], limpiarSerie)
names(precios_contexto) <- TICKERS_CONTEXTO
saveRDS(precios_contexto, file.path(RUTA_DATOS, "precios_contexto.rds"))

# 5. Construccion de la serie de referencia
# Media equiponderada de los indices en precios normalizados; sirve para
# datar picos y valles. Genera: referencia.
referencia <- construirReferencia(panel_precios)
saveRDS(referencia, file.path(RUTA_DATOS, "referencia.rds"))

# 6. Definicion de las ventanas de crisis
# Fechas de las fases antes, durante y despues de cada crisis a partir del
# criterio pico-a-valle. Genera: ventanas.
ventanas <- definirVentanas(referencia)
saveRDS(ventanas, file.path(RUTA_DATOS, "ventanas.rds"))
guardarTabla(ventanas, "tabla_3_3_ventanas")

# 7. Calculo de rendimientos logaritmicos
# Rendimientos logaritmicos diarios del panel de indices. Genera:
# panel_rendimientos.
indices <- setdiff(names(panel_precios), "Fecha")
panel_rendimientos <- data.frame(Fecha = panel_precios$Fecha[-1])
for (idx in indices) panel_rendimientos[[idx]] <- rendimientosLog(panel_precios[[idx]])
saveRDS(panel_rendimientos, file.path(RUTA_DATOS, "panel_rendimientos.rds"))

# 8. Calculo de las variables de comportamiento
# Cuatro variables por indice, crisis y fase, y comprobacion exploratoria
# de la colinealidad rentabilidad-drawdown en la fase aguda. Genera:
# variables, tabla de correlacion entre variables.
guardarTabla(tamanoVentanaMovil(panel_precios$Fecha),
             "tabla_diagnostico_ventana_movil")
variables <- calcularVariablesComportamiento(panel_precios, ventanas)
saveRDS(variables, file.path(RUTA_DATOS, "variables.rds"))

matriz_cor <- cor(variables[COLS_VARIABLES], use = "complete.obs")
guardarTabla(as.data.frame(round(matriz_cor, 3)), "tabla_correlacion_variables")
aguda <- variables[variables$fase == "durante", ]
message("Correlacion rentabilidad-drawdown en fase aguda: ",
        round(cor(aguda$rent_anual, aguda$drawdown, use = "complete.obs"), 3))

# 9. Estandarizacion de las variables
# Estandariza (z-score) dentro de cada conjunto que se agrupa (crisis y
# fase). Genera: variables_std.
variables_std <- estandarizarVariables(variables)
saveRDS(variables_std, file.path(RUTA_DATOS, "variables_std.rds"))

# 10. Seleccion del numero de clusters
# Codo y silueta en las tres crisis (fase aguda) y propuesta de k comun.
# El valor se calcula a partir de los datos; conviene confirmarlo a la
# vista de la Figura 3.4. Genera: seleccion_k, K_COMUN, figura del codo/silueta.
seleccion_k <- seleccionarK(variables_std)
guardarTabla(seleccion_k$curvas, "tabla_codo_silueta")
guardarFigura(graficarCodoSilueta(seleccion_k$curvas), "figura_3_4_codo_silueta")
K_COMUN <- seleccion_k$k_propuesto
message("k comun propuesto (silueta media maxima): ", K_COMUN)

# 11. Aplicacion de K-Means
# Agrupamiento por crisis (fase aguda, analisis principal) y por crisis y
# fase (soporte de los complementarios), con semilla y multiples reinicios.
# Genera: res_crisis, res_crisis_fase, tabla de centroides.
res_crisis      <- ejecutarKMeans(variables_std, k = K_COMUN, por = "crisis")
res_crisis_fase <- ejecutarKMeans(variables_std, k = K_COMUN, por = "crisis_fase")
saveRDS(res_crisis,      file.path(RUTA_DATOS, "etiquetas_crisis.rds"))
saveRDS(res_crisis_fase, file.path(RUTA_DATOS, "etiquetas_crisis_fase.rds"))

centroides_todos <- do.call(rbind, lapply(names(res_crisis), function(cr) {
  d <- describirCentroides(res_crisis[[cr]]$centroides); d$crisis <- cr; d
}))
guardarTabla(centroides_todos, "tabla_centroides")

asignaciones <- do.call(rbind, lapply(names(res_crisis), function(cr) {
  e <- res_crisis[[cr]]$etiquetas
  data.frame(crisis = cr, indice = names(e), cluster = as.integer(e), stringsAsFactors = FALSE)
}))
guardarTabla(asignaciones, "tabla_asignaciones_cluster")

# 12. Contraste de las hipotesis
# La COVID se evalua por separabilidad; 2008 y 2022, de forma descriptiva
# a partir de la composicion estructural de cada grupo (bloque 13). El
# Rand ajustado se reserva para comparar particiones ENTRE crisis
# (bloque 14). Genera: tabla de contraste.
perfil <- cargarPerfil()
contraste <- list()
for (cr in names(res_crisis)) {
  e <- res_crisis[[cr]]$etiquetas
  m <- matrizCluster(variables_std, cr, "durante")
  if (HIPOTESIS[[cr]]$contraste == "separabilidad") {
    r <- contrastarSeparabilidad(m, e)
    contraste[[cr]] <- data.frame(crisis = cr, tipo = "separabilidad",
      estadistico = round(r$silueta_media, 3), veredicto = r$veredicto,
      stringsAsFactors = FALSE)
  } else {
    contraste[[cr]] <- data.frame(crisis = cr, tipo = "composicion_estructural",
      estadistico = round(siluetaMedia(m, e), 3),
      veredicto = "evaluacion descriptiva: vease la composicion estructural",
      stringsAsFactors = FALSE)
  }
}
contraste <- do.call(rbind, contraste)
guardarTabla(contraste, "tabla_contraste_hipotesis")
print(contraste)

# 13. Caracterizacion estructural de los clusters
# Cruce de los grupos con el perfil estructural externo (nivel de
# desarrollo y region geografica), si esta disponible.
# Genera: tabla de cruce y tablas de composicion estructural, y la figura
# de composicion regional.
if (!is.null(perfil)) {
  cruce <- do.call(rbind, lapply(names(res_crisis), function(cr) {
    tab <- cruzarPerfilClusters(res_crisis[[cr]]$etiquetas, perfil)
    if (!is.null(tab)) { tab$crisis <- cr; tab }
  }))
  if (!is.null(cruce)) guardarTabla(cruce, "tabla_cluster_vs_perfil")

  composicion <- list()
  for (variable in c("desarrollo", "region")) {
    tab <- do.call(rbind, lapply(names(res_crisis), function(cr) {
      x <- composicionEstructural(res_crisis[[cr]]$etiquetas, perfil, variable)
      if (!is.null(x)) { x$crisis <- cr; x$variable <- variable; x }
    }))
    if (!is.null(tab)) {
      composicion[[variable]] <- tab
      guardarTabla(tab, paste0("tabla_composicion_", variable))
    }
  }
  if (!is.null(composicion[["region"]])) {
    guardarFigura(graficarComposicionEstructural(composicion[["region"]], "region"),
                  "figura_composicion_regional")
  }
} else {
  message("Caracterizacion estructural pendiente: cumplimenta profiles/",
          ARCHIVO_PERFIL, ".")
}

# 14. Analisis complementarios y de robustez
# Cambio entre particiones, migracion entre fases, separabilidad y
# reaccion-recuperacion. Genera: tablas y figura de migracion.
tabla_cambio        <- cambioEntreParticiones(res_crisis)
tabla_migracion     <- migracionEntreFases(res_crisis_fase)
tabla_separabilidad <- separabilidadGrupos(res_crisis)
tabla_reaccion      <- reaccionRecuperacion(panel_precios, ventanas)

guardarTabla(tabla_cambio,        "tabla_cambio_entre_particiones")
guardarTabla(tabla_migracion,     "tabla_migracion_entre_fases")
guardarTabla(tabla_separabilidad, "tabla_separabilidad")
guardarTabla(tabla_reaccion,      "tabla_reaccion_recuperacion")
guardarFigura(graficarMigracion(tabla_migracion), "figura_migracion_fases")

# 15. Activos de contexto durante las fases agudas
# Comportamiento del oro, el petroleo y la deuda publica estadounidense en
# la fase aguda de cada crisis, con las mismas ventanas oficiales. No
# intervienen en el agrupamiento y no alteran ninguna salida previa: sirven
# para caracterizar el entorno de cada episodio. Genera: tabla y figura de
# los activos de contexto.
tabla_contexto <- comportamientoContexto(precios_contexto, ventanas)
guardarTabla(tabla_contexto, "tabla_contexto_crisis")
guardarFigura(graficarContextoCrisis(precios_contexto, ventanas),
              "figura_contexto_crisis")
print(tabla_contexto)

# 16. Generacion y exportacion de tablas y figuras
# Figuras finales de referencia, agrupaciones y precios relativos. Las
# tablas se han guardado en cada bloque en resultados/tablas.
guardarFigura(graficarVentanas(referencia, ventanas), "figura_3_3_ventanas_temporales")
guardarFigura(graficarPreciosRelativos(panel_precios), "figura_precios_relativos")
for (cr in names(res_crisis)) {
  m <- matrizCluster(variables_std, cr, "durante")
  guardarFigura(graficarMapaClusters(m, res_crisis[[cr]]$etiquetas, cr),
                paste0("figura_clusters_", cr))
}

message("main_TFM.R completado. Tablas en ", RUTA_TABLAS, " y figuras en ", RUTA_FIGURAS, ".")
