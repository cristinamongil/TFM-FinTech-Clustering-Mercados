# Análisis exploratorio con ventanas móviles

**Simulación exploratoria. No forma parte del análisis oficial del TFM.**

Esta carpeta es completamente **aditiva**: el script solo *lee*
`data/panel_precios.rds` y `data/ventanas.rds` del pipeline principal y
escribe exclusivamente dentro de `exploratorio_ventanas/`. No sobrescribe
ningún CSV, PNG ni RDS del proyecto original.

## Pregunta que se explora

Dentro de cada crisis, ¿algunos mercados modifican su comportamiento antes
que otros?

El análisis es **descriptivo y ex post**. No es un modelo predictivo, no
mide causalidad y no permite hablar de anticipación ni de indicadores
adelantados. Aunque cada ventana usa únicamente información anterior o
igual a su fecha final, los regímenes se identifican al final del proceso
utilizando la trayectoria completa del episodio, de modo que detectar un
cambio antes del pico **no** significa que el mercado lo anticipara.

## Metodología

1. **Ventanas móviles retrospectivas.** Para cada mercado y cada fecha *t*
   se construye una observación con las *w* sesiones que terminan en *t*
   (información ≤ *t*). Especificación principal: *w* = 63 sesiones, la
   misma longitud que usa el TFM para volatilidad y correlación.

2. **Cuatro variables por ventana**, adaptadas al nuevo diseño:
   - `rent_anual` = suma de rendimientos logarítmicos de la ventana ÷ *w* × 252.
   - `volatilidad` = desviación típica de esos rendimientos × √252.
   - `drawdown` = `min(P / cummax(P) − 1)` sobre los precios de la ventana.
   - `correlacion_media` = media de las correlaciones por pares del mercado
     con los otros 18, calculadas sobre los rendimientos de la ventana.

   A diferencia del pipeline principal, la rentabilidad y el drawdown se
   calculan **dentro de la ventana** y no sobre la fase completa, que es lo
   que corresponde matemáticamente a este diseño.

3. **Periodo de cada crisis** = fase anterior + aguda + posterior, con las
   fechas de pico y valle que ya fija `data/ventanas.rds`. No se redefine
   ninguna crisis.

4. **K-Means por mercado y crisis** sobre las ventanas estandarizadas
   (z-score dentro de cada conjunto), con *k* = 3, `nstart` = 50 y semilla
   fijada por mercado y crisis.

5. **Identificación del régimen de tensión.** Regla determinista sobre los
   centroides estandarizados: se elige el clúster que maximiza
   `volatilidad + correlacion_media − rent_anual − drawdown`. Las etiquetas
   numéricas de K-Means no se usan como criterio. El solapamiento con la
   fase aguda del pipeline principal se calcula y se guarda como
   comprobación en el archivo de diagnóstico.

   *Nota:* una primera versión identificaba el régimen por su solapamiento
   con la fase aguda. Ese criterio falla cuando la fase aguda es corta
   respecto a la ventana, como ocurre en la COVID-19, y seleccionaba el
   régimen de calma. Por eso el criterio definitivo se basa en los
   centroides.

6. **Fecha de cambio** = primera entrada sostenida en ese régimen, es decir,
   la primera racha de al menos *p* sesiones consecutivas. Principal
   *p* = 5; comprobación con *p* = 10.

**Unidades del desfase.** El panel común solo conserva las fechas en que
negocian los 19 mercados a la vez, unas 154 sesiones al año frente a las
~252 de una bolsa individual. Por eso las tablas dan el desfase respecto al
pico en tres unidades: sesiones del panel común, días naturales y sesiones
del calendario propio de cada mercado.

7. **Sensibilidad:** *w* ∈ {42, 63, 84} × *p* ∈ {5, 10}. 63 y 5 son la
   especificación principal; el resto sirve solo para comprobar estabilidad.

## Archivos generados

| Archivo | Contenido |
|---|---|
| `tables/fechas_cambio_v63_p5.csv` | Especificación principal: fecha de cambio y desfase respecto al pico en tres unidades (sesiones del panel común, días naturales y sesiones del propio mercado), antes/después, régimen y duración de la racha |
| `tables/sensibilidad_ventana_persistencia.csv` | Las seis combinaciones de ventana y persistencia |
| `tables/resumen_por_crisis.csv` | Primero, segundo, tercero y último en cambiar, dispersión temporal y número de mercados que cambian antes del pico |
| `tables/diagnostico_regimenes_v63_p5.csv` | Centroides de los tres regímenes de cada mercado, puntuación de tensión, cuál se selecciona y su solapamiento con la fase aguda |
| `figures/timeline_<crisis>.png` | Cronología de cambios por mercado, con línea vertical en el pico |
| `outputs/parametros_ejecucion.csv` | Parámetros, semilla, número de índices y de fechas, fecha de ejecución |

## Cómo reproducirlo

Desde la raíz del proyecto, con el pipeline principal ya ejecutado (deben
existir `data/panel_precios.rds` y `data/ventanas.rds`):

```r
Rscript exploratorio_ventanas/scripts/ventanas_moviles.R
```

Solo necesita R base: usa `stats::kmeans` y `grDevices::png`, sin paquetes
externos. Con las semillas fijadas, dos ejecuciones producen resultados
idénticos.
