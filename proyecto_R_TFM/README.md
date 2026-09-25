# TFM — Clustering de mercados bursátiles ante crisis de distinta naturaleza

Implementación en R del análisis empírico del Trabajo Fin de Máster
*«Análisis comparativo del comportamiento de los mercados bursátiles ante
crisis mediante técnicas de clustering»* (Cristina Mongil de la Cal, Máster
Universitario en Tecnologías del Sector Financiero: FinTech, UC3M, 2025-2026).

El proyecto agrupa mediante K-Means 19 índices bursátiles internacionales
en la fase aguda de tres crisis de distinta naturaleza (financiera de 2008,
COVID-19 de 2020 y geopolítico-energética de 2022) y compara las
agrupaciones obtenidas. Sigue el marco CRISP-DM y el flujo del capítulo 3
de la memoria: expectativas formuladas de antemano → variables de
comportamiento → K-Means → evaluación de las expectativas → caracterización
estructural → análisis complementarios.

## Estructura del proyecto

```
TFM-crisis-clustering/
├── TFM-crisis-clustering.Rproj   proyecto de RStudio
├── README.md
├── main_TFM.R                    SCRIPT PRINCIPAL (se ejecuta de arriba abajo)
├── init.R                        librerías, configuración y carga de módulos
├── config.R                      parámetros y decisiones metodológicas
├── ExtraccionDatos.R             descarga, caché, limpieza, alineación y disponibilidad
├── financialFuns.R               rentabilidades y precios relativos
├── features.R                    variables de comportamiento y estandarización (3.5)
├── crisisWindows.R               serie de referencia y ventanas pico-a-valle (3.3)
├── clustering.R                  selección de k, K-Means y alineación de etiquetas (3.8, 3.11)
├── expectations.R                evaluación de las expectativas y Rand ajustado (3.9)
├── structuralProfile.R           perfil estructural externo y caracterización (3.10)
├── complementary.R               análisis complementarios (3.11)
├── seedCheck.R                   comprobación con diez semillas alternativas (3.8)
├── plots.R                       figuras
├── exportResults.R               guardado trazable de tablas y figuras
├── basedata/                     datos descargados de Yahoo Finance (un CSV por ticker)
├── data/                         objetos intermedios procesados (.rds)
├── profiles/                     perfil estructural de cada mercado (desarrollo y región)
└── resultados/
    ├── tablas/                   tablas de salida (.csv)
    └── figuras/                  figuras de salida (.png)
```

## Función de cada archivo

- **main_TFM.R** — orquesta el análisis completo en diecisiete bloques
  numerados, desde la preparación del entorno hasta la comprobación de
  semillas.
- **init.R** — comprueba e instala los paquetes que falten, carga
  `config.R` y hace `source()` de los módulos de funciones. No ejecuta el
  análisis.
- **config.R** — universo de 19 índices y 3 activos de contexto, fechas de
  descarga, intervalos de búsqueda de las tres crisis, duración de las
  fases, ventana móvil de tres meses naturales, factor de anualización,
  rango de k, semilla, número de reinicios, umbrales del análisis de
  reacción y recuperación y rutas. No realiza cálculos.
- **ExtraccionDatos.R** — descarga con `quantmod`, guarda un CSV auditable
  por ticker en `basedata/`, limpia y alinea las series en fechas comunes
  de negociación y comprueba su disponibilidad histórica.
- **financialFuns.R** — rentabilidades logarítmicas, rentabilidad
  anualizada y precios normalizados.
- **features.R** — volatilidad realizada y correlación media sobre una
  ventana móvil retrospectiva de tres meses naturales, drawdown máximo,
  resumen por fase y estandarización (z-score) dentro de cada conjunto que
  se agrupa.
- **crisisWindows.R** — serie de referencia equiponderada y delimitación de
  las fases anterior, aguda y posterior por el criterio pico-a-valle.
- **clustering.R** — selección de k por codo y silueta, K-Means con semilla
  fija y 50 reinicios, descripción de los centroides y alineación de
  etiquetas entre fases (se prueban las seis correspondencias posibles y,
  en caso de empate, se elige la de mayor suma de índices de Jaccard).
- **expectations.R** — evaluación descriptiva de las expectativas (silueta
  media y tamaño de los grupos) e índice de Rand ajustado para comparar las
  particiones de crisis distintas.
- **structuralProfile.R** — carga del perfil estructural externo (nivel de
  desarrollo según FTSE Russell y región según el esquema M49 de Naciones
  Unidas) y cruce con los grupos.
- **complementary.R** — cambio entre particiones, migración entre fases,
  separabilidad, reacción y recuperación (con su resumen por crisis) y
  comportamiento de los activos de contexto.
- **seedCheck.R** — repite el agrupamiento de la fase aguda con diez
  semillas alternativas y lo compara con el del análisis. Se ejecuta al
  final de `main_TFM.R`, aunque también puede lanzarse por separado.
- **plots.R** — figuras del proyecto con `ggplot2`.
- **exportResults.R** — creación de directorios y guardado de tablas y
  figuras con nombres trazables.

## Diferencia entre `basedata/`, `data/` y `resultados/`

- **basedata/** — datos originales tal como se descargan de Yahoo Finance,
  un CSV por ticker, y la tabla `_equivalencia_tickers.csv` entre el ticker
  y el nombre de archivo.
- **data/** — objetos intermedios del análisis en formato `.rds`.
- **resultados/** — salidas finales: `tablas/` (CSV) y `figuras/` (PNG).

## Paquetes necesarios

`quantmod`, `zoo`, `cluster` (incluido en R), `ggplot2` y `reshape2`.
`init.R` instala automáticamente los que falten.

## Cómo ejecutar

1. Abrir `TFM-crisis-clustering.Rproj` en RStudio (fija el directorio de
   trabajo en la raíz del proyecto; todas las rutas son relativas).
2. Abrir `main_TFM.R` y ejecutarlo de arriba abajo con *Source*.

Los datos se leen de `basedata/`, por lo que la ejecución reproduce los
resultados de la memoria sin necesidad de volver a descargarlos. Para
forzar una nueva descarga, borrar el CSV correspondiente o llamar a
`descargarUniverso(tickers, refrescar = TRUE)`; en ese caso Yahoo Finance
puede haber revisado algún dato histórico y los resultados podrían variar
ligeramente.

## Correspondencia entre salidas y memoria

| Archivo en `resultados/tablas/` | Memoria |
|---|---|
| `tabla_disponibilidad_historica.csv` | Anexo A |
| `tabla_3_3_ventanas.csv` | Tabla 4.1 y Fig. 3.3 |
| `tabla_correlacion_variables.csv` | Tabla 4.2 |
| `tabla_codo_silueta.csv` | Tabla 4.3 y Anexo E |
| `tabla_centroides.csv` | Tabla 4.4 |
| `tabla_asignaciones_cluster.csv` | Anexo B |
| `tabla_evaluacion_expectativas.csv` | Apartado 4.4 |
| `tabla_composicion_desarrollo.csv`, `tabla_composicion_region.csv`, `tabla_cluster_vs_perfil.csv` | Tablas 4.5 y 4.6, Fig. 4.5 |
| `tabla_cambio_entre_particiones.csv` | Tabla 4.7 |
| `tabla_migracion_entre_fases.csv` | Tabla 4.8, Fig. 4.6 y Anexo C |
| `tabla_resumen_reaccion_recuperacion.csv` | Tabla 4.9 |
| `tabla_reaccion_recuperacion.csv` | Anexo D |
| `tabla_contexto_crisis.csv` | Tabla 4.10 |
| `tabla_separabilidad.csv` | Apartado 4.8 |
| `comprobacion_semillas.csv` | Apartado 3.8 |
| `tabla_diagnostico_ventana_movil.csv` | Apartado 3.5 (observaciones por ventana) |

## Notas metodológicas

- **Ventana móvil.** La volatilidad y la correlación media se calculan
  sobre una ventana retrospectiva de tres meses naturales aplicada al panel
  de fechas comunes (unas cuarenta observaciones de media).
- **Anualización.** Se emplea un factor de 252 sesiones. Como el
  agrupamiento se realiza sobre variables estandarizadas, este factor actúa
  solo como escala y no influye en los grupos.
- **DAX y Bovespa.** Sus tickers (`^GDAXI`, `^BVSP`) corresponden a
  índices de rentabilidad total, frente a los índices de precios del resto;
  la memoria (apartado 4.8) señala que los resultados no muestran un
  comportamiento común entre ambos que pueda relacionarse claramente con
  esta diferencia.
- **Empate en la alineación de etiquetas.** En la crisis de 2022, dos
  correspondencias entre la fase anterior y la aguda dejan el mismo número
  de mercados en su grupo; el desempate por el índice de Jaccard se informa
  por consola durante la ejecución.
