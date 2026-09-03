# TFM — Clustering de mercados ante crisis de distinta naturaleza

Implementación en R del análisis empírico del Trabajo Fin de Máster
*"Analítica de datos en series temporales financieras"* (Cristina Mongil de
la Cal, Máster en Tecnologías del Sector Financiero: FinTech).

El proyecto identifica, mediante K-Means, los patrones de agrupación de un
conjunto de índices bursátiles internacionales ante tres crisis de
distinta naturaleza (financiera de 2008, COVID-19 de 2020 y
geopolítico-energética de 2022) y los interpreta económicamente. Sigue el
marco CRISP-DM y el flujo de la metodología del capítulo 3 de la memoria:
hipótesis declaradas de antemano → variables de comportamiento → K-Means →
contraste → interpretación → análisis complementarios.

## Finalidad

Reproducir de forma ordenada y auditable todo el análisis descrito en la
memoria, desde la descarga de los datos hasta las tablas y figuras que se
incorporan al TFM.

## Estructura del proyecto

```
TFM-crisis-clustering/
├── TFM-crisis-clustering.Rproj   proyecto de RStudio
├── README.md
├── init.R                        librerías, configuración y carga de módulos
├── config.R                      parámetros y decisiones metodológicas
├── ExtraccionDatos.R             descarga, caché, limpieza, alineación y disponibilidad
├── financialFuns.R               rentabilidades y precios relativos
├── features.R                    variables de comportamiento y estandarización
├── crisisWindows.R               serie de referencia y ventanas pico-a-valle (3.3)
├── clustering.R                  selección de k y K-Means (3.8)
├── hypothesisContrast.R          contraste de hipótesis: Rand ajustado y separabilidad (3.9)
├── structuralProfile.R           perfil estructural externo y caracterización (3.10)
├── complementary.R               análisis complementarios (3.11)
├── plots.R                       figuras
├── exportResults.R               guardado trazable de tablas y figuras
├── main_TFM.R                    SCRIPT PRINCIPAL (se ejecuta de arriba abajo)
├── basedata/                     datos crudos descargados de Yahoo (un CSV por ticker)
├── data/                         objetos intermedios procesados (.rds)
├── profiles/                     perfil económico externo (CSV a cumplimentar)
└── resultados/
    ├── tablas/                   tablas de salida (.csv)
    └── figuras/                  figuras de salida (.png)
```

`init.R`, `config.R` y los tres archivos de núcleo (`ExtraccionDatos.R`,
`financialFuns.R`, `features.R`) siguen la filosofía del proyecto de clase.
El resto de archivos son módulos auxiliares, cada uno correspondiente a un
bloque de la metodología del capítulo 3, cuya función se indica arriba.

## Función de cada archivo

- **init.R** — comprueba e instala los paquetes que falten, carga
  `config.R` y hace `source()` de los módulos de funciones. No ejecuta el
  análisis.
- **config.R** — universo de 19 índices y 3 activos de contexto, fechas de
  descarga, intervalos de las tres crisis, duración de las fases, ventana
  móvil, factor de anualización, rango de k, semilla, número de reinicios,
  umbrales de contraste y rutas. No realiza cálculos.
- **ExtraccionDatos.R** — descarga con `quantmod`, cachea un CSV auditable
  por ticker en `basedata/`, limpia y alinea las series y comprueba su
  disponibilidad histórica.
- **financialFuns.R** — rentabilidades logarítmicas, rentabilidad
  acumulada y anualizada y precios normalizados.
- **features.R** — volatilidad realizada, drawdown máximo y correlación
  media móvil; resumen por fase; construcción del conjunto por índice,
  crisis y fase; y estandarización z-score dentro de cada conjunto que se
  agrupa.
- **crisisWindows.R** — serie de referencia equiponderada y delimitación
  de las ventanas antes/durante/después por el criterio pico-a-valle.
- **clustering.R** — selección de k por codo y silueta, K-Means con
  semilla y reinicios, y descripción de los centroides.
- **hypothesisContrast.R** — índice de Rand ajustado (implementado en R
  base), empleado para comparar particiones entre crisis, y contraste de
  separabilidad.
- **structuralProfile.R** — carga y validación del perfil estructural
  externo (nivel de desarrollo y región) y su cruce con las etiquetas de
  cluster.
- **complementary.R** — cambio entre particiones, migración entre fases,
  separabilidad y reacción-recuperación.
- **plots.R** — figuras del proyecto con `ggplot2`.
- **exportResults.R** — creación de directorios y guardado de tablas y
  figuras con nombres trazables.
- **main_TFM.R** — orquesta el análisis completo en quince bloques
  numerados.

## Diferencia entre `basedata/`, `data/` y `resultados/`

- **basedata/** — datos originales tal como se descargan de Yahoo Finance,
  un CSV por ticker (con las columnas OHLC, cierre ajustado y volumen para
  poder auditar la descarga) y una tabla de equivalencia
  `_equivalencia_tickers.csv` entre el ticker y el nombre de archivo.
- **data/** — objetos intermedios del análisis en formato `.rds` (panel de
  precios, rendimientos, ventanas, variables, etiquetas, centroides…), que
  conservan los tipos de R y evitan recomputar.
- **resultados/** — únicamente salidas listas para revisar o incorporar al
  TFM: `tablas/` (CSV) y `figuras/` (PNG).

## Paquetes necesarios

`quantmod`, `zoo`, `cluster` (viene con R base), `ggplot2` y `reshape2`.
`init.R` instala automáticamente los que falten.

## Cómo ejecutar

1. Abrir `TFM-crisis-clustering.Rproj` en RStudio (fija el directorio de
   trabajo en la raíz del proyecto; todas las rutas son relativas).
2. Abrir `main_TFM.R` y ejecutarlo de arriba abajo con *Source*.

`main_TFM.R` comienza con `source("init.R")` y a continuación recorre los
quince bloques: preparación, descarga, disponibilidad, limpieza y
alineación, serie de referencia, ventanas, rendimientos, variables,
estandarización, selección de k, K-Means, contraste, interpretación,
complementarios y exportación.

## Archivos que se generan

- En `basedata/`: un CSV por ticker y la tabla de equivalencia.
- En `data/`: los objetos intermedios `.rds`.
- En `resultados/tablas/`: disponibilidad histórica, ventanas, correlación
  entre variables, codo y silueta, centroides, asignaciones, contraste de
  hipótesis, cambio entre particiones, migración, separabilidad y
  reacción-recuperación.
- En `resultados/figuras/`: ventanas temporales (Figura 3.3), codo y
  silueta (Figura 3.4), precios relativos, mapas de clusters por crisis y
  migración entre fases.

## Cómo forzar una nueva descarga

La descarga reutiliza los CSV de `basedata/` si son válidos y cubren el
periodo. Para volver a descargar, borrar el CSV correspondiente o llamar a
`descargarActivo(ticker, refrescar = TRUE)` (o `descargarUniverso(tickers,
refrescar = TRUE)`).

## Decisiones metodológicas pendientes

- **Perfil estructural externo (Tabla 3.6).** El nivel de desarrollo
  (clasificación de FTSE Russell) y la región geográfica (esquema M49 de
  Naciones Unidas) son datos externos que se cumplimentan en
  `profiles/perfil_estructural.csv`. Sin ellos, la caracterización
  estructural de los grupos queda pendiente; el resto del análisis se
  ejecuta igualmente.
- **Número de clusters común.** `main_TFM.R` lo calcula a partir del codo
  y la silueta y lo asigna a `K_COMUN`. Conviene confirmar ese valor a la
  vista de la Figura 3.4 antes de dar los resultados por definitivos.
- **DAX e Ibovespa.** Sus tickers (`^GDAXI`, `^BVSP`) corresponden a
  índices de retorno total, frente a los índices de precios del resto. Se
  conserva la especificación original; una eventual sustitución o prueba
  de sensibilidad se decidirá más adelante. Cambiar un ticker se hace solo
  en `config.R`.
- **Cobertura de FTSEMIB.MI.** El bloque de disponibilidad comprueba si
  todos los índices cubren 2008. Si alguno no lo hace, el análisis se
  detiene y pide una decisión expresa, porque excluir un índice altera la
  muestra de 20 definida en el Word.
