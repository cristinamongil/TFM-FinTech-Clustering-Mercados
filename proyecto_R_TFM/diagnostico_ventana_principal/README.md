# Diagnóstico de la ventana móvil principal

**Comprobación de sensibilidad. No forma parte del análisis oficial del TFM
y no modifica ningún resultado.**

El script solo *lee* código y objetos del pipeline principal y escribe
exclusivamente dentro de `diagnostico_ventana_principal/`.

## Qué se compara

- **Escenario A (oficial).** Volatilidad y correlación con ventana de 63
  filas del panel común de los 19 mercados. Es exactamente lo que hace
  `features.R` con `N_VENTANA_MOVIL <- 63L`.
- **Escenario B (sensibilidad).** Idéntico en todo salvo que la ventana son
  **tres meses naturales** terminados en cada fecha *t*. Solo cambian
  volatilidad y correlación; la rentabilidad anualizada y el drawdown no se
  tocan.

Todo lo demás se mantiene: mismo panel, mismos 19 mercados, mismas crisis,
fases, picos y valles, mismas cuatro variables, misma estandarización por
crisis y fase, mismo K-Means con *k* = 3, misma semilla (123), mismos 50
reinicios y la misma regla determinista de alineación de etiquetas.

No se ha probado ninguna otra longitud de ventana. Tres meses naturales es
la única alternativa evaluada, para comprobar qué habría ocurrido si la
descripción «aproximadamente un trimestre» hubiera sido literal.

## Cómo se ejecuta

Desde la raíz del proyecto, con el pipeline principal ya ejecutado:

```r
Rscript diagnostico_ventana_principal/scripts/diagnostico_ventana.R
```

Solo necesita R base. La silueta se reimplementa con su definición estándar
porque el paquete `cluster` no está disponible en el entorno de ejecución;
la fórmula es la misma que usa `cluster::silhouette`.

## Archivos generados

| Archivo | Contenido |
|---|---|
| `B_tamano_ventana_3meses.csv` | Número de observaciones comunes dentro de una ventana de tres meses naturales |
| `comparacion_A_vs_B_fase_aguda.csv` | Mercados que conservan grupo, ARI A vs B, siluetas, y composición del grupo de mayor correlación media |
| `centroides_A_vs_B.csv` | Centroides de los tres grupos en cada escenario y crisis |
| `pertenencia_A_vs_B.csv` | Grupo de cada mercado en A y en B (etiquetas alineadas), con región y nivel de desarrollo |
| `rand_entre_crisis_A_vs_B.csv` | Índice de Rand ajustado entre pares de crisis en ambos escenarios |
| `migraciones_A_vs_B.csv` | Porcentaje de mercados sin cambios entre fases en cada escenario |
| `migraciones_detalle_A_vs_B.csv` | Número de cambios de grupo por mercado y crisis en A y en B |
| `arrastre_entre_fases_escenarioA.csv` | Qué parte de cada ventana móvil pertenece a la propia fase y qué parte procede de antes |

## Advertencia

Estas tablas son material de diagnóstico. Ninguna de sus cifras debe
sustituir a las del capítulo 4 ni citarse como resultado del trabajo.
