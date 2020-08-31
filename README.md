En este repositorio hay pequeñas librerias para calcular el CCL a través de Cedears y ADRs de papeles argentinos.

Dentro de las rutinitas en R están las fechas de inicio y fin y luego genera dos csv:

- ADRs_Argentinos/adr_argentinos.csv
- Cedear/cedears.csv
- cc/ccl.csv

Si no se toca el código, calcula los últimos 180 días desde la fecha en la que se ejecuta.

Luego graba el set calculado como "ccl+fecha_inicio-fecha_final.csv" con el formato yyyymmdd en el path Data/ccl/

Luego genera un calculo de CCL "promedio" con Galicia, BMA, YPF y EDN como para tener una referencia y lo baja a: "CCLProm+fecha_inicio-fecha_final.csv"

Finalmente graba un archivo jpg con el gráfico CCL Promedio calculado en la misma carpeta con el siguiente formato de nombre: "CCLPromGraf+fecha_inicio-fecha_final.csv"

