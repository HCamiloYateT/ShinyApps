# Posición Dólares y Café

## ¿Qué hace esta app?
Esta carpeta contiene una aplicación **Shiny con navbarPage** orientada a procesos de negocio internos de Racafé. El nombre funcional detectado es **Sacos por Fijar**.

## Comentarios generales del código
- La estructura sigue el patrón clásico de Shiny (`ui.R`, `server.R`, `global.R` cuando aplica).
- En varias apps se utiliza carpeta `www/` para recursos estáticos (logos, estilos CSS y gifs de carga).
- Se observan módulos y scripts auxiliares en `misc/`, `modules/` o `tabs/` para separar responsabilidades.
- Recomendación: centralizar validaciones de entrada y manejo de errores para hacer más robusto el mantenimiento.

## Tipo de app
- **Framework principal:** Shiny con navbarPage
- **Entrada principal detectada:** `ui.R` + `server.R`

## Dependencias detectadas
- `apputils`
- `shinydashboard`
- `bslib`
- `shinyWidgets`
- `shinyjs`
- `readxl`
- `tidyverse`
- `dplyr`
- `dashboardthemes`
- `DT`
- `scales`
- `plotly`
- `lubridate`
- `RColorBrewer`
- `RODBC`
- `Microsoft365R`
- `rhandsontable`
- `bizdays`
- `RQuantLib`
- `tictoc`

## Estructura principal
- `Preparacion de Datos.R` (archivo)
- `global.R` (archivo)
- `server.R` (archivo)
- `ui.R` (archivo)
- `www` (carpeta)

## Notas
- Este README fue generado automáticamente a partir del código fuente de la carpeta para facilitar su revisión inicial.
