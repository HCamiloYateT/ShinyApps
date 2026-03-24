# Portal de Apps

## ¿Qué hace esta app?
Esta carpeta contiene una aplicación **Shiny con navbarPage** orientada a procesos de negocio internos de Racafé. El nombre funcional detectado es **Ingrese sus Credenciales**.

## Comentarios generales del código
- La estructura sigue el patrón clásico de Shiny (`ui.R`, `server.R`, `global.R` cuando aplica).
- En varias apps se utiliza carpeta `www/` para recursos estáticos (logos, estilos CSS y gifs de carga).
- Se observan módulos y scripts auxiliares en `misc/`, `modules/` o `tabs/` para separar responsabilidades.
- Recomendación: centralizar validaciones de entrada y manejo de errores para hacer más robusto el mantenimiento.

## Tipo de app
- **Framework principal:** Shiny con navbarPage
- **Entrada principal detectada:** `ui.R` + `server.R`

## Dependencias detectadas
- `shiny`
- `shiny.semantic`
- `modules`
- `shinyjs`
- `shinyauthr`
- `shinyWidgets`
- `tidyverse`
- `bslib`

## Estructura principal
- `global.R` (archivo)
- `server.R` (archivo)
- `ui.R` (archivo)
- `www` (carpeta)

## Notas
- Este README fue generado automáticamente a partir del código fuente de la carpeta para facilitar su revisión inicial.
