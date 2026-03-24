# Landing

## ¿Qué hace esta app?
Esta carpeta contiene una aplicación **Shiny (base)** orientada a procesos de negocio internos de Racafé. El nombre funcional detectado es **Landing**.

## Comentarios generales del código
- La estructura sigue el patrón clásico de Shiny (`ui.R`, `server.R`, `global.R` cuando aplica).
- En varias apps se utiliza carpeta `www/` para recursos estáticos (logos, estilos CSS y gifs de carga).
- Se observan módulos y scripts auxiliares en `misc/`, `modules/` o `tabs/` para separar responsabilidades.
- Recomendación: centralizar validaciones de entrada y manejo de errores para hacer más robusto el mantenimiento.

## Tipo de app
- **Framework principal:** Shiny (base)
- **Entrada principal detectada:** `app.R`

## Dependencias detectadas
- `readxl`
- `tidyverse`

## Estructura principal
- `Aplicaciones.xlsx` (archivo)
- `Landing.Rproj` (archivo)
- `Noticias.xlsx` (archivo)
- `app.R` (archivo)
- `index.html` (archivo)
- `www` (carpeta)

## Notas
- Este README fue generado automáticamente a partir del código fuente de la carpeta para facilitar su revisión inicial.
