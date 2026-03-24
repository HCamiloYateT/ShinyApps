# ShinyApps - Documentación por carpeta

Este repositorio contiene múltiples aplicaciones Shiny. Se agregó un `README.md` específico en cada carpeta de aplicación con:

- propósito funcional de la app,
- comentarios generales del código,
- tipo/framework detectado (por ejemplo, **bs4Dash**, **shinydashboard** o Shiny base),
- dependencias detectadas desde `library()` y `require()`,
- estructura principal de archivos.

## Inventario de aplicaciones

| Carpeta | Tipo detectado | # Dependencias | Ejemplos de dependencias |
|---|---:|---:|---|
| `CRMClienteNacional/CRMNacional` | bs4Dash | 13 | shiny, bs4Dash, dplyr, tidyr, lubridate, plotly… |
| `Calificacion Navieras` | shinydashboard | 14 | shinydashboard, shinyWidgets, shinyjs, tidyverse, dashboardthemes, leaflet… |
| `Ciclos de Operaciones` | shinydashboard | 13 | shinydashboard, shinyWidgets, shinyjs, readxl, tidyverse, dashboardthemes… |
| `Contacto` | Shiny con navbarPage | 14 | shiny, bslib, shinydashboard, shinydashboardPlus, shinyjs, shinyauthr… |
| `CreacionQR` | Shiny con navbarPage | 9 | shiny, bslib, shinyBS, shinyjs, shinybusy, shinyWidgets… |
| `InformeBanRep` | Shiny con navbarPage | 22 | tidyverse, DBI, lubridate, bizdays, openxlsx2, blastula… |
| `Informes de Gestión` | Shiny con navbarPage | 19 | apputils, shiny, bslib, shinydashboard, shinyjs, shinyauthr… |
| `Ingreso Informacion PyG` | Shiny con navbarPage | 16 | shiny, bslib, shinyBS, shinydashboard, shinyjs, shinyWidgets… |
| `Landing` | Shiny (base) | 2 | readxl, tidyverse |
| `Mezcla optima` | shinydashboard | 7 | shinydashboard, shinyjs, dashboardthemes, tidyverse, DT, scales… |
| `Portal de Apps` | Shiny con navbarPage | 8 | shiny, shiny.semantic, modules, shinyjs, shinyauthr, shinyWidgets… |
| `PortalAplicaciones` | Shiny con navbarPage | 11 | shiny, shinyjs, shinyWidgets, tidyverse, lubridate, bslib… |
| `Posición Dólares y Café` | Shiny con navbarPage | 20 | apputils, shinydashboard, bslib, shinyWidgets, shinyjs, readxl… |
| `Sicex` | shinydashboard | 15 | shinydashboard, shinyjs, readxl, tidyverse, dashboardthemes, leaflet… |
| `Usage` | Shiny con navbarPage | 12 | shiny, bslib, shinyBS, shinyjs, shinytoastr, shinyWidgets… |

## Dependencias globales detectadas (unión)

`shiny`, `bs4Dash`, `dplyr`, `tidyr`, `lubridate`, `plotly`, `gt`, `reactable`, `writexl`, `scales`, `stringr`, `racafeModulos`, `kableExtra`, `shinydashboard`, `shinyWidgets`, `shinyjs`, `tidyverse`, `dashboardthemes`, `leaflet`, `DT`, `daterangepicker`, `ggalluvial`, `ggplot2`, `RColorBrewer`, `readxl`, `xts`, `bslib`, `shinydashboardPlus`, `shinyauthr`, `apputils`, `googlesheets4`, `shinyBS`, `shinybusy`, `colourpicker`, `qrcode`, `DBI`, `bizdays`, `openxlsx2`, `blastula`, `rlang`, `forcats`, `dygraphs`, `prophet`, `rgdal`, `lazyeval`, `shinytoastr`, `phosphoricons`, `rhandsontable`, `lpSolve`, `shiny.semantic`, `modules`, `connectwidgets`, `httr`, `racafe`, `RODBC`, `Microsoft365R`, `RQuantLib`, `tictoc`, `ggTimeSeries`

## Nota metodológica

La clasificación y el inventario se generaron de forma automática analizando los archivos `ui.R`, `server.R`, `global.R`, `app.R`, `*.R` y `*.Rmd` de cada carpeta de app.