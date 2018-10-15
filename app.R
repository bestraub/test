## 2018-26-09 bstraub
## Tagesmitteltemperatur

# install.packages("webshot",repos = "http://cran.us.r-project.org")

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(htmlwidgets))
suppressPackageStartupMessages(library(dygraphs))
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(webshot))

source('plotfunctions.R')

source('data_import.R')


source("dygraph-extra-shiny.R") # zum Abspeichern von dygraphs als png


source('myui.R')
source('myserver.R')

shinyApp(ui=ui,server=server)

