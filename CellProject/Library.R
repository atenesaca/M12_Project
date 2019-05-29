library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(Biobase)
library(GEOquery)
library(limma)
library("reshape2")
library(ggplot2)
library(dplyr)
library(limma)
library(annotate)
library(shinycustomloader)
library(shinyjs)
library(plotly)
library(d3heatmap)
library(DBI)
# library(digest)
if (!requireNamespace("EnhancedVolcano", quietly = TRUE))
    BiocManager::install("EnhancedVolcano")
library(EnhancedVolcano)
if(!require("ggdendro")){
    install.packages("ggdendro")
}
library(ggdendro)
if(!require("dashboardthemes")){
  library(devtools)
  install_github("nik01010/dashboardthemes", force=TRUE)
}
library(dashboardthemes)


