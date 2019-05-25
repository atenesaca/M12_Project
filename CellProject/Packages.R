install.packages("shinydashboard")
install.packages("shinycustomloader")
install.packages("d3heatmap")
install.packages("plotly")
install.packages('DT')
install.packages("shinyWidgets")
install.packages("ggdendro")
install.packages("BiocManager")

source("http://bioconductor.org/biocLite.R")
biocLite("GEOquery")

BiocManager::install("AnnotationDbi")
BiocManager::install("EnhancedVolcano")

library(devtools)
install_github("nik01010/dashboardthemes")
