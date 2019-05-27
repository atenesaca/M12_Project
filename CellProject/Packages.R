install.packages("shinydashboard")
install.packages("shinycustomloader")
install.packages("d3heatmap")
install.packages("plotly")
install.packages('DT')
install.packages("shinyWidgets")
install.packages("ggdendro")
install.packages("BiocManager")
install.packages("RMySQL")
install.packages("shinyjs")

BiocManager::install("GEOquery")
BiocManager::install("AnnotationDbi")
if (!requireNamespace("EnhancedVolcano", quietly = TRUE))
    BiocManager::install("EnhancedVolcano")

library(devtools)
install_github("nik01010/dashboardthemes")

# sudo apt-get install -y libmariadb-client-lgpl-dev
