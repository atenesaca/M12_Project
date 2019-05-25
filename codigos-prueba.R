library(EnhancedVolcano)
library(scales)
library(tidyverse)
if (!requireNamespace("topGO", quietly = TRUE))
  BiocManager::install("topGO")
library(topGO)
geo <- getGEO("gds858")

matriz <- GDS2eSet(geo, do.log2=TRUE)

groups <- pData(matriz)[,2]

y <- exprs(matriz)
design <- model.matrix(~ groups)
fit <- lmFit(y, design) 
ebayes <- eBayes(fit)
t<- topTable(ebayes, coef = 2, n=1000)
?topTable
volcanoplot(ebayes, coef=2, highlight=5)

## enhanced
t<- topTable(ebayes, coef = 2, adjust = "fdr", n=1000)
EnhancedVolcano(t,
                lab = rownames(t),
                x = 'logFC',
                y = 'P.Value')

geneID <- rownames(t)
geneList <- factor(as.integer(t))
names(geneList) <- geneID

top_GO_data <- new("topGOdata", ontology = "BP", allGenes = geneID, nodeSize = 10)
