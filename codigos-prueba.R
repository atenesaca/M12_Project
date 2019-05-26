library(EnhancedVolcano)
library(scales)
if (!requireNamespace("topGO", quietly = TRUE))
  BiocManager::install("topGO")
library(topGO)
library(stringr)
geo <- getGEO("gds858")

matriz <- GDS2eSet(geo, do.log2=TRUE)

groups <- pData(matriz)[,2]
groups<- make.names(groups)
### older ###
y <- exprs(matriz)
dim(y)[1]
length(rownames(y))
design <- model.matrix(~ groups)
fit <- lmFit(y, design) 
ebayes <- eBayes(fit)

## newer ##

contrast <- function(x){
  x <- make.names(x)
  num <- 1
  gro <- as.character(x)
  vec <- unique(gro)
  cont <- length(vec)
  h <- ""
  v <- vector()
  m <- vector()
  for (i in 1:cont){
    v[i] <- vec[i]
  }
  for (i in 1:cont){
    for (j in cont:i){
      if(v[i] != v[j]){
        h <- paste(v[i],v[j], sep = "-")
        m[num] <- h
        num <- num+1
      }
    }
  }
  m
}

gru <- contrast(groups)


groups<- make.names(groups)
vect <- unique(groups)
fac <- factor(groups,levels=vect)
design <- model.matrix(~ 0 + fac)
colnames(design) <- vect
gru <- gru
df <- lmFit(y,design)
contrast <- makeContrasts(contrasts = gru,levels=design)
datafitcon <-  contrasts.fit(df,contrast)
ebayes <-  eBayes(datafitcon)

colnames(ebayes$coefficients)
t<- topTable(ebayes, coef = 2, n=1000)
volcanoplot(ebayes, coef=2, highlight=5)

## enhanced
x<-"control-motile"
t<- ?topTable(ebayes, coef = x, adjust = "fdr", n=length(rownames(y)))
EnhancedVolcano(t,
                lab = rownames(t),
                title = paste0("Fold changes for this contrast group: ", x),
                x = 'logFC',
                y = 'P.Value')

str(ebayes)
