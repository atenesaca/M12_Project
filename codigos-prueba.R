library(EnhancedVolcano)
library(scales)
geo <- getGEO("gds858", destdir=".")

  e <- GDS2eSet(geo, do.log2=TRUE)

class(groups) <- pData(e)[,2]

y <- exprs(e)
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


