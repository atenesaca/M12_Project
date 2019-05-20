library(EnhancedVolcano)
geo <- getGEO("gds858", destdir=".")

e <- GDS2eSet(geo, do.log2=TRUE)

groups <- pData(e)[,2]

y <- exprs(e)
design <- model.matrix(~factor(groups))
fit <- lmFit(y, design)
ebayes <- eBayes(fit)

t<- topTable(ebayes, coef = 2, n=1000)
head(ebayes$coefficients[,2])
volcanoplot(ebayes, coef=2, highlight=5, xlim=c(-15,15))


t<- topTable(ebayes, coef = 2, n=1000)
EnhancedVolcano(t,
                lab = rownames(t),
                x = 'logFC',
                y = 'P.Value')
