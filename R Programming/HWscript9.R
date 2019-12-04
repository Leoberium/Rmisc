# Прочтение данных
readata <- read.table(file = "hw6.counts.txt", header = TRUE, sep = " ", check.names = FALSE)
# Удаление генов, где нет экспрессии
zerows <- apply(readata, 1, function(i) all(i == 0))
readata <- readata[zerows == FALSE, ]
# Нормировка (для каждого гена, а не образца)
readata <- apply(readata, 2, function(i) i/sum(i))
readata <- sweep(x = readata, MARGIN = 1, STATS = rowMeans(readata), FUN = "-")
readata <- sweep(x = readata, MARGIN = 1, STATS = apply(readata, 1, sd), FUN = "/")
# Проверка, что среднее - 0, стд. отклонение - 1
all(rowMeans(readata) < 1e-15)
all(abs(apply(readata, 1, sd) - 1) < 1e-15)
# Проверка данных
ncol(readata) # 113
nrow(readata) # 16571
braininds <- grepl("brain", colnames(readata))
liverinds <- grepl("liver", colnames(readata)) 
sum(braininds); sum(liverinds) # 56 brain; 57 liver
all(!is.nan(readata))
all(!is.na(readata))
# PCA
pcaobj <- prcomp(t(readata))
dim(pcaobj$x)
pcaobj$x
var <- round(pcaobj$sdev/sum(pcaobj$sdev)*100, 1)
# Больше возраст - больше точка
ages <- log(as.integer(substring(text = colnames(readata), first = 7)), base = 8)
names(ages) <- colnames(readata)
# Brain - blue, liver - red. Чем больше возраст, тем темнее точка
cols <- rgb(0, 0, 3 - ages[braininds], names = names(ages[braininds]), 
            maxColorValue = 2) # blue - brain
cols <- c(cols, rgb(3 - ages[liverinds], 0, 0, names = names(ages[liverinds]), 
                     maxColorValue = 2)) # red - liver
# Brain - pch=19, liver - pch=15
pchs <- ifelse(braininds, 19, 15)
names(pchs) <- colnames(readata)
par(mfrow = c(2, 2))
plot(pcaobj$x[, 1:2], col = cols, pch = pchs, cex = ages, main = "PCA 1-2",
     xlab = paste0('PC1 (', var[1], '%)'),
     ylab = paste0('PC2 (', var[2], '%)'))
plot(pcaobj$x[, 3:4], col = cols, pch = pchs, cex = ages, main = "PCA 3-4",
     xlab = paste0('PC3 (', var[3], '%)'),
     ylab = paste0('PC4 (', var[4], '%)'))
# MDS
cormat <- cor(readata)
# mds <- cmdscale(dist(t(readata)), k = 4)
mds <- cmdscale(1-cormat, k = 4)
plot(mds[, 1:2], col = cols, pch = pchs, cex = ages, main = "MDS 1-2",
     xlab = "MDS 1", ylab = "MDS 2")
plot(mds[, 3:4], col = cols, pch = pchs, cex = ages, main = "MDS 3-4",
     xlab = "MDS 3", ylab = "MDS 4")
heatmap(cormat, symm = TRUE, col = rev(heat.colors(100)), 
        distfun = function(x) as.dist(1-x),
        RowSideColors = cols, ColSideColors = cols)
# T-test
pvals <- apply(readata, 1, function(x) t.test(x[braininds], x[liverinds])$p.value)
(alpha <- max(pvals[p.adjust(pvals, method = "BH") < 0.05])) # 0.038 - порог
genes <- pvals < alpha
sum(genes) # 12675 - значимых генов
permutator <- function() {
  permvals <- apply(readata[, sample(1:ncol(readata))], 1,
                    function(x) t.test(x[braininds], x[liverinds])$p.value)
  return(sum(permvals < alpha))
}
permsign <- replicate(100, permutator())
permsign
hist(permsign, 25)
min(permsign) # 31
max(permsign) # 6153
mean(permsign) # 568.32
median(permsign) # 225.5
# При FDR<0.05 полученный результат в 12675 значимых генов значим, так как
# он явно не из распределения, полученного в пермутациях (случайные группы образцов).
