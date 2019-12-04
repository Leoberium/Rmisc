# Прочтение данных
readata <- read.table(file = "hw6.counts.txt", header = TRUE, sep = " ", check.names = FALSE)
# Удаление генов, где нет экспрессии
zerows <- apply(readata, 1, function(i) all(i == 0))
readata <- readata[zerows == FALSE, ]
# Нормировка на размер библиотеки
readata <- sweep(x = readata, MARGIN = 2, STATS = colSums(readata), FUN = "/")
readata <- readata*10^6 # CPM
# Возраст, ткань
colnames(readata)
ages <- as.numeric(substring(text = colnames(readata), first = 7))
ages2 <- ages^0.5
ages4 <- ages^0.25
tissue <- as.factor(substring(text = colnames(readata), first = 1, last = 5))
# anova(lm)
rawdata <- apply(readata, 1, function(counts) {
  result <- anova(lm(counts ~ tissue + ages2 + ages4 + tissue:ages2 + tissue:ages4))$`Pr(>F)`[1:5]
  names(result) <- c('tissue', 'ages2', 'ages4', 'tissue:ages2', 'tissue:ages4')
  return(result)
})
rawdata <- as.data.frame(t(rawdata))
head(rawdata)
apply(rawdata, 2, function(x) sum(x<0.05)) # до поправки
calcdata <- as.data.frame(apply(rawdata, 2, function(x) p.adjust(p = x, method = 'BH')))
head(calcdata)
apply(calcdata, 2, function(x) sum(x<0.05)) # после поправки
# Пороги p-value
alpha <- sapply(colnames(rawdata), function(name) max(rawdata[, name][calcdata[, name] < 0.05]))
alpha
alpha['tissue'] # 0.0402 - порог p-value для разницы между тканями
# Сколько генов меняют экспрессию между тканями?
a <- sum(calcdata$tissue < 0.05) # 13347
# Сколько генов меняют экспрессию с возрастом?
b <- sum(calcdata$ages2 < 0.05 | calcdata$ages4 < 0.05) # 12656
# Сколько генов меняют экспрессию с возрастом по-разному в разных тканях?
c <- sum(calcdata$`tissue:ages2` < 0.05 | calcdata$`tissue:ages4` < 0.05) # 12692
# Число значимых во всех 3 категориях
gstats <- c(a, b, c)
names(gstats) <- c('tissue', 'age', 'tissue-age')
# Пермутации
permutator <- function() {
  pdata <- as.data.frame(t(apply(readata[, sample(1:ncol(readata))], 1, function(counts) {
    result <- anova(lm(counts ~ tissue + ages2 + ages4 + tissue:ages2 + tissue:ages4))$`Pr(>F)`[1:5]
    names(result) <- c('tissue', 'ages2', 'ages4', 'tissue:ages2', 'tissue:ages4')
    return(result)
  })))
  a <- sum(pdata$tissue < alpha['tissue'])
  b <- sum(pdata$ages2 < alpha['ages2'] | pdata$ages4 < alpha['ages4'])
  c <- sum(pdata$`tissue:ages2` < alpha['tissue:ages2'] | pdata$`tissue:ages4` < alpha['tissue:ages4'])
  result <- c(a, b, c)
  names(result) <- c('tissue', 'age', 'tissue-age')
  return(c(a, b, c))
}
(permstats <- replicate(100, permutator()))
permstats <- t(permstats)
colnames(permstats) <- c('tissue', 'age', 'tissue-age')
permstats
gstats # 13347, 12656, 12692 - результаты из правильно размеченных групп
apply(permstats, 2, max) # 4575, 5185, 6718 - макс. из пермутаций
apply(permstats, 2, median) # 339, 559, 620 - медианы из пермутаций
# В каждой из групп при пермутациях доля значимых генов значительно ниже, чем в случае без пермутаций.
# Таким образом, полученный результат неслучаен и действительно показывает гены, которые значимо меняют экспрессию. 