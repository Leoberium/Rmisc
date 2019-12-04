# Лев Мазаев
# Прочтение и преобразование данных
readata <- read.table(file = "counts5.txt", header = TRUE, sep = " ")
readata <- sweep(x = readata, MARGIN = 2, STATS = colSums(readata), FUN = "/")
readata <- readata*10^6 # CPM
head(readata)

# Удаление строк с нулями
zerows <- apply(readata, 1, function(i) all(i == 0))
readata <- readata[zerows == FALSE, ]

# Т-тест
pvals <- sapply(1:nrow(readata), function(i) t.test(readata[i, 1:4], readata[i, 5:8])$p.value)
hist(pvals)
pnum <- sum(pvals < 0.05)
falsediscoveryrate <- 0.05*nrow(readata)/pnum
truenum <- floor(pnum*(1-falsediscoveryrate))
# Получено 1511 генов с p < 0.05, с учётом FDR=0.496 значимых генов (без ложных) оказывается 761

# Перестановки
permutator <- function() {
  inds <- sample(1:8)
  permutedata <- readata[inds]
  permutepvals <- sapply(1:nrow(permutedata), function(i) {
    t.test(permutedata[i, 1:4], permutedata[i, 5:8])$p.value
  })
  return(sum(permutepvals < 0.05))
}
permnums <- replicate(100, permutator())
min(permnums)
max(permnums[permnums != pnum])
sum(permnums != pnum)
hist(permnums, 80)
abline(v = pnum, col = "red")
# При перестановках столбцов число генов с pvalue < 0.05 снижается до
# 450-650, что говорит о том, что результат, полученный при правильно расставленных
# столбцах - не случайный. Результат, полученный при правильных группах, значительно отклоняется
# от распределения результатов, полученных при неправильных группах.
# Возможен случай, когда столбцы при использовании sample выстроились в 
# правильном порядке, тогда снова получается 1511. 