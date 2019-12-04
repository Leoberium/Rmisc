# Прочтение и преобразование данных
readata <- read.table(file = "counts5.txt", header = TRUE, sep = " ")

# Удаление строк с нулями
zerows <- apply(readata, 1, function(i) all(i == 0))
readata <- readata[zerows == FALSE, ]

C <- sum(readata[, 1:4])
E <- sum(readata[, 5:8])

tables <- lapply(1:nrow(readata), function(i) {
  mat <- matrix(0, ncol = 2, nrow = 2)
  mat[1, 1] <- sum(readata[i, 1:4])
  mat[2, 1] <- C - mat[1, 1]
  mat[1, 2] <- sum(readata[i, 5:8])
  mat[2, 2] <- E - mat[1, 2]
  return(mat)
})
tables
pvals <- sapply(1:length(tables), function(i) fisher.test(tables[[i]])$p.value)
hist(pvals)
padj <- p.adjust(pvals, method = 'BH')
alpha <- max(pvals[padj < 0.05])
FDR <- alpha*length(tables)/sum(padj < 0.05)
sum(padj < 0.05)

permutator <- function() {
  readata <- readata[sample(1:8)]
  tables <- lapply(1:nrow(readata), function(i) {
    mat <- matrix(0, ncol = 2, nrow = 2)
    mat[1, 1] <- sum(readata[i, 1:4])
    mat[2, 1] <- C - mat[1, 1]
    mat[1, 2] <- sum(readata[i, 5:8])
    mat[2, 2] <- E - mat[1, 2]
    return(mat)
  })
  pvals <- sapply(1:length(tables), function(i) fisher.test(tables[[i]])$p.value)
  ps <- sum(pvals < alpha)
  FDR <- alpha*length(tables)/ps
  return(c(ps, FDR))
}
replicate(10, permutator())
