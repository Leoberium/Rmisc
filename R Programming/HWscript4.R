# Лев Мазаев

randomtext <- function(alphabet, length) {
  return(paste0(sample(alphabet, length, replace = TRUE), collapse = ""))
}

atgc1e5 <- randomtext(c("A", "T", "G", "C"), 100000)
# Сколько раз встречается каждая буква?
table(strsplit(atgc1e5, ""))

countwords <- function(string, lsw) {
  subwords <- substring(string, 1:(nchar(string)- lsw + 1), lsw:nchar(string))
  return(sort(table(subwords), decreasing = TRUE))
}

countwords("abbba", 2)
countwords(atgc1e5, 3)

library(ape)
setwd("~/R_L/HSE/")
ecoli <- paste0(read.dna("sequence.fasta", format = "fasta", as.character = TRUE), collapse = "")

# Нужна более быстрая функция:
countwords2 <- function(string, lsw) {
  ones <- unlist(strsplit(string, ""))
  names(ones) <- 1:length(ones)
  subword <- function(name) {
    i <- as.integer(name)
    if (i <= length(ones) - lsw + 1) return(paste0(ones[i:(i + lsw - 1)], collapse = ""))
  }
  return((sort(table(unlist(lapply(names(ones), subword))), decreasing = TRUE)))
}

# Получилось значительно быстрее:
system.time(countwords(atgc1e5, 3))
system.time(countwords2(atgc1e5, 3))

ans <- countwords2(ecoli, 6)[c(1, 2, 4095, 4096)]
names(ans) <- toupper(names(ans))
ans
# Ответ:
# CGCCAG - 5883
# CTGGCG - 5666
# CTAGAC - 25
# CCTAGG - 24

# Ещё можно воспользоваться функцией str_sub из пакета stringr:
library(stringr)
countwords3 <- function(string, lsw) {
  return(sort(table(str_sub(string, 1:(nchar(ecoli) - lsw + 1), lsw:nchar(ecoli)))))
}
# Так получается быстрее всего:
countwords3(ecoli, 6)[c(1, 2, 4095, 4096)]
