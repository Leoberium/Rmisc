# Лев Мазаев

# Новая функция
countWords <- function(string, k) {
  # разбиение на вектор букв
  ones <- unlist(strsplit(string, ""))
  # индексы подслов
  indices <- seq_len(length.out = length(ones) - k + 1) 
  # получение подслов
  subWords <- sapply(indices, function(i) paste0(ones[i:(i + k - 1)], collapse = ""))
  # частоты подслов
  return(table(subWords))
}

# Старая функция
countWordsSlow <- function(string, k) {
  subWords <- substring(string, 1:(nchar(string)- k + 1), k:nchar(string))
  return(table(subWords))
}

# Функция, генерирующая текст для сравнения
randomText <- function(alphabet, length) {
  return(paste0(sample(alphabet, length, replace = TRUE), collapse = ""))
}

atgc1e5 <- randomText(c("A", "C", "T", "G"), 1e5)

# Новая функция значительно быстрее
system.time(countWords(atgc1e5, 5))
system.time(countWordsSlow(atgc1e5, 5))

# Загрузка генома e.coli
library(ape)
ecoli <- paste0(read.dna("sequence.fasta", format = "fasta", as.character = TRUE), collapse = "")

# Наблюдаемые частоты всех слов длины 6
observedFreqs <- countWords(ecoli, 6)
sort(observedFreqs, decreasing = TRUE)[c(1, 2, 4095, 4096)]

# Функция расчёта ожидаемых частот слов заданной длины
estimateWords <- function(string, k) {
  # количество слов заданной длины
  numWords <- nchar(string) - k + 1
  # буквы и их вероятности
  probLetters <- table(unlist(strsplit(string, "")))/nchar(string)
  stringLetters <- names(probLetters)
  # комбинации букв и их вероятностей
  preWords <- expand.grid(lapply(1:k, function(i) stringLetters))
  preProbs <- expand.grid(lapply(1:k, function(i) probLetters))
  # слова заданной длины и их вероятности
  probWords <- apply(preProbs, 1, prod)
  names(probWords) <- apply(preWords, 1, paste0, collapse = "")
  # частоты слов заданной длины
  freqWords <- probWords * numWords
  return(as.table(freqWords))
}

# Ожидаемые частоты всех слов длины 6
estimatedFreqs <- estimateWords(ecoli, 6)

# Отношение наблюдаемой и ожидаемой частот
ratioWords <- sort(observedFreqs/estimatedFreqs)
# 5 самых больших и самых малых частот
ratioWords[-(6:4091)]

# Другая функция расчёта ожидаемых частот
estimateWords2 <- function(string, k) {
  # эта часть аналогична предыдущей функции
  numWords <- nchar(string) - k + 1
  probLetters <- table(unlist(strsplit(string, "")))/nchar(string)
  stringLetters <- names(probLetters)
  # слова заданной длины и их вероятности
  probWords <- as.vector(Reduce(outer, lapply(2:k, function(i) probLetters), probLetters))
  names(probWords) <- as.vector(Reduce(function(...) outer(FUN = paste0, ...), lapply(2:k, function(i) stringLetters), stringLetters))
  # частоты слов заданной длины
  freqWords <- probWords * numWords
  return(as.table(freqWords))
}

# Отношение наблюдаемой и ожидаемой частот
ratioWords2 <- sort(observedFreqs/estimateWords(ecoli, 6))
# 5 самых больших и самых малых частот
ratioWords2[-(6:4091)]

# Результат такой же:
identical(ratioWords, ratioWords2)

# Ответ:
# cctagg     ctagac     tcctag     cctaga     tctagg     ccagcg     gctggc     gccagc     ctggcg     cgccag 
# 0.01868764 0.01984544 0.02067750 0.02143307 0.02168838 3.85619702 3.87310684 4.03130716 4.32755341 4.44040068

# Ещё одна функция оценки частот, но это симуляция через sample, поэтому ответ всегда разный и неверный
simulateWords <- function(string, k) {
  numWords <- nchar(string) - k + 1
  probLetters <- table(unlist(strsplit(string, "")))/nchar(string)
  stringLetters <- names(probLetters) 
  wordCreator <- function() {
    preWord <- sample(stringLetters, k, replace = TRUE, prob = probLetters)
    return(paste0(preWord, collapse = ""))
  }
  subWords <- replicate(numWords, wordCreator())
  return(table(subWords))
}
# По времени работает ~ countWords
simulatedFreqs <- simulateWords(ecoli, 6)
ratioWords3 <- sort(observedFreqs/simulatedFreqs)
ratioWords3[-(6:4091)]
