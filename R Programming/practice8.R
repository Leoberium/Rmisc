genN <- matrix(rnorm(50000), ncol = 5)
genC <- rbind(matrix(rnorm(2500, 3), ncol = 5), matrix(rnorm(47500), ncol = 5))
pvals <- sapply(1:10000, function(i) t.test(genN[i, ], genC[i, ])$p.value)
alpha <- 0.05
sn <- sum(pvals < alpha)
hist(pvals)
falspositiverate <- alpha*10000/sn

pvalsBH <- p.adjust(pvals, method = "BH")
(snBH <- sum(pvalsBH < 0.05))
(alphaBH <- 0.05*snBH/10000)
max(pvals[pvalsBH < 0.05])
(falspositiverateBH <- alphaBH*10000/snBH)

pvalsBO <- p.adjust(pvals, method = "bo")
(snBO <- sum(pvalsBO < 0.05))
(alphaBO <- 0.05*snBO/10000)
max(pvals[pvalsBO < 0.05])
(falspositiverateBO <- alphaBO*10000/snBO)

sigrows <- pvalsBH < 0.05
effsize <- rowMeans(genC[sigrows, ]) - rowMeans(genN[sigrows, ])
hist(effsize, 30)

genN2 <- matrix(rnorm(50000), ncol = 5)
genC2 <- rbind(matrix(rnorm(2500, 3), ncol = 5), matrix(rnorm(47500), ncol = 5))
pvals2 <- sapply(1:10000, function(i) t.test(genN2[i, ], genC2[i, ])$p.value)
pvalsBH2 <- p.adjust(pvals2, method = "BH") 

sigrows2 <- pvalsBH2 < 0.05
sum(sigrows2)
sum(sigrows)

sum(sigrows == TRUE & sigrows2 == TRUE)

badrows <- pvals < 0.05
badrows2 <- pvals2 < 0.05
effsize2 <- rowMeans(genC[badrows, ]) - rowMeans(genN[badrows, ])
hist(effsize, 30)
hist(effsize2, 30)
sum(sigrows[1:500])
