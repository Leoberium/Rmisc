setwd('~/ME/HSE/Project/')
library(stringr)
library(dplyr)
library(purrr)
library(doMC)
library(ggplot2)
library(pROC)
registerDoMC(7)

# data loading
df <- read.table(file = '~/BioData/DGRP/GSE101827_ATAC_raw_counts_31_DGRP_mergedPeaks.tsv',
           header = TRUE, sep = '\t', check.names = FALSE, stringsAsFactors = FALSE)
df_regions <- df$`Chr:Start-End`
df$`Chr:Start-End`

# library size normalization
raw_counts <- as.matrix(df[2:32])
norm_counts <- sweep(x = raw_counts, MARGIN = 2,
                     STATS = 1e-6 * colSums(raw_counts), FUN = '/')
norm_counts

# obtaining regions
pre <- str_split(string = df_regions, pattern = ':', simplify = TRUE)
chromosomes <- pre[, 1]
starts <- as.integer(str_split(string = pre[, 2], pattern = '-', simplify = TRUE)[, 1])
ends <- as.integer(str_split(string = pre[, 2], pattern = '-', simplify = TRUE)[, 2])
bed_df <- data.frame(chr = paste0('chr', chromosomes), start = starts, end = ends,
                     id = paste0('id', seq_len(length.out = length(chromosomes))))
write.table(x = bed_df, file = 'foo.bed', sep = '\t', quote = FALSE,
            row.names = FALSE, col.names = FALSE)

# filtering low counts
norepeats_bed <- read.table('atac_regions_wo_repeats.bed', sep = '\t', stringsAsFactors = FALSE)
regID_norepeats <- as.integer(str_sub(norepeats_bed$V4, start = 3))
regID_norepeats
norepeats_lens <- ends[regID_norepeats] - starts[regID_norepeats]
c_filtered <- apply(sweep(norm_counts[regID_norepeats, ], 1, norepeats_lens, '/'), 1,
      function(x) sum(x < 0.03467) > 30 )
reads_per_bp <- sweep(raw_counts[regID_norepeats, ], 1, norepeats_lens, '/')
head(reads_per_bp)
c_filtered_2 <- apply(reads_per_bp, 1, function(x) sum(x < 0.2) >= 30)
used_lines <- regID_norepeats[!c_filtered_2]
sum(c_filtered_2)
write.table(x = norepeats_bed[!c_filtered_2, ], file = 'filtered_regions.bed',
            sep = '\t', quote = FALSE, row.names = FALSE, col.names = FALSE)
norm_filtered_counts <- norm_counts[regID_norepeats, ][!c_filtered_2, ]
norm_filtered_counts <- as.data.frame(norm_filtered_counts)
norm_filtered_counts$id <- used_lines
# write.table(x = norm_filtered_counts, file = 'normalized_counts.txt',
#            row.names = FALSE)
norm_filtered_counts <- read.table(file = 'normalized_counts.txt',
                                   header = TRUE, check.names = FALSE)

# calling snps in regions
snpmat <- read.table(file = 'FINAL_FINAL_FINAL.bed', sep = ',', stringsAsFactors = FALSE, na.strings = 'None')
colnames(snpmat) <- c('chr', 'regstart', 'regend', 'regID', 'pos', '195',
                 '208', '235', '26', '358', '360', '362', '40', '427',
                 '437', '491', '509', '551', '57', '630', '639', '737',
                 '748', '820', '821', '897', '907', '913', 'ref', 'alt')
snpmat$'snpID' <- paste0(str_sub(snpmat$chr, start = 4), '_', snpmat$pos, '_SNP')

snps_to_filter_out <- apply(snpmat[6:28], 1, function(row) sum(is.na(row)) > 5)
sum(snps_to_filter_out)
snpmat <- snpmat[!snps_to_filter_out, ]
head(snpmat)

correspondence <- read.table(file = 'correspondence.txt', sep = '-', stringsAsFactors = FALSE)
colnames(correspondence) <- c('dgrp', 'line')
correspondence

dgrp_order <- as.integer(str_sub(string = colnames(norm_filtered_counts)[-32], start = 1, end = 5))
dgrp_order

line_order <- sapply(dgrp_order, function(x) correspondence[correspondence$dgrp == x, 2])
line_order

snpidlist <- seq_len(length.out = nrow(snpmat))
names(snpidlist) <- snpmat$snpID
rownames(norm_filtered_counts) <- paste0('id', norm_filtered_counts$id)
head(snpmat)

glms <- lapply(snpidlist, function(id) {
  lin <- snpmat[id, ]
  regID <- lin$regID
  lin <- lin[6:28]
  x <- unlist(lin[, as.character(line_order)])
  y <- unlist(norm_filtered_counts[regID, -32])
  glm(formula = y ~ x, family = 'quasipoisson')
})
p_values <- sapply(glms, function(model) anova(model, test = 'Chisq')$`Pr(>Chi)`[2])

sum(is.na(p_values))
p_values <- p_values[!is.na(p_values)]
fdr <- 0.05
adjusted_p_values <- p.adjust(p = p_values, method = 'BH')
sum(adjusted_p_values < fdr)


# unsignificant_regions
unsignificant <- adjusted_p_values[adjusted_p_values > 0.95]
unsignificant_regions <- unique(snpmat$regID[snpmat$snpID %in% names(unsignificant)])
length(unsignificant_regions)
snp_in_unsignificant <- snpmat[snpmat$snpID %in% names(unsignificant), c(4, 5, 29, 30)]
snp_in_unsignificant$pos <- snp_in_unsignificant$pos - snpmat$regstart[snpmat$snpID %in% names(unsignificant)]
snp_in_unsignificant
# write.table(x = snp_in_unsignificant, file = 'snp_unsig.in', sep = '\t',
#            quote = FALSE, row.names = FALSE, col.names = FALSE)
unsig_regions_to_extract <- unique(snpmat[snpmat$regID %in% unsignificant_regions, c(1, 2, 3, 4)])
# write.table(x = unsig_regions_to_extract, file = 'unsig_sequences_extraction.bed', sep = '\t',
#            quote = FALSE, row.names = FALSE, col.names = FALSE)

# significant regions
significant <- adjusted_p_values[adjusted_p_values < fdr]
significant_regions <- unique(snpmat$regID[snpmat$snpID %in% names(significant)])
length(significant_regions)
snp_in <- snpmat[snpmat$snpID %in% names(significant), c(4, 5, 29, 30)]
snp_in$pos <- snp_in$pos - snpmat$regstart[snpmat$snpID %in% names(significant)]
snp_in
# write.table(x = snp_in, file = 'snp.in', sep = '\t', quote = FALSE, row.names = FALSE, col.names = FALSE)
regions_to_extract <- unique(snpmat[snpmat$regID %in% significant_regions, c(1, 2, 3, 4)])
# write.table(x = regions_to_extract, file = 'sig_sequences_extraction.bed', sep = '\t', quote = FALSE,
#            row.names = FALSE, col.names = FALSE)

# reading/writing significant/unsignificant snps
write.table(x = significant, file = 'significant_snps.txt')
write.table(x = unsignificant, file = 'unsignificant_snps.txt')
significant <- read.table(file = 'significant_snps.txt')
s <- significant$x
names(s) <- rownames(significant)
significant <- s
unsignificant <- read.table(file = 'unsignificant_snps.txt')
s <- unsignificant$x
names(s) <- rownames(unsignificant)
unsignificant <- s

# reading/writing significant/unsignificant regions
significant_regions <- read.table(file = 'seqtk_pipe/sig_sequences_extraction.bed',
                                  sep = '\t', stringsAsFactors = FALSE)
significant_regions <- significant_regions$V4
unsignificant_regions <- read.table(file = 'seqtk_pipe/unsig_sequences_extraction.bed',
                                    sep = '\t', stringsAsFactors = FALSE)
unsignificant_regions <- unsignificant_regions$V4

# accessible - 1, closed - 0
signs <- sapply(names(significant), function(x) glms[[x]]$coefficients[2])
signs
names(signs) <- str_replace(string = names(signs), pattern = '.x', replacement = '')
access <- sapply(significant_regions, function(name) {
  snpss <- snpmat$snpID[snpmat$regID == name]
  snpss <- snpss[snpss %in% names(signs)]
  change <- signs[snpss]
  access <- sum(change[change > 0])
  close <- abs(sum(change[change < 0]))
  if (access > close) {
    ans <- c(0, 1)
  } else {
    ans <- c(1, 0)
  }
  names(ans) <- c('ref', 'mut')
  return(ans)
})
access <- t(access)
write.table(x = access, file = 'access.txt')
access
access <- read.table('access.txt')

# cluster-buster results for significant ones
mut <- read.table(file = '~/BioData/DGRP/cbust/sig/mut_crm_fullcut.bed', header = TRUE, sep = '\t', stringsAsFactors = FALSE, comment.char = "$")
ref <- read.table(file = '~/BioData/DGRP/cbust/sig/ref_crm_fullcut.bed', header = TRUE, sep = '\t', stringsAsFactors = FALSE, comment.char = "$")

region_motifs <- lapply(significant_regions, function(name) {
  submut <- mut[mut$seq_name == name & mut$cluster_or_motif == 'motif', ]
  mut_motifs <- unique(submut$cluster_id_or_motif_name)
  subref <- ref[ref$seq_name == name & ref$cluster_or_motif == 'motif', ]
  ref_motifs <- unique(subref$cluster_id_or_motif_name)
  motifs <- intersect(mut_motifs, ref_motifs)
  if (access[name, 1] == 1 & access[name, 2] == 0) {
    result <- sapply(motifs, function(motif) {
      mut_score <- max(submut$cluster_or_motif_score[submut$cluster_id_or_motif_name == motif])
      ref_score <- max(subref$cluster_or_motif_score[subref$cluster_id_or_motif_name == motif])
      ref_score - mut_score
    })
  } else {
    result <- sapply(motifs, function(motif) {
      mut_score <- max(submut$cluster_or_motif_score[submut$cluster_id_or_motif_name == motif])
      ref_score <- max(subref$cluster_or_motif_score[subref$cluster_id_or_motif_name == motif])
      mut_score - ref_score
    })
  }
})

names(region_motifs) <- significant_regions
x1 <- unique(mut$cluster_id_or_motif_name[mut$cluster_or_motif == 'motif'])
x2 <- unique(ref$cluster_id_or_motif_name[ref$cluster_or_motif == 'motif'])
motif_names <- sort(x1)
write.table(x = motif_names, file = 'motif_names.txt')
motif_vector <- numeric(length = length(motif_names))
names(motif_vector) <- motif_names

sapply(region_motifs, function(obj) {
  for (name in names(obj)) {
    motif_vector[name] <<- motif_vector[name] + obj[name]
  }
})

# cluster-buster results for unsignificant ones
unsig_mut <- read.table(file = '~/BioData/DGRP/cbust/unsig/unsig_mut_crm_cut.bed', header = TRUE, sep = '\t', stringsAsFactors = FALSE, comment.char = "$")
unsig_ref <- read.table(file = '~/BioData/DGRP/cbust/unsig/unsig_ref_crm_cut.bed', header = TRUE, sep = '\t', stringsAsFactors = FALSE, comment.char = "$")
motif_names
vertical1c <- sapply(motif_names, function(name) {
  mut_max <- aggregate(formula = cluster_or_motif_score ~ seq_name,
                       data = mut,
                       FUN = max,
                       subset = cluster_id_or_motif_name == name)
  ref_max <- aggregate(formula = cluster_or_motif_score ~ seq_name,
                       data = ref,
                       FUN = max,
                       subset = cluster_id_or_motif_name == name)
  merged <- merge(mut_max, ref_max, by = 'seq_name')
  delta1 <- abs(merged[, 3] - merged[, 2])
  l11 <- sum(delta1 > 3); l12 <- sum(delta1 <= 3)
  mut_max <- aggregate(formula = cluster_or_motif_score ~ seq_name,
                       data = unsig_mut,
                       FUN = max,
                       subset = cluster_id_or_motif_name == name)
  ref_max <- aggregate(formula = cluster_or_motif_score ~ seq_name,
                       data = unsig_ref,
                       FUN = max,
                       subset = cluster_id_or_motif_name == name)
  merged <- merge(mut_max, ref_max, by = 'seq_name')
  delta2 <- abs(merged[, 3] - merged[, 2])
  l21 <- sum(delta2 > 3); l22 <- sum(delta2 <= 3)
  m <- rbind(c(l11, l12), c(l21, l22))
  p <- fisher.test(m)$p.value
  return(-log(p))
})

#### test ####
x <- aggregate(formula = cluster_or_motif_score ~ seq_name,
          data =  mut,
          FUN = max,
          subset = cluster_id_or_motif_name == 'elemento-AACCGGTT')
y <- aggregate(formula = cluster_or_motif_score ~ seq_name,
               data = ref,
               FUN = max,
               subset = cluster_id_or_motif_name == 'elemento-AACCGGTT')
z <- aggregate(formula = cluster_or_motif_score ~ seq_name,
               data = unsig_mut,
               FUN = max,
               subset = cluster_id_or_motif_name == 'elemento-AACCGGTT')
k <- aggregate(formula = cluster_or_motif_score ~ seq_name,
               data =  unsig_ref,
               FUN = max,
               subset = cluster_id_or_motif_name == 'elemento-AACCGGTT')
test <- merge(x, y, by.x = 'seq_name', by.y = 'seq_name')
delta1 <- abs(test$cluster_or_motif_score.y - test$cluster_or_motif_score.x)
l11 <- sum(delta1 > 3)
l12 <- sum(delta1 <= 3)
test2 <- merge(z, k, by = 'seq_name')
delta2 <- abs(test2[, 3] - test2[, 2])
l21 <- sum(delta2 > 3)
l22 <- sum(delta2 <= 3)
res <- fisher.test(rbind(c(l11, l12), c(l21, l22)))
-log(res$p.value)
#### ####
# fig1C
motif_vector <- round(motif_vector, 3)
motif_vector
library(ggplot2)
df <- data.frame(x = motif_vector, y = vertical1c)
ggplot(df, aes(x = x, y = y)) +
  geom_point()
write.table(x = df, file = 'figure1C.txt')
write.table(sort(motif_vector), file = 'deltaCRM.txt', sep = ',', row.names = FALSE, col.names = FALSE)

# trying to obtain fig1D
new_access <- as.data.frame(access)
new_access$regID <- rownames(access)
new_access
x <- tidyr::gather(new_access, key = 'type', value = 'predicted', ref, mut)
head(x)
pred <- x$predicted

res <- sapply(motif_names, function(name) {
  subref <- ref[ref$cluster_id_or_motif_name == name, ]
  refvec <- numeric(length = length(significant_regions))
  for (i in 1:length(significant_regions)) {
    regid <- significant_regions[i]
    if (regid %in% subref$seq_name) {
      score <- max(subref$cluster_or_motif_score[subref$seq_name == regid])
      if (score > 4) refvec[i] <- 1
    }
  }
  submut <- mut[mut$cluster_id_or_motif_name == name, ]
  mutvec <- numeric(length = length(significant_regions))
  for (i in 1:length(significant_regions)) {
    regid <- significant_regions[i]
    if (regid %in% submut$seq_name) {
      score <- max(submut$cluster_or_motif_score[submut$seq_name == regid])
      if (score > 4) mutvec[i] <- 1
    }
  }
  obs <- c(refvec, mutvec)
  roc1 <- roc(response = obs, predictor = pred)
  auc05 <- roc1$auc - 0.5
  roc2 <- roc(response = obs, predictor = sample.int(2, 6406, replace = TRUE) - 1)
  obj <- roc.test(roc1, roc2, method = 'bootstrap', boot.n = 2000)
  p <- -log(obj$p.value)
  return(c(auc05, p))
})

res
mainres <- t(res)
mainres <- as.data.frame(mainres)
colnames(mainres) <- c('x', 'y')
head(mainres)
ggplot(mainres, aes(x = x, y = y)) +
  geom_point()
write.table(mainres, file = 'prefig1D.txt')

# one more try for figure 1D
motifscores_mut <- mut %>%
  filter(cluster_or_motif == 'motif') %>% 
  group_by(cluster_id_or_motif_name, seq_name) %>% 
  summarise(score = max(cluster_or_motif_score))
motifscores_ref <- ref %>% 
  filter(cluster_or_motif == 'motif') %>% 
  group_by(cluster_id_or_motif_name, seq_name) %>% 
  summarise(score = max(cluster_or_motif_score))
motifscores_mut <- motifscores_mut %>% 
  mutate(present = score > 4)
motifscores_ref <- motifscores_ref %>% 
  mutate(present = score > 4)
mut_present <- motifscores_mut %>% group_by(cluster_id_or_motif_name) %>% summarise(pst = sum(present) > 50)
ref_present <- motifscores_ref %>% group_by(cluster_id_or_motif_name) %>% summarise(pst = sum(present) > 50)
motifs_to_take <- intersect(mut_present$cluster_id_or_motif_name[mut_present$pst], ref_present$cluster_id_or_motif_name[ref_present$pst])
motifscores_mut <- motifscores_mut %>% filter(cluster_id_or_motif_name %in% motifs_to_take)
motifscores_ref <- motifscores_ref %>% filter(cluster_id_or_motif_name %in% motifs_to_take)
new_access <- read.table(file = 'new_access.txt')
regions <- rownames(new_access)
motif_both_pred <- map(motifs_to_take, function(motif) {
  motif_ref <- motifscores_ref %>% filter(cluster_id_or_motif_name == motif)
  motif_mut <- motifscores_mut %>% filter(cluster_id_or_motif_name == motif)
  obs_ref <- map_int(regions, function(region) {
    if (region %in% motif_ref$seq_name) {
      if (motif_ref$present[motif_ref$seq_name == region]) {
        return(TRUE)
      } else {return(FALSE)}
    } else {return(FALSE)}
  })
  obs_mut <- map_int(regions, function(region) {
    if (region %in% motif_mut$seq_name) {
      if (motif_mut$present[motif_mut$seq_name == region]) {
        return(TRUE)
      } else {return(FALSE)}
    } else {return(FALSE)}
  })
  obs <- c(obs_ref, obs_mut)
  return(obs)
})
glm_pred <- c(access$ref, access$mut)
res <- map(motif_both_pred, function(pred) {
  random_pred <- sample.int(n = 2, size = 6406, replace = TRUE) - 1
  roc1 <- roc(response = glm_pred, predictor = pred)
  roc2 <- roc(response = glm_pred, predictor = random_pred)
  area_under_curve <- auc(roc1) - 0.5
  p_value <- roc.test(roc1 = roc1, roc2 = roc2, method = 'bootstrap', boot.n = 2000, parallel = TRUE)$p.value
  vec <- c(area_under_curve, p_value)
  names(vec) <- c('AUC', 'P-value')
  return(vec)
})
names(res) <- motifs_to_take
x <- sapply(res, function(x) return(x))
x <- t(x)
x <- as.data.frame(x)
ggplot(x, aes(x = AUC, y = -log(`P-value`))) +
  geom_point()
x
# new glm to filter out noise
library(edgeR)
calcNormFactors(raw_counts)
norm_counts <- sweep(x = raw_counts, MARGIN = 2, STATS = calcNormFactors(raw_counts), FUN = '/')
head(norm_counts)
norm_filtered_counts <- norm_counts[regID_norepeats, ][!c_filtered_2, ]
norm_filtered_counts <- as.data.frame(norm_filtered_counts)
norm_filtered_counts$id <- used_lines
head(norm_filtered_counts)

snpmat <- read.table(file = 'FINAL_FINAL_FINAL.bed', sep = ',', stringsAsFactors = FALSE, na.strings = 'None')
colnames(snpmat) <- c('chr', 'regstart', 'regend', 'regID', 'pos', '195',
                      '208', '235', '26', '358', '360', '362', '40', '427',
                      '437', '491', '509', '551', '57', '630', '639', '737',
                      '748', '820', '821', '897', '907', '913', 'ref', 'alt')
snpmat$'snpID' <- paste0(str_sub(snpmat$chr, start = 4), '_', snpmat$pos, '_SNP')
snps_to_filter_out <- apply(snpmat[6:28], 1, function(row) sum(is.na(row)) > 5)
sum(snps_to_filter_out)
snpmat <- snpmat[!snps_to_filter_out, ]
head(snpmat)
correspondence <- read.table(file = 'correspondence.txt', sep = '-', stringsAsFactors = FALSE)
colnames(correspondence) <- c('dgrp', 'line')
correspondence
dgrp_order <- as.integer(str_sub(string = colnames(norm_filtered_counts)[-32], start = 1, end = 5))
dgrp_order
line_order <- sapply(dgrp_order, function(x) correspondence[correspondence$dgrp == x, 2])
line_order
snpidlist <- seq_len(length.out = nrow(snpmat))
names(snpidlist) <- snpmat$snpID
rownames(norm_filtered_counts) <- paste0('id', norm_filtered_counts$id)
head(snpmat)
glms <- lapply(snpidlist, function(id) {
  lin <- snpmat[id, ]
  regID <- lin$regID
  lin <- lin[6:28]
  x <- unlist(lin[, as.character(line_order)])
  y <- unlist(norm_filtered_counts[regID, -32])
  glm(formula = y ~ x, family = 'gaussian')
})
p_values <- sapply(glms, function(model) anova(model, test = 'Chisq')$`Pr(>Chi)`[2])

sum(is.na(p_values))
p_values <- p_values[!is.na(p_values)]
fdr <- 0.05
adjusted_p_values <- p.adjust(p = p_values, method = 'BH')
sum(adjusted_p_values < fdr)
significant <- adjusted_p_values[adjusted_p_values < fdr]
significant_regions <- unique(snpmat$regID[snpmat$snpID %in% names(significant)])
length(significant_regions)

regions <- rownames(access)
regions
length(intersect(regions, significant_regions))
regions <- intersect(regions, significant_regions)
new_access <- access[regions, ]
write.table(x = new_access, file = 'new_access.txt')
# gaussian access
signs <- sapply(names(significant), function(x) glms[[x]]$coefficients[2])
signs
names(signs) <- str_replace(string = names(signs), pattern = '.x', replacement = '')
access <- sapply(significant_regions, function(name) {
  snpss <- snpmat$snpID[snpmat$regID == name]
  snpss <- snpss[snpss %in% names(signs)]
  change <- signs[snpss]
  access <- sum(change[change > 0])
  close <- abs(sum(change[change < 0]))
  if (access > close) {
    ans <- c(0, 1)
  } else {
    ans <- c(1, 0)
  }
  names(ans) <- c('ref', 'mut')
  return(ans)
})
access <- t(access)
write.table(x = access, file = 'gaussian_access.txt')

# gaussian preprocess
gaussianscores <- read.table(file = '~/BioData/DGRP/cbust/sig/cut_gaussian_output.bed', header = TRUE,
                             sep = '\t', stringsAsFactors = FALSE, comment.char = "$")
gmotifscores <- gaussianscores %>% 
  filter(cluster_or_motif == 'motif') %>% 
  group_by(cluster_id_or_motif_name, seq_name) %>% 
  summarise(score = max(cluster_or_motif_score))
gmotifscores <- gmotifscores %>%
  mutate(present = score > 4)
g_present <- gmotifscores %>% group_by(cluster_id_or_motif_name) %>% summarise(pst = sum(present) > 50)
motifs_to_take <- g_present$cluster_id_or_motif_name[g_present$pst]
gmotifscores <- gmotifscores %>% filter(cluster_id_or_motif_name %in% motifs_to_take)

# fig1D for only ref/gaussian sequences
gaussian_access <- read.table(file = 'gaussian_access.txt')
regions <- rownames(gaussian_access)
motif_ref_pred <- map(motifs_to_take, function(motif) {
  motif_ref <- motifscores_ref %>% filter(cluster_id_or_motif_name == motif)
  obs_ref <- map_int(regions, function(region) {
    if (region %in% motif_ref$seq_name) {
      if (motif_ref$present[motif_ref$seq_name == region]) {
        return(TRUE)
      } else {return(FALSE)}
    } else {return(FALSE)}
  })
  return(obs_ref)
})
motif_gauss_pred <- map(motifs_to_take, function(motif) {
  motif_ref <- gmotifscores %>% filter(cluster_id_or_motif_name == motif)
  obs_ref <- map_int(regions, function(region) {
    if (region %in% motif_ref$seq_name) {
      if (motif_ref$present[motif_ref$seq_name == region]) {
        return(TRUE)
      } else {return(FALSE)}
    } else {return(FALSE)}
  })
  return(obs_ref)
})
# glm_pred <- new_access$ref
# glm_pred
random_pred <- sample.int(n = 2, size = length(glm_pred), replace = TRUE) - 1
glm_pred <- gaussian_access$ref
res2 <- map(motif_gauss_pred, function(obs) {

  roc1 <- roc(response = glm_pred, predictor = obs)
  roc2 <- roc(response = glm_pred, predictor = random_pred)
  area_under_curve <- auc(roc1) - 0.5
  p_value <- roc.test(roc1 = roc1, roc2 = roc2, method = 'bootstrap', boot.n = 2000, parallel = TRUE)$p.value
  vec <- c(area_under_curve, p_value)
  names(vec) <- c('AUC', 'P-value')
  return(vec)
})
names(res2) <- motifs_to_take
y <- sapply(res2, function(x) return(x))
y <- t(y)
y <- as.data.frame(y)
y[y$AUC > 0, ]
write.table(x = y, file = 'aurocdata.txt')
ggplot(y, aes(x = AUC, y = -log(`P-value`))) +
  geom_point()

# new extraction
sig_regions <- rownames(gaussian_access)
regions_to_extract <- unique(snpmat[snpmat$regID %in% sig_regions, c(1, 2, 3, 4)])
write.table(x = regions_to_extract, file = 'gaussian_sequences_extraction.bed', sep = '\t', quote = FALSE,
            row.names = FALSE, col.names = FALSE)
# boxplot
allregionsgrh <- read.table(file = '~/BioData/DGRP/cut_allregionsgrhscoring.bed', header = TRUE, sep = '\t', stringsAsFactors = FALSE, comment.char = "$")
filteredregionsgrh <- allregionsgrh %>% 
  filter(cluster_or_motif == 'motif', cluster_or_motif_score > 7)
grh_regions <- unique(filteredregionsgrh$seq_name)
grh_regions
norm_counts <- as_tibble(norm_counts)
norm_counts$id <- paste0('id', seq.int(length.out = nrow(norm_counts)))
norm_counts <- norm_counts %>% 
  mutate(grh = id %in% grh_regions)
ggdf <- tibble(y = rowSums(norm_counts[1:31]) / 31, x = norm_counts$grh)
write.table(ggdf, 'ggdf.txt')
ggplot(data = ggdf, mapping = aes(x = x, y = y)) +
  geom_boxplot()
wilcox.test(x = ggdf$y[ggdf$x], y = ggdf$y[!ggdf$x], alternative = 'greater')
# motif searching
motif_bed <- read.table(file = 'BioData/DGRP/motif_search.bed', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                        comment.char = '')
head(motif_bed)
unique(motif_bed$`# chrom`)
x <- motif_bed %>%
  filter(cluster_or_motif == 'motif') %>% 
  filter(cluster_id_or_motif_name == 'taipale-NAAACCGGTTTN-GRHL1-full')
write.table(x, file = 'motif_genome.bed', sep = '\t', quote = FALSE,
            col.names = FALSE, row.names = FALSE)
x <- read.table(file = 'sorted_motif_genome.bed', sep = '\t', header = FALSE, stringsAsFactors = FALSE, check.names = FALSE,
                comment.char = '')
x <- arrange(x, desc(V5))
y <- x[1:16000, ]
head(y)
y <- y[!duplicated(y$V2), ]
y$V2 <- y$V2 - 300
y$V3 <- y$V3 + 300
z <- y[, 1:3]
write.table(z, file = 'windows.bed', sep = '\t', quote = FALSE,
            col.names = FALSE, row.names = FALSE)

# bound/unbound
unbound_scores <- read.table(file = 'BioData/DGRP/unbound_motifs.bed', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
           comment.char = '')
bound_scores <- read.table(file = 'BioData/DGRP/bound_motifs.bed', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                           comment.char = '')
head(unbound_scores)
head(bound_scores)
names(bound_scores)[1] <- 'name'
names(unbound_scores)[1] <- 'name'
library(dplyr)
bound_feature <- bound_scores %>% 
  filter(cluster_or_motif == 'motif') %>% 
  group_by(name, cluster_id_or_motif_name) %>% 
  summarise(max(cluster_or_motif_score))
unbound_feature <- unbound_scores %>% 
  filter(cluster_or_motif == 'motif') %>% 
  group_by(name, cluster_id_or_motif_name) %>% 
  summarise(max(cluster_or_motif_score))
write.table(x = bound_feature, file = 'bound_features.txt', row.names = FALSE, quote = FALSE)
write.table(x = unbound_feature, file = 'unbound_features.txt', row.names = FALSE, quote = FALSE)
