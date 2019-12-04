# 1
dataset <- read.table(file = '1.txt', header = TRUE)
dataset
glmFit <- glm(res ~ sex * wep * alh, dataset, family = 'poisson')
glmFit
coef(glmFit)
summary(glmFit)
anovaobj <- anova(glmFit, test = 'Chisq')
anovaobj$`Resid. Dev`
# значимо влияют пол, тип оружия, кол-во алкоголя, взаимодействия пола и типа оружия
# и типа оружия и кол-ва алкоголя
predobs <- data.frame(obs = dataset$res, pred = predict(glmFit, type = 'response'))
ggplot(predobs, aes(x = obs, y = pred)) +
  geom_point() + geom_smooth(method = 'glm')
# 2
pred2 <- sapply(1:nrow(dataset), function(i) {
  glmoptFit <- glm(res ~ sex + wep + alh + sex:wep + wep:alh, dataset[-i, ], family = 'poisson')
  pred <- predict(glmoptFit, newdata = dataset[i, ], type = 'response')
  return(pred)
})
predobs2 <- data.frame(obs = dataset$res, pred = pred2)
predobs2
cor(predobs2$obs, predobs2$pred)
ggplot(predobs2, aes(x = obs, y = pred)) +
  geom_point() + geom_smooth(method = 'glm')
