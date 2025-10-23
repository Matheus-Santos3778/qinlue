library(ggplot2)
library(patchwork)
library(GGally)
library(tidyr)
library(car)
require(glmnet)

raw_data <- read.csv2('data/dados.csv')

summary(raw_data)

#Univariate analysis
hist(log(raw_data$populacao))
hist(raw_data$taxa_mort_agressao)
hist(raw_data$taxa_mortalidade)
hist(raw_data$taxa_nascimentos)
hist(raw_data$porc_agropecuaria)
hist(raw_data$porc_industria)

colnames(raw_data)
summary(raw_data)

vars <- c('populacao', 'pib_percap', 'taxa_nascimentos', 'taxa_urb', 'ideb', 'cobertura_bcg', 'taxa_jovens')

plots <- lapply(vars, function(v) {
  ggplot(raw_data, aes_string(x = v, y = 'acidentes')) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_bw() +
    labs(title = v)
})

wrap_plots(plots, ncol = 3)

raw_data$populacao <- log(raw_data$populacao)
raw_data$pib_percap <- log(raw_data$pib_percap + 1)
raw_data$taxa_nascimentos <- log(raw_data$taxa_nascimentos + 1)
raw_data$taxa_urb <- log(raw_data$taxa_urb)
raw_data$cobertura_bcg <- log(raw_data$cobertura_bcg)

plots <- lapply(vars, function(v) {
  ggplot(raw_data, aes_string(x = v, y = 'acidentes')) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_bw() +
    labs(title = v)
})

wrap_plots(plots, ncol = 3)

ggpairs(raw_data[, c(vars, 'acidentes')])

ajuste1 <- glm(acidentes ~ populacao, 
                     family = poisson(link = 'log'), 
                     data = raw_data)

ajuste2 <- glm(acidentes ~ . - cod_mun - nome_mun, 
               family = poisson(link = 'log'), 
               data = raw_data)

ajuste3 <- glm(acidentes ~ . - cod_mun - nome_mun - pib_percap, 
               family = poisson(link = 'log'), 
               data = raw_data)

summary(ajuste2)
vif(ajuste2)

summary(ajuste1)
vif(ajuste1)

ajuste_step <- step(ajuste2, direction = "both")

y <- raw_data$acidentes
x <- as.matrix(raw_data[vars])

ajuste_lasso <- glmnet(x, y, family = 'poisson', alpha = 1)
summary(ajuste_lasso)

par(cex = 1.2, las = 1)


ajuste_lin <- lm(acidentes ~ populacao, data = raw_data)
summary(ajuste_lin)

anova(ajuste1, ajuste2, test = "Chisq")
residuals.glm(ajuste)

AIC(ajuste1, ajuste2)
BIC(ajuste1, ajuste2)
