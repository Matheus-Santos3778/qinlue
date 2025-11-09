library(ggplot2)
library(patchwork)
library(GGally)
library(tidyr)
library(car)
require(glmnet)
require(ggResidpanel)
require(hnp)
library(DHARMa)
require(gamlss)

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
raw_data$pib_percap <- log1p(raw_data$pib_percap)
raw_data$taxa_nascimentos <- log1p(raw_data$taxa_nascimentos)
raw_data$taxa_urb <- log(raw_data$taxa_urb)
raw_data$cobertura_bcg <- log(raw_data$cobertura_bcg)
#raw_data$acidentes <- log1p(raw_data$acidentes)

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

summary(ajuste1)

summary(ajuste2)
vif(ajuste2)

anova(ajuste1, ajuste2)

#Análise do ajuste com Poisson e log
par(mfrow = c(2,2))
plot(ajuste2, which = 1:4)

resid_panel(ajuste2, plots = c("resid", "qq", "ls", "cookd"), qqbands = TRUE, nrow = 2)

par(mfrow = c(1,1))
hnp(ajuste2, pch = 20, cex = 1.2)

#Transformando as variáveis
raw_data$populacao <- log(raw_data$populacao)
raw_data$pib_percap <- log1p(raw_data$pib_percap)
raw_data$taxa_nascimentos <- log1p(raw_data$taxa_nascimentos)
raw_data$taxa_urb <- log(raw_data$taxa_urb)
raw_data$cobertura_bcg <- log(raw_data$cobertura_bcg)

ajuste_transf <- glm(acidentes ~ . - cod_mun - nome_mun, 
               family = poisson(link = 'log'), 
               data = raw_data)

summary(ajuste2)
summary(ajuste_transf)
vif(ajuste_transf)

anova(ajuste2, ajuste_transf)

#Análise do ajuste com Poisson e log
par(mfrow = c(2,2))
plot(ajuste2, which = 1:4)
plot(ajuste_transf, which = 1:4)

resid_panel(ajuste_transf, plots = c("resid", "qq", "ls", "cookd"), qqbands = TRUE, nrow = 2)

par(mfrow = c(1,2))
hnp(ajuste2, pch = 20, cex = 1.2)
hnp(ajuste_transf, pch = 20, cex = 1.2)

#Parece ter indícios claros de superdispersão
#Tivemos um alternation limit reached, então aumentos o maxit para ter mais iterações
ajustenb <- glm.nb(acidentes ~ . - cod_mun - nome_mun, data=raw_data, maxit=100)

par(mfrow = c(2,2))
plot(ajustenb, which = 1:4)

#resid_panel(ajustenb, plots = c("resid", "qq", "ls", "cookd"), qqbands = TRUE, nrow = 2)

par(mfrow = c(1,1))
hnp(ajustenb, pch = 20, cex = 1.2)

#Testes da NB com link Log
sim_nb <- simulateResiduals(ajustenb)
#Gráficos com Resíduos simulados
plot(sim_nb)
#Testes de inflação de zeros e de dispersão (sub ou sob)
testDispersion(sim_nb)
testZeroInflation(sim_nb)

#ZIP
ajusteZIP <- gamlss(acidentes ~ . - cod_mun - nome_mun,
                     sigma.formula =~ . - cod_mun - nome_mun,
                     family = ZIP, data = raw_data)
plot(ajusteZIP)

ajusteZAP <- gamlss(acidentes ~ . - cod_mun - nome_mun,
                    sigma.formula =~ . - cod_mun - nome_mun,
                    family = ZAP, data = raw_data)
plot(ajusteZAP)

ajusteZINBI <- gamlss(acidentes ~ . - cod_mun - nome_mun,
                    sigma.formula =~ . - cod_mun - nome_mun,
                    family = ZINBI, data = raw_data)
summary(ajusteZINBI)
plot(ajusteZINBI)

ajusteZANBI <- gamlss(acidentes ~ . - cod_mun - nome_mun,
                      sigma.formula =~ . - cod_mun - nome_mun,
                      family = ZANBI, data = raw_data, method = RS(2000))
summary(ajusteZANBI)
plot(ajusteZANBI)

res <- qresid(ajusteZANBI)
summary(res)
sum(is.na(res))

AIC(ajuste2, ajuste_transf, ajustenb, ajusteZIP, ajusteZAP, ajusteZINBI, ajusteZANBI)
BIC(ajuste2, ajuste_transf, ajustenb, ajusteZIP, ajusteZAP, ajusteZINBI, ajusteZANBI)
