#-----------------------------------------------------------------------
# Curso em Planejamento e Análise de Experimentos com R
#   · Análise de Experimentos Fatoriais
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2020-fev-18 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

library(emmeans)
library(nlme)
library(tidyverse)

#-----------------------------------------------------------------------
# Produção de soja em função de adubação com K e nível de água.

# Inspeciona os dados.
url <- "http://leg.ufpr.br/~walmes/data/soja.txt"
browseURL(url)

# Importa os dados pela URL.
tb <- read_tsv(file = url,
               comment = "#",
               locale = locale(decimal_mark = ","))
str(tb)

# Elimina o atributo para prints pais enxutos.
attr(tb, "spec") <- NULL

# Tabela de ocorrência dos pontos experimentais.
xtabs(~agua + potassio, data = tb)

# Diagrama de dispersão.
ggplot(data = tb,
       mapping = aes(x = potassio,
                     y = rengrao,
                     color = factor(agua))) +
    # geom_point() +
    geom_jitter(width = 5) +
    stat_summary(mapping = aes(group = agua),
                 geom = "line",
                 fun.y = "mean")

# Usando facetas para repartir.
ggplot(data = tb,
       mapping = aes(x = potassio,
                     y = rengrao)) +
    facet_wrap(facets = ~agua) +
    geom_point() +
    stat_summary(mapping = aes(group = bloco),
                 geom = "line",
                 fun.y = "mean")

#-----------------------------------------------------------------------

str(tb)

tb <- tb %>%
    mutate(A = factor(agua),
           K = factor(potassio),
           bloco = factor(bloco))
str(tb)

tb[74, ]

m0 <- lm(rengrao ~ bloco + A * K, data = tb[-74, ])
# m0 <- lm(rengrao ~ bloco + A * K, data = tb)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)

m1 <- aov(rengrao ~ bloco + A/K, data = tb[-74, ])
coef(m1)
anova(m1)

summary(m1,
        split = list("A:K" = list(A37.5 = c(1, 4, 7, 10),
                                  A50.0 = c(2, 5, 8, 11),
                                  A62.5 = c(3, 6, 8, 12))))

m2 <- aov(rengrao ~ bloco + K/A, data = tb[-74, ])
coef(m2)
anova(m2)

summary(m2,
        split = list("K:A" = list(K0 = c(1, 6),
                                  K30 = c(2, 7),
                                  K60 = c(3, 8),
                                  K120 = c(4, 9),
                                  K180 = c(5, 10))))

#-----------------------------------------------------------------------

# emmeans(m0, specs = ~A + K)
emm <- emmeans(m0, specs = ~K | A)
emm

# emm <- emmeans(m0, specs = ~A | K,
#                at = list(K = c("60", "120", "180")))

# contrast(emm, method = "pairwise")

tb_means <- multcomp::cld(emm) %>%
    as.data.frame()

ggplot(data = tb_means,
       mapping = aes(x = K, y = emmean)) +
    facet_wrap(facets = ~A) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lower.CL,
                                ymax = upper.CL),
                  width = 0)

#-----------------------------------------------------------------------

m0 <- lm(rengrao ~ bloco + A * potassio,
         data = tb[-74, ])

par(mfrow = c(2, 2))
plot(m0)
layout(1)

m0 <- lm(rengrao ~ bloco + A * (potassio + I(potassio^2)),
         data = tb[-74, ])

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)

m1 <- lm(rengrao ~ bloco + A * potassio + I(potassio^2),
         data = tb[-74, ])

anova(m1, m0)

summary(m1)

emm <- emmeans(m1, specs = ~A + potassio,
               at = list(potassio = seq(0, 180, by = 3))) %>%
    as.data.frame()
head(emm)

ggplot(data = emm,
       mapping = aes(x = potassio, y = emmean)) +
    facet_wrap(facets = ~A) +
    geom_ribbon(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                fill = "tomato") +
    geom_line() +
    geom_point(data = tb[-74, ],
               mapping = aes(x = potassio, y = rengrao))

tb_used <- m1$model
tb_used$fitted <- fitted(m1)

tb_used %>%
    group_by(A) %>%
    summarise(R2 = cor(rengrao, fitted)^2)

b <- coef(m1)
cbind(b0 = b[1] + sum(0.2 * b[2:5]),
      b1 = b[8],
      b2 = b[9])
cbind(b0 = b[1] + sum(0.2 * b[2:5]) + b[6],
      b1 = b[8] + b[10],
      b2 = b[9])
cbind(b0 = b[1] + sum(0.2 * b[2:5]) + b[7],
      b1 = b[8] + b[11],
      b2 = b[9])

#-----------------------------------------------------------------------

ggplot(data = tb,
       mapping = aes(x = potassio,
                     y = rengrao)) +
    facet_wrap(facets = ~agua) +
    geom_point()

n_375 <- nls(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             data = filter(tb, A == "37.5"),
             start = list(b0 = 12, b1 = 15/50, bb = 50))
coef(n_375)

n_500 <- nls(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             data = filter(tb, A == "50"),
             start = list(b0 = 15, b1 = 15/50, bb = 60))
coef(n_500)

n_625 <- nls(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             data = filter(tb[-74, ], A == "62.5"),
             start = list(b0 = 15, b1 = 15/50, bb = 90))
coef(n_625)

nfit <- nlsList(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                    b1 * bb * (potassio > bb) | A,
                data = tb[-74, ],
                start = c(b0 = 15, b1 = 15/50, bb = 50))
# summary(nfit)
coef(nfit)

tb_grouped <- groupedData(rengrao ~ potassio | bloco, tb[-74, ])

fit0 <- nlme(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             fixed = b0 + b1 + bb ~ A,
             random = b0 ~ 1,
             start = c(13.52, 15.71 - 13.52, 15.43 - 13.52,
                       0.22, 0, 0,
                       49, 58 - 49, 93 - 49),
             data = tb_grouped,
             verbose = TRUE)
summary(fit0)

fit1 <- update(fit0,
               fixed = list(b0 ~ 1, b1 ~ 1, bb ~ A),
               start = c(13.52,
                         0.22,
                         49, 58 - 49, 93 - 49))

anova(fit0, fit1)

pred <- expand.grid(A = levels(tb$A),
                    potassio = seq(0, 180, by = 3))
pred$y <- predict(fit1, newdata = pred, level = 0)

ggplot(data = tb,
       mapping = aes(x = potassio,
                     y = rengrao)) +
    facet_wrap(facets = ~A) +
    geom_point() +
    geom_line(data = pred,
              mapping = aes(x = potassio, y = y))

#-----------------------------------------------------------------------
# Modelo de regressão múltipla.

url <- "http://leg.ufpr.br/~walmes/data/chimarrita.txt"
tb <- read_tsv(file = url, comment = "#")
attr(tb, "spec") <- NULL
str(tb)

tb <- filter(tb, fer == "sim")

lattice::splom(select_if(tb, is.numeric))

#-----------------------------------------------------------------------
# Desfolha do algodão.

# Inspeciona os dados.
url <- "http://leg.ufpr.br/~walmes/data/desfolha_algodao.txt"
browseURL(url)

# Importa os dados pela URL.
tb <- read_tsv(file = url, comment = "#")
str(tb)

# Elimina o atributo para prints pais enxutos.
attr(tb, "spec") <- NULL

# Tabela de ocorrência dos pontos experimentais.
ftable(xtabs(~bloco + fase + desf, data = tb))

tb %>%
    arrange(bloco, fase, desf, planta)

# Média por vaso (duas plantas por vaso).
tb2 <- tb %>%
    group_by(bloco, fase, desf) %>%
    summarise_at(.vars = vars(nestrrep:pesocap),
                 .funs = "mean",
                 na.rm = TRUE) %>%
    ungroup()
str(tb2)

# dput(unique(tb2$fase)[c(5, 1, 3, 4, 2)])
l <- c("veget", "botflor", "floresc", "maça", "capulho")
tb2 <- tb2 %>%
    mutate(fase = factor(fase, levels = l))

levels(tb2$fase)

# Usando facetas para repartir.
ggplot(data = tb2,
       mapping = aes(x = desf,
                     y = pesocap)) +
    facet_wrap(facets = ~fase) +
    geom_point() +
    stat_summary(geom = "line",
                 fun.y = "mean")
