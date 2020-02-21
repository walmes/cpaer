#-----------------------------------------------------------------------
# Curso em Planejamento e Análise de Experimentos com R
#   · Análise de Covariância
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

library(tidyverse)
library(labestData)
library(emmeans)

# Análise de covariância.
BanzattoQd9.2.1
PimentelTb16.2.1
PimentelTb16.3.1
RamalhoEg13.2
RamalhoEx13.1
RamalhoEx13.2
RamalhoEx13.3
RamalhoTb13.6
ZimmermannTb14.3

#-----------------------------------------------------------------------
#

tb <- ZimmermannTb14.3
str(tb)

ggplot(data = tb,
       mapping = aes(x = reorder(cult, stand),
                     y = stand)) +
    geom_point()

ggplot(data = tb,
       mapping = aes(x = cult,
                     y = prod,
                     color = bloc,
                     size = stand)) +
    geom_point()

m0 <- lm(prod ~ bloc + stand + cult, data = tb)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)
summary(m0)

drop1(m0, test = "F")

mean(tb$stand)

tb_means <- emmeans(m0, specs = ~cult, at = list(stand = 100)) %>%
    multcomp::cld()

ggplot(data = tb_means,
       mapping = aes(x = reorder(cult, emmean), y = emmean)) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.0f", emmean)),
              hjust = 0,
              nudge_x = 0.05)

#-----------------------------------------------------------------------

url <- "http://leg.ufpr.br/~walmes/data/castracao.txt"
tb <- read_tsv(url, comment = "#")
str(tb)
