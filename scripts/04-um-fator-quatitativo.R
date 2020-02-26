#-----------------------------------------------------------------------
# Curso em Planejamento e Análise de Experimentos com R
#   · Análise de Experimentos com um Fator Quantitativo
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2020-fev-20 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

library(tidyverse)
library(labestData)
ls("package:labestData")

#-----------------------------------------------------------------------
# Datasets.

BanzattoQd7.2.1  # Dose.
DemetrioTb7.1    # Dose em blocos.
DiasEg6.2        # Dose em bLocos.
FariaQd14.3      # Dose em blocos.

#-----------------------------------------------------------------------

tb <- BanzattoQd7.2.1
str(tb)

ggplot(data = tb,
       mapping = aes(x = gesso, y = peso)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ poly(x, degree = 3)) +
    stat_summary(geom = "line", fun.y = "mean")

m0 <- lm(peso ~ gesso + I(gesso^2) + I(gesso^3),
         data = tb)
summary(m0)

# m0 <- lm(peso ~ poly(gesso, degree = 3),
#          data = tb)
# summary(m0)
#
# m0 <- lm(peso ~ poly(gesso, degree = 3, raw = TRUE),
#          data = tb)
# summary(m0)

pred <- data.frame(gesso = seq(0, 300, by = 5))

aux <- predict(m0, newdata = pred, interval = "confidence")
head(aux)

pred <- cbind(pred, as.data.frame(aux))
head(pred)

ggplot(data = pred,
       mapping = aes(x = gesso, y = fit)) +
    geom_ribbon(mapping = aes(ymin = lwr, ymax = upr),
                fill = "seagreen", alpha = 0.2) +
    geom_line(color = "orange") +
    annotate(geom = "text",
             x = 300,
             y = 140,
             label = "y == 138 + 0.44 * x -0.0023 * x^2 + 0.0000034 * x^3",
             parse = TRUE,
             hjust = 1,
             size = 3.5) +
    geom_vline(xintercept = 137.97)

round(coef(m0), digits = 7)

obj_fun <- function(x) {
    -predict(m0, newdata = data.frame(gesso = x))
}
obj_fun(x = 100)

opt <- optim(par = c(x = 100), fn = obj_fun)
opt

#-----------------------------------------------------------------------

tb <- FariaQd14.3
str(tb)

ggplot(data = tb,
       mapping = aes(x = P, y = mspa, color = bloc)) +
    geom_point()

m0 <- lm(mspa ~ bloc + P + I(P^2), data = tb)
m0 <- lm(mspa ~ bloc + P + I(P^2),
         data = tb,
         contrasts = list(bloc = contr.sum))
anova(m0)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

summary(m0)
round(coef(m0), 4)

library(emmeans)

pred <- emmeans(m0,
                specs = ~P,
                at = list(P = seq(0, 150, 5))) %>%
    as.data.frame()
head(pred)

ggplot(data = pred,
       mapping = aes(x = P, y = emmean)) +
    geom_ribbon(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                fill = "seagreen", alpha = 0.2) +
    geom_line(color = "orange")

#-----------------------------------------------------------------------

tb <- DemetrioTb7.1
str(tb)

ggplot(data = tb,
       mapping = aes(x = dose, y = producao, color = bloco)) +
    geom_point()

#-----------------------------------------------------------------------

tb <- DiasEg6.2
str(tb)

ggplot(data = tb,
       mapping = aes(x = npk, y = prod, color = bloc)) +
    geom_point()

#-----------------------------------------------------------------------
