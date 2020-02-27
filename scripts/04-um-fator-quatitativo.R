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

BanzattoQd7.2.1  # Dose em DIC.
DemetrioTb7.1    # Dose em DBC.
DiasEg6.2        # Dose em DBC.
FariaQd14.3      # Dose em DBC.

#-----------------------------------------------------------------------
# Análise de experimento com fator quantitativo em DIC.

# Estrutura dos dados.
tb <- BanzattoQd7.2.1
str(tb)

# Número de pontos de suporte e repetições.
xtabs(~gesso, data = tb)

# Análise exploratória.
ggplot(data = tb,
       mapping = aes(x = gesso, y = peso)) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ poly(x, degree = 3)) +
    stat_summary(geom = "line", fun.y = "mean")

# Retorna o gráfico de vários ajustes de polinômio.
poly_fits <- map(1:6,
                 .f = function(d) {
                     ggplot(data = tb,
                            mapping = aes(x = gesso, y = peso)) +
                         facet_wrap(facets = as.character(d)) +
                         geom_point(pch = 1) +
                         geom_smooth(method = "lm",
                                     formula = y ~ poly(x, degree = d)) +
                         stat_summary(geom = "point", fun.y = "mean",
                                      pch = 3, col = "red")
                 })

# Exibe o resultado de vários ajustes polinomiais.
invoke(.f = gridExtra::grid.arrange, poly_fits)

# Ajuste o polinômio cúbico.
m0 <- lm(peso ~ gesso + I(gesso^2) + I(gesso^3),
         data = tb)
summary(m0)

# Ajuste com a `poly()` que usa polinômios ortogonais.
# m0 <- lm(peso ~ poly(gesso, degree = 3),
#          data = tb)
# summary(m0)

# Ajuste com a `poly()` que usa polinômios ordinários.
# m0 <- lm(peso ~ poly(gesso, degree = 3, raw = TRUE),
#          data = tb)
# summary(m0)

# Sequência para fazer a predição.
pred <- data.frame(gesso = seq(0, 300, by = 5))
aux <- predict(m0, newdata = pred, interval = "confidence")
head(aux)

# Junta tudo em uma mesma tabela.
pred <- cbind(pred, as.data.frame(aux))
head(pred)

coef(m0)

# COMMENT: a equação será passada manualmente mas é possível atumatizar
# esse processo.
round(coef(m0), digits = 7)

# Gráfico dos valores preditos sobre os observados.
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

# Função para minimizar e deteminar o ponto crítico numericamente.
obj_fun <- function(x) {
    -predict(m0, newdata = data.frame(gesso = x))
}

# Avalia a função em um ponto.
obj_fun(x = 100)

# Determina o ponto crítico.
opt <- optim(par = c(x = 100), fn = obj_fun)
opt$par    # Ponto crítico da dose de gesso.
-opt$value # Produtividade esperada no ponto crítico.

#-----------------------------------------------------------------------
# Análise em DBC.

# Tabela de dados.
tb <- FariaQd14.3
str(tb)

# Diagrama de dispersão.
ggplot(data = tb,
       mapping = aes(x = P, y = mspa, color = bloc)) +
    geom_point()

# Ajuste do modelo:
# y_{ij} = \mu + Bloc_i + \beta P_j + \beta_P_j^2 + e_{ij}.
m0 <- lm(mspa ~ bloc + P + I(P^2), data = tb)
coef(m0)

# Equação para o caso da restrição zerar o efeito do primeiro nível.
m0$assign
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m0)[m0$assign == 0] + sum(0.2 * coef(m0)[m0$assign == 1]),
        coef(m0)[m0$assign == 2],
        coef(m0)[m0$assign == 3])

# Muda a parametrização para obter a equação mais fácil.
m1 <- lm(mspa ~ bloc + P + I(P^2),
         data = tb,
         contrasts = list(bloc = contr.sum))
coef(m1)

# Equação para o caso da restrição soma de efeitos igual a zero.
m1$assign
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m1)[m1$assign == 0],
        coef(m1)[m1$assign == 2],
        coef(m1)[m1$assign == 3])

# NOTE: Isso ocorre pois a soma dos efeitos de bloco contém a restrição
# de somar zero.

# Inspeção dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de análise de variância.
anova(m0)

# Estimativas e testes de hipótese para cada parâmetro.
summary(m0)

library(emmeans)

# Equação média tem que acomodar de forma equilibrada o efeito de
# blocos. A `emmeans()` faz isso mas a `predict()` não.
pred <- emmeans(m0,
                specs = ~P,
                at = list(P = seq(0, 150, 5))) %>%
    as.data.frame()
head(pred)

# Gráfico com bandas de confiança.
ggplot(data = pred,
       mapping = aes(x = P, y = emmean)) +
    geom_ribbon(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                fill = "seagreen", alpha = 0.2) +
    geom_line(color = "orange") +
    geom_jitter(data = tb,
               mapping = aes(x = P, y = mspa),
               pch = 1, width = 2)

#-----------------------------------------------------------------------
# Mais um exemplo.

tb <- DiasEg6.2
str(tb)

ggplot(data = tb,
       mapping = aes(x = npk, y = prod, color = bloc)) +
    geom_point()

# Retorna o gráfico de vários ajustes de polinômio.
poly_fits <- map(1:4,
                 .f = function(d) {
                     ggplot(data = tb,
                            mapping = aes(x = npk, y = prod)) +
                         facet_wrap(facets = as.character(d)) +
                         geom_point(pch = 1) +
                         geom_smooth(mapping = aes(group = 1),
                                     method = "lm",
                                     formula = y ~ poly(x, degree = d)) +
                         stat_summary(geom = "point", fun.y = "mean",
                                      pch = 3, col = "red")
                 })

# Exibe o resultado de vários ajustes polinomiais.
invoke(.f = gridExtra::grid.arrange, poly_fits)

# ATTENTION: Esses ajustes são apenas exploratórios porque o efeito de
# bloco ainda precisa se acomodado.

# Ajuste do modelo com o grau 3.
m0 <- lm(prod ~ bloc + poly(npk, degree = 3, raw = TRUE), data = tb)
coef(m0)

# Note que os coeficientes do `poly()` tem o mesmo número agora.
m0$assign
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2 + (%0.7f) * x^3",
        coef(m0)[1],
        coef(m0)[5],
        coef(m0)[6],
        coef(m0)[7])

# Equação média tem que acomodar de forma equilibrada o efeito de
# blocos. A `emmeans()` faz isso mas a `predict()` não.
pred <- emmeans(m0,
                specs = ~npk,
                at = list(npk = seq(0, 100, by = 2))) %>%
    as.data.frame()
head(pred)

# Gráfico com bandas de confiança.
ggplot(data = pred,
       mapping = aes(x = npk, y = emmean)) +
    geom_ribbon(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                fill = "seagreen", alpha = 0.2) +
    geom_line(color = "orange") +
    geom_jitter(data = tb,
               mapping = aes(x = npk, y = prod),
               pch = 1, width = 2)

#-----------------------------------------------------------------------
# Mais um exemplo.

tb <- DemetrioTb7.1
str(tb)

ggplot(data = tb,
       mapping = aes(x = dose, y = producao, color = bloco)) +
    geom_point()

#-----------------------------------------------------------------------
