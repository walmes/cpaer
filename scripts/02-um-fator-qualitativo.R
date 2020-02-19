#-----------------------------------------------------------------------
# Curso em Planejamento e Análise de Experimentos com R
#   · Análise de Experimentos com um Fator Qualitativo
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
ls("package:labestData")

#-----------------------------------------------------------------------
# Controle de pulgões.

# Documentação.
help(BanzattoQd3.2.1, help_type = "html")

# Estrutura do objeto.
str(BanzattoQd3.2.1)

# Tabela de frequência para os tratamentos.
xtabs(~trat, data = BanzattoQd3.2.1)

# Dados desemplilhados.
BanzattoQd3.2.1 %>%
    spread(key = "rept", value = "pulgoes")

# Média e desvio-padrão das observações em cada nível.
BanzattoQd3.2.1 %>%
    group_by(trat) %>%
    summarise_at(.vars = "pulgoes", .funs = c("mean", "sd")) %>%
    arrange(mean)

# Diagrama de dispersão.
ggplot(data = BanzattoQd3.2.1,
       mapping = aes(x = reorder(trat, pulgoes),
                     y = pulgoes)) +
    scale_y_log10() +
    # geom_point()
    geom_boxplot() +
    geom_jitter(width = 0.2,
                height = 0,
                color = "#2369bd",
                pch = 19,
                size = 4)

# DANGER: boxplot apenas com número razoável de valores, sugestão >= 15.
# ggplot(data = BanzattoQd3.2.1,
#        mapping = aes(x = reorder(trat, pulgoes),
#                      y = pulgoes)) +
#     geom_boxplot()

#-----------------------------------------------------------------------
# Ajuste do modelo.

# Ajuste do modelo.
m0 <- lm(pulgoes ~ trat, data = BanzattoQd3.2.1)

is.list(m0)
names(m0)
str(m0)
class(m0)
methods(class = "lm")

m0$residuals
residuals(m0)

# Estimativas dos efeitos. Restrição de zerar primeiro nível.
cbind(coef(m0))

# Matriz de contrastes sob a retrição zerar primeiro nível.
K <- cbind(1, contrasts(BanzattoQd3.2.1$trat))
K

# Médias estimadas pelo modelo.
K %*% coef(m0)

# Médias amostrais.
BanzattoQd3.2.1 %>%
    group_by(trat) %>%
    summarise_at(.vars = "pulgoes", .funs = "mean")

m1 <- update(m0, contrasts = list(trat = "contr.SAS"))
coef(m1)

BanzattoQd3.2.1 <- BanzattoQd3.2.1 %>%
    mutate(trat = relevel(trat, ref = "Testemunha"))

levels(BanzattoQd3.2.1$trat)

m0 <- update(m0,  data = BanzattoQd3.2.1)
coef(m0)

# Inspeção dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Transformação boxcox.
MASS::boxcox(m0)
abline(v = c(0, 1/3), lty = 2, col = 2)

# Atualiza o modelo anterior passando o log na resposta.
m0 <- update(m0, log(.) ~ .)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

anova(m0)
summary(m0)

k <- nlevels(BanzattoQd3.2.1$trat) - 1
p <- 1 - (1 - 0.05)^k
p

pred <- data.frame(trat = levels(BanzattoQd3.2.1$trat))
pred <- cbind(pred,
              as.data.frame(predict(m0,
                                    newdata = pred,
                                    interval = "confidence")))
pred$trat <- reorder(pred$trat, pred$fit)
head(pred)

pred <- pred %>%
    mutate_at(.vars = c("fit", "lwr", "upr"), .funs = "exp")
pred

ggplot(data = pred,
       mapping = aes(x = trat, y = fit)) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.2f", fit)),
              hjust = 0,
              nudge_x = 0.05)

tb_tukey <- agricolae::HSD.test(m0, trt = "trat", console = TRUE)
str(tb_tukey)

tb_tukey <- tb_tukey$groups %>%
    rownames_to_column(var = "trat")
tb_tukey
pred

tb_means <- inner_join(pred, tb_tukey, by = "trat")

ggplot(data = tb_means,
       mapping = aes(x = reorder(trat, fit), y = fit)) +
    # geom_point() +
    geom_col() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.2f %s", fit, groups)),
              hjust = 0,
              nudge_x = 0.05)

ggplot(data = tb_means,
       mapping = aes(x = reorder(trat, fit), y = fit)) +
    # geom_point() +
    geom_col() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.2f %s", fit, groups)),
              hjust = 0,
              nudge_x = 0.05) +
    coord_flip()

#-----------------------------------------------------------------------
# Fazer a casualização.

trat <- rep(x = c(levels(BanzattoQd3.2.1$trat), "Novo"),
            times = 8)

# Opção 1: sorteie os níveis para as unidades experimentais ordenadas.
data.frame(trat = sample(trat), ue = 1:length(trat))

# Opção 2: sorteie as unidades experimentais ordenadas para os níveis.
data.frame(trat = trat, ue = sample(1:length(trat)))

#-----------------------------------------------------------------------

# Análise de experimento em blocos.
tb <- BanzattoQd4.5.2
str(tb)

ggplot(data = tb,
       mapping = aes(x = promalin, y = peso, color = bloco)) +
    geom_point() +
    geom_line(mapping = aes(group = bloco))

m0 <- lm(peso ~ bloco + promalin, data = tb)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

MASS::boxcox(m0)

anova(m0)

# Estimated Marginal Means (LS means).
library(emmeans)

emm <- emmeans(m0, spec = ~promalin)
# class(emm)
# str(emm)
# slots(emm)
attr(emm, "linfct") %*% coef(m0)
coef(m0)

contrast(emm, method = "pairwise")
tb_means <- multcomp::cld(emm) %>%
    as.data.frame() %>%
    mutate(cld = c("b", "b", "b", "ab", "a"))

ggplot(data = tb_means,
       mapping = aes(y = reorder(promalin, emmean), x = emmean)) +
    geom_point() +
    geom_errorbarh(mapping = aes(xmin = lower.CL, xmax = upper.CL),
                   height = 0) +
    geom_text(mapping = aes(label = sprintf("%0.1f %s", emmean, cld)),
              vjust = 0, nudge_y = 0.05)



#-----------------------------------------------------------------------
# Datasets.

# DIC.
BanzattoQd3.4.1  # Básico.
BanzattoQd3.2.1  # Contagem.
BanzattoQd3.7.1  # Contrastes.
BanzattoQd7.2.1  # Dose.
CostaEx5.7.3     # Dose.
DiasEg10.1       # Fatorial.
DiasEg10.2       # Fatorial.
DiasEg5.1        # Contagem.
ZimmermannTb16.5 # DBC Sobrevivência.

# DBC.
BanzattoQd4.5.2 # Contrastes.
BanzattoQd5.3.1 # Típico.
BanzattoQd6.2.5 # Fatorial fonte dose.
BanzattoQd6.4.2 # Fatorial época adubo.
BanzattoQd7.3.3 # Fatorial variedade adubação.
BarbinEx14      # NPK em 2^3 em 3 blocos.
BarbinPg125     # NPK base 2.
BarbinPg137     # NPK base 3.
BarbinPg156     # forma e dose de aplicação.
DemetrioTb7.1   # Dose em blocos.
DiasEg6.2       # Dose em bLocos.
FariaQd14.3     # Dose em blocos.
PimentelEg5.2   # DBC típico.
PimentelEx5.8.4 # DBC típico.
PimentelEx5.8.5 # DBC típico.

# DQL
BarbinPg104       # Típico.
PimentelEg6.2     # Típico.
PimentelEx6.6.3   # Contrastes.
PimentelTb6.3.1   # Fatorial triplo.
PimentelTb9.3.1   # Fatorial duplo e parcela subdividida.
ZimmermannTb14.9  # Valor ausente.
ZimmermannTb16.10 # Contagem.
ZimmermannTb5.11  # Proporção amostral.
ZimmermannTb5.15  # Contagem.
ZimmermannTb5.2   # Típico.

#-----------------------------------------------------------------------
