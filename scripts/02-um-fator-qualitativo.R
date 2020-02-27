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
# ls("package:labestData")

#-----------------------------------------------------------------------
# Controle de pulgões.

# Documentação.
help(BanzattoQd3.2.1, help_type = "html")

# Estrutura do objeto.
str(BanzattoQd3.2.1)

# Tabela de frequência para os tratamentos.
xtabs(~trat, data = BanzattoQd3.2.1) %>%
    cbind()

# Dados desemplilhados para uma visão retangular.
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
    # geom_boxplot() + # Usar boxplot com r >= 15.
    # geom_point() +
    geom_jitter(width = 0.1,
                height = 0,
                color = "#2369bd",
                pch = 19,
                size = 4)

# DANGER: boxplot apenas com número razoável de valores, sugestão >= 15.
ggplot(data = BanzattoQd3.2.1,
       mapping = aes(x = reorder(trat, pulgoes),
                     y = pulgoes)) +
    geom_boxplot()

ggplot(data = BanzattoQd3.2.1,
       mapping = aes(x = reorder(trat, pulgoes),
                     y = pulgoes)) +
    geom_boxplot(width = 0.4) +
    geom_point(position = position_nudge(x = 0.25)) +
    scale_y_sqrt()

#-----------------------------------------------------------------------
# Ajuste do modelo.

# Ajuste do modelo: y_{ij} = \mu + \tau_i + e_{ij}.
m0 <- lm(pulgoes ~ trat, data = BanzattoQd3.2.1)


# Inspeciona o objeto.
is.list(m0)           # É uma lista?
typeof(m0)            # Qual o tipo?
length(m0)            # Qual a dimensão?
names(m0)             # Nomes dos elementos.
class(m0)             # Classe do objeto.
methods(class = "lm") # Métodos associados à classe.
str(m0)               # Estrutura do objeto.

# Formas de extração.
m0$residuals  # Por endereçamento direto.
residuals(m0) # Por função extratora ou getter.

# Estimativas dos efeitos. Atenção para a restrição de zerar o efeito do
# primeiro nível.
cbind(coef(m0))

# Matriz de contrastes sob a retrição zerar o efeito do primeiro nível.
K <- cbind(1, contrasts(BanzattoQd3.2.1$trat))
unname(K)

# Médias por tratamento estimadas pelo modelo.
K %*% coef(m0)

# Médias amostrais.
BanzattoQd3.2.1 %>%
    group_by(trat) %>%
    summarise_at(.vars = "pulgoes", .funs = "mean")

# Trocando o tipo de contraste, ou seja, de restrição paramétrica.
# Agora o último nível tem efeito igual a zero.
m1 <- update(m0, contrasts = list(trat = "contr.SAS"))
coef(m1)

# Troca a ordem dos níveis do fator.
BanzattoQd3.2.1 <- BanzattoQd3.2.1 %>%
    mutate(trat = relevel(trat, ref = "Testemunha"))
levels(BanzattoQd3.2.1$trat)

# Ajusta novamente com nova disposição dos níveis. A testemunha é o
# nível de referência.
m0 <- update(m0,  data = BanzattoQd3.2.1)
coef(m0)

# Inspeção dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# COMMENT: violação da suposição de relação média-variância nula que
# endereça a suposição de homocedasticidade. Atenção, ainda pode haver
# violação da homocedasticidade mesmo com relação média-variância nula.

# Transformação Box-Cox.
MASS::boxcox(m0)
abline(v = c(0, 1/3), lty = 2, col = 2)

# COMMENT: o valor de lambda que maximiza a log-verossimilhança
# (leia-se, por enquanto, atendimento dos pressupostos) pode ainda não
# corrigir as violações.

# Atualiza o modelo anterior passando o log na resposta.
m0 <- update(m0, log(.) ~ .)

# Inspeção após a transformação da resposta.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de análise de variância.
anova(m0)

# Quadro de estimativas. Aqui estão os contrastes contra a testemunha.
summary(m0)

# Probabilidade de ao menos um erro de decisão ao fazer comparações
# multiplas entre k médias sob a hipótese nula.
k <- nlevels(BanzattoQd3.2.1$trat)
p <- 1 - (1 - 0.05)^choose(k, 2)
p

# COMMENT: Ou seja, a chance de declarar que ao menos um contraste seja
# diferente de zero sem de fato ser é de 40% quando são comparadas 5
# médias duas a duas (10 hipóteses).

# Médias ajustadas ou valores preditos.
pred <- data.frame(trat = levels(BanzattoQd3.2.1$trat))
pred <- cbind(pred,
              as.data.frame(predict(m0,
                                    newdata = pred,
                                    interval = "confidence")))
pred$trat <- reorder(pred$trat, pred$fit)
head(pred)

# Voltando para a escala natural.
pred <- pred %>%
    mutate_at(.vars = c("fit", "lwr", "upr"), .funs = "exp")
pred

# Gráfico de segmentos.
ggplot(data = pred,
       mapping = aes(x = trat, y = fit)) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0.05) +
    geom_text(mapping = aes(label = sprintf("%0.2f", fit)),
              hjust = 0,
              nudge_x = 0.05) +
    geom_point(data = BanzattoQd3.2.1,
               mapping = aes(x = trat, y = pulgoes),
               position = position_nudge(x = -0.1),
               pch = 1)

#-----------------------------------------------------------------------
# Testes de comparações de médias.

# Teste LSD (least significant difference).
tb_lsd <- agricolae::LSD.test(m0, trt = "trat", console = TRUE)
names(tb_lsd)
tb_lsd$groups

# Teste SNK (Student-Newman-Keuls).
tb_snk <- agricolae::SNK.test(m0, trt = "trat", console = TRUE)
names(tb_snk)
tb_snk$groups

# Teste HSD (honest significant difference).
tb_tukey <- agricolae::HSD.test(m0, trt = "trat", console = TRUE)
names(tb_tukey)
tb_tukey$groups

# Prepara para juntar com outra tabela.
tb_tukey <- tb_tukey$groups %>%
    rownames_to_column(var = "trat")
tb_tukey

# Junta resultados do Tukey com a tabela de IC.
tb_means <- inner_join(pred, tb_tukey, by = "trat")
tb_means

# Pontos com segmentos.
ggplot(data = tb_means,
       mapping = aes(x = reorder(trat, fit), y = fit)) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.1f %s", fit, groups)),
              angle = 90,
              hjust = 0.5,
              nudge_x = -0.1)

# Pontos com segmentos e eixos invertidos.
ggplot(data = tb_means,
       mapping = aes(x = reorder(trat, fit), y = fit)) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.1f %s", fit, groups)),
              hjust = 0.5, vjust = -0.5) +
    coord_flip()

# Barra com segmentos.
ggplot(data = tb_means,
       mapping = aes(x = reorder(trat, fit), y = fit)) +
    geom_col() +
    geom_errorbar(mapping = aes(ymin = lwr, ymax = upr),
                  width = 0.2) +
    geom_label(mapping = aes(label = sprintf("%0.0f %s", fit, groups)),
               hjust = 0,
               nudge_x = 0.05)

#-----------------------------------------------------------------------
# Fazer a casualização.

trat <- rep(x = c(levels(BanzattoQd3.2.1$trat), "Novo"),
            times = 8)
trat

# Opção 1: sorteie os níveis para as unidades experimentais ordenadas.
data.frame(trat = sample(trat), ue = 1:length(trat))

# Opção 2: sorteie as unidades experimentais ordenadas para os níveis.
data.frame(trat = trat, ue = sample(1:length(trat)))

#-----------------------------------------------------------------------
# Delineamento de Blocos Completos Casualizados.

# Usa um nome curto que é ágil.
tb <- BanzattoQd4.5.2
str(tb)

# Valores observados conectados conforme o bloco.
ggplot(data = tb,
       mapping = aes(x = reorder(promalin, peso),
                     y = peso,
                     color = bloco)) +
    geom_point() +
    geom_line(mapping = aes(group = bloco))

# Ajuste do modelo: y_{ij} = \mu + Bloco_i + Trat_j + \epsilon_{ij}
m0 <- lm(peso ~ bloco + promalin, data = tb)

# Análise dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de análise de variância.
anova(m0)

# emmeans: Estimated Marginal Means (aka LS means).
library(emmeans)
ls("package:emmeans")

# Médias marginais para efeito de promalin.
emm <- emmeans(m0, spec = ~promalin)
emm

# Classe e conteúdo.
typeof(emm)
class(emm)
slotNames(emm)

# A matriz de combinações lineares usadas para as médias.
emm@linfct          # Extração direta.
slot(emm, "linfct") # Extração pelo slot.
attr(emm, "linfct") # Extração pelo atributo.
coef(m0)

# Contrastes par a par.
# ATTENTION: já é feita correção dos p-valores para multiplicidade.
contrast(emm, method = "pairwise")

# Sem ajuste dos p-valores. Isso é o teste t sem correção para a
# multiplicidade, equivalente ao LSD.
contrast(emm, method = "pairwise", adjust = "none")

# Contraste com resumo compacto de letras.
tb_means <- multcomp::cld(emm) %>%
    as.data.frame() %>%
    mutate(cld = c("b", "b", "b", "ab", "a"))
tb_means

# Automatizando a conversão de números para letras.
# Função definida em um arquivo separado e carregada.
source("./funcoes.R")
body(num2let)

tb_means <- tb_means %>%
    mutate(cld = num2let(.group))

# Gráfico com segmentos de erro.
ggplot(data = tb_means,
       mapping = aes(y = reorder(promalin, emmean), x = emmean)) +
    geom_point() +
    geom_errorbarh(mapping = aes(xmin = lower.CL, xmax = upper.CL),
                   height = 0) +
    geom_text(mapping = aes(label = sprintf("%0.1f %s", emmean, cld)),
              vjust = 0, nudge_y = 0.05) +
    labs(x = "Peso", y = "Tratamento")

#-----------------------------------------------------------------------
# Delineamento Quadrado Latino.

# Nome curto para ficar mais manuzeável.
tb <- PimentelTb6.3.1
str(tb)

# Visualização dos dados.
ggplot(data = tb,
       mapping = aes(x = adub,
                     y = prod,
                     color = linhas,
                     shape = colunas)) +
    geom_point()

# Ajuste do modelo:
# y_{ijk} = \mu + Lin_i + Col_j + Adub_k + \epsilon_{ijk}
m0 <- lm(prod ~ linhas + colunas + adub, data = tb)

# Análise dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de análise de variância.
anova(m0)

# Médias ajustadas para nível de adubação.
emm <- emmeans(m0, spec = ~adub)
emm

# Erro tipo I para uma colação de 15 hipóteses.
# choose(6, 2): contrastes entre 6 médias).
1 - (1 - 0.05)^15

# Nível de significância para o teste individual como global a 5%.
1 - (1 - 0.05)^(1/15)

1 - (1 - 0.0034)^15  # Testando.
1 - (1 - 0.05/15)^15 # Com a aproximação feita bom Bonferroni.

# Sem correção para a multiplicidade.
tb_means <- multcomp::cld(emm, adjust = "none") %>%
    as.data.frame()
tb_means

# Com a correção de Bonferroni.
tb_means <- multcomp::cld(emm, adjust = "bonferroni") %>%
    as.data.frame()
tb_means

# Gráficos com segmentos.
ggplot(data = tb_means,
       mapping = aes(y = reorder(adub, emmean), x = emmean)) +
    geom_point() +
    geom_errorbarh(mapping = aes(xmin = lower.CL, xmax = upper.CL),
                   height = 0) +
    geom_text(mapping = aes(label = sprintf("%0.1f", emmean)),
              vjust = 0, nudge_y = 0.05) +
    labs(y = "Tipos de adubação", x = "Produção")

#-----------------------------------------------------------------------
# Outros datasets para praticar a análise.

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
BarbinPg156     # Fatorial com forma e dose de aplicação.
DemetrioTb7.1   # Dose em blocos.
DiasEg6.2       # Dose em bLocos.
FariaQd14.3     # Dose em blocos.
PimentelEg5.2   # DBC típico.
PimentelEx5.8.4 # DBC típico.
PimentelEx5.8.5 # DBC típico.

# DQL.
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
