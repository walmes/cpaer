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

library(emmeans) # Médias, contrastes e comparações múltiplas.
library(nlme)    # Modelos de efeitos mistos.
library(tidyverse)

#-----------------------------------------------------------------------
# Produção de soja em função de adubação com K e nível de água.

# Serão feitas 3 análises para este experimento:
#
# 1. Considerando que ambos os fatores são qualitativos com estudo da
# interação por meio de testes de comparação múltipla.
#
# 2. Com o fator de potássio quantitativo fazendo o estudo da interação
# por meio do ajuste de curvas polinômiais.
#
# 3. Com o fator de potássio quantitativo fazendo o estudo por meio de
# modelos não lineares, no caso, o resposta-platô.

#-----------------------------------------------------------------------
# Importação e análise exploratória.

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
# Fatorial com fatores qualitativos.

# Cria versões com fatores qualitativos.
tb <- tb %>%
    mutate(A = factor(agua),
           K = factor(potassio),
           bloco = factor(bloco))
str(tb)

# Observação comprometida que será removida da análise.
tb[74, ]

# m0 <- lm(rengrao ~ bloco + A * K, data = tb)
m0 <- lm(rengrao ~ bloco + A * K,
         data = tb[-74, ])

# Inspeção dos pressupostos.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Quadro de análise de variância.
anova(m0)

# Modelo com efeito de K dentro de A.
m1 <- aov(rengrao ~ bloco + A/K, data = tb[-74, ])

coef(m1)  # Efeitos estimados.
anova(m1) # Partição das somas de quadrados.

# Decomposição das SQ em efeito de K dentro de cada nível de A.
summary(m1,
        split = list("A:K" = list(A37.5 = c(1, 4, 7, 10),
                                  A50.0 = c(2, 5, 8, 11),
                                  A62.5 = c(3, 6, 8, 12))))

# Outra forma de testar.
# dput(names(coef(m1)))
car::linearHypothesis(m1,
                      c("A37.5:K30 = 0",
                        "A37.5:K60 = 0",
                        "A37.5:K120 = 0",
                        "A37.5:K180 = 0"))
car::linearHypothesis(m1,
                      c("A50:K30 = 0",
                        "A50:K60 = 0",
                        "A50:K120 = 0",
                        "A50:K180 = 0"))
car::linearHypothesis(m1,
                      c("A62.5:K30 = 0",
                        "A62.5:K60 = 0",
                        "A62.5:K120 = 0",
                        "A62.5:K180 = 0"))

# É possível passar uma matriz, que confere mais flexibilidade, mas dá
# mais trabalho. O segredo é colocar os 1 no lugar certo.
names(coef(m1))
H <- matrix(0, ncol = length(coef(m1)), nrow = 4)
H[, grep(pattern = "A37.5", x = names(coef(m1)))] <- diag(4)
H

# Usando a matriz que especifica hipótese linear.
car::linearHypothesis(m1, H)

# Modelo com efeito de A dentro de níveis de K.
m2 <- aov(rengrao ~ bloco + K/A, data = tb[-74, ])
coef(m2)
anova(m2)

# Decomposição das SQ em efeito de A dentro de cada nível de K.
summary(m2,
        split = list("K:A" = list(K0 = c(1, 6),
                                  K30 = c(2, 7),
                                  K60 = c(3, 8),
                                  K120 = c(4, 9),
                                  K180 = c(5, 10))))

#-----------------------------------------------------------------------
# Desdobramento da interação com testes de médias.

# Médias para cada combinação A:K.
emmeans(m0, specs = ~A + K)

# Restrigindo para um nível de A.
emmeans(m0, specs = ~A + K, at = list(A = "37.5"))

# As mesmas médias mas com a divisão.
emm <- emmeans(m0, specs = ~K | A)
emm

# Constrastes par a par dentro de cada divisão.
contrast(emm, method = "pairwise")

# NOTE: Contrastes não tem o mesmo erro-padrão por causa do
# desbalanceamento. Existem dois erros-padrões: aqueles entre celas com
# 5 repetições e aqueles entre celas de 5 contra de 4.

# Contrastes par a par seguido das identificação.
tb_means <- multcomp::cld(emm) %>%
    as.data.frame()
tb_means

# Carrega objetos definidos em arquivo separado.
source("./funcoes.R")
body(num2let)

tb_means <- tb_means %>%
    group_by(A) %>%
    mutate(cld = num2let(.group)) %>%
    ungroup()

ggplot(data = tb_means,
       mapping = aes(x = K, y = emmean)) +
    facet_wrap(facets = ~A) +
    geom_point() +
    geom_errorbar(mapping = aes(ymin = lower.CL,
                                ymax = upper.CL),
                  width = 0) +
    geom_text(mapping = aes(label = sprintf("%0.1f %s", emmean, cld)),
              angle = 90,
              vjust = -1/2)

#-----------------------------------------------------------------------
# Análise do experimento com polinômio.

# Apenas efeito linear de potássio.
m0 <- lm(rengrao ~ bloco + A * potassio,
         data = tb[-74, ])

# Notável falta de ajuste.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Retorna o gráfico de vários ajustes de polinômio.
poly_fits <- map(1:4,
                 .f = function(d) {
                     ggplot(data = tb[-74, ],
                            mapping = aes(x = potassio,
                                          y = rengrao,
                                          color = A)) +
                         facet_wrap(facets = as.character(d)) +
                         geom_jitter(pch = 1, width = 5,
                                     show.legend = FALSE) +
                         geom_smooth(method = "lm",
                                     formula = y ~ poly(x, degree = d),
                                     show.legend = FALSE) +
                         stat_summary(geom = "point",
                                      fun.y = "mean",
                                      pch = 19,
                                      show.legend = FALSE)
                 })

# Exibe o resultado de vários ajustes polinomiais.
invoke(.f = gridExtra::grid.arrange, poly_fits)

# Ajuste com polinômios.
m0 <- lm(rengrao ~ bloco + A * (potassio + I(potassio^2) + I(potassio^3)),
         data = tb[-74, ])

# Inspeção.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Verifica a falta de ajuste em cada nível de A.
ggplot(data = cbind(m0$model, r = residuals(m0), f = fitted(m0)),
       mapping = aes(x = f, y = r)) +
    facet_wrap(facets = ~A) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth(method = "loess", se = FALSE)

# Quadro de análise de variância.
anova(m0)
drop1(m0, scope = . ~ ., test = "F")

# NOTE: O a significância do termo cúbico está na "zona cinza".

# Modelo até efeito quadrático.
m1 <- lm(rengrao ~ bloco + A * (potassio + I(potassio^2)),
         data = tb[-74, ])

# Razão entre modelos encaixados. Testa a significância dos termos de 3º
# grau.
anova(m1, m0)

# COMMENT: Tomando a decisão de forma fria e procedural, ao nível
# nominal de 5%, não há evidência que suporte efeito de 3º do potássio.

# Quadro de testes F.
anova(m1)

# NOTE: Um modelo com coeficiente de 2º grau comum aos níveis de água é
# mais parsimonioso.

# Termo quadrático comum a todos os níveis de água.
m2 <- lm(rengrao ~ bloco + A * potassio + I(potassio^2),
         data = tb[-74, ])
anova(m2, m1)

# Estimativas dos parâmetros.
summary(m2)

# Como ficam as equações?
m2$assign
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m2)["(Intercept)"] + sum(0.2 * coef(m2)[2:5]),
        coef(m2)["potassio"],
        coef(m2)["I(potassio^2)"])
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m2)["(Intercept)"] + sum(0.2 * coef(m2)[2:5]) + coef(m2)["A50"],
        coef(m2)["potassio"] + coef(m2)["A50:potassio"],
        coef(m2)["I(potassio^2)"])
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m2)["(Intercept)"] + sum(0.2 * coef(m2)[2:5]) + coef(m2)["A62.5"],
        coef(m2)["potassio"] + coef(m2)["A62.5:potassio"],
        coef(m2)["I(potassio^2)"])

# Para obter-se as equações precisa-se conhecer a restrição paramétrica
# empregada para os fatores qualitativos (no caso, blocos e A) e saber
# selecionar e ponderar os coeficientes corretamente. Todavia, em alguns
# casos pode-se reajustar o modelo modelo de modo a facilitar bastante a
# determinação das equações. Veja a seguir.

m3 <- lm(rengrao ~  0 + A/potassio + I(potassio^2) + bloco,
         data = tb[-74, ],
         contrasts = list(bloco = "contr.sum"))
anova(m3, m2) # A soma de quadrados igual diz que são o mesmo modelo.

# Equações.
coef(m3)
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m3)["A37.5"],
        coef(m3)["A37.5:potassio"],
        coef(m3)["I(potassio^2)"])
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m3)["A50"],
        coef(m3)["A50:potassio"],
        coef(m3)["I(potassio^2)"])
sprintf("(%0.1f) + (%0.3f) * x + (%0.5f) * x^2",
        coef(m3)["A62.5"],
        coef(m3)["A62.5:potassio"],
        coef(m3)["I(potassio^2)"])

# Médias ajustadas.
emm <- emmeans(m2,
               specs = ~A + potassio,
               at = list(potassio = seq(0, 180, by = 3))) %>%
    as.data.frame()
head(emm)

# Gráfico com bandas de confiança.
ggplot(data = emm,
       mapping = aes(x = potassio, y = emmean)) +
    facet_wrap(facets = ~A) +
    geom_ribbon(mapping = aes(ymin = lower.CL, ymax = upper.CL),
                fill = "tomato",
                alpha = 0.7) +
    geom_line() +
    geom_point(data = tb[-74, ],
               mapping = aes(x = potassio, y = rengrao))

# Dados usados no ajuste.
tb_used <- m2$model
tb_used$fitted <- fitted(m2)

# R² dentro de cada nível de água.
tb_used %>%
    group_by(A) %>%
    summarise(R2 = cor(rengrao, fitted)^2)

# R² global.
tb_used %>%
    summarise(R2 = cor(rengrao, fitted)^2)

# Extraindo o R² global do modelo ajustado.
summary(m2)$r.squared

#-----------------------------------------------------------------------
# Ajuste de modelo não linear resposta-platô.

ggplot(data = tb[-74, ],
       mapping = aes(x = potassio,
                     y = rengrao)) +
    facet_wrap(facets = ~agua) +
    geom_point()

# TIP: Primeiro faça ajustes separados cada condição experimental
# (e.g. nível de água). Depois que ajustar, vá combinando os dados em
# ajustes maiores que acomodam mais fatores simultaneamente.

# Função e lista de valores iniciais para orientar o ajuste.
mod_fun <- function(x, b0, b1, bb) {
    b0 + b1 * x * (x <= bb) + b1 * bb * (x > bb)
}
start <- list(b0 = 12, b1 = 15/50, bb = 50)

# Testa se a função está próxima dos dados.
ggplot(data = filter(tb[-74, ], agua == "37.5"),
       mapping = aes(x = potassio,
                     y = rengrao)) +
    geom_point() +
    stat_function(fun = mod_fun, args = start)

# Ajuste para o nível 37.5.
n_375 <- nls(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             data = filter(tb, A == "37.5"),
             start = list(b0 = 12, b1 = 15/50, bb = 50))
coef(n_375)

# Ajuste para o nível 50.
n_500 <- nls(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             data = filter(tb, A == "50"),
             start = list(b0 = 15, b1 = 15/50, bb = 60))
coef(n_500)

# Ajuste para o nível 62.5.
n_625 <- nls(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             data = filter(tb[-74, ], A == "62.5"),
             start = list(b0 = 15, b1 = 15/50, bb = 90))
coef(n_625)

# Ajuste para cada nível de forma sequêncial. Detalhe, os valores
# iniciais são os mesmos o que pode não ser problemático.
nfit <- nlsList(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                    b1 * bb * (potassio > bb) | A,
                data = tb[-74, ],
                start = c(b0 = 15, b1 = 15/50, bb = 50))
# summary(nfit)
coef(nfit)

# Estrutura de dados agrupados para acomodar o efeito de bloco como
# termo de efeito aleatório.
tb_grouped <- groupedData(rengrao ~ potassio | bloco, tb[-74, ])

# Ajuste com todos os fatores experimentais/estruturais acomodados.
fit0 <- nlme(rengrao ~ b0 + b1 * potassio * (potassio <= bb) +
                 b1 * bb * (potassio > bb),
             fixed = b0 + b1 + bb ~ A,
             random = b0 ~ 1,
             start = c(13.52, 15.71 - 13.52, 15.43 - 13.52,
                        0.22,             0,             0,
                          49,       58 - 49,       93 - 49),
             data = tb_grouped,
             verbose = TRUE)
summary(fit0)

# Quadro de testes F para efeito de água em cada conjunto de parâmetros.
anova(fit0, type = "marginal")

# COMMENT: os testes indicam que b0 e b1 não diferem devido a fator
# água. Assim, pode-se simplificar o modelo.

fit1 <- update(fit0,
               fixed = list(b0 ~ 1, b1 ~ 1, bb ~ A),
               start = c(13.52,
                         0.22,
                         49, 58 - 49, 93 - 49))

# Compara os modelos encaixados.
anova(fit0, fit1)

# Faz a predição.
pred <- expand.grid(A = levels(tb$A),
                    potassio = seq(0, 180, length.out = 51))
pred$y <- predict(fit1, newdata = pred, level = 0)

# Gráfico dos valores preditos.
ggplot(data = tb[-74, ],
       mapping = aes(x = potassio,
                     y = rengrao)) +
    facet_wrap(facets = ~A) +
    geom_point() +
    geom_line(data = pred,
              mapping = aes(x = potassio, y = y))

# Para fazer as bandas de confiança tem-se que fazer operações
# matriciais. Não é difícil. Veja exemplo em
# http://leg.ufpr.br/~walmes/analises/CECarducci/secagem-solos.html.

#-----------------------------------------------------------------------
# Modelo de regressão múltipla.

url <- "http://leg.ufpr.br/~walmes/data/chimarrita.txt"
tb <- read_tsv(file = url, comment = "#")
attr(tb, "spec") <- NULL
str(tb)

tb <- filter(tb, fer == "sim")
str(tb)

lattice::splom(select(tb, ends_with("48")))
lattice::splom(select(tb, ends_with("60")))

names(tb)

tb_sel <- tb %>%
    select(matches("[[:alpha:]]0$"), lesao48, lesao60)
tb_sel

tb_long <- tb_sel %>%
    gather(key = "variavel", value = "valor", -lesao60, -lesao48)
str(tb_long)

ggplot(data = tb_long,
       mapping = aes(x = valor, y = lesao60)) +
    facet_wrap(facets = ~variavel, scale = "free_x") +
    geom_point() +
    geom_smooth()

names(tb_sel)

m0 <- lm(lesao60 ~ (cor0 + ida0 + ms0 + ss0 + f0 + b0)^2,
         data = tb_sel)

par(mfrow = c(2, 2))
plot(m0)
layout(1)

summary(m0)

m1 <- update(m0, . ~ ms0 + b0, data = tb_sel)
anova(m0, m1)

summary(m1)

m2 <- step(m0)
summary(m2)

pred <- with(na.omit(tb_sel),
             expand.grid(cor0 = seq(min(cor0), max(cor0), length.out = 20),
                         # ms0 = seq(min(ms0), max(ms0), length.out = 20),
                         ms0 = mean(ms0),
                         ida0 = mean(ida0),
                         ss0 = mean(ss0),
                         # b0 = mean(b0),
                         b0 = seq(min(b0), max(b0), length.out = 20)))
pred$lesao60 <- predict(m2, newdata = pred)

ggplot(data = pred,
       mapping = aes(x = b0, y = ms0, fill = lesao60, z = lesao60)) +
    geom_tile() +
    geom_contour(color = "black") +
    scale_fill_distiller(palette = 4)

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

#-----------------------------------------------------------------------
