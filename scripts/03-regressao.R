#-----------------------------------------------------------------------
# Curso em Planejamento e Análise de Experimentos com R
#   · Análise de Regressão Linear e Não Linear
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2020-fev-18 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Carrega pacotes.

library(lattice)
library(latticeExtra)
library(asbio) # press().
library(car)   # residualPlots() além de outras.

#-----------------------------------------------------------------------
# Dados de inventário florestal.

cr <- read.csv2("http://leg.ufpr.br/~walmes/data/cristo-rei.csv",
                header = TRUE, stringsAsFactors = FALSE)
rb <- read.csv2("http://leg.ufpr.br/~walmes/data/reboucas.csv",
                header = TRUE, stringsAsFactors = FALSE)
str(cr)

da <- rbind(cbind(bair = "CRei", cr),
            cbind(bair = "Rebo", rb))
str(da)

da <- subset(da, tam > 10)

#-----------------------------------------------------------------------
# Visualização.

xyplot(valor ~ tam, groups = bair,
       data = da,
       auto.key = TRUE,
       type = c("p", "smooth", "g"),
       xlab = expression("Tamanho do imóvel" ~ (m^2)),
       ylab = "Valor de venda (R$)")

xyplot(valor ~ tam, groups = bair,
       data = da,
       auto.key = TRUE,
       scales = list(x = list(log = 10), y = list(log = 10)),
       type = c("p", "smooth", "g"),
       xlab = expression("Tamanho do imóvel" ~ (log ~ m^2)),
       ylab = "Valor de venda (R$)",
       xscale.components = xscale.components.log10.3,
       yscale.components = yscale.components.log10.3)

#-----------------------------------------------------------------------

# Dividir o preço por 1000 e criar o log das variáveis.
da <- within(da, {
    valor <- valor/1000
    lvalor <- log10(valor)
    ltam <- log10(tam)
})

str(da)

splom(da[-(1:2)], groups = da$bair, cex = 0.2)

# Modelo com as variáveis na escala original.
m0 <- lm(valor ~ bair * tam, data = da)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Modelo com as variáveis na escala log.
m0 <- lm(lvalor ~ bair * ltam, data = da)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

# Modelo com inclusão de quartos e banheiros.
m1 <- lm(lvalor ~ bair * ltam + quar + banh, data = da)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m1)
layout(1)

# Resíduos parciais com teste para inclusão de termo quadrático.
residualPlots(m1)

m2 <- lm(lvalor ~ bair * (ltam + I(ltam^2)) + quar + banh, data = da)
anova(m1, m2)

# Estimativas e medidas de ajuste.
summary(m2)

# Distribuição de frequência de quarto e banheiro.
xtabs(~quar + banh, data = da)

#-----------------------------------------------------------------------
# Predição.

pred <- with(da, expand.grid(bair = levels(bair),
                             quar = 2,
                             banh = 2,
                             ltam = seq(min(ltam), max(ltam),
                                        length.out = 20)))

fit <- predict(m2, newdata = pred, interval = "confidence")
pred <- cbind(pred, fit)
str(pred)

source(paste0("https://raw.githubusercontent.com/",
              "walmes/wzRfun/master/R/panel.cbH.R"))

p1 <- xyplot(fit ~ ltam, data = pred, type = "l",
             ly = pred$lwr, uy = pred$upr,
             xlab = expression("Tamanho do imóvel" ~ (log ~ m^2)),
             ylab = "Valor de venda (R$)",
             cty = "bands",
             groups = bair, alpha = 0.5,
             prepanel = prepanel.cbH,
             panel.groups = panel.cbH,
             panel = panel.superpose)
p1

xyplot(valor ~ tam, groups = bair,
       data = subset(da, quar == 2 & banh == 2),
       auto.key = TRUE,
       scales = list(x = list(log = 10), y = list(log = 10)),
       xlab = expression("Tamanho do imóvel" ~ (m^2)),
       ylab = "Valor de venda (R$)",
       xscale.components = xscale.components.logpower,
       yscale.components = yscale.components.logpower) +
    as.layer(p1)

cat(format(Sys.time(),
           format = "Updated in %Y-%m-%d.\n\n"))
sessionInfo()

source("config/setup.R")

#-----------------------------------------------------------------------
# Load packages.

library(lattice)
library(latticeExtra)
library(asbio)
library(car)

#-----------------------------------------------------------------------
# Dados de inventário florestal.

dap <- read.table("http://www.leg.ufpr.br/~walmes/data/dap.txt",
                  header = TRUE, sep = "\t")
names(dap) <- c("d", "h")

str(dap)

# Ordenar para evitar efeito espaguete quando plotar.
dap <- dap[order(dap$d), ]

# Nova base que contém d e h observados.
dapcc <- dap[complete.cases(dap), ]

# Renomeia as linhas
rownames(dapcc) <- NULL
head(dapcc)

#-----------------------------------------------------------------------
# Visualização.

xyplot(h ~ d,
       data = dap,
       type = c("p", "smooth", "g"),
       xlab = "Diâmetro à altura do peito (cm)",
       ylab = "Altura (m)")

#-----------------------------------------------------------------------

# Linear com raíz.
m0 <- lm(h ~ d + sqrt(d), data = dapcc)
summary(m0)

# Diagnóstico.
par(mfrow = c(2, 2))
plot(m0)
layout(1)

#-----------------------------------------------------------------------
# Medidas de influência.

inf <- influence.measures(m0)

# Sinalizando os pontos influentes.
summary(inf)

# Influentes pelo DFFITS.
dfits <- inf$is.inf[, 4]

er <- extendrange(dapcc$d, f = 0.05)
pred <- data.frame(d = seq(er[1], er[2], length.out = 30))
pred$y <- predict(m0, newdata = pred)

plot(h ~ d, dapcc)
lines(y ~ d, data = pred, col = 2)
with(dapcc, points(d[dfits], h[dfits], col = 2, pch = 19))

#-----------------------------------------------------------------------

id <- which(dfits)

# Refazer a análise com os pontos removidos.
m1 <- update(m0, data = dapcc[-id, ])
summary(m1)

# Fazendo predição com intervalo de confiança e predição futura.
Yp <- predict(m1, newdata = pred, interval = "confidence")
Yf <- predict(m1, newdata = pred, interval = "prediction")
colnames(Yf) <- toupper(colnames(Yf))

pred <- cbind(pred, Yp, Yf)
str(pred)

#-----------------------------------------------------------------------

# Inclusão da expressão do modelo.
c0 <- coef(m1)
co <- format(c(abs(c0), summary(m1)$r.squared), digits = 3, trim = TRUE)
sinal <- ifelse(c0 < 0, "-", "+")
co[seq_along(sinal)] <- paste(sinal, co[seq_along(sinal)], sep = "")

l <- c("Predito", "IC predito", "IC obs. futura")
key <- list(lines = list(lty = 1),
            text = list(l[1]),
            rect = list(col = c("red"), alpha = 0.1, lty = 3),
            text = list(l[2]),
            rect = list(col = c("blue"), alpha = 0.1, lty = 3),
            text = list(l[3]))

source(paste0("https://raw.githubusercontent.com/",
              "walmes/wzRfun/master/R/panel.cbH.R"))

p1 <- xyplot(h ~ d, data = dapcc[-id, ], xlim = er, xlab = "DAP (cm)",
             ylab = "Altura (m)", key = key)

p2 <- xyplot(fit ~ d, data = pred, type = "l", ly = pred$lwr,
             uy = pred$upr, cty = "bands", fill = "red",
             prepanel = prepanel.cbH, panel = panel.cbH)

p3 <- xyplot(FIT ~ d, data = pred, type = "l", ly = pred$LWR,
             uy = pred$UPR, cty = "bands", fill = "blue",
             prepanel = prepanel.cbH, panel = panel.cbH)

eqn <- substitute(hat(h) == b0 ~ b1 * d ~ b2 * sqrt(d) ~~~~ (R^2 == r2),
                  list(b0 = co[1], b1 = co[2], b2 = co[3], r2 = co[4]))

p1 +
    as.layer(p2) +
    as.layer(p3) +
    layer(panel.text(x = 20, y = 10, label = eqn, bty = "n"))

cat(format(Sys.time(),
           format = "Updated in %Y-%m-%d.\n\n"))
sessionInfo()

#-----------------------------------------------------------------------
# Load packages.

library(lattice)
library(latticeExtra)
library(car)
library(alr3)
library(nlstools)
library(nls2)
library(rootSolve)
library(wzRfun)

# url <- "http://nls2.googlecode.com/svn-history/r4/trunk/R/as.lm.R"
# download.file(url, dest = basename(url))
# path <- ifelse(Sys.info()["user"] == "walmes", basename(url), url)
# source(path)
source("~/Dropbox/CursoR/GeneticaEsalq/as.lm.R")

#-----------------------------------------------------------------------
# Ajuste de modelo de regressão não linear.

# turk0
str(turk0)

xyplot(Gain ~ A, data = turk0, type = c("p", "smooth"))

#-----------------------------------------------------------------------
# Valores iniciais baseados na interpretação gráfica.
# Modelo: th0 + th1 * x/(th2 + x);

start <- list(th0 = 625, th1 = 800 - 625, th2 = 0.1)

xyplot(Gain ~ A, data = turk0) +
    layer(panel.curve(th0 + th1 * x/(th2 + x)),
          data = start)

#-----------------------------------------------------------------------
# Ajuste.

n0 <- nls(Gain ~ th0 + th1 * A/(th2 + A),
          data = turk0,
          start = start)
summary(n0)

xyplot(Gain ~ A, data = turk0)+
    layer(panel.curve(th0 + th1 * x/(th2 + x), col = 2),
          data = as.list(coef(n0)))

#-----------------------------------------------------------------------
# Intervalos de confiança.

# Baseado na log-verossimilhança.
confint(n0)

# Baseado na aproximação quadrática da verossimilhança, conhecido como
# intervalos de Wald ou assintóticos. São simétricos por construção.
confint.default(n0)

#-----------------------------------------------------------------------
# Colocar bandas de confiança.

# Modelo escrito como função dos parâmetros (theta).
f <- function(theta, xx){
    with(as.list(theta),
         th0 + th1 * xx/(th2 + xx))
}

# Matriz de derivadas parciais em theta (n x p).
gradient(f, x = coef(n0), xx = c(0, 0.2, 0.4))

pred <- data.frame(A = seq(0, 0.5, l = 20))
pred$fit <- predict(n0, newdata = pred)
der <- gradient(f, x = coef(n0), xx = pred$A)
str(der)

# Etapas até o IC passando pelo erro padrão e margem de erro.
F <- der
U <- chol(vcov(n0))
pred$se <- sqrt(apply(F %*% t(U), 1, function(x) sum(x^2)))
tval <- qt(p = c(lwr = 0.025, upr = 0.975), df = df.residual(n0))
me <- outer(pred$se, tval, "*")
pred <- cbind(pred, sweep(me, 1, pred$fit, "+"))
str(pred)

# Equação do modelo ajustado.
coef(n0)
formula(n0)

# Observados, preditos e a banda de confiança.
xyplot(Gain ~ A, data = turk0) +
    as.layer(xyplot(fit ~ A, data = pred, type = "l",
                    prepanel = prepanel.cbH, cty = "bands",
                    ly = pred$lwr, uy = pred$upr, panel = panel.cbH))

#-----------------------------------------------------------------------
# Região de confiança para os parâmetros.

apropos("contour")
ncp0 <- nlsContourRSS(n0)
plot(ncp0)

ncr0 <- nlsConfRegions(n0)
plot(ncr0)

#-----------------------------------------------------------------------
# Consumo de energia (KWH/dia) em função da temperatura (F).

str(segreg)
xyplot(C ~ Temp, data = segreg, type = c("p", "smooth"))

#-----------------------------------------------------------------------
# Ajuste do modelo platô linear.
# f(x) = th0 + th1 * (x - th2) * (x >= th2) + 0 * (x < th2)

start <- list(th0 = 75, th1 = 0.5, th2 = 50)
xyplot(C ~ Temp, data = segreg) +
    layer(panel.curve(th0 + th1 * (x - th2) * (x >= th2) +
                      0 * (x < th2)), data = start)

# Ajuste.
n2 <- nls(C ~ th0 + th1 * (Temp - th2) * (Temp >= th2) +
              0 * (Temp < th2),
          data = segreg, start = start)

# Estimativas e medidas de ajuste.
summary(n2)

# Valor de F e R².
R2nls(n2)

# Intervalos de confiança.
# confint(n2)
confint.default(n2)

# Observados e preditos.
xyplot(C ~ Temp, data = segreg) +
    layer(panel.curve(th0 + th1 * (x - th2) * (x >= th2) +
                      0 * (x < th2), col = 4), data = as.list(coef(n2)))

#-----------------------------------------------------------------------
# Análise dos resíduos.

m2 <- as.lm(n2)
par(mfrow = c(2, 2))
plot(m2)
layout(1)

#-----------------------------------------------------------------------
# Colocar bandas de confiança.

f <- function(theta, xx) {
    with(as.list(theta),
         th0 + th1 * (xx - th2) * (xx >= th2) +
         0 * (xx < th2))
}

pred <- data.frame(Temp = sort(c(seq(10, 80, l = 100),
                                 coef(n2)["th2"] +
                                 c(-0.001, 0, 0.001))))

pred$fit <- predict(n2, newdata = pred)
der <- gradient(f, x = coef(n2), xx = pred$Temp)
str(der)

F <- der
U <- chol(vcov(n2))
pred$se <- sqrt(apply(F %*% t(U), 1, function(x) sum(x^2)))
tval <- qt(p = c(lwr = 0.025, upr = 0.975), df = df.residual(n2))
me <- outer(pred$se, tval, "*")
pred <- cbind(pred, sweep(me, 1, pred$fit, "+"))
str(pred)

# Equação do modelo ajustado.
coef(n2)
formula(n2)

# Arredonda as estimativas.
theta <- mapply(round,
                x = coef(n2),
                digits = c(2, 4, 2),
                SIMPLIFY = FALSE)
theta

# Equação.
formula(n2)
eq <- substitute(
    expr = c(
        expression(C==th0~", se"~Temp < th2),
        expression(C==th0 + th1 * (Temp - th2)~", se"~Temp >= th2)),
    env = theta)
eval(eq)

# Observados, preditos e a banda de confiança.
xyplot(C ~ Temp, data = segreg) +
    as.layer(xyplot(fit ~ Temp, data = pred, type = "l",
                    prepanel = prepanel.cbH, cty = "bands",
                    ly = pred$lwr, uy = pred$upr,
                    panel = panel.cbH)) +
    layer(panel.key(points = FALSE, text = eval(eq),
                    corner = c(0.05, 0.95)))

## #-----------------------------------------------------------------------
## # Exemplos da documentação.
##
## # library(wzRfun)
## # help(rp.nls, h = "html")
##
## # Se não possuir o pacote wzRfun, carregue a função da fonte.
## source(paste0("https://raw.githubusercontent.com/walmes/wzRfun/",
##               "master/R/rp.nls.R"))
##
## #--------------------------------------------
## # A non linear regression.
##
## library(rpanel)
##
## data(turk0, package = "alr3")
## str(turk0)
##
## plot(Gain ~ A, data = turk0,
##      xlab = "Metionine", ylab = "Weight gain")
##
## turk.fit <- rp.nls(model = Gain ~ Int + (Asy - Int) * A/(Half + A),
##                    data = turk0,
##                    start = list(Int = c(600, 650),
##                                 Asy = c(750, 850),
##                                 Half = c(0, 0.2)),
##                    extra = function(Int, Asy, Half) {
##                        abline(h = c(Asy, Int), v = Half,
##                               col = "green")
##                    },
##                    xlab = "Metionine", ylab = "Weight gain")
##
## summary(turk.fit)
## confint(turk.fit)
##
## f <- formula(turk.fit)
##
## plot(Gain ~ A, data = turk0,
##      xlab = "Metionine", ylab = "Weight gain")
## with(as.list(coef(turk.fit)), {
##     eval(call("curve", f[[3]], add = TRUE,
##               xname = intersect(all.vars(f[-2]), names(turk0))))
## })
##
## #--------------------------------------------
## # A more interesting example.
##
## library(lattice)
##
## xyplot(rate ~ conc, groups = state, data = Puromycin,
##        type = c("p", "smooth"), auto.key = TRUE)
##
## Puro.fit <- rp.nls(model = rate ~
##                        Int + (Top - Int) * conc/(Half + conc),
##                    data = Puromycin,
##                    start = list(Int = c(20, 70),
##                                 Top = c(100, 200),
##                                 Half = c(0, 0.6)),
##                    subset = "state",
##                    gpar = list(try = list(col = 2, lty = 2),
##                                fit = list(col = "blue", lwd = 1.5)),
##                    xlab = "Concentration", ylab = "Rate",
##                    xlim = c(0, 1.2), ylim = c(40, 220))
##
## length(Puro.fit)
## sapply(Puro.fit, coef)
## sapply(Puro.fit, logLik)
## sapply(Puro.fit, deviance)
##
## #-----------------------------------------------------------------------
## # 1. Ajuste de curvas de retenção de água do solo.
##
## cra <- read.table("http://www.leg.ufpr.br/~walmes/data/cra_manejo.txt",
##                   header = TRUE, sep = "\t")
##
## cra$tens[cra$tens == 0] <- 0.1
## cras <- subset(cra, condi == "LVA3,5")
## cras <- aggregate(umid ~ posi + prof + tens, data = cras, FUN = mean)
## cras$caso <- with(cras, interaction(posi, prof))
## cras$ltens <- log(cras$tens)
##
## xyplot(umid ~ ltens | posi, groups = prof, data = cras,
##        type = c("p", "a"))
##
## # modelo : van Genuchten com retrição de Mualem.
## # ltens  : representado por ltens (log da tensão matricial, psi).
## # umid   : representado por umid, conteúdo de água do solo ( % ).
## # Us     : assíntota inferior, mínimo da função, quando x -> +infinito.
## # Ur     : assíntota superior, máximo da função, quando x -> -infinito.
## # al     : locação, translada o ponto de inflexão.
## # n      : forma, altera a taxa ao redor da inflexão.
##
## model <- umid ~ Ur + (Us - Ur)/(1 + exp(n * (al + ltens)))^(1 - 1/n)
##
## start <- list(Ur = c(init = 0.2, from = 0, to = 0.5),
##               Us = c(init = 0.6, from = 0.4, to = 0.8),
##               al = c(1, -2, 4),
##               n = c(1.5, 1, 4))
##
## cra.fit <- rp.nls(model = model,
##                   data = cras,
##                   start = start,
##                   subset = "caso")
##
## sapply(cra.fit, coef)
##
## #-----------------------------------------------------------------------
## # 2. Curva de produção em função da desfolha do algodão.
##
## cap <- read.table("http://www.leg.ufpr.br/~walmes/data/algodão.txt",
##                   header = TRUE, sep = "\t", encoding = "latin1")
##
## cap$desf <- cap$desf/100
## cap <- subset(cap, select = c(estag, desf, pcapu))
## cap$estag <- factor(cap$estag, labels = c("vegetativo", "botão floral",
##                                           "florescimento", "maçã",
##                                           "capulho"))
## str(cap)
##
## xyplot(pcapu ~ desf | estag, data = cap, layout = c(5, 1),
##        xlab = "Nível de desfolha artificial", ylab = "Peso de capulhos")
##
## # modelo: potência.
## # desf  : representado por desf (nível de desfolha artifical).
## # pcapu : representado por pcapu (peso de capulhos), produto do algodão.
## # f0    : intercepto, valor da função quando x=0 (situação ótima).
## # delta : diferença no valor da função para x=0 e x=1 (situação
## #         extrema).
## # curv  : forma, indica como a função decresce, se th3=0 então função
## #         linear.
##
## model <- pcapu ~ f0 - delta * desf^exp(curv)
## start <- list(f0 = c(30, 25, 35),
##               delta = c(8, 0, 16),
##               curv = c(0, -2, 4))
##
## cap.fit <- rp.nls(model = model,
##                   data = cap,
##                   start = start,
##                   subset = "estag")
##
## length(cap.fit)
## sapply(cap.fit, coef)
## lapply(cap.fit, summary)
##
## #-----------------------------------------------------------------------
## # 3. Curva de produção em função do nível de potássio no solo.
##
## soja <- read.table("http://www.leg.ufpr.br/~walmes/data/soja.txt",
##                    header = TRUE, sep = "\t", encoding = "latin1",
##                    dec = ",")
##
## soja$agua <- factor(soja$agua)
## str(soja)
##
## xyplot(rengrao ~ potassio|agua, data=soja)
##
## # modelo: linear-plato.
## # x: representado por potássio, conteúdo de potássio do solo.
## # y: representado por rengrao, redimento de grãos por parcela.
## # f0: intercepto, valor da função quando x=0.
## # tx: taxa de incremento no rendimento por unidade de x.
## # brk: valor acima do qual a função é constante.
##
## model <- rengrao ~ f0 + tx * potassio * (potassio < brk) +
##     tx * brk * (potassio >= brk)
## start <- list(f0 = c(15, 5, 25),
##               tx = c(0.2, 0, 1),
##               brk = c(50, 0, 180))
##
## pot.fit <- rp.nls(model = model, data = soja, start = start,
##                   subset = "agua")
##
## sapply(pot.fit, coef)
##
## #-----------------------------------------------------------------------
## # 4. Curva de lactação.
##
## lac <- read.table("http://www.leg.ufpr.br/~walmes/data/saxton_lactacao1.txt",
##                   header = TRUE, sep = "\t", encoding = "latin1")
##
## lac$vaca <- factor(lac$vaca)
## str(lac)
##
## xyplot(leite ~ dia | vaca, data = lac)
##
## # modelo: de Wood (nucleo da densidade gama).
## # x: representado por dia, dia após parto.
## # y: representado por leite, quantidade produzida.
## # th1: escala, desprovido de interpretação direta.
## # th2: forma, desprovido de interpretação direta.
## # th3: forma, desprovido de interpretação direta.
##
## model <- leite ~ th1 * dia^th2 * exp(-th3 * dia)
## start <- list(th1 = c(15, 10, 20),
##               th2 = c(0.2, 0.05, 0.5),
##               th3 = c(0.0025, 0.001, 0.008))
##
## lac.fit <- rp.nls(model = model,
##                   data = lac,
##                   start = start,
##                   subset = "vaca",
##                   xlim = c(0, 310))
##
## sapply(lac.fit, coef)
##
## #-----------------------------------------------------------------------
## # 5. Curvas de crescimento em placa de petri.
##
## cre <- read.table("http://www.leg.ufpr.br/~walmes/data/cresmicelial.txt",
##                   header = TRUE, sep = "\t", encoding = "latin1")
##
## cre$isolado <- factor(cre$isolado)
## cre$mmdia <- sqrt(cre$mmdia)
## str(cre)
##
## xyplot(mmdia ~ temp | isolado, data = cre)
##
## # modelo: quadrático na forma canônica.
## # x: representado por temp, temperatura da câmara de crescimento.
## # y: representado por mmdia, taxa média de crescimento.
## # thy: valor da função no ponto de máximo.
## # thc: curvatura ou grau de especificidade à condição ótima.
## # thx: ponto de máximo, temperatura de crescimento mais rápido.
##
## model <- mmdia ~ thy + thc * (temp - thx)^2
## start <- list(thy = c(4, 0, 7),
##               thc = c(-0.05, 0, -0.5),
##               thx = c(23, 18, 30))
##
## mic.fit <- rp.nls(model = model,
##                   data = cre,
##                   start = start,
##                   subset = "isolado",
##                   xlim = c(17, 31),
##                   ylim = c(0, 6))
##
## t(sapply(mic.fit, coef))
##
## #-----------------------------------------------------------------------
## # 6. Curva de secagem do solo em microondas.
##
## sec <- read.table("http://www.leg.ufpr.br/~walmes/data/emr11.txt",
##                   header = TRUE, sep = "\t", encoding = "latin1")
## str(sec)
##
## xyplot(umrel ~ tempo|nome, data=sec)
##
## # modelo: logístico.
## # x: representado por tempo, período da amostra dentro do microondas.
## # y: representado por umrel, umidade relativa o conteúdo total de água.
## # th1: assíntota superior.
## # th2: tempo para evaporar metade do conteúdo total de água.
## # th3: proporcional à taxa máxima do processo.
##
## model <- umrel ~ th1/(1 + exp(-(tempo - th2)/th3))
## start <- list(th1 = c(1, 0.8, 1.2),
##               th2 = c(15, 0, 40),
##               th3 = c(8, 2, 14))
##
## sec.fit <- rp.nls(model = model,
##                   data = sec,
##                   start = start,
##                   subset = "nome")
##
## sapply(sec.fit, coef)
## lapply(sec.fit, summary)

cat(format(Sys.time(),
           format = "Updated in %Y-%m-%d.\n\n"))
sessionInfo()
