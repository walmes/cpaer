#-----------------------------------------------------------------------
# Curso em Planejamento e Análise de Experimentos com R
#   · Análise Exploratória de Dados Experimentais
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                leg.ufpr.br/~walmes · github.com/walmes
#                                        walmes@ufpr.br · @walmeszeviani
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2020-fev-19 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Pacotes.

# Manipulação e visualização de dados.
library(tidyverse)

#-----------------------------------------------------------------------
# Análise exploratória de dados.

# COMMENT: Esperimento fatorial que avaliou o volume de raízes para
# genótipos de sorgo em função da dose de um estimulante de
# crescimento. O ensaio foi feito em delineamento inteiramente
# casualizado.

# Importa os dados pela URL.
# getwd()
# setwd("")
url <- "http://leg.ufpr.br/~walmes/data/volume.txt"
tb <- read_tsv(file = url)
str(tb)

# Elimina o atributo para prints pais enxutos.
attr(tb, "spec") <- NULL

# Classe do objeto. Determina operações e propriedades.
class(tb)

# Visão em tabela interativa.
View(tb)

# Tabela de ocorrência dos pontos experimentais.
xtabs(~gen + dose, data = tb)

# Médias por estratos.
tb %>%
    group_by(gen) %>%
    summarise_at(.vars = "volu", .funs = c("mean", "sd")) %>%
    arrange(mean)

# Tabela cruzada com médias nas intercções.
tb %>%
    group_by(gen, dose) %>%
    summarise_at(.vars = "volu", .funs = "mean") %>%
    ungroup() %>%
    spread(key = "dose", value = "volu")

# Diagrama de dispersão.
ggplot(data = tb,
       mapping = aes(x = gen, y = volu, color = factor(dose))) +
    geom_point() +
    labs(x = "Genótipo", y = "Volume de raízes", color = "Dose")

# Níveis ordenados pela resposta média.
ggplot(data = tb,
       mapping = aes(x = reorder(gen, volu),
                     y = volu,
                     color = factor(dose))) +
    geom_point() +
    labs(x = "Genótipo", y = "Volume de raízes", color = "Dose")

# Linhas conectando médias.
ggplot(data = tb,
       mapping = aes(x = reorder(gen, volu),
                     y = volu,
                     color = factor(dose))) +
    geom_point() +
    stat_summary(mapping = aes(group = dose),
                 geom = "line",
                 fun.y = "mean") +
    labs(x = "Genótipo", y = "Volume de raízes", color = "Dose")

# Usando facetas para repartir.
ggplot(data = tb,
       mapping = aes(x = dose,
                     y = volu)) +
    facet_wrap(facets = ~gen) +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean") +
    labs(x = "Genótipo", y = "Volume de raízes")

#-----------------------------------------------------------------------
# Produção de soja em função de adubação com K e nível de água.

# ATTENTION: por comodidade, os nomes `url`, `tb`, etc são
# reutilizados. Isso é uma má prática mas se usado de forma planejada e
# consciente, pode economizar tempo.

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

# Médias por estratos.
tb %>%
    group_by(potassio) %>%
    summarise_at(.vars = "rengrao", .funs = c("mean", "sd"))

# Diagrama de dispersão.
ggplot(data = tb,
       mapping = aes(x = potassio,
                     y = rengrao,
                     color = factor(agua))) +
    # geom_point() +
    geom_jitter(width = 5) +
    stat_summary(mapping = aes(group = agua),
                 geom = "line",
                 fun.y = "mean") +
    labs(x = "Potássio no solo",
         y = "Rendimento de grãos",
         color = "Conteúdo de água") +
    theme(legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0))

# Usando facetas para repartir.
ggplot(data = tb,
       mapping = aes(x = potassio,
                     y = rengrao)) +
    facet_wrap(facets = ~agua) +
    geom_point() +
    stat_summary(mapping = aes(group = bloco),
                 geom = "line",
                 fun.y = "mean") +
    labs(x = "Potássio no solo",
         y = "Rendimento de grãos")

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

# Duas unidades de mensuração em cada parcela, ou seja, duas plantas por
# vaso.
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

# Coloca as fases de crescimento com níveis na ordem cronológica.
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
                 fun.y = "mean") +
    labs(x = "Nível de desfolha artificial",
         y = "Peso de capulhos por parcela")

#-----------------------------------------------------------------------
# Secagem do solo em microondas.

# Inspeciona os dados.
url <- "http://leg.ufpr.br/~walmes/data/emr11.txt"
browseURL(url)

# Importa os dados pela URL. Define os tipos de valor para leitura.
tb <- read_tsv(file = url,
               col_types = cols(
                   nome = col_factor(NULL),
                   solo = col_factor(NULL),
                   tempo = col_integer()))
str(tb)

# Imprime para reuso as especificações de cada variável.
spec(tb)

# Elimina o atributo para prints pais enxutos.
attr(tb, "spec") <- NULL

# Tabela de ocorrência dos pontos experimentais.
xtabs(~nome + tempo, data = tb)

# Usando facetas para repartir.
ggplot(data = tb,
       mapping = aes(x = tempo,
                     y = umrel)) +
    facet_wrap(facets = ~nome) +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean") +
    labs(x = "Período no microondas",
         y = "Conteúdo de umidade relativa extraída do solo")

# Adição de linha de tendência.
ggplot(data = tb,
       mapping = aes(x = tempo,
                     y = umrel)) +
    facet_wrap(facets = ~nome) +
    geom_point() +
    geom_smooth() +
    labs(x = "Período no microondas",
         y = "Conteúdo de umidade relativa extraída do solo",
         caption = "Linhas azuis usadas para indicar a tendência.")

#-----------------------------------------------------------------------
# Curva de retenção de água do solo.

# Inspeciona os dados.
url <- "http://leg.ufpr.br/~walmes/data/cra_manejo.txt"
browseURL(url)

# Importa os dados pela URL.
tb <- read_tsv(file = url,
               comment = "#",
               col_types = cols(
                   condi = col_factor(NULL),
                   posi = col_factor(NULL)))
str(tb)

# Imprime para reuso as especificações de cada variáveis.
spec(tb)

# Elimina o atributo para prints pais enxutos.
attr(tb, "spec") <- NULL

# Tabela de ocorrência dos pontos experimentais.
ftable(xtabs(~condi + posi + prof, data = tb))

# Usando facetas para repartir.
ggplot(data = tb,
       mapping = aes(x = tens,
                     y = umid,
                     color = posi)) +
    facet_wrap(facets = ~condi) +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean")

tb <- tb %>%
    mutate(tens0 = replace(tens, tens == 0, 0.25))

# Usando facetas para repartir.
ggplot(data = tb,
       mapping = aes(x = tens0,
                     y = umid,
                     color = posi)) +
    # facet_wrap(facets = ~condi) +
    facet_grid(facets = prof ~ condi) +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean") +
    scale_x_log10() +
    labs(x = "Tensão matricial",
         y = "Umidade do solo",
         color = "Posição de\ncoleta")

#-----------------------------------------------------------------------
# Temperatura corporal de suinos.

url <- "http://leg.ufpr.br/~walmes/data/preabate.txt"
browseURL(url)

tb <- read_tsv(file = url, locale = locale(decimal_mark = ","))
str(tb)

# Cria variável com codificação mais descritiva dos níveis.
tb <- tb %>%
    mutate(trat2 = case_when(trat == 1 ~ "Com aspersão",
                             trat == 2 ~ "Sem aspersão"))

xtabs(~trat2 + trat, data = tb)

ggplot(data = tb,
       mapping = aes(x = hora,
                     y = temp)) +
    facet_wrap(facets = ~trat2) +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean") +
    geom_smooth(span = 0.3) +
    labs(x = "Tempo de espera para abate (min)",
         y = expression("Temperatura corporal" ~ (""^degree * C))) +
    ggtitle(label = "Temperatura corporal de suínos")

ggplot(data = tb,
       mapping = aes(x = hora,
                     y = temp)) +
    facet_grid(facets = trat2 ~ rep) +
    geom_point() +
    # stat_summary(geom = "line", fun.y = "mean") +
    geom_smooth(span = 0.33) +
    labs(x = "Tempo de espera para abate (min)",
         y = expression("Temperatura corporal" ~ (""^degree * C))) +
    ggtitle(label = "Temperatura corporal de suínos",
            subtitle = "Tratamentos x Unidades Experimentais (Animais)")

#-----------------------------------------------------------------------
# Ocorrência de praga em

url <- "http://leg.ufpr.br/~walmes/data/mosca_algodao_aval.txt"
browseURL(url)

tb <- read_tsv(file = url, comment = "#")
str(tb)

# Série ao longo dos dias de avaliação.
ggplot(data = tb,
       mapping = aes(x = aval,
                     y = totnin)) +
    facet_wrap(facets = ~dexp) +
    geom_line(mapping = aes(group = rept),
              color = "gray") +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean", size = 1) +
    labs(x = "Dias após exposição",
         y = "Número de ninfas")

# Empilha os valores dos terços da planta.
tb_long <- tb %>%
    gather(key = "terco", value = "ninfas", nin1ter:nin3ter)
str(tb_long)

ggplot(data = tb_long,
       mapping = aes(x = aval,
                     y = ninfas,
                     color = terco)) +
    facet_wrap(facets = ~dexp, nrow = 1) +
    # geom_point() +
    geom_jitter(width = 0.1) +
    stat_summary(geom = "line", fun.y = "mean") +
    labs(x = "Dias após exposição",
         y = "Número de ninfas",
         color = "Posição")

ggplot(data = tb_long,
       mapping = aes(x = aval,
                     y = ninfas)) +
    facet_grid(facets = terco ~ dexp) +
    geom_point() +
    stat_summary(geom = "line", fun.y = "mean") +
    labs(x = "Dias após exposição",
         y = "Número de ninfas",
         color = "Posição")

#-----------------------------------------------------------------------
# Mais exemplos.

url <- "http://leg.ufpr.br/~walmes/data/abacaxicra.txt"
url <- "http://leg.ufpr.br/~walmes/data/osmo.txt"
url <- "http://leg.ufpr.br/~walmes/data/soja_nematoide.txt"
url <- "http://leg.ufpr.br/~walmes/data/giberelina_milho_ivg.txt"
url <- "http://leg.ufpr.br/~walmes/data/gado_crescimento.txt"
url <- "http://leg.ufpr.br/~walmes/data/chimarrita.txt"

#-----------------------------------------------------------------------
