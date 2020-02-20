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
