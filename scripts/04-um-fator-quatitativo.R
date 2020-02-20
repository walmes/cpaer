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
    geom_point()

#-----------------------------------------------------------------------

tb <- FariaQd14.3
str(tb)

ggplot(data = tb,
       mapping = aes(x = P, y = mspa, color = bloc)) +
    geom_point()

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
