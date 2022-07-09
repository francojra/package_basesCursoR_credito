# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 09/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(basesCursoR)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
library(gridExtra)

# Identificar bases disponíveis ------------------------------------------------------------------------------------------------------------

basesCursoR::bases_disponiveis()

# Carregar base de dados -------------------------------------------------------------------------------------------------------------------

cred <- basesCursoR::pegar_base("credito")
View(cred)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

cred1 <- cred %>%
  select(estado_civil, trabalho, despesas, renda) 
View(cred1)  
glimpse(cred1)

cred1$trabalho <- as.factor(cred1$trabalho)
cred1$estado_civil <- as.factor(cred1$estado_civil)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

cred2 <- cred1 %>%
  group_by(estado_civil) %>%
  summarise(med = mean(despesas),
            sd = sd(despesas),n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(cred2)

p1 <- ggplot(cred2, aes(x = fct_reorder(estado_civil, med), y = med)) +
  geom_col(fill = "#7fc97f", color = "black") +
  geom_errorbar(aes(x = estado_civil, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Estado Civil", y = "Despesas")
p1
