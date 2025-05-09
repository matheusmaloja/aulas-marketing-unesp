install.packages("readxl")
install.packages("tidyverse")
install.packages("radiant")
install.packages("admisc")

library(admisc)
library(radiant)
library(readxl)
library(tidyverse)

icecream <- read_xlsx("icecream2.xlsx")

# atributo1, atributo2, etc. são vetores com um elemento em que 
#primeiro fornecemos o nome do atributo seguido por um ponto e vírgula, 
#e depois fornecemos todos os níveis dos atributos separados por ponto e vírgula
attribute1 <- "Flavor; Raspberry; Chocolate; Strawberry; Mango; Vanilla"
attribute2 <- "Package; Homemade waffle; Cone; Pint"
attribute3 <- "Light; Low fat; No low fat"
attribute4 <- "Organic; Organic; Not organic"

# agora combine esses diferentes atributos em um vetor com c()
attributes <- c(attribute1, attribute2, attribute3, attribute4)

# Resumo do planejamento experimental (DOE)
summary(doe(attributes, seed = 123))

# Resumo do DOE com 30 tentativas
summary(doe(attributes, seed = 123, trials = 30))

# um respondente
respondent1 <- icecream %>% filter(respondent == "Individual 1")

# salve a análise de conjoint em um objeto, pois vamos usá-lo como entrada para summary(), plot() e predict() mais tarde
conjoint_respondent1 <- conjoint(respondent1, rvar = "rating", 
                                 evar = c("Flavor","Packaging","Light","Organic")) 

# Resumo da análise de conjoint
summary(conjoint_respondent1)
plot(conjoint_respondent1)

# Execute esta regressão se estiver interessado em saber qual preditor é significativo ou qual é o R-quadrado do modelo geral
summary(lm(rating ~ Flavor + Packaging + Light + Organic, data = respondent1))

# perfis - utilidades previstas
profiles <- icecream %>% 
  filter(respondent == "Individual 1") %>% 
  select(Flavor,Packaging,Light,Organic)

profiles

# prever as classificações para os perfis com base na análise de conjoint
predict(conjoint_respondent1, profiles)

Flavor <- c("Raspberry","Chocolate","Mango","Strawberry","Vanilla")
Organic <- c("Organic","Not organic")

expand.grid(Flavor, Organic)

# há uma maneira mais fácil de obter os níveis dos atributos do que criar os vetores manualmente:
icecream <- icecream %>% 
  mutate(Flavor = factor(Flavor), 
         Packaging = factor(Packaging),
         Light = factor(Light),
         Organic = factor(Organic))

levels(icecream$Flavor) # certifique-se de que Flavor está como fator!

# agora crie todos os perfis
profiles.all <- expand.grid(levels(icecream$Flavor),levels(icecream$Packaging),levels(icecream$Light),levels(icecream$Organic)) %>% 
  rename("Flavor" = "Var1", "Packaging" = "Var2", "Light" = "Var3", "Organic" = "Var4") # renomeie as variáveis criadas por expand.grid (não se esqueça disso, caso contrário o predict não saberá onde procurar cada atributo)

# prever as classificações de todos os perfis
predict(conjoint_respondent1, profiles.all) %>% 
  arrange(desc(Prediction)) # mostrar os sorvetes com a maior classificação prevista no topo

# Muitos respondentes
conjoint_allrespondents <- conjoint(icecream, rvar = "rating", 
                                    evar = c("Flavor","Packaging","Light","Organic")) # igual ao anterior, mas com um conjunto de dados diferente.

summary(conjoint_allrespondents) 
plot(conjoint_allrespondents)
predict(conjoint_allrespondents, profiles.all) %>%
  arrange(desc(Prediction))

# use slice() para selecionar linhas
market_profiles <- profiles.all %>% 
  slice(c(4, 16, 23, 38)) # dos perfis, selecione as linhas 4, 16, 23 e 38 como os quatro perfis

market_profiles

# Modelo de conjoint para todos os respondentes
conjoint_allrespondents <- conjoint(icecream, rvar = "rating",
                                    evar = c("Flavor","Packaging","Light","Organic"))

# Prever com base nos perfis de mercado
predict(conjoint_allrespondents, market_profiles) %>%
  arrange(desc(Prediction))

# mesmo modelo que antes, mas agora adicionando by = "respondent"
conjoint_perrespondent <- conjoint(icecream, rvar = "rating", evar = c("Flavor","Packaging","Light","Organic"), by = "respondent")

# Prever e ordenar por respondente
predict(conjoint_perrespondent, market_profiles) %>% 
  arrange(respondent, desc(Prediction)) # ordenar por respondente e depois por classificação prevista

# Perfis mais bem avaliados
highest_rated <- predict(conjoint_perrespondent, market_profiles) %>% 
  group_by(respondent) %>% 
  mutate(ranking = rank(Prediction))

# veja o resultado
highest_rated %>% 
  arrange(respondent, ranking)

# precisamos reter apenas o sorvete mais bem classificado
highest_rated <- highest_rated %>% 
  arrange(respondent, ranking) %>% 
  filter(ranking == 4)

highest_rated

#calculando a participação de mercado 
market_share <- highest_rated %>% 
  group_by(Flavor, Packaging, Light, Organic) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

market_share

