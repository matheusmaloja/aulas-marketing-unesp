####EXemplo 1

#Notas para para 4 marcas de equipamentos de escritório, dividido em 6 variaveis
#large_choice - ampla escolha
#low_prices - precos baixos
#service_quality - Qualidade de serviço
#product_quality - qualidade do produto
#conveniencie - conveniência
#preference_score - score de preferencia

library(tidyverse)
library(readxl)

office <- read_excel("escritorio.xlsx","attributes") 

office <- office %>% 
  mutate(brand = factor(brand)) #Transformar a marca em fator 

#chamar pacote
library(FactoMineR)

office.df <- office %>% 
  select(- brand) %>% # A entrada para a análise de componentes principais deve conter apenas as dimensões, não o(s) identificador(es), então vamos remover os identificadores.
  as.data.frame() # em seguida, mudar o tipo do objeto para 'data.frame'. Isso é necessário para a função PCA.

rownames(office.df) <- office$brand # Definir os nomes das linhas do data.frame como as marcas (isso é importante mais tarde ao fazer um biplot).
office.pca <- PCA(office.df, graph=FALSE) # Realizar a análise de componentes principais.

office.pca$eig # e visualizar a tabela com informações sobre a variância explicada.

office.pca.two <- PCA(office.df, ncp = 2, graph=FALSE) # Solicite dois fatores preenchendo o argumento ncp.
office.pca.two$var$cor 

loadings <- as_tibble(office.pca.two$var$cor) %>% # Precisamos capturar os loadings como um data frame em um novo objeto. Use as_tibble(), caso contrário, não conseguimos acessar os diferentes fatores.
  mutate(variable = rownames(office.pca.two$var$cor), # Mantenha o controle dos nomes das linhas (estes são removidos ao converter para tibble).
         communality = Dim.1^2 + Dim.2^2, # O operador ^ eleva um valor a uma certa potência. Para calcular a comunalidade, precisamos somar os quadrados dos loadings em cada fator.
         uniqueness = 1 - communality) # Calcula a unicidade como 1 - comunalidade.
loadings

install.packages("factoextra") # Instale o pacote 'factoextra' para visualizações de PCA
library(factoextra)

fviz_pca_biplot(office.pca.two, repel = TRUE) # Plote os loadings e as marcas juntos em um gráfico.

#Exemplo 2

# Carregar e visualizar dados
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
head(brand.ratings) #Verificar os dados

#perform - desempenho
#leader - líder
#latest - mais recente
#fun - diversão
#serious - séria
#bargain - custo beneficio
#value - valor
#trendy - tendencia
#rebuy - recompra
#brand - marca (a - j)
#notas de 1 a 10 

# Resumo e estrutura dos dados
summary(brand.ratings)
str(brand.ratings)

# Reescalonamento dos dados
x <- 1:1000
x.sc <- (x - mean(x)) / sd(x)
summary(x.sc)

# Reescalonar todas as variáveis de uma vez usando scale()
brand.sc <- brand.ratings
brand.sc[, 1:9] <- data.frame(scale(brand.ratings[, 1:9]))
summary(brand.sc)


# Visualizar correlação com corrplot
library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order="hclust")

# Agregar média das avaliações por marca
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
brand.mean

# Nomear as linhas com as marcas e remover coluna redundante
rownames(brand.mean) <- brand.mean[, 1]
brand.mean <- brand.mean[, -1]

# Heatmap das médias dos adjetivos por marca
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(brand.mean),
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          main="\n\n\n\n\nAtributos da Marca")

# Análise de Componentes Principais (PCA)
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)

# Gráfico Scree Plot da PCA
plot(brand.pc, type="l")

# Biplot dos dois primeiros componentes principais
biplot(brand.pc)

# PCA usando médias agregadas por marca
brand.mu.pc <- prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)

# Biplot para o posicionamento da marca com a média dos adjetivos
biplot(brand.mu.pc, main="Posicionamento da Marca", cex=c(1, 1))

# Comparar diferenças entre marcas específicas (exemplo entre 'c' e 'e')
brand.mean["c", ] - brand.mean["e", ]

# Encontrar a média de quatro marcas específicas e comparar com uma marca
colMeans(brand.mean[c("b", "c", "f", "g"), ]) - brand.mean["e", ]

