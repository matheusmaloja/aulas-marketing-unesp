#Pacotes Necessários

#Análise de Cluster
library(NbClust)
library(tidyverse) 
library(readxl)
library(dplyr)
        
#Análise de Cluster

#puxando dados
seg.df <- read_excel("segmentation_office.xlsx","SegmentationData")

#indicando que id do respondente e professional são fatores
seg.df <- seg.df %>% 
  mutate(respondent_id = factor(respondent_id),
         professional = factor(professional, labels = c("non-professional","professional")))

#puxando variáveis de interesse
cluster.data <- seg.df %>%
  dplyr::select(variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy) 

#formando cluster hierarquivo através do método de ward 
hierarchical.clustering <- hclust(dist(cluster.data), method = "ward.D2") 
plot(hierarchical.clustering)

#Quantos clusters?
duda <- NbClust(cluster.data, distance = "euclidean", method = "ward.D2", max.nc = 6)
pseudot2 <- NbClust(cluster.data, distance = "euclidean", method = "ward.D2", max.nc = 6)

duda$All.index
pseudot2$All.index
duda$Best.nc


# há um elemento de aleatoriedade na análise de clusters
# isso significa que você não obterá sempre o mesmo resultado toda vez que fizer uma análise de clusters
# se você quiser sempre obter o mesmo resultado, precisa fixar o gerador de números aleatórios do R com o comando set.seed
set.seed(1)

# o argumento nstart deve ser incluído e configurado como 25, mas sua explicação está fora do escopo deste tutorial
kmeans.clustering <- kmeans(cluster.data, 3, nstart = 25)

#criando fatores com nome de clusters (cluster1, 2 e 3 e atribuindo ao frame de km group)
seg.df <- seg.df %>% 
  mutate(km.group = factor(kmeans.clustering$cluster, labels=c("cl1","cl2","cl3"))) # Fatorize o indicador de cluster do data frame kmeans.clustering e adicione-o ao data frame equipment.

#entendendo os dados por cluster
seg.df %>% 
  group_by(km.group) %>% # group by cluster (km.group)
  summarise(count = n(), 
            variety = mean(variety_of_choice), 
            electronics = mean(electronics), 
            furniture = mean(furniture), 
            service = mean(quality_of_service), 
            prices = mean(low_prices), 
            return = mean(return_policy)) # Then ask for the number of respondents and for the means of the ratings. 

#existe diferença entre a renda dos grupos?
seg.aov.seg <- aov(income ~ km.group , data=seg.df)
anova(seg.aov.seg)

#confirma a diferença de renda 
TukeyHSD(aov(income ~ km.group, data=seg.df), 
         "km.group")           
#groups
plot(hierarchical.clustering)
rect.hclust(hierarchical.clustering , k=3, border="red")

#Análise Discriminante 
#Qual a média/proporçao de renda, idade e profissao por cluster?
seg.df %>% 
group_by(km.group) %>% # Grupo de clientes por cluster.
  summarize(income = mean(income), 
            age = mean(age), 
            professional = mean(as.numeric(professional)-1))

library("MASS") # chamando o pacote MASS

lda.cluster3 <- lda(km.group ~ income + age + professional, data=seg.df, CV=TRUE) # CV = TRUE garante que podemos armazenar a previsão do LDA na etapa seguinte
seg.df <- seg.df %>% 
  mutate(class = factor(lda.cluster3$class, labels = c("lda1","lda2","lda3"))) # Salve a previsão do LDA como um fator. (As previsões são armazenadas em lda.cluster3$class)

ct <- table(seg.df$km.group, seg.df$class) # quantas observações em cada cluster foram corretamente previstas como estando naquele cluster pela LDA?
ct

prop.table(ct)# adiciona as porcentagens na diagonal
sum(diag(prop.table(ct))) # proporcao correta predita

lda.cluster3.formula <- lda(km.group ~ income + age + professional, data=seg.df, CV=FALSE) # CV = FALSE ensures that we view the formula that we can use for prediction
lda.cluster3.formula

# vamos criar um novo data frame para inserir novos clientes
new_data <- tibble(income = c(65, 65, 35, 35), # definir a renda
                   age = c(20, 35, 45, 60), #definir a idade
                   professional = c("professional","non-professional","non-professional","professional"))

# olhando os novos clientes
new_data

new_data <- new_data %>% 
  mutate(prediction = predict(lda.cluster3.formula, new_data)$class) 
# Crie uma nova coluna chamada prediction no quadro de dados new_data e armazene nela a prediction,
# acessada por $class, para new_data com base na fórmula do LDA com base nos dados antigos (use o LDA onde CV = FALSE).

# olhando para a predição
new_data

#Tarefa

#Qual a média da renda dos clusters?

#Média por renda
seg.df %>% 
  group_by(km.group) %>% 
  summarise(mean_income = mean(income, na.rm = TRUE))

#Qual a média da idade dos clusters?
seg.df %>% 
  group_by(km.group) %>% 
  summarise(mean_age = mean(age, na.rm = TRUE))

#O cluster é formado por mais pessoas com ou sem trabalho?
seg.df %>%
  group_by(km.group, professional) %>%
  summarise(count = n()) %>%
  arrange(km.group, desc(count))

#Avaliando os clusters com base nas informações que você possui, como você nomearia os três? E porque?

#A avaliação a respeito dos atributos pode ser explicado Com base nos dados demográficos (idade e renda)? 
seg.df %>% 
  group_by(km.group) %>% # group by cluster (km.group)
  summarise(count = n(), 
            variety = mean(variety_of_choice), 
            electronics = mean(electronics), 
            furniture = mean(furniture), 
            service = mean(quality_of_service), 
            prices = mean(low_prices), 
            return = mean(return_policy)) 

#A avaliação muda por pessoas empregadas x nao empregadas por cluster?
seg.df %>% 
  group_by(km.group, professional) %>% # group by cluster (km.group)
  summarise(count = n(), 
            variety = mean(variety_of_choice), 
            electronics = mean(electronics), 
            furniture = mean(furniture), 
            service = mean(quality_of_service), 
            prices = mean(low_prices), 
            return = mean(return_policy))

#testar se diferenças são significativas 
# Filtrar os dados apenas para o cluster 3 (se quiser testar para outro grupo, basta alterar onde tem 3 pelo cluster que quiser)
cluster3_data <- seg.df %>%
  filter(km.group == "cl3")
t_test_result <- t.test(low_prices ~ professional, data = cluster3_data)

# Ver o resultado do teste t
t_test_result

#Você confiaria apenas nesses dados para clusterizar um grupo de clientes? justifique sua resposta. 

