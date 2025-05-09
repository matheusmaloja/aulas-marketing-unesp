# CONFIGURANDO SEU DIRETÓRIO
# Criar pasta na área de trabalho chamada marketing e inserir o documento dentro
# Session > Set Working Directory > Choose Directory > Selecionar pasta marketing

# ATRIBUINDO DADOS A OBJETOS
airbnb <- read.csv("aula1.csv", sep = ",", header = TRUE)
View(airbnb)

#VERIFICAR ESTRUTURA DOS DADOS 
str(airbnb)

# TRANSFORMANDO EM FATORES
airbnb$host_id_c <- as.character(airbnb$host_id)

# TRANSFORMAÇÕES NUMÉRICAS
head(airbnb$overall_satisfaction)
airbnb$overall_satisfaction_100 <- airbnb$overall_satisfaction * 20
head(airbnb$overall_satisfaction_100)

# TRANSFORMAÇÕES COM MUTATE
airbnb <- mutate(airbnb, 
                 host_id_c = as.character(host_id),
                 overall_satisfaction_100 = overall_satisfaction * 20)

#INSTALANDO DPLYR
install.packages("dplyr")
library(dplyr)

# INCLUIR OU EXCLUIR E RENOMEAR VARIÁVEIS (COLUNAS)
# Excluir
airbnb <- select(airbnb, -country, -survey_id)

# Renomear
airbnb <- rename(airbnb, country = city, city = borough)

View(airbnb)

# INCLUIR E EXCLUIR OBSERVAÇÕES (LINHAS)
# Vetor de palavras
airbnb_topten <- c("Brussel","Antwerpen","Gent","Charleroi","Liege","Brugge","Namur","Leuven","Mons","Aalst")
airbnb_topten

# Vetor de números
number_vector <- c(0,2,4,6)
number_vector

# INCLUIR OU EXCLUIR OBSERVAÇÕES COM A FUNÇÃO FILTER
# Instalar pacote Hmisc
install.packages("Hmisc")
library(Hmisc)

# FILTER
airbnb.topten <- filter(airbnb, city %in% airbnb_topten) 
View(airbnb.topten)

# OPERADOR PIPE
airbnb <- read.csv("aula1.csv") %>% 
  mutate (host_id_c = as.character(host_id), overall_satisfaction_100 = overall_satisfaction * 20) %>% 
  select(-country, -survey_id) %>% 
  rename(country = city, city = borough) %>% 
  filter(city %in% c("Brussel","Antwerpen","Gent","Charleroi","Liege","Brugge","Namur","Leuven","Mons","Aalst")) 

# AGRUPAR E SUMARIZAR
# Agrupar e Sumarizar por Cidade
airbnb %>% 
  group_by(city) %>% 
  summarise(nr_per_city = n())

# COLOCAR EM ORDEM (CRESCENTE)
airbnb %>% 
  group_by(city) %>%
  summarise(nr_per_city = n()) %>%
  arrange(nr_per_city) 

# COLOCAR EM ORDEM (DESCRESCENTE)
airbnb %>% 
  group_by(city) %>%
  summarise(nr_per_city = n()) %>%
  arrange(desc(nr_per_city)) 

# ESTÁTISTICAS DESCRITIVAS
# Verificar as estatísticas descritivas de preço geral
summary(airbnb$price)

# Preço médio por cidade
airbnb.summary <- airbnb %>% 
  group_by(city) %>%
  summarise(nr_per_city = n(), average_price = mean(price)) %>% 
  arrange(desc(average_price)) 
print(airbnb.summary, n = Inf) 

# Mediana e preço Máximo
airbnb %>%
  group_by(city) %>%
  summarise(nr_per_city = n(), 
            average_price = mean(price),
            median_price = median(price), 
            max_price = max(price)) %>% 
  arrange(desc(median_price),
          desc(max_price)) 

# GRÁFICOS
# Instalar pacote Hmisc e ggplot
install.packages("Hmisc")
install.packages("ggplot2")
library(ggplot2)
library(Hmisc)

# Selecionando as 10 cidades Principais
airbnb_topten <- airbnb %>% 
  filter(city %in% c("Brussel","Antwerpen","Gent","Charleroi","Liege","Brugge","Namur","Leuven","Mons","Aalst")) 

# SCATTERPLOT
ggplot(data = airbnb_topten, mapping = aes(x = city, y = price)) + 
  geom_point()

# JITTER
ggplot(data = airbnb_topten, mapping = aes(x = city, y = price)) + 
  geom_jitter()

# HISTOGRAMA
ggplot(data = airbnb_topten, mapping = aes(x = price)) + 
  geom_histogram() 

# PLOTAR A MÉDIA
ggplot(data = airbnb_topten, mapping = aes(x = city, y = price)) + 
  geom_jitter() +
  stat_summary(fun.y=median, colour="tomato3", size = 4, geom="point")

# EXPORTAR
# Para exportar em csv
library(readr)
write_excel_csv(airbnb, "airbnb.csv")
