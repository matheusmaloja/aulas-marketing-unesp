#Chamando os dados 
df <- read.csv("final4.csv", sep=";", header = TRUE)

#Bibliotecas a serem utilizadas
library(NbClust)
library(tidyverse) 
library(readxl)
library(ggplot2)
library(marketr)
library(dplyr)
library(lexiconPT)
library(tm)
library(tidytext)
library(DT)
ls("package:lexiconPT")


#Indicando que id do respondente e professional são fatores
df <- df %>% 
  mutate(id_cliente = factor(id_cliente))
         
#Dados Exploratórios
#Qual a média da renda dos clusters?

#Média por renda
df %>% 
  group_by(km.group) %>% 
  summarise(mean_income = mean(income, na.rm = TRUE))

#Qual a média da idade dos clusters?
df %>% 
group_by(km.group) %>% 
summarise(mean_age = mean(age, na.rm = TRUE))

#Qual a média de filhos por clusters?
df %>% 
  group_by(km.group) %>% 
  summarise(mean_kids = mean(kids, na.rm = TRUE))

#Genero por cluster
hist.df1 <- df %>%
  group_by(km.group, gender) %>%
  summarise(count = n()) %>%
  arrange(km.group, desc(count))
hist.df1

#Inscrição no programa de fidelildade
hist.df <- df %>%
  group_by(km.group, subscribe) %>%
  summarise(count = n()) %>%
  arrange(km.group, desc(count))
hist.df

#histograma
ggplot(hist.df, aes(x = as.factor(km.group), y = count, fill = subscribe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cluster", y = "Número de pessoas", fill = "Inscrição") +
  ggtitle("Distribuição de inscritos e não inscritos por cluster") +
  theme_minimal()

#calcular NPS para cada cluster
#Calcular NPS
nps <- nps_trend(df, trend_var = km.group, min_surveys = 1)
nps

#Visualizar graficamente 
ggplot(nps, aes(x = km.group, y = nps, fill = km.group)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(nps, 2)), vjust = -0.3) +  # Exibe os valores de NPS nas barras
  labs(x = "Cluster", y = "NPS", title = "NPS por Cluster") +
  theme_minimal()

#CSAT GERAL POR CLUSTER

# Calcular o CSAT por cluster
df <- df %>%
  mutate(satisfied_customers = if_else(ambiente >= 4 & atendimento >= 4 & qualidade >= 4, 1, 0))

# Calculando o CSAT por cluster
csat_summary_cluster <- df %>%
  group_by(km.group) %>%
  summarise(
    total_responses = n(),
    satisfied_count = sum(satisfied_customers, na.rm = TRUE),
    csat_score = (satisfied_count / total_responses) * 100  # Percentual de satisfeitos
  ) %>%
  ungroup()
csat_summary_cluster


#plotar em um gráfico
ggplot(csat_summary_cluster, aes(x = km.group, y = csat_score, fill = km.group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(csat_score, 2)), vjust = -0.5) +
  labs(x = "Cluster", y = "CSAT (%)", title = "CSAT por Cluster") +
  theme_minimal()

#Satisfação individual por cluster 

csat_individual_cluster <- df %>%
  group_by(km.group) %>%
  summarise(
    ambiente_csat = (sum(ambiente >= 4) / n()) * 10,    # Ajuste na escala após o cálculo
    qualidade_csat = (sum(qualidade >= 4) / n()) * 10,  # Ajuste na escala após o cálculo
    atendimento_csat = (sum(atendimento >= 4) / n()) * 10  # Ajuste na escala após o cálculo
  ) %>%
  ungroup()

csat_individual_cluster

# Plotar gráficos separados para cada métrica

# Ambiente CSAT por Cluster
ggplot(csat_individual_cluster, aes(x = km.group, y = ambiente_csat, fill = km.group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(ambiente_csat, 2)), vjust = -0.5) +
  labs(x = "Cluster", y = "CSAT Ambiente", title = "CSAT de Ambiente por Cluster") +
  theme_minimal()

# Qualidade CSAT por Cluster
ggplot(csat_individual_cluster, aes(x = km.group, y = qualidade_csat, fill = km.group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(qualidade_csat, 2)), vjust = -0.5) +
  labs(x = "Cluster", y = "CSAT Qualidade", title = "CSAT de Qualidade por Cluster") +
  theme_minimal()

# Atendimento CSAT por Cluster
ggplot(csat_individual_cluster, aes(x = km.group, y = atendimento_csat, fill = km.group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(atendimento_csat, 2)), vjust = -0.5) +
  labs(x = "Cluster", y = "CSAT Atendimento", title = "CSAT de Atendimento por Cluster") +
  theme_minimal()

# Analise qualitativa - pode rodar tudo até lá embaixo 
# Pré-processamento dos comentários
df$comentario <- df$comentario %>%
  str_to_lower() %>%
  str_replace_all(" *-+ *", "") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  removeNumbers() %>%
  trimws()

# Filtra comentários vazios
df <- df %>%
  filter(nchar(comentario) > 0)

# Tokenização
texto_un <- df %>%
  unnest_tokens(output = "words", input = comentario)

# Determinação de polaridade
tb_sen <- inner_join(texto_un, oplexicon_v3.0[, c("term", "polarity")],
                     by = c("words" = "term"), relationship = "many-to-many")

# Agregação de polaridade
tb <- tb_sen %>%
  group_by(km.group) %>%
  summarise(sentiment = sum(polarity) / n())

# Tabela final com resultados
tb_u <- tb %>%
  inner_join(df[, c("km.group", "comentario")]) %>%
  select(km.group, sentiment, comentario)

# Definir estilo para a tabela HTML
vals <- seq(-1, 1, by = 0.1)
cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 6, name = "Spectral"))(length(vals))
style <- styleInterval(cuts = head(vals[-1], n = -1), values = cols[-1])

# Tabela HTML formatada
html_table <- datatable(tb_u, colnames = c("Avaliação", "Sentimento", "Opinião Geral")) %>%
  formatRound(columns = "sentiment", digits = 2) %>%
  formatStyle(columns = names(tb_u), valueColumns = "sentiment", target = "cell",
              backgroundColor = style)

html_table




