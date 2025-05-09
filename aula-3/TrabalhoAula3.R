install.packages("marketr")
install.packages("dplyr")
install.packages("ggplot2")
library(marketr)
library(dplyr)
library(ggplot2)

#NPS
dados <- read.csv("sorveteriaregiao.csv", sep = ";", header = TRUE)
dados$date <- as.Date(dados$date, format = "%d/%m/%Y") #Forçar leitura em data
str(dados)
head(dados)

#Calcular NPS
nps <- nps_calc(dados)
nps

#Calcular NPS por cidade e por tempo
nps3 <- nps_trend(dados, trend_var = date,  city, min_surveys = 1)
nps3

#visualizar graficamente
ggplot(nps3, aes(x = date, y = nps, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do NPS por Cidade ao Longo de 2023")

# Filtrar os dados para incluir apenas três cidades (exemplo: Belo Horizonte, Curitiba, São Paulo)
nps3_filt <- nps3 %>% filter(city %in% c("Bauru"))

# Criar o gráfico com as cidades filtradas
ggplot(nps3_filt, aes(x = date, y = nps, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do NPS por Cidade ao Longo de 2023")


#Calcular CSAT
csat_scores <- dados %>%
  summarise(
    csat_atendimento = mean(ifelse(csat_atendimento >= 4, 1, 0), na.rm = TRUE) * 100,
    csat_ambiente = mean(ifelse(csat_ambiente >= 4, 1, 0), na.rm = TRUE) * 100,
    csat_qualidade = mean(ifelse(csat_qualidade >= 4, 1, 0), na.rm = TRUE) * 100
  )

print(csat_scores)

# Calcular CSAT para Atendimento
csat_atendimento_summary <- dados %>%
  mutate(month = format(date, "%Y-%m")) %>%  # Extraindo o mês e ano da data
  mutate(satisfied = ifelse(csat_atendimento >= 4, 1, 0)) %>%  # Marcar respostas positivas
  group_by(city, month) %>%  # Agrupar por cidade e mês
  summarise(
    total_responses = n(),
    positive_responses = sum(satisfied),
    csat_atendimento = (positive_responses / total_responses) * 100
  ) %>%
  ungroup()

# Visualizar o resultado
print(csat_atendimento_summary)

# Calcular CSAT para Ambiente
csat_ambiente_summary <- dados %>%
  mutate(month = format(date, "%Y-%m")) %>%  # Extraindo o mês e ano da data
  mutate(satisfied = ifelse(csat_ambiente >= 4, 1, 0)) %>%  # Marcar respostas positivas
  group_by(city, month) %>%  # Agrupar por cidade e mês
  summarise(
    total_responses = n(),
    positive_responses = sum(satisfied),
    csat_ambiente = (positive_responses / total_responses) * 100
  ) %>%
  ungroup()

# Visualizar o resultado
print(csat_ambiente_summary)

# Calcular CSAT para Qualidade
csat_qualidade_summary <- dados %>%
  mutate(month = format(date, "%Y-%m")) %>%  # Extraindo o mês e ano da data
  mutate(satisfied = ifelse(csat_qualidade >= 4, 1, 0)) %>%  # Marcar respostas positivas
  group_by(city, month) %>%  # Agrupar por cidade e mês
  summarise(
    total_responses = n(),
    positive_responses = sum(satisfied),
    csat_qualidade = (positive_responses / total_responses) * 100
  ) %>%
  ungroup()

# Visualizar o resultado
print(csat_qualidade_summary)

#Grafico CSAT Atendimento 
ggplot(csat_atendimento_summary, aes(x = as.Date(paste0(month, "-01")), y = csat_atendimento, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do CSAT Atendimento por Cidade ao Longo de 2023",
       x = "Mês",
       y = "CSAT (%)") +
  theme_minimal()

#Criar grafico filtrado por cidade
# Filtrar os dados por cidade
csat_filt <- csat_atendimento_summary %>% filter(city %in% c("Bauru"))

# Criar o gráfico com as cidades filtradas
ggplot(csat_filt, aes(x = as.Date(paste0(month, "-01")), y = csat_atendimento, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do CSAT Atendimento por Cidade ao Longo de 2023",
       x = "Mês",
       y = "CSAT (%)") +
  theme_minimal()

#Grafico CSAT Qualidade
ggplot(csat_qualidade_summary, aes(x = as.Date(paste0(month, "-01")), y = csat_qualidade, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do CSAT Qualidade por Cidade ao Longo de 2023",
       x = "Mês",
       y = "CSAT (%)") +
  theme_minimal()

#Criar grafico filtrado por cidade
# Filtrar os dados por cidade
csat_filt <- csat_qualidade_summary %>% filter(city %in% c("Bauru"))

# Criar o gráfico com as cidades filtradas
ggplot(csat_filt, aes(x = as.Date(paste0(month, "-01")), y = csat_qualidade, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do CSAT Qualidade por Cidade ao Longo de 2023",
       x = "Mês",
       y = "CSAT (%)") +
  theme_minimal()

#Grafico CSAT Ambiente
ggplot(csat_ambiente_summary, aes(x = as.Date(paste0(month, "-01")), y = csat_ambiente, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do CSAT Ambiente por Cidade ao Longo de 2023",
       x = "Mês",
       y = "CSAT (%)") +
  theme_minimal()

#Criar grafico filtrado por cidade
# Filtrar os dados por cidade
csat_filt <- csat_ambiente_summary %>% filter(city %in% c("Bauru"))

# Criar o gráfico com as cidades filtradas
ggplot(csat_filt, aes(x = as.Date(paste0(month, "-01")), y = csat_ambiente, color = city)) +
  geom_line() +
  facet_wrap(~ city) +
  labs(title = "Tendência do CSAT Qualidade por Cidade ao Longo de 2023",
       x = "Mês",
       y = "CSAT (%)") +
  theme_minimal()









