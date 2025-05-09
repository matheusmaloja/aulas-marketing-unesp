# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(corrplot)

# Carregar o dataset (ajuste o caminho conforme necessário)
airbnb <- read.csv('airbnb2.csv', sep = ";", header = TRUE)

# --------------------------------------------------------------
# Análise descritiva: Número de quartos por cidade e tipo de acomodação
# --------------------------------------------------------------
airbnb_summary <- airbnb %>%
  group_by(property_type, room_type, bed_type) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))

airbnb_summary

# --------------------------------------------------------------
# Teste ANOVA: Existe diferença de preço entre os tipos propriedade?
# --------------------------------------------------------------
anova_property_type <- aov(price ~ property_type, data = airbnb) 
summary(anova_property_type)

# Teste Tukey HSD para diferenças entre grupos (tipos de propriedade)
tukey_property_type <- TukeyHSD(anova_property_type)
print(tukey_property_type)
plot(tukey_property_type)

# --------------------------------------------------------------
# Teste ANOVA: Existe diferença de preço entre os quartos?
# --------------------------------------------------------------
anova_room <- aov(price ~ room_type, data = airbnb) 
summary(anova_room)

# Teste Tukey HSD para diferenças entre grupos (cidades)
tukey_room <- TukeyHSD(anova_room)
print(tukey_room)
plot(tukey_room)

# --------------------------------------------------------------
# Teste ANOVA: Existe diferença de preço entre os tipos de cama?
# --------------------------------------------------------------
anova_bed <- aov(price ~ bed_type, data = airbnb) 
summary(anova_bed)

# Teste Tukey HSD para diferenças entre grupos (cidades)
tukey_bed <- TukeyHSD(anova_bed)
print(tukey_bed)
plot(tukey_bed)

# --------------------------------------------------------------
# Matriz de Correlação: Preço, Satisfação Geral, Avaliações e Capacidade
# --------------------------------------------------------------
airbnb_corr <- airbnb %>%
  select(price, accommodates, bedrooms, beds, number_of_reviews, bathrooms)

cor_matrix <- cor(airbnb_corr, use = "complete.obs")
corrplot(cor_matrix, method = "number", type = "lower", bg = "grey")

# --------------------------------------------------------------
# Regressão linear múltipla: Preço ~ acomodação, quartos, camas, numero de comentários e banheiros
# --------------------------------------------------------------
lm_multiple <- lm(price ~ accommodates + bedrooms + beds + number_of_reviews + bathrooms, data = airbnb)
summary(lm_multiple)

# Análise dos resíduos da regressão múltipla
residuals_multiple <- as_tibble(resid(lm_multiple))

# Histograma dos resíduos
ggplot(data = residuals_multiple, mapping = aes(x = value)) + 
  geom_histogram(bins = 30) +
  labs(title = "Histograma dos Resíduos", x = "Resíduos")

# Gráfico de resíduos vs valores previstos
residuals_predicted <- tibble(residuals = resid(lm_multiple), predicted = predict(lm_multiple))

ggplot(data = residuals_predicted, mapping = aes(x = predicted, y = residuals)) + 
  geom_point() +
  labs(title = "Resíduos vs Valores Previstos", x = "Valores Previstos", y = "Resíduos")

# --------------------------------------------------------------
# Remoção de outliers: Preço com base no IQR (Intervalo Interquartil)
# --------------------------------------------------------------
# Identificar outliers com base no IQR
Q1 <- quantile(airbnb$price, 0.25)
Q3 <- quantile(airbnb$price, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Filtrar dados sem os outliers
airbnb_filtered <- airbnb %>%
  filter(price >= lower_limit & price <= upper_limit)

# --------------------------------------------------------------
# Regressão múltipla após remover outliers
# --------------------------------------------------------------
lm_filtered <- lm(price ~ accommodates + bedrooms + beds + number_of_reviews + bathrooms, data = airbnb_filtered)
summary(lm_filtered)

# Histograma dos resíduos do modelo ajustado
residuals_filtered <- as_tibble(resid(lm_filtered))
ggplot(data = residuals_filtered, mapping = aes(x = value)) + 
  geom_histogram(bins = 30) +
  labs(title = "Histograma dos Resíduos (Sem Outliers)", x = "Resíduos")

# Gráfico de resíduos vs valores previstos (sem outliers)
residuals_predicted_filtered <- tibble(residuals = resid(lm_filtered), predicted = predict(lm_filtered))

ggplot(data = residuals_predicted_filtered, mapping = aes(x = predicted, y = residuals)) + 
  geom_point() +
  labs(title = "Resíduos vs Valores Previstos (Sem Outliers)", x = "Valores Previstos", y = "Resíduos")

# --------------------------------------------------------------
# Transformação logarítmica no preço
# --------------------------------------------------------------
airbnb$log_price <- log(airbnb$price + 1)  # Adicionar 1 para evitar log(0)

# Ajustar modelo com o log do preço
lm_log <- lm(log_price ~ accommodates + bedrooms + beds + number_of_reviews + bathrooms, data = airbnb)
summary(lm_log)

# Histograma dos resíduos do modelo logarítmico
residuals_log <- as_tibble(resid(lm_log))
ggplot(data = residuals_log, mapping = aes(x = value)) + 
  geom_histogram(bins = 30) +
  labs(title = "Histograma dos Resíduos (Log do Preço)", x = "Resíduos")

# Gráfico de resíduos vs valores previstos (log do preço)
residuals_predicted_log <- tibble(residuals = resid(lm_log), predicted = predict(lm_log))

ggplot(data = residuals_predicted_log, mapping = aes(x = predicted, y = residuals)) + 
  geom_point() +
  labs(title = "Resíduos vs Valores Previstos (Log do Preço)", x = "Valores Previstos", y = "Resíduos")

