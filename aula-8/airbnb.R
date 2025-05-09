library(ggplot2)
library(dplyr)
library(corrplot)

airbnb <- read.csv('airbnb.csv', sep = ",", header = TRUE)

# Filtrar linhas onde 'overall_satisfaction' seja maior que 0
airbnb <- airbnb %>% 
  filter(overall_satisfaction > 0)

# Regressão linear simples
#Preço x Satisfação Geral 
linearmodel <- lm(price ~ overall_satisfaction, data = airbnb)
summary(linearmodel)

ggplot(data = airbnb, mapping = aes(x = overall_satisfaction, y = price)) +
  geom_jitter()

# Correlação
cor(airbnb$price, airbnb$overall_satisfaction, use = "pairwise.complete.obs")

airbnb.corr <- airbnb %>%
  select(price, overall_satisfaction, reviews, accommodates)

cor(airbnb.corr)
corrplot(cor(airbnb.corr), method = "number", type = "lower", bg = "grey")

# Teste de p-valores
round(cor.mtest(airbnb.corr)$p, 5)

# Regressão linear múltipla
linearmodel <- lm(price ~ overall_satisfaction + reviews + accommodates, data = airbnb)
summary(linearmodel)

# Regressão Linear Multipla Com Interação
linearmodel <- lm(price ~ overall_satisfaction * reviews, data = airbnb)
summary(linearmodel)

# Estatísticas descritivas
airbnb %>%
  summarize(min = min(reviews),
            Q1 = quantile(reviews, .25),
            Q2 = quantile(reviews, .50),
            Q3 = quantile(reviews, .75),
            max = max(reviews),
            mean = mean(reviews))

airbnb.reviews <- airbnb %>%
  mutate(review_group = case_when(reviews <= quantile(reviews, .33) ~ "low",
                                  reviews <= quantile(reviews, .66) ~ "medium",
                                  TRUE ~ "high"),
         review_group = factor(review_group, levels = c("low", "medium", "high")))

# Visualização por grupo
ggplot(data = airbnb.reviews, mapping = aes(x = overall_satisfaction, y = log(price, base = exp(1)))) + 
  facet_wrap(~ review_group) +
  geom_jitter() +
  stat_smooth(method = "lm", se = FALSE)

# Análise de resíduos
linearmodel <- lm(price ~ overall_satisfaction * reviews, data = airbnb)
residuals <- as_tibble(resid(linearmodel))

ggplot(data = residuals, mapping = aes(x = value)) + 
  geom_histogram()

# Resíduos e valores previstos
residuals_predicted <- tibble(residuals = resid(linearmodel), 
                              predicted = predict(linearmodel))

ggplot(data = residuals_predicted, mapping = aes(x = predicted, y = residuals)) + 
  geom_point()






