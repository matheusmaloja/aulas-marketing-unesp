# Carregar o dataset
df_anuncios <- read.csv("anuncio.csv", sep = ";", header = TRUE)

# Verificar os primeiros dados
head(df_anuncios)

# Modelo de Regressao
modelo <- lm(PrecoConversao ~ GastoAnuncio + Impressoes + Cliques + Conversoes + Alcance, data=df_anuncios)

# Resumo do modelo
summary(modelo)



