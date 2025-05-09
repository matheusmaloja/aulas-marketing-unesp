#MODELAGEM DE TÓPICOS 

install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("ggtern")
install.packages("LDAvis")
install.packages("slam")

library(tidyverse)   # Recursos de manipulação e visualização.
library(tidytext)    # Manipulação de texto a la tidyverse.
library(tm)          # Mineração de texto.
library(topicmodels) # Modelagem de tópicos.
library(wordcloud)   # Núvem de palavras.
library(ggtern)      # Gráfico ternário.
library(LDAvis)      # Visualização de modelos LDA.
library(slam)

#Puxando Dados 
dados <- read.csv("bar.csv", sep = ";", header = TRUE) |> as_tibble()

#Adicionando uma coluna com ID aleatória para cada linha 
dados <- dados %>%
  mutate(id = paste0("ID", 1:n()))

View(dados)

#Criação do corpus 
cps <- VCorpus(VectorSource(dados$Comentario),
               readerControl = list(language = "portuguese"))

#Visualização dos Documentos Após Pré-processamento
sapply(cps[1:3], content) %>%
  map(str_wrap, width = 60) %>%
  walk(cat, "\n\n")

#Funções de pré-processamento
#Trocando toda pontuação do texto por espaços
replacePunctuation <-
  content_transformer(FUN = function(x) {
    return(gsub(pattern = "[[:punct:]]+",
                replacement = " ",
                x = x))
  })

#Stop Words Personalizadas 
my_sw <- c(
  "mercado", "supermercado", "bom", "produto", "cliente", "loja", "ano", "local", 
  "rede", "dia", "fazer", "ser", "muito", "pouco", "melhor", "outro", "tudo", 
  "pra", "bem", "vez", "super", "mais"
)

#Limpeza dos dados
# Limpeza dos dados
cps2 <- cps %>%
tm_map(FUN = content_transformer(tolower)) %>% # Converte todo o texto para letras minúsculas
tm_map(FUN = replacePunctuation)  %>%          # Substitui pontuação por espaços
tm_map(FUN = removeWords, words = stopwords("portuguese"))  %>% # Remove stopwords padrão em português
tm_map(FUN = removeWords, words = my_sw)  %>%  # Remove stopwords personalizadas
tm_map(FUN = stemDocument, language = "portuguese")  %>% # Aplica stemming para reduzir palavras às suas raízes
tm_map(FUN = removeNumbers)           %>%      # Remove números do texto
tm_map(FUN = stripWhitespace)       %>%        # Remove espaços em branco extras
tm_map(FUN = content_transformer(trimws))  # Remove espaços em branco no início e fim


#Criação da Matriz Documento-Termos
dtm <- DocumentTermMatrix(cps2)
dtm

#reducao da sparsidade
rst <- removeSparseTerms(dtm, sparse = 0.975)
rst

# Verifique a soma das entradas de cada linha
row_sums_rst <- row_sums(rst)

# Remova as linhas que não contêm termos
rst <- rst[row_sums_rst > 0, ]

#Modelo LDA
fit <- LDA(rst, k = 3, control = list(seed = 1234))

#Analise de Tópicos
table(topics(fit))
terms(fit)
fit_coefs <- posterior(fit)
str(fit_coefs)

#Distribuição dos topicos
topic_coef <- tidy(fit, matrix = "gamma")
aux <- sample_n(topic_coef, size = 150) %>%
  arrange(topic, gamma) %>%
  mutate(document = fct_reorder(document, row_number()))

ggplot(data = aux) +
  aes(x = document, y = gamma, fill = factor(topic)) +
  geom_col(position = "fill") +
  labs(fill = "Tópico predominante") +
  coord_flip()

#Distribuição dos termos dos tópicos
terms_coef <- tidy(fit, matrix = "beta")
topn_terms <- terms_coef %>%
  group_by(topic) %>%
  top_n(n = 50, wt = beta) %>%
  ungroup()

#Grafico por termo de tópico
pp <- topn_terms %>%
  group_by(topic) %>%
  do(plot = {
    ggplot(.) +
      aes(x = reorder(term, beta), y = beta) +
      geom_col() +
      labs(x = "Termos", y = "Frequência") +
      coord_flip()
  })
length(pp$plot)

do.call(what = gridExtra::grid.arrange, args = c(pp$plot, nrow = 1))

#Nuvem de Palavra por Tópicos
topn_terms <- terms_coef %>%
  group_by(topic) %>%
  top_n(300, beta) %>%
  ungroup()

i <- 0
pal <- c("Reds", "Blues", "Greens", "Purples")[1:fit@k]

oldpar <- par()
par(mfrow = c(2, 2), mar = c(0, 0, 0, 0))
topn_terms %>%
  group_by(topic) %>%
  do(plot = {
    i <<- i + 1
    wordcloud(words = .$term,
              freq = .$beta,
              min.freq = 1,
              max.words = 300,
              random.order = FALSE,
              colors = tail(brewer.pal(9, pal[i]), n = 5))
  })

####

#MODELAGEM DE TÓPICOS (mais simples)
install.packages("stm")
install.packages("dplyr")
install.packages("knitr")
install.packages("tm")
install.packages("SnowballC")
library(stm)
library(dplyr)
library(knitr)
library(tm)
library(SnowballC)

#Limpar Texto

limpar_texto <-\(x){
  x <- gsub("\\b[IVXLCDM]+\\b", "", x)
  return(x)}

analise <- limpar_texto(dados$Comentario)

#Pré-processamento

processed <- textProcessor(analise, 
                           metadata = dados,
                           ucp = TRUE,
                           onlycharacter = TRUE,
                           wordLengths = c(3, Inf),
                           language = "portuguese")

#associando texto com metadados
str(processed)

#Visualização de termos removidos 
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

#Preparação dos termos para modelagem 
out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta,
                     lower.thresh = 1)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

fit <- stm(
  documents = docs, vocab = vocab, data = meta,  K = 10,
  max.em.its = 75, init.type = "Spectral",seed = 05959 ,verbose = FALSE)

#Rotulação dos topicos
labelTopics(fit)

#Visualização dos Topicos
plot(fit, type = "summary", xlim = c(0, 0.99))

plot(fit, type = "perspectives", topics = c(3, 10))
mod.out.corr <- topicCorr(fit)
plot(mod.out.corr)

#Co Ocorrencia de Palavras
install.packages("tm")       
install.packages("tidytext") 
install.packages("dplyr")    
install.packages("igraph")   
install.packages("tidyr")   
install.packages("ggraph")  

library(tm)       # Texto Mining - Para processamento de texto e análise de texto.
library(tidytext) # Análise de texto em formato tidy - Para manipulação e visualização de dados textuais.
library(dplyr)    # Manipulação de dados - Para transformar e resumir dados.
library(igraph)   # Análise de redes - Para criar, manipular e visualizar gráficos e redes.
library(tidyr)    # Manipulação de dados - Para arrumar e transformar dados em formato tidy.
library(ggraph)   # Visualização de redes - Para criar gráficos de redes e visualizações baseadas em grafos.


#Criando um novo corpus para a análise
document <- Corpus(VectorSource(dados$Comentario))

#LimparTexto

# Aplicar as transformações no texto
document <- tm_map(document, content_transformer(tolower))       # Converte para minúsculas
document <- tm_map(document, removeNumbers)                      # Remove números
document <- tm_map(document, removeWords, stopwords("portuguese")) # Remove palavras de parada em português
document <- tm_map(document, removePunctuation)                   # Remove pontuações

#Cria dataframe a partir dos dados processados
dadosN <- data.frame(text = sapply(document, as.character), stringAsFactors = FALSE)

#criando bigramas
New_bigramas <- dadosN%>%
  unnest_tokens(New_bigramas, text, token = "ngrams", n = 2)
New_bigramas

#separar bi gramas
bigramassep <- New_bigramas %>%
  separate(New_bigramas, c("word1", "word2"), sep = " ")

#Limpar stopwords dos bigramas 
bigramasfiltr <- bigramassep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#contagem de bigramas
bigramascont <- bigramasfiltr %>%
  count(word1, word2, sort = TRUE)
bigramascont

# Remover linhas com NA
bigramascont <- bigramascont %>%
  drop_na()

View(bigramascont)

# Criar o grafo a partir do dataframe limpo
grafico <- bigramascont %>%
  filter(n > 10) %>%
  graph_from_data_frame()

#Criar grafico baseado nas co-ocorrencias
set.seed(2017)
ggraph(grafico, layout = "nicely")+
  geom_edge_link() +
  geom_node_point()+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Analise de Sentimentos

install.packages("tm")        
install.packages("tidytext") 
install.packages("tidyverse") 
install.packages("DT")        
install.packages("wordcloud") 
install.packages("dplyr")     
install.packages("stringr")   
install.packages("tibble")    
install.packages("tidyr")     
library(tm)        # #tm: Ferramentas para mineração de texto e pré-processamento.
library(tidytext)  # #tidytext: Análise de texto em formato tidy.
library(tidyverse) # #tidyverse: Conjunto de pacotes para manipulação e visualização de dados.
library(DT)        # #DT: Tabelas interativas para visualização de dados.
library(wordcloud) # #wordcloud: Criação de nuvens de palavras.
library(dplyr)     # #dplyr: Manipulação de dados.
library(stringr)   # #stringr: Manipulação e operação de strings.
library(tibble)    # #tibble: Estruturas de dados modernas e legíveis.
library(tidyr)     # #tidyr: Transformação de dados em formato tidy.


#Carregar Dados
dados <- read.csv("bar.csv", sep = ";", header = TRUE)
View(dados)

#Adicionando uma coluna com ID aleatória para cada linha 
dados <- dados %>%
  mutate(id = paste0("ID", 1:n()))

View(dados)

#Carregamento dicionario lexico portugues

install.packages("lexiconPT")
library(lexiconPT)
ls("package:lexiconPT")

# Faz o pré-processamento do texto
dados$Comentario <- dados$Comentario %>%
  str_to_lower() %>%                      # Caixa baixa.
  str_replace_all(" *-+ *", "") %>%       # Remove hífen.
  str_replace_all("[[:punct:]]", " ") %>% # Pontuação por espaço.
  removeNumbers() %>%                     # Remove números.
  trimws()                                # Remove espaços nas bordas.

# Stop words padrão do idioma português
stop_words_pt <- stopwords(kind = "pt")

# Remoção das stop words
dados$Comentario <- dados$Comentario %>%
  removeWords(words = c("bom", "muito", "pouco", stop_words_pt))

# Filtra documentos vazios ou com menos de 1 palavra
dados <- dados %>%
  filter(nchar(Comentario) > 0)

# Faz tokenização nas palavras individuais e empilha as palavras
texto_un <- dados %>%
  unnest_tokens(output = "words", input = Comentario)

# Verifica se a tokenização foi bem-sucedida
if (nrow(texto_un) == 0) {
  warning("A tokenização não produziu resultados. Verifique os dados de entrada.")
} else {
  print(texto_un)
}

#Operacoes para Determinar Polaridade

# Operações para determinar a polaridade.

# Uma amostra do dicionário de termos rotulados.
sample_n(oplexicon_v3.0, size = 20) %>%
  arrange(polarity)

# Contagem por polaridade.
oplexicon_v3.0 %>%
  count(polarity, sort = TRUE)

tb_sen <- inner_join(texto_un,
                     oplexicon_v3.0[, c("term", "polarity")],
                     by = c("words" = "term"),
                     relationship = "many-to-many")

# Agora o termos tem sua polaridade presente na tabela.
sample_n(tb_sen, size = 20)

# Faz a agregação da polaridade por documento.
tb <- tb_sen %>%
  group_by(id) %>%
  summarise(soma = sum(polarity),
            n = n(),
            sentiment = soma/n)
tb

# Desidade expírica kernel do escore de sentimento.
ggplot(tb, aes(x = sentiment)) +
  geom_density(fill = "orange", alpha = 0.25) +
  geom_rug() +
  labs(x = "Polaridade", y = "Densidade")

# Frequência relativa acumulada.
ggplot(tb, aes(x = sentiment)) +
  stat_ecdf() +
  geom_rug() +
  labs(x = "Polaridade", y = "Frequência")

# As avaliações mais positivas.
tb %>%
  top_n(sentiment, n = 10) %>%
  inner_join(dados[, c("id", "Comentario")]) %>%
  select(sentiment, Comentario)

# As avaliações mais negativas.
tb %>%
  top_n(sentiment, n = -100) %>%
  inner_join(dados[, c("id", "Comentario")]) %>%
  select(sentiment, Comentario)

# Exibição dos resultados.

# Tabela com as avaliações originais sem o preprocessamento.
tb_u <- tb %>%
  inner_join(dados[, c("id", "Comentario")]) %>%
  select(id, sentiment, Comentario) 

# Valores e cores para formatação condicional das cédulas da tabela.
vals <- seq(-1, 1, by = 0.1)
cols <- colorRampPalette(
  RColorBrewer::brewer.pal(n = 6, name = "Spectral"))(length(vals))

plot(vals, col = cols, pch = 19, cex = 5)

# Define o estilo de formatação condicional.
style <- styleInterval(cuts = head(vals[-1], n = -1),
                       values = cols[-1])

html_table <-
  datatable(tb_u,
            colnames = c("Avaliação",
                         "Sentimento",
                         "Opinião Geral")) %>%
  formatRound(columns = "sentiment", digits = 2) %>%
  formatStyle(columns = names(tb_u),
              valueColumns = "sentiment",
              target = "cell",
              backgroundColor = style)
html_table

#Frequencia de palavra positiva/negativa

# Determina as frequências dos termos de polaridade não nula.
tb_words <- tb_sen %>%
  count(words, polarity, sort = TRUE) %>%
  filter(polarity != 0)

#transforma dados em nuvem de palavras
tb_cloud <- tb_words %>%
  spread(key = "polarity", value = "n", fill = 0) %>%
  rename("negative" = "-1", "positive" = "1")
tb_cloud

#Cria um dataframe com as palavras
tb <- as.data.frame(tb_cloud[, c("negative", "positive")])
rownames(tb) <- tb_cloud$words
head(tb)

# Faz nuvem de palavras.
comparison.cloud(tb,
                 colors = c("red", "blue"),
                 max.words = min(nrow(tb), 150))

# Gráfico de barras para as palavras de maior ocorrência.
n_words <- 20
tb_bars <- tb_words %>%
  mutate(score = polarity * n) %>%
  group_by(polarity) %>%
  top_n(n, n = n_words) %>%
  ungroup()

ggplot(data = tb_bars,
       mapping = aes(x = reorder(words, score),
                     y = score,
                     fill = score)) +
  geom_col(color = "black") +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  coord_flip() +
  theme_light() +
  theme(legend.position = c(0.95, 0.5),
        legend.justification = c(1, 0.5)) +
  labs(y = "Frequência de ocorrência",
       x = "Termo",
       fill = "Frequência de\nocorrência")
