library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)
library(censobr)
library(scales)

#Carregando conjunto de Dados 
conj_dados = list_geobr()
renda <- read.csv("DomicilioRenda_SP2.csv", sep = ";", header = TRUE)
lanchonetes <- read.csv("latlon.csv", sep = ";", header = TRUE)

#Transformando variáveis para numéricas 
cols <- c("V003", "V004", "V005", "V006", "V007", "V008", "V009", "V010", "V011", "V012", "V013", "V014")
renda[cols] <- lapply(renda[cols], as.numeric)

# Criando variável renda média (Quantidade de pessoas por renda total)
renda$total <- (renda$V003 + renda$V004) / rowSums(renda[, c("V005", "V006", "V007", "V008", "V009", "V010", "V011", "V012", "V013", "V014")])
renda$total

#Separando do pacote GEOBR os limites de cada unidade censitária de Bauru
r = read_census_tract(code_tract = 3506003)

#Gráfico
ggplot() + geom_sf(data = r, fill = "#2D3E50", color="#FEBF57", size =.15, 
                   show.legend = FALSE)

#Separando a área urbana 
ru = read_census_tract(code_tract = 3506003, year = 2000, zone = "urban")
ggplot() + geom_sf(data = ru, fill = "#2D3E50", color="#FEBF57", size =.15, 
                   show.legend = FALSE)

#Separando por área rural
rr = read_census_tract(code_tract = 3506003, year = 2000, zone = "rural")
ggplot() + geom_sf(data = rr, fill = "#2D3E50", color="#FEBF57", size =.15, 
                   show.legend = FALSE)

# Convertendo ambas as colunas para `character` para preservar os dígitos completos
r$code_tract <- as.character(r$code_tract)
renda$Cod_setor <- format(renda$Cod_setor, scientific = FALSE)

# Mesclar os dataframes com base nos IDs correspondentes
#Um dos códigos mais importantes
r_merged <- left_join(r, renda, by = c("code_tract" = "Cod_setor"))

# Criar o mapa com `V003` sobre os polígonos
ggplot() + 
  geom_sf(data = r_merged, aes(fill = total), color = "#FEBF57", size = 0.15, show.legend = FALSE) +
  geom_text(data = r_merged, aes(geometry = geom, label = V003), stat = "sf_coordinates", color = "white", size = 3) +
  labs(title = "Mapa com Valores de renda") +
  theme_minimal()

# Plotar o mapa com os valores de V003
ggplot() + 
  geom_sf(data = r_merged, aes(geometry = geom, fill = total), color = "#FEBF57", size = 0.15, show.legend = FALSE) +
  geom_sf_text(data = r_merged, aes(geometry = geom, label = V003), color = "white", size = 3) +
  labs(title = "Mapa com Valores de V003") +
  theme_minimal()

# Criar o mapa de Calor para Renda (inserir legenda apenas em locais com renda > 10000)
ggplot() + 
  geom_sf(data = r_merged, aes(geometry = geom, fill = total), color = "#FEBF57", size = 0.15, alpha = 0.7) + # Camada de polígonos dos setores censitários
  geom_sf_text(data = r_merged %>% filter(total > 10000), aes(geometry = geom, label = total), color = "white", size = 2) + # Camada de texto para valores de V003 acima de 10.000
  labs(title = "Mapa com Valores de Renda Total (Filtrados por > 10000)") + # Título e ajustes de tema
  scale_fill_viridis_c(name = "total") + # Escala de cor para a variável V003
  theme_minimal() # Tema minimalista

# Criar o mapa
ggplot() + 
  geom_sf(data = r_merged, aes(geometry = geom, fill = total), color = "#FEBF57", size = 0.15, alpha = 0.7) +  # Camada de polígonos dos setores censitários com coloração baseada em "total"
  labs(title = "Mapa de Calor de Renda por Setores Censitários") +  # Título e ajustes de tema
  scale_fill_viridis_c(name = "Renda Total", option = "magma", direction = -1) +  # Escala de cor contínua para representar a variável de renda
  theme_minimal() +  # Tema minimalista
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) # Remover bordas dos eixos e grid para focar no mapa

setores_jardim_estoril <- c("350600305000027", "350600305000028", "350600305000029", "350600305000030", "350600305000031",
                            "350600305000062", "350600305000063", "350600305000064", "350600305000065", "350600305000066",
                            "350600305000067", "350600305000068", "350600305000103", "350600305000104", "350600305000105",
                            "350600305000106", "350600305000107", "350600305000108")
setores_vila_universitaria <- c("350600305000195", "350600305000196", "350600305000197", "350600305000198", 
                                "350600305000256", "350600305000257", "350600305000258", "350600305000259")

# Filtrar cada bairro separadamente
dados_jardim_estoril <- r_merged %>% filter(code_tract %in% setores_jardim_estoril)
dados_vila_universitaria <- r_merged %>% filter(code_tract %in% setores_vila_universitaria)

# Remover linhas com valores ausentes em Longitude e Latitude
lanchonetes <- na.omit(lanchonetes[, c("Longitude", "Latitude")])

# Agora, converta para um objeto `sf`
lanchonetes_sf <- st_as_sf(lanchonetes, coords = c("Longitude", "Latitude"), crs = 4326)

# Se o mapa `r_merged` estiver em um CRS diferente, faça a transformação para o mesmo sistema
lanchonetes_sf <- st_transform(lanchonetes_sf, crs = st_crs(r_merged))

# Filtrar as lanchonetes que estão dentro de cada bairro
lanchonetes_jardim_estoril <- st_intersection(lanchonetes_sf, dados_jardim_estoril)
lanchonetes_vila_universitaria <- st_intersection(lanchonetes_sf, dados_vila_universitaria)

# Gráfico para Jardim Estoril com pontos de lanchonetes
ggplot() +
  geom_sf(data = dados_jardim_estoril, aes(fill = total), color = "#FEBF57", size = 0.15, alpha = 0.7) +
  geom_sf(data = lanchonetes_jardim_estoril, color = "red", size = 2, alpha = 0.8) +  # Somente lanchonetes dentro do bairro
  labs(title = "Jardim Estoril") +
  scale_fill_viridis_c(name = "Renda Total", option = "magma", direction = -1) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

# Gráfico para Vila Universitária com pontos de lanchonetes
ggplot() +
  geom_sf(data = dados_vila_universitaria, aes(fill = total), color = "#FEBF57", size = 0.15, alpha = 0.7) +
  geom_sf(data = lanchonetes_vila_universitaria, color = "red", size = 2, alpha = 0.8) +  # Somente lanchonetes dentro do bairro
  labs(title = "Vila Universitária") +
  scale_fill_viridis_c(name = "Renda Total", option = "magma", direction = -1) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())





















