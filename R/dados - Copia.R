#--------------------------------------------
# Leitura e tratamento dos dados
#--------------------------------------------
# bibliotecas
library(tidyverse)
library(plotly)
library(dplyr)



url <- "https://raw.githubusercontent.com/andersonara/datasets/master/aval_carros_nota.txt"
dados <- read.table(file = url,
                    header = TRUE,
                    sep = "\t",
                    quote = "",
                    stringsAsFactors = FALSE)
dados$carro <- str_to_title(dados$carro)
dados$carro <- str_replace(dados$carro, "\\bKa\\b", "KA")
dados$carro <- str_replace(dados$carro, "\\bHb20\\b", "HB20")


# conhecer os carros
unique(dados$carro)



# criando uma coluna "marca" para cada carro
marca <- function(carro) {
  if (carro %in% c("Gol", 
                   "Voyage", 
                   "Saveiro",
                   "Jetta",
                   "Crossfox", 
                   "Polo")) {
    return("Volkswagen")
  } else if (carro %in% c("Fox", 
                          "Onix", 
                          "Celta", 
                          "Montana", 
                          "Agile", 
                          "S10")) {
    return("Chevrolet")
  } else if (carro %in% c("Punto", 
                          "Strada", 
                          "Uno", 
                          "Bravo", 
                          "Siena",
                          "Cruze",
                          "Agile")) {
    return("Fiat")
  } else if (carro %in% c("KA", 
                          "Ecosport", 
                          "Focus", 
                          "Ranger",
                          "Idea",
                          "Palio")) {
    return("Ford")
  } else if (carro %in% c("Duster", 
                          "Fluence", 
                          "Clio", 
                          "Sandero", 
                          "Logan")) {
    return("Renault")
  } else if (carro %in% c("J3")) {
    return("JAC")
  } else if (carro %in% c("City", 
                          "Fit")) {
    return("Honda")
  } else if (carro %in% c("HB20", 
                          "Tucson")) {
    return("Hyundai")
  } else {
    return("Outra marca")
  }
}
# incluindo a coluna marca na tabela dados
dados$marca <- sapply(dados$carro, marca)
unique(dados$marca)

# exportando os dados em .csv

# Exporte os dados para o arquivo .csv
#write.csv(dados,
#          file = ".R/dados_csv/dados.csv", 
#          row.names = FALSE,
#          fileEncoding = "UTF-8")


# criando uma nova tabela, transformando as linhas da coluna "item" em colunas usando a função pivot_wider e adicionando o índice
dados2 <- dados %>%
  pivot_wider(names_from = item, values_from = nota) %>%
  mutate(indice = row_number())

# incluindo classificação NPS
dados2 <- dados2 %>%
  mutate(Class_NPS = case_when(
    Recomendação <= 6 ~ "Detratores",
    Recomendação <= 8 ~ "Passivos",
    TRUE ~ "Promotores"
  ))
dados2 <- dados2 %>%
  pivot_longer(cols = c(
    Estilo,
    Acabamento,
    `Posição de dirigir`,
    Instrumentos,
    Interior,
    `Porta-malas`,
    Desempenho,
    Motor,
    Câmbio,
    Freios,
    Suspensão,
    Consumo,
    Estabilidade,
    `Custo-Benefício `
    ),
               names_to = "CSAT",
               values_to = "Valor CSAT") 


# Exporte os dados para o arquivo .csv
#write.csv(dados2,
#          file = ".R/dados_csv/dados2.csv", 
#          row.names = FALSE,
#          fileEncoding = "UTF-8")


# criando paleta de cores
paleta_cores <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
dados_cores <- data.frame(marca = unique(dados$marca), cor = paleta_cores)

color_chevrolet <- dados_cores$cor[dados_cores$marca == "Chevrolet"]
color_fiat <- dados_cores$cor[dados_cores$marca == "Fiat"]
color_ford <- dados_cores$cor[dados_cores$marca == "Ford"]
color_honda <- dados_cores$cor[dados_cores$marca == "Honda"]
color_hyundai <- dados_cores$cor[dados_cores$marca == "Hyundai"]
color_jac <- dados_cores$cor[dados_cores$marca == "JAC"]
color_renault <- dados_cores$cor[dados_cores$marca == "Renault"]
color_volkswagen <- dados_cores$cor[dados_cores$marca == "Volkswagen"]

dados_cores_NPS <- data.frame(
  Class_NPS = c("Promotores", "Passivos", "Detratores"),
  cores_NPS = c("#00BF6F", "#FCD036", "#E53935")
)


color_promotores <- dados_cores_NPS$cores_NPS[dados_cores_NPS$Class_NPS == "Promotores"]
color_passivos <- dados_cores_NPS$cores_NPS[dados_cores_NPS$Class_NPS == "Passivos"]
color_detratores <- dados_cores_NPS$cores_NPS[dados_cores_NPS$Class_NPS == "Detratores"]



#########################################
# Contagem de carros por marca e carro
dados_carro <- dados2 %>%
  group_by(marca, carro) %>%
  summarise(quantidade = n(), .groups = "drop") %>%
  arrange(marca, carro)
dados_carro <- data.frame(dados_carro)

# Adicionar coluna de soma da quantidade por marca
dados_carro <- dados_carro %>%
  group_by(marca) %>%
  mutate(total_marca = ifelse(row_number() == 1, sum(quantidade), 0))

# Conectar cores aos dados
dados_carro <- merge(dados_carro, dados_cores, by = "marca", all = TRUE)

# total veiculos
total_veiculos <- sum(dados_carro$quantidade)
total_veiculos <- format(total_veiculos, big.mark = ".", decimal.mark = ",")

# total tipo veiculos
tipo_veiculos <- length(unique(dados_carro$carro))

# total marca
quantidade_marca <- length(unique(dados_carro$marca))



# Criação de ranking
prop <- dados_carro
prop$freq_marca <- prop$total_marca / sum(prop$total_marca)
prop$freq_carro <- prop$quantidade / sum(prop$quantidade)

prop_marca <- subset(prop[c("marca", "total_marca", "cor", "freq_marca")], total_marca != 0) %>%
  arrange(desc(total_marca)) %>%
  mutate(indice = row_number())

melhores_marcas <- round((sum(prop_marca$freq[1:3]) * 100), 1)

