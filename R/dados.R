#--------------------------------------------
# Leitura e tratamento dos dados
#--------------------------------------------
# bibliotecas
library(tidyverse)



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


# criando uma nova tabela
dados2 <- dados
# transformando as linhas da coluna "item" em colunas usando a função pivot_wider
dados2 <- pivot_wider(dados, names_from = item, values_from = nota)

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



