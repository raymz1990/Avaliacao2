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
                   "Fox",
                   "Polo")) {
    return("Volkswagen")
  } else if (carro %in% c("Onix", 
                          "Celta", 
                          "Montana", 
                          "Agile",
                          "Cruze",
                          "S10")) {
    return("Chevrolet")
  } else if (carro %in% c("Punto", 
                          "Strada", 
                          "Uno", 
                          "Bravo",
                          "Idea",
                          "Palio",
                          "Siena")) {
    return("Fiat")
  } else if (carro %in% c("KA", 
                          "Ecosport", 
                          "Focus", 
                          "Ranger")) {
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

# criando 3ª tabela:
dados3 <- dados2 %>%
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

dados_cores_CSAT <- data.frame(
  Class_CSAT = c("Muito Insatisfeito", "Insatisfeito", "Neutro", "Satisfeito", "Muito Satisfeito"),
  cores_CSAT = c("#FF4D4D", "#FF9C4D", "#FFD24D", "#A5D24D", "#4DAF7C")
)

color_mtoinsatisfeito <- dados_cores_CSAT$cores_CSAT[dados_cores_CSAT$Class_CSAT == "Muito Insatisfeito"]
color_insatisfeito <- dados_cores_CSAT$cores_CSAT[dados_cores_CSAT$Class_CSAT == "Insatisfeito"]
color_neutro <- dados_cores_CSAT$cores_CSAT[dados_cores_CSAT$Class_CSAT == "Neutro"]
color_satisfeito <- dados_cores_CSAT$cores_CSAT[dados_cores_CSAT$Class_CSAT == "Satisfeito"]
color_mtosatisfeito <- dados_cores_CSAT$cores_CSAT[dados_cores_CSAT$Class_CSAT == "Muito Satisfeito"]



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


#########################################

# Tabela NPS
dados_NPS <- dados2 %>%
  group_by(marca, carro, Class_NPS) %>%
  summarise(Valor = n(), .groups = "drop") %>%
  arrange(marca, carro, Class_NPS)
dados_NPS <- merge(dados_NPS, dados_cores_NPS, by = "Class_NPS", all = TRUE)

Promotores_total <- sum(dados_NPS$Valor[dados_NPS$Class_NPS == "Promotores"])
Promotores_total <- format(Promotores_total, big.mark = ".", decimal.mark = ",") 
Promotores <- round(sum(dados_NPS$Valor[dados_NPS$Class_NPS == "Promotores"]) / 
                      sum(dados_NPS$Valor), 4) * 100

Detratores_total <- sum(dados_NPS$Valor[dados_NPS$Class_NPS == "Detratores"])
Detratores_total <- format(Detratores_total, big.mark = ".", decimal.mark = ",") 
Detratores <- round(sum(dados_NPS$Valor[dados_NPS$Class_NPS == "Detratores"]) / 
                      sum(dados_NPS$Valor), 4) * 100

Passivos_total <- sum(dados_NPS$Valor[dados_NPS$Class_NPS == "Passivos"])
Passivos_total <- format(Passivos_total, big.mark = ".", decimal.mark = ",") 
Passivos <- round(sum(dados_NPS$Valor[dados_NPS$Class_NPS == "Passivos"]) / 
                    sum(dados_NPS$Valor), 4) * 100

NPS_indice <- round((Promotores - Detratores), 4)

# Tabela NPS por Marca
dados_NPS_marca <- dados_NPS %>%
  group_by(marca, Class_NPS, cores_NPS) %>%
  summarise(Valor = sum(Valor), .groups = "drop") %>%
  arrange(marca, Class_NPS, cores_NPS) 
dados_NPS_marca <- dados_NPS_marca %>%
  group_by(marca) %>%
  mutate(freq = round(Valor / sum(Valor) * 100, 2))

# Tabela NPS por Carro
dados_NPS_carro <- dados_NPS %>%
  group_by(carro) %>%
  mutate(freq = round(Valor / sum(Valor) * 100, 2))

# Tabela Geral Carro
dados_NPS_Geral <- dados_NPS %>%
  select(marca, carro, Class_NPS, Valor) %>%
  rename(Marca = marca, Carro = carro) %>%
  pivot_wider(names_from = Class_NPS, values_from = Valor) %>%
  group_by(Carro) %>%
  mutate(Total = Promotores + Passivos + Detratores) %>%
  mutate(NPS = round(
    ((Promotores / (Promotores + Passivos + Detratores ))  - 
    (Detratores / (Promotores + Passivos + Detratores ))) * 100, 2))

# Tabela Marca
dados_NPS_Geral2 <- dados_NPS_marca %>%
  select(marca, Class_NPS, Valor) %>%
  rename(Marca = marca) %>%
  pivot_wider(names_from = Class_NPS, values_from = Valor) %>%
  group_by(Marca) %>%
  mutate(Total = Promotores + Passivos + Detratores) %>%
  mutate(NPS = round(
    ((Promotores / (Promotores + Passivos + Detratores ))  - 
       (Detratores / (Promotores + Passivos + Detratores ))) * 100, 2))



# Tabela CSAT
# unique(dados3$CSAT)


dados_CSAT <- dados3 %>%
  select(indice, marca, carro, CSAT, `Valor CSAT`) %>%
  rename(Marca = marca, Carro = carro) %>%
  pivot_wider(names_from = CSAT, values_from = `Valor CSAT`) %>%
  mutate(Media = round(rowMeans(select(., Estilo, Acabamento, `Posição de dirigir`, Instrumentos, Interior, `Porta-malas`, Desempenho, Motor, Câmbio, Freios, Suspensão, Consumo, Estabilidade, `Custo-Benefício `)), 2)) %>%
  mutate(Class_CSAT = case_when(
    Media < 2 ~ "Muito Insatisfeito",
    Media < 4 ~ "Insatisfeito",
    Media <= 6 ~ "Neutro",
    Media <= 8 ~ "Satisfeito",
    TRUE ~ "Muito Satisfeito"
  ))
dados_CSAT <- merge(dados_CSAT, dados_cores_CSAT, by = "Class_CSAT", all = TRUE)

# Tabela CSAT por Marca
dados_CSAT_marca <- dados_CSAT %>%
  group_by(Marca, Class_CSAT) %>%
  summarise(Valor = n(), .groups = "drop") %>%
  arrange(Marca, Class_CSAT) 
dados_CSAT_marca <- dados_CSAT_marca %>%
  group_by(Marca) %>%
  mutate(freq = round(Valor / sum(Valor) * 100, 2))
dados_CSAT_marca <- merge(dados_CSAT_marca, dados_cores_CSAT, by = "Class_CSAT", all = TRUE)


## Tabela CSAT por Carro
dados_CSAT_carro <- dados_CSAT %>%
  group_by(Marca, Carro, Class_CSAT) %>%
  summarise(Valor = n(), .groups = "drop") %>%
  arrange(Marca, Carro, Class_CSAT) 
dados_CSAT_carro <- dados_CSAT_carro %>%
  group_by(Carro) %>%
  mutate(freq = round(Valor / sum(Valor) * 100, 2))
dados_CSAT_carro <- merge(dados_CSAT_carro, dados_cores_CSAT, by = "Class_CSAT", all = TRUE)

# Tabela Geral Carro
dados_CSAT_Geral <- dados_CSAT_carro %>%
  select(Marca, Carro, Class_CSAT, Valor) %>%
  pivot_wider(names_from = Class_CSAT, values_from = Valor) %>%
  group_by(Carro) %>%
  mutate(
    `Muito Insatisfeito` = replace(`Muito Insatisfeito`, is.na(`Muito Insatisfeito`), 0),
    Insatisfeito = replace(Insatisfeito, is.na(Insatisfeito), 0),
    Neutro = replace(Neutro, is.na(Neutro), 0),
    Satisfeito = replace(Satisfeito, is.na(Satisfeito), 0),
    `Muito Satisfeito` = replace(`Muito Satisfeito`, is.na(`Muito Satisfeito`), 0),
    Total = `Muito Insatisfeito` + Insatisfeito + Neutro + Satisfeito + `Muito Satisfeito`
  )


# Tabela Marca
dados_CSAT_Geral2 <- dados_CSAT_marca %>%
  select(Marca, Class_CSAT, Valor) %>%
  pivot_wider(names_from = Class_CSAT, values_from = Valor) %>%
  group_by(Marca) %>%
  mutate(
    `Muito Insatisfeito` = replace(`Muito Insatisfeito`, is.na(`Muito Insatisfeito`), 0),
    Insatisfeito = replace(Insatisfeito, is.na(Insatisfeito), 0),
    Neutro = replace(Neutro, is.na(Neutro), 0),
    Satisfeito = replace(Satisfeito, is.na(Satisfeito), 0),
    `Muito Satisfeito` = replace(`Muito Satisfeito`, is.na(`Muito Satisfeito`), 0),
    Total = `Muito Insatisfeito` + Insatisfeito + Neutro + Satisfeito + `Muito Satisfeito`
  )


########### ESTATISTICA ######
# Criar o dataframe para o heatmap NPS
dados_heatmap <- dados2 %>%
  select(marca, Recomendação) %>%
  group_by(marca, Recomendação) %>%
  summarise(valor = n(), .groups = 'drop') %>%
  arrange(marca, Recomendação)
dados_heatmap[is.na(dados_heatmap)] <- 0
colnames(dados_heatmap) <- c("Marca", "Recomendação", "Valor")

media <- round(mean(dados2$Recomendação),1)
recomendacao_10 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 10]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_9 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 9]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_8 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 8]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_7 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 7]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_6 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 6]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_5 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 5]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_4 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 4]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_3 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 3]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_2 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 2]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_1 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 1]) / sum(dados_heatmap$Valor),3) * 100
recomendacao_0 <- round(sum(dados_heatmap$Valor[dados_heatmap$Recomendação == 0])/ sum(dados_heatmap$Valor),3) * 100

# Criar o dataframe para o heatmap CSAT
dados_heatmap2 <- dados_CSAT %>%
  select(Marca, Media) 
dados_heatmap2$Nota <- round(dados_heatmap2$Media)
dados_heatmap2 <- dados_heatmap2%>%
  select(Marca, Nota) %>%
  group_by(Marca, Nota) %>%
  summarise(valor = n(), .groups = 'drop') 
colnames(dados_heatmap2) <- c("Marca", "Média CSAT", "Valor")



#dados_heatmap3 <- dados3 %>%
#  select(marca, carro, CSAT, `Valor CSAT`)
#colnames(dados_heatmap3) <- c("Marca", "Carro", "CSAT", "Nota")
#dados_heatmap3 <- dados_heatmap3 %>%
#  count(Nota, name = "valor")


#  group_by(Nota) %>%
#  
#  summarise(valor = n(), .groups = 'drop')
#  arrange(marca, carro, CSAT, `Valor CSAT`)
#colnames(dados_heatmap2) <- c("Marca", "Média CSAT", "Valor")





















