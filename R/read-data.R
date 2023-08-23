if(!require(data.table)){install.packages('data.table')};library(data.table)
if(!require(tidyverse)){install.packages('tidyverse')};library(tidyverse)
if(!require(dtplyr)){install.packages('dtplyr')};library(dtplyr)

# Função para dicotomizar repostas
dicotomizar <- function(responses,corrections){
  dic_matrix <- (str_split(responses,pattern = "",simplify = T)==str_split(corrections,pattern = "",simplify = T))*1
  
  apply(dic_matrix, 1, str_flatten)
  
}

# Ano de estudo
year <- 2022

# Provas de matemática
provas <- c(1075,1076,1077,1078)

# Lendo arquivo
file <- paste0("data/ENEM/",year,"/DADOS/MICRODADOS_ENEM_2022.csv")
data <- fread(file,
              integer64='character',
              skip=0,  #Ler do inicio
              nrow=-1, #Ler todos os registros
              na.strings = "", 
              showProgress = TRUE,encoding = "Latin-1")


# Filtrando e dicotomizando repostas
data_exam <- data |> 
  lazy_dt() |> 
  filter(TP_PRESENCA_MT == 1) |> 
  filter(CO_PROVA_MT %in% provas) |> 
  select(NU_INSCRICAO,CO_PROVA_MT, TX_RESPOSTAS_MT, TX_GABARITO_MT)|> 
  mutate(TX_CORRECAO_MT = dicotomizar(TX_RESPOSTAS_MT,TX_GABARITO_MT)) |> 
  as.data.frame()
    
# Lendo itens 
itens <- data.table::fread(input=paste0("data/ENEM/",year,"/DADOS/ITENS_PROVA_2022.csv"),integer64='character')
itens <- itens |> 
  filter(SG_AREA=="MT") |> 
  filter(CO_PROVA %in% provas) |> 
  select(CO_POSICAO, CO_ITEM, CO_HABILIDADE,TX_COR,CO_PROVA, IN_ITEM_ADAPTADO) |> 
  mutate(CO_POSICAO=CO_POSICAO-135) |> 
  arrange(CO_ITEM)|> 
  as.data.frame()
data_exam
# Tratando respostas e ajustando ordenação de itens
responses <- data.frame(
  NU_INSCRICAO=data_exam$NU_INSCRICAO,
  CO_PROVA_MT=data_exam$CO_PROVA_MT,
  str_split(data_exam$TX_CORRECAO_MT,pattern = "",simplify = T))|> 
  pivot_longer(X1:X45,names_to = "CO_POSICAO",values_to = "RESPONSE")

responses <- responses |>
  lazy_dt() |> 
  mutate(CO_POSICAO  = as.numeric(str_remove(CO_POSICAO ,"X"))) |> 
  left_join(select(itens,CO_PROVA,CO_POSICAO,CO_ITEM), by = c("CO_PROVA_MT"="CO_PROVA","CO_POSICAO")) |> 
  mutate(CO_ITEM = str_pad(CO_ITEM,width = 6,side = "left",pad = "0")) |> 
  select(NU_INSCRICAO,CO_ITEM,RESPONSE) |> 
  mutate(RESPONSE = as.numeric(RESPONSE)) |> 
  arrange(NU_INSCRICAO,CO_ITEM)|> 
  as.data.frame() |> 
  pivot_wider(names_from = "CO_ITEM",values_from = "RESPONSE") 


# Salvando respostas dicotomizadas
fwrite(x = responses,file = paste0("data/ENEM/",year,"/DADOS/RESPONSES.csv"))