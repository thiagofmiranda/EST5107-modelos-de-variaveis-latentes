if(!require(data.table)){install.packages('data.table')};library(data.table)
if(!require(tidyverse)){install.packages('tidyverse')};library(tidyverse)

year <- 2022

file <- paste0("data/ENEM/",year,"/DADOS/MICRODADOS_ENEM_2022.csv")

data <- fread(file,
              integer64='character',
              skip=0,  #Ler do inicio
              nrow=-1, #Ler todos os registros
              na.strings = "", 
              showProgress = TRUE,encoding = "Latin-1")

data_exam <- data |> 
  filter(TP_PRESENCA_MT == 1) |> 
  select(NU_INSCRICAO, TX_RESPOSTAS_MT, TX_GABARITO_MT)

resp <- data_exam$TX_RESPOSTAS_MT[1:2]
gab <- data_exam$TX_GABARITO_MT[1:2]

dicotomizar <- function(responses,corrections){
  dic_matrix <- (str_split(responses,pattern = "",simplify = T)==str_split(corrections,pattern = "",simplify = T))*1
  
  apply(dic_matrix, 1, str_flatten)
  
}

data_exam <- data_exam |> 
  mutate(TX_CORRECAO_MT = dicotomizar(TX_RESPOSTAS_MT,TX_GABARITO_MT))

