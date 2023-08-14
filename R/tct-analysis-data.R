library(tidyverse)
library(data.table)
library(psych)
library(psychometric)
#library(coefficientalpha)
#library(ltm)

bisserialCorr <- function(data_tct){
  data_tct <- as.data.frame(data_tct)
  all <- biserial(x=rowSums(data_tct),y=as.matrix(data_tct))
  
  out <- numeric(0)
  for (i in 1:ncol(data_tct)) {
    out[i] <- (biserial(x=rowSums(data_tct[,-i]),y=as.matrix(data_tct[,i])))
  }
  
  data.frame(corr.biss = all, corr.biss.out = out)
}

# Ano de estudo
year <- 2022

# Lendo arquivo
file <- paste0("data/ENEM/",year,"/DADOS/RESPONSES.csv")
data <- fread(file,
              integer64='character',
              skip=0,  #Ler do inicio
              nrow=-1, #Ler todos os registros
              na.strings = "", 
              showProgress = TRUE,encoding = "Latin-1")

itens <- data.table::fread(input=paste0("data/ENEM/",year,"/DADOS/ITENS_PROVA_2022.csv"),integer64='character')


# itens anulados (Sem gabarito)
itens |> 
  filter(TX_GABARITO=="X")

# O item 39443 será removido da análise
data_tct <- data |> 
  dplyr::select(-NU_INSCRICAO,-`039443`)


# Análise TCT
# psych package
psych_alpha_enem <- alpha(data_tct)

# ltm package
ltm_descript_enem <- ltm::descript(data_tct)

# psychometric package
psychometric_item.exam_enem <- item.exam(data_tct, discrim=TRUE)

biss_corr <- bisserialCorr(data_tct)

tct_enem <- psychometric_item.exam_enem |> 
  dplyr::select(Difficulty,Discrimination,Sample.SD,Item.total,Item.Tot.woi) |> 
  rownames_to_column("Item") |> 
  tibble() |> 
  bind_cols(biss_corr) |> 
  mutate(AlphaSem = ltm_descript_enem$alpha[-1,1]) |> 
  setNames(c("Item","Dificuldade","Discriminacao","DesvioPadrao","CorrelacaoPontoBisserial","CorrelacaoPontoBisserialSem","CorrelacaoBisserial","CorrelacaoBisserialSem","AlphaSem"))

save(tct_enem,file = "lab/tct_enem.RData")


# ----------------
library(foreign)

mathb <- read.spss("data/mathb.sav")
a <- data.frame(mathb)
mathbitems <- a[,2:15]

# Análise TCT
# psych package
psych_alpha_mathb <- alpha(mathbitems)

# ltm package
ltm_descript_mathb <- ltm::descript(mathbitems)

# psychometric package
psychometric_item.exam_mathb <- item.exam(mathbitems, discrim=TRUE)

biss_corr <- bisserialCorr(mathbitems)

tct_mathb <- psychometric_item.exam_mathb |> 
  dplyr::select(Difficulty,Discrimination,Sample.SD,Item.total,Item.Tot.woi) |> 
  rownames_to_column("Item") |> 
  tibble() |> 
  bind_cols(biss_corr) |> 
  mutate(AlphaSem = ltm_descript_mathb$alpha[-1,1]) |> 
  setNames(c("Item","Dificuldade","Discriminacao","DesvioPadrao","CorrelacaoPontoBisserial","CorrelacaoPontoBisserialSem","CorrelacaoBisserial","CorrelacaoBisserialSem","AlphaSem"))

save(tct_mathb,file = "lab/tct_mathb.RData")


# ----------------
library(foreign)
library(itemanalysis)

atitudes <- read.spss("data/baseunionfinal.sav")
a <- data.frame(atitudes)
atitudesitems <- a[,c(4:25)]

tct_atitudesitems <- itemanalysis2(atitudesitems,options = 1:5,correction=TRUE, span.par=.3,verbose=F)

stats <- data.frame(tct_atitudesitems$item.stat) 

prop <- data.frame(tct_atitudesitems$dist.sel) |> 
  setNames(paste0("Prop.",1:5))

bis.cor <- data.frame(tct_atitudesitems$dist.disc) |> 
  setNames(paste0("Point.Biserial.",1:5))

tct_atitudes <- bind_cols(stats,prop,bis.cor) |> 
  rownames_to_column("Item")



save(tct_atitudes,file = "lab/tct_atitudes.RData")
