if(!require(archive)){install.packages("archive")};library(archive)

# Função de download de microdados
getENEMData <- function(year){
  options(timeout = max(10000, getOption("timeout")))
  
  link <- paste0("https://download.inep.gov.br/microdados/microdados_enem_",year,".zip")
  
  file <- paste0("data/ENEM/",year,"/microdados_enem_",year,".zip")
  
  path <- paste0("data/ENEM/",year)
  if(!dir.exists(path)){dir.create(path)}
  
  download.file(url = link, destfile = file)
  
  archive_extract(file,dir = path)
  
}

# Baixando microdados (Esses dados não estarão no github)
getENEMData("2019")
