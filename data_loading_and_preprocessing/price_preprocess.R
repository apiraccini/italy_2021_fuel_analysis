### read and preprocess price data

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(data.table)
library(lubridate)
library(dplyr)


# utils ------------------------------------------------------------------------

proj_dir <- "."
data_dir <- paste0(proj_dir, "/data")


# data reading -----------------------------------------------------------------

### leggo dataset prezzi (solo Q1 o Q1_Q4)
get_price_df <- function(data_dir, all = F){
  
  file_names_price <- c()
  
  for(i in 1:ifelse(all,4,1)){
    files_dir_price <- paste0("data/2021_", i, "/prezzi")
    file_names_price <- c(file_names_price, list.files(path = files_dir_price, pattern = "*.csv", full.names = T))
  }
  
  cat("Leggo i dati...\n")
  price_bigdf_raw <- plyr::rbind.fill(lapply(file_names_price, function(x) fread(x, skip = 1)))# read.csv(x, sep = ";", skip = 1, stringsAsFactors = T)))
  print(dim(price_bigdf_raw))
  
  cat("Creo covariate temporali da data...\n")
  price_bigdf_raw$Diesel <- ifelse(price_bigdf_raw$descCarburante %in% c("Gasolio", "Blue Diesel"), 1, 0)
  price_bigdf_raw$dtComu <- as.POSIXlt(price_bigdf_raw$dtComu, format = "%d/%m/%Y %H:%M:%OS")
  
  price_bigdf_raw <- cbind(price_bigdf_raw, as.data.frame(unclass(price_bigdf_raw$dtComu)[c("hour", "wday", "mon")]))
  price_bigdf_raw$week <- week(price_bigdf_raw$dtComu)
  
  cat("Filtro solo le unità con carburante Diesel e modalità Self-Service...\n")
  price_bigdf <- price_bigdf_raw %>% filter(Diesel == 1 & isSelf ==1) %>% select(idImpianto, prezzo, hour, wday, week, mon)
  rm(price_bigdf_raw)
  
  cat("Riduco il dataset ottenendo per ogni Impianto la media settimanale dei prezzi...\n")
  price_df <- price_bigdf %>% group_by(idImpianto, week) %>% summarise(pmedio = mean(prezzo), mese = first(mon))
  rm(price_bigdf)
  
  price_df
}

system.time(price_bigdf <- get_price_df(data_dir, all = T))
gc()
str(price_bigdf)
dim(price_bigdf) # 211k con solo q1, 822k con q1-q4 --> onesto, meno di un Giga in RAM
length(table(price_bigdf$idImpianto)) #23609


# save modified data -----------------------------------------------------------

readr::write_csv2(price_bigdf, file = "data/price_df.csv")
save(price_bigdf, file = "data/price_df.RData")
