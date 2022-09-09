### read and preprocess anagraphical data, then merge with geographical info

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(data.table)
library(tidyverse)


# utils ------------------------------------------------------------------------

proj_dir <- "~/R_Jobs/StatAziende"
data_dir <- paste0(proj_dir, "/data")

if (normalizePath(getwd()) != normalizePath(proj_dir)) setwd(proj_dir)


# data reading (by AP)----------------------------------------------------------

### leggo dataset anagrafica (solo Q1 o Q1_Q4)
get_anag_df <- function(data_dir, all = F){
  
  file_names_anag <- c()
  anag_varnames <- c("idImpianto", "Bandiera", "Tipo.Impianto", "Comune", "Provincia", "Latitudine", "Longitudine")
  
  for(i in 1:ifelse(all,4,1)){
    files_dir_anag <- paste0("data/2021_", i, "/anagrafica")
    file_names_anag <- c(file_names_anag, list.files(path = files_dir_anag, pattern = "*.csv", full.names = T))
  }
  
  anag_bigdf_raw <- plyr::rbind.fill(lapply(file_names_anag, function(x) fread(x, skip = 1)))
  cat("Ho letto i dati...\n")
  
  colnames(anag_bigdf_raw) <- make.names(colnames(anag_bigdf_raw))
  print(dim(anag_bigdf_raw))
  anag_bigdf <- anag_bigdf_raw[, anag_varnames]
  rm(anag_bigdf_raw)
  
  anag_bigdf %>% distinct(idImpianto, .keep_all = T) # la maggior parte sono duplicati!!!
}

system.time(anag_bigdf <- get_anag_df(data_dir, all = T))
gc()
str(anag_bigdf)
dim(anag_bigdf)
length(table(anag_bigdf$idImpianto))


# add geographical information (by LC)------------------------------------------

anag_bigdf <- anag_bigdf %>% 
  mutate(Provincia = ifelse(is.na(Provincia), "NAP", Provincia))

# problema accenti: soluzione brute force -> tolgo tutti gli apostrofi
anag_bigdf <- anag_bigdf %>% mutate(Comune = gsub("'", "", Comune))


# dataset con comune - regione - provincia -------------------------------------
comuni <- readxl::read_xls("data/Elenco-comuni-italiani.xls", sheet = 1) %>% as_tibble()
names(comuni) <- make.names(names(comuni))

comuni <- comuni %>% 
  select(Denominazione.in.italiano, Denominazione.Regione, Sigla.automobilistica)

comuni <- comuni %>% 
  rename(Comune = Denominazione.in.italiano,
         Regione = Denominazione.Regione,
         Provincia = Sigla.automobilistica) %>% 
  mutate(Comune = str_to_upper(Comune)) # nomi comuni in upper case come in anag

comuni <- comuni %>% 
  mutate(Comune = stringi::stri_trans_general(Comune, "Latin-ASCII")) # elimino accenti

comuni <- comuni %>% mutate(Comune = gsub("'", "", Comune)) # elimino apostrofi

comuni <- comuni %>% mutate(Provincia = ifelse(is.na(Provincia), "NAP", Provincia))

# quali comuni perdo
# which(!sapply(unique(anag_bigdf$Comune), function(x) x %in% unique(comuni$Comune))) # come si puo scrivere meglio??


# dataset con le informazioni geografiche dei comuni ---------------------------
comuni_geo <- readxl::read_xls("data/Classificazioni statistiche-e-dimensione-dei-comuni_30_06_2021.xls",
                               sheet = 1)
comuni_geo <- comuni_geo %>% select(`Denominazione (Italiana e straniera)`,
                                    `Popolazione residente al 31/12/2020`,
                                    `Zona altimetrica`,
                                    `Comune litoraneo`, `Comune isolano`, `Zone costiere`,
                                    `Grado di urbanizzazione`)
comuni_geo <- comuni_geo %>% rename(Comune = `Denominazione (Italiana e straniera)`,
                                    Popolazione = `Popolazione residente al 31/12/2020`,
                                    Altitudine = `Zona altimetrica`,
                                    Litorale = `Comune litoraneo`,
                                    Isola = `Comune isolano`,
                                    Costa = `Zone costiere`,
                                    Urbanizzazione = `Grado di urbanizzazione`)

comuni_geo <- comuni_geo %>% mutate(Comune = str_to_upper(Comune)) # nomi comuni in upper case come in anag

comuni_geo <- comuni_geo %>% 
  mutate(Comune = stringi::stri_trans_general(Comune, "Latin-ASCII")) # elimino accenti

comuni_geo <- comuni_geo %>% mutate(Comune = gsub("'", "", Comune)) # elimino apostrofi

# elimino il nome il lingua straniera dopo "/" o "-"
comuni_geo <- comuni_geo %>% mutate(Comune = ifelse(Comune != comuni$Comune, gsub("\\/.*", "", Comune), Comune))
comuni_geo <- comuni_geo %>% mutate(Comune = ifelse(Comune != comuni$Comune, gsub("\\-.*", "", Comune), Comune))

# which(!sapply(unique(anag_bigdf$Comune), function(x) x %in% unique(comuni_geo$Comune)))

comuni <- comuni %>% filter(Comune %in% unique(anag_bigdf$Comune)) # tengo solo i comuni che sono in anag
comuni_geo <- comuni_geo %>% filter(Comune %in% unique(anag_bigdf$Comune))

# ci sono alcuni paesi che hanno lo stesso nome -> sono pochi e poco significativi -> si possono eliminare
which(table(comuni$Comune)!=1)
comuni_rep <- c("CALLIANO", "CASTRO", "LIVO", "PATERNO", "SAMONE", "SAN TEODORO")
comuni <- comuni %>% filter(!(Comune %in% comuni_rep))
comuni_geo <- comuni_geo %>% filter(!(Comune %in% comuni_rep))

# Altrimenti si puo far così -> aggiungo le prime lettere della regione 
# comuni$Comune[comuni$Comune %in% comuni_rep] <- paste(comuni$Comune[comuni$Comune %in% comuni_rep],
#                                                       str_sub(comuni$Regione[comuni$Comune %in% comuni_rep],1,3))
# comuni_geo$Comune[comuni_geo$Comune %in% comuni_rep] <- paste(comuni_geo$Comune[comuni_geo$Comune %in% comuni_rep],
#                                                       str_sub(comuni$Regione[comuni_geo$Comune %in% comuni_rep],1,3))


# combino i due dataset appena costruiti ---------------------------------------

comuni_fin <- inner_join(comuni, comuni_geo, by = "Comune")
comuni_fin

rm(comuni, comuni_geo)

# espando anag_df con le nuove informazioni
anag_bigdf <- anag_bigdf %>% select(-Provincia) # elimino la provincia che ce l'ho in entrambe le tabelle
anag_bigdf <- inner_join(anag_bigdf, comuni_fin, by="Comune")


str(anag_bigdf)


# CALCOLO DEL NUMERO DI IMPIANTI VICINI ----------------------------------------

anag_bigdf <- anag_bigdf %>% mutate(Latitudine = as.numeric(Latitudine), Longitudine = as.numeric(Longitudine)) %>%
  na.omit()
anag_bigdf <- anag_bigdf %>% filter(Latitudine>35 & Latitudine<47)
anag_bigdf <- anag_bigdf %>% filter(Longitudine>6 & Longitudine<18)

summary(anag_bigdf %>% select(Latitudine, Longitudine))

#install.packages("geosphere")
library(geosphere)

dist <- 2.5 # * 2 km: lato del quadrato
# a quanto corrispondondo 10 km in differenza tra Latitudini
km2lat <- round(uniroot(function(x) distHaversine(p1=c(30,40), p2=c(30,40+x))/1000 - dist, interval = c(0,0.1))$root,3)
# a quanto corrispondondo 10 km in differenza di Latitudine
# ovviamente cambiando la Longitudine cambia la distanza tra le Latitudini
# ma non ci serve questa precisione, consideriamo rispetto alla Latitudine mediana
km2long <- round(uniroot(function(x) distHaversine(p1=c(10,median(anag_bigdf$Latitudine)), p2=c(10+x,median(anag_bigdf$Latitudine)))/1000 - dist, interval = c(0,1))$root,3)

# faccio il calcolo per quelli in un quadrato di lato 5 km e in un quadrato di lato 500 m
dist <- 0.25
km2lat_close <- round(uniroot(function(x) distHaversine(p1=c(30,40), p2=c(30,40+x))/1000 - dist, interval = c(0,0.1))$root,3)
km2long_close <- round(uniroot(function(x) distHaversine(p1=c(10,median(anag_bigdf$Latitudine)), p2=c(10+x,median(anag_bigdf$Latitudine)))/1000 - dist, interval = c(0,1))$root,3)

# quindi ora per ogni impianto dobbiamo calcolare quanti sono entro +- 0.045 di latitudine e +- 0.062 di longitudine
# ci mette una vita ma who cares, dobbiamo calcolarlo solo una volta
# forse con data.table è piu veloce
n <- n2 <-  rep(NA, nrow(anag_bigdf))
for (i in 1:nrow(anag_bigdf)){
  lat <- anag_bigdf$Latitudine[i]
  long <- anag_bigdf$Longitudine[i]
  n[i] <- anag_bigdf %>% filter( (Latitudine > lat - km2lat & Latitudine < lat + km2lat) & 
                                   Longitudine > long - km2long & Longitudine < long + km2long) %>%
    summarise(n()) %>% unlist()
  n2[i] <- anag_bigdf %>% filter( (Latitudine > lat - km2lat_close & Latitudine < lat + km2lat_close) & 
                                   Longitudine > long - km2long_close & Longitudine < long + km2long_close) %>%
    summarise(n()) %>% unlist()
  if(i %% 1000 == 0) cat(round(i/nrow(anag_bigdf)*100), "% \n")
}

anag_bigdf$Vicini <- n
anag_bigdf$MoltoVicino <- ifelse(n2>1 & anag_bigdf$Tipo.Impianto!="Autostrada", 1, 0)

# controllo che i valori siano sensati
summary(anag_bigdf$Vicini)
table(anag_bigdf$MoltoVicino)
# gli impianti con molti vicini sono quelli nelle grandi città (Milano, Napoli, Roma) o nelle vicinanze
anag_bigdf %>% filter(Vicini > 20) %>% select(Comune) %>% unique() %>% head(30)
# gli impianti con nessun vicino sono isole e in montagna
anag_bigdf %>% filter(Vicini == 1) %>% select(Comune) %>% unique() %>% head(30) 

str(anag_bigdf)


# save modified data -----------------------------------------------------------

readr::write_csv2(anag_bigdf, file = "data/anag_df.csv")
save(anag_bigdf, file = "data/anag_df.RData")