### merge price and anagraphical data, add external regional info

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(dplyr)


# data loading -----------------------------------------------------------------

load("data/anag_df.RData")
load("data/price_df.RData")

# identify number of unique ids
length(table(anag_bigdf$idImpianto)) #25439
length(table(price_bigdf$idImpianto)) #23609 --> join usando price come riferimento


# merge dataframes -------------------------------------------------------------

system.time(bigdf <- merge(x = price_bigdf, y = anag_bigdf, by = "idImpianto"))
str(bigdf)
nrow(bigdf) == nrow(na.omit(bigdf))


# quick preprocessing ----------------------------------------------------------

# Nomi
bigdf <- bigdf %>% rename(Settimana = week,
                          Prezzo = pmedio,
                          Mese = mese)
str(bigdf)

# Unita' mal registrate
bigdf <- bigdf %>% filter(Settimana != 53)
bigdf <- bigdf %>% filter(Prezzo > 1 & Prezzo < 2)

# Zona geografica e regioni a statuto speciale
zona <- c("Nord-Est", "Nord-Ovest", "Nord-Ovest", "Centro", "Nord-Ovest", "Centro",
          "Sud", "Sud", "Sud", "Sud", "Centro", "Nord-Est", "Nord-Est", "Nord-Est",
          "Isole", "Centro", "Nord-Ovest", "Sud", "Isole", "Sud")
zona_df <- data.frame(Regione = unique(bigdf$Regione), Zona = zona)
zona_df <- zona_df %>% 
  mutate(Statuto.Speciale = ifelse(Regione %in% c("Trentino-Alto Adige/Südtirol", 
                                                  "Friuli-Venezia Giulia", "Sicilia", "Umbria",
                                                  "Valle d'Aosta/Vallée d'Aoste", "Sardegna"), "Si", "No"))
bigdf <- inner_join(bigdf, zona_df, "Regione")

# Grande Comune
bigdf$Grande.Comune <- ifelse(bigdf$Popolazione > 200*1e3, 1, 0)

# Nomi
bigdf <- bigdf %>% mutate(Urbanizzazione = as.factor(Urbanizzazione),
                          Litorale = as.factor(Litorale),
                          Isoletta = as.factor(Isola),
                          Costa = as.factor(Costa), 
                          Trimestre = as.factor(ceiling((Mese+1)/3)),
                          Tipo.Impianto = as.factor(Tipo.Impianto),
                          Mese = Mese+1,
                          Zona = as.factor(Zona),
                          MoltoVicino = as.factor(MoltoVicino),
                          Statuto.Speciale = as.factor(Statuto.Speciale),
                          Grande.Comune = as.factor(Grande.Comune),
                          Popolazione = cut(Popolazione, c(0, 2000, 10000, 50000, 3*1e6)),
                          Altitudine1 = as.factor(Altitudine),
                          Latitudine = as.numeric(Latitudine),
                          Longitudine = as.numeric(Longitudine),)

# Altitudine
bigdf$Altitudine <- "Pianura"
bigdf$Altitudine[bigdf$Altitudine1 %in% c("1", "2")] <- "Montagna"
bigdf$Altitudine[bigdf$Altitudine1 %in% c("3", "4")] <- "Collina"
bigdf <- bigdf %>% select(-c(Altitudine1, Litorale, Isola)) %>% mutate(Altitudine = as.factor(Altitudine))

str(bigdf)


# join with percentage of families with pollution problems (2019) --------------

poll_reg_pre <- read.csv("data/PROBLEMS_REGIONE.csv")
str(poll_reg_pre)
poll_reg_pre$Territorio[poll_reg_pre$Territorio == "Valle d'Aosta / VallÃ©e d'Aoste"] <- "Valle d'Aosta/Vallée d'Aoste"
poll_reg_pre$Territorio[poll_reg_pre$Territorio %in% c("Provincia Autonoma Trento", "Provincia Autonoma Bolzano / Bozen")] <- "Trentino-Alto Adige/Südtirol"
poll_reg <- poll_reg_pre %>% 
  filter(Seleziona.periodo == 2019) %>%
  filter(Misura == "valori per cento") %>% 
  filter(Presenza.di.problemi.nella.zona.di.residenza == "inquinamento") %>% 
  select(Territorio, Value) %>% 
  filter(Territorio %in% unique(bigdf$Regione)) %>% 
  rename(Regione = Territorio, Perc.Inquinamento.Regione = Value)
poll_reg
poll_reg[5,2] <- poll_reg[5,2] + poll_reg[6,2]
poll_reg <- poll_reg[-6,]
poll_reg[is.na(poll_reg[,2]),2] <- min(poll_reg[,2], na.rm = T)
poll_reg

bigdf <- merge(bigdf, poll_reg, by = "Regione")
str(bigdf)


# join with 2020 regional mobility info ----------------------------------------

mob_reg_pre <- read.csv("data/MOBILITA_LAVORO_REGIONALE.csv", header = T)
mob_reg_pre$Territorio[mob_reg_pre$Territorio == "Trentino Alto Adige / SÃ¼dtirol"] <- "Trentino-Alto Adige/Südtirol"
mob_reg_pre$Territorio[mob_reg_pre$Territorio == "Valle d'Aosta / VallÃ©e d'Aoste"] <- "Valle d'Aosta/Vallée d'Aoste"
mob_reg <- mob_reg_pre %>% 
  filter(Tipo.dato == "usano mezzi di trasporto") %>%
  filter(Misura == "per 100 persone con le stesse caratteristiche") %>% 
  select(Territorio, Value) %>% 
  filter(Territorio %in% unique(bigdf$Regione)) %>% 
  rename(Regione = Territorio, Perc.Mezzi.Lavoro.Regione = Value)

bigdf <- merge(bigdf, mob_reg, by = "Regione")
str(bigdf)


# join with regional mobility info (train and bus) -----------------------------

# train info
train_reg_pre <- read.csv("data/PERC_UTILIZZO_TRENO.csv")
train_reg_pre$Territorio[train_reg_pre$Territorio == "Trentino Alto Adige / SÃ¼dtirol"] <- "Trentino-Alto Adige/Südtirol"
train_reg_pre$Territorio[train_reg_pre$Territorio == "Valle d'Aosta / VallÃ©e d'Aoste"] <- "Valle d'Aosta/Vallée d'Aoste"

train_reg <- train_reg_pre %>% 
  filter(TIPO_DATO_AVQ == "14_TR") %>% 
  filter(Misura == "per 100 persone con le stesse caratteristiche") %>% 
  select(Territorio, Value) %>% 
  filter(Territorio %in% unique(bigdf$Regione)) %>% 
  rename(Regione = Territorio, Perc.Train.Regione = Value)

# pullman info
pull_reg_pre <- read.csv("data/PERC_UTILIZZO_PULLMAN.csv")
pull_reg_pre$Territorio[pull_reg_pre$Territorio == "Trentino Alto Adige / SÃ¼dtirol"] <- "Trentino-Alto Adige/Südtirol"
pull_reg_pre$Territorio[pull_reg_pre$Territorio == "Valle d'Aosta / VallÃ©e d'Aoste"] <- "Valle d'Aosta/Vallée d'Aoste"

pull_reg <- pull_reg_pre %>%
  filter(TIPO_DATO_AVQ == "14_COACH") %>%
  filter(Misura == "per 100 persone con le stesse caratteristiche") %>%
  select(Territorio, Value) %>%
  filter(Territorio %in% unique(bigdf$Regione)) %>%
  rename(Regione = Territorio, Perc.Pullman.Regione = Value)

# autobus info
autobus_reg_pre <- read.csv("data/PERC_UTILIZZO_AUTOBUS.csv")
autobus_reg_pre$Territorio[autobus_reg_pre$Territorio == "Trentino Alto Adige / SÃ¼dtirol"] <- "Trentino-Alto Adige/Südtirol"
autobus_reg_pre$Territorio[autobus_reg_pre$Territorio == "Valle d'Aosta / VallÃ©e d'Aoste"] <- "Valle d'Aosta/Vallée d'Aoste"

autobus_reg <- autobus_reg_pre %>% 
  filter(TIPO_DATO_AVQ == "14_BUS") %>% 
  filter(Misura == "per 100 persone con le stesse caratteristiche") %>% 
  select(Territorio, Value) %>% 
  filter(Territorio %in% unique(bigdf$Regione)) %>% 
  rename(Regione = Territorio, Perc.Autobus.Regione = Value)

mob2_reg <- Reduce(function(x,y) merge(x, y, by = "Regione"), list(train_reg, pull_reg, autobus_reg))
mob2_reg <- mob2_reg %>%
  mutate(Perc.Mezzi.Regione = Perc.Autobus.Regione + Perc.Pullman.Regione + Perc.Train.Regione) %>% 
  select(Regione, Perc.Mezzi.Regione)

bigdf <- merge(bigdf, mob2_reg, by = "Regione")
str(bigdf)


# join with 2020 regional GDP(PIL) ---------------------------------------------

pil_reg_pre <- read.csv("data/PIL_REGIONALE.csv", header = T)
pil_reg_pre$Territorio[pil_reg_pre$Territorio == "Trentino Alto Adige / SÃ¼dtirol"] <- "Trentino-Alto Adige/Südtirol"
pil_reg_pre$Territorio[pil_reg_pre$Territorio == "Valle d'Aosta / VallÃ©e d'Aoste"] <- "Valle d'Aosta/Vallée d'Aoste"
pil_reg <- pil_reg_pre %>%
  filter(Tipo.aggregato == "prodotto interno lordo ai prezzi di mercato") %>% 
  filter(Valutazione == "prezzi correnti") %>% 
  filter(Seleziona.periodo == 2020) %>% 
  filter(Edizione == "Dic-2021") %>% 
  select(Territorio, Value) %>% 
  distinct() %>% 
  filter(Territorio %in% unique(bigdf$Regione)) %>% 
  rename(Regione = Territorio, log.PIL.Regione = Value)
pil_reg$log.PIL.Regione <- log(pil_reg$log.PIL.Regione)

bigdf <- merge(bigdf, pil_reg, by = "Regione")
str(bigdf)


# handling of Bandiera (reduce nlevels) ----------------------------------------

bigdf$Bandiera <- factor(bigdf$Bandiera)
bigdf$Bandiera <- factor(bigdf$Bandiera, levels = c(levels(bigdf$Bandiera), "altro"))

bandiera.altro <- bigdf %>% group_by(Bandiera) %>%  summarise(tot = n()) %>% 
  filter(tot < 10000) %>% pull(Bandiera)

bigdf$Bandiera[bigdf$Bandiera %in% bandiera.altro ] = "altro"
bigdf$Bandiera <- factor(bigdf$Bandiera)
levels(bigdf$Bandiera) <- make.names(levels(bigdf$Bandiera))


# handling of factor levels ----------------------------------------------------

levels(bigdf$Popolazione) <- c("<2.000 ab.", "2.000-10.000 ab.", "10.000-50.000 ab.", "50.000+ ab.")
bigdf$Popolazione <- relevel(bigdf$Popolazione, ref = "<2.000 ab.")

levels(bigdf$MoltoVicino) <- c("No", "Si")
bigdf$MoltoVicino <- relevel(bigdf$MoltoVicino, ref = "No")

levels(bigdf$Grande.Comune) <- c("No","Si")
bigdf$Grande.Comune <- relevel(bigdf$Grande.Comune, ref = "No")

levels(bigdf$Isoletta) <- c("No", "Si")
bigdf$Isoletta <- relevel(bigdf$Isoletta, ref = "No")

levels(bigdf$Costa) <- c("No", "Si")
bigdf$Costa <- relevel(bigdf$Costa, ref = "No")

levels(bigdf$Urbanizzazione) <- c("Alta", "Media", "Bassa")
bigdf$Urbanizzazione <- relevel(bigdf$Urbanizzazione, ref = "Bassa")

bigdf$Bandiera <- relevel(bigdf$Bandiera, ref = "altro")
bigdf$Zona <- relevel(bigdf$Zona, ref = "Centro")
bigdf$Tipo.Impianto <- relevel(bigdf$Tipo.Impianto, ref = "Altro")
bigdf$Statuto.Speciale <- relevel(bigdf$Statuto.Speciale, ref = "No")

bigdf$Regione <- as.factor(bigdf$Regione)
bigdf$Regione <- relevel(bigdf$Regione, ref = "Lazio")

#bigdf$Regione[bigdf$Regione == "Trentino-Alto Adige/Südtirol"] <- "Trentino Alto Adige"
#bigdf$Regione[bigdf$Regione == "Emilia-Romagna"] <- "Emilia Romagna"
#bigdf$Regione[bigdf$Regione == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"
#bigdf$Regione[bigdf$Regione == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"

str(bigdf)


# save final dataframe ---------------------------------------------------------

write.csv2(bigdf, file = "data/bigdf.csv")
save(bigdf, file = "data/bigdf.RData")
