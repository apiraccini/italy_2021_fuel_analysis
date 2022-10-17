### identify and filter informative/well registered data

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)


# data reading -----------------------------------------------------------------

load("data/bigdf.RData")


# imputazione valori mancanti --------------------------------------------------

# prezzi in formato matriciale
mat <- bigdf %>% 
  select(idImpianto, Settimana, Regione, Bandiera, Prezzo) %>%
  arrange(Settimana) %>% 
  pivot_wider(names_from = "Settimana", values_from = "Prezzo")
mat

# differenze prime (mantengo primo valore della serie per tornare indietro tramite cumsum)
dif <- as_tibble(cbind(mat[,1:4], t(apply(mat[,-(1:3)], 1, diff))))
dif$Bandiera <- as.character(dif$Bandiera)
dif

b <- dif[,-1] %>%
  group_by(Regione, Bandiera) %>%
  summarise(across(everything(), list(median), na.rm=TRUE))
coln <- 1:52
colnames(dif) <- c(colnames(dif)[1:3], coln)
colnames(b) <- c(colnames(b)[1:2], coln)
b

dim(na.omit(b))
dim(b[,-c(27,28)]); dim(na.omit(b[,-c(27,28)]))
# gli na sono in corrispondenza delle settimane 25 e 26 che ne hanno molti in partenza
# sostituiti con media mobile semplice di ordine 5
b <- cbind(b[,c(1,2)], t(apply(b[,-c(1,2)], 1, function(series) imputeTS::na_ma(series,2,"simple"))))
dim(b); dim(na.omit(b))

# imputazione valori mancanti (~3 min)
system.time(for(i in 4:ncol(dif)){
  if (i %% 5 == 0) cat(round(i/(ncol(dif)-4)*100, 1), "%\n")
  for (j in 1:nrow(b)){
    cond <- dif$Regione %in% b$Regione[j] & dif$Bandiera %in% b$Bandiera[j] & is.na(dif[,i])
    dif[cond, i] <- b[j, i-1]
  }
})
dif

dif <- dif %>% select(-Regione, -Bandiera)
dif

# backtransformation
mat1 <- as_tibble(cbind(dif[,1], t(apply(dif[,-1], 1, cumsum))))
mat1

imputed_prices <- mat1 %>% pivot_longer(!idImpianto, names_to = "Settimana", values_to = "Prezzo")
imputed_prices

fixed_covariates <- bigdf %>% select(-Mese, -Trimestre, -Settimana, -Prezzo) %>% distinct()
df <- fixed_covariates[rep(1:NROW(fixed_covariates), each = 52),]
df$Settimana = rep(1:52, NROW(fixed_covariates))

df <- merge(df, imputed_prices, by = c('idImpianto', 'Settimana')) %>% 
  arrange(idImpianto, Settimana) %>% 
  as_tibble()

df <- bigdf %>%
  select(Settimana, Mese) %>%
  distinct(Settimana, .keep_all = T) %>% 
  merge(df, by = "Settimana") %>% 
  arrange(idImpianto, Settimana) %>% 
  as_tibble()

df

# abbastanza bene, alcune serie rimangono molto diverse dalle altre
# ma a questo punto si puo evitare di rimuovere arbitrariamente unita'
# set.seed(42)
# ids <- sample(unique(www$idImpianto), 5000)
# na.omit(www) %>%
#   filter(idImpianto %in% ids) %>% 
#   select(idImpianto, Settimana, Prezzo1) %>%
#   group_by(idImpianto, Settimana) %>%
#   summarise(m = mean(Prezzo1)) %>%
#   ggplot(aes(x = Settimana, y = m, group = idImpianto)) +
#   geom_line(aes(alpha = 0.5)) +
#   theme(legend.position = 'none')


# salvataggio dataframe finale -------------------------------------------------

df_cleaned <- df
write.csv2(df_cleaned, file = "data/df_cleaned.csv")
save(df_cleaned, file = "data/df_cleaned.RData")
