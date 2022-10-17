# Analisi fattori macroeconomici

# libraries --------------------------------------------------------------------

rm(list=ls())
library(tidyverse)


# read data --------------------------------------------------------------------

fuel_data <- read.csv("data/weekly_fuel_prices_all_data_from_2005_to_20220330.csv")
str(fuel_data)

# PRICE = prezzo finale
# VAT = tasse (cioè iva credo)
# EXCISE = accise
# NET = prezzo senza tasse
# CHANGE = differenza del prezzo rispetto all'oservazione precedente

diesel <- fuel_data %>% filter(PRODUCT_NAME == "Automotive gas oil") %>% select(-PRODUCT_NAME) 
diesel <- diesel %>% rename(DATE = SURVEY_DATE)
diesel <- diesel %>% mutate(DATE = as.Date(DATE))

crud.oil <- read.csv("data/Crud_price.csv") %>% mutate(DATE = as.Date(DATE)) %>% 
  rename(crud.d  = DCOILBRENTEU)
str(crud.oil)

ue2dol <- read.csv("data/dol_eur_rate.csv") %>% mutate(DATE = as.Date(DATE)) %>% 
  rename(eu2dol.exc  = DEXUSEU)
str(ue2dol)

df <- inner_join(crud.oil, ue2dol, by="DATE")
df <- inner_join(df, diesel) %>% filter(DATE < as.Date("2022-01-01"))
df <- df %>% mutate(mese = months(DATE), year = format(DATE, "%Y"))
df <- df %>% mutate(crud.eu = crud.d/eu2dol.exc) # trasformo la quatazione brett in valuta EURO
str(df)


# grafici ----------------------------------------------------------------------

# prezzo gasolio (senza tasse) vs quatazione Brett petrolio grezzo
## divido per 159 (i litri di un barile) in modo che siano nella stessa unità di misura
analisi_macro <- df %>% select(DATE, NET, crud.eu) %>%
  mutate(NET = NET/1000, crud.eu = crud.eu/159) %>%
  gather(key = "variable", value = "value", -DATE) %>%
  ggplot(aes(x = DATE, y=value)) +
  geom_line(aes(color = variable)) + 
  labs(title = "Analisi macroeconomica",
       subtitle = "Confronto fra prezzo netto del gasolio e prezzo del petrolio al barile",
       x = "", y = "Prezzo") +
  scale_color_discrete(name = "", labels = c("Prezzo netto gasolio (€/l)", "Prezzo petrolio al barile (€/l)")) +
  theme(legend.position = "bottom")

analisi_macro
ggsave("code/eda/plots/analisi_macro.pdf", plot = analisi_macro, width = 7, height = 5)


# BONUS: analisi cointegrazione ------------------------------------------------
## Dal grafico è ovvio che ci sia una relazione di cointegrazione

library(urca)
library(dynlm)

df %>% select(NET, crud.eu) %>% acf

df.lm <- df %>% select(NET, crud.eu) %>% mutate(NET = NET/1000, crud.eu = crud.eu/159)
m1 <- lm(NET ~ crud.eu, data=df.lm)
summary(m1)
res1 <- resid(m1)

layout(1:3)
plot.ts(res1)
acf(res1)  
pacf(res1)
layout(1)

test.r1 <- ur.df(res1)
summary(test.r1) # non c'è radice unitaria --> ok relazione di cointegrazione

# ecm
df.ecm <- df.lm %>% cbind(res1) %>% ts() 
str(df.ecm)
ecm1 <- dynlm(d(NET) ~ 1 + L(res1) + d(crud.eu), data=df.ecm)
summary(ecm1)
acf(residuals(ecm1))
pacf(residuals(ecm1)) 
# rimane dell'autocorrelazione nei residui --> aggiungere ritardi nel modello dinamico dynlm
