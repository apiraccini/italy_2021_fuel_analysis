### exploratory visualizations

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(ggridges)


# load data --------------------------------------------------------------------

load("data/df.RData")
str(df)

df <- df %>% as_tibble()
str(df)


# boxplot per distrib y tra livelli fattori ------------------------------------

# boxplot by regione
prezzo_by_regione <- ggplot(df, aes(x = Regione, y = y, fill = Zona)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per regione",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_regione
ggsave("code/eda/plots_extra/prezzo_by_regione.pdf", plot = prezzo_by_regione, width = 7, height = 5)


# boxplot by zona
prezzo_by_zona <- ggplot(df, aes(x = Zona, y = y, fill = Zona)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per zona geografica",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_zona
ggsave("code/eda/plots_extra/prezzo_by_zona.pdf", plot = prezzo_by_zona, width = 7, height = 5)


# boxplot by tipo impianto
prezzo_by_impianto <- ggplot(df, aes(x = Tipo.Impianto, y = y, fill = Tipo.Impianto)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per tipo di impianto",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_impianto
ggsave("code/eda/plots_extra/prezzo_by_impianto.pdf", plot = prezzo_by_impianto, width = 7, height = 5)


# boxplot by urbanizzazione
prezzo_by_urbanizzazione <- ggplot(df, aes(x = Urbanizzazione, y = y, fill = Urbanizzazione)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per livello di urbanizzazione",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_urbanizzazione
ggsave("code/eda/plots_extra/prezzo_by_urbanizzazione.pdf", plot = prezzo_by_urbanizzazione, width = 7, height = 5)


# boxplot by zona altimetrica
prezzo_by_altitudine <- ggplot(df, aes(x = Altitudine, y = y, fill = Altitudine)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_viridis_d(option = "cividis") + 
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per zona altimetrica",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_altitudine
ggsave("code/eda/plots_extra/prezzo_by_altitudine.pdf", plot = prezzo_by_altitudine, width = 7, height = 5)


# boxplot by popolazione
prezzo_by_popolazione <- ggplot(df, aes(x = Popolazione, y = y, fill = Popolazione)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per fasce di popolazione",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_popolazione
ggsave("code/eda/plots_extra/prezzo_by_popolazione.pdf", plot = prezzo_by_popolazione, width = 7, height = 5)


# boxplot by grande comune
prezzo_by_grandecomune <- ggplot(df, aes(x = Grande.Comune, y = y, fill = Grande.Comune)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  labs(title = "Distribuzione del prezzo del gasolio",
       subtitle = "Stratificazione per grandi comuni",
       x = "Prezzo", y = "") +
  theme(legend.position = "none")

prezzo_by_grandecomune
ggsave("code/eda/plots_extra/prezzo_by_grandecomune.pdf", plot = prezzo_by_grandecomune, width = 7, height = 5)
