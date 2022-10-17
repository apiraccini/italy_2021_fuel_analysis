### temporal visualizations

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


# analisi marginale risposta ---------------------------------------------------

# andamento prezzo medio con bande (by AP)
prezzo_weekly <- df %>% 
  group_by(Settimana) %>% 
  summarise(prezzo_medio = mean(Prezzo),
            prezzo_mediano = median(Prezzo),
            prezzo_sd = sd(Prezzo),
            low = quantile(Prezzo, 0.025),
            up = quantile(Prezzo, 0.975)) %>% 
  ggplot(aes(x = Settimana, y = prezzo_medio)) +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = .7, fill = "wheat") +
  geom_line(col = "orange") +
  geom_point(color = "coral") +
  labs(x = "Settimana", y = "Prezzo", title = "Andamento del prezzo del gasolio", subtitle = "Anno 2021") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

prezzo_weekly
ggsave("code/eda/plots/prezzo_weekly.pdf", plot = prezzo_weekly, width = 7, height = 5)


# stima di densita' divisa per settimane
df$Mese <- as.factor(df$Mese)
levels(df$Mese) <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno",
                        "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre")
prezzo_monthly <- df %>% group_by(idImpianto, Mese) %>% summarise(Prezzo = mean(Prezzo)) %>% 
  ggplot(aes(x = Prezzo, y = Mese, fill = as.numeric(Mese))) + 
  geom_density_ridges(alpha = 0.7) +
  scale_fill_continuous(low = "orange", high = "red") + 
  #theme_ridges() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) +
  labs(title = "Andamento del prezzo del gasolio",
       subtitle = "Stima di densità e trend mensile")

prezzo_monthly
ggsave("code/eda/plots/prezzo_monthly.pdf", plot = prezzo_monthly, width = 7, height = 5)


y_monthly <- df %>% group_by(idImpianto, Mese) %>% summarise(Prezzo = mean(y)) %>% 
  ggplot(aes(x = Prezzo, y = Mese, fill = as.numeric(Mese))) + 
  geom_density_ridges(alpha = 0.7) +
  scale_fill_continuous(low = "orange", high = "red") + 
  #theme_ridges() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) +
  labs(title = "Andamento del prezzo del gasolio",
       subtitle = "Stima di densità e trend mensile")

y_monthly


# visualizzazioni temporali stratificate ---------------------------------------

# andamento prezzo medio per regione
trend_prezzo_by_regione <- df %>% group_by(Settimana, Regione) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, color = Regione)) +
  geom_line() + 
  scale_colour_viridis_d(option = "turbo") + 
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per Regione")

trend_prezzo_by_regione
ggsave("code/eda/plots_extra/trend_prezzo_by_regione.pdf", plot = trend_prezzo_by_regione, width = 7, height = 5)


# andamento prezzo medio per zona geografica
trend_prezzo_by_zona <- df %>% group_by(Settimana, Zona) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, colour = Zona)) +
  geom_line() + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per zona geografica")

trend_prezzo_by_zona
ggsave("code/eda/plots_extra/trend_prezzo_by_zona.pdf", plot = trend_prezzo_by_zona, width = 7, height = 5)


# andamento prezzo medio per tipo impianto
trend_prezzo_by_impianto <- df %>% group_by(Settimana, Tipo.Impianto) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, colour = Tipo.Impianto)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per tipo di impianto")

trend_prezzo_by_impianto
ggsave("code/eda/plots_extra/trend_prezzo_by_impianto.pdf", plot = trend_prezzo_by_impianto, width = 7, height = 5)


# andamento prezzo medio per zona altimetrica
trend_prezzo_by_altitudine <- df %>% group_by(Settimana, Altitudine) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, colour = Altitudine)) +
  geom_line() +
  scale_colour_viridis_d(option = "cividis") +
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per zona altimetrica")

trend_prezzo_by_altitudine
ggsave("code/eda/plots_extra/trend_prezzo_by_altitudine.pdf", plot = trend_prezzo_by_altitudine, width = 7, height = 5)


# andamento prezzo medio per livello urbanizzazione
trend_prezzo_by_urbanizzazione <- df %>% group_by(Settimana, Urbanizzazione) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, colour = Urbanizzazione)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per livello di urbanizzazione")

trend_prezzo_by_urbanizzazione
ggsave("code/eda/plots_extra/trend_prezzo_by_urbanizzazione.pdf", plot = trend_prezzo_by_urbanizzazione, width = 7, height = 5)


# andamento prezzo medio per costa o no
trend_prezzo_by_litorale <- df %>% group_by(Settimana, Costa) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, colour = Costa)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per zone costiere")

trend_prezzo_by_litorale
ggsave("code/eda/plots_extra/trend_prezzo_by_litorale.pdf", plot = trend_prezzo_by_litorale, width = 7, height = 5)


# andamento prezzo medio per popolazione
trend_prezzo_by_popolazione <- df %>% group_by(Settimana, Popolazione) %>% summarise(Prezzo = mean(Prezzo)) %>%
  ggplot(aes(x = Settimana, y = Prezzo, colour = Popolazione)) +
  geom_line() + 
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Andamento del prezzo del gasolio", subtitle = "Stratificazione per fasce di popolazione")

trend_prezzo_by_popolazione
ggsave("code/eda/plots_extra/trend_prezzo_by_popolazione.pdf", plot = trend_prezzo_by_popolazione, width = 7, height = 5)
