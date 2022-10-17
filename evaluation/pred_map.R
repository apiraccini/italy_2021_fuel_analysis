### Visualize predictions on the map for a given model

rm(list = ls())
set.seed(42)


# libraries ---------------------------------------------------------------

library(tidyverse)
library(ggpubr)


# read data ---------------------------------------------------------------

load("data/df.RData")

str(df)
head(df)


# read shapefiles ---------------------------------------------------------

#install.packages('sf')
library(sf)

prov21 <- st_read("data/shapefiles/ProvCM01012021_g")
head(prov21) # sigla e' la chiave
prov21 <- prov21 %>% rename(Provincia = SIGLA)

reg21 <- st_read("data/shapefiles/Reg01012021_g")
head(reg21) # sigla e' la chiave
df <- df %>% mutate(Regione = gsub("/.*", "", Regione))
reg21 <- reg21 %>% rename(Regione = DEN_REG)
reg21$Regione <- ifelse(reg21$Regione == "Friuli Venezia Giulia", "Friuli-Venezia Giulia", reg21$Regione)

ggplot(data = prov21) + geom_sf()
ggplot(data = reg21) + geom_sf()


# load model(s) -----------------------------------------------------------

load("code/modeling/models/additivi.RData")

to_map <- df %>% select(idImpianto, Settimana, Longitudine, Latitudine, Provincia, Regione, Prezzo, fitted, y)
to_map$fitted2 <- fitted(m.gam1)
to_map$Prezzo.previsto <- df$fitted + to_map$fitted2

# by idImpianto
map1 <- to_map %>%
  group_by(idImpianto) %>%
  summarise(Longitudine = first(Longitudine), Latitudine = first(Latitudine),
            Provincia = first(Provincia), Regione = first(Regione),
            Prezzo = mean(Prezzo), Prezzo.previsto = mean(Prezzo.previsto)) %>% 
  st_as_sf(coords = c("Longitudine", "Latitudine"), crs = 4326)

map1_plot <- ggplot() +
  geom_sf(data = map1, aes(color = Prezzo.previsto)) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  geom_sf(data = reg21, fill = NA) + 
  labs(title = "Previsione del prezzo del gasolio",
       subtitle = "Modello additivo\nPrevisioni medie annuali per impianto") +
  theme(legend.title=element_blank())
map1_plot

# by Regione
map2 <- to_map %>% group_by(Regione) %>% summarise(Prezzo.previsto = mean(Prezzo.previsto))
map2_plot <- right_join(reg21, map2, by = "Regione") %>% 
  ggplot(aes(fill = Prezzo.previsto)) +
  geom_sf() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  #labs(title = "Previsione del prezzo del gasolio",
  #     subtitle = "Modello additivo\nPrevisioni medie annuali per regione") + 
  theme(legend.title=element_blank())
map2_plot

# by Provincia
map3 <- to_map %>% group_by(Provincia) %>% summarise(Prezzo.previsto = mean(Prezzo.previsto))
map3_plot <- right_join(prov21, map3, by = "Provincia") %>% 
  ggplot(aes(fill = Prezzo.previsto)) +
  geom_sf() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  #labs(title = "Previsione del prezzo del gasolio",
  #     subtitle = "Modello additivo\nPrevisioni medie annuali per provincia") +
  theme(legend.title=element_blank())
map3_plot


### temporal
names(to_map)
prev_weekly <- to_map %>% 
  group_by(Settimana) %>% 
  summarise(prezzo_real = mean(Prezzo),
            prezzo_prev = mean(Prezzo.previsto),
            low = quantile(Prezzo.previsto, 0.025),
            up = quantile(Prezzo.previsto, 0.975)) %>% 
  ggplot(aes(x = Settimana, y = prezzo_prev)) +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = .7, fill = "wheat") +
  geom_line(col = "orange", size = 2) +
  geom_line(aes(y = prezzo_prev-0.0005), col = "black", alpha = .7, linetype = "dashed") +
  #geom_point(color = "coral") +
  labs(x = "Settimana", y = "Prezzo previsto", title = "Andamento previsto del prezzo del gasolio", subtitle = "Anno 2021") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

prev_weekly


# confronto previsti vs osservati - 1
prev_weekly2 <- to_map %>% 
  group_by(Settimana) %>% 
  summarise(prezzo_real = mean(Prezzo),
            prezzo_prev = mean(Prezzo.previsto)) %>% 
  ggplot(aes(x = prezzo_real, y = prezzo_prev)) +
  geom_point(color = "azure4", size = 3, alpha = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "coral", linetype = "dashed", size = 1.1, alpha = 0.5) +
  labs(x = "Prezzo", y = "Prezzo previsto", title = "Confronto tra le medie settimanali per tutti gli impianti") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

prev_weekly2


# confronto previsti vs osservati - 2
ggplot(to_map, aes(x = Prezzo, y = Prezzo.previsto)) + 
  geom_hex(bins = 40, color = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, col = "green", linetype = "dashed", size = 1.3, alpha = 0.5) +
  scale_fill_viridis_c(option = "B") + 
  labs(x = "Prezzo", y = "Prezzo previsto",
       title = "Confronto tra valori reali e previsti") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))
  

# combine spatial plots
library(ggpubr)
p <- ggarrange(map2_plot, map3_plot)
p <- annotate_figure(p, top = text_grob(paste0("Prezzo medio previsto del gasolio (€/l) \nAnno 2021, Media(sd): ", round(mean(to_map$Prezzo.previsto),3)," (", round(sd(to_map$Prezzo.previsto),2),")"), size = 16))
p


# salvataggio ------------------------------------------------------------------

ggsave("code/evaluation/plots/map_reg_prov_21_previsti.pdf", plot = p, width = 9, height = 5)

ggsave("code/evaluation/plots/prev_weekly.pdf", plot = prev_weekly, width = 7, height = 5)
ggsave("code/evaluation/plots/prev_weekly2.pdf", plot = prev_weekly2, width = 7, height = 5)

ggsave("code/evaluation/plots/mappa_previsioni_bestmodel_byImpianto.pdf", plot = map1_plot, width = 7, height = 5)
ggsave("code/evaluation/plots/mappa_previsioni_bestmodel_byRegione.pdf", plot = map2_plot, width = 7, height = 5)
ggsave("code/evaluation/plots/mappa_previsioni_bestmodel_byProvincia.pdf", plot = map3_plot, width = 7, height = 5)
