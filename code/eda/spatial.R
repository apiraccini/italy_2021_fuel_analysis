### Spatial visualization

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
head(df)


# read shapefiles --------------------------------------------------------------

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


# prezzo medio per provincia e per regione -------------------------------------

Prezzo_21_r <- df %>% group_by(Regione) %>% summarise(Prezzo = mean(Prezzo))
plot_21_r <- right_join(reg21, Prezzo_21_r, by = "Regione") %>% 
  ggplot(aes(fill = Prezzo)) +
  geom_sf() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  #labs(title = "Prezzo medio del gasolio (€/l)", subtitle = paste0("Anno 2021, Media(sd): ", round(mean(df$Prezzo),3)," (", round(sd(df$Prezzo),2),")")) + 
  theme(legend.title=element_blank())

Prezzo_21_p <- df %>% group_by(Provincia) %>% summarise(Prezzo = mean(Prezzo))
plot_21_p <- right_join(prov21, Prezzo_21_p, by = "Provincia") %>% 
  ggplot(aes(fill = Prezzo)) +
  geom_sf() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  #labs(title = "Prezzo medio del gasolio (€/l)", subtitle = paste0("Anno 2021, Media(sd): ", round(mean(df$Prezzo),3)," (", round(sd(df$Prezzo),2),")")) +
  theme(legend.title=element_blank())


library(ggpubr)
p <- ggarrange(plot_21_r, plot_21_p)
p <- annotate_figure(p, top = text_grob(paste0("Prezzo medio del gasolio (€/l) \nAnno 2021, Media(sd): ", round(mean(df$Prezzo),3)," (", round(sd(df$Prezzo),2),")"), size = 16))
p

ggsave("code/eda/plots/map_reg_prov_21.pdf", plot = p, width = 9, height = 5)
#ggsave("code/eda/plots/map_reg_21.pdf", plot = plot_21_r, width = 7, height = 5)
#ggsave("code/eda/plots/map_prov_21.pdf", plot = plot_21_p, width = 7, height = 5)
