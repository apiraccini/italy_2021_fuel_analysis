### Linear mixed effects model

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(lme4)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
head(df)


# MODELLAZIONE -----------------------------------------------------------------

# risposta e covariate
features <- c("Regione", "Bandiera", "Tipo.Impianto", "Popolazione",
              "Altitudine", "Costa", "Urbanizzazione", "Vicini",
              "MoltoVicino", "Grande.Comune", "Isoletta")

features2 <- c("Bandiera", "Tipo.Impianto", "Popolazione",
               "Altitudine", "Costa", "Urbanizzazione", "Vicini",
               "MoltoVicino", "Grande.Comune", "Isoletta", 
               "Statuto.Speciale", "Zona", "Perc.Inquinamento.Regione",
               "Perc.Mezzi.Lavoro.Regione", "Perc.Mezzi.Regione", "log.PIL.Regione")

features3 <- c("Bandiera", "Tipo.Impianto", "Popolazione",
              "Altitudine", "Costa", "Urbanizzazione", "Vicini",
              "MoltoVicino", "Grande.Comune", "Isoletta")

response <- "y"

# gestione covariate
tipovar <- sapply(df[,features], class)
tipovar

quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
qual <- setdiff(features, quant)

df[,qual] <- lapply(df[,qual], factor)

str(df[,c(features, response)])

df <- df %>% mutate(Regione = as.factor(Regione),
                    Provincia = as.factor(Provincia))


# modello lineare con effetti casuali  -----------------------------------------
# (Regione effetto fisso, Provincia casuale)

form1 <- as.formula(paste0("y ~ ",paste0(features, collapse = " + "), " + (1|Provincia)"))
form1
system.time(m.ran1 <- lmer(form1, data = df))

summary(m.ran1)
sigmas1 <- as.data.frame(VarCorr(m.ran1))[,4]
cci1 <- sigmas1[1]/sum(sigmas1)
names(cci1) <- "Provincia"
cci1


# modello lineare con effetti casuali ------------------------------------------
# (Regione casuale) 

form2 <- as.formula(paste0("y ~ ",paste0(features3, collapse = " + "), " + (1|Regione)"))
form2
system.time(m.ran2 <- lmer(form2, data = df))

summary(m.ran2)
sigmas2 <- as.data.frame(VarCorr(m.ran2))[,4]
cci2 <- sigmas2[1]/sum(sigmas2)
names(cci2) <- c("Regione")
cci2


# modello lineare con effetti casuali ------------------------------------------
# (Regione/Provincia casuale) 

form3 <- as.formula(paste0("y ~ ",paste0(features3, collapse = " + "), " + (1|Regione/Provincia)"))
form3
system.time(m.ran3 <- lmer(form3, data = df))

summary(m.ran3)
sigmas3 <- as.data.frame(VarCorr(m.ran3))[,4]
cci3 <- sigmas3[1:2]/sum(sigmas3)
names(cci3) <- c("Provincia", "Regione")
cci3


# visualizzazione coefficienti -------------------------------------------------

plot_coef <- function(m.ran){
  dati.rand <- data.frame(stima = fixef(m.ran)[-1])
  dati.rand$segno <- ifelse(dati.rand$stima > 0, "1", "-1")
  dati.rand$variabile <- names(summary(m.ran)$coefficients[-1,1])
  dati.rand <- arrange(dati.rand, stima)
  dati.rand$variabile <- factor(dati.rand$variabile, levels = unique(dati.rand$variabile))
  
  coef.m.rand <- ggplot(dati.rand) +
    geom_bar(aes(x = variabile, y = stima, fill = segno), stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = c("coral", "seagreen1")) +
    geom_hline(yintercept = 0) +
    labs(x = "", y = "", title = "Modello ad effetti misti", subtitle = "Effetti fissi stimati") +
    coord_flip() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_text(margin = ggplot2::margin(15,0,0,0)),
      axis.text = element_text(size = 9),
      legend.position = "none",
      axis.line = element_line(colour = "black"),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14)
    )
  
  coef.m.rand
}

coef.m.rand1 <- plot_coef(m.ran1)
coef.m.rand2 <- plot_coef(m.ran2)
coef.m.rand3 <- plot_coef(m.ran3)

coef.m.rand1
coef.m.rand2
coef.m.rand3


# salvataggio modelli e grafici ------------------------------------------------

save(m.ran1, m.ran2, m.ran3, cci1, cci2, cci3, coef.m.rand1, coef.m.rand2, coef.m.rand3, 
     file = "code/modeling/models/random.RData")

ggsave("code/modeling/plots/coefficienti_random_1.pdf", plot = coef.m.rand1, width = 7, height = 5)
ggsave("code/modeling/plots/coefficienti_random_2.pdf", plot = coef.m.rand2, width = 7, height = 5)
ggsave("code/modeling/plots/coefficienti_random_3.pdf", plot = coef.m.rand3, width = 7, height = 5)
