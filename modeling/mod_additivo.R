### Additive model + mixed effects

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(mgcv)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
head(df)


# MODELLAZIONE -----------------------------------------------------------------

# risposta e covariate
features <- c("Regione", "Bandiera", "Tipo.Impianto", "Popolazione",
              "Altitudine", "Costa", "Urbanizzazione", "Vicini",
              "MoltoVicino", "Grande.Comune", "Isoletta")

features4 <- c("Bandiera", "Tipo.Impianto", "Popolazione",
               "Altitudine", "Costa", "Urbanizzazione", "Vicini",
               "MoltoVicino", "Grande.Comune", "Isoletta")

response <- "y"

df <- df %>% mutate(Provincia = as.factor(Provincia))


# modello additivo base --------------------------------------------------------
# interazione smooth tra Longitudine e Latitudine

tipovar <- sapply(df[,features], class)
quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
qual <- setdiff(features, quant)

form1 <- as.formula(paste0("y ~ ", 
                           paste0(qual, collapse = " + "), 
                           " + ",
                           paste0(quant, collapse = " + "),
                           " + s(Longitudine, Latitudine, bs = 'gp', k = 500, m = 2)"))
form1

system.time(m.gam1 <- bam(form1, family = gaussian, data = df,
              method = "fREML", discrete = T, gamma = 1.2, scale = -1, select = T))

#gam.check(m.gam1)
summary(m.gam1)
anova(m.gam1)
plot(m.gam1, seWithMean = TRUE, shade = 1, pages = 1)
vis.gam(m.gam1, view = c("Longitudine", "Latitudine"), plot.type = "contour", color = "cm", type = "response")
vis.gam(m.gam1, view = c("Longitudine", "Latitudine"), plot.type = "persp", color = "cm", type = "response", theta = -45, phi = 45)
#gratia::draw(m.gam1) # fa il ggplot ma e' molto lento..


# modello additivo  ------------------------------------------------------------
# modellazione Longitudine e Latitudine

tipovar <- sapply(df[,features4], class)
quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
qual <- setdiff(features4, quant)
form2 <- as.formula(paste0("y ~ ", 
                           paste0(qual, collapse = " + "), 
                           " + ",
                           paste0(quant, collapse = " + "),
                           " + s(Longitudine, Latitudine, bs = 'gp', k = 500, m = 2)"))
form2

system.time(m.gam2 <- bam(form2, family = gaussian, data = df,
              method = "fREML", discrete = T, gamma = 1.2, scale = -1, select = T))

#gam.check(m.gam2)
summary(m.gam2)
anova(m.gam2)
plot(m.gam2, seWithMean = TRUE, shade = 1, pages = 1)
vis.gam(m.gam2, view = c("Longitudine", "Latitudine"), plot.type = "contour", color = "cm", type = "response")
vis.gam(m.gam2, view = c("Longitudine", "Latitudine"), plot.type = "persp", color = "cm", type = "response", theta = -45, phi = 45)


# valutazione modelli ----------------------------------------------------------

AIC(m.gam1, m.gam2)
BIC(m.gam1, m.gam2)
anova(m.gam1, m.gam2)


# visualizzazione effetti lineari ----------------------------------------------

plot_coef <- function(m.gam){
  c <- m.gam$coefficients[1:min(which(substr(names(m.gam$coefficients), 0, 1) == "s"))-1]
  dati.gam <- data.frame(stima = c[-1])
  dati.gam$segno <- ifelse(dati.gam$stima > 0, "1", "-1")
  dati.gam$variabile <- names(c)[-1]
  dati.gam <- arrange(dati.gam, stima)
  dati.gam$variabile <- factor(dati.gam$variabile, levels = unique(dati.gam$variabile))
  
  coef.m.rand <- ggplot(dati.gam) +
    geom_bar(aes(x = variabile, y = stima, fill = segno), stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = c("coral", "seagreen1")) +
    geom_hline(yintercept = 0) +
    labs(x = "", y = "", title = "Modello additivo", subtitle = "Effetti fissi stimati") +
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

coef.m.gam1 <- plot_coef(m.gam1)
coef.m.gam2 <- plot_coef(m.gam2)

coef.m.gam1
coef.m.gam2


# salvataggio modelli stimati --------------------------------------------------

save(m.gam1, m.gam2, coef.m.gam1, coef.m.gam2, file = "code/modeling/models/additivi.RData")

ggsave("code/modeling/plots/coefficienti_additivo_1.pdf", plot = coef.m.gam1, width = 7, height = 5)
ggsave("code/modeling/plots/coefficienti_additivo_2.pdf", plot = coef.m.gam2, width = 7, height = 5)
