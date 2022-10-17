### Summary of modeling results

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(lme4)
library(mgcv)
library(lightgbm)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
df <- as_tibble(df)
df


# load models ------------------------------------------------------------------

load("code/modeling/models/lineare.RData")
load("code/modeling/models/random.RData")
load("code/modeling/models/additivi.RData")
load("code/modeling/models/lgb.RData")


# model scores -----------------------------------------------------------------

load("code/modeling/models/scores.RData")
ls()

knitr::kable(t(scores), digits = 5)


# gradient boosting ------------------------------------------------------------

# a livello previsivo il modello migliore è il boosting, ma per privilegiare 
# l'interpretazione si considerano i risultati degli altri modelli, utilizzando 
# il variable importance plot come supplemento all'interpretazione
plot(imp.lgb)


# modelli lineari --------------------------------------------------------------

formula(m.lin1)
formula(m.lin2)
# l'inserimento della covariate relativa alla Regione migliora le prestazioni del
# modello rispetto alla modellazione con covariate alternative ausiliarie per 
# caratterizzare la regione
summary(m.lin2)
anova(m.lin2)

plot(coef.m.lin1) # focus su bandiera, regione
plot(coef.m.lin2) # focus su bandiera


# modelli ad effetti misti -----------------------------------------------------

formula(m.ran1); cci1
formula(m.ran2); cci2
formula(m.ran3); cci3
# m.ran2, che utlizza (1|Regione) fa come m.lin1 che utilizza la covariata come
# effetto fisso
# m.ran1 e m.ran3 hanno prestazioni equivalenti e migliori dei modelli precedenti
# sono anche sostanzialmente equivalenti
# il modello più interessante sembra essere m.ran1 (stima l'effetto fisso su Regione)
summary(m.ran1)
anova(m.ran1)

plot(coef.m.rand1)


# modelli additivi -------------------------------------------------------------

formula(m.gam1)
formula(m.gam2)
# i modelli sono simili e ottengono adattamento e capacità predittiva nettamente 
# migliori degli altri
# tra i due si favorisce il secondo (l'aggiunta di regione non migliora il modello
# a livello predittivo, l'effetto spaziale viene modellato sufficientemente bene
# dal secondo modello)

summary(m.gam1)
anova(m.gam1)
plot(coef.m.gam1)
plot(m.gam1, seWithMean = TRUE, shade = 1, pages = 1)
vis.gam(m.gam1, view = c("Longitudine", "Latitudine"), plot.type = "contour", color = "cm", type = "response")
vis.gam(m.gam1, view = c("Longitudine", "Latitudine"), plot.type = "persp", color = "cm", type = "response", theta = 45, phi = 45)

plot(coef.m.gam2)
plot(m.gam2, seWithMean = TRUE, shade = 1, pages = 1)
vis.gam(m.gam2, view = c("Longitudine", "Latitudine"), plot.type = "contour", color = "cm", type = "response")
vis.gam(m.gam2, view = c("Longitudine", "Latitudine"), plot.type = "persp", color = "cm", type = "response", theta = 45, phi = 45)


# playaround -------------------------------------------------------------------

plot_coef <- function(m.ran){
  dati.rand <- data.frame(stima = fixef(m.ran)[-1])
  dati.rand$segno <- ifelse(dati.rand$stima > 0, "1", "-1")
  dati.rand$variabile <- names(summary(m.ran)$coefficients[-1,1])
  dati.rand <- dati.rand[grepl("Regione", dati.rand$variabile),]
  dati.rand$variabile <- substr(dati.rand$variabile, 8, 50)
  dati.rand <- arrange(dati.rand, stima)
  dati.rand$variabile <- factor(dati.rand$variabile, levels = unique(dati.rand$variabile))
  
  coef.m.rand <- ggplot(dati.rand) +
    geom_bar(aes(x = variabile, y = stima, fill = segno), stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = c("coral", "seagreen1")) +
    geom_hline(yintercept = 0) +
    labs(x = "", y = "", title = "Modello ad effetti misti", subtitle = "Effetti fissi stimati - variabile Regione") +
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
pp <- plot_coef(m.ran1)
pp

ggsave("code/modeling/plots/coefficienti_random_1_previsto.pdf", plot = pp, width = 7, height = 5)
