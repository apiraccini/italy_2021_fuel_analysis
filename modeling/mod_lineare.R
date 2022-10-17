### Linear models

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
df


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

response <- "y"

# gestione covariate
df$Regione <- as.factor(df$Regione)
tipovar <- sapply(df[,features], class)
tipovar

quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
qual <- setdiff(features, quant)

sapply(df[,qual], table) # occhio a isoletta
# df[,quant] <- scale(df[,quant])
# df[,qual] <- lapply(df[,qual], factor)

str(df[,c(features, response)])


# modello lineare 1 (benchmark) ------------------------------------------------

system.time(m.lin1 <- lm(y ~ ., data = df[,c(features, response)]))

summary(m.lin1)
anova(m.lin1)
confint(m.lin1)


# modello lineare 2 ------------------------------------------------------------

system.time(m.lin2 <- lm(y ~ ., data = df[,c(features2, response)]))

summary(m.lin2)
anova(m.lin2)
confint(m.lin2)


# visualizzazione coefficienti -------------------------------------------------

plot_coef <- function(m.lin){
  dati.lin <- data.frame(coef(m.lin)[-1], confint(m.lin)[-1,])
  colnames(dati.lin) <- c("stima", "lower", "upper")
  dati.lin$segno <- ifelse(dati.lin$stima > 0, "1", "-1")
  dati.lin$variabile <- names(m.lin$coefficients[-1])
  dati.lin <- arrange(dati.lin, stima)
  dati.lin$variabile <- factor(dati.lin$variabile, levels = unique(dati.lin$variabile))
  
  coef.m.lin <- ggplot(dati.lin) +
    geom_bar(aes(x = variabile, y = stima, fill = segno), stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = c("coral", "seagreen1")) +
    geom_errorbar(aes(x = variabile, ymin = lower, ymax = upper),
                  width = 0.4, colour = "black", alpha = 01, size = 0.5) +
    geom_hline(yintercept = 0) +
    labs(x = "", y = "", title = "Modello lineare", subtitle = "Coefficienti stimati") +
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
  
  coef.m.lin
}

coef.m.lin1 <- plot_coef(m.lin1)
coef.m.lin2 <- plot_coef(m.lin2)

coef.m.lin1
coef.m.lin2


# salvataggio modelli e grafici ------------------------------------------------

save(m.lin1, m.lin2, coef.m.lin1, coef.m.lin2,  file = "code/modeling/models/lineare.RData")

ggsave("code/modeling/plots/coefficienti_lineare_1.pdf", plot = coef.m.lin1, width = 7, height = 5)
ggsave("code/modeling/plots/coefficienti_lineare_2.pdf", plot = coef.m.lin2, width = 7, height = 5)
