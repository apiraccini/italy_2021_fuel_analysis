### Gradient Boosting (lighGBM)

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(lightgbm)


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
#features <- features2

response <- "y"

# gestione covariate
df <- df %>% mutate(Regione = as.factor(Regione),
                    Vicini = as.double(Vicini))
tipovar <- sapply(df[,features], class)
tipovar

quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
qual <- setdiff(features, quant)

sapply(df[,qual], table)
df[,qual] <- lapply(df[,qual], function(x) as.numeric(as.factor(x)))

str(df[,c(features, response)])


# gradient boosting ------------------------------------------------------------

set.seed(42)
intrain <- sample(1:NROW(df), 0.8*NROW(df))
invalid <- setdiff(1:NROW(df), intrain)
dtrain <- lgb.Dataset(data = as.matrix(df[intrain,features]), 
                      label = df$y[intrain],
                      categorical_feature = qual)
dval <- lgb.Dataset(data = as.matrix(df[invalid,features]), 
                      label = df$y[invalid],
                      categorical_feature = qual)

params <- list(objective = "regression",
               metric = "rmse",
               feature_fraction = 1,
               bagging_fraction = 0.9,
               bagging_freq = 5,
               max_depth = 5,
               num_leaves = 50,
               learning_rate = 0.3, 
               num_threads = 4,
               seed = 42)

system.time(m.lgb <- lgb.train(params = params, 
                      data = dtrain,
                      nrounds = 7000,
                      valids = list(valid = dval),  
                      early_stopping_rounds = 5,
                      eval_freq = 25, 
                      eval = "rmse",
                      verbose = 1)) # ~ 5 min


# variable importance ----------------------------------------------------------

system.time(m.imp <- lgb.importance(m.lgb))
str(m.imp)

plot_importance <- function(m.imp){
  d <- m.imp[,1:2]
  plot <- d %>%
    mutate(Feature = as.factor(Feature)) %>% 
    mutate(Feature = fct_reorder(Feature, Gain)) %>%
    ggplot( aes(x=Feature, y=Gain, fill="coral")) +
    geom_bar(stat="identity", alpha=.6, width=.4) +
    labs(title = "Gradient boosting", subtitle = "Importanza relativa delle variabili")+
    coord_flip() + xlab("") + ylab("") +
    theme(legend.position = "none",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14))
  
  plot
}

imp.lgb <- plot_importance(m.imp)
imp.lgb


# salvataggio modelli e grafici ------------------------------------------------

save(m.lgb, imp.lgb,  file = "code/modeling/models/lgb.RData")

ggsave("code/modeling/plots/var_importance_boosting.pdf", plot = imp.lgb, width = 7, height = 5)
