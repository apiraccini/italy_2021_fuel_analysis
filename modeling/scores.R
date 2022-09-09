### Scores (train-test)

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(lme4)
library(mgcv)
library(lightgbm)


# utils ------------------------------------------------------------------------

proj_dir <- "~/R_Jobs/StatAziende"
if (normalizePath(getwd()) != normalizePath(proj_dir)) setwd(proj_dir)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
df


# MODELING ---------------------------------------------------------------------

# modeling generics ------------------------------------------------------------

# funzione per valutazione modelli (train-test)
evaluate_model <- function(df, pred, set = NULL, test.prop = 0.5){
  
  if(is.null(set)){
    set.seed(42)
    set <- sample(c("train", "test"), NROW(df), replace = T, prob = c(1-test.prop, test.prop))
  }
  
  y <- df$y[set == "test"]
  yhat <- pred(dftrain = df[set == "train",], dftest = df[set == "test",])
  
  mae <- mean(abs(y - yhat))
  rmse <- sqrt(mean((y - yhat)^2))
  r2 <- 1- sum((y-yhat)^2)/sum((y-mean(y))^2)
  
  return(t(data.frame(MAE = mae, RMSE = rmse, R2 = r2)))
}

# train-test split
set.seed(42)
test.prop <- 0.5
set <- sample(c("train", "test"), NROW(df), replace = T, prob = c(1-test.prop, test.prop))

# risposta e covariate
# lineare, random1 (+ |Provincia), additivo1 (+ s(Longitudine, Latitudine))
features <- c("Regione", "Bandiera", "Tipo.Impianto", "Popolazione",
              "Altitudine", "Costa", "Urbanizzazione", "Vicini",
              "MoltoVicino", "Grande.Comune", "Isoletta")

# lineare2
features2 <- c("Bandiera", "Tipo.Impianto", "Popolazione",
               "Altitudine", "Costa", "Urbanizzazione", "Vicini",
               "MoltoVicino", "Grande.Comune", "Isoletta", 
               "Statuto.Speciale", "Zona", "Perc.Inquinamento.Regione",
               "Perc.Mezzi.Lavoro.Regione", "Perc.Mezzi.Regione", "log.PIL.Regione")

# random2 (+1|Regione), random3 (+1|Regione/Provincia), additivo2 (+ s(Longitudine, Latitudine)
features3 <- c("Bandiera", "Tipo.Impianto", "Popolazione",
               "Altitudine", "Costa", "Urbanizzazione", "Vicini",
               "MoltoVicino", "Grande.Comune", "Isoletta")

response <- "y"

df <- df %>% mutate(Provincia = as.factor(Provincia),
                    Vicini = as.double(Vicini))


# modello lineare 1 (benchmark) ------------------------------------------------

pred.lin <- function(dftrain, dftest){
  form.lin1 <- as.formula(paste0("y ~ ", paste0(features, collapse = " + ")))
  predict(lm(form.lin1, data = dftrain), newdata = dftest)
}

scores <- data.frame(Lineare1 = evaluate_model(df, pred.lin, set = set))
t(scores)


# modello lineare 2 ------------------------------------------------------------

pred.lin2 <- function(dftrain, dftest){
  form.lin2 <- as.formula(paste0("y ~ ", paste0(features2, collapse = " + ")))
  predict(lm(form.lin2, data = dftrain), newdata = dftest)
}

scores$Lineare2 <- evaluate_model(df, pred.lin2, set = set)
t(scores)


# modello lineare ad effetti casuali -------------------------------------------
# (Regione fisso, Provincia casuale)

pred.ran1 <- function(dftrain, dftest){
  form.rand1 <- as.formula(paste0("y ~ ",paste0(features, collapse = " + "), " + (1|Provincia)"))
  predict(lmer(form.rand1, data = dftrain), newdata = dftest, allow.new.levels = T)
}

scores$Random1 <- evaluate_model(df, pred.ran1, set = set)
t(scores)


# modello lineare ad effetti casuali -------------------------------------------
# (Regione casuale) 

pred.ran2 <- function(dftrain, dftest){
  form.rand2 <- as.formula(paste0("y ~ ", paste0(features3, collapse = " + "), " + (1|Regione)"))
  predict(lmer(form.rand2, data = dftrain), newdata = dftest, allow.new.levels = T)
}

scores$Random2 <- evaluate_model(df, pred.ran2, set = set)
t(scores)


# modello lineare ad effetti casuali -------------------------------------------
# (Regione/Provincia casuale) 

pred.ran3 <- function(dftrain, dftest){
  form.rand3 <- as.formula(paste0("y ~ ",paste0(features3, collapse = " + "), " + (1|Regione/Provincia)"))
  predict(lmer(form.rand3, data = dftrain), newdata = dftest, allow.new.levels = T)
}

scores$Random3 <- evaluate_model(df, pred.ran3, set = set)
t(scores)


# modello additivo base --------------------------------------------------------
# interazione smooth tra Latitudine e Longitudine + effetto fisso regione

pred.gam1 <- function(dftrain, dftest){
  tipovar <- sapply(df[,features], class)
  quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
  qual <- setdiff(features, quant)
  form.gam1 <- as.formula(paste0("y ~ ", 
                             paste0(qual, collapse = " + "), 
                             " + ",
                             paste0(quant, collapse = " + "),
                             " + s(Longitudine, Latitudine, bs = 'gp', k = 500, m = 2)"))
  predict(bam(form.gam1, family = gaussian, data = dftrain,
              method = "fREML", discrete = T, gamma = 1.2, scale = -1, select = T),
          newdata = dftest)
}

scores$Additivo1 <- evaluate_model(df, pred.gam1, set = set)
t(scores)


# modello additivo -------------------------------------------------------------
# interazione smooth tra Latitudine e Longitudine

pred.gam2 <- function(dftrain, dftest){
  tipovar <- sapply(df[,features3], class)
  quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
  qual <- setdiff(features3, quant)
  form.gam2 <- as.formula(paste0("y ~ ", 
                             paste0(qual, collapse = " + "), 
                             " + ",
                             paste0(quant, collapse = " + "),
                             " + s(Longitudine, Latitudine, bs = 'gp', k = 500, m = 2)"))
  predict(bam(form.gam2, family = gaussian, data = dftrain,
              method = "fREML", discrete = T, gamma = 1.2, scale = -1, select = T),
          newdata = dftest)
}

scores$Additivo2 <- evaluate_model(df, pred.gam2, set = set)
t(scores)


# gradient boosting -------------------------------------------------------

pred.lgb <- function(dftrain, dftest){
  tipovar <- sapply(df[,features], class)
  quant <- names(tipovar[tipovar %in% c("numeric", "integer")])
  qual <- setdiff(features, quant)
  set.seed(42)
  intrain <- sample(1:NROW(dftrain), 0.8*NROW(dftrain))
  invalid <- setdiff(1:NROW(dftrain), intrain)
  dtrain <- lgb.Dataset(data = sapply(dftrain[intrain,features], as.numeric), 
                        label = dftrain$y[intrain],
                        categorical_feature = qual)
  dval <- lgb.Dataset(data = sapply(dftrain[invalid,features], as.numeric), 
                      label = dftrain$y[invalid],
                      categorical_feature = qual)
  params <- list(objective = "regression",
                 metric = "rmse",
                 feature_fraction = 1,
                 bagging_fraction = 0.9,
                 bagging_freq = 5,
                 max_depth = 5,
                 num_leaves = 50,
                 learning_rate = 0.1, 
                 num_threads = 4,
                 seed = 42)
  m.lgb <- lgb.train(params = params, 
                     data = dtrain,
                     nrounds = 7000,
                     valids = list(valid = dval),  
                     early_stopping_rounds = 5,
                     eval_freq = 25, 
                     eval = "rmse",
                     verbose = -1)
  predict(m.lgb, data = sapply(dftest[,features], as.numeric))
}

scores$Boosting <- evaluate_model(df, pred.lgb, set = set)
t(scores)


# tabella errori ---------------------------------------------------------------

knitr::kable(t(scores), digits = 5)
#  |          |     MAE|    RMSE|      R2|
#  |:---------|-------:|-------:|-------:|
#  |Lineare1  | 0.03055| 0.04959| 0.37827|
#  |Lineare2  | 0.03195| 0.05041| 0.35755|
#  |Random1   | 0.03032| 0.04915| 0.38937|
#  |Random2   | 0.03055| 0.04959| 0.37827|
#  |Random3   | 0.03032| 0.04915| 0.38937|
#  |Additivo1 | 0.02989| 0.04795| 0.41875|
#  |Additivo2 | 0.02994| 0.04800| 0.41760|
#  |Boosting  | 0.02531| 0.04029| 0.58962|


# salvataggio risultati --------------------------------------------------------

save(scores, file = "code/modeling/models/scores.RData")
