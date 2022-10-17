### create response for modeling

rm(list = ls())
set.seed(42)


# libraries --------------------------------------------------------------------

library(tidyverse)


# data reading -----------------------------------------------------------------

load("data/df_cleaned.RData")
str(df_cleaned)

# df_merge --> bigdf
# df_clean --> df_cleaned
# df_responses --> df


# obtain modeling data ---------------------------------------------------------

# model Prezzo on Settimana and macroeconomical variables and take residuals
# macroeconomical variables: crude oil, natural gas, eur over usd, eur over gbp 
oil_pre <- read.csv("data/CRUD.MI.csv", header = T)
oil <- data.frame(Settimana = 1:52, Prezzo.oil = oil_pre$Close[-1])

gas_pre <- read.csv("data/NGAS.MI.csv", header = T)
gas <- data.frame(Settimana = 1:52, Prezzo.gas = gas_pre$Close[-1])

eurusd_pre <- read.csv("data/EUR_USD.csv", header = T, dec = ",")
eurusd <- data.frame(Settimana = 1:52, EUR.USD = rev(eurusd_pre$Ultimo))

eurgdp_pre <- read.csv("data/EUR_GBP.csv", header = T, dec = ",")
eurgdp <- data.frame(Settimana = 1:52, EUR.GBP = rev(eurgdp_pre$Ultimo))

# obtain dataset for modeling
financial_info <- Reduce(function(x,y) merge(x, y, by = "Settimana"), list(oil, gas, eurusd, eurgdp))
d <- merge(df_cleaned %>% select(idImpianto, Settimana, Prezzo), financial_info, by = "Settimana") %>% 
  arrange(idImpianto, Settimana) %>% 
  as_tibble()
d

d %>% 
  group_by(Settimana) %>% 
  summarise(m = mean(Prezzo), low = quantile(Prezzo, 0.025), up = quantile(Prezzo, 0.975)) %>% 
  ggplot(aes(x = Settimana, y = m)) + 
  geom_ribbon(aes(ymin = low, ymax = up), alpha = .7, fill = "wheat") +
  geom_line(color = "orange") + 
  geom_point(color = "coral") +
  labs(x = "Settimana", y = "Livello medio", title = "Andamento del prezzo del gasolio") +
  theme(legend.position = "none")


# utils function ---------------------------------------------------------------

evaluate_response <- function(d, subt = NULL){
  
  serie_media <- d %>% 
    group_by(Settimana) %>% 
    summarise(m = mean(y), low = quantile(y, 0.025), up = quantile(y, 0.975)) %>% 
    ggplot(aes(x = Settimana, y = m)) + 
    geom_ribbon(aes(ymin = low, ymax = up), alpha = .7, fill = "wheat") +
    geom_line(color = "orange") + 
    geom_point(color = "coral") +
    coord_cartesian(ylim = c(-0.00025, 0.00025)) + 
    labs(x = "Settimana", y = "Livello medio", title = "Valutazione della risposta", subtitle = subt) +
    theme(legend.position = "none")
  
  tab2 <- d %>%
    group_by(idImpianto) %>% 
    summarise(Acf = acf(y, lag.max = 20, plot = F)$acf[-1],
              Pacf = pacf(y, lag.max = 20, plot = F)$acf)
  tab2$Lag <- rep(1:20, length(unique(d$idImpianto)))
  
  acf_media <- tab2 %>%
    group_by(Lag) %>% 
    summarise(myacf = mean(Acf),
              low = quantile(Acf, 0.025),
              up = quantile(Acf, 0.975)) %>%   
    ggplot(aes(x = Lag, y = myacf)) +
    geom_ribbon(aes(ymin = low, ymax = up), alpha = .7, fill = "wheat") +
    geom_line(color = "orange") + 
    geom_point(color = "coral") +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
    scale_x_continuous(breaks = 0:20) +
    labs(x = "lag", y = "ACF media", title = "Valutazione della risposta", subtitle = subt) +
    theme(legend.position = "none",
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14))
  
  pacf_media <- tab2 %>%
    group_by(Lag) %>%
    summarise(mypacf = mean(Pacf),
              low = quantile(Pacf, 0.025),
              up = quantile(Pacf, 0.975)) %>%
    ggplot(aes(x = Lag, y = mypacf)) +
    geom_ribbon(aes(ymin = low, ymax = up), alpha = .7, fill = "wheat") +
    geom_line(color = "orange") +
    geom_point(color = "coral") +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
    scale_x_continuous(breaks = 0:20) +
    labs(x = "lag", y = "PACF media") +
    theme(legend.position = "none")
    
  plot <- ggpubr::ggarrange(acf_media, pacf_media, ncol = 1)
  plot
}


# response 1 -------------------------------------------------------------------

# residuals from mean
mean_df <- d %>% group_by(Settimana) %>% summarise(Prezzo.medio.settimanale = mean(Prezzo))
d <- inner_join(d, mean_df, by = c("Settimana"))
d$y <- d$Prezzo - d$Prezzo.medio.settimanale
str(d)

evaluate_response(d, "Deviazioni dalla media settimanale")
d <- d %>% select(-y, -Prezzo.medio.settimanale)


# response 2 -------------------------------------------------------------------

# residuals from additive model
library(mgcv)
covs <- c("Prezzo.oil", "Prezzo.gas", "EUR.USD", "EUR.GBP")
str(d)

form <- as.formula(paste0("Prezzo ~ " , paste0("s(", covs, ", bs = 'cr', k = 15)", collapse = " + ")))
system.time(m.gam <- bam(form, data = d, method = "fREML", discrete = T))
#system.time(m.gam <- bam(Prezzo ~ s(Settimana, bs = "cr", k = 13), data = d, method = "fREML", discrete = T))
#system.time(m.gam <- bam(Prezzo ~ s(Settimana, by = idImpianto, bs = "cr", k = 13), data = dd, method = "fREML", discrete = T))

summary(m.gam)
anova(m.gam)
#plot(m.gam, seWithMean = TRUE, shade = 1, pages = 1) # useful for interpretation

d$y <- m.gam$residuals
evaluate_response(d, "Residui GAM")
d <- d %>% select(-y)


# response 3 -------------------------------------------------------------------

# residuals from boosting
library(lightgbm)

dtrain <- lgb.Dataset(data = as.matrix(d %>% select(-Prezzo, -idImpianto, -Settimana)), label = d$Prezzo)

params <- list(objective = "regression",
               metric = "rmse",
               feature_fraction = 1,
               bagging_fraction = 1,
               bagging_freq = 5,
               max_depth = 5,
               num_leaves = 25,
               learning_rate = 0.025, 
               num_threads = 4,
               verbose = -1,
               seed = 42)

lgb.model.cv = lgb.cv(params = params,
                      data = dtrain,
                      nrounds = 7000,
                      early_stopping_rounds = 50,
                      eval_freq = 20,
                      eval = "rmse",
                      nfold = 4)

best.iter <- lgb.model.cv$best_iter
#best.iter <- 445

lgb.model <- lgb.train(params = params,
                       data = dtrain,
                       nrounds = best.iter)

d$fitted <- predict(lgb.model, as.matrix(d %>% select(-Prezzo, -idImpianto, -Settimana)))
d$y <- d$Prezzo - d$fitted
evaluate_response(d, "Residui GBM") 
# d <- d %>% select(-y)


# final dataframe --------------------------------------------------------------

df <- d %>% 
  select(idImpianto, Settimana, y, fitted) %>% 
  merge(df_cleaned, by = c("idImpianto", "Settimana")) %>% 
  arrange(idImpianto, Settimana) %>% as_tibble()

df
str(df)
summary(df)

plot_y <- evaluate_response(df)
plot_y


# save results -----------------------------------------------------------------

ggsave("code/eda/plots/valutazione_risposta.pdf", plot = plot_y, width = 7, height = 5)

write.csv2(df, file = "data/df.csv")
save(df, file = "data/df.RData")
