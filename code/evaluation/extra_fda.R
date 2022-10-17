### extra - functional analysis

rm(list = ls())
set.seed(42)
opar <- par()


# libraries --------------------------------------------------------------------

library(fda)
library(refund)
library(tidyverse)


# read data --------------------------------------------------------------------

load("data/df.RData")

str(df)
df <- as_tibble(df)


# functional analysis ----------------------------------------------------------

# campiono 3000 impianti
set.seed(42)
ids <- sample(unique(df$idImpianto), 3000, F)

# ottengo dati sul prezzo in formato matriciale sett*impianto, convariate relative
# a bandiera e regione
prezzo <- df %>% 
  filter(idImpianto %in% ids) %>% 
  select(idImpianto, Regione, Bandiera, Settimana, Prezzo) %>%
  arrange(Settimana) %>% 
  pivot_wider(names_from = "Settimana", values_from = "Prezzo")
data <- covmat <- prezzo %>% select(idImpianto, Regione, Bandiera) %>% as.data.frame()
y <- prezzo %>% select(-idImpianto, -Regione, -Bandiera) %>% as.matrix()

ids <- data$idImpianto
prezzo <- t(y)
settimana <- 1:52

# plotting function
ggmatplot <- function(d, col = "coral", tit = NULL){
  d <- reshape2::melt(d, id="x")
  p <- ggplot(d, aes_string(x="Var1", y="value", group="Var2")) +
    geom_line(colour = col, alpha = 0.5) +
    labs(x = "Settimana", y = "", title = tit) +
    theme(legend.position = "none")
  p
}
#ggmatplot(prezzo)

# funzioni base
order <- 6
nbasis <- length(settimana) + order - 2
basis_prezzo <- create.bspline.basis(range(settimana), nbasis, order, settimana)

# penalita per il lisciamento
Lfd.degree <- 4
fdPar <- fdPar(basis_prezzo, Lfd.degree, lambda = 5)

# lisciamento e storage oggetto fd
prezzo_fd <- smooth.basis(settimana, prezzo, fdPar)$fd
plot(prezzo_fd)
prezzo_smooth <- eval.fd(settimana, prezzo_fd)
data$prezzo_smooth <- t(prezzo_smooth)

# grafici
par(mfrow = c(2, 1), mar = rep(2, 4))
prezzo %>% matplot(type = "l", main = "Dati originali")
prezzo_smooth %>% matplot(type = "l", main = "Dati lisciati tramite B-splines")
par(opar)

prezzo_smooth %>% matplot(type = "l", col = "lightgrey", main = "Andamento del prezzo del gasolio nel 2021", xlab = "Settimana", ylab = "")
lines(apply(prezzo_smooth, 1, mean), col = 2, lwd = 2)

# derivata prima e seconda
D1 <- int2Lfd(1)
D2 <- int2Lfd(2)
prezzo_d1 <- deriv.fd(prezzo_fd, D1) %>% eval.fd(settimana, .)
prezzo_d2 <- deriv.fd(prezzo_fd, D2) %>% eval.fd(settimana, .)
par(mfrow = c(2, 1), mar = rep(2,4))
matplot(prezzo_d1, type = "l", col = "lightgrey", main = "Velocità (D1)", xlab = "Settimana", ylab = "") # derivata prima (crescita)
lines(apply(prezzo_d1, 1, mean), col = 2, lwd = 2)
abline(h = 0, lty = 2)
matplot(prezzo_d2, type = "l", col = "lightgrey", main = "Accelerazione (D2)", xlab = "Settimana", ylab = "") # derivata prima (crescita)
lines(apply(prezzo_d2, 1, mean), col = 2, lwd = 2)
par(opar)

# boxplot
fbp <- boxplot(prezzo_fd)
plot(fbp$depth)

# covarianza
cov.h <- var.fd(prezzo_fd)
str(cov.h) 

grid <- seq(1, 52, len = 100)
varcov.h <- eval.bifd(grid, grid, cov.h)
persp(grid, grid, varcov.h, theta = -45, phi = 25, r = 3, expand = 0.5, ticktype = "detailed",
      xlab = "Tempo", ylab = "Tempo", zlab = "Covarianza prezzi")
contour(grid, grid, varcov.h, xlab = "Tempo", ylab = "Tempo", main = "Covarianza prezzi")
image(grid, grid, varcov.h, xlab = "Tempo", ylab = "Tempo", main = "Covarianza prezzi")

# f-PCA (chiamate armoniche --> definiscono la miglior base per rappresentare i dati)
ncomp <- 4
prezzo_pca <- pca.fd(prezzo_fd, ncomp)

print(prezzo_pca)$harmonics
str(prezzo_pca)

par(mfrow = c(2,2), mar = rep(2, 4))
plot(prezzo_pca, cex.main = 0.9)
par(opar)

plot(prezzo_pca$harmonics)

# bande di confidenza simultanee per la media
# library(SCBmeanfd)
# set.seed(42)
# h <- cv.select(settimana, t(prezzo_smooth), degree = 1, interval = c(0.2, 1), gridsize = 100)
# scby <- scb.mean(settimana, t(prezzo_smooth), bandwidth = h, scbtype = "both", gridsize = 100)
# plot(scby)

# NB!!!! usare fd2funData e autoplot (ha metodo per funData)

# modeling ---------------------------------------------------------------------

str(data)
data$Regione <- factor(data$Regione)

# m1
m <- fosr(prezzo_smooth ~ Bandiera, method = "OLS", data = data)
plot(m, split = 1, ylabel = "", titles = levels(data$Bandiera))
par(opar)
rsq <- 1- colSums((y - m$yhat)^2)/colSums((y - colMeans(y))^2)
plot(rsq, type = "l", col = "coral", lwd = 3, main = paste0("Functional R2 (mean: ",  round(mean(rsq), 4), ")"))

matplot(prezzo_smooth, type = "l", col = "light grey")
matlines(t(m$yhat), type = "l", lty = 1)
matplot(t(m$yhat), type = "l", lty = 1)

# m2
m2 <- fosr(prezzo_smooth ~ Regione, method = "OLS", data = data)
plot(m2, split = 1, ylabel = "", titles = levels(data$Regione))
par(opar)
rsq <- 1- colSums((y - m2$yhat)^2)/colSums((y - colMeans(y))^2)
plot(rsq, type = "l", col = "coral", lwd = 3, main = paste0("Functional R2 (mean: ",  round(mean(rsq), 4), ")"))

matplot(settimana, prezzo_smooth, type = "l", col = "light grey")
matlines(settimana, t(m2$yhat), type = "l", lty = 1)
matplot(settimana, t(m2$yhat), type = "l", lty = 1)

# m3
m3 <- fosr(prezzo_smooth ~ Bandiera + Regione, method = "OLS", data = data)
plot(m3, split = 1, ylabel = "", titles = c(levels(data$Bandiera), levels(data$Regione)))
layout(1)
rsq <- 1- colSums((y - m3$yhat)^2)/colSums((y - colMeans(y))^2)
plot(rsq, type = "l", col = "coral", lwd = 3, main = paste0("Functional R2 (mean: ",  round(mean(rsq), 4), ")"))

matplot(prezzo_smooth, type = "l", col = "light grey")
matlines(t(m3$yhat), type = "l", lty = 1)
matplot(t(m3$yhat), type = "l", lty = 1)
