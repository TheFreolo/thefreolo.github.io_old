df <- read.csv( "./data/sleep.csv" , header = T)
View(df)

# 1
# Usando le informazioni contenute nel file di descrizione, 
# si determini se il dataset contiene errori nei dati.
# In caso si sostituiscano tali dati con NA.

df1 <- df

df1[df1 == -999.00] <- NA

View(df1)

# 2
# Per ogni variabile, si sostituiscano gli NA con la mediana della stessa 
# variabile.

summary(df1)

## Ci sono NA nelle colonne da 4 a 8 

colnames(df1)
df2 <- df1

colToNARM <- seq(4,8)

for (j in colToNARM) {
  IndexNA <- which(is.na(df2[, j]))
  df2[IndexNA, j ] = median(df2[, j ], na.rm = T)
}
summary(df2)
View(df2)

# 3
# Si calcolino Media, deviazione standard e decili delle variabili 
# BodyWt, BrainWt, TotSleep, GestTime.

mean(df2[, "BodyWt"])
mean(df2[, "BrainWt"])
mean(df2[, "TotSleep"])
mean(df2[, "GestTime"])

sd(df2[, "BodyWt"])
sd(df2[, "BrainWt"])
sd(df2[, "TotSleep"])
sd(df2[, "GestTime"])

quantile(df2[, "BodyWt"], seq(0,1,0.1))
quantile(df2[, "BrainWt"], seq(0,1,0.1))
quantile(df2[, "TotSleep"], seq(0,1,0.1))
quantile(df2[, "GestTime"], seq(0,1,0.1))

# 4
# Si visualizzi la distribuzione delle variabili "BodyWt" e "TotSleep" 
# usando gli istogrammi ed i boxplot.

par(mfrow= c(2,2), mar = c(2,2,2,1))
hist(df2[, "BodyWt"], main = "Body Wt")
boxplot(df2[, "BodyWt"], main = "Body Wt")
hist(df2[, "TotSleep"], main = "Total Sleep")
boxplot(df2[, "TotSleep"], main = "Total Sleep")

# Dopo aver visualizzato la variabile Body Wt, si provi una trasformazione 
# dei dati che possa migliorarne la visualizzazione. Giustificare tale scelta.

## Una naturale trasformazione e' quella che si ottiene considerando
## il logaritmo in base 10, in questo modo si tiene conto dell'ordine 
## dell'unita' di misura. Si salvino i dati trasformati nella colonna 
## "BodyWtTRANS".

df2[,"BodyWtTRANS"] <- log10(df2[,"BodyWt"])
par(mfrow= c(2,2), mar = c(2,2,2,1))
hist(df2[, "BodyWtTRANS"], main = "Body Wt (log10)")
boxplot(df2[, "BodyWtTRANS"], main = "Body Wt (log10)")
hist(df2[, "TotSleep"], main = "Total Sleep")
boxplot(df2[, "TotSleep"], main = "Total Sleep")

# 5
# Si valuti un modello di regressione lineare tra li variabili "TotSleep" 
# e "BodyWtTRANS". Si visualizzino i dati, la retta di regressione, 
# la distribuzione dei residui e si confrontino con i quantili della normale. 
# Si commentino i risultati grafici e il valore di R quadro.

par(mfrow= c(1,1), mar = c(2,2,2,1))
plot(TotSleep ~ BodyWtTRANS, data = df2)
regr <- lm(TotSleep ~ BodyWtTRANS, data = df2)
summary(regr)

par(mfrow= c(3,1), mar = c(2,2,2,1))
plot(TotSleep ~ BodyWtTRANS, data = df2)
abline(regr$coefficients, col = "red")
hist(regr$residuals)
qqnorm(regr$residuals)
qqline(regr$residuals)
