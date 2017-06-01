########
# ES 7 #
########


# Importare, visualizzare a capire la struttura del dataset
df15 <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00381/PRSA_data_2010.1.1-2014.12.31.csv")
View(df15)
str(df15)
summary(df15)
hist(df15$pm2.5)
boxplot(df15$pm2.5)

# Siamo soprattutto interessati alla colonna 
# pm2.5 che inizia subito con degli NA, 
# sostituiamoli come indicato dal test

# Faccio una copia del dataset, così da poter poi lavorare direttamente con df15

df15_backup <- df15

# Vediamo di definire la regola per sostituire i dati mancanti
# Consideriamo per semplicità la prima riga
df15[1,]
# Cerchiamo i dati per ora == 0 e Is e Ir == 0
head(df15[df15$hour == 0 & df15$Is == 0  & df15$Ir == 0,])

# Sostituisco allora i valori che sono NA all'ora 0 e in cui non piove con la media

head(df15[df15$hour == 0 & df15$Is == 0  & df15$Ir == 0 & is.na(df15$pm2.5),])

mean(df15[df15$hour == 0 & df15$Is == 0  & df15$Ir == 0, "pm2.5"], na.rm = T)

df15$pm2.5[df15$hour == 0 & df15$Is == 0  & df15$Ir == 0 & is.na(df15$pm2.5)] <- mean(df15[df15$hour == 0 & df15$Is == 0  & df15$Ir == 0, "pm2.5"], na.rm = T)

# con un semplice ciclo for possiamo ripetere il comando per tutte le ore
# Inoltre posso distinguere all'interno dello stesso, le 4 possibili condizioni meteo 

for (i in 0:23){
  df15$pm2.5[df15$hour == i & df15$Is == 0  & df15$Ir == 0 & is.na(df15$pm2.5)] <- mean(df15[df15$hour == i & df15$Is == 0  & df15$Ir == 0, "pm2.5"], na.rm = T)
  df15$pm2.5[df15$hour == i & df15$Is  > 0  & df15$Ir == 0 & is.na(df15$pm2.5)] <- mean(df15[df15$hour == i & df15$Is  > 0  & df15$Ir == 0, "pm2.5"], na.rm = T)
  df15$pm2.5[df15$hour == i & df15$Is == 0  & df15$Ir  > 0 & is.na(df15$pm2.5)] <- mean(df15[df15$hour == i & df15$Is == 0  & df15$Ir  > 0, "pm2.5"], na.rm = T)
  df15$pm2.5[df15$hour == i & df15$Is  > 0  & df15$Ir  > 0 & is.na(df15$pm2.5)] <- mean(df15[df15$hour == i & df15$Is  > 0  & df15$Ir  > 0, "pm2.5"], na.rm = T)
}

# verifico che non ci siano NA in pm2.5
any(is.na(df15$pm2.5)) 
# abbiamo rimosso tutti gli NA in questa colonna!

# Per visualizzare i dati potrebbe essere comodo combinare data e ora in una nuova colonna
# in un formato che R riconosce essere data e ora.
# posso adattare il comando visto per l'inquinamento a San Andreas
df15$DateTime <- as.POSIXct(paste(df15[, "year"],"-", df15[, "month"], "-" ,df15[, "day"], df15[, "hour"] )  , format = "%Y - %m - %d %H")
plot(df15$DateTime,df15$pm2.5, cex = .2, xlab = "Time", ylab = "PM 2.5 (ug/m3)", main = "Inquinamento a Pechino")
abline(h = 50, col = "red")

# Misurazioni che superano 5 volte il limite nel 2013
length(df15[ df15$year == "2013" & df15$pm2.5 >  50*5 , "pm2.5"   ])

# sono ben 1154 
# In un anno ci sono 24*365 misurazioni: 
# facendo il rapporto dei due numeri, scopriamo che il 13% delle misurazione super 
# di 5 volte la soglia massima!

# Aggiungo una colonna as.Date per proseguire con l'esercizio

df15$Date <- as.Date(paste(df15[, "year"],"-", df15[, "month"], "-" ,df15[, "day"])  , format = "%Y - %m - %d")
any(is.na(df15$Date)) #controllo che sia stato letto tutto bene

names(df15)


# Creo il nuovo dataset
NewData <- aggregate(df15$pm2.5 ~ df15$Date, FUN = mean)
NewData$max <- aggregate(df15$pm2.5 ~ df15$Date, FUN = max)[2]
NewData$Is <- aggregate(df15$Is ~ df15$Date, FUN = max)[2]
NewData$Ir <- aggregate(df15$Ir ~ df15$Date, FUN = max)[2]
NewData <- as.data.frame(NewData)
colnames(NewData) <- c("Date", "meanPM", "maxPM", "Is", "Ir") # non necessario

# trasformo le colonne Is e Ir in booleani: 1 se maggiori di 0, 0 altrimenti

NewData$Is <- ifelse(NewData$Is > 0, 1,0)
NewData$Ir <- ifelse(NewData$Ir > 0, 1,0)

par(mfrow = c(2,2), mar = c(2,2,2,2))
boxplot(NewData$meanPM[NewData$Is == 0 & NewData$Ir == 0], main = "no snow - no rain ")
boxplot(NewData$meanPM[NewData$Is == 1 & NewData$Ir == 0], main = "snow - no rain ")
boxplot(NewData$meanPM[NewData$Is == 0 & NewData$Ir == 1], main = "no snow -  rain ")
boxplot(NewData$meanPM[NewData$Is == 1 & NewData$Ir == 1], main = " snow & rain ")

par(mfrow = c(1,1), mar = c(3,3,3,3))
plot(NewData$Date[NewData$Is == 1], NewData$maxPM[NewData$Is == 1 ], xlab = "Time", ylab = "PM 2.5 (ug/m3)", main = "max PM2.5 - Neve")

length(NewData[NewData$meanPM > 50 & NewData$Ir == 1, "meanPM" ])
# nonostante la pioggia, in 5 anni, 301 giorni hanno superato la soglia di inquinamento.

########
# ES 8 #
########

# Se salviamo la funzione in un file separato chiamato my_fun.R, con il comando:
# source("fun.R")
# posso usarla poi nel workspace.
# my_fun(argomento1, argomento2, argomento3)


my_fun <- function(x,y, coeff){
  # calcolo i valori y hat :hy
  hy <- coeff[1] + x*coeff[2];
  #calcolo le differenze y_i - hy_i e y_i - mean(y)
  
  sum_hy <- sum((y - hy)^2);
  sum_mean <- sum((y - mean(y))^2);
  
  R <- 1 - sum_hy/sum_mean;
  
  return(R)
}

# Verifico che la funzione restituisca un valore simile a quello di summary()

lm_cars <- lm(mpg ~ hp, data = mtcars)
lm_cars_summary <- summary(lm_cars)
print(" I valori sono uguali?")
print(lm_cars_summary$r.squared == my_fun(mtcars$hp, mtcars$mpg, lm_cars$coefficients))

lm_cars_wt <- lm(mpg ~ wt, data = mtcars)
lm_cars_wt_summary <- summary(lm_cars_wt)
print(" I valori sono uguali?")
print(lm_cars_wt_summary$r.squared == my_fun(mtcars$wt, mtcars$mpg, lm_cars_wt$coefficients))
print("Di quanto differiscono?")
print(lm_cars_wt_summary$r.squared - my_fun(mtcars$wt, mtcars$mpg, lm_cars_wt$coefficients))
# Ok, sono praticamente uguali!