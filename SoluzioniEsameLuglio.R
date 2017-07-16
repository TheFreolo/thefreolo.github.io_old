###################
##   Soluzione   ##
## VAN WYCK BLVD ##
###################

df3 <- df1[df1$station == 'VAN WYCK BLVD',]
View(df3)
str(df3)

summary(df3)

#Sono presenti NA per ENTRIESn_hourly e EXITSn_hourly
# sostituiamoli come richiesto

NAIndex <- which(is.na(df3$ENTRIESn_hourly))

df3$ENTRIESn_hourly[NAIndex] <- mean(c(df3$ENTRIESn_hourly[NAIndex-1], df3$ENTRIESn_hourly[NAIndex+1]))

NAIndex2 <- which(is.na(df3$EXITSn_hourly))

df3$EXITSn_hourly[NAIndex2] <- mean(c(df3$EXITSn_hourly[NAIndex2-1], df3$EXITSn_hourly[NAIndex2+1]))

# Verifico la rimozione
summary(df3[, c("EXITSn_hourly", "ENTRIESn_hourly")])

#ok, possiamo procedere con le altre richieste

quantile(df3$ENTRIESn_hourly, seq(0,1,.1))
mean(df3$ENTRIESn_hourly)
median(df3$ENTRIESn_hourly)

quantile(df3$EXITSn_hourly, seq(0,1,.1))
mean(df3$EXITSn_hourly)
median(df3$EXITSn_hourly)

# Boxplot

par(mfrow = c(2,1), mar = c(2,2,1,1))
boxplot(ENTRIESn_hourly ~ day_week, data = df3, main = 'Entrate orarie')
boxplot(EXITSn_hourly ~ day_week, data = df3, main = 'Uscite orarie' )

# In generale sembra che le entrate abbiano una variabilità maggiore e possano 
# raggiungere numeri più grandi.
# Il giorno 2 sembra essere quello che in media ha più entrate, mentre il giorno 4 più uscite. 

table(df3[,c('day_week', 'rain')])
# il giorno con più pioggia è il giorno 2 (parimerito con il giorno 0)
# Scelgo il giorno 2

par(mfrow = c(2,1), mar = c(2,2,1,1))
hist(df3[ (df3$day_week == 2 & df3$rain == 1),  "ENTRIESn_hourly"], main = 'Entrate orarie - pioggia', breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000), freq = F)
hist(df3[ (df3$day_week == 2 & df3$rain == 0),  "ENTRIESn_hourly"], main = 'Entrate orarie - no pioggia', breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000,3500, 4000), freq = F)

# Ad un'analisi visiva, risulta che in caso di pioggia ci sia tendenzialmente un numero maggiore di ingressi.


my_fun <- function(x,a){
  n <- length(x)
  my_mean = (1/n)*sum(x)
  my_var = (1/(n-1))*sum(x^2)-(n/(n-1))*my_mean^2
  return (c(my_mean-qt((1+a)/2, df=n-1)*sqrt(1/n*my_var),my_mean+qt((1+a)/2,df=n-1)*sqrt(1/n*my_var)))
}

#Confronto con una funzione (e dati) di libreria:

my_fun(mtcars$mpg, .95)

t.test(mtcars$mpg)

#I risultati coincidono

## Federico Reali 