# Esercizio 1

df <- read.table( url("https://www.dropbox.com/s/nntvi0mspzjrqgv/shares.tsv?dl=1") , header = T , sep= "\t")

View(df)

#1 
df1 <- df
df1$Index <- as.Date(df1$Index)
df2 <- df1[df1$Index < "1998-05-13" & df1$Index > "1991-01-01",]

#2
summary(df2)
# Sono presenti diversi NA nelle varie colonne, 
# li seleziono e rimuovo uno per volta

## AAPL
j <- which(is.na(df2$AAPL))
df2$AAPL[j] <- mean(df2$AAPL[c(j-1,j+1) ])
any(is.na(df2$AAPL)) #rimosso tutti gli NA

## MSFT
k <- which(is.na(df2$MSFT))
df2$MSFT[k] <- mean(df2$MSFT[c(k-1,k+1) ])
any(is.na(df2$MSFT)) #rimosso tutti gli NA

## IBM
which(is.na(df2$IBM)) # in questo caso ce ne sono 2 non consecutivi
l <- which(is.na(df2$IBM))
df2$IBM[l[1]] <- mean(df2$IBM[c(l[1]-1,l[1]+1) ])
df2$IBM[l[2]] <- mean(df2$IBM[c(l[2]-1,l[2]+1) ])
any(is.na(df2$IBM)) #rimosso tutti gli NA

#3
mean(df2$AAPL)
mean(df2$MSFT)
mean(df2$IBM)

var(df2$AAPL)
var(df2$MSFT)
var(df2$IBM)

sd(df2$AAPL)
sd(df2$MSFT)
sd(df2$IBM)

quantile(df2$AAPL, probs = c(0.2, 0.5,0.8) )
quantile(df2$MSFT, probs = c(0.2, 0.5,0.8) )
quantile(df2$IBM, probs = c(0.2, 0.5,0.8) )

# 4 Si determini inoltre l'intervallo di confidenza unilaterale superiore (+∞) per la media delle tre azioni.

t.test(df2$AAPL, alternative = "greater")
t.test(df2$MSFT, alternative = "less")
t.test(df2$IBM)

# 5 

par(mfrow = c(3,1), mar = c(4,2,2,2)+ 0.1 )
hist(df2$AAPL, freq = F, main = "Apple", xlab = "$")
hist(df2$MSFT, freq = F, main = "Microsoft", xlab = "$")
hist(df2$IBM , freq = F, main = "IBM", xlab = "$")


# Esercizio 2

df5 <- read.csv( url("https://www.dropbox.com/s/vipxnjvpvyh258i/data.csv?dl=1") , header = T)
View(df5)
summary(df5)
#1 

par(mfrow = c(1,1))
plot( military ~ log10(gdp), data = df5)

reg <- lm(literacy ~ log10(gdp), data = df5)
summary(reg)

par(mfrow = c(4,1), mar = c(2,2,2,2)+ 0.1 )
plot(literacy ~ log10(gdp), data = df5)
abline(reg$coefficients, col = "red")
plot(reg$residuals)
hist(reg$residuals)
qqnorm(reg$residuals)
qqline(reg$residuals, col = "green")

# Il valore di R^2 non è molto altro, ma visualizzando i risultati 
# sembra che il modello possa spiegare in modo acettabile l'andamento del
# alfabetizzazione come consegueza dell'aumento della ricchezza di un paese.
# Analizzando l'isogramma e la retta dei quantili dei residui,
# sembrano che essi seguano una distribuzione normale centrata in 0.
# Inoltre non presentano pattern e quindi non si hanno ragioni
# per supporre che non siano indipendenti.

# 2
myfun <- function(a,b){
  a1 <- na.omit(a)
  b1 <- na.omit(b)
  if (length(a1) == length(b1)){
    y <- 0
    for (i in seq(1,length(a1))) {
      y <- y + a1[i]*b1[i]
    }
    return(y)
  }
    else {
      print("la dimensione dei vettori è differente")
  }
}


myfun(df5$literacy,df5$gdp )
myfun(df5$literacy,df5$military )
myfun(df5$income,df5$military )
