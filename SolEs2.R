## Si trovi un modo adeguato per importare i dati relativi al 
## reddito nazionale lordo pro capite e alla percentuale di 
## strade asfaltate in R. Dopo aver analizzato e preparato i dataset, 
## si usino i dati per investigare le due variabili rispetto ad un 
## paese del G7, un paese in via di sviluppo ed un paese del terzo 
## mondo a scelta, nel periodo dal 1990 al 2009. 
## Si analizzino i dati e si visualizzino. 

getwd()

setwd("C:/Dropbox/Bonaccorsi/Calcolo delle probabilita e statistica matematica/2018/R/Lezione 5/")

install.packages("openxlsx")
library("openxlsx")

dfPPP <- read.xlsx("./data/indicatorGNIpercapitaPPP.xlsx")
dfroad <- read.xlsx("./data/roads paved.xlsx")

dfPPP <- read.xlsx("../../Lezione 5/data/indicatorGNIpercapitaPPP.xlsx")
dfroad <- read.xlsx("../../Lezione 5/data/roads paved.xlsx")

# Seleziono SOLO le colonne relative agli anni che mi interessano per le analsi
# e ovviamente la prima colonna con i nomi

dfroad1 <- dfroad[,paste(c("Roads,.paved.(%.of.total.roads)",paste(1990:2009)))]
dfPPP1 <- dfPPP[,paste(c(names(dfPPP)[1], 1990:2009))]


View(dfroad1)
View(dfPPP1)

# Cerchiamo di capire quanti e quali sono i paesi per cui ho dati completi:

# questo passaggio non e' obbligatorio, si possono anche scegliere paesi con dati incompleti, facendo attenzione ai NA
View(na.omit(dfroad1) )
View(na.omit(dfPPP1) )

# vogli controllare quali sono i paesi nell'intersezione dei due dataframe (Si puo' anche fare a mano)
na.omit(dfroad1)[1]
na.omit(dfPPP1)[1]
intersect(as.matrix(na.omit(dfroad1)[1]) , as.matrix(na.omit(dfPPP1)[1]) )

# Scelgo come paesi G7, in via di sviluppo e terzo mondo, usando la mappa in http://www.nationsonline.org/oneworld/third_world_countries.htm
# UK (unico con dati completi) , Ukraine e Marocco

# Seleziono i dati relativi ai paesi

dfroad2 <- dfroad1[dfroad1$`Roads,.paved.(%.of.total.roads)` == "United Kingdom" | dfroad1$`Roads,.paved.(%.of.total.roads)` == "Ukraine" | dfroad1$`Roads,.paved.(%.of.total.roads)` == "Morocco",]
dfPPP2 <- dfPPP1[dfPPP1$`GNI.per.capita,.PPP.(current.international.$)` == "United Kingdom" | dfPPP1$`GNI.per.capita,.PPP.(current.international.$)` == "Ukraine" | dfPPP1$`GNI.per.capita,.PPP.(current.international.$)` == "Morocco",]

View(dfroad2)
View(dfPPP2)

# Studiamo il summary per ANNO

summary(dfroad2)
summary(dfPPP2)

# Studiamo il summary per paese (si puo' fare in piu' modi)

dfroad3 <- t(dfroad2)[-1,]
dfroad3 <- as.data.frame(dfroad3)
names(dfroad3) <- t(dfroad2)[1,]
summary(dfroad3)


summary(t(dfPPP2)[-1,] )
dfPPP3 <- t(dfPPP2)[-1,]
dfPPP3 <- as.data.frame(dfPPP3)
names(dfPPP3) <- t(dfPPP2)[1,]

dfPPP3$Morocco <- as.numeric(as.character(dfPPP3$Morocco))
dfPPP3$Ukraine <- as.numeric(as.character(dfPPP3$Ukraine))
dfPPP3$`United Kingdom` <- as.numeric(as.character(dfPPP3$`United Kingdom`))
summary(dfPPP3)
# Fine parte opzionale

# Visualizzo i dati
par(mfrow = c(3,2))
for (i in 1:3){
  plot(1990:2009, dfroad2[i,-1], main = dfroad2$`Roads,.paved.(%.of.total.roads)`[i], ylab = "% of paver roads")
  plot(1990:2009,dfPPP2[i,-1]  , main = dfPPP2$`GNI.per.capita,.PPP.(current.international.$)`[i], ylab = "GPD $" )
}


# Codice per avere i nomi dei paesi nei boxplot, invece che il numero di riga originale
row.names(dfroad2) <- dfroad2[,1]
row.names(dfPPP2) <- dfPPP2[,1]

# Boxplot
par(mfrow = c(1,2))
boxplot(t(dfroad2[,-1]) , main = "Paved roads (%)")
boxplot(t(dfPPP2[,-1]), main = "GPD $" )

## Inoltre si usi la regressione lineare per analizzare 
## la relazione tra le variabili per uno dei due paesi scelti. 
## Si commentino e discutano i risultati.

# Voglio analizzare i dati per il Marocco e vedere se la relazione tra GPD e strade asfaltate e' lineare

reg1 <- lm(t(dfroad2[1,-1]) ~ t(dfPPP2[1,-1]) )
summary(reg1)

# da una prima analisi di R^2 la nostra supposizione sembra giusta.
# Visualizziamo piu' nel dettaglio i risultati

par(mfrow = c(2,2), mar = c(2,2,1,1))
# Retta di regressione
plot(t(dfroad2[1,-1]) ~ t(dfPPP2[1,-1]))
abline(reg1$coefficients, col = "green")
# Pattern nei residui
plot(reg1$residuals, main = "Residui")
# Distribuzione in quantili 
qqnorm(reg1$residuals)
qqline(reg1$residuals)
hist(reg1$residuals)

# Sembra che il modello sia piuttosto adeguato, 
# anche se osservando i residui, risulta piu' adeguato rispetto alle prime osservazioni del dataset (GPD piu' basso).
# I residui non presentano pattern evidenti, mentre la distribuzione in quantili si allontanta da quella di riferimento per i valori piu' grandi.

## 29/05/2019
## Federico Reali
