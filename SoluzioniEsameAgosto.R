
#################
## Svolgimento ##
#################

# Ho trasformato il file in csv usando Excell

df2 <- read.csv('./uffootball.csv')

# oppure 
library(readxl)
df2 <- read_xls('./uffootball.xls')


# Seleziono le variabili diinteresse

df3 <- df2[, c(1,2,3,4,5,9)]

# Struttura e summary
str(df3)
summary(df3) 

# Numero maggiore di scontri
table(df3$Opponent)[which.max(table(df3$Opponent))]

table(df3[df3$Opponent == "Georgia ","GatorOutcm"])
# in caso di uso di read_xls non serve lo spazio!
# table(df3[df3$Opponent == "Georgia","GatorOutcm"])

mean(as.numeric(df3[df3$Opponent == "Georgia ","OppWins_1A"], na.rm = TRUE))
sd(as.numeric(df3[df3$Opponent == "Georgia ","OppWins_1A"], na.rm = TRUE))

# Boxplot 
boxplot(OppWins_1A~GatorOutcm, data = df3)
# per  rimuovere i warning:
df4 <- df3
df4$OppWins_1A <- as.numeric(df4$OppWins_1A)
df4$GatorOutcm <- as.factor(df4$GatorOutcm)
boxplot(OppWins_1A~GatorOutcm, data = df4)

# Seleziono Alabama
table(df3[df2$Opponent == "Alabama ","GatorOutcm"])

# numero totale scontri:

sum(table(df3[df3$Opponent == "Alabama ","GatorOutcm"]))

# Probabilità: #vittorie/# scontri tot

table(df3[df3$Opponent == "Alabama ","GatorOutcm"])/sum(table(df3[df3$Opponent == "Alabama ","GatorOutcm"]))

# Circa 40%

# Con lo stesso procedimento calcoliamo la probabiltà per le atre due squadre

table(df3[df3$Opponent == "Kentucky ","GatorOutcm"])/sum(table(df3[df3$Opponent == "Kentucky ","GatorOutcm"]))

table(df3[df3$Opponent == "Tennessee ","GatorOutcm"])/sum(table(df3[df3$Opponent == "Tennessee ","GatorOutcm"]))

# La squadra contri cui ha vinto più spesso è il Kentucky.
# Secondo i dati ci aspettiamo che sia più probabile una vittoria contro 
# questa squadra.

# 29.08.2017
# Federico Reali
