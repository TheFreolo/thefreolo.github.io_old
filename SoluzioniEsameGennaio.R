#1
library(MASS)

#2
df <- crabs[,c("sex", "FL", "RW")]

#3
hist(df$FL)

hist(df$RW)

#4
boxplot(RW ~ sex , data = df)


#5
lm1 <- lm(RW ~ FL , data = df)

summary(lm1)

#6
par(mfrow = c(3,1), mar = c(2,2,1,1))
plot(RW ~ FL, data = df)
abline(lm1$coefficients, col = "red")
plot(lm1$residuals, main = "Resudui")
qqnorm(lm1$residuals)
qqline(lm1$residuals)

# I residui sembrano seguire un andamento non casuale.
# 
# Da una veloce analisi dei dati infatti si evince che
# i primi le misurazioni 1-50 e 101-150 riguadano granchi maschi,
# mentre le restanti sono femminili. 
# 
# Questo sembra suggerire diversi residui valori per i residui.
# Questo suggerisce che si dovrebbe considerare un modello 
# generalizzato che tiene anche conto della variabile sex.