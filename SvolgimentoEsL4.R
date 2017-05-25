#Ex1

df5 <- read.table(file = "../data/Gray_Kangaroos.tsv" , sep="\t", header = TRUE)

View(df5)

plot(df5)

my_kang <- lm(Y ~ X, data = df5)
summary(my_kang)

my_kang$residuals

summary(my_kang)$r.squared

par(mfrow = c(1,2))
plot(my_kang$residuals, main = "Residui")

qqnorm(my_kang$residuals)
qqline(my_kang$residuals)


#Ex3

df1 <- read.csv(url("https://raw.githubusercontent.com/socviz/soc880/master/data/gapminder.csv"))
df2 <- aggregate(cbind(lifeExp, gdpPercap, pop) ~ continent + year,data = df1,  FUN = mean)

par(mfrow = c(1,2))
plot(lifeExp ~ gdpPercap, data = df2)
plot(lifeExp ~ log(gdpPercap), data = df2)

# Vediamo che una trasformazione logaritmica sembra rendere la relazione lineare.

# Tuttavia la richiesta è per continente, quindi dovremmo visualizzare i dati separatamente


unique(df2$continent) # i dati descrivono i cinque continenti

par(mfrow = c(5,1), mar=c(2,1,1,1)) #imposto i margini per migliorare la visializzazione
for (i in unique(df2$continent)) {
  plot(df2[df2$continent == i, "gdpPercap" ], df2[df2$continent == i, "lifeExp" ], main = i)
}

# separiamo i dati per continenti

Af <- df2[df2$continent == unique(df2$continent)[1], c("gdpPercap", "lifeExp")]
Am <- df2[df2$continent == unique(df2$continent)[2], c("gdpPercap", "lifeExp")]
As <- df2[df2$continent == unique(df2$continent)[3], c("gdpPercap", "lifeExp")]
Eu <- df2[df2$continent == unique(df2$continent)[4], c("gdpPercap", "lifeExp")]
Oc <- df2[df2$continent == unique(df2$continent)[5], c("gdpPercap", "lifeExp")]

# definiamo i modelli

lm_Af <- lm(lifeExp ~ gdpPercap, data = Af)
lm_Am <- lm(lifeExp ~ gdpPercap, data = Am)
lm_As <- lm(lifeExp ~ gdpPercap, data = As)
lm_Eu <- lm(lifeExp ~ gdpPercap, data = Eu)
lm_Oc <- lm(lifeExp ~ gdpPercap, data = Oc)

summary(lm_Af)
summary(lm_Am)
summary(lm_As)
summary(lm_Eu)
summary(lm_Oc)

# visualizziamo America ed Europa che ci servono in seguito

# impostiamo gli assi con ylim negli stessi range così da comparare più facilmente i risultati

par(mfrow = c(1,2), mar=c(3,2,2,2)) #imposto i margini per migliorare la visializzazione
plot(lifeExp ~ gdpPercap, data = Am, main = "America", ylim = c(50, 90))
abline(lm_Am$coefficients)

plot(lifeExp ~ gdpPercap, data = Eu, main = "Europa", ylim = c(50, 90))
abline(lm_Eu$coefficients)

# uso di predict

lm_Am_Eu <- predict(lm_Am, newdata = Eu)
lm_Eu_Am <- predict(lm_Eu, newdata = Am)

# visualizzo i risultati

par(mfrow = c(1,2), mar=c(3,2,2,2)) #imposto i margini per migliorare la visializzazione
plot(lifeExp ~ gdpPercap, data = Eu, main = "Europa", ylim = c(50, 120))
points(Eu$gdpPercap, lm_Am_Eu, col = "red", cex = .5) #aggiungo i punti che mi indica il modello

plot(lifeExp ~ gdpPercap, data = Am, main = "Americhe", ylim = c(50, 120))
points(Am$gdpPercap, lm_Eu_Am, col = "red", cex = .5) #aggiungo i punti che mi indica il modello

# Calcolo la somma quadratica dei residui per i due dataset invertiti

(SSRes_Am_Eu = sum((Eu$lifeExp - lm_Am_Eu)^2))

(SSRes_Ee_Am = sum((Am$lifeExp - lm_Eu_Am)^2))

# questi dati ci confermano quello che visivamente è chiaro:
# usare il modello ottenuto dai dati europei sui dati americani 
# funziona meglio che il viceversa. Perché accade questo?

# Si ripeta la analisi usando una trasformazione logaritmica. Ci sono miglioramenti?