#Ex1

df1 <- read.table(url("http://extras.springer.com/2012/978-1-4614-1301-1/AsthmaLOS.txt"), sep = "\t", header = TRUE)
View(df1)
names(df1)
str(df1)
summary(df1[,c("age", "owner.type")])

# owner.type da descrizione dovrebe essere 1 o 2, invece il minimo è 0.
# dobbiamo allora individuare le righe che contengono i dati errati

which(df1$owner.type == 0)

df1[df1$owner.type == 0,]
df1[df1$age == 0,]
# le stesse righe contengono anche altri errori, ad esempio l'età e l'hospital.id
# riportano valori inusuali

# Visualizziamo l'istogramma rimuovendo tali misurazioni
hist( df1[- which(df1$owner.type == 0), "age" ], main = "Dati rimossi")


#sostituiamo i dati con la mediana
df2 <- df1
median(df2$age)
df2[which(df1$age == 0),"age" ] <- median(df2$age)

par(mfrow = c(1,2))
hist(df1[df1$age>0, "age"], main = "Dati rimossi")
hist(df2$age, main = "Dati sostituiti")

# Confrontando i due istogrammi vediamo che c'è una minima differenza.
# Questo non ci sorprende visto che i dati sono molti e il valore assunto, 
# cioè 0 non è "vicino" alle misurazioni.


# Ex3

rm(df1) # rimuoviamo i dataframe per riusare gli stessi nomi
rm(df2)

df1 <- read.csv(url("https://raw.githubusercontent.com/socviz/soc880/master/data/gapminder.csv"))
View(df1)
names(df1)

# Aggreghiamo i dati rispetto ai continenti e dato che l'esercizio chiede l'"andamento"
# dobbiamo anche aggiungere la variabile tempo. Aggreghiamo usando la media.
df2 <- aggregate(cbind(lifeExp, gdpPercap, pop) ~ continent + year,data = df1,  FUN = mean)
View(df2)

# Visualizziamo gli andamenti richiesti e rendiamoli più leggibili differenziando 
# i colori in base ai continenti
par(mfrow = c(3,1))
par(mfrow = c(3,1), mar=c(3,2,2,1)) #imposto i margini per migliorare la visializzazione

plot(lifeExp ~ year, data = df2, col=continent, main = "lifeExp vs year")
plot(lifeExp ~ gdpPercap, data = df2, col=continent , main = "lifeExp vs gdp")
plot(lifeExp ~ pop, data = df2, col=continent , main = "lifeExp vs pop")

# Proviamo alcune trasformazioni 
plot(lifeExp ~ year, data = df2, col=continent, main = "lifeExp vs year")
plot(lifeExp ~ log(gdpPercap), data = df2, col=continent , main = "lifeExp vs sqrt(gdp)")
plot(lifeExp ~ sqrt(pop), data = df2, col=continent , main = "lifeExp vs log(pop)")

