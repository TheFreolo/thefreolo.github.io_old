
df <- read.csv( url("https://www.dropbox.com/s/4p42bfbcjt7jbmh/data.csv?dl=1") , header = T)

df20 <- read.csv( url("https://www.dropbox.com/s/yy6poi7q4vvo27e/conts.txt?dl=1") , header = T)

#0. Si leggano i dataset relativi al relativi ai dati socioeconimici di 154 nazioni e al continente di appartenenza.

#1. Si uniscano i due dataset in modo da averne uno solo. Le rimanenti analisi isi ntendono su tale dataset.
View(df)
View(df20)
str(df)
str(df20)

# Si può notare che i dataset hanno lunghezza diversa, quindi 
# per aggregare i dati ho bisogno di una funzione come merge o aggregate,
# non posso usare cbind!

df1 <- merge(df, df20, by = "country")

#2. Si analizzi il dataset e si rimuovano i paesi per cui non si hanno informazioni sul continente. Inoltre, si individiuno eventuali errori nei dati. Si sostituiscano gli NA con __la mediana della stessa variabile nello stesso continente__.

str(df1)
summary(df1)

df2 <- df1[!is.na(df1$cont),]

summary(df2)

##GDP
which(is.na(df2$gdp))
i <- which(is.na(df2$gdp))
i[1]
df2$gdp[i[1]] <- median(df2$gdp[ df2$cont == df2$cont[i[1]]], na.rm = T)
df2$gdp[i[2]] <- median(df2$gdp[ df2$cont == df2$cont[i[2]]], na.rm = T)
anyNA(df2$gdp)

##income
which(is.na(df2$income))
j <- which(is.na(df2$income))
df2$income[j[1]] <- median(df2$income[ df2$cont == df2$cont[j[1]]], na.rm = T)
df2$income[j[2]] <- median(df2$income[ df2$cont == df2$cont[j[2]]], na.rm = T)
df2$income[j[3]] <- median(df2$income[ df2$cont == df2$cont[j[3]]], na.rm = T)
anyNA(df2$income)

##literacy
which(is.na(df2$literacy))
k <- which(is.na(df2$literacy))
df2$literacy[k] <- median(df2$literacy[ df2$cont == df2$cont[k]], na.rm = T)
anyNA(df2$literacy)

##military
which(is.na(df2$military))
l <- which(is.na(df2$military))
df2$military[l] <- median(df2$military[ df2$cont == df2$cont[l]], na.rm = T)
anyNA(df2$literacy)

#3. Si calcoli media, varianza e quartili delle variabili numeriche. Visualizzino le righe relative ai primi e ultimi 5 paesi rispetto al valore di _gdp_.

summary(df2)

head(df2[order(df2$gdp),])
tail(df2[order(df2$gdp),])

#4. Si visualizzi la distribuzione di 'income' e 'GDP'. Si visualizzino inoltre i boxplot per le variabili 'milirary' e 'income' per continente.

par(mfrow= c(4,1), mar = c(2,2,1,1))
hist(df2$income, main = "Income")
hist(df2$gdp, main = "GDP")
boxplot(income ~ cont, data = df2)
boxplot(military ~ cont, data = df2)

#5. Considerando la soglia di confidenza del 95%, si verifichi se la media della spesa militare (_military_) tra i paesi del continente _AS_ e _EU_ differiscono in modo significativo. Cosa di può dire della medie della variabile _income_? Si commentino i risultati.

t.test(df2$military[df2$cont == "AS"], df2$military[df2$cont == "EU"])
t.test(df2$income[df2$cont == "AS"], df2$income[df2$cont == "EU"])

## I due risultati mostrano che la differenza in spesa media militare tra Asia e Europa
## non è significativa, mentre quella di introito medio lo è. 
## Se visualizziamo i boxplot si può notare che ci sono diverse nazioni 
## asiatiche con spesa militare molto alta. Come noto la media è influenzata da
## (potenziali) outliers e questo potrebbe spiegare il risultato.
## Tuttavia, fa riflettere che in nazioni dove in media i cittadini sono più poveri,
## la spesa militare non sia significativamente minore.

