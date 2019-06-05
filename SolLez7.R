
# Apple
AAPL <- read.csv("./data/AAPL.csv", header = T)
View(AAPL)
str(AAPL)
AAPL$Index <- as.Date(AAPL$Index)

# Microsoft

MSFT <- read.csv("./data/MSFT.csv", header = T)
View(MSFT)
MSFT$Index <- as.Date(MSFT$Index)

# IBM

IBM <- read.csv("./data/IBM.csv", header = T)
View(IBM)
IBM$Index <- as.Date(IBM$Index)


# Unire in un inico dataset
AAPL$X <- NULL
View(AAPL)
MSFT$X <- NULL
IBM$X <- NULL

df <- merge(AAPL, MSFT, by = "Index")
df <- merge(df, IBM, by = "Index" )
View(df)

# 2 Si visualizzi l’andamento dei valori delle tre azioni al variare del tempo. 

par(mfrow = c(3,1) , mar = c(2,1,1,1))
plot(df$Index, df$AAPL, col = "red", main = "AAPL")
plot(df$Index, df$MSFT, col = "blue", main = "MSFT", ylab = "$")
plot(df$Index, df$IBM , main = "IBM", xlab = "Time")

# 2. Si analizzino inoltre gli indici di dispersione IQR, Range e deviazione standard

IQR(df$AAPL, na.rm = T )
IQR(df$MSFT, na.rm = T )
IQR(df$IBM, na.rm = T )

range(df$AAPL, na.rm = T )
range(df$MSFT, na.rm = T )
range(df$IBM, na.rm = T )

sd(df$AAPL, na.rm = T )
sd(df$MSFT, na.rm = T )
sd(df$IBM, na.rm = T )


# 3. Per ognuna delle tre azioni, si calcolino gli intervalli di confidenza bilateri al livello 95% per la media dei prezzi assunti negli anni dal 2013 al 2015.

df1 <- df[df$Index > "2012-12-31" & df$Index < "2016-01-01",]
View(df1)
t_APPL <- t.test(df1$AAPL)
t_APPL$conf.int[1:2]

t.test(df1$AAPL)
t.test(df1$MSFT)
t.test(df1$IBM)

# 4. Si consideri un modello di regressione lineare per analizzare l’andamento dei prezzi delle azioni di Microsoft rispetto a quello delle azioni di IBM per il periodo dal Dicembre 1990 (incluso) al Giugno 2007 (incluso). Si visualizzi la retta di regressione rispetto ai dati 

df2 <- df[df$Index > "1990-11-30" & df$Index < "2007-07-31",]

View(df2)

reg <- lm(MSFT ~ IBM, data = df2)

summary(reg)

#4. i visualizzi la retta di regressione rispetto ai dati
par(mfrow = c(1,1))
plot(MSFT~IBM, data = df2)
abline(reg$coefficients, col = "red")

cor(na.omit(df2[,c("IBM", "MSFT")]) )
0.9514939^2

#4 . Conclusioni
# Il modello sembra spiegare piuttosto bene i dati, tuttavia sembra 
# strano che l'andamento di uno causi cambiamenti nell'andamento dell'altro
# e' piu' plausibile che, essendo due titoli del comparti tecnologico, 
# il loro andamento in borsa sia simile poiche tutto il comparto solitamente
# sale o scendie insieme. 



