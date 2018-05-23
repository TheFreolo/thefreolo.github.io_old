# Soluzione

# 1.
df3 <- read.table("./data/DataL5.tsv", sep = "\t", header = T, stringsAsFactors = T)
# 2. Supponiamo che il numero sia 5
df4 <- df3[df3$Idataset == 5,]
df4$IIdataset <- NULL
View(df4)
str(df4)
# Si nota subito che qualcosa nella lettura di x e y non è andato bene in quanto risultano levels,
# mentre dovrebbero essere numeri
# Comuqnue il dataset è tidy

# Convertiamo i dati in formato numerico e notiamo che
as.numeric(df4$x) # non funziona (perche partiamo da un dataframe di factors)!!!!
as.numeric(as.matrix(df4$x)) #funziona (grazie google)

df4$x <- as.numeric(as.matrix(df4$x))
df4$y <- as.numeric(as.matrix(df4$y))

str(df4)
# Meglio

# 4. Cerchiamo i NA
summary(df4)

any(is.na(df4$x))
any(is.na(df4$y))
# abbiamo NA solo per la variabile y, in quale riga è?

which(is.na(df4$y))
# sostituiamo quel valore con la mediana
# prima mi copio i dati in un nuovo dataset
df5 <- df4

df5[143, "y"] <- median(df5$y, na.rm = T)
## OPPURE
df5[which(is.na(df5$y)), "y"] <- median(df5$y, na.rm = T)
## OPPURE
df5[is.na(df5$y), "y"] <- median(df5$y, na.rm = T)
# Verifichiamo di aver effettivamente rimosso il NA
any(is.na(df5$y))
# OK!

# 5.
summary(df5)
sd(df5$x)
sd(df5$y)

# 6.
par(mfrow = c(2,2), mar = c(2,2,1,1))
hist(df5$x, main = "X")
hist(df5$y, main = "Y")
boxplot(df5$x, main = "X")
boxplot(df5$y, main = "Y")

# 7.

par(mfrow = c(1,1), mar = c(2,2,1,1))
plot(df5$x, df5$y)