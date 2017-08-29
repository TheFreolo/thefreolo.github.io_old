
# ########### #
# Svolgimento #
# ########### #

# Matricola dispari

# Leggo i dataset 

df10 <- read.csv("./Count1.csv")
df10$X <- NULL
str(df10)
df11 <- read.csv("./WeatherData.csv")
df11$X <- NULL
str(df11)

# Si uniscano i due dataset

df20 <- merge(df10,df11, by = "dteday")
View(df20)

summary(df20$cnt)
# Sicuramente ci sono NA, Inf e forse altro.
which(df20$cnt == Inf | df20$cnt == -Inf)
# Ci sono misurazioni negative? (non dovrebbero esserci!)
which(df20$cnt < 0)
# Unico negativo e' il 415
# Inizio rimuovendo Inf e -8

IndexINF <- which(df20$cnt == Inf | df20$cnt == -Inf | df20$cnt < 0 )

# Sostituisco con la mediana per lo stesso mese dello stesso anno escluso negativi, Inf e NA

df20$cnt[IndexINF[1]] <- median(df20$cnt[ df20$mnth == df20$mnth[IndexINF[1]] & df20$yr == df20$yr[IndexINF[1]] & df20$cnt > 0 & df20$cnt != Inf ], na.rm = T)

df20$cnt[IndexINF[1]]

df20$cnt[IndexINF[2]] <- median(df20$cnt[ df20$mnth == df20$mnth[IndexINF[2]] & df20$yr == df20$yr[IndexINF[2]] & df20$cnt > 0 & df20$cnt != Inf ], na.rm = T)
df20$cnt[IndexINF[2]]

#controllo la rimozione
any(df20$cnt == Inf | df20$cnt == -Inf | df20$cnt < 0)
#OK!

#Rimuovo nello stesso modo gli NA

which(is.na(df20$cnt))
IndexNA <- which(is.na(df20$cnt))

# Avendo gia tolto gli Inf e i negativi posso rimuovere quei controlli
for (i in IndexNA){
  df20$cnt[i] <- median(df20$cnt[ df20$mnth == df20$mnth[i] & df20$yr == df20$yr[i] ], na.rm = T)
}

any(is.na(df20$cnt)) #rimosso tutti gli NA

# Quartili

quantile(df20$cnt, c(.25, .5, .75))
# IQR 
IQR(df20$cnt)
# Range
(max(df20$cnt) - min(df20$cnt))
# Media
mean(df20$cnt)
# Sd
sd(df20$cnt)

#boxplot(cnt~mnth, data = df20[df20$season == 3, ], xlab = "Mese", ylab = "Numero noleggi")

hist(df20$cnt[df20$season == 3], xlab = "Numero noleggi", main = "Noleggi estivi")


# Regressione

df20$dteday <- as.Date(df20$dteday)

reg_data <- df20[df20$dteday > "2011-03-31" & df20$dteday < "2011-06-01" , ]

boxplot(reg_data$cnt)

which(reg_data$cnt < 1000)

reg_data <- reg_data[- 16,]

boxplot(reg_data$cnt)


# Matricola dispari


lm_cnt_date <- lm(cnt~dteday, data = reg_data)

summary(lm_cnt_date)


par(mfrow = c(3,1), mar = c(2,2,1,1))
plot(cnt~dteday, data = reg_data)
abline(lm_cnt_date$coefficients, col = "red")
plot(lm_cnt_date$residuals, main = "Resudui")
qqnorm(lm_cnt_date$residuals)
qqline(lm_cnt_date$residuals)