#file.choose()

library(data.table)
library(ggplot2)

theme_set(theme_classic())

data <- read.table("C:\\Users\\fc55066\\Downloads\\blood.pressure.txt",
                   sep = ',',
                   header = T)

data <- as.data.table(data)

str(data)

data[, `:=` (
             race=factor(race),
             alcohol = factor(alcohol),
             trt = factor(trt),
             stress = factor(stress),
             salt = factor(salt),
             chldbear = factor(chldbear),
             income = factor(income),
             educatn = factor(educatn),
             hypertension = factor(hypertension),
             overwt = factor(overwt))]


table(data[, gender])

summary(data)

a <- lm(sbp~age, data)
b <- lm(sbp~bmi, data)
summary(a)
summary(b)


ggplot(data, aes(age, sbp, colou)) +
  geom_point(aes(pch = stress)) +
  scale_y_continuous(limits = c(0, 250)) +
  geom_smooth(method  ='lm', formula = y~x)

ggplot(data, aes(bmi, sbp, colour = 'blue')) +
  geom_point() +
  scale_y_continuous(limits = c(0, 250)) +
  geom_smooth(method  ='lm', formula = y~x)

ggplot(data, aes(bmi, sbp)) +
  geom_point(aes(colour = smoke)) +
  scale_y_continuous(limits = c(0, 250)) +
  geom_smooth(method  ='lm', formula = y~x)

ggplot(data, aes(bmi, sbp)) +
  geom_point(aes(size = stress)) +
  scale_y_continuous(limits = c(0, 250)) +
  geom_smooth(method  ='lm', formula = y~x)

# Exercicios
# 1
names <- colnames(data)

#2
number <- table()


tapply(data$bmi, data$hypertension, summary)

par(mfrow = c(1, 2))
boxplot(data[, weight]~data[, gender], 
        main = 'Weight', 
        ylab = 'Weight', 
        xlab = 'Gender', 
        col = c('red', 'blue'))
boxplot(data[,height]~data[, gender], 
        main = 'Height', 
        ylab = 'Height', 
        xlab = 'Gender',
        col = c('red', 'blue'))

tab <-table(data$exercise, data$hypertension)

chisq.test(tab)
fisher.test(tab)

addmargins(tab)


dim(data)

head(data)

dados.1 <- data[bmi >30 & alcohol == 1, bmi]
dados.2 <- data[bmi >30 & alcohol == 2, bmi]
dados.3 <- data[bmi >30 & alcohol == 3, bmi]

length(dados.1)
length(dados.2)
length(dados.3)

grupo <- c(rep(1, length(dados.1)),

           
             rep(2, length(dados.2)),
           rep(3, length(dados.3)))

par(mfrow = c(1,1))
boxplot(dados.1, dados.2, dados.3,
        horizontal = T,
        col = c('orange',
                'pink',
                'lightblue'))

data.m <- data[gender == 'M']
data.f <- data[gender == 'F']

summary(data.m)
summary(data.f)

imc <- round(703.0704 * (data$weight/data$height^2), 1)
data$bmi <- imc

f <- function(peso , altura){
  imc <- round(703.0704 * (data$weight/data$height^2), 1)
  n1 <- length(imc)
  n2 <- length(imc>30)
  grupo <- ifelse(imc<18.5,1,
                  ifelse(imc>18.5 & imc< 24.9, 2,
                  ifelse(imc> 24.9, 3, 4)))
  return(list(imc, n1, n2, grupo))
}

b <- f(data$weight, data$height)

summary(b[[1]])

c <- data[bmi > 40, .(age, gender)]
summary(c)

succ <- 0
i <- 1
repeat{
  if (data$married[i] == 'Y') succ <- succ + 1
  i <- i + 1
  if (i>length(data$married)) break()
}





