## Aula 2 GLM

library(ggplot2)
library(data.table)
library(car)

x<- rnorm(100, 0, 1)
y<- seq(100)

sampling <- seq(50, 100)
sd_s <- seq(1, 10)
cor(x, y)


a <- 0
while (abs(cor(x, y)) <= 0.8){
  x <- rnorm(100, sample(sampling, 1), 5)
  y <- rnorm(100, sample(sampling, 1), sample(sd_s, 1))
  c <- cor(x, y)
  if(cor(x, y) > c){
    print(c)
  }
}

summary(a)
plot(x, y)
x <- 0 
y <- 0

x_1 <- x
y_1 <- y

x_2 <- x
y_2 <- y

plot(x_1, y_1)
plot(x_2, y_2)


## Aula

# a)
# file.choose()
data
data <- fread( "C:\\Users\\fc55066\\Downloads\\Risk_Data_WWDaniel.csv")

colnames(data) <- c('subject', 'oxygen', 'sbp',
                    'choles_tot', 'choles_HDL', 'trig')
oxygen <- data[, oxygen]
sbp <- data[, sbp]
choles_tot <- data[, choles_tot]
choles_HDL <- data[, choles_HDL]
trig <- data[, trig]

# b)

cor_1 <- cor(oxygen, sbp)
cor_2 <- cor(oxygen, choles_tot)
cor_3 <- cor(oxygen, choles_HDL)
cor_4 <- cor(oxygen, trig)

correlations <- c(cor_1, cor_2, cor_3, cor_4) ; correlations


par(mfrow = c(2, 2))

plot(sbp, oxygen, col = 'blue')
plot(choles_tot, oxygen, col = 'red')
plot(choles_HDL, oxygen, col = 'orange')
plot(trig, oxygen)

# c)
model <- lm(data = data, formula = oxygen ~ choles_HDL)
summary(model)



# d)
aov_mod <- anova(model)
cat('F value is',aov_mod$`F value`, 'and so with p value = ', round(aov_mod$`Pr(>F)`, 1),
    'we reject H0 and conclude that beta 1 is differente from 0')

## fitted model is oxygen = 16.307 + 0.3715*choles_HDL

confint(model)

ggplot(data, aes(choles_HDL, oxygen)) + 
  geom_point(position = 'jitter',
             pch = 1) +
  geom_abline(slope = model$coefficients[2],
              intercept = model$coefficients[1],
              colour = 'blue')

summary(model$res)

par(mfrow = c(3, 2))
qqnorm(model$res)
qqline(model$res)
hist(model$res)
qqPlot(model$res)
boxplot(model$res,
        horizontal = T)


plot(choles_HDL, model$res)
abline(h = 0)

plot(model$fitted.values, scale(model$residuals))
abline(h = 0)


plot(model)

