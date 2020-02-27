
library(ggplot2)

theme_set(theme_classic())

x <- c(5.0, 1.6, 2.0, 3.5, 4.7, 3.1, 2.4, 4.1, 1.2, 2.9)

y <- c(23.5, 16.1, 16.1, 20.6, 22.8, 20.3, 18.7, 22.5, 14.4, 19.4)

data <- data.frame(x, y)

ggplot(data, aes(x, y)) +
  geom_point()

mean_x <- mean(x)
mean_y <- mean(y)

var_x <- var(x)
var_y <- var(y)
covariance <- cov(x, y)

cov(data)
cor(data)

empirical_variance <- function(x){
  n <- length(x)
  mean_squared <- mean(x) ** 2
  return((1/(n-1)) * (sum((x**2)) - ((n/(n-1)) * mean_squared)))
}

empirical_variance(x)

correlation <- function(x, y){
  sum_1 <- sum(x*y)
  n <- length(x)
  miux <- mean(x)
  miuy <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  return((sum_1 - n*miux*miuy)/((n-1) * sqrt(var_x * var_y)))
  
}

correlation(x, y)

slope <- function(x, y){
  return(cor(x, y) * (sd(y)/sd(x)))
}


slope(x, y)


model <- lm(data, formula = y~x)
summary(model)
y_estimate <- model$fitted.values
res <- model$residuals
SST <- sum((y - mean(y)) ** 2)
SSE <- sum((y_estimate - mean(y)) ** 2)
SSR <- sum((y - y_estimate) ** 2)

SST == (SSE + SSR)


(1/length(x)) * sum(y_estimate) == mean(y)

(1/length(y)) * sum(res) == 0

## 3
file.choose()

data <- read.table("C:\\Users\\fc55066\\Downloads\\pasis.dat.txt")

ggplot(data, aes(id,pa)) +
  geom_point(colour = 'blue') +
  geom_smooth(method = 'lm')
    

a <- cor(data$id, data$pa)
cat('The value for correlation is', a,  'Which indicates that
    there could be a linear correlation between this two variables')

slope(data$id, data$pa)

model_regression <- lm(data, formula = pa ~ id)

summary(model_regression)

x <- data$id
y <- data$pa
y_estimate <- model_regression$fitted.values

SST <- sum((y - mean(y)) ** 2)
SSE <- sum((y_estimate - mean(y)) ** 2)
SSR <- sum((y - y_estimate) ** 2)

cat(SST, '=', SSE, '+', SSR)

SST == SSE +SSR

data$pa_mod <- data$pa / 10
model_regression_mod <- lm(data, formula = pa_mod ~ id)
summary(model_regression_mod)


ggplot(data, aes(id,pa_mod)) +
  geom_point(colour = 'blue') +
  geom_point(aes(id, pa), colour = 'red') +
  geom_smooth(method = 'lm') +
  scale_x_continuous(breaks = seq(17, 90, 1)) +
  geom_smooth(aes(id, pa), method = 'lm') +
  geom_point(data = data['id' >= 18], aes(id, pa)) +
  geom_smooth(data = data['id' >= 18], aes(id, pa),
              method = 'lm',
              colour = 'red') + 
  geom_smooth(method = 'lm',
              formula = x ~y)

model_regression_adults <- lm(data['id' >= 18], formula = pa ~ id)
summary(model_regression)
summary(model_regression_adults)


coef_1 <-model_regression$coefficients[2]
coef_2 <- model_regression_adults$coefficients[2]
inter_1 <- model_regression$coefficients[1]
inter_2 <- model_regression_adults$coefficients[1]



model_regression

model_regression_inverted <- lm(data, formula  = id ~pa)
summary(model_regression_inverted)





shapiro.test(data$pa)

hist(data$pa,
     breaks = 15)


boxplot(data$pa)









