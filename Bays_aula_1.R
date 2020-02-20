# function 1, exercicio 1


func_1 <- function(x, sigma){
 a <- ((x/sigma**2)*(exp(-(x**2/sigma**2))))
 return(a)
}

x <- seq(0, 12, 0.05)

plot(x, func_1(x, 1),
     main = 'Rayleigh Distribution',
     ylim = c(0, 0.6),
     type = 'l')
lines(x, func_1(x, 2),
      col = 'red')
lines(x, func_1(x, 3),
      col = 'blue')
lines(x, func_1(x, 4),
      col = 'green')
lines(x, dchisq(x, 4),
       col = 'purple')

# Exercice 2

# func_2 <- function(a){
#   if(a < 0){
#     return((1/4)*exp(a/2))
#   }else{
#     return(-((1/4)*exp(a/2)))
#   }
# }
# 
x2 <- seq(-10, 10, 0.05)
# plot(x2, func_2(x2))

f <- function(x){
  y <- ifelse(x<0, 0.25*exp(x/2), 0.25*exp(-x/2))
  return(y)
}

plot(x2, f(x2),
     col = 'red',
     type = 'l',
     main = 'fdp laplace')


# Exercice 3

file.choose()
data <- read.table("C:\\Users\\fc55066\\Downloads\\Glicemia.txt")
data <- as.data.frame(data)
str(data)

colnames(data) <- 'obs'
summary(data)


multi_func <- function(x){
  mean <- sum(x)/length(x)
  max_data <- max(x)
  min_data <- min(x)
  return(c(mean, max_data, min_data))
}

multi_func(data[, 'obs'])

hist(data[, 'obs'],
     col = 'lightblue')

