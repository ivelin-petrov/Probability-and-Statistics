# непрекъснато разпределение
# интервал от време
# Гаусово (нормално) разпределение

# генериране на опити (n), средна стойност (мю)
# стандартно отклонение (разпръснатост на данните)
x = rnorm(1000,167,7)
hist(x,probability=T,breaks=300)
lines(density(x))
boxplot(x)

mean(x>160 & x<171) # емперична вероятност

pnorm(171,165,7) - pnorm(160,165,7) # теоретична вероятност за 160<x<171
pnorm(173,165,7,lower.tail=F) # x>173

y = qnorm(0.73,165,7) # p(x<?) = 0.73

quantile(x,0.73)
quantile(x)

quantile(x,0.50) - quantile(x,0.25) # между медиана (0.50 квантил) и 0.25 квантил

# rexp, dexp, pexp, qexp -> експоненциално разпределение

# стандартизиране -> стандартно нормално разпределение (с център = 0, станд. отклонение = 1)
# нормално разпределение
z = rnorm(1000,0,1)
q2=quantile(z,seq(0,1,by=0.01))
q1=qnorm(seq(0,1,by=0.01)) # теоретични квантили -> разпределение на данните
plot(q1,q1)

# (x-мю)/станд.отклонение ~ N(0,1) (зависимост) пример отгоре 

# разпознаване на нормално разпределение:
# 1) hist
# 2) boxplot
# 3) qqnorm(x), qqline(x) -> пример отгоре
# 4) shapiro.test(x) -> хипотези
# при p.value < 0.05 върната стойност от shapiro.text(x) 
# -> H0 нулева хипотеза не е вярна и я отхвърляме
# -> H1 първа хипотеза е вярна

# H0:x~N(мю,(станд.отклонение)^2)
# H1:x~not~N(мю,(станд.отклонение)^2)

# равномерно разпределение - uniform
# r,d,p,q(unif) 
runif(1000,1,12) # вер. между 1 и 12
punif(1/2) # вероятност между 0 и 1

# task1

# N(mu,sd^2) -> rnorm(n,mu,sd)
# lines(density(x)) -> емп. плътност

# dnorm -> теоретична плътност

# а)
x=rnorm(1000,5,sqrt(2))
hist(x,probability = T)
lines(density(x)) # емперична плътност

s=seq(0,10,by=0.5)
probs=dnorm(s,5,sqrt(2)) # теор. вер. за дадена стойност
lines(s,probs,col="red") # теоретична плътност

boxplot(x)

# б)
y=runif(1000,1,sqrt(5))
hist(y,probability = T)
lines(density(y)) # емперична плътност
boxplot(y)
  
# в)
z=rexp(1000,sqrt(3))
hist(z,probability = T)
lines(density(z)) # емперична плътност
boxplot(z)

# д)
n1 = rnorm(2000,10,1)
n2 = rnorm(2000,1,1)
m = cbind(n1,n2)
idx = sample(c(1,2),prob=c(0.2,0.8),replace=TRUE,size=2000)
n3 = m[1:2000,idx]
hist(n3,probability = T) # бимодално

# task2

# x %>% / hist(.) -> hist(x)

# Exp(lambda) -> x1,x2,...,xn

sum(rexp(1000,3)) # x1,x2,...,xn
x=replicate(2*10^3,sum(rexp(1000,3)))
hist(x,probability = T)
lines(density(x))

# (1/n)*SUM(xi) ~ N(mu,sd^2)

# R markdown -> new file
# knit -> html
# # lorem ipsum ```{r code}

# task3

# hist, boxplot, qqnorm -> за да определим дали данните идват от нормално разпределение
