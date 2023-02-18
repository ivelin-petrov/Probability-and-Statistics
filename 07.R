# x1~Bi(n,p)
# x2~Bi(n,p)
# x1+x2 -> Bi

x1 = rnorm(1000,1,7) # mu1, sigma1^2
x2 = rnorm(1000,3,7) # mu2, sigma2^2
hist(x1+x2) # -> norm ~N(mu1 + mu2, sigma1^2 + sigma2^2)

# xi ~ N(mu, sigma^2)
# x1+...+xn -> (1/n)*SUM(xi) ~ N(mu, sigma^2/n)
# n -> inf => sigma^2/n -> 0 

y = replicate(1000,mean(rnorm(10,173,7)))
hist(y)

# sigma/sqrt(n) -> ñòàíäàðòíà ãðåøêà SE(x)

# (X1 - mu1)/sigma1 ~ N(0,1) ñòàíäàðòèçèðàíå
# P(-||- < q(0.95)) = 0.95 îò ñòîéíîñòèòå
# îò -q äî q -> 0.90

# (X1 - mu1)/(sigma1/sqrt(n)) ~ N(0,1)
# P(x/-(sigma/sqrt(n))*q < mu < x/+(sigma/sqrt(n))*q) = 0.90

f = function(n=100,mu=17,sigma=3) {
  x = rnorm(100,mu,sigma)
  m = mean(x)
  
  q = qnorm(0.95,0,1)
  return (m-(sigma/sqrt(n))*q < mu && mu < m+(sigma/sqrt(n))*q)
}

x = replicate(1000, f())
mean(x)
table(x)

# SD(x) = sqrt((1/n-1)*SUM(xi - x/)^2)
# dt,qt,pt,rt

g = function(n=100,mu=17,sigma=3) {
  x = rnorm(n,mu,sigma)
  ci = t.test(x)$conf.int
  
  return (ci[1] < mu && mu < ci[2])
}

y = replicate(1000, g())
mean(y)
table(y)

x = rnorm(1000,3,7)
t.test(x)

# task1

z = replicate(100, g(20,3,2)) # N(3,4) -> sigma = 4
mean(z)

# task2

x=c(10.0, 13.6, 13.2, 11.6, 12.5, 14.2, 14.9, 14.5, 13.4, 8.6, 11.5, 16.0, 14.2, 16.8, 17.9, 17.0)

# (1) > 30 => t.test(x)
# (2) < 30
# (2.1) ? x~N(mu,sigma^2) -> hist,lines(density)/boxplot/qqnorm,qqline/
                          # shapiro.test(x), p.value >= 0.05 -> it's normal

# àêî å íîðìàëíî -> t.test(x)
# ako íå å íîðìàëíî -> wilcox.test(x,conf.int=T,conf.level=0.95)
hist(x,probability=T)
lines(density(x))

boxplot(x)

qqnorm(x)
qqline(x)

shapiro.test(x)

t.test(x, conf.level = 0.90)
t.test(x, conf.level = 0.95)

# task3

install.packages("UsingR")
library("UsingR")

# à
hist(rat, probability = T)
lines(density(rat))

boxplot(rat)

qqnorm(rat)
qqline(rat)

shapiro.test(rat)

t.test(rat, conf.level = 0.96)

# á
length(exec.pay)

hist(exec.pay, probability = T)
lines(density(exec.pay))

shapiro.test(exec.pay)

wilcox.test(exec.pay,conf.int=T,conf.level = 0.96)

# task4

?prop.test

prop.test(87,150,conf.level = 0.92)

prop.test(870,1500,conf.level = 0.92)


