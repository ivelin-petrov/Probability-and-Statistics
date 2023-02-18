# (x/ - mu)/(sigma/sqrt(n)) ~ N(0,1)

# y1,...,yn ~ Exp(lambda)
# (y/ - E(y))/(sqrt(var(y)/n)) ~ N(0,1), n > 30 

# var(y) = 1/(lambda^2)
# E(y) = 1/lambda

f = function(k=100){
  x = rexp(k,3)
  return (mean(x) - 1/3)/((1/3)/sqrt(k))
}

hist(replicate(1000,f()), probability=T)

#

#       two.sided     greater     less   
# Ho := mu? == mu  | mu? <= mu | mu? >= mu 
# Ha := mu? =/= mu | mu? > mu  | mu? < mu

# alpha = 0.05

# sqrt(1/(n-1)*SUM(xi - x\)^2) -> sqrt(E(x - E(x))^2)

# T(n-1), pt, qt, rt, dt
# t.test(x, mu=3, alternative = ...)

# task1

g = function(n=10,h=5){
  x = rnorm(n,2,2)
  m=mean(x)
  s=sd(x)
  t = (m-h)/(s/sqrt(n)) # T(n-1)
  
  pvalue=pt(t,n-1)
  print(2*pvalue) # < alpha -> mu=5
  
  t.test(x,mu=h,alternative="two.sided")
}

g(100)
g(30,3)

# n<30  x~N(mu,sigma^2) => t.test
# n>30  x~not~N(mu,sigma^2) => t.test
# n<30  x~not~N(mu,sigma^2) => wilcox.test(x,mu=3,alternative="two.sided")

# task2

library("UsingR")

length(vacation)
hist(vacation, probability=T)

t.test(vacation, mu=24, alternative = "two.sided")
t.test(vacation, mu=24, alternative = "less")

# task3

prop.test(42,100,0.5,alternative="less") # alpha > 0.0669
prop.test(420,1000,0.5,alternative="less")

# task4

x = c(12.8, 3.5, 2.9, 9.4, 8.7, 0.7, 0.2, 2.8, 1.9, 2.8, 3.1, 15.8)

# n<30
hist(x,probability=T)
lines(density(x))

boxplot(x)

qqnorm(x)
qqline(x)

shapiro.test(x) 
# pvalue = 0.05 => 0.51 !< pvalue
wilcox.test(x,mu=5,alternative="greater")

# task5

library("UsingR")
boxplot(cancer)

# stomach, bronchus, colon, ovary, breast

x=cancer$stomach
hist(x)

# not norm, n<30
wilcox.test(x,mu=100,alternative="less") #Ho -> "less"
wilcox.test(x,mu=100,alternative="greater") #Ho -> "greater"

# task6

library("UsingR")

sum(survey$Sex=="Male",na.rm=T)

prop.table(table(survey$Sex,survey$Smoke),1)

male_smoking = survey$Smoke[survey$Sex == "Male"]
n=length(male_smoking) # 118
x=sum(male_smoking != "Never", na.rm = T)

# n>30
prop.test(28,118,p=0.2,alternative="greater")
# Ho := p<=0,2, Ha := p>0.2


