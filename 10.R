# t = (x/ - 5)/(s/sqrt(n)) ~ T(n-1)
# за mu = 5, ако s = sigma -> нормално разпр.

# SUM(i=1,n)((Oi-Ei)^2/Ei) ~ Хi^2(n-1)
# Oi - извадка, Hi - очаквани стойности
# Ho = Ei, ако pvalue < alpha - отхвърляме хипотезата Ho

x=rchisq(1000,2) # 2 степени на свобода
hist(x)

# dchisq (плътност), rchisq, pchisq, qchisq

# task1

o=c(125,410,310,300,318,298,148)
e=mean(o)

xcq = sum((o-e)^2/e)
pchisq(xcq,6,lower.tail = FALSE) # степени на свобода n-1 = 6
# => отхв. Ho за равни стойности всеки ден

chisq.test(o) # p = c(1/7,...,1/7) default

# task2

library(UsingR)

p = pi2000[1:200]
obs_p = table(p)

chisq.test(obs_p)
# не успяваме да отхв. хипотезата

# task3

obs = c(102,108,90,95,82,40)
other = 1036-sum(obs)

obs = c(102,108,90,95,82,40,other)
prob = c(0.127,0.0956,0.0817,0.0751,0.0697,0.0675,0.4834)

chisq.test(obs,p=prob)
# отхв. хипотезата, че текстът е на английски

# task4

# P(XY) = P(X)*P(Y) -> независими
# Vx,Vy: P(X=x,Y=y)=P(X=x)*P(Y=y)
# таблица X/Y (3,3) -> P(X=x1) = P(X=x1 /\ (Y=y1 \/ Y=y2 \/ Y=y3)) = 
  # = SUM(i=1,3)(P(X=x1 /\ Y=yi))

# X -> с колан, без колан
# Y -> (наранявания) без, леки, средни, тежки
# P(без нар. /\ без колан) = P(без нар.)*P(без колан) -> незав.
# ако са независими => SUM(i,r)SUM(j,c)((Oij - Eij)^2/Eij) ~ Xi^2(r-1)(c-1)
# chisq.test(table(x,y))

m = matrix(c(12813,647,359,42,65963,4000,2642,303),nrow=2,byrow=TRUE)
chisq.test(m) # автоматично тестване за независимост
# отхв. Ho за независимост на данните
# с Ho винаги тестваме за независимост

# task5

m = matrix(c(44,74,79,72,31,14,25,27,24,10,15,20,20,23,9,3,5,5,0,0),
           nrow=4,byrow=TRUE)
chisq.test(m)
# не успяваме да отхв. Ho за независимост

# task6

# диск. равномерно разпределение -> равни вероятности
# чрез p пробваме кое разпределение наблюдаваме

# пример биномно -> prob=dbinom(0:6,6,0.5) -> теоретични вер.
# n*prob -> SUM((ni-n*prob)^2/n*prob) ~ Хi(6)

n=1000
x=rbinom(n,6,0.5)
t=table(x)
p=dbinom(0:6,6,0.5)
s=sum((t-n*p)^2/n*p)

pchisq(s,df=6,lower.tail=FALSE)
chisq.test(t,p=p)

# непрекъснати случ. вел.
# y=cuts(x,seq(0,20,by=2))
# table(y) -> аналогично на горното
# Ho: X~N(mi,sigma^2), взимаме стойностите в интервалите с
  # pnorm(x2,mu,sigma^2) - pnorm(x1,mu,sigma^2) -> vector p
  # chisq.test(t,p=p)




