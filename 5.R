sample(1:2,size=30,replace=TRUE,prob=c(3/4,1/4))

sample(1:6,size=30,replace=TRUE,prob=c(0.4,0.2,0.1,0.1,0.1,0.1))

# биномно разпределение

# p(x=k) = binom(n k)*(p^k)*(1-p)^(n-k) -> dbinom(k,n,p)
dbinom(5,20,1/6)

y=dbinom(0:20,20,1/6)
names(y)=0:20
barplot(y)

z=rbinom(220,20,1/6) # 20 attempts, 220 simulations, prob for success = 1/6

# емперична вероятност
mean(z==5) # sum(z==5)/220
t=table(z)
prop.table(t)
barplot(prop.table(t))

# теоретична вероятност
sum(dbinom(0:10,20,1/6)) # p(x<=10) = SUM(0:10)p(x=i)
pbinom(10,20,1/6)
pbinom(10,20,1/6,lower.tail=F) # връща лицето на p(x>10)

qbinom(0.75,20,1/6) # quantile стойности с вероятност по-малка или равна на 0.75
qbinom(0.75,20,1/6,lower.tail=F) # стойностите с вероятност по-голяма от 0.75

# поасоново разпределение - малковероятни събития
rpois(100,1/3) # симулиране
dpois(3,5)   # p(x=k) вероятност вектор от конкретни неща
ppois(7,3)   # p(x<=k)
qpois(6,1/3) # p(x<=?)=p

# геометрично разпределение - неуспехите до първия успех
# rgeom, dgeom, pgeom, qgeom

# хупергеометрично разпределение -> task3
# rhyper, dhyper, phyper, qhyper

# отрицателно биномно разпределение
# rnbinom, dnbinom, pnbinom, qnbinom

# task1
dbinom(0:5,30,1/6)
sum(dbinom(0:5,30,1/6))
pbinom(5,30,1/6)

x=rbinom(1000,30,1/6)
t=table(x<=5)
prop.table(t)
mean(x<=5)
prop.table(table(x))
pt=prop.table(table(x))
sum(pt[1:6])

qbinom(0.75,30,1/6,lower.tail=F)
qbinom(0.25,30,1/6)

# task2
dnbinom(5,3,0.2) # !!! fails = 5, successes = 3, n = 8 !!!
pnbinom(3,3,0.2,lower.tail=F) # 1-pnbinom(3,3,0.2) 1-(?<=6) -> (?>6)

sum(dnbinom(2:5,3,0.2)) # fails=2,3,4 or 5, successes = 3
pnbinom(5,3,0.2) - pnbinom(1,3,0.2)

# task3
# white+black=7+6=13, n=8 тегления
x = rhyper(1000,7,6,8)
summary(x)
sd(x) # стандартно отклонение

mean(x)
mean(x==3)
dhyper(3,7,6,8)

p=dhyper(2:7,7,6,8)
x=2:7

ex=sum(p*x)
dx=sum((x-ex)^2)

x = rhyper(1000,7,6,8)
pt=prop.table(table(x))
z=rbind(pt,p)
barplot(z,beside=T)
