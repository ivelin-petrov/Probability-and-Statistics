# x1, ..., xn
# y1, ..., yn
# Ho: E(x) = E(y)

# shapiro.test -> Ho

# t.test(x,y,paired = T,var.equal = F), n<30
# (x/ - E(x))/(sqrt(var(y)/n)) ~ N(0,1), n>30
# n<30 and not norm -> wilcox.test(-||-)

# pvalue < e^(-7) -> Ho: E(x) == E(y) -> Ha: E(x) =/= E(y)

# paired=T x1(-t->)x1', ..., xn(-t->)xn'

# task1

x=c(4,1,7,9)
y=c(10,3,2,11)
# n<30
hist(x,probability=T)
hist(y,probability=T)

qqnorm(x)
qqline(x)

shapiro.test(x) # pvalue -> norm
shapiro.test(y) # pvalue -> norm

t.test(x,y,paired=F,var.equal=T)
# wilcox.test
wilcox.test(x,y,paired=F,var.equal=T)

# task2

prop.test(351,605)
prop.test(71,195)
# Ho: p1==p2    | Ho: p1<=p2
# Ha: p1=/=p2   | Ha: p1>p2
prop.test(x=c(351,71),n=c(605,195),alternative="greater")

# task3

x=c(15,10,13,7,9,8,21,9,14,8) # medicine
y=c(15,14,12,8,14,10,7,16,10,15,12) # placebo
# n<30
hist(x)
boxplot(x)

hist(y)
qqnorm(y)
qqline(y)

shapiro.test(x)
shapiro.test(y)

t.test(x,y,paired=F,var.equal=T,alternative="less") 
# alpha = 0.10 -> pvalue > alpha => Ho
wilcox.test(x,y,paired=F,var.equal=T,alternative="less") 
# Ho: medicine >= placebo
# Ha: x => "less"

# alpha = 0.05 -> <= 0.05
# pvalue < alpha => alpha
# test

# task4

x=c(70,85,63,54,65,80,75,95,52,55)
y=c(72,86,62,55,63,80,78,90,53,57)

# n<30
hist(x)

qqnorm(x)
qqline(x)
shapiro.test(x)

hist(y)

qqnorm(y)
qqline(y)
shapiro.test(y)

t.test(x,y,paired=T,var.equal=T)
# wilcox.test(x,y,paired=T,var.equal=T)

# task5

library("UsingR")

x=ewr$AA
y=ewr$NW

hist(x)

qqnorm(x)
qqline(x)

hist(y)

qqnorm(y)
qqline(y)

# n>30
t.test(x,y,paired=F,var.equal=T)

# task6

# x, y -> website data
# n>30
t.test(x,y,paired=F,var.equal=T)

# task7

x=c(39,50,61,67,40,40,54)
y=c(60,53,42,41,40,54,63,69)

hist(x)
qqnorm(x)
qqline(x)
shapiro.test(x)

hist(y)
qqnorm(y)
qqline(y)
shapiro.test(y)

t.test(x,y,paired=F,var.equal=T,alternative="less")

# task8
# ...


