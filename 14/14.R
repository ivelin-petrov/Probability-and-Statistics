# E(x100) <= 17
# t.test(x,mu=17,alt = " ") greater/less

# E(x9) = 17
# провер€ваме дали е нормално
# t.test, wilcox.test

# x9, y9 -> 2 извадки
# E(x9) = E(y9)
# провер€ваме дали са нормални
# t.test - за нормално, wilcox.test - иначе
# t.test(x, mu=..., alt=" ", paired = T)

# x20(1), x20(2), x20(3), ..., x20(k)
# първо провер€ваме дали са нормално
# #? норм.
# oneway.test(x~group,data=df)
# function -> stack
# x/val -> ... | group/index -> 1.1...1, 2.2...2, ..., k.k...k

y=c(12,4,1,7,4,10)
n=length(y)
sy=1/n*sum(y) # == mean(y)

vy=1/(n-1)*sum((y-sy)^2) # == var(y)

# средно y/ = 1/n*SUM(yi)
# дисперси€ var(у) = 1/(n-1)*SUM(i=1,n)(yi-y/)^2
# Ho: b1=b2=...=bk=0 (моделът н€ма сила)

# f=(SSR/p)/(RSS/(n-p-1)) ~ разпред. F(p,n-p-1)
# деление на 2 статистики с хи квадрат разпред.
# F-statistic от summary

# n=nrow(mtcars)
# p=2 (предиктори)
# pf(F-statistic value, p, n-p-1, lower.tail = FALSE)

# anova test
# y ~ bo + b1.x1 + ... + bk.xk
# Ho: b1=b2=...=bk=0 (y не се вли€е от групите)

# oneway.test()
# провер€ваме дали една от статистиките се различава значително от останалите по средно (≈)

#task1

y = data.frame(p1=c(5,4,4,6,4,6,3,3,4,5),
               p2=c(3,2,4,5,3,4,3,4,2,4),
               p3=c(4,6,4,2,4,5,5,3,6,4))

# провер€ваме дали са norm
shapiro.test(y$p1)
shapiro.test(y$p2)
shapiro.test(y$p3)

sy=stack(y)
# alpha=0.05
oneway.test(values ~ ind, data=sy)
# не отхв. нулевата хипотеза

model = lm(values ~ ind, data = sy)
anova(model)
# anova(lm(values ~ ind, data = sy))

# ако данните не идват от нормално разпред.
# kruskal.test(-||-)

#task2

InsectSprays

a=InsectSprays$count[InsectSprays$spray == "A"]
b=InsectSprays$count[InsectSprays$spray == "B"]
c=InsectSprays$count[InsectSprays$spray == "C"]
d=InsectSprays$count[InsectSprays$spray == "D"]
e=InsectSprays$count[InsectSprays$spray == "E"]
f=InsectSprays$count[InsectSprays$spray == "F"]

shapiro.test(a)
shapiro.test(b)
shapiro.test(c) # not norm
shapiro.test(d) # not norm
shapiro.test(e)
shapiro.test(f)

shapiro.test(InsectSprays$count)

#oneway.test(count ~ spray, data=InsectSprays)
kruskal.test(count ~ spray, data=InsectSprays)

df = data.frame(c=InsectSprays$count[InsectSprays$spray == "C"],
                d=InsectSprays$count[InsectSprays$spray == "D"],
                e=InsectSprays$count[InsectSprays$spray == "E"])

kruskal.test(values ~ ind, data=stack(df))
anova(lm(values ~ ind, data=stack(df)))

#task3

results = read.csv("drug.txt")
shapiro.test(results$response)

oneway.test(response ~ drug, data = results)
kruskal.test(response ~ drug, data = results)

#task4

# y1,y2,group
# cbind(y1,y2) ~ group
# manova

# manova(cbind(y1,y2) ~ group,data=...)
# summary.aov()

iris # данни

m=manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data=iris)
summary.aov(m)



### преговор

# x=c(A,B,C...,A,D,A,B)
# y=c(a,a,b,...,c,a,d)

#P(X=x,Y=y)
prop.table(table(x,y))

#P(X=x|Y=y)
prop.table(table(x,y),2)

#t=table(y), a=7,b=3,c=2
#chisq.test(t) -> default за p = c(1/3,1/3,1/3) - с еднакво prob.
#chisq.test(t, p=c(0.3,0.2,0.5))

#chisq.test(x,y) -> тестваме за независимост
# \/x,y P(X=x/\Y=y)=P(X=x)*P(Y=y)
# Ho: тестваме за независимост на x и y

# x,group
# boxplot(x~group) ще направи boxplot за вс€ка група

x=c(10,7,5,5,12,3)
barplot(table(x))

# tidyverse -> group_by(df,gr)

# категорни данни - дискретни категории
# числови данни -> hist, boxplot(спр€мо друга пром.) 

#Ћинейна регреси€
# коеф. на наклона b1^=0.95 ~ N(b1,sigma^2)
# за x=1 съответства y=0.95

# Ho: b1 = 0.79
# t = (b1^ - b1?)/sd(b1^) ~ T(n-2)
# pt(t,df,lower.tail=F) -> за положителни
# pt(t,df,lower,tail=T) -> за отрицателни

# за теоретичните квантили -> qnorm
# за емпиричните квантили  -> на базата на данните -> quantile(x)


