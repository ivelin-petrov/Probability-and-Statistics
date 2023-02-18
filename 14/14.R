# E(x100) <= 17
# t.test(x,mu=17,alt = " ") greater/less

# E(x9) = 17

# t.test, wilcox.test

# x9, y9
# E(x9) = E(y9)

# t.test, wilcox.test
# t.test(x, mu=..., alt=" ", paired = T)

# x20(1), x20(2), x20(3), ..., x20(k)

# oneway.test(x~group,data=df)
# function -> stack
# x/val -> ... | group/index -> 1.1...1, 2.2...2, ..., k.k...k

y=c(12,4,1,7,4,10)
n=length(y)
sy=1/n*sum(y) # == mean(y)

vy=1/(n-1)*sum((y-sy)^2) # == var(y)

# y/ = 1/n*SUM(yi)
# var(ó) = 1/(n-1)*SUM(i=1,n)(yi-y/)^2
# Ho: b1=b2=...=bk=0 

# f=(SSR/p)/(RSS/(n-p-1)) ~> F(p,n-p-1)

# F-statistic summary

# n=nrow(mtcars)
# p=2
# pf(F-statistic value, p, n-p-1, lower.tail = FALSE)

# anova test
# y ~ bo + b1.x1 + ... + bk.xk
# Ho: b1=b2=...=bk=0

# oneway.test()

#task1

y = data.frame(p1=c(5,4,4,6,4,6,3,3,4,5),
               p2=c(3,2,4,5,3,4,3,4,2,4),
               p3=c(4,6,4,2,4,5,5,3,6,4))

shapiro.test(y$p1)
shapiro.test(y$p2)
shapiro.test(y$p3)

sy=stack(y)
# alpha=0.05
oneway.test(values ~ ind, data=sy)

model = lm(values ~ ind, data = sy)
anova(model)
# anova(lm(values ~ ind, data = sy))

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

iris # äàííè

m=manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data=iris)
summary.aov(m)



###

# x=c(A,B,C...,A,D,A,B)
# y=c(a,a,b,...,c,a,d)

#P(X=x,Y=y)
prop.table(table(x,y))

#P(X=x|Y=y)
prop.table(table(x,y),2)

#t=table(y), a=7,b=3,c=2
#chisq.test(t) -> default p = c(1/3,1/3,1/3)
#chisq.test(t, p=c(0.3,0.2,0.5))

#chisq.test(x,y)
# \/x,y P(X=x/\Y=y)=P(X=x)*P(Y=y)
# Ho

# x,group
# boxplot(x~group)

x=c(10,7,5,5,12,3)
barplot(table(x))

# tidyverse -> group_by(df,gr)

# hist, boxplot 

# b1^=0.95 ~ N(b1,sigma^2)
# x=1, y=0.95

# Ho: b1 = 0.79
# t = (b1^ - b1?)/sd(b1^) ~ T(n-2)
# pt(t,df,lower.tail=F)
# pt(t,df,lower,tail=T)

# qnorm, quantile(x)


