#task1

#install.packages("car")
library(car)

d = function(p){
  shots = c(8,5,12,11,12,8,6,7,11,7,11,13,15,
            12,17,12,9,15,8,11,11,13,10,8,12,12,11,
            13,12,14,9,11,13,10,10,12,13,10,15,12,15,12)
  
  return (dbinom(shots,size=30,prob=p,log=TRUE))
}

s=seq(0,1,length.out=42)

z=sapply(s,d)

#fixed prob = 0.09756098
scatterplot((1:42),z[,5])

#task2

#mtcars

# most horsepower -> mtcars[,4]
x=order(mtcars$hp,decreasing=TRUE)[1]
mtcars[x,]

# least weight -> mtcars[,6]
y=order(mtcars$wt)[1:5]
mtcars[y,]

# miles/gallon ~ number of cylinders
boxplot(mtcars$mpg ~ mtcars$cyl,xlab="Number of Cylinders",ylab="Miles Per Gallon")

plot(mtcars$hp,mtcars$mpg,xlab="Horsepower",ylab="Miles Per Gallon")
# correlation
cor(mtcars$hp,mtcars$mpg) # = -0.7761684

# quantile
quantile(mtcars$hp,0.80) # >200
quantile(mtcars$hp,0.27)

m=mean(mtcars$hp)
s=sd(mtcars$hp)

# p(x>?)=p, lower.tail=FALSE
qnorm(0.2,mean=m,sd=s,lower.tail=FALSE) #~204
# p(x<=?)=p
#qnorm(0.8,mean=m,sd=s,lower.tail=TRUE)

# p(x<k)
pnorm(100,mean=m,sd=s) #~0.25

prop.table(table(mtcars$cyl,mtcars$gear),2)
barplot(prop.table(table(mtcars$cyl,mtcars$gear),2),beside=T,legend=T)

# P({num. of cyl. = 8} | {num. of gears = 5}) = 0.40
prop.table(table(mtcars$cyl,mtcars$gear),2)[3,3]

#task3

#неточно решение
f = function(){
  for (n in 20:150) {
    x=table(sample(1:20,size=n,replace=TRUE))
    if (nrow(x)==20) {
      return (n)
    }
  }
}

g = function(){
  all_purchases = 0
  result = c()
  while(nrow(table(result)) != 20){
    one_purchase = sample(1:20,size=1,replace=TRUE)
    result = append(result, one_purchase)
    all_purchases = all_purchases + 1
  }
  #print(table(result))
  return (all_purchases)
}

replicate(10,g())

