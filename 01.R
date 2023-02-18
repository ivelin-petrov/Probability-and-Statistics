# Practicum 1 - R

# vector
# matrix
# list
# string
# boolean
# function

# filter, map
# Âåêòîðè
# c -> combine

x = c(1,2,3,4,5) # ctrl + enter
y = c(11,22,33,44,55)
sum((2*(x + y)/3)^2)
cumsum((2*(x + y)/3)^2)

mean(x) # sum(x)/length(x)

z = c(x,y)

#index
x[4]
z[3]
z[-3]
z[c(1,3,8)]
z[-c(1,3,8)]

#áóëåâè ìàñêè
bools = z > 4
z[bools] # filter

bools = z %% 2 == 0
z[bools] # filter even num

# èíäåêñèòå çàïî÷âàò îò 1, à íå îò 0
# ðåäèöè
s = 1:100
# ?seq
s2 = seq(from = 1, to = 100, by = 1)
#s2 = seq(1,100,1)
s3 = seq(from = 1, to = 100, length.out = 300)

# çàäà÷à îò êîíòðîëíî
n = 100
x = 3.4
# sum of x^i, for i -> 1-n
sum(x^(1:n))

f = function(n,x){sum(x^(1:n))}

# info çà ïðîìåíëèâèòå
str("Nikola")
typeof("Nikola")
length("Nikola") # 1 -> vector

h = 50
length(h) # 1

# matrix
m = matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
?matrix
View(m)

# m[<i>,<j>]
m[2,3]
m[-2, c(1,3)]

# ñîðòèðàíå è íàðåäáà
v = c(6,7,3,2,1,5,7,9,4)
sort(v)
v[order(v)]

# sort matrix
m = matrix(v, nrow=3, ncol=3, byrow = TRUE)
# m[order(m[,1])]

index_first_col_sorted = order(m[,1])
m[index_first_col_sorted, ]

# èìåíóâàíå íà ìàòðèöà
colnames(m) = c("a","b","c")
rownames(m) = c("a2","b2","c2")

rbind(cbind(m,m))
cbind(rbind(m,m))

# list
l = list("model" = "Camaro", "speed" = 300)
df = data.frame(names = c("Ivan", "Dimitar", "Stoyan"), age = c(12,13,14))

install.packages("UsingR")
library("UsingR")

# min, max
which.max(z)
i = which(z %% 2 == 0)
z[i]
!(z %% 2 == 0)

# task 1
x = c(8,3,8,7,15,9,12,4,9,10,5,1)
m = matrix(x, nrow = 4, ncol = 3, dimnames = list(c("a","b","c","d")))

m2 = cbind(m, c(1,3,5,7))
# i = order(m2[ ,1])
i = order(m2[ ,1], m2[ ,2], decreasing = T)
m2[i, ]

# task 2
library(UsingR)
help(homedata)
View(homedata)

# homedata$y1970

index_of_max_price_2000 = which.max(homedata$y2000)
homedata[index_of_max_price_2000, ]
# homedata$y2000[index_of_max_price_2000]

index_of_min_price_2000 = which.min(homedata$y2000)
homedata[index_of_min_price_2000, ]

i = order(homedata$y2000, decreasing = TRUE)[1:5]
i[1:5] # i(c[1,2,3,4,5])
homedata[i, ]

# i = which(homedata$y2000 > 750000)
# length(i)

sum(homedata$y2000 > 750000)
# T + F = 1
# T + T + T = 3

is_pricy = homedata$y2000 > 750000
sum(is_pricy)
mean(homedata$y1970[is_pricy])

has_decreased = homedata$y1970 > homedata$y2000
homedata[has_decreased, ]
homedata$y2000[has_decreased]

perc_increase = ((homedata$y2000 - homedata$y1970) / homedata$y1970)
i = order(perc_increase, decreasing = TRUE)[1:10]
homedata[i, ]
View(homedata[i, ])

# task 3

install.packages("MASS")
library(MASS)
View(survey)
table(survey$Sex)
# survey$Sex[survey$Sex == "Male"]
sum(survey$Sex == "Male", na.rm = TRUE)
sum(survey$Sex == "Male" & survey$Smoke != "Never", na.rm = TRUE)

mean(survey$Height[survey$Sex == "Male"], na.rm = TRUE)
survey[order(survey$Age)[1:6], ]
survey$Height[order(survey$Age)[1:6]]
survey$Sex[order(survey$Age)[1:6]]


