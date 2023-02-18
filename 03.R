library("UsingR")

x = sample(c("A","B","C","D"), size = 100, replace = TRUE, prob = c(0.2,0.1,0.2,0.5))
barplot(table(x))

survey$Height
barplot(table(survey$Height))

plot(survey$Height)

library(MASS)

hist(survey$Height, breaks = 10)

my_heights = rnorm(1000)
hist(my_heights, breaks = 100, probability = TRUE)
lines(density(my_heights))

hist(runif(1000, -5, 5), breaks = 300)

median(survey$Height, na.rm = TRUE)
mean(survey$Height, na.rm = TRUE)

quantile(survey$Height, na.rm = TRUE)
boxplot(survey$Height)

boxplot(my_heights)

mean(survey$Height, na.rm = T)

var(survey$Height, na.rm = T)

sd(survey$Height, na.rm = T)

# mean(survey$Height, na.rm = T) - sd(survey$Height, na.rm = T)

split.screen(c(2,2))
screen(1)
boxplot(my_heights)

screen(2)
boxplot(survey$Height)

screen(3)
hist(survey$Height)

# boxplot
boxplot(survey$Height ~ survey$Sex)

# task1
median(survey$Height, na.rm = T)
m = mean(survey$Height, na.rm = T)
s = sd(survey$Height, na.rm = T)
quantile(survey$Height, na.rm = T)

quantile(survey$Height, 0.43, na.rm = T)

quantile(survey$Height, c(0.43, 0.67), na.rm = T)
quantile(survey$Height, seq(0, 1, by = 0.01), na.rm = T)

x = survey$Height
sum(x >= m-s & x < m+s, na.rm = TRUE)
mean(x >= m-s & x < m+s, na.rm = TRUE)

table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf)))
prop.table(table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf))))
table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf)))[2]
prop.table(table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf))))[2]

# library(dplyr)
# between(x, m-s, m+s)

summary(x)

# task2
# - boxplot
boxplot(survey$Height ~ survey$Sex)
# - hist
male_heights = survey$Height[survey$Sex == "Male"]
female_heights = survey$Height[survey$Sex == "Female"]

split.screen(c(2,1))
screen(1)
hist(male_heights, probability = T)
lines(density(male_heights, na.rm = T), col = "red")

screen(2)
hist(female_heights, probability = T)
lines(density(female_heights, na.rm = T), col = "green")

# task3
hist(survey$Pulse, probability = T)
lines(density(survey$Pulse, na.rm = T))

# task4
library("UsingR")
homedata
plot(homedata$y1970, homedata$y2000)
cor(homedata$y1970, homedata$y2000)

# 
split.screen(c(2,1))
screen(1)
hist(homedata$y1970, probability = T)
lines(density(homedata$y1970, na.rm = T), col = "red")

screen(2)
hist(homedata$y2000, probability = T)
lines(density(homedata$y2000, na.rm = T), col = "green")

# task5
anscombe

plot(anscombe)

plot(anscombe$x1, anscombe$y1)
cor(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
cor(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
cor(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)
cor(anscombe$x4, anscombe$y4)

boxplot(anscombe)

df = anscombe[c("x4", "y4")]
df
max(df$y4)
which.max(df$y4) # index
df[-which.max(df$y4), ]
new_df = df[-which.max(df$y4), ]
# cor
cor(new_df$x4, new_df$y4)
