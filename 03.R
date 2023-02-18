library("UsingR")

x = sample(c("A","B","C","D"), size = 100, replace = TRUE, prob = c(0.2,0.1,0.2,0.5))
barplot(table(x))

survey$Height
barplot(table(survey$Height)) # ãðåøíî

plot(survey$Height)

library(MASS)
# õèñòîãðàìà
hist(survey$Height, breaks = 10)

my_heights = rnorm(1000)
hist(my_heights, breaks = 100, probability = TRUE)
lines(density(my_heights))

hist(runif(1000, -5, 5), breaks = 300)

# êâàíòèëè è ìåäèàíè(ñðåäåí åëåìåíò = 2-ðè êâàíòèë (50%))
median(survey$Height, na.rm = TRUE)
mean(survey$Height, na.rm = TRUE)

quantile(survey$Height, na.rm = TRUE) # ìåäèàíà 50% (äåáåëà ÷åðíà ëèíèÿ), 75% - 3-òè êâàíòèë
boxplot(survey$Height) # êâàíòèëè, ñèâà ÷àñò = 50% (îò 25% äî 75%)

boxplot(my_heights) # èìàìå ìàðãèíàëíè (èçêëþ÷èòåëíè) ñòîéíîñòè ïîä è íàä ïîñëåäíèòå ÷åðòè (whiskers)

mean(survey$Height, na.rm = T)
# äèñïåðñèÿ - ñðåäíî ðàçñòîÿíèå îò ñðåäíîòî 1/n-1 * SUM(xi - X), X - ñðåäíà ñòîéíîñò (mean)
var(survey$Height, na.rm = T)

# ñòàíäàðòíî îòêëîíåíèå - sqrt(var)
sd(survey$Height, na.rm = T)
# ñðåäíî - 1 * äèñïåðñèÿ, ñðåäíî + 1 * äèñïåðñèÿ

# mean(survey$Height, na.rm = T) - sd(survey$Height, na.rm = T)

split.screen(c(2,2))
screen(1)
boxplot(my_heights)

screen(2)
boxplot(survey$Height)

screen(3)
hist(survey$Height)

# boxplot â çàâèñèìîñò îò êàòåãîðíà ïðîìåíëèâà
boxplot(survey$Height ~ survey$Sex)

# task1
median(survey$Height, na.rm = T)
m = mean(survey$Height, na.rm = T)
s = sd(survey$Height, na.rm = T) # ñòàíäàðòíî îòêëîíåíèå
quantile(survey$Height, na.rm = T) # êâàíòèëè

# âèñî÷èíàòà, òàêàâà ÷å 43% îò õîðàòà ñà ïî-íèñêè îò íåÿ
quantile(survey$Height, 0.43, na.rm = T)

quantile(survey$Height, c(0.43, 0.67), na.rm = T)
quantile(survey$Height, seq(0, 1, by = 0.01), na.rm = T)

# áðîéêà íà ïîïàäàùèòå ìåæäó (m-s,m+s)
x = survey$Height
sum(x >= m-s & x < m+s, na.rm = TRUE) # áóëåâà ìàñêà
mean(x >= m-s & x < m+s, na.rm = TRUE)

# âòîðè íà÷èí
table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf)))
prop.table(table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf))))
table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf)))[2]
prop.table(table(cut(x, breaks = c(-Inf, m-s, m+s, +Inf))))[2]

# library(dplyr)
# between(x, m-s, m+s)

# îáîáùàâàíå
summary(x)

# task2
# - boxplot ñïðÿìî ìúæå è æåíè
boxplot(survey$Height ~ survey$Sex)
# - hist çà ìúæå è æåíè íà åäèí åêðàí
male_heights = survey$Height[survey$Sex == "Male"]
female_heights = survey$Height[survey$Sex == "Female"]

split.screen(c(2,1))
screen(1)
hist(male_heights, probability = T)
lines(density(male_heights, na.rm = T), col = "red")

screen(2)
hist(female_heights, probability = T)
lines(density(female_heights, na.rm = T), col = "green") # ëèíèè çà ïëúòíîñò

# task3
hist(survey$Pulse, probability = T)
lines(density(survey$Pulse, na.rm = T))

# task4
# êîðåëàöèÿ
library("UsingR")
homedata
plot(homedata$y1970, homedata$y2000) # òàáëèöà çà êîðåëàöèÿòà
cor(homedata$y1970, homedata$y2000)

# çàåäíî
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
max(df$y4) # ñòîéíîñò
which.max(df$y4) # index
df[-which.max(df$y4), ]
new_df = df[-which.max(df$y4), ]
# cor - äàâà ãðåøêà, çàðàäè ñòàíäàðòíîòî îòêëîíåíèå íà x, êîåòî å 0 (ðàâíè ñòîéíîñòè)
cor(new_df$x4, new_df$y4)
