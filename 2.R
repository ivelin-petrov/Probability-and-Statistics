library("UsingR")

survey$Sex

x <- factor(x = c("A", "A", "C", "D", "D"), levels = c("A", "C", "D"))

sum(x == "A")
table(x)

sum(x == "A") / length(x)
mean(x == "A")

sum(survey$Sex == "Male", na.rm = TRUE)

# полове на студенти
table(survey$Sex)
prop.table(table(survey$Sex))

table(survey$Sex, survey$Smoke)
prop.table(table(survey$Sex, survey$Smoke))
# условна вероятност по първа променлива
prop.table(table(survey$Sex, survey$Smoke), 1) # 9/118
prop.table(table(survey$Sex, survey$Smoke), 2) # 89/(89+99)

barplot(table(survey$Smoke))
barplot(table(survey$Sex, survey$Smoke), legend = T, beside = T)

barplot(prop.table(table(survey$Sex, survey$Smoke)), legend = T, beside = T)
# условна вероятност по пол (1)
barplot(prop.table(table(survey$Sex, survey$Smoke), 1), legend = T, beside = T)
# условна вероятност по пол (2)
barplot(prop.table(table(survey$Smoke, survey$Sex), 2), legend = T, beside = T)

pie(table(survey$Smoke))


# task1
sum(survey$Sex == "Male", na.rm = TRUE)
table(survey$Sex)
sum(survey$Sex == "Male" & survey$Smoke != "Never", na.rm = TRUE)
sum(table(survey$Smoke, survey$Sex)[-2,2])
# survey[, "Height"] == survey$Height
mean(survey$Height[survey$Sex == "Male"], na.rm = TRUE)
survey[order(survey$Age)[1:6], c("Height", "Sex")]
# sort(survey$Age)[1:6] -> стойности
# order(survey$Age)[1:6] -> индекси

# task2
sum(survey$Smoke == "Regul", na.rm = TRUE)
# sum(survey$Smoke == "Regul", na.rm = TRUE) / length(survey$Smoke[!is.na(survey$Smoke)])
mean(survey$Smoke == "Regul", na.rm = TRUE)
prop.table(table(survey$Smoke))[4]

prop.table(table(survey$Smoke, survey$Sex))["Regul","Male"]
prop.table(table(survey$Smoke, survey$Sex))[4,2]

# маргинализиране по пол
prop.table(table(survey$Smoke, survey$Sex), 2)[4,2]
# 12/118
prop.table(table(survey$Smoke, survey$Sex), 1)[4,2]
prop.table(table(survey$Smoke, survey$Sex), 1)["Regul","Male"]

# task3
barplot(table(survey$Smoke))
barplot(table(survey$Sex, survey$Smoke))
barplot(prop.table(table(survey$Sex, survey$Smoke)))

barplot(prop.table(table(survey$Smoke, survey$Sex),2), beside = T, legend = T)
barplot(prop.table(table(survey$Smoke, survey$Sex),1), beside = T, legend = T)


# task4
survey$AgeGroup = ifelse(survey$Age < 20, "A", ifelse(survey$Age < 25, "B", "C"))
#table(survey$AgeGroup)
barplot(table(survey$AgeGroup))
table(survey$AgeGroup, survey$Smoke)
#prop.table(table(survey$AgeGroup, survey$Smoke))
barplot(table(survey$AgeGroup, survey$Smoke), beside = T, legend = T)

prop.table(table(survey$AgeGroup, survey$Smoke), 2)
barplot(prop.table(table(survey$AgeGroup, survey$Smoke), 1), beside = T, legend = T)
barplot(prop.table(table(survey$AgeGroup, survey$Smoke), 2), beside = T, legend = T)


