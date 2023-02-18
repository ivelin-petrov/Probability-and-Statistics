# log(p(x)/(1-p(x))) = b0 + b1*h
# p(x;b0,b1) = 1/(1+exp^-(b0+b1*xi))

# argmax(b0,b1)(PROD(p(xi;b0,b1)))

# метод на максимално правдоподобие
# класификация
# glm(Y ~ X, data)

# разделяне на данните
# чертаем графика въз основа на 80% от данните
# 20% от данните -> търсим accuracy = SUM(i=1,n)(yi^ = yi)

#task1

# read.csv("path/students.txt), getwd(), setwd()
students_df = read.csv("students.txt")
plot(students_df)

n = nrow(students_df)
train_idx = sample(1:n, size = 0.8*n, replace = FALSE)

train_set = students_df[train_idx, ] # 80%
test_set = students_df[-train_idx, ] # 20%

mean(students_df$admit == 1)

model = glm(admit ~ gre + gpa + rank, data = train_set)
summary(model) # AIC -> min (информ. загуба)

result_prob = predict.glm(model, newdata = test_set)
result = 0 + (result_prob > 0.5)

accuracy = mean(test_set$admit == result)
accuracy

first_type_error = mean((test_set$admit == 0) & (result == 1))
first_type_error

second_type_error = mean((test_set$admit == 1) & (result == 0))
second_type_error

predict_df = data.frame(gre = c(700), gpa = c(3.5), rank = c(2))
predict.glm(model, newdata = predict_df, type = "response")

#task2

install.packages("mlbench")
library("mlbench")
data(BreastCancer)

mean(BreastCancer$Class == "malignant")

n = nrow(BreastCancer)
train_idx = sample(1:n, size = 0.8*n, replace = FALSE)

train_set = BreastCancer[train_idx, ] # 80%
test_set = BreastCancer[-train_idx, ] # 20%

y = 0 + (train_set$Class == "malignant")

model = glm(y ~ Epith.c.size + Mitoses,data = train_set)

result_prob = predict.glm(model, newdata = test_set)
result = 0 + (result_prob > 0.5)

accuracy = mean((0 + (test_set$Class == "malignant")) == result)
accuracy

z = 0 + (test_set$Class == "malignant")

first_type_error = mean((z == 0) & (result == 1))
first_type_error

second_type_error = mean((z == 1) & (result == 0))
second_type_error


