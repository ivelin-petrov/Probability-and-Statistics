# x-> = (x1,...,xn)
# y-> = (y1,...,yn)

# yi = b0 + b1*xi + ei
# ei ~ N(0,sigma^2)

# yi/\ = b0/\ + b1/\*x

# model = lm(score ~ hours, data=lp)
# plot(model)

# x' = x^2
# y ~ b0 + b1*x'
# lm(y ~ x^2)

# lm(y ~ x1+x2,...), (score,hours,iq)
# plot, scatterplot

# summary(model) -> R^2, R^2 adj.



#task1
plot(mtcars)

plot(mtcars$disp, mtcars$mpg)
cor(mtcars$disp, mtcars$mpg)

init = lm(mpg ~ 1, data=mtcars)
summary(init)
#for(var in vars\{mpg}) {m = lm(y~var), if...}

summary(lm(mpg ~ disp, data=mtcars))
summary(lm(mpg ~ cyl, data=mtcars))
summary(lm(mpg ~ drat, data=mtcars))
summary(lm(mpg ~ wt, data=mtcars)) # best R^2 adj.

summary(lm(mpg ~ log(disp), data=mtcars))
summary(lm(mpg ~ log(disp) + wt, data=mtcars))

summary(lm(mpg ~ log(disp) + log(hp), data=mtcars))
summary(lm(mpg ~ log(disp) + log(wt), data=mtcars)) 

#task2

# session -> set working directory -> choose directory
read.csv2("<filename>")
read.csv("Height.txt", sep = "\t", dec = ".")

height_df = read.csv("Height.txt", sep = "\t")
plot(height_df)

height_cm_df = 2.54 * height_df
plot(height_cm_df)

model = lm(Height ~ dadheight, data = height_cm_df)

plot(height_cm_df$dadheight, height_cm_df$Height)
plot(height_cm_df$momheight, height_cm_df$Height)
abline(model, col = "red")

model_summary = summary(model)
model_summary$adj.r.squared

#model = lm(Height ~ dadheight + momheight, data = height_cm_df)
model = lm(Height ~ ., data = height_cm_df)

predict.lm(model, 
           newdata = data.frame(momheight = c(160,162,166),
                                dadheight = c(176,180,185)),
           interval = "confidence",
           level = 0.98)

model_summary = summary(model)
model_summary$adj.r.squared

# rmse = sqrt((SUM(yi-yi/\)^2)/n) = sqrt(1/n*SUM(ei/\^2))
# MAE = 1/n*SUM(i=1,n)|yi-yi/\|

# training set/test set
# x1,x2,...,xn / x1',x2',...,xn'
# y1,y2,...,yn / y1',y2',...,yn'
# ei' = yi' - yi/\

# train_index = sample(1:n, size = n*0.8, replace = T)
# train_set = df[train_index, ]
# test_set = df[-train_index, ]

#task3
data_df = data.frame(
  height = c(100,200,300,450,600,800,1000),
  dist = c(253,337,395,451,495,534,574)
)

model = lm(dist ~ I(1/height), data=data_df) 
# x^2, I(1/x), log(x), sqrt(x)
model = lm(dist ~ log(height), data=data_df) # R^2 adj. = 0.9973
# çà 1*log(height) -> 139.3 dist
summary(model)

plot(data_df$height, data_df$dist)
#abline(model, col = "red")
points(x = df$height, y = model$fitted.values, col = "red", type = "o")

predict.lm(model, 
           newdata = data.frame(height = c(501,503,510)),
           interval = "confidence",
           level = 0.86)

#task4
library(UsingR)

lm(list ~., data=homeprice)

model = lm(list ~ rooms, data=homeprice)
summary(model)

lm(list ~ full + half, data=homeprice)
lm(list ~ full, data=homeprice)
lm(list ~ half, data=homeprice)

plot(homeprice)


