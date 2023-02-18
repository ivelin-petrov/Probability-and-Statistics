# y(i) = b(1).x(i) + eps(i) + b(0)
# eps(i) ~ N(0,sigma^2)

# argmin p(y^(->), y(->)) = p(b(0)^ + b(1)^.x(->), y(->))^2
# y^(->) = b(0)^(->) + b(1)^(->).x(->)

# loss = (yi - (b0 + b1.xi))^2
# cost = SUM(loss) = SUM(yi - (b0 + b1.xi))^2

# b0^ = y/ - b1^.x/
# b1^ = (SUM((xi - x/).(yi - y/))/(SUM(xi - x/)^2)

# linear model
# lm(y ~ x, data=df), lm(y~x)
# lm(sales ~ tv)


#task1

people_df = data.frame(
  age = c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37),
  pulse = c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)
)

plot(people_df)

model = lm(formula = pulse ~ age, data = people_df)

model_summary = summary(model)

# t = (b0^ - 220)/sd(b0^) ~ T(n-2)

# t = (b1^ - (-1))/sd(b1^) ~ T(n-2)

n = nrow(people_df)
beta0_est = model$coefficients[1]
beta0_hyp = 220
beta0_se = model_summary$coefficients[1,2]

t = (beta0_est - beta0_hyp) / beta0_se

pt(t,n-2)

# y = b0 + b1.x
beta1_est = model$coefficients[2] # == model_summary$coefficients[2,1]
beta1_hyp = -1
beta1_se = model_summary$coefficients[2,2]

t2 = (beta1_est - beta1_hyp) / beta1_se

hist(rt(1000,n-2),probability = TRUE)
pt(t2,n-2,lower.tail = FALSE)

# t = (b1^ - 0)/sd(b0^)
# summary(model) -> H0
# pvalue

# RSE = sqrt(SUM(yi-yi^)^2)
# R-squared
# Adjusted R-squared

# y^(x=30) = b0^ + b1^.30
x = 30 # x = 30,40,50
beta0_est + beta1_est*x

predict.lm(model, newdata = data.frame(age=c(30,40,50)))

predict.lm(model, newdata = data.frame(age=c(30,40,50)),
           interval = "confidence", level = 0.90)

# y = b0 + b1.log(x)
# y = b0 + b1.x^2

lm(pulse ~ age^2, data = people_df)
lm(pulse ~ log(age), data = people_df)

#task2

#model = lm(formula = mtcars$mpg ~ mtcars$cyl)
#model = lm(formula = mtcars$disp ~ mtcars$wt)
model = lm(formula = mtcars$mpg[mtcars$cyl == 4] ~ mtcars$disp[mtcars$cyl == 4])
model = lm(formula = mtcars$hp[mtcars$cyl == 8] ~ mtcars$disp[mtcars$cyl == 8])
summary(model)

table(mtcars$cyl)


