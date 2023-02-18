#

f = function(n = 100){
  dice = sample(1:6, size = n, replace = TRUE)
  dice_df = as.data.frame(table(dice))
  chisq.test(dice_df[,2], p = c(1/6,1/6,1/6,1/6,1/6,1/6))$p.value
}

rep1 = replicate(10000, f())
rep2 = replicate(10000, f(200))
rep3 = replicate(10000, f(400))

sum(rep1 < 0.05)
sum(rep2 < 0.05)
sum(rep3 < 0.05)

#

g = function(n = 15){
  data = runif(n,min=5,max=12)
  shapiro.test(data)$p.value
}

rep1 = replicate(10000,g())
rep2 = replicate(10000,g(25))
rep3 = replicate(10000,g(50))
rep4 = replicate(10000,g(90))

#

sum(rep1 < 0.05)
sum(rep2 < 0.05)
sum(rep3 < 0.05)
sum(rep4 < 0.05)
#
sum(rep1 >= 0.05)
sum(rep2 >= 0.05)
sum(rep3 >= 0.05)
sum(rep4 >= 0.05)

#

h = function(n=30,case=1){
  x = sample(1:10,size=n,replace=TRUE)
  
  if(case == 1){
    eps = rnorm(n,mean = 0,sd = 2)      #(à)
  }else if(case == 2){
    eps = runif(n,min = -3.5,max = 3.5) #(á)
  }else if(case == 3){
    v = rexp(n,0.7)
    w = rexp(n,0.7)
    eps = v - w                         #(â)
  }else{
    u = rexp(n,1/2)
    eps = u - 2                         #(ã)
  }
  
  y = 5*x + eps
  
  model = lm(y ~ x)
  b1_est = summary(model)$coefficients[2,1]
  b1_sd = summary(model)$coefficients[2,2]
  
  q <- pt(0.975, df = n - 2)
  
  left = b1_est - q*b1_sd
  right = b1_est + q*b1_sd
  
  return (c(right-left,b1_est))
}

k=10000

###
rep_a1 = replicate(k,h(n=30,case=1))
# 2)
mean(rep_a1[1,])
# 3)
mean(rep_a1[2,])
# 4)
hist(rep_a1[2,], xlab = "b1_est")
qqnorm(rep_a1[2,])
qqline(rep_a1[2,], col = "red")

rep_a2 = replicate(k,h(n=50,case=1))
# 2)
mean(rep_a2[1,])
# 3)
mean(rep_a2[2,])
# 4)
hist(rep_a2[2,], xlab = "b1_est")
qqnorm(rep_a2[2,])
qqline(rep_a2[2,], col = "red")

rep_a3 = replicate(k,h(n=100,case=1))
# 2)
mean(rep_a3[1,])
# 3)
mean(rep_a3[2,])
# 4)
hist(rep_a3[2,], xlab = "b1_est")
qqnorm(rep_a3[2,])
qqline(rep_a3[2,], col = "red")

rep_a4 = replicate(k,h(n=500,case=1))
# 2)
mean(rep_a4[1,])
# 3)
mean(rep_a4[2,])
# 4)
hist(rep_a4[2,], xlab = "b1_est")
qqnorm(rep_a4[2,])
qqline(rep_a4[2,], col = "red")


###
rep_b1 = replicate(k,h(n=30,case=2))
# 2)
mean(rep_b1[1,])
# 3)
mean(rep_b1[2,])
# 4)
hist(rep_b1[2,], xlab = "b1_est")
qqnorm(rep_b1[2,])
qqline(rep_b1[2,], col = "green")

rep_b2 = replicate(k,h(n=50,case=2))
# 2)
mean(rep_b2[1,])
# 3)
mean(rep_b2[2,])
# 4)
hist(rep_b2[2,], xlab = "b1_est")
qqnorm(rep_b2[2,])
qqline(rep_b2[2,], col = "green")

rep_b3 = replicate(k,h(n=100,case=2))
# 2)
mean(rep_b3[1,])
# 3)
mean(rep_b3[2,])
# 4)
hist(rep_b3[2,], xlab = "b1_est")
qqnorm(rep_b3[2,])
qqline(rep_b3[2,], col = "green")

rep_b4 = replicate(k,h(n=500,case=2))
# 2)
mean(rep_b4[1,])
# 3)
mean(rep_b4[2,])
# 4)
hist(rep_b4[2,], xlab = "b1_est")
qqnorm(rep_b4[2,])
qqline(rep_b4[2,], col = "green")

###
rep_c1 = replicate(k,h(n=30,case=3))
# 2)
mean(rep_c1[1,])
# 3)
mean(rep_c1[2,])
# 4)
hist(rep_c1[2,], xlab = "b1_est")
qqnorm(rep_c1[2,])
qqline(rep_c1[2,], col = "blue")

rep_c2 = replicate(k,h(n=50,case=3))
# 2)
mean(rep_c2[1,])
# 3)
mean(rep_c2[2,])
# 4)
hist(rep_c2[2,], xlab = "b1_est")
qqnorm(rep_c2[2,])
qqline(rep_c2[2,], col = "blue")

rep_c3 = replicate(k,h(n=100,case=3))
# 2)
mean(rep_c3[1,])
# 3)
mean(rep_c3[2,])
# 4)
hist(rep_c3[2,], xlab = "b1_est")
qqnorm(rep_c3[2,])
qqline(rep_c3[2,], col = "blue")

rep_c4 = replicate(k,h(n=500,case=3))
# 2)
mean(rep_c4[1,])
# 3)
mean(rep_c4[2,])
# 4)
hist(rep_c4[2,], xlab = "b1_est")
qqnorm(rep_c4[2,])
qqline(rep_c4[2,], col = "blue")


###
rep_d1 = replicate(k,h(n=30,case=4))
# 2)
mean(rep_d1[1,])
# 3)
mean(rep_d1[2,])
# 4)
hist(rep_d1[2,], xlab = "b1_est")
qqnorm(rep_d1[2,])
qqline(rep_d1[2,], col = "purple")

rep_d2 = replicate(k,h(n=50,case=4))
# 2)
mean(rep_d2[1,])
# 3)
mean(rep_d2[2,])
# 4)
hist(rep_d2[2,], xlab = "b1_est")
qqnorm(rep_d2[2,])
qqline(rep_d2[2,], col = "purple")

rep_d3 = replicate(k,h(n=100,case=4))
# 2)
mean(rep_d3[1,])
# 3)
mean(rep_d3[2,])
# 4)
hist(rep_d3[2,], xlab = "b1_est")
qqnorm(rep_d3[2,])
qqline(rep_d3[2,], col = "purple")

rep_d4 = replicate(k,h(n=500,case=4))
# 2)
mean(rep_d4[1,])
# 3)
mean(rep_d4[2,])
# 4)
hist(rep_d4[2,], xlab = "b1_est")
qqnorm(rep_d4[2,])
qqline(rep_d4[2,], col = "purple")


