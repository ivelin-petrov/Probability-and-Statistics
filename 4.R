 f = function(x=3,y=4){return (function(z){})}

 # f()
 # f()(2) -> 9
 
 # f2 = function(x=3,y=4){return x+y}
 
 table(sample(c("B","W"),size=100,replace=TRUE))
 
 x = sample(c("B",'W'),size=100,replace=TRUE,prob=c(0.2,0.8))
 table(x)
 sum(x=="B")
 # mean(x=="B")
 
 # install.package(tidyverse)
 # library(tidyverse)
 
 # keep(x, function(y), y>4) # filter
 # map(vector, ~.x^2)
 # cumsum(x) # 1,7,9,3 -> 1,8,17,20
 # cummean(x) -> средно на първи елем., средно на първите два, средно на първите три и т.н.
 
 replicate(1000,sum(sample(c("B","W"),size=100,replace=TRUE)=="B"))
 
 # detect(x,~.x>7) # връща първия елемент, който изпълнява някакво условие
 
 # detect(x,~.x>7)
 # detectIndex(c(4,9,1))
 
 # for(i in 1:100){}
 
 # task1
 f=function(n=100){
   x=sample(1:6,size=n,replace=TRUE)
   mean(x==6)
 }
 # f()
 
 install.packages("tidyverse")
 library(tidyverse)
 
 x = replicate(1000,f())
 y = cummean(x)
 plot(y,type="l")
 abline(h=1/6,col="red")

 f1=function(x=3,n=5){sum((x^(1:n))/(1:n))} 
 
 # task2
 library(tidyverse)
 # detect_index
 f2=function(p=0.5){detect_index(cumprod((365:1)/365),~.x<1-p)}
 
 bd=function(p){
   result=1
   for(i in 1:365){
     result=result*((365-i)/365)
     if(result<1-p){
       return(i+1)
     }
     
   }
 }
 
 # task3
 tenis=function(father=0.3,mother=0.4){
   x=c((mother*father+(1-mother)*father*mother),
       (father*mother+(1-father)*mother*father))
   which.max(x)
 }
 
 # task5
 coin=function(k=100){
   x=replicate(k,sample(c("E","T"),size=5,replace=TRUE,prob=c(0.5,0.5))==c("E","E","T","E","T"))
   # y = paste(x,collapse="")
   # gregexpr("EETET",y)
   mean(x)
 }
