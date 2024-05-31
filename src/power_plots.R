library(tidyverse)
library(ggplot2)
library(forcats)
library(dplyr)
library(broom)
library(MVN)
library(agricolae)
library(ggcorrplot) 
library(pwr)
library(BSDA)
library(gridExtra)
require(moonBook)
require(webr)
library(nortest)
library(pwr)
library(MKpower)
library(gridExtra)
library(ggdist)


a<-.05
n1<- 20
n2<- 20

m1<- 4
m2<- 3

sig<- 4

x<- seq(-7,7,.1)
plot(x,dt(x,n1+n2-2), type = 'l',lwd = 3,xlab='t values',
     ylab='Probability', main='Two-sample t Test')

polygon(c(qt(1-a/2,n1+n2-2),seq(qt(1-a/2,n1+n2-2), 5, .1)),
        c(0,dt(seq(qt(1-a/2,n1+n2-2),5,.1),n1+n2-2)), density = 50, col = 'red')
polygon(c(qt(a/2,n1+n2-2),seq(qt(a/2,n1+n2-2), -5, -.1)),
        c(0,dt(seq(qt(a/2,n1+n2-2),-5,-.1),n1+n2-2)), density = 50, col = 'red')

legend('topleft', paste('Alpha = ', a))

# assuming H1 is true

lines(x,dt(x,n1+n2-2, sqrt(n1*n2/(n1+n2)),(m1-m2)/sig),lwd=3,col='blue')

polygon(c(qt(1-a/2,n1+n2-2),seq(qt(1-a/2,n1+n2-2), 5, .1)),
        c(0,dt(seq(qt(1-a/2,n1+n2-2),5,.1),n1+n2-2,
               sqrt(n1*n2/(n1+n2))*(m1-m2)/sig)), density = 50, col = 'green', angle=135)
polygon(c(qt(a/2,n1+n2-2),seq(qt(a/2,n1+n2-2), -5, -.1)),
        c(0,dt(seq(qt(a/2,n1+n2-2),-5,-.1),n1+n2-2,
               sqrt(n1*n2/(n1+n2))*(m1-m2)/sig)), density = 50, col = 'green', angle=135)

legend('topright', c('Under H0','Under H1','Rejection Region','Power'),
       lty = 1, col = c('black', 'blue', 'red', 'green'), lwd = 3)

(r <- pt(qt(1-a/2,n1+n2-2), n1+n2-2,sqrt(n1*n2/(n1+n2))*(m1-m2)/sig,lower.tail=FALSE))

(l <- pt(qt(a/2,n1+n2-2), n1+n2-2,sqrt(n1*n2/(n1+n2))*(m1-m2)/sig,lower.tail=FALSE))
k1<- 1
k2<- 1
n<- 20
cat('Sample 1 n ',k1*n,' Sample 2 n = ', k2*n)

(r <- pt( qt(  1-a/2, n*(k1+k2)-2),n*(k1+k2)-2,
          sqrt(n*(k1*k2/(k1+k2)))*(m1-m2)/sig,
          lower.tail = FALSE))

(l <- pt(qt(a/2, n*(k1+k2)-2),n*(k1+k2)-2,
         sqrt(n*(k1*k2/(k1+k2)))*(m1-m2)/sig))

while(r+l < .8) {
  n<-n+1
  r <- pt( qt(  1-a/2, n*(k1+k2)-2),n*(k1+k2)-2,
           sqrt(n*(k1*k2/(k1+k2)))*(m1-m2)/sig,
           lower.tail = FALSE)
  l <- pt(qt(a/2, n*(k1+k2)-2),n*(k1+k2)-2,
          sqrt(n*(k1*k2/(k1+k2)))*(m1-m2)/sig)
}
cat('Sample 1 n ',k1*n,' Sample 2 n = ', k2*n)

