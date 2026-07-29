library(MASS)  # for mvrnorm
library(cauchypca)
library(psych)
library(writexl)

set.seed(25)
n <- 40
p_true = 0.5

m1=function(x) #loc
{
  mle=cauchy.mle(x)
  return(as.vector(mle$param)[1])
}
m2=function(x) #sc
{
  mle=cauchy.mle(x)
  m=as.vector(mle$param)[2]
  
  return(sqrt(1-m^2))
}
m3 <- function(x){
  qd <- (quantile(x,0.75)-quantile(x,0.25))/2
  return(sqrt(1-qd^2))
  
}
med_Z <- rep(0,n)
Z_mle <- rep(0,n)
Z_sc <- rep(0,n)
qd_z <- rep(0,n)
sc <- rep(0,n)
est1_z <- rep(0,n)
est2_z <- rep(0,n)
est3_z <- rep(0,n)
est4_z <- rep(0,n)
est5_z <- rep(0,n)
size <- rep(0,n)

for(k in 1:n)
{
  set.seed(1234)
  

    Sigma <- matrix(c(1, p_true, p_true, 1), nrow = 2)
    data <- mvrnorm(k*25, mu = c(0, 0), Sigma = Sigma)
    
    X <- data[, 1]
    Y <- data[, 2]
    Z <- X/Y
    med_Z[k] = median(Z)
    Z_mle[k] = m1(Z) 
    sc[k] = cor(  X, Y)
    Z_sc[k]= m2(Z)
    qd_z[k] <- m3(Z)
  
  est1_z[k] = med_Z[k]
  
  est2_z[k] =  Z_mle[k] 
  
  est3_z[k] = Z_sc[k]
  
  est4_z[k] = qd_z[k]
  
  est5_z[k] = sc[k]
  size[k] = k*25
  
}
df <- data.frame(sample_size=size, med_z = est1_z, loc_mle_z = est2_z, sc_mle_z = est3_z, qd_z = est4_z, cor_X_Y = est5_z )

tmp <- write_xlsx(list(mysheet = df), path = "D:/project_report/rho_estimator200.xlsx")
readxl::read_xlsx(tmp)
plot(size,est1_z,col=2,type="l", lwd=2,xlim=c(1,1000),ylim=c(0,1.0),xlab=expression(paste("sample size")),ylab=expression(paste(rho~"(estimated)")),main=expression(paste("Estimation of "~rho)))
lines(size,est2_z,type="l",lwd=2,col=3,add=T)
lines(size,est3_z,type="l",lwd=2,col=4,add=T)
lines(size,est4_z,type="l",lwd=2,col=5,add=T)
lines(size,est5_z,type="l",lwd=2,col=6,add=T)
abline(h=0.5,col=8)
legend(600,0.2,cex=.6,legend=c(expression(paste("median_z")),expression(paste("location_mle")),expression(paste("sample_correlation")),expression(paste("scale_mle")),expression(paste("quartile_deviation"))) , col = c(2, 3, 6, 4,5), lty = 1)

