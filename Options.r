install.packages('derivmkts')
library(derivmkts)

#----------------------------Section1----------------------------
rm(list = ls())
##Calculate and plot the implied volatility for 'Out-of-the-money' call options
#Calculate
s=11421.74  #Current closing price of the underlying asset
k1=c(11500, 11600, 11700) #Strike price
r=0.0  #Risk-free rate
tt=20/252  #Time to marturity in years
d=0.0  #Dividend yield, annualized, continuously-compounded
price1=c(137.5, 95.5 ,66) #Option price when computing an implied value
impliedVforc=numeric(length(k1)) #create a vector to store value


for(i in 1:length(k1)){
   impliedVforc[i] = bscallimpvol(s=s, k=k1[i], r=r, tt=tt, d=d, price=price1[i])
}

impliedVforc #show the implied volatility

#plot
plot(k1,impliedVforc, type="p", col="blue",pch = 19,
     main=c("2020/1/30",'Out-of-the-money call options','Implied Volatility'), 
     xlab="Strike price", ylab="Implied Vol")

##Calculate and plot the implied volatility for 'Out-of-the-money' put options

s=11421.74  
k2=c(11400,11300,11200)
r=0.0 
tt=20/252
d=0.0
price2=c(218,174.5,142.5)
impliedVforp=numeric(length(k2))

for(i in 1:length(k2)){
  impliedVforp[i] = bsputimpvol(s=s, k=k2[i], r=r, tt=tt, d=d, price=price2[i])
}

impliedVforp

plot(k2,impliedVforp, type="p", col="red",pch = 19,
     main=c("2020/1/30",'Out-of-the-money put options','Implied Volatility'), 
     xlab="Strike price", ylab="Implied Vol")


#reference:https://rdrr.io/cran/derivmkts/man/implied.html

#----------------------------Section2----------------------------
rm(list = ls()) #remove all the variables in order to save space

##Estimate the volatility by using the historical ^TWII data from 2019/12/23 to 2019/01/30
TWII = read.csv("C:/Users/user/Desktop/TSECWI.csv", header = TRUE, sep = ","
                ,quote = "\"", dec = ".", fill = TRUE)

closingP = TWII$Close
Dailyreturn= numeric(length(closingP)-1)
for(i in 1:length(Dailyreturn)){
  Dailyreturn[i] =  log(closingP[i+1] / closingP[i])
}

x1=sum(Dailyreturn^2)
x2=sum(Dailyreturn)

#The estimate of the standard deviation of the daily return
x3= (x1/(length(Dailyreturn)-1) - x2/(length(Dailyreturn)*(length(Dailyreturn)-1)) ) ^0.5  

#The estimate for the volatility per annum
x4 =x3*(252^0.5)

x4

##Calculate the Greeks 'Out-of-the-money' call options

s=11421.7; k1=c(11500, 11600, 11700); 
v=x4;
r=0.0; tt=20/252; d=0.0;

Greeksforc = greeks(bscall(s=s, k=k1, v=v, r=r, tt=tt, d=d))

Greeksforc

##Calculate the Greeks 'Out-of-the-money' call options

s=11421.74; k2=c(11400,11300,11200);
v=x4;
r=0.0; tt=20/252; d=0.0; 

Greeksforp = greeks(bsput(s=s, k=k2, v=v, r=r, tt=tt, d=d))

Greeksforp

