## unit 9.5 code for turn-in#

install.packages("tseries")
library(tseries)
install.packages("quantmod")
sessionInfo()
# if(!require(installr)) {
#   install.packages("installr"); require(installr)} #load / install+load installr
# 
# # using the package:
# updateR() 

# my_get_quote<- tseries::get.hist.quote
# fix(my_get_quote)
# 
# spx_data<- my_get_quote(instrument = "^gspc", quote = "close", provider = "yahoo")
# 
# length(spx_data)

 #spx_data<- get.hist.quote(instrument ="^gspc",quote = "close", provider = "yahoo", origin = "1990-01-01")
spx_data<- get.hist.quote(instrument ="^gspc",quote = "Close")
 
luv<- get.hist.quote("LUV",quote="Close")

length(spx_data)

spx_return<- log(lag(spx_data))- log(spx_data)

length(spx_return)

spx_volatility<- sd(spx_return)*sqrt(250)*100
spx_volatility

vol<- function(d, logrets){
  var = 0
  lam = 0
  varlist<- c()
  for(r in logrets){
    lam = lam*(1-1/d)+1
  var = (1-1/lam)*var+(1/lam)*r^2
    varlist<- c(varlist,var)
  
  }
sqrt(varlist)
}

volest<- vol(10,spx_return)
volest2<- vol(30,spx_return)
volest3<- vol(100, spx_return)

plot(volest,type = "l")
lines(volest2, type = "l", col = "red")
lines(volest3, type = "l", col = "blue")



