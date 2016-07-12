# Unit 9 - 9.5 code
Alex Deshowitz  
July 10, 2016  

* This markdown file will walk through downloading the data, calculating log returns, calculating volatility, and calculing it over the length of the data.  Additionally, this document will provide graphs of the volatility data with 3 different decay factors.

* First, we need to load the required packages


```r
library(tseries)
```

* Download the data


```r
spx_data<- get.hist.quote(instrument ="^gspc",quote = "Close")
```

* Create the log return with 1 period lag


```r
spx_return<- log(lag(spx_data))- log(spx_data)
```

* Create the volatiltiy parameter ( 250 trading days)


```r
spx_volatility<- sd(spx_return)*sqrt(250)*100

spx_volatility
```

```
## [1] 17.99872
```

* We can see that the SPX volaitlity is .18

* Now, we input a function for the volatility


```r
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
```

* Then we create three different volatility curve decay factors


```r
volest<- vol(10,spx_return)
volest2<- vol(30,spx_return)
volest3<- vol(100, spx_return)
```


* finally, we plot the decay factors for the volatility


```r
plot(volest,type = "l")
lines(volest2, type = "l", col = "red")
lines(volest3, type = "l", col = "blue")
```

![](9_5_rmd_code_files/figure-html/plots-1.png)<!-- -->
