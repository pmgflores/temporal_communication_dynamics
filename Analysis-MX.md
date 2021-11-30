Communicated emotions Mexico
================

### Import Data

``` r
setwd("C:\\Users\\Pablo M. Flores\\OneDrive\\Pablo Flores Bautista\\UCDavis\\PhD\\Papers\\2020 Communicated emotions\\Paper\\Git repository\\communicated emotions")

Base <- read.csv("DDBB_to_RR_Mexico.csv", header = TRUE)

# Creation time series by five minutes
sadness_MX <- aggregate(sadness~one_minutes, data = Base, sum)
anger_MX <- aggregate(anger~one_minutes, data = Base, sum)
fear_MX <- aggregate(fear~one_minutes, data = Base, sum)
disgust_MX <- aggregate(disgust~one_minutes, data = Base, sum)
joy_MX <- aggregate(joy~one_minutes, data = Base, sum)
count_MX <- aggregate(count~one_minutes, data = Base, sum)

#Time Series variables mean
sadness <- sadness_MX$sadness
anger <- anger_MX$anger
fear <- fear_MX$fear
disgust <- disgust_MX$disgust
joy <- joy_MX$joy
count <- count_MX$count

emotions_MX <- as.data.frame(cbind(sadness, anger, fear, disgust, joy, count))
```

### Change point analysis

Based on the variation of the amount of tweets the process is divided in
phases

``` r
library('forecast')
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library('changepoint')
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Successfully loaded changepoint package version 2.2.2
    ##  NOTE: Predefined penalty values changed in version 2.2.  Previous penalty values with a postfix 1 i.e. SIC1 are now without i.e. SIC and previous penalties without a postfix i.e. SIC are now with a postfix 0 i.e. SIC0. See NEWS and help files for further details.

``` r
ggtsdisplay(emotions_MX$count, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Tweets about Mexico earthquake',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-MX_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
cp_count <- cpt.var(emotions_MX[,6], method='PELT')

plot(cp_count, main='Change points tweets during Mexico earthquake')
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
detach('package:changepoint')
```

### Time series plots

``` r
ggtsdisplay(emotions_MX$sadness, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Mexico earthquake (Sadness)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-MX_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggtsdisplay(emotions_MX$anger, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Mexico earthquake (Anger)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-MX_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggtsdisplay(emotions_MX$fear, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Mexico earthquake (Fear)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-MX_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
ggtsdisplay(emotions_MX$disgust, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Mexico earthquake (Disgust)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-MX_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
ggtsdisplay(emotions_MX$joy, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Mexico earthquake (Joy)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-MX_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
detach('package:forecast')
```

### Univariate analysis

Adjust models to the different time series

``` r
#auto.arima(emotions_MX$sadness)

#auto.arima(emotions_MX$anger)

#auto.arima(emotions_MX$fear)

#auto.arima(emotions_MX$disgust)

#auto.arima(emotions_MX$joy)

#detach('package:forecast')
```

## Granger Causality

### Toda-Yamamoto Granger causality correction

The work developed for Toda and Yamamoto (1995) allows to adjust the
Granger-causality test in cases in which the time series under analysis
are non-stationary. Using the Toda-Yamamoto procedure it is possible to
analyze time series even if they are cointegrated to determine their
Granger-causality. To perform Toda-Yamamoto we (1) determine the maximum
order of integration for each one of the time series, (2) determine the
lag for the VAR model, (3) determine if the time series are
cointegrated, (4) compute the VAR model, (5) test Granger causality
using Toda-Yamamoto.

``` r
toda.yamamoto <- function(var, max.oi) {
  # It requires the VAR function plus the maximum order of integration with the time series at level
  ty.df <- eval(var$call$y);
  ty.varnames <- colnames(ty.df);
  ty.lags <- var$p + max.oi;
  ty.augmented_var <- VAR(ty.df, ty.lags, type=var$type);
  
  ty.results <- data.frame(predictor = character(0), causes = character(0), chisq = numeric(0), p = numeric(0));
  
  for (current_variable in ty.varnames) {
    # Construct the restriction matrix: to test if *current_variable* causes any of the others,
    # Test if the lagged values of current variable (ignoring the lags added with max.oi) are jointly insignificant
    
    ty.restrictions <- as.matrix(Bcoef(ty.augmented_var))*0+1;
    ty.coefres <- head(grep(current_variable, colnames(ty.restrictions), value=T), -1);
    ty.restrictions[which(rownames(ty.restrictions) != current_variable), ty.coefres] <- 0;
    # Estimate restricted var
    ty.restricted_var <- restrict(ty.augmented_var, 'manual', resmat=ty.restrictions);
    
    for (k in 1:length(ty.varnames)) {
      if (ty.varnames[k] != current_variable) {
        my.wald <- waldtest(ty.augmented_var$varresult[[k]], ty.restricted_var$varresult[[k]], test='Chisq');
        ty.results <- rbind(ty.results, data.frame(
          predictor = current_variable, 
          causes = ty.varnames[k], 
          chisq = as.numeric(my.wald$Chisq[2]), 
          p = my.wald$`Pr(>Chisq)`[2])
        );
      }
    }
  }
  return(ty.results);
}
```

The analysis is performed in all the stages defined by the change point
analysis previously performed

**Stage 1**

``` r
library('vars')
```

    ## Loading required package: MASS

    ## Loading required package: strucchange

    ## Loading required package: sandwich

    ## Loading required package: urca

    ## Loading required package: lmtest

``` r
library('forecast')

cp_emotions_MX <- changepoint::cpts(cp_count)

# Plot
ts.plot(emotions_MX[1:cp_emotions_MX[1],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 1')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_MX$sadness[1:cp_emotions_MX[1]])
```

    ## Series: emotions_MX$sadness[1:cp_emotions_MX[1]] 
    ## ARIMA(0,1,0) 
    ## 
    ## sigma^2 estimated as 15.89:  log likelihood=-327.82
    ## AIC=657.63   AICc=657.67   BIC=660.4

``` r
auto.arima(emotions_MX$anger[1:cp_emotions_MX[1]])
```

    ## Series: emotions_MX$anger[1:cp_emotions_MX[1]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.2279
    ## s.e.   0.0919
    ## 
    ## sigma^2 estimated as 3.957:  log likelihood=-246
    ## AIC=496.01   AICc=496.11   BIC=501.53

``` r
auto.arima(emotions_MX$fear[1:cp_emotions_MX[1]])
```

    ## Series: emotions_MX$fear[1:cp_emotions_MX[1]] 
    ## ARIMA(2,1,0) 
    ## 
    ## Coefficients:
    ##           ar1     ar2
    ##       -0.1700  0.2100
    ## s.e.   0.0899  0.0896
    ## 
    ## sigma^2 estimated as 11.18:  log likelihood=-306.31
    ## AIC=618.61   AICc=618.83   BIC=626.9

``` r
auto.arima(emotions_MX$disgust[1:cp_emotions_MX[1]])
```

    ## Series: emotions_MX$disgust[1:cp_emotions_MX[1]] 
    ## ARIMA(1,1,0) 
    ## 
    ## Coefficients:
    ##           ar1
    ##       -0.3887
    ## s.e.   0.0846
    ## 
    ## sigma^2 estimated as 3.361:  log likelihood=-236.52
    ## AIC=477.03   AICc=477.14   BIC=482.56

``` r
auto.arima(emotions_MX$joy[1:cp_emotions_MX[1]])
```

    ## Series: emotions_MX$joy[1:cp_emotions_MX[1]] 
    ## ARIMA(1,1,0) 
    ## 
    ## Coefficients:
    ##           ar1
    ##       -0.2601
    ## s.e.   0.0888
    ## 
    ## sigma^2 estimated as 38.01:  log likelihood=-378.37
    ## AIC=760.73   AICc=760.84   BIC=766.26

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_MX[1:cp_emotions_MX[1],1:5], lag=10)
var_lag
```

    ## Best AIC:  lag= 2 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_1 <- ca.jo(emotions_MX[1:cp_emotions_MX[1],1:5], type = 'trace', ecdet='const', K=2)
summary(coi_1)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 3.327858e-01 2.890263e-01 1.516036e-01 1.065348e-01 2.581353e-02
    ## [6] 1.665335e-16
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##            test 10pct  5pct  1pct
    ## r <= 4 |   3.03  7.52  9.24 12.97
    ## r <= 3 |  16.10 17.85 19.96 24.60
    ## r <= 2 |  35.17 32.00 34.91 41.07
    ## r <= 1 |  74.74 49.65 53.12 60.16
    ## r = 0  | 121.68 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l2    anger.l2     fear.l2 disgust.l2     joy.l2
    ## sadness.l2  1.00000000  1.00000000   1.0000000  1.0000000   1.000000
    ## anger.l2   -1.10559374 -3.64178122   0.8280117  1.2126634  -2.154106
    ## fear.l2     0.47590547 -1.92807725  -1.1105983  0.2373538   9.490610
    ## disgust.l2 -2.47405154  4.27826069  -2.7354474 -1.5260006   4.937456
    ## joy.l2     -0.04183477  0.05570869   0.9848326 -0.8132497  -5.554150
    ## constant    5.49200749  4.15310283 -16.0107955  9.5898874 -20.299781
    ##                 constant
    ## sadness.l2    1.00000000
    ## anger.l2      0.02995952
    ## fear.l2      12.94382705
    ## disgust.l2   -5.86444964
    ## joy.l2       -7.67956642
    ## constant   -142.83100526
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##             sadness.l2    anger.l2     fear.l2   disgust.l2       joy.l2
    ## sadness.d  0.082591245 -0.21406176 -0.24325696 -0.033381423 -0.004957234
    ## anger.d    0.291926299  0.03545803 -0.10981785 -0.017706731 -0.002266528
    ## fear.d    -0.005566578 -0.21554190 -0.14695298  0.060057236 -0.006782405
    ## disgust.d  0.285954531 -0.13835915 -0.07768597  0.006612921 -0.002367264
    ## joy.d      0.154680104 -0.30372221 -0.34943406  0.129193030 -0.002881041
    ##               constant
    ## sadness.d 8.926099e-18
    ## anger.d   2.763499e-18
    ## fear.d    1.082176e-17
    ## disgust.d 9.515839e-18
    ## joy.d     3.453336e-18

``` r
# (4) VAR
var_result_1 <- VAR(emotions_MX[1:cp_emotions_MX[1],1:5], p=3)

#VAR stabilty
roots(var_result_1)
```

    ##  [1] 0.9530503 0.8244735 0.8244735 0.6394027 0.6394027 0.6330195 0.5566183
    ##  [8] 0.5566183 0.5052539 0.4275189 0.4275189 0.3676670 0.3676670 0.3286927
    ## [15] 0.1864400

``` r
serial.test(var_result_1)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_1
    ## Chi-squared = 354.34, df = 325, p-value = 0.1263

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_1, max.oi=1)
```

    ##    predictor  causes     chisq          p
    ## 1    sadness   anger 7.4835710 0.05798209
    ## 2    sadness    fear 1.8388544 0.60651650
    ## 3    sadness disgust 3.8318176 0.28020683
    ## 4    sadness     joy 1.1579476 0.76310686
    ## 5      anger sadness 1.9609151 0.58055841
    ## 6      anger    fear 2.0353878 0.56509441
    ## 7      anger disgust 0.6253283 0.89061033
    ## 8      anger     joy 1.9714701 0.57834925
    ## 9       fear sadness 2.9105141 0.40562891
    ## 10      fear   anger 1.4227527 0.70021016
    ## 11      fear disgust 8.8425847 0.03145870
    ## 12      fear     joy 3.6759263 0.29865157
    ## 13   disgust sadness 0.4637141 0.92678780
    ## 14   disgust   anger 0.3056612 0.95896027
    ## 15   disgust    fear 0.1029344 0.99148298
    ## 16   disgust     joy 0.1659803 0.98288484
    ## 17       joy sadness 4.4453462 0.21721740
    ## 18       joy   anger 5.0518336 0.16804074
    ## 19       joy    fear 1.8986458 0.59370722
    ## 20       joy disgust 3.3084768 0.34646458

**Stage 2**

``` r
# Plot
ts.plot(emotions_MX[cp_emotions_MX[1]:cp_emotions_MX[2],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 2')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_MX$sadness[cp_emotions_MX[1]:cp_emotions_MX[2]])
```

    ## Series: emotions_MX$sadness[cp_emotions_MX[1]:cp_emotions_MX[2]] 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1    mean
    ##       0.9225  -0.8072  6.9966
    ## s.e.  0.0628   0.0810  0.3088
    ## 
    ## sigma^2 estimated as 1.825:  log likelihood=-170.57
    ## AIC=349.13   AICc=349.55   BIC=359.55

``` r
auto.arima(emotions_MX$anger[cp_emotions_MX[1]:cp_emotions_MX[2]])
```

    ## Series: emotions_MX$anger[cp_emotions_MX[1]:cp_emotions_MX[2]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.7895
    ## s.e.   0.0623
    ## 
    ## sigma^2 estimated as 0.9376:  log likelihood=-137.27
    ## AIC=278.55   AICc=278.67   BIC=283.74

``` r
auto.arima(emotions_MX$fear[cp_emotions_MX[1]:cp_emotions_MX[2]])
```

    ## Series: emotions_MX$fear[cp_emotions_MX[1]:cp_emotions_MX[2]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.8623
    ## s.e.   0.0467
    ## 
    ## sigma^2 estimated as 1.748:  log likelihood=-168.29
    ## AIC=340.57   AICc=340.7   BIC=345.76

``` r
auto.arima(emotions_MX$disgust[cp_emotions_MX[1]:cp_emotions_MX[2]])
```

    ## Series: emotions_MX$disgust[cp_emotions_MX[1]:cp_emotions_MX[2]] 
    ## ARIMA(1,0,1) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1    mean
    ##       0.8887  -0.7535  3.4448
    ## s.e.  0.1167   0.1525  0.1704
    ## 
    ## sigma^2 estimated as 0.6691:  log likelihood=-120.37
    ## AIC=248.74   AICc=249.16   BIC=259.16

``` r
auto.arima(emotions_MX$joy[cp_emotions_MX[1]:cp_emotions_MX[2]])
```

    ## Series: emotions_MX$joy[cp_emotions_MX[1]:cp_emotions_MX[2]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.9299  -0.0725
    ## s.e.   0.0913   0.0224
    ## 
    ## sigma^2 estimated as 7.006:  log likelihood=-236.83
    ## AIC=479.66   AICc=479.91   BIC=487.44

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_MX[cp_emotions_MX[1]:cp_emotions_MX[2],1:5], lag=10)
var_lag
```

    ## Best AIC:  lag= 8 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_2 <- ca.jo(emotions_MX[cp_emotions_MX[1]:cp_emotions_MX[2],1:5], type = 'trace', ecdet = 'const', K=4)
summary(coi_2)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1]  3.762399e-01  1.976052e-01  1.411932e-01  1.103329e-01  4.026032e-02
    ## [6] -4.103763e-18
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 4 |  3.94  7.52  9.24 12.97
    ## r <= 3 | 15.17 17.85 19.96 24.60
    ## r <= 2 | 29.78 32.00 34.91 41.07
    ## r <= 1 | 50.92 49.65 53.12 60.16
    ## r = 0  | 96.23 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l4    anger.l4     fear.l4 disgust.l4     joy.l4
    ## sadness.l4   1.0000000  1.00000000  1.00000000  1.0000000  1.0000000
    ## anger.l4     7.3371417 -0.09191940 -0.25814169 -2.0118102  1.6226979
    ## fear.l4    -14.9949906  0.43649924 -0.41391422 -0.7884451 -0.3367868
    ## disgust.l4  -8.0998663 -2.16906550  0.23184641  0.9985529  0.1397815
    ## joy.l4       5.5813067 -0.07193964  0.03331504 -0.2715663 -1.3943615
    ## constant    -0.9906906 -0.74262234 -4.66066577  6.6192534  7.8594579
    ##               constant
    ## sadness.l4  1.00000000
    ## anger.l4   -0.06043342
    ## fear.l4     0.46269469
    ## disgust.l4 -1.17797763
    ## joy.l4     -1.16452619
    ## constant   18.89395926
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##             sadness.l4     anger.l4     fear.l4 disgust.l4        joy.l4
    ## sadness.d  0.047632990 -0.115458735 -0.47358111  0.2355690 -0.0002872033
    ## anger.d    0.006321824  0.329910721 -0.14034098  0.1963126 -0.0156352255
    ## fear.d     0.092754087  0.009622547  0.04080234  0.2119175  0.0223732783
    ## disgust.d  0.017654473  0.267181465 -0.25270644  0.1070197  0.0150959303
    ## joy.d     -0.003183965 -0.170705932 -0.15483593  0.5954630  0.0627115749
    ##                constant
    ## sadness.d  5.128740e-16
    ## anger.d    2.104915e-16
    ## fear.d     8.450089e-16
    ## disgust.d  3.412057e-16
    ## joy.d     -2.467189e-16

``` r
# (4) VAR
var_result_2 <- VAR(emotions_MX[cp_emotions_MX[1]:cp_emotions_MX[2],1:5], p=4)

#VAR stabilty
roots(var_result_2)
```

    ##  [1] 0.9018196 0.8249790 0.8249790 0.7882673 0.7882673 0.7618063 0.7600940
    ##  [8] 0.7486501 0.7486501 0.7438920 0.7438920 0.6827342 0.6827342 0.6479462
    ## [15] 0.5998266 0.5998266 0.5976814 0.5976814 0.5767832 0.5767832

``` r
serial.test(var_result_2)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_2
    ## Chi-squared = 305.45, df = 300, p-value = 0.4019

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_2, max.oi=1)
```

    ##    predictor  causes     chisq           p
    ## 1    sadness   anger  8.352968 0.079471416
    ## 2    sadness    fear 15.103638 0.004491021
    ## 3    sadness disgust 11.200250 0.024403310
    ## 4    sadness     joy  4.794258 0.309066637
    ## 5      anger sadness  4.679871 0.321748899
    ## 6      anger    fear  5.276813 0.260055388
    ## 7      anger disgust  1.692398 0.792098115
    ## 8      anger     joy  8.367995 0.078991037
    ## 9       fear sadness  8.570061 0.072791802
    ## 10      fear   anger  1.817877 0.769210146
    ## 11      fear disgust  6.303520 0.177598877
    ## 12      fear     joy  5.560023 0.234503542
    ## 13   disgust sadness  2.753658 0.599858401
    ## 14   disgust   anger  4.952775 0.292177538
    ## 15   disgust    fear  6.895713 0.141503032
    ## 16   disgust     joy  3.261462 0.515061378
    ## 17       joy sadness  5.733007 0.219994469
    ## 18       joy   anger  3.660776 0.453856504
    ## 19       joy    fear 15.937822 0.003103742
    ## 20       joy disgust  5.566747 0.233924376

**Stage 3**

``` r
# Plot
ts.plot(emotions_MX[cp_emotions_MX[2]:cp_emotions_MX[3],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 3')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_MX$sadness[cp_emotions_MX[2]:cp_emotions_MX[3]])
```

    ## Series: emotions_MX$sadness[cp_emotions_MX[2]:cp_emotions_MX[3]] 
    ## ARIMA(0,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##         mean
    ##       4.2735
    ## s.e.  0.1200
    ## 
    ## sigma^2 estimated as 1.224:  log likelihood=-127.18
    ## AIC=258.35   AICc=258.5   BIC=263.21

``` r
auto.arima(emotions_MX$anger[cp_emotions_MX[2]:cp_emotions_MX[3]])
```

    ## Series: emotions_MX$anger[cp_emotions_MX[2]:cp_emotions_MX[3]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.9111
    ## s.e.   0.0681
    ## 
    ## sigma^2 estimated as 1.034:  log likelihood=-119.56
    ## AIC=243.12   AICc=243.27   BIC=247.96

``` r
auto.arima(emotions_MX$fear[cp_emotions_MX[2]:cp_emotions_MX[3]])
```

    ## Series: emotions_MX$fear[cp_emotions_MX[2]:cp_emotions_MX[3]] 
    ## ARIMA(0,1,2) 
    ## 
    ## Coefficients:
    ##           ma1     ma2
    ##       -1.0773  0.2045
    ## s.e.   0.1032  0.1167
    ## 
    ## sigma^2 estimated as 0.9405:  log likelihood=-115.06
    ## AIC=236.12   AICc=236.42   BIC=243.38

``` r
auto.arima(emotions_MX$disgust[cp_emotions_MX[2]:cp_emotions_MX[3]])
```

    ## Series: emotions_MX$disgust[cp_emotions_MX[2]:cp_emotions_MX[3]] 
    ## ARIMA(0,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##         mean
    ##       2.2832
    ## s.e.  0.0760
    ## 
    ## sigma^2 estimated as 0.4909:  log likelihood=-88.8
    ## AIC=181.61   AICc=181.76   BIC=186.47

``` r
auto.arima(emotions_MX$joy[cp_emotions_MX[2]:cp_emotions_MX[3]])
```

    ## Series: emotions_MX$joy[cp_emotions_MX[2]:cp_emotions_MX[3]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.8849
    ## s.e.   0.0651
    ## 
    ## sigma^2 estimated as 5.871:  log likelihood=-191.49
    ## AIC=386.98   AICc=387.13   BIC=391.81

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_MX[cp_emotions_MX[2]:cp_emotions_MX[3],1:5], lag=10)
var_lag
```

    ## Best AIC:  lag= 1 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_3 <- ca.jo(emotions_MX[cp_emotions_MX[2]:cp_emotions_MX[3],1:5], type = 'trace', ecdet = 'const', K=3)
summary(coi_3)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1]  4.754448e-01  3.130399e-01  2.617431e-01  2.399867e-01  6.573522e-02
    ## [6] -1.338727e-17
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##            test 10pct  5pct  1pct
    ## r <= 4 |   5.51  7.52  9.24 12.97
    ## r <= 3 |  27.74 17.85 19.96 24.60
    ## r <= 2 |  52.32 32.00 34.91 41.07
    ## r <= 1 |  82.73 49.65 53.12 60.16
    ## r = 0  | 134.99 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##              sadness.l3   anger.l3     fear.l3   disgust.l3     joy.l3
    ## sadness.l3  1.000000000   1.000000  1.00000000  1.000000000  1.0000000
    ## anger.l3    0.630663167   5.045943 -0.63949774 -3.429446376 -1.0688282
    ## fear.l3    -0.623998503 -31.807724 -0.31778825 -2.013845177  0.9766277
    ## disgust.l3 -2.185883771   3.601677  0.78560582  1.791843231 -2.6217532
    ## joy.l3      0.007277977  10.419106 -0.07983326  0.002362053  0.7108296
    ## constant    0.983131892  -8.022285 -2.30224437  8.914732925 -4.4236805
    ##              constant
    ## sadness.l3  1.0000000
    ## anger.l3   -1.7341375
    ## fear.l3     0.9597986
    ## disgust.l3 -1.0873312
    ## joy.l3      0.2655283
    ## constant   -7.0073369
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##           sadness.l3     anger.l3    fear.l3 disgust.l3      joy.l3
    ## sadness.d -0.1234360 -0.012020968 -1.0415445 0.15188480 -0.02536904
    ## anger.d   -0.1551589 -0.023579355 -0.3797405 0.22978991  0.01746945
    ## fear.d     0.3377279  0.003920138 -0.3786883 0.14838499 -0.07580480
    ## disgust.d  0.5196515 -0.017078704 -0.4495961 0.09197756  0.01725262
    ## joy.d      0.4963602 -0.075844621 -0.4375181 0.12577443 -0.22049115
    ##                constant
    ## sadness.d -3.232755e-16
    ## anger.d   -3.958528e-16
    ## fear.d    -2.230272e-16
    ## disgust.d -3.446403e-16
    ## joy.d     -5.542233e-16

``` r
# (4) VAR
var_result_3 <- VAR(emotions_MX[cp_emotions_MX[2]:cp_emotions_MX[3],1:5], p=3)

#VAR stabilty
roots(var_result_3)
```

    ##  [1] 0.8540197 0.6769088 0.6769088 0.6582279 0.6582279 0.6171288 0.6171288
    ##  [8] 0.5759552 0.5759552 0.5386728 0.5386728 0.5369168 0.5369168 0.4290006
    ## [15] 0.2150286

``` r
serial.test(var_result_3)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_3
    ## Chi-squared = 283.79, df = 325, p-value = 0.9519

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_3, max.oi=1)
```

    ##    predictor  causes      chisq          p
    ## 1    sadness   anger  1.0820771 0.78140241
    ## 2    sadness    fear 10.3430805 0.01586404
    ## 3    sadness disgust  6.1915124 0.10265552
    ## 4    sadness     joy  2.1431373 0.54323520
    ## 5      anger sadness  3.1075895 0.37533262
    ## 6      anger    fear  6.9338208 0.07403731
    ## 7      anger disgust  4.1211125 0.24867966
    ## 8      anger     joy  1.0518565 0.78870695
    ## 9       fear sadness  1.3958412 0.70650987
    ## 10      fear   anger  2.9245436 0.40340598
    ## 11      fear disgust  0.5679811 0.90372161
    ## 12      fear     joy  6.5961542 0.08594658
    ## 13   disgust sadness  0.8583447 0.83546675
    ## 14   disgust   anger  0.7285369 0.86647212
    ## 15   disgust    fear  2.4978069 0.47568756
    ## 16   disgust     joy  1.1966885 0.75379865
    ## 17       joy sadness  0.5418100 0.90961447
    ## 18       joy   anger  3.3094794 0.34632547
    ## 19       joy    fear  1.6682630 0.64401256
    ## 20       joy disgust  2.7118724 0.43821348

**Stage 4**

``` r
# Plot
ts.plot(emotions_MX[cp_emotions_MX[3]:cp_emotions_MX[4],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 4')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_MX$sadness[cp_emotions_MX[3]:cp_emotions_MX[4]])
```

    ## Series: emotions_MX$sadness[cp_emotions_MX[3]:cp_emotions_MX[4]] 
    ## ARIMA(1,1,2) 
    ## 
    ## Coefficients:
    ##          ar1      ma1      ma2
    ##       0.0431  -0.8297  -0.0632
    ## s.e.  0.4606   0.4587   0.4023
    ## 
    ## sigma^2 estimated as 1.304:  log likelihood=-384.03
    ## AIC=776.07   AICc=776.23   BIC=790.12

``` r
auto.arima(emotions_MX$anger[cp_emotions_MX[3]:cp_emotions_MX[4]])
```

    ## Series: emotions_MX$anger[cp_emotions_MX[3]:cp_emotions_MX[4]] 
    ## ARIMA(1,0,0) with non-zero mean 
    ## 
    ## Coefficients:
    ##          ar1    mean
    ##       0.2434  2.1118
    ## s.e.  0.0641  0.0694
    ## 
    ## sigma^2 estimated as 0.6932:  log likelihood=-306.72
    ## AIC=619.43   AICc=619.53   BIC=629.98

``` r
auto.arima(emotions_MX$fear[cp_emotions_MX[3]:cp_emotions_MX[4]])
```

    ## Series: emotions_MX$fear[cp_emotions_MX[3]:cp_emotions_MX[4]] 
    ## ARIMA(1,1,2) 
    ## 
    ## Coefficients:
    ##           ar1      ma1      ma2
    ##       -0.6176  -0.2452  -0.5868
    ## s.e.   0.6504   0.6283   0.5592
    ## 
    ## sigma^2 estimated as 0.6028:  log likelihood=-288.42
    ## AIC=584.85   AICc=585.01   BIC=598.9

``` r
auto.arima(emotions_MX$disgust[cp_emotions_MX[3]:cp_emotions_MX[4]])
```

    ## Series: emotions_MX$disgust[cp_emotions_MX[3]:cp_emotions_MX[4]] 
    ## ARIMA(1,1,1) 
    ## 
    ## Coefficients:
    ##          ar1      ma1
    ##       0.0118  -0.8690
    ## s.e.  0.0761   0.0384
    ## 
    ## sigma^2 estimated as 0.3453:  log likelihood=-219.71
    ## AIC=445.43   AICc=445.53   BIC=455.97

``` r
auto.arima(emotions_MX$joy[cp_emotions_MX[3]:cp_emotions_MX[4]])
```

    ## Series: emotions_MX$joy[cp_emotions_MX[3]:cp_emotions_MX[4]] 
    ## ARIMA(2,1,1) 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ma1
    ##       -0.2271  -0.1109  -0.8258
    ## s.e.   0.0765   0.0739   0.0480
    ## 
    ## sigma^2 estimated as 2.649:  log likelihood=-472.03
    ## AIC=952.05   AICc=952.22   BIC=966.11

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_MX[cp_emotions_MX[3]:cp_emotions_MX[4],1:5], lag=20)
var_lag
```

    ## Best AIC:  lag= 1 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_4 <- ca.jo(emotions_MX[cp_emotions_MX[3]:cp_emotions_MX[4],1:5], type = 'trace', ecdet='const', K=4)
summary(coi_4)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 2.175123e-01 1.958958e-01 1.392509e-01 8.671774e-02 5.615152e-02
    ## [6] 5.551115e-17
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##            test 10pct  5pct  1pct
    ## r <= 4 |  14.16  7.52  9.24 12.97
    ## r <= 3 |  36.38 17.85 19.96 24.60
    ## r <= 2 |  73.12 32.00 34.91 41.07
    ## r <= 1 | 126.54 49.65 53.12 60.16
    ## r = 0  | 186.63 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l4    anger.l4     fear.l4 disgust.l4    joy.l4   constant
    ## sadness.l4  1.00000000  1.00000000   1.0000000  1.0000000  1.000000  1.0000000
    ## anger.l4   -2.97799361  0.06860124   0.3271506 -0.7003238 -1.598841 -1.1326301
    ## fear.l4     0.04413325 -0.95039965   6.7919455  0.9667007 -2.321373 -0.3096483
    ## disgust.l4 -0.61960306 -1.34635413 -10.3858061  0.3140666  4.670336  0.1832889
    ## joy.l4      0.41195101  0.17681582   2.4531440 -0.9186478  1.079625 -0.5038426
    ## constant    1.42896100 -0.09089799 -15.3120532 -0.2831808 -8.232143  8.9383266
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##           sadness.l4    anger.l4      fear.l4  disgust.l4      joy.l4
    ## sadness.d 0.09205772 -0.53986621 -0.054348435 -0.18472592 -0.05817586
    ## anger.d   0.34427006 -0.25852671 -0.029550340 -0.05591902 -0.02462424
    ## fear.d    0.05472216  0.20663401 -0.068496605 -0.08575275 -0.02226462
    ## disgust.d 0.08069637  0.15805519 -0.006394148 -0.06483171 -0.04446654
    ## joy.d     0.03889671 -0.07186541 -0.098416512  0.14052516 -0.11469646
    ##                constant
    ## sadness.d  4.048271e-17
    ## anger.d    1.297463e-16
    ## fear.d    -1.296891e-16
    ## disgust.d  6.120455e-18
    ## joy.d     -2.246984e-16

``` r
# (4) VAR
var_result_4 <- VAR(emotions_MX[cp_emotions_MX[3]:cp_emotions_MX[4],1:5], p=4)

#VAR stabilty
roots(var_result_4)
```

    ##  [1] 0.8822108 0.8146129 0.6584944 0.6584944 0.6472277 0.6472277 0.6457944
    ##  [8] 0.6457944 0.6328781 0.6328781 0.5950955 0.5950955 0.5809247 0.5809247
    ## [15] 0.4883894 0.4883894 0.4816779 0.4679018 0.4679018 0.4311785

``` r
serial.test(var_result_4)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_4
    ## Chi-squared = 332.61, df = 300, p-value = 0.09461

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_4, max.oi=1)
```

    ##    predictor  causes     chisq         p
    ## 1    sadness   anger 6.8285050 0.1452333
    ## 2    sadness    fear 1.1855409 0.8804734
    ## 3    sadness disgust 3.4380325 0.4873630
    ## 4    sadness     joy 1.1815309 0.8811300
    ## 5      anger sadness 1.7302101 0.7852229
    ## 6      anger    fear 0.4508525 0.9781057
    ## 7      anger disgust 0.8703311 0.9287782
    ## 8      anger     joy 0.1219622 0.9982145
    ## 9       fear sadness 1.5369058 0.8200846
    ## 10      fear   anger 3.3270658 0.5046554
    ## 11      fear disgust 1.9017231 0.7538285
    ## 12      fear     joy 0.6594968 0.9562282
    ## 13   disgust sadness 3.5880878 0.4646114
    ## 14   disgust   anger 1.9366053 0.7474177
    ## 15   disgust    fear 3.0803190 0.5444753
    ## 16   disgust     joy 5.1785630 0.2694616
    ## 17       joy sadness 3.3446538 0.5018885
    ## 18       joy   anger 2.6287268 0.6217428
    ## 19       joy    fear 4.2237538 0.3765702
    ## 20       joy disgust 3.8586825 0.4254688

**Stage 5**

``` r
# Plot
ts.plot(emotions_MX[cp_emotions_MX[4]:cp_emotions_MX[5],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 5')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_MX$sadness[cp_emotions_MX[4]:cp_emotions_MX[5]])
```

    ## Series: emotions_MX$sadness[cp_emotions_MX[4]:cp_emotions_MX[5]] 
    ## ARIMA(1,1,2) with drift 
    ## 
    ## Coefficients:
    ##           ar1      ma1      ma2    drift
    ##       -0.1281  -0.9124  -0.0513  -0.0047
    ## s.e.   0.4873   0.4875   0.4768   0.0016
    ## 
    ## sigma^2 estimated as 0.6986:  log likelihood=-417.16
    ## AIC=844.31   AICc=844.5   BIC=863.42

``` r
auto.arima(emotions_MX$anger[cp_emotions_MX[4]:cp_emotions_MX[5]])
```

    ## Series: emotions_MX$anger[cp_emotions_MX[4]:cp_emotions_MX[5]] 
    ## ARIMA(1,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ar1      ma1    drift
    ##       -0.1343  -0.9774  -0.0021
    ## s.e.   0.0576   0.0249   0.0007
    ## 
    ## sigma^2 estimated as 0.2555:  log likelihood=-248.43
    ## AIC=504.86   AICc=504.99   BIC=520.15

``` r
auto.arima(emotions_MX$fear[cp_emotions_MX[4]:cp_emotions_MX[5]])
```

    ## Series: emotions_MX$fear[cp_emotions_MX[4]:cp_emotions_MX[5]] 
    ## ARIMA(1,1,3) with drift 
    ## 
    ## Coefficients:
    ##           ar1      ma1      ma2     ma3    drift
    ##       -0.3800  -0.6692  -0.3182  0.0660  -0.0031
    ## s.e.   0.9139   0.9105   0.9284  0.0853   0.0019
    ## 
    ## sigma^2 estimated as 0.3624:  log likelihood=-305.81
    ## AIC=623.62   AICc=623.88   BIC=646.54

``` r
auto.arima(emotions_MX$disgust[cp_emotions_MX[4]:cp_emotions_MX[5]])
```

    ## Series: emotions_MX$disgust[cp_emotions_MX[4]:cp_emotions_MX[5]] 
    ## ARIMA(1,1,1) 
    ## 
    ## Coefficients:
    ##           ar1      ma1
    ##       -0.0990  -0.9438
    ## s.e.   0.0569   0.0174
    ## 
    ## sigma^2 estimated as 0.16:  log likelihood=-169.56
    ## AIC=345.13   AICc=345.2   BIC=356.59

``` r
auto.arima(emotions_MX$joy[cp_emotions_MX[4]:cp_emotions_MX[5]])
```

    ## Series: emotions_MX$joy[cp_emotions_MX[4]:cp_emotions_MX[5]] 
    ## ARIMA(1,1,1) with drift 
    ## 
    ## Coefficients:
    ##          ar1      ma1    drift
    ##       0.1129  -0.9721  -0.0061
    ## s.e.  0.0644   0.0356   0.0027
    ## 
    ## sigma^2 estimated as 1.382:  log likelihood=-532.56
    ## AIC=1073.12   AICc=1073.24   BIC=1088.4

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_MX[cp_emotions_MX[4]:cp_emotions_MX[5],1:5], lag=20)
var_lag
```

    ## Best AIC:  lag= 1 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_4 <- ca.jo(emotions_MX[cp_emotions_MX[4]:cp_emotions_MX[5],1:5], type = 'trace', ecdet='const', K=5)
summary(coi_4)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 0.23919445 0.19927973 0.15569101 0.13818990 0.03517025 0.00000000
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##            test 10pct  5pct  1pct
    ## r <= 4 |  11.92  7.52  9.24 12.97
    ## r <= 3 |  61.45 17.85 19.96 24.60
    ## r <= 2 | 117.80 32.00 34.91 41.07
    ## r <= 1 | 191.81 49.65 53.12 60.16
    ## r = 0  | 282.84 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l5    anger.l5     fear.l5 disgust.l5      joy.l5
    ## sadness.l5  1.00000000   1.0000000  1.00000000  1.0000000  1.00000000
    ## anger.l5    0.05740042   2.9779021 -3.67495344 -0.5676633 -0.37000813
    ## fear.l5    -1.16518563  19.7979763  0.25133353 -0.7756583  0.70955798
    ## disgust.l5 -1.98719581 -34.4866080  0.08275188  0.3373640  1.25711221
    ## joy.l5      0.49607011  -0.4571533  0.33665806 -0.4576801  0.04208862
    ## constant    0.00710677   0.9543318  0.59992787  0.7239152 -3.56367373
    ##              constant
    ## sadness.l5  1.0000000
    ## anger.l5   -0.6945071
    ## fear.l5    -2.2013560
    ## disgust.l5 -1.2091044
    ## joy.l5     -0.5439476
    ## constant   19.9293162
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##            sadness.l5     anger.l5      fear.l5   disgust.l5      joy.l5
    ## sadness.d -0.12211373 -0.010470912  0.080631565 -0.323856899 -0.12921674
    ## anger.d    0.06824206 -0.001829777  0.266378796  0.015687802 -0.06079420
    ## fear.d     0.36596173 -0.026210653 -0.006923049  0.006518573 -0.07783638
    ## disgust.d  0.21160363  0.016253941  0.033878257  0.005814116 -0.05367077
    ## joy.d     -0.47408300  0.017700527 -0.151163168  0.865956477 -0.13947447
    ##                constant
    ## sadness.d  3.865432e-17
    ## anger.d   -8.346142e-19
    ## fear.d    -3.187438e-18
    ## disgust.d -2.325217e-17
    ## joy.d     -1.328493e-17

``` r
# (4) VAR
var_result_4 <- VAR(emotions_MX[cp_emotions_MX[4]:cp_emotions_MX[5],1:5], p=5)

#VAR stabilty
roots(var_result_4)
```

    ##  [1] 0.9204604 0.7048116 0.7048116 0.7036048 0.7036048 0.7031975 0.7031975
    ##  [8] 0.7015436 0.7015436 0.6983830 0.6983830 0.6971569 0.6859498 0.6859498
    ## [15] 0.6064568 0.6064568 0.5813909 0.5813909 0.5756117 0.5756117 0.5478246
    ## [22] 0.5478246 0.4857302 0.4857302 0.1083162

``` r
serial.test(var_result_4)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_4
    ## Chi-squared = 302.16, df = 275, p-value = 0.125

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_4, max.oi=1)
```

    ##    predictor  causes      chisq            p
    ## 1    sadness   anger  3.8325808 0.5737633226
    ## 2    sadness    fear  5.8615586 0.3199238340
    ## 3    sadness disgust  5.1622253 0.3964047002
    ## 4    sadness     joy  0.4547989 0.9936850364
    ## 5      anger sadness  0.6782976 0.9841384854
    ## 6      anger    fear  2.4951560 0.7772244098
    ## 7      anger disgust  9.5293073 0.0897250218
    ## 8      anger     joy  3.1013605 0.6843624295
    ## 9       fear sadness  1.9637462 0.8541383714
    ## 10      fear   anger  6.3597518 0.2727700527
    ## 11      fear disgust  4.5324258 0.4755567331
    ## 12      fear     joy  6.9527651 0.2241769980
    ## 13   disgust sadness  5.4376181 0.3648347800
    ## 14   disgust   anger  2.8224433 0.7273369014
    ## 15   disgust    fear  4.7028161 0.4532139802
    ## 16   disgust     joy  2.2128941 0.8189713317
    ## 17       joy sadness 12.1495382 0.0327952299
    ## 18       joy   anger 15.5757792 0.0081656271
    ## 19       joy    fear 25.2819651 0.0001229076
    ## 20       joy disgust 14.1186335 0.0148731447

**Stage 6**

``` r
# Plot
ts.plot(emotions_MX[cp_emotions_MX[5]:dim(emotions_MX)[1],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 6')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-MX_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_MX$sadness[cp_emotions_MX[5]:dim(emotions_MX)[1]])
```

    ## Series: emotions_MX$sadness[cp_emotions_MX[5]:dim(emotions_MX)[1]] 
    ## ARIMA(5,1,1) 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3      ar4      ar5      ma1
    ##       -0.0931  -0.0916  -0.1762  -0.0467  -0.1208  -0.9146
    ## s.e.   0.0461   0.0461   0.0451   0.0466   0.0461   0.0200
    ## 
    ## sigma^2 estimated as 0.09843:  log likelihood=-142.48
    ## AIC=298.96   AICc=299.16   BIC=329.19

``` r
auto.arima(emotions_MX$anger[cp_emotions_MX[5]:dim(emotions_MX)[1]])
```

    ## Series: emotions_MX$anger[cp_emotions_MX[5]:dim(emotions_MX)[1]] 
    ## ARIMA(2,1,4) 
    ## 
    ## Coefficients:
    ##           ar1     ar2      ma1      ma2     ma3     ma4
    ##       -0.2820  0.5007  -0.7017  -0.7713  0.4185  0.1002
    ## s.e.   0.2953  0.3180   0.2949   0.2031  0.3126  0.0548
    ## 
    ## sigma^2 estimated as 0.02871:  log likelihood=199.49
    ## AIC=-384.99   AICc=-384.78   BIC=-354.76

``` r
auto.arima(emotions_MX$fear[cp_emotions_MX[5]:dim(emotions_MX)[1]])
```

    ## Series: emotions_MX$fear[cp_emotions_MX[5]:dim(emotions_MX)[1]] 
    ## ARIMA(2,1,3) 
    ## 
    ## Coefficients:
    ##          ar1     ar2      ma1     ma2     ma3
    ##       0.4925  0.2147  -1.4915  0.2660  0.2456
    ## s.e.  0.5674  0.4783   0.5577  1.0152  0.4732
    ## 
    ## sigma^2 estimated as 0.05365:  log likelihood=25.56
    ## AIC=-39.13   AICc=-38.97   BIC=-13.21

``` r
auto.arima(emotions_MX$disgust[cp_emotions_MX[5]:dim(emotions_MX)[1]])
```

    ## Series: emotions_MX$disgust[cp_emotions_MX[5]:dim(emotions_MX)[1]] 
    ## ARIMA(5,1,5) 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3      ar4      ar5      ma1      ma2      ma3
    ##       -0.2960  -0.0459  -0.0360  -0.8384  -0.1476  -0.7355  -0.2128  -0.0026
    ## s.e.   0.1334   0.1120   0.1772   0.1737   0.0482   0.1279   0.1602   0.1516
    ##          ma4      ma5
    ##       0.8124  -0.7477
    ## s.e.  0.0789   0.1841
    ## 
    ## sigma^2 estimated as 0.02152:  log likelihood=281.22
    ## AIC=-540.44   AICc=-539.95   BIC=-492.93

``` r
auto.arima(emotions_MX$joy[cp_emotions_MX[5]:dim(emotions_MX)[1]])
```

    ## Series: emotions_MX$joy[cp_emotions_MX[5]:dim(emotions_MX)[1]] 
    ## ARIMA(2,1,4) 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ma1     ma2      ma3      ma4
    ##       -0.1441  -0.9795  -0.8164  0.8528  -0.9151  -0.0177
    ## s.e.   0.0177   0.0192   0.0477  0.0543   0.0523   0.0449
    ## 
    ## sigma^2 estimated as 0.2171:  log likelihood=-362.18
    ## AIC=738.35   AICc=738.56   BIC=768.59

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_MX[cp_emotions_MX[5]:dim(emotions_MX)[1],1:5], lag=30)
var_lag
```

    ## Best AIC:  lag= 3 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_6 <- ca.jo(emotions_MX[cp_emotions_MX[5]:dim(emotions_MX)[1],1:5], type = 'trace', ecdet='const', K=8)
summary(coi_6)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 0.14391432 0.10565951 0.09840167 0.07987137 0.03081046 0.00000000
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##            test 10pct  5pct  1pct
    ## r <= 4 |  17.15  7.52  9.24 12.97
    ## r <= 3 |  62.77 17.85 19.96 24.60
    ## r <= 2 | 119.53 32.00 34.91 41.07
    ## r <= 1 | 180.73 49.65 53.12 60.16
    ## r = 0  | 265.88 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l8    anger.l8     fear.l8  disgust.l8     joy.l8
    ## sadness.l8  1.00000000  1.00000000  1.00000000  1.00000000  1.0000000
    ## anger.l8   -0.36039857  0.70279442  1.87350069 -1.52270436 -4.8045036
    ## fear.l8     0.26582895 -0.54830912 -3.57774559 -0.52513493 -1.2315480
    ## disgust.l8 -5.55153320 -0.72544293 -0.51388281  0.46307419  0.7745595
    ## joy.l8      1.00076814 -0.51512773  0.55273118  0.11649824 -0.6713823
    ## constant   -0.02063288  0.04471303  0.04003536 -0.06855294  0.8778145
    ##              constant
    ## sadness.l8  1.0000000
    ## anger.l8   -0.3400594
    ## fear.l8    -0.7512646
    ## disgust.l8 -0.2840151
    ## joy.l8      0.4558437
    ## constant   -2.0320391
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##           sadness.l8    anger.l8       fear.l8 disgust.l8     joy.l8
    ## sadness.d 0.15295598 -0.41172471  0.0707371706 -0.4907780 0.08065007
    ## anger.d   0.07174947 -0.13836616 -0.0002360222  0.1083894 0.05044578
    ## fear.d    0.09843465 -0.04353531  0.2393688614 -0.2559811 0.05626089
    ## disgust.d 0.27051341  0.05624381 -0.0161536030 -0.1301417 0.02863000
    ## joy.d     0.06385404  0.87916237 -0.1264074170 -0.6308835 0.11076859
    ##                constant
    ## sadness.d -4.226478e-17
    ## anger.d   -2.177812e-17
    ## fear.d    -9.059846e-18
    ## disgust.d  4.222238e-18
    ## joy.d      4.626484e-17

``` r
# (4) VAR
var_result_6 <- VAR(emotions_MX[cp_emotions_MX[5]:dim(emotions_MX)[1],1:5], p=8)

#VAR stability
roots(var_result_6)
```

    ##  [1] 0.9389385 0.8423343 0.8423343 0.8260034 0.8260034 0.8106315 0.8106315
    ##  [8] 0.8027975 0.8027975 0.7743041 0.7743041 0.7652649 0.7652649 0.7636160
    ## [15] 0.7589801 0.7589801 0.7578829 0.7578829 0.7493846 0.7493846 0.7343759
    ## [22] 0.7343759 0.7273862 0.7273862 0.7172491 0.7172491 0.7108170 0.7108170
    ## [29] 0.7043449 0.6816863 0.6816863 0.6730540 0.6730540 0.6445330 0.6445330
    ## [36] 0.6029202 0.6029202 0.5664884 0.5664884 0.1288984

``` r
serial.test(var_result_6)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_6
    ## Chi-squared = 224.73, df = 200, p-value = 0.1109

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_6, max.oi=1)
```

    ##    predictor  causes     chisq            p
    ## 1    sadness   anger  4.673575 0.7918268143
    ## 2    sadness    fear  2.252241 0.9722857827
    ## 3    sadness disgust 13.894667 0.0845525287
    ## 4    sadness     joy  7.181418 0.5171916540
    ## 5      anger sadness  4.871626 0.7712046617
    ## 6      anger    fear  8.821585 0.3575698136
    ## 7      anger disgust 12.792566 0.1191888625
    ## 8      anger     joy  8.196710 0.4144947838
    ## 9       fear sadness  8.386920 0.3966154964
    ## 10      fear   anger 10.573404 0.2270613392
    ## 11      fear disgust 11.009114 0.2011833341
    ## 12      fear     joy  4.874481 0.7709036806
    ## 13   disgust sadness  5.026868 0.7547006306
    ## 14   disgust   anger  4.569729 0.8024170405
    ## 15   disgust    fear  9.260487 0.3208005257
    ## 16   disgust     joy  3.528324 0.8969803458
    ## 17       joy sadness 16.966670 0.0304580194
    ## 18       joy   anger 14.153119 0.0778607029
    ## 19       joy    fear 12.027884 0.1499641073
    ## 20       joy disgust 27.345270 0.0006161755
