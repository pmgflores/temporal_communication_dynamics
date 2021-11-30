Communicated emotions LA
================

### Import Data

``` r
setwd("C:\\Users\\Pablo M. Flores\\OneDrive\\Pablo Flores Bautista\\UCDavis\\PhD\\Papers\\2020 Communicated emotions\\Paper\\Git repository\\communicated emotions")

Base <- read.csv("DDBB_to_RR_LA.csv", header = TRUE)

# Creation time series by five minutes
sadness_LA <- aggregate(sadness~one_minutes, data = Base, sum)
anger_LA <- aggregate(anger~one_minutes, data = Base, sum)
fear_LA <- aggregate(fear~one_minutes, data = Base, sum)
disgust_LA <- aggregate(disgust~one_minutes, data = Base, sum)
joy_LA <- aggregate(joy~one_minutes, data = Base, sum)
count_LA <- aggregate(count~one_minutes, data = Base, sum)

#Time Series variables mean
sadness <- sadness_LA$sadness
anger <- anger_LA$anger
fear <- fear_LA$fear
disgust <- disgust_LA$disgust
joy <- joy_LA$joy
count <- count_LA$count

emotions_LA <- as.data.frame(cbind(sadness, anger, fear, disgust, joy, count))
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
ggtsdisplay(emotions_LA$count, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'Tweets about LA earthquake',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-LA_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
cp_count <- cpt.var(emotions_LA[,6], method='PELT')

plot(cp_count, main='Change points tweets during LA earthquake')
```

![](Analysis-LA_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
detach('package:changepoint')
```

### Time series plots

``` r
ggtsdisplay(emotions_LA$sadness, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'LA earthquake (Sadness)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-LA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggtsdisplay(emotions_LA$anger, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'LA earthquake (Anger)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-LA_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggtsdisplay(emotions_LA$fear, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'LA earthquake (Fear)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-LA_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
ggtsdisplay(emotions_LA$disgust, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'LA earthquake (Disgust)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-LA_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
ggtsdisplay(emotions_LA$joy, plot.type = "partial", points = FALSE, smooth = TRUE,
                      main = 'LA earthquake (Joy)',
                      xlab = 'Time', ylab = 'Variation')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Analysis-LA_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

``` r
detach('package:forecast')
```

### Univariate analysis

Adjust models to the different time series

``` r
#auto.arima(emotions_LA$sadness)

#auto.arima(emotions_LA$anger)

#auto.arima(emotions_LA$fear)

#auto.arima(emotions_LA$disgust)

#auto.arima(emotions_LA$joy)

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

cp_emotions_LA <- changepoint::cpts(cp_count)

# Plot
ts.plot(emotions_LA[1:cp_emotions_LA[1],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 1')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-LA_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_LA$sadness[1:cp_emotions_LA[1]])
```

    ## Series: emotions_LA$sadness[1:cp_emotions_LA[1]] 
    ## ARIMA(0,2,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.9104
    ## s.e.   0.0518
    ## 
    ## sigma^2 estimated as 482.5:  log likelihood=-388.11
    ## AIC=780.22   AICc=780.36   BIC=785.13

``` r
auto.arima(emotions_LA$anger[1:cp_emotions_LA[1]])
```

    ## Series: emotions_LA$anger[1:cp_emotions_LA[1]] 
    ## ARIMA(5,1,1) 
    ## 
    ## Coefficients:
    ##          ar1     ar2     ar3      ar4      ar5      ma1
    ##       0.7273  0.1007  0.0884  -0.0313  -0.4431  -0.7233
    ## s.e.  0.1267  0.1260  0.1255   0.1255   0.1092   0.1224
    ## 
    ## sigma^2 estimated as 268.7:  log likelihood=-365.67
    ## AIC=745.34   AICc=746.76   BIC=762.6

``` r
auto.arima(emotions_LA$fear[1:cp_emotions_LA[1]])
```

    ## Series: emotions_LA$fear[1:cp_emotions_LA[1]] 
    ## ARIMA(3,1,3) 
    ## 
    ## Coefficients:
    ##          ar1      ar2     ar3      ma1     ma2      ma3
    ##       2.1914  -1.6856  0.3931  -2.3540  2.1955  -0.7853
    ## s.e.  0.2214   0.4159  0.2338   0.1832  0.3218   0.1744
    ## 
    ## sigma^2 estimated as 145.3:  log likelihood=-339.36
    ## AIC=692.72   AICc=694.14   BIC=709.98

``` r
auto.arima(emotions_LA$disgust[1:cp_emotions_LA[1]])
```

    ## Series: emotions_LA$disgust[1:cp_emotions_LA[1]] 
    ## ARIMA(0,1,0) 
    ## 
    ## sigma^2 estimated as 140.5:  log likelihood=-338.55
    ## AIC=679.1   AICc=679.15   BIC=681.57

``` r
auto.arima(emotions_LA$joy[1:cp_emotions_LA[1]])
```

    ## Series: emotions_LA$joy[1:cp_emotions_LA[1]] 
    ## ARIMA(1,2,2) 
    ## 
    ## Coefficients:
    ##           ar1     ma1      ma2
    ##       -0.9858  0.0650  -0.8342
    ## s.e.   0.0382  0.0961   0.0987
    ## 
    ## sigma^2 estimated as 490.7:  log likelihood=-387.9
    ## AIC=783.81   AICc=784.3   BIC=793.62

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_LA[1:cp_emotions_LA[1],1:5], lag=10)
var_lag
```

    ## Best AIC:  lag= 10 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 3

``` r
# (3) Cointegration
coi_1 <- ca.jo(emotions_LA[1:cp_emotions_LA[1],1:5], type = 'trace', ecdet='const', K=3)
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
    ## [1] 3.441530e-01 2.232240e-01 1.493571e-01 1.054051e-01 7.246207e-02
    ## [6] 1.208985e-16
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 4 |  6.39  7.52  9.24 12.97
    ## r <= 3 | 15.86 17.85 19.96 24.60
    ## r <= 2 | 29.61 32.00 34.91 41.07
    ## r <= 1 | 51.08 49.65 53.12 60.16
    ## r = 0  | 86.94 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l3    anger.l3     fear.l3 disgust.l3    joy.l3    constant
    ## sadness.l3    1.000000   1.0000000   1.0000000  1.0000000  1.000000   1.0000000
    ## anger.l3      7.803069   2.3595477  -0.9042677  0.4078240  1.736975  -3.0942503
    ## fear.l3     -26.903104  -7.2224825   0.5104597 -0.7474793 -7.839738   0.5776051
    ## disgust.l3   49.158491   2.0528139   0.3913519 -0.8411801 -9.478738  13.6427853
    ## joy.l3      -10.275138  -0.3368996  -0.7333505 -0.4214998  6.714601  -5.1318561
    ## constant   -861.206512 166.0272072 -27.1416699  2.3898088 12.110276 280.4820529
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##            sadness.l3   anger.l3     fear.l3   disgust.l3       joy.l3
    ## sadness.d -0.03663184 0.03745255 -0.12596145 -0.010602011  0.001750443
    ## anger.d   -0.03319311 0.06359594  0.07260781 -0.004824735  0.003903983
    ## fear.d    -0.01629811 0.07356003 -0.06471890  0.037434555  0.004266265
    ## disgust.d -0.01901826 0.01403859 -0.02447919  0.101977632  0.001436069
    ## joy.d     -0.03322300 0.07091919 -0.10254337  0.144328800 -0.010482697
    ##                constant
    ## sadness.d  3.494943e-18
    ## anger.d   -1.142519e-16
    ## fear.d    -1.429685e-16
    ## disgust.d -3.383840e-17
    ## joy.d     -1.736257e-16

``` r
# (4) VAR
var_result_1 <- VAR(emotions_LA[1:cp_emotions_LA[1],1:5], p=3)

#VAR stabilty
roots(var_result_1)
```

    ##  [1] 0.9426223 0.9426223 0.7755518 0.7755518 0.6941923 0.6743389 0.6743389
    ##  [8] 0.6388921 0.6388921 0.6049850 0.5934504 0.5934504 0.4358921 0.3687717
    ## [15] 0.3687717

``` r
serial.test(var_result_1)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_1
    ## Chi-squared = 336.58, df = 325, p-value = 0.3175

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_1, max.oi=2)
```

    ##    predictor  causes     chisq          p
    ## 1    sadness   anger  2.111526 0.71525518
    ## 2    sadness    fear  2.303462 0.68013881
    ## 3    sadness disgust  4.031330 0.40178241
    ## 4    sadness     joy  4.622093 0.32831486
    ## 5      anger sadness  8.251450 0.08278958
    ## 6      anger    fear  4.539640 0.33787301
    ## 7      anger disgust  3.822994 0.43049063
    ## 8      anger     joy  6.151222 0.18813597
    ## 9       fear sadness  7.725896 0.10215101
    ## 10      fear   anger  5.622588 0.22916221
    ## 11      fear disgust  5.634312 0.22817319
    ## 12      fear     joy  2.903733 0.57406262
    ## 13   disgust sadness 14.728622 0.00529847
    ## 14   disgust   anger  5.298119 0.25805306
    ## 15   disgust    fear  9.910810 0.04195717
    ## 16   disgust     joy  7.199343 0.12572145
    ## 17       joy sadness 10.103594 0.03871834
    ## 18       joy   anger  1.549930 0.81776167
    ## 19       joy    fear  3.990911 0.40723727
    ## 20       joy disgust  7.059680 0.13276781

**Stage 2**

``` r
# Plot
ts.plot(emotions_LA[cp_emotions_LA[1]:cp_emotions_LA[2],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 2')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-LA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_LA$sadness[cp_emotions_LA[1]:cp_emotions_LA[2]])
```

    ## Series: emotions_LA$sadness[cp_emotions_LA[1]:cp_emotions_LA[2]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.7467  -0.5488
    ## s.e.   0.0793   0.1928
    ## 
    ## sigma^2 estimated as 51.63:  log likelihood=-318.15
    ## AIC=642.29   AICc=642.56   BIC=649.92

``` r
auto.arima(emotions_LA$anger[cp_emotions_LA[1]:cp_emotions_LA[2]])
```

    ## Series: emotions_LA$anger[cp_emotions_LA[1]:cp_emotions_LA[2]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.6086  -0.3563
    ## s.e.   0.0838   0.1701
    ## 
    ## sigma^2 estimated as 17.5:  log likelihood=-267.12
    ## AIC=540.24   AICc=540.51   BIC=547.87

``` r
auto.arima(emotions_LA$fear[cp_emotions_LA[1]:cp_emotions_LA[2]])
```

    ## Series: emotions_LA$fear[cp_emotions_LA[1]:cp_emotions_LA[2]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.8722  -0.2624
    ## s.e.   0.0747   0.0594
    ## 
    ## sigma^2 estimated as 17.6:  log likelihood=-267.88
    ## AIC=541.77   AICc=542.03   BIC=549.4

``` r
auto.arima(emotions_LA$disgust[cp_emotions_LA[1]:cp_emotions_LA[2]])
```

    ## Series: emotions_LA$disgust[cp_emotions_LA[1]:cp_emotions_LA[2]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.7730  -0.2728
    ## s.e.   0.0811   0.0893
    ## 
    ## sigma^2 estimated as 13.65:  log likelihood=-255.66
    ## AIC=517.32   AICc=517.59   BIC=524.95

``` r
auto.arima(emotions_LA$joy[cp_emotions_LA[1]:cp_emotions_LA[2]])
```

    ## Series: emotions_LA$joy[cp_emotions_LA[1]:cp_emotions_LA[2]] 
    ## ARIMA(2,1,2) with drift 
    ## 
    ## Coefficients:
    ##           ar1     ar2      ma1      ma2    drift
    ##       -0.0776  0.3788  -0.6463  -0.2815  -0.5964
    ## s.e.   0.5521  0.1376   0.5974   0.4766   0.0796
    ## 
    ## sigma^2 estimated as 38.07:  log likelihood=-302.57
    ## AIC=617.14   AICc=618.11   BIC=632.4

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_LA[cp_emotions_LA[1]:cp_emotions_LA[2],1:5], lag=10)
var_lag
```

    ## Best AIC:  lag= 1 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 1

``` r
# (3) Cointegration
coi_2 <- ca.jo(emotions_LA[cp_emotions_LA[1]:cp_emotions_LA[2],1:5], type = 'trace', ecdet = 'const', K=3)
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
    ## [1]  3.354272e-01  2.564117e-01  1.450114e-01  1.300731e-01  5.021038e-02
    ## [6] -4.204217e-17
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 4 |  4.74  7.52  9.24 12.97
    ## r <= 3 | 17.56 17.85 19.96 24.60
    ## r <= 2 | 31.97 32.00 34.91 41.07
    ## r <= 1 | 59.23 49.65 53.12 60.16
    ## r = 0  | 96.82 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##            sadness.l3   anger.l3      fear.l3 disgust.l3       joy.l3
    ## sadness.l3  1.0000000  1.0000000   1.00000000   1.000000   1.00000000
    ## anger.l3   -0.4283134  0.3355753  -6.01458283  -3.682419  -0.87720993
    ## fear.l3    -0.8122915 -0.1364902   1.47729253  -5.500121   0.31541088
    ## disgust.l3 -1.8858549 -0.6421895   3.86918934   6.268618  -0.06196102
    ## joy.l3      0.5218403 -0.7555739  -0.06290745   1.167168  -0.23793669
    ## constant   13.9431330 -0.7802930 -27.98796418 -17.795418 -25.34236048
    ##                constant
    ## sadness.l3    1.0000000
    ## anger.l3     -3.2281773
    ## fear.l3      -0.2702786
    ## disgust.l3    2.5172888
    ## joy.l3        0.9560115
    ## constant   -114.5115052
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##            sadness.l3    anger.l3      fear.l3   disgust.l3      joy.l3
    ## sadness.d  0.53028919 -0.25696579  0.024312705 -0.010622315 -0.20707775
    ## anger.d    0.55606866 -0.01439448  0.075793862 -0.005252078 -0.07873519
    ## fear.d     0.45865328  0.07357991 -0.018629984  0.061385789 -0.09900836
    ## disgust.d  0.62134320  0.05842059 -0.006374269 -0.029453704 -0.07112797
    ## joy.d     -0.03439719  0.61272156  0.024994780  0.020096523 -0.17162986
    ##                constant
    ## sadness.d -3.426700e-15
    ## anger.d   -8.954203e-16
    ## fear.d     1.151490e-16
    ## disgust.d -1.459044e-15
    ## joy.d      5.791310e-15

``` r
# (4) VAR
var_result_2 <- VAR(emotions_LA[cp_emotions_LA[1]:cp_emotions_LA[2],1:5], p=3)

#VAR stabilty
roots(var_result_2)
```

    ##  [1] 0.9620506 0.7290677 0.7290677 0.6833148 0.6833148 0.6424224 0.5418710
    ##  [8] 0.5076058 0.5076058 0.5070231 0.5070231 0.3813951 0.3813951 0.3364675
    ## [15] 0.3364675

``` r
serial.test(var_result_2)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_2
    ## Chi-squared = 353.02, df = 325, p-value = 0.1368

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_2, max.oi=1)
```

    ##    predictor  causes     chisq          p
    ## 1    sadness   anger 9.3768486 0.02467823
    ## 2    sadness    fear 5.8555517 0.11885339
    ## 3    sadness disgust 9.6246140 0.02204197
    ## 4    sadness     joy 2.2799074 0.51638104
    ## 5      anger sadness 2.6335499 0.45163815
    ## 6      anger    fear 2.6596354 0.44713047
    ## 7      anger disgust 1.4492619 0.69402917
    ## 8      anger     joy 1.5705392 0.66608814
    ## 9       fear sadness 1.8675865 0.60033893
    ## 10      fear   anger 4.0556385 0.25551852
    ## 11      fear disgust 3.9476829 0.26716913
    ## 12      fear     joy 2.7445641 0.43270713
    ## 13   disgust sadness 1.5807041 0.66377290
    ## 14   disgust   anger 5.8451063 0.11939419
    ## 15   disgust    fear 1.9406532 0.58481525
    ## 16   disgust     joy 0.6688759 0.88049907
    ## 17       joy sadness 1.7769666 0.61996004
    ## 18       joy   anger 6.6086135 0.08547608
    ## 19       joy    fear 2.9451366 0.40016161
    ## 20       joy disgust 1.8208981 0.61039787

**Stage 3**

``` r
# Plot
ts.plot(emotions_LA[cp_emotions_LA[2]:cp_emotions_LA[3],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 3')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-LA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_LA$sadness[cp_emotions_LA[2]:cp_emotions_LA[3]])
```

    ## Series: emotions_LA$sadness[cp_emotions_LA[2]:cp_emotions_LA[3]] 
    ## ARIMA(3,1,1) with drift 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ar3      ma1    drift
    ##       0.0006  -0.0600  -0.0560  -0.8976  -0.3013
    ## s.e.  0.1779   0.1629   0.1568   0.1492   0.0729
    ## 
    ## sigma^2 estimated as 23.84:  log likelihood=-196.62
    ## AIC=405.24   AICc=406.66   BIC=418.37

``` r
auto.arima(emotions_LA$anger[cp_emotions_LA[2]:cp_emotions_LA[3]])
```

    ## Series: emotions_LA$anger[cp_emotions_LA[2]:cp_emotions_LA[3]] 
    ## ARIMA(5,1,0) with drift 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3      ar4      ar5    drift
    ##       -0.9658  -0.8615  -0.7398  -0.4639  -0.2194  -0.1794
    ## s.e.   0.1200   0.1603   0.1690   0.1576   0.1205   0.0829
    ## 
    ## sigma^2 estimated as 8.49:  log likelihood=-161.84
    ## AIC=337.68   AICc=339.61   BIC=353.01

``` r
auto.arima(emotions_LA$fear[cp_emotions_LA[2]:cp_emotions_LA[3]])
```

    ## Series: emotions_LA$fear[cp_emotions_LA[2]:cp_emotions_LA[3]] 
    ## ARIMA(1,1,2) 
    ## 
    ## Coefficients:
    ##          ar1      ma1     ma2
    ##       0.9873  -1.9350  0.9451
    ## s.e.  0.0292   0.0803  0.0790
    ## 
    ## sigma^2 estimated as 7.748:  log likelihood=-161.5
    ## AIC=330.99   AICc=331.65   BIC=339.75

``` r
auto.arima(emotions_LA$disgust[cp_emotions_LA[2]:cp_emotions_LA[3]])
```

    ## Series: emotions_LA$disgust[cp_emotions_LA[2]:cp_emotions_LA[3]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.8739  -0.1935
    ## s.e.   0.0889   0.0465
    ## 
    ## sigma^2 estimated as 6.853:  log likelihood=-156.87
    ## AIC=319.75   AICc=320.13   BIC=326.31

``` r
auto.arima(emotions_LA$joy[cp_emotions_LA[2]:cp_emotions_LA[3]])
```

    ## Series: emotions_LA$joy[cp_emotions_LA[2]:cp_emotions_LA[3]] 
    ## ARIMA(0,1,1) with drift 
    ## 
    ## Coefficients:
    ##           ma1    drift
    ##       -0.8692  -0.2736
    ## s.e.   0.1225   0.0742
    ## 
    ## sigma^2 estimated as 17.02:  log likelihood=-186.88
    ## AIC=379.76   AICc=380.15   BIC=386.33

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_LA[cp_emotions_LA[2]:cp_emotions_LA[3],1:5], lag=10)
var_lag
```

    ## Best AIC:  lag= 10 
    ## Best BIC:  lag= 1 
    ## Best HQ :  lag= 10

``` r
# (3) Cointegration
coi_3 <- ca.jo(emotions_LA[cp_emotions_LA[2]:cp_emotions_LA[3],1:5], type = 'trace', ecdet = 'const', K=3)
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
    ## [1]  4.290731e-01  2.683953e-01  2.299211e-01  1.329856e-01  5.567811e-02
    ## [6] -3.029355e-16
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##           test 10pct  5pct  1pct
    ## r <= 4 |  3.67  7.52  9.24 12.97
    ## r <= 3 | 12.80 17.85 19.96 24.60
    ## r <= 2 | 29.52 32.00 34.91 41.07
    ## r <= 1 | 49.52 49.65 53.12 60.16
    ## r = 0  | 85.39 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##            sadness.l3   anger.l3     fear.l3  disgust.l3    joy.l3   constant
    ## sadness.l3   1.000000  1.0000000   1.0000000  1.00000000  1.000000  1.0000000
    ## anger.l3     3.780120  3.0236522   2.0606116 -0.45973204  1.923350 -0.5453860
    ## fear.l3     -3.758603 -0.8827191   5.7105602 -0.87684229  1.132973 -0.9680876
    ## disgust.l3  -4.774483 -0.4966229  -6.9207583 -0.48009483 -3.406131 -1.0047238
    ## joy.l3       1.696038 -2.2605800  -0.4980534  0.05415738 -3.012270 -0.1076551
    ## constant    -5.552456  7.3335247 -46.1114749 -1.99082260 65.728199 23.6363109
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##            sadness.l3    anger.l3     fear.l3   disgust.l3     joy.l3
    ## sadness.d  0.20784082 -0.27352737 -0.16762325 -0.370438854 0.02477690
    ## anger.d   -0.04625494 -0.26202603 -0.05221598 -0.014854356 0.01402972
    ## fear.d     0.10779934 -0.07886646 -0.12523035  0.215401601 0.01211711
    ## disgust.d  0.14251999 -0.15767872 -0.02957121 -0.015207294 0.02025575
    ## joy.d     -0.15640653 -0.01297936 -0.13313784  0.009664922 0.03510583
    ##                constant
    ## sadness.d -1.297048e-14
    ## anger.d   -7.299163e-15
    ## fear.d    -1.378508e-14
    ## disgust.d -6.153133e-15
    ## joy.d     -3.187747e-15

``` r
# (4) VAR
var_result_3 <- VAR(emotions_LA[cp_emotions_LA[2]:cp_emotions_LA[3],1:5], p=3)

#VAR stabilty
roots(var_result_3)
```

    ##  [1] 0.9520626 0.7022684 0.7022684 0.6949560 0.6673119 0.6588793 0.6588793
    ##  [8] 0.6574128 0.6574128 0.5822682 0.5622900 0.5622900 0.5554144 0.5554144
    ## [15] 0.1056908

``` r
serial.test(var_result_3)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_3
    ## Chi-squared = 317.83, df = 325, p-value = 0.6015

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_3, max.oi=1)
```

    ##    predictor  causes     chisq          p
    ## 1    sadness   anger 4.8049736 0.18664778
    ## 2    sadness    fear 4.4440074 0.21733940
    ## 3    sadness disgust 5.5334891 0.13664926
    ## 4    sadness     joy 5.6457315 0.13017748
    ## 5      anger sadness 5.8440510 0.11944896
    ## 6      anger    fear 0.7473526 0.86201358
    ## 7      anger disgust 5.3792650 0.14604091
    ## 8      anger     joy 5.2945105 0.15145900
    ## 9       fear sadness 8.6618798 0.03414113
    ## 10      fear   anger 5.0350138 0.16925114
    ## 11      fear disgust 8.6041296 0.03504462
    ## 12      fear     joy 3.3409167 0.34198828
    ## 13   disgust sadness 0.8152760 0.84580983
    ## 14   disgust   anger 2.0341076 0.56535781
    ## 15   disgust    fear 2.1699736 0.53788683
    ## 16   disgust     joy 2.9144775 0.40499989
    ## 17       joy sadness 6.6319908 0.08459996
    ## 18       joy   anger 6.4870755 0.09017361
    ## 19       joy    fear 6.1464343 0.10469920
    ## 20       joy disgust 7.4522821 0.05879729

**Stage 4**

``` r
# Plot
ts.plot(emotions_LA[cp_emotions_LA[3]:cp_emotions_LA[4],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 4')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-LA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_LA$sadness[cp_emotions_LA[3]:cp_emotions_LA[4]])
```

    ## Series: emotions_LA$sadness[cp_emotions_LA[3]:cp_emotions_LA[4]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.7034
    ## s.e.   0.1062
    ## 
    ## sigma^2 estimated as 11.55:  log likelihood=-211.23
    ## AIC=426.46   AICc=426.61   BIC=431.22

``` r
auto.arima(emotions_LA$anger[cp_emotions_LA[3]:cp_emotions_LA[4]])
```

    ## Series: emotions_LA$anger[cp_emotions_LA[3]:cp_emotions_LA[4]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.7745
    ## s.e.   0.0838
    ## 
    ## sigma^2 estimated as 3.895:  log likelihood=-167.86
    ## AIC=339.72   AICc=339.88   BIC=344.49

``` r
auto.arima(emotions_LA$fear[cp_emotions_LA[3]:cp_emotions_LA[4]])
```

    ## Series: emotions_LA$fear[cp_emotions_LA[3]:cp_emotions_LA[4]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.8405
    ## s.e.   0.0622
    ## 
    ## sigma^2 estimated as 3.789:  log likelihood=-166.9
    ## AIC=337.81   AICc=337.96   BIC=342.57

``` r
auto.arima(emotions_LA$disgust[cp_emotions_LA[3]:cp_emotions_LA[4]])
```

    ## Series: emotions_LA$disgust[cp_emotions_LA[3]:cp_emotions_LA[4]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.8081
    ## s.e.   0.0622
    ## 
    ## sigma^2 estimated as 3.16:  log likelihood=-159.56
    ## AIC=323.12   AICc=323.28   BIC=327.89

``` r
auto.arima(emotions_LA$joy[cp_emotions_LA[3]:cp_emotions_LA[4]])
```

    ## Series: emotions_LA$joy[cp_emotions_LA[3]:cp_emotions_LA[4]] 
    ## ARIMA(0,1,1) 
    ## 
    ## Coefficients:
    ##           ma1
    ##       -0.8094
    ## s.e.   0.0665
    ## 
    ## sigma^2 estimated as 9.628:  log likelihood=-204.13
    ## AIC=412.27   AICc=412.42   BIC=417.03

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_LA[cp_emotions_LA[3]:cp_emotions_LA[4],1:5], lag=15)
var_lag
```

    ## Best AIC:  lag= 12 
    ## Best BIC:  lag= 12 
    ## Best HQ :  lag= 12

``` r
# (3) Cointegration
coi_4 <- ca.jo(emotions_LA[cp_emotions_LA[3]:cp_emotions_LA[4],1:5], type = 'trace', ecdet='const', K=3)
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
    ## [1] 4.636419e-01 4.014956e-01 3.312804e-01 2.033242e-01 7.846224e-02
    ## [6] 2.220446e-16
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##            test 10pct  5pct  1pct
    ## r <= 4 |   6.37  7.52  9.24 12.97
    ## r <= 3 |  24.10 17.85 19.96 24.60
    ## r <= 2 |  55.49 32.00 34.91 41.07
    ## r <= 1 |  95.53 49.65 53.12 60.16
    ## r = 0  | 144.12 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##            sadness.l3   anger.l3    fear.l3 disgust.l3      joy.l3    constant
    ## sadness.l3  1.0000000  1.0000000  1.0000000  1.0000000   1.0000000   1.0000000
    ## anger.l3   -3.5239655 -2.1118598 -0.6943264  1.4055720  -1.7921331   0.3592773
    ## fear.l3    -3.3263441  0.9239621  0.3804062 -1.5648488   0.5257205  -0.5094271
    ## disgust.l3  5.5024447 -2.4969472  0.2157627 -1.6976488  -6.9354340  -0.4290711
    ## joy.l3     -0.7754942  1.2772463 -0.9459281 -0.2077991  -4.4395707  -0.5637447
    ## constant   17.7201452 -5.5692235 -0.2127663  4.5221379 184.8615785 -10.8679327
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##            sadness.l3    anger.l3    fear.l3  disgust.l3     joy.l3
    ## sadness.d  0.02205464 -0.10618840 -0.8786961 -0.12746687 0.03335586
    ## anger.d    0.11394906  0.26226848 -0.2302352 -0.13979959 0.01950797
    ## fear.d     0.24129836  0.02271422 -0.3993480  0.22864063 0.01384525
    ## disgust.d -0.10347559  0.14207201 -0.3328767  0.08168623 0.01870476
    ## joy.d     -0.03536975 -0.31484676  0.2379048  0.07525094 0.03391255
    ##                constant
    ## sadness.d -1.914061e-15
    ## anger.d    6.737776e-16
    ## fear.d     4.590531e-17
    ## disgust.d  2.213166e-15
    ## joy.d     -1.165412e-15

``` r
# (4) VAR
var_result_4 <- VAR(emotions_LA[cp_emotions_LA[3]:cp_emotions_LA[4],1:5], p=3)

#VAR stabilty
roots(var_result_4)
```

    ##  [1] 0.82614207 0.68181122 0.68181122 0.66511526 0.66511526 0.63721285
    ##  [7] 0.63070350 0.63070350 0.56155543 0.56155543 0.44436074 0.39250681
    ## [13] 0.39250681 0.20807792 0.06095404

``` r
serial.test(var_result_4)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_4
    ## Chi-squared = 300.41, df = 325, p-value = 0.8324

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_4, max.oi=1)
```

    ##    predictor  causes     chisq          p
    ## 1    sadness   anger 1.7427294 0.62747623
    ## 2    sadness    fear 0.2505450 0.96904443
    ## 3    sadness disgust 3.3876384 0.33563009
    ## 4    sadness     joy 3.6184682 0.30571910
    ## 5      anger sadness 3.9000478 0.27246140
    ## 6      anger    fear 4.0104552 0.26033737
    ## 7      anger disgust 4.4423795 0.21748785
    ## 8      anger     joy 5.2090835 0.15711181
    ## 9       fear sadness 2.2410616 0.52390586
    ## 10      fear   anger 0.9629467 0.81021672
    ## 11      fear disgust 1.4148056 0.70206792
    ## 12      fear     joy 4.4178158 0.21973912
    ## 13   disgust sadness 5.7636193 0.12369444
    ## 14   disgust   anger 0.9941274 0.80267295
    ## 15   disgust    fear 1.8238469 0.60975938
    ## 16   disgust     joy 3.0568422 0.38294399
    ## 17       joy sadness 3.4623393 0.32567953
    ## 18       joy   anger 3.7361998 0.29139488
    ## 19       joy    fear 1.9649678 0.57970952
    ## 20       joy disgust 9.2742619 0.02585816

**Stage 5**

``` r
# Plot
ts.plot(emotions_LA[cp_emotions_LA[4]:dim(emotions_LA)[1],1:5], col=c('red', 'blue', 'skyblue', 'orange', 'black'), main='Time series stage 5')
legend("topright",c("Sadness","Anger","Fear","Disgust","Joy"),col=c('red', 'blue', 'skyblue', 'orange', 'black'),lwd=c(2),bty="n")
```

![](Analysis-LA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# (1) order of integration
auto.arima(emotions_LA$sadness[cp_emotions_LA[4]:dim(emotions_LA)[1]])
```

    ## Series: emotions_LA$sadness[cp_emotions_LA[4]:dim(emotions_LA)[1]] 
    ## ARIMA(1,1,2) with drift 
    ## 
    ## Coefficients:
    ##          ar1      ma1     ma2    drift
    ##       0.9852  -1.8756  0.8788  -0.0162
    ## s.e.  0.0124   0.0191  0.0184   0.0098
    ## 
    ## sigma^2 estimated as 2.296:  log likelihood=-2038.69
    ## AIC=4087.39   AICc=4087.44   BIC=4112.46

``` r
auto.arima(emotions_LA$anger[cp_emotions_LA[4]:dim(emotions_LA)[1]])
```

    ## Series: emotions_LA$anger[cp_emotions_LA[4]:dim(emotions_LA)[1]] 
    ## ARIMA(1,1,2) with drift 
    ## 
    ## Coefficients:
    ##           ar1     ma1      ma2    drift
    ##       -0.9410  0.0901  -0.8275  -0.0085
    ## s.e.   0.0495  0.0467   0.0394   0.0040
    ## 
    ## sigma^2 estimated as 0.9733:  log likelihood=-1561.5
    ## AIC=3133   AICc=3133.06   BIC=3158.07

``` r
auto.arima(emotions_LA$fear[cp_emotions_LA[4]:dim(emotions_LA)[1]])
```

    ## Series: emotions_LA$fear[cp_emotions_LA[4]:dim(emotions_LA)[1]] 
    ## ARIMA(1,1,2) with drift 
    ## 
    ## Coefficients:
    ##           ar1      ma1      ma2    drift
    ##       -0.1747  -0.7234  -0.1385  -0.0087
    ## s.e.   0.8586   0.8655   0.7678   0.0040
    ## 
    ## sigma^2 estimated as 1.269:  log likelihood=-1709.19
    ## AIC=3428.39   AICc=3428.44   BIC=3453.46

``` r
auto.arima(emotions_LA$disgust[cp_emotions_LA[4]:dim(emotions_LA)[1]])
```

    ## Series: emotions_LA$disgust[cp_emotions_LA[4]:dim(emotions_LA)[1]] 
    ## ARIMA(1,1,2) with drift 
    ## 
    ## Coefficients:
    ##           ar1     ma1      ma2    drift
    ##       -0.9130  0.0610  -0.8101  -0.0076
    ## s.e.   0.0665  0.0619   0.0524   0.0033
    ## 
    ## sigma^2 estimated as 0.7103:  log likelihood=-1386.34
    ## AIC=2782.68   AICc=2782.73   BIC=2807.75

``` r
auto.arima(emotions_LA$joy[cp_emotions_LA[4]:dim(emotions_LA)[1]])
```

    ## Series: emotions_LA$joy[cp_emotions_LA[4]:dim(emotions_LA)[1]] 
    ## ARIMA(1,1,1) with drift 
    ## 
    ## Coefficients:
    ##          ar1      ma1    drift
    ##       0.0079  -0.8971  -0.0134
    ## s.e.  0.0331   0.0136   0.0050
    ## 
    ## sigma^2 estimated as 2.506:  log likelihood=-2087.89
    ## AIC=4183.78   AICc=4183.82   BIC=4203.84

``` r
# (2) lag selection
var_lag <- tsDyn::lags.select(emotions_LA[cp_emotions_LA[4]:dim(emotions_LA)[1],1:5], lag=30)
var_lag
```

    ## Best AIC:  lag= 8 
    ## Best BIC:  lag= 2 
    ## Best HQ :  lag= 3

``` r
# (3) Cointegration
coi_5 <- ca.jo(emotions_LA[cp_emotions_LA[4]:dim(emotions_LA)[1],1:5], type = 'trace', ecdet='const', K=3)
summary(coi_5)
```

    ## 
    ## ###################### 
    ## # Johansen-Procedure # 
    ## ###################### 
    ## 
    ## Test type: trace statistic , without linear trend and constant in cointegration 
    ## 
    ## Eigenvalues (lambda):
    ## [1] 2.610855e-01 2.502574e-01 2.075485e-01 1.869285e-01 2.032486e-02
    ## [6] 2.220446e-16
    ## 
    ## Values of teststatistic and critical values of test:
    ## 
    ##             test 10pct  5pct  1pct
    ## r <= 4 |   22.79  7.52  9.24 12.97
    ## r <= 3 |  252.49 17.85 19.96 24.60
    ## r <= 2 |  510.70 32.00 34.91 41.07
    ## r <= 1 |  830.41 49.65 53.12 60.16
    ## r = 0  | 1166.27 71.86 76.07 84.45
    ## 
    ## Eigenvectors, normalised to first column:
    ## (These are the cointegration relations)
    ## 
    ##             sadness.l3   anger.l3    fear.l3 disgust.l3     joy.l3    constant
    ## sadness.l3  1.00000000   1.000000   1.000000  1.0000000   1.000000   1.0000000
    ## anger.l3   -0.26842673 -10.598976  15.773263 -0.5188050   6.298390   0.4717832
    ## fear.l3    -0.35924688   7.429532   8.197369 -1.2156367   3.100956   0.5540050
    ## disgust.l3 -1.31010479   5.937384 -13.625441  1.8219713   3.767955   1.0822946
    ## joy.l3      0.10345329  -2.340900  -8.327401 -0.9115314   5.336979   0.6768952
    ## constant    0.01466212  -1.222477  -2.458479  0.1243293 -74.847586 -75.0623840
    ## 
    ## Weights W:
    ## (This is the loading matrix)
    ## 
    ##           sadness.l3    anger.l3       fear.l3  disgust.l3       joy.l3
    ## sadness.d -0.3760322 -0.01906650 -0.0208124251 -0.21379660 -0.004274123
    ## anger.d    0.2455663  0.03741644 -0.0244852719 -0.04909688 -0.002434782
    ## fear.d     0.1428022 -0.06167547 -0.0256739427  0.09038500 -0.002563303
    ## disgust.d  0.3678304 -0.00356849 -0.0001069853 -0.13260574 -0.002257268
    ## joy.d     -0.1377139  0.02200182  0.0215410074  0.19787991 -0.004759337
    ##               constant
    ## sadness.d 4.764100e-17
    ## anger.d   1.099784e-17
    ## fear.d    2.751691e-17
    ## disgust.d 3.363064e-18
    ## joy.d     3.655052e-17

``` r
# (4) VAR
var_result_5 <- VAR(emotions_LA[cp_emotions_LA[4]:dim(emotions_LA)[1],1:5], p=3)

#VAR stability
roots(var_result_5)
```

    ##  [1] 0.968924326 0.598220311 0.598220311 0.486446672 0.411106338 0.362194166
    ##  [7] 0.362194166 0.343364431 0.343364431 0.341796273 0.341796273 0.308904040
    ## [13] 0.308904040 0.118994244 0.008937326

``` r
serial.test(var_result_5)
```

    ## 
    ##  Portmanteau Test (asymptotic)
    ## 
    ## data:  Residuals of VAR object var_result_5
    ## Chi-squared = 552.54, df = 325, p-value = 4.841e-14

``` r
# (5) Toda-Yamamoto

toda.yamamoto(var_result_5, max.oi=1)
```

    ##    predictor  causes      chisq            p
    ## 1    sadness   anger 10.2183156 1.679868e-02
    ## 2    sadness    fear  5.7190500 1.261085e-01
    ## 3    sadness disgust 16.5617820 8.696140e-04
    ## 4    sadness     joy  0.5482198 9.081770e-01
    ## 5      anger sadness  0.5752679 9.020701e-01
    ## 6      anger    fear  5.7389216 1.250267e-01
    ## 7      anger disgust  0.4915131 9.207520e-01
    ## 8      anger     joy  6.1228713 1.057829e-01
    ## 9       fear sadness  1.1406367 7.672744e-01
    ## 10      fear   anger  1.6621823 6.453740e-01
    ## 11      fear disgust  2.8374358 4.173743e-01
    ## 12      fear     joy  2.1306314 5.457407e-01
    ## 13   disgust sadness  1.2755302 7.349517e-01
    ## 14   disgust   anger  1.4094690 7.033167e-01
    ## 15   disgust    fear  0.8163090 8.455623e-01
    ## 16   disgust     joy  2.8207790 4.200907e-01
    ## 17       joy sadness 28.1294329 3.411780e-06
    ## 18       joy   anger 16.1218531 1.070582e-03
    ## 19       joy    fear 39.3642798 1.453070e-08
    ## 20       joy disgust 16.3598522 9.567257e-04
