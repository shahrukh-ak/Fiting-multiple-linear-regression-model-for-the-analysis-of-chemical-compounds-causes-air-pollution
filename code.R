#importing data
install.packages("missMDA")
library(missMDA)
data(ozone)
attach(ozone)
head(ozone)
maxO3   T9  T12  T15 Ne9 Ne12 Ne15     Vx9    Vx12    Vx15 maxO3v  vent
20010601    87 15.6 18.5 18.4   4    4    8  0.6946 -1.7101 -0.6946     84  Nord
20010602    NA 17.0 18.4 17.7   5    5    7 -4.3301 -4.0000 -3.0000     87  Nord
20010603    92 15.3 17.6 19.5   2    5    4  2.9544  1.8794  0.5209     82   Est
20010604   114 16.2 19.7 22.5   1   NA    0  0.9848      NA      NA     92  <NA>
  20010605    94 17.4 20.5 20.4   8    8    7 -0.5000 -2.9544 -4.3301    114 Ouest
20010606    80 17.7 19.8 18.3   6    6    7 -5.6382 -5.0000 -6.0000     NA Ouest
pluie
20010601   Sec
20010602   Sec
20010603  <NA>
  20010604   Sec
20010605   Sec
20010606 Pluie
dim(ozone)
[1] 112  13



# Generate the list of variables included in the data using R.
names(ozone)
[1] "maxO3" "T9" "T12" "T15" "Ne9" "Ne12" "Ne15"
[8] "Vx9" "Vx12" "Vx15" "maxO3v" "vent" "pluie"
# Create the subset of the data containing only the first 11 variables.
data=ozone[,1:11]
attach(data)
The following objects are masked from ozone:
  maxO3, maxO3v, Ne12, Ne15, Ne9, T12, T15, T9, Vx12,
Vx15, Vx9
names(data)
[1] "maxO3" "T9" "T12" "T15" "Ne9" "Ne12" "Ne15"
[8] "Vx9" "Vx12" "Vx15" "maxO3v"

data=ozone[,-c(12,13)]
names(data)
[1] "maxO3" "T9" "T12" "T15" "Ne9" "Ne12" "Ne15"
[8] "Vx9" "Vx12" "Vx15" "maxO3v"
head(data)
maxO3 T9 T12 T15 Ne9 Ne12 Ne15 Vx9 Vx12
20010601 87 15.6 18.5 18.4 4 4 8 0.6946 -1.7101
20010602 NA 17.0 18.4 17.7 5 5 7 -4.3301 -4.0000
20010603 92 15.3 17.6 19.5 2 5 4 2.9544 1.8794
20010604 114 16.2 19.7 22.5 1 NA 0 0.9848 NA
20010605 94 17.4 20.5 20.4 8 8 7 -0.5000 -2.9544
20010606 80 17.7 19.8 18.3 6 6 7 -5.6382 -5.0000
Vx15 maxO3v
20010601 -0.6946 84
20010602 -3.0000 87
20010603 0.5209 82
20010604 NA 92
20010605 -4.3301 114
20010606 -6.0000 NA
# Fiting a multiple linear regression model for maxO3 as a response variable and all the
# remaining 10 variables as regressor variables. 
# Using summary option to identify which of these variables appear to be significant.
newdata=na.omit(data)
attach(newdata)
The following objects are masked from data:
  maxO3, maxO3v, Ne12, Ne15, Ne9, T12, T15, T9, Vx12,
Vx15, Vx9
The following objects are masked from ozone:
  maxO3, maxO3v, Ne12, Ne15, Ne9, T12, T15, T9, Vx12,
Vx15, Vx9
head(newdata)
          maxO3   T9  T12  T15 Ne9 Ne12 Ne15     Vx9    Vx12    Vx15 maxO3v
20010601    87 15.6 18.5 18.4   4    4    8  0.6946 -1.7101 -0.6946     84
20010603    92 15.3 17.6 19.5   2    5    4  2.9544  1.8794  0.5209     82
20010605    94 17.4 20.5 20.4   8    8    7 -0.5000 -2.9544 -4.3301    114
20010617    83 15.4 17.4 16.6   8    7    7 -4.3301 -2.0521 -3.0000     70
20010620   145 21.0 24.6 26.9   0    1    1 -0.3420 -1.5321 -0.6840    121
20010622   121 19.7 24.2 26.9   2    1    0  1.5321  1.7321  2.0000     81
model=lm(maxO3 ~ T9 + T12+ T15+ Ne9+ Ne12+ Ne15+ Vx9 + Vx12+ Vx15+ maxO3v)
model
Call:
  lm(formula = maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + Vx9 +
       Vx12 + Vx15 + maxO3v)
Coefficients:
  (Intercept) T9 T12 T15 Ne9
2.9941 6.0162 -4.2879 2.2675 -1.2472
Ne12 Ne15 Vx9 Vx12 Vx15
0.1917 -0.2909 3.1040 -0.8650 -0.3501
maxO3v
0.3023

summary(model)
Call:
  lm(formula = maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 + Vx9 +
       Vx12 + Vx15 + maxO3v)
Residuals:
  Min 1Q Median 3Q Max
-34.283 -9.348 -1.107 8.886 24.448
Coefficients:
  Estimate Std. Error t value Pr(>|t|)
(Intercept) 2.9941 25.8868 0.116 0.9089
T9 6.0162 3.7342 1.611 0.1208
T12 -4.2879 3.7180 -1.153 0.2606
T15 2.2675 3.4019 0.667 0.5117
Ne9 -1.2472 2.1251 -0.587 0.5630
Ne12 0.1917 2.4544 0.078 0.9384
Ne15 -0.2909 2.3791 -0.122 0.9037
Vx9 3.1040 2.5347 1.225 0.2331
Vx12 -0.8650 2.2006 -0.393 0.6979
Vx15 -0.3501 2.0442 -0.171 0.8655
maxO3v 0.3023 0.1336 2.263 0.0334 *
  ---
  Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 15.83 on 23 degrees of freedom
Multiple R-squared: 0.7757, Adjusted R-squared: 0.6781 
F-statistic: 7.952 on 10 and 23 DF, p-value: 2.156e-05

#  Hence, only maxO3v variable is significant