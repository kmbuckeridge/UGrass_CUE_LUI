setwd("/Users/katebuckeridge/Dropbox/R/UGrass/cue")
setwd("C:/Users/kbuckeri/Dropbox/R/UGrass/cue") ## at work


luicue <- read.csv("metaotu2018.csv", header=TRUE, sep=",") 

fit <- lm(CUE_G~LUIwT, data=luicue)
summary(fit) #adj r2=-0.007,P=0.82

#for ANCOVA 
library(car)
library(multcomp)
#library(compute.es)
#library(effects)
#library(pastecs)
#library(WRS)

#R uses non-orthogonal contrasts so need to set to orthogonal:
contrasts(luicue$Site)=contr.poly(9) 

#testing homogeneity of regression slopes - just looking at interaction - if sig then no HOV:

#the components of CUE and LUI
fit2 <- aov(CUE_N~LUIwT*Site, data=luicue)
Anova(fit2, type="III")

# Response: X13CO2_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)   14.0337   1 399.8756 < 2.2e-16 ***
# LUIlevel       0.0728   2   1.0369   0.35804    
# Site           0.6366   8   2.2672   0.02787 *  
# LUIlevel:Site  3.9352  16   7.0081 8.806e-11 ***
# Residuals      3.7903 108 
# Response: X13CO2_G
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)   1843.20   1 1564.690 < 2.2e-16 ***
# LUIlevel        38.92   2   16.520 5.589e-07 ***
# Site           275.24   8   29.207 < 2.2e-16 ***
# LUIlevel:Site   40.22  16    2.134    0.0116 *  
# Residuals      126.05 107  
# Response: X13MBC_G
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept)   7958.5   1 1186.5536 < 2.2e-16 ***
# LUIlevel       201.8   2   15.0461 1.721e-06 ***
# Site           579.6   8   10.8009 4.509e-11 ***
# LUIlevel:Site  445.3  16    4.1491 3.661e-06 ***
# Residuals      724.4 108
# Response: X13MBC_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)   30.1684   1 166.0304 < 2.2e-16 ***
# LUIlevel       0.7859   2   2.1627  0.120162    
# Site          21.2714   8  14.6332  3.83e-14 ***
# LUIlevel:Site  6.7633  16   2.3263  0.005647 ** 
# Residuals     18.8972 104 
# Response: CUE_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)   12.2268   1 568.5979 < 2.2e-16 ***
# LUIlevel       0.0649   2   1.5098    0.2257    
# Site           1.2894   8   7.4951  7.17e-08 ***
# LUIlevel:Site  0.4191  16   1.2182    0.2667    
# Residuals      2.2579 105 
# Response: CUE_G
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept)   20.2810   1 3643.8608 < 2.2e-16 ***
# LUIlevel       0.0031   2    0.2829 0.7541295    
# Site           0.5772   8   12.9640 6.464e-13 ***
# LUIlevel:Site  0.2624  16    2.9465 0.0004675 ***
# Residuals      0.5955 107 

# Anova Table (Type III tests)
# 
# Response: CUE_G
#              Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 4.5592   1 638.2887 < 2.2e-16 ***
# LUIwT       0.0005   1   0.0680    0.7947    
# Site        0.4557   8   7.9753 1.612e-08 ***
# LUIwT:Site  0.0338   8   0.5908    0.7838    No interaction, so we can say no effect of LUI on CUE_G
# Residuals   0.8286 116

# Response: CUE_N
#             Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 2.23210   1 105.0861 < 2.2e-16 ***
# LUIwT       0.04105   1   1.9326   0.16718    
# Site        0.99126   8   5.8335 3.055e-06 ***
# LUIwT:Site  0.30850   8   1.8155   0.08113 .  Ditto for LUI on CUE_N
# Residuals   2.42144 114 

# Response: X13CO2_G
#              Sum Sq  Df  F value  Pr(>F)    
# (Intercept) 393.72   1 301.0725 < 2e-16 ***
# LUIwT         4.23   1   3.2354 0.07466 .  
# Site        216.50   8  20.6944 < 2e-16 ***
# LUIwT:Site   17.33   8   1.6563 0.11654    Ditto for 13CO2_G
# Residuals   151.70 116

# Response: X13CO2_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 4.8947   1 109.7427 < 2.2e-16 ***
# LUIwT       0.1178   1   2.6417    0.1068    
# Site        0.4520   8   1.2668    0.2675    
# LUIwT:Site  2.5606   8   7.1762 1.051e-07 ***  13CO2_N not sig but varies by site
#   Residuals   5.2184 117

# Response: X13MBC_N
# Sum Sq  Df F value    Pr(>F)    
# (Intercept)  7.1062   1 35.7946 2.633e-08 ***
#   LUIwT        0.0848   1  0.4270   0.51477    
# Site        19.1703   8 12.0703 2.428e-12 ***
#   LUIwT:Site   3.6011   8  2.2674   0.02751 *  So does MB13C_N
#   Residuals   22.4337 113 

# Response: X13MBC_G
#             Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 1681.54   1 173.1482 < 2.2e-16 ***
# LUIwT         29.72   1   3.0598   0.08288 .  
# Site         463.24   8   5.9625 2.087e-06 ***
# LUIwT:Site    98.52   8   1.2680   0.26682    MB13C_G does not - no LUI on CUEG, regardless of site
# Residuals   1136.26 117 

#should also do a Levene's test for equality of variances
#The variances of the dependent variable for the conditional distributions are equal, known as the assumption of homogeneity of variance
# this is an assumption for anova too - should adjust the data
leveneTest(CUE_N~Site,luicue)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value    Pr(>F)    
# group   8  4.4501 9.281e-05 ***
#       123                      
leveneTest(CUE_G~Site,luicue)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value    Pr(>F)    
# group   8  7.3858 5.091e-08 ***
#       125  
G_log <- log(luicue$CUE_G)
N_log <- log(luicue$CUE_N)
leveneTest(N_log~Site,luicue)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   8  2.7424 0.008076 **
# 123 
leveneTest(G_log~Site,luicue)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)    
# group   8  9.5838 2.76e-10 ***
# 125
#transform did not really help

fit2 <- aov(CUE_N~Nit*Site, data=luicue)
Anova(fit2, type="III")

#the components of LUI
# Response: CUE_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 15.6790   1 747.7641 < 2.2e-16 ***
# Nit          0.0008   1   0.0367   0.84844    
# Site         2.2008   8  13.1200 2.777e-13 ***
# Nit:Site     0.3194   8   1.9041   0.06595 .  
# Residuals    2.3903 114 
fit2 <- aov(X13CO2_N~Nit*Site, data=luicue)
Anova(fit2, type="III")
# Response: X13CO2_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 15.8890   1 392.6187 < 2.2e-16 ***
# Nit          0.3249   1   8.0289  0.005424 ** 
# Site         0.9708   8   2.9985  0.004279 ** 
# Nit:Site     2.9635   8   9.1534 1.023e-09 ***
# Residuals    4.7349 117 
fit2 <- aov(X13MBC_N~Nit*Site, data=luicue)
Anova(fit2, type="III")
# Response: X13MBC_N
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept) 31.745   1 162.8669 < 2.2e-16 ***
# Nit          0.005   1   0.0232   0.87929    
# Site        25.213   8  16.1694 1.006e-15 ***
# Nit:Site     4.152   8   2.6626   0.01026 *  
# Residuals   22.025 113
fit2 <- aov(CUE_G~Nit*Site, data=luicue)
Anova(fit2, type="III")
# Response: CUE_G
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept) 26.5604   1 3731.6989 < 2.2e-16 ***
# Nit          0.0116   1    1.6301    0.2042    
# Site         0.7520   8   13.2077 2.013e-13 ***
# Nit:Site     0.0339   8    0.5961    0.7795    
# Residuals    0.8256 116 
fit2 <- aov(X13CO2_G~Nit*Site, data=luicue)
Anova(fit2, type="III")
# Response: X13CO2_G
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept) 2338.66   1 1791.8563 < 2.2e-16 ***
# Nit           30.23   1   23.1650  4.52e-06 ***
# Site         417.97   8   40.0306 < 2.2e-16 ***
# Nit:Site      16.67   8    1.5962    0.1333    
# Residuals    151.40 116
fit2 <- aov(X13MBC_G~Nit*Site, data=luicue)
Anova(fit2, type="III")
# Response: X13MBC_G
# Sum Sq  Df   F value    Pr(>F)    
# (Intercept) 10334.9   1 1251.0085 < 2.2e-16 ***
# Nit           295.0   1   35.7079 2.536e-08 ***
# Site         1048.8   8   15.8693 1.169e-15 ***
# Nit:Site      228.3   8    3.4547  0.001313 ** 
# Residuals     966.6 117

## ancova doesn't work for other components b/c singularities, run a lm instead

fit <- lm(CUE_N~Nit*Site, data=luicue)
summary(fit)
# Call:
#   lm(formula = CUE_N ~ Nit * Site, data = luicue)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.46300 -0.05880  0.01288  0.06653  0.42133 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.527438   0.019288  27.345  < 2e-16 ***
# Nit          0.004289   0.022391   0.192  0.84844    
# Site.L       0.183465   0.063232   2.901  0.00446 ** 
# Site.Q       0.002614   0.062208   0.042  0.96656    
# Site.C      -0.031891   0.060094  -0.531  0.59667    
# Site^4      -0.189024   0.058101  -3.253  0.00150 ** 
# Site^5      -0.238061   0.058637  -4.060 9.03e-05 ***
# Site^6      -0.087700   0.052781  -1.662  0.09935 .  
# Site^7       0.234433   0.054506   4.301 3.61e-05 ***
# Site^8       0.264458   0.052306   5.056 1.65e-06 ***
# Nit:Site.L   0.074177   0.070487   1.052  0.29487    
# Nit:Site.Q  -0.168472   0.061320  -2.747  0.00698 ** 
# Nit:Site.C   0.060928   0.066335   0.918  0.36031    
# Nit:Site^4  -0.038262   0.070758  -0.541  0.58974    
# Nit:Site^5   0.053244   0.076490   0.696  0.48779    
# Nit:Site^6   0.001512   0.066430   0.023  0.98188    
# Nit:Site^7  -0.020129   0.067274  -0.299  0.76533    
# Nit:Site^8   0.063833   0.056330   1.133  0.25951  

fit <- lm(CUE_N~Till*Site, data=luicue)
summary(fit)
# Call:
#   lm(formula = CUE_N ~ Till * Site, data = luicue)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -0.463 -0.066  0.006  0.064  0.448 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.514764   0.014598  35.264  < 2e-16 ***
# Till         0.018568   0.009527   1.949  0.05368 .  
# Site.L       0.215809   0.045928   4.699 7.20e-06 ***
# Site.Q      -0.070956   0.045047  -1.575  0.11792    
# Site.C       0.002953   0.043984   0.067  0.94658    
# Site^4      -0.222041   0.045199  -4.912 2.95e-06 ***
# Site^5      -0.222662   0.043276  -5.145 1.09e-06 ***
# Site^6      -0.074719   0.044096  -1.694  0.09283 .  
# Site^7       0.210625   0.040689   5.177 9.50e-07 ***
# Site^8       0.270688   0.041875   6.464 2.45e-09 ***
# Till:Site.L  0.044737   0.037532   1.192  0.23569    
# Till:Site.Q -0.087603   0.033320  -2.629  0.00971 ** 
# Till:Site.C -0.040305   0.059786  -0.674  0.50154    
# Till:Site^4  0.011585   0.020934   0.553  0.58104    
# Till:Site^5  0.059679   0.050015   1.193  0.23520    
# Till:Site^6        NA         NA      NA       NA    
# Till:Site^7        NA         NA      NA       NA    
# Till:Site^8        NA         NA      NA       NA

fit <- lm(CUE_G~Till*Site, data=luicue)
summary(fit)

# lm(formula = CUE_G ~ Till * Site, data = luicue)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.2113 -0.0375 -0.0010  0.0435  0.2987 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.669958   0.008412  79.645  < 2e-16 ***
# Till        -0.005636   0.005225  -1.079 0.282868    
# Site.L       0.101337   0.026249   3.861 0.000184 ***
# Site.Q       0.113970   0.025705   4.434 2.08e-05 ***
# Site.C      -0.088658   0.025211  -3.517 0.000619 ***
# Site^4      -0.230031   0.026077  -8.821 1.14e-14 ***
# Site^5      -0.025397   0.025055  -1.014 0.312803    
# Site^6       0.050961   0.025572   1.993 0.048568 *  
# Site^7       0.023852   0.023606   1.010 0.314340    
# Site^8      -0.019804   0.024296  -0.815 0.416639    
# Till:Site.L  0.017601   0.021639   0.813 0.417628    
# Till:Site.Q  0.007490   0.018574   0.403 0.687483    
# Till:Site.C -0.001475   0.034507  -0.043 0.965966    
# Till:Site^4 -0.004673   0.011812  -0.396 0.693107    
# Till:Site^5  0.015552   0.028926   0.538 0.591830    
# Till:Site^6        NA         NA      NA       NA    
# Till:Site^7        NA         NA      NA       NA    
# Till:Site^8        NA         NA      NA       NA  

fit <- lm(CUE_G~Mow*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.20545 -0.03400  0.00533  0.03567  0.36343 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.634013   0.030874  20.536  < 2e-16 ***
#   Mow          0.018910   0.037368   0.506  0.61376    
# Site.L       0.144465   0.074980   1.927  0.05640 .  
# Site.Q       0.173220   0.053136   3.260  0.00145 ** 
# Site.C      -0.184385   0.111809  -1.649  0.10176    
# Site^4      -0.227544   0.074348  -3.061  0.00273 ** 
# Site^5       0.022209   0.052964   0.419  0.67573    
# Site^6      -0.007226   0.141579  -0.051  0.95938    
# Site^7       0.096073   0.134271   0.716  0.47569    
# Site^8      -0.038767   0.044530  -0.871  0.38573    
# Mow:Site.L  -0.075440   0.174784  -0.432  0.66680    
# Mow:Site.Q  -0.041465   0.049336  -0.840  0.40234    
# Mow:Site.C   0.127435   0.133600   0.954  0.34209    
# Mow:Site^4  -0.032716   0.125983  -0.260  0.79555    
# Mow:Site^5   0.102091   0.156541   0.652  0.51555    
# Mow:Site^6         NA         NA      NA       NA    
# Mow:Site^7         NA         NA      NA       NA    
# Mow:Site^8         NA         NA      NA       NA 

fit <- lm(CUE_N~Mow*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.46300 -0.06672  0.00700  0.06470  0.43438 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.446748   0.057208   7.809 2.72e-12 ***
# Mow          0.102256   0.070231   1.456 0.148074    
# Site.L       0.477115   0.140896   3.386 0.000965 ***
# Site.Q      -0.006462   0.096737  -0.067 0.946856    
# Site.C      -0.377049   0.210955  -1.787 0.076471 .  
# Site^4       0.105139   0.143148   0.734 0.464128    
# Site^5      -0.027914   0.099494  -0.281 0.779543    
# Site^6      -0.629223   0.270285  -2.328 0.021630 *  
# Site^7       0.684433   0.252474   2.711 0.007721 ** 
# Site^8       0.183436   0.081997   2.237 0.027174 *  
# Mow:Site.L  -0.462578   0.327399  -1.413 0.160344    
# Mow:Site.Q  -0.024920   0.090816  -0.274 0.784258    
# Mow:Site.C   0.460965   0.250992   1.837 0.068812 .  
# Mow:Site^4  -0.569171   0.240721  -2.364 0.019704 *  
# Mow:Site^5   0.461993   0.292105   1.582 0.116442    
# Mow:Site^6         NA         NA      NA       NA    
# Mow:Site^7         NA         NA      NA       NA    
# Mow:Site^8         NA         NA      NA       NA 

fit <- lm(CUE_N~Graze*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.60111 -0.06160 -0.00173  0.06400  0.42412 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   0.10594    0.18735   0.565  0.57287   
# Graze         0.14466    0.09177   1.576  0.11769   
# Site.L        1.25165    0.43540   2.875  0.00482 **
# Site.Q        0.62004    0.27454   2.258  0.02580 * 
# Site.C       -1.74900    0.68245  -2.563  0.01167 * 
# Site^4        0.88645    0.41587   2.132  0.03517 * 
# Site^5        0.51686    0.30815   1.677  0.09620 . 
# Site^6       -2.08379    0.82786  -2.517  0.01321 * 
# Site^7        2.25355    0.79996   2.817  0.00571 **
# Site^8       -0.60370    0.37241  -1.621  0.10775   
# Graze:Site.L -0.56067    0.25953  -2.160  0.03282 * 
# Graze:Site.Q -0.15332    0.14237  -1.077  0.28375   
# Graze:Site.C  1.07644    0.40549   2.655  0.00906 **
# Graze:Site^4 -0.98106    0.36672  -2.675  0.00856 **
# Graze:Site^5 -0.38336    0.19977  -1.919  0.05747 . 
# Graze:Site^6  1.49940    0.62684   2.392  0.01838 * 
# Graze:Site^7 -1.25714    0.48306  -2.602  0.01047 * 
# Graze:Site^8       NA         NA      NA       NA 

fit <- lm(CUE_G~Graze*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.21807 -0.03511  0.00560  0.03913  0.34304 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.83387    0.10389   8.026 8.73e-13 ***
#   Graze        -0.08039    0.05114  -1.572   0.1186    
# Site.L       -0.31346    0.24084  -1.302   0.1956    
# Site.Q       -0.13791    0.15225  -0.906   0.3669    
# Site.C        0.42225    0.37758   1.118   0.2657    
# Site^4       -0.58040    0.22961  -2.528   0.0128 *  
# Site^5       -0.22461    0.17056  -1.317   0.1905    
# Site^6        0.78306    0.45775   1.711   0.0898 .  
# Site^7       -0.76330    0.44297  -1.723   0.0875 .  
# Site^8        0.26773    0.20646   1.297   0.1973    
# Graze:Site.L  0.22939    0.14348   1.599   0.1126    
# Graze:Site.Q  0.05859    0.07932   0.739   0.4616    
# Graze:Site.C -0.23149    0.22435  -1.032   0.3043    
# Graze:Site^4  0.27103    0.20241   1.339   0.1832    
# Graze:Site^5  0.01909    0.11084   0.172   0.8635    
# Graze:Site^6 -0.52750    0.34649  -1.522   0.1306    
# Graze:Site^7  0.58432    0.26769   2.183   0.0310 *  
# Graze:Site^8       NA         NA      NA       NA 

fit <- lm(X13CO2_G~Graze*Site, data=luicue)
summary(fit)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.6574 -0.4945 -0.0051  0.4957  2.8200 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    9.35759    1.48804   6.289 5.73e-09 ***
# Graze         -1.82040    0.73241  -2.485  0.01435 *  
# Site.L       -11.57268    3.44955  -3.355  0.00107 ** 
# Site.Q        -2.69977    2.18066  -1.238  0.21818    
# Site.C        14.47967    5.40810   2.677  0.00849 ** 
# Site^4        -3.49557    3.28872  -1.063  0.29002    
# Site^5        -3.11857    2.44291  -1.277  0.20428    
# Site^6        15.04721    6.55632   2.295  0.02351 *  
# Site^7       -16.15861    6.34466  -2.547  0.01217 *  
# Site^8         4.53185    2.95712   1.533  0.12809    
# Graze:Site.L   6.30099    2.05500   3.066  0.00269 ** 
# Graze:Site.Q   0.06783    1.13615   0.060  0.95250    
# Graze:Site.C  -9.09008    3.21334  -2.829  0.00550 ** 
# Graze:Site^4   5.35216    2.89905   1.846  0.06739 .  
# Graze:Site^5   4.96873    1.58761   3.130  0.00221 ** 
# Graze:Site^6 -10.30593    4.96272  -2.077  0.04002 *  
# Graze:Site^7   5.41195    3.83409   1.412  0.16074    
# Graze:Site^8        NA         NA      NA       NA 

fit <- lm(X13CO2_N~Graze*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.50906 -0.12716 -0.01249  0.10176  0.65400 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.32686    0.27623   1.183   0.2391  
# Graze         0.23957    0.13419   1.785   0.0768 .
# Site.L        0.22223    0.64381   0.345   0.7306  
# Site.Q       -0.11710    0.40355  -0.290   0.7722  
# Site.C       -0.01685    1.00969  -0.017   0.9867  
# Site^4        0.34528    0.61437   0.562   0.5752  
# Site^5        0.09778    0.45779   0.214   0.8312  
# Site^6       -0.33497    1.22133  -0.274   0.7844  
# Site^7        0.16434    1.17753   0.140   0.8892  
# Site^8       -0.45833    0.55362  -0.828   0.4094  
# Graze:Site.L -0.35793    0.38422  -0.932   0.3535  
# Graze:Site.Q  0.19488    0.20570   0.947   0.3454  
# Graze:Site.C -0.34025    0.60130  -0.566   0.5726  
# Graze:Site^4  0.01335    0.54363   0.025   0.9805  
# Graze:Site^5 -0.11724    0.29803  -0.393   0.6947  
# Graze:Site^6  0.20614    0.92723   0.222   0.8245  
# Graze:Site^7 -0.19243    0.70813  -0.272   0.7863  
# Graze:Site^8       NA         NA      NA       NA 

fit <- lm(X13MBC_N~Graze*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.53556 -0.12818 -0.00969  0.10803  2.41182 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.8973     0.5559  -1.614 0.109255    
# Graze          0.6888     0.2722   2.531 0.012749 *  
# Site.L         3.9258     1.2943   3.033 0.002997 ** 
# Site.Q         1.4137     0.8193   1.725 0.087154 .  
# Site.C        -5.7060     2.0252  -2.818 0.005706 ** 
# Site^4         3.6053     1.2345   2.921 0.004213 ** 
# Site^5         1.9031     0.9144   2.081 0.039648 *  
# Site^6        -7.3972     2.4550  -3.013 0.003187 ** 
# Site^7         7.0191     2.3723   2.959 0.003756 ** 
# Site^8        -2.7313     1.1044  -2.473 0.014869 *  
# Graze:Site.L  -1.8239     0.7701  -2.368 0.019550 *  
# Graze:Site.Q   0.1690     0.4231   0.400 0.690266    
# Graze:Site.C   3.0218     1.2027   2.513 0.013386 *  
# Graze:Site^4  -3.7648     1.0876  -3.461 0.000757 ***
# Graze:Site^5  -1.7211     0.5925  -2.905 0.004416 ** 
# Graze:Site^6   5.1933     1.8589   2.794 0.006112 ** 
# Graze:Site^7  -4.0745     1.4325  -2.844 0.005277 ** 
# Graze:Site^8       NA         NA      NA       NA

fit <- lm(X13MBC_G~Graze*Site, data=luicue)
summary(fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6.142 -1.768  0.266  1.594  9.661 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    22.134      3.482   6.357 4.04e-09 ***
# Graze          -6.201      1.691  -3.666 0.000371 ***
# Site.L        -28.648      8.115  -3.530 0.000593 ***
# Site.Q         -6.318      5.087  -1.242 0.216718    
# Site.C         31.590     12.727   2.482 0.014471 *  
# Site^4        -19.357      7.744  -2.500 0.013810 *  
# Site^5         -7.041      5.771  -1.220 0.224809    
# Site^6         42.747     15.395   2.777 0.006390 ** 
# Site^7        -47.984     14.843  -3.233 0.001590 ** 
# Site^8         11.979      6.979   1.716 0.088701 .  
# Graze:Site.L   17.846      4.843   3.685 0.000347 ***
# Graze:Site.Q    4.572      2.593   1.763 0.080422 .  
# Graze:Site.C  -17.301      7.580  -2.283 0.024243 *  
# Graze:Site^4    9.335      6.853   1.362 0.175731    
# Graze:Site^5    3.179      3.757   0.846 0.399181    
# Graze:Site^6  -26.408     11.688  -2.259 0.025694 *  
# Graze:Site^7   28.892      8.926   3.237 0.001570 ** 
# Graze:Site^8       NA         NA      NA       NA 

fit <- lm(X13MBC_G~Mow*Site, data=luicue)
summary(fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.801 -2.305  0.274  1.776 11.863 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 11.89876    1.23115   9.665   <2e-16 ***
# Mow         -1.31952    1.48884  -0.886   0.3772    
# Site.L      -3.06051    2.99125  -1.023   0.3083    
# Site.Q       5.07760    2.12278   2.392   0.0183 *  
# Site.C       0.05173    4.45740   0.012   0.9908    
# Site^4      -3.86605    2.96709  -1.303   0.1951    
# Site^5       2.94737    2.11268   1.395   0.1656    
# Site^6       5.19722    5.64737   0.920   0.3593    
# Site^7      -6.06746    5.35492  -1.133   0.2594    
# Site^8      -3.31698    1.76362  -1.881   0.0624 .  
# Mow:Site.L   1.39459    6.96139   0.200   0.8416    
# Mow:Site.Q  -3.18080    1.97664  -1.609   0.1102    
# Mow:Site.C   2.30850    5.32514   0.434   0.6654    
# Mow:Site^4   3.03818    5.02938   0.604   0.5469    
# Mow:Site^5   1.80552    6.21826   0.290   0.7720    
# Mow:Site^6        NA         NA      NA       NA    
# Mow:Site^7        NA         NA      NA       NA    
# Mow:Site^8        NA         NA      NA       NA 

fit <- lm(X13MBC_N~Mow*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.24100 -0.18047 -0.02444  0.12700  2.39485 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.68846    0.18504   3.721 0.000308 ***
#   Mow          0.06381    0.22671   0.281 0.778870    
# Site.L       0.68290    0.45852   1.489 0.139101    
# Site.Q      -0.41063    0.31852  -1.289 0.199898    
# Site.C      -0.45857    0.68269  -0.672 0.503111    
# Site^4      -0.12362    0.46307  -0.267 0.789980    
# Site^5      -0.76890    0.32155  -2.391 0.018401 *  
# Site^6      -1.00378    0.87170  -1.152 0.251882    
# Site^7       0.68579    0.81414   0.842 0.401328    
# Site^8       0.35512    0.26443   1.343 0.181904    
# Mow:Site.L  -0.59727    1.05638  -0.565 0.572899    
# Mow:Site.Q   0.10991    0.29549   0.372 0.710601    
# Mow:Site.C   0.38415    0.80999   0.474 0.636199    
# Mow:Site^4  -0.18409    0.77632  -0.237 0.812978    
# Mow:Site^5   0.53851    0.94200   0.572 0.568655    
# Mow:Site^6        NA         NA      NA       NA    
# Mow:Site^7        NA         NA      NA       NA    
# Mow:Site^8        NA         NA      NA       NA

fit <- lm(X13CO2_N~Mow*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.64600 -0.15100  0.00106  0.10933  0.65400 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.622778   0.086779   7.177 6.44e-11 ***
# Mow         -0.063486   0.104942  -0.605  0.54635    
# Site.L      -0.497859   0.210841  -2.361  0.01982 *  
# Site.Q      -0.001843   0.149626  -0.012  0.99019    
# Site.C       0.367222   0.314183   1.169  0.24480    
# Site^4      -0.241757   0.209137  -1.156  0.24999    
# Site^5      -0.290625   0.148914  -1.952  0.05331 .  
# Site^6       0.692081   0.398060   1.739  0.08466 .  
# Site^7      -0.643157   0.377446  -1.704  0.09097 .  
# Site^8      -0.199591   0.124310  -1.606  0.11099    
# Mow:Site.L   0.130240   0.490679   0.265  0.79113    
# Mow:Site.Q   0.251468   0.139325   1.805  0.07360 .  
# Mow:Site.C  -0.719103   0.375347  -1.916  0.05777 .  
# Mow:Site^4   0.939645   0.354500   2.651  0.00912 ** 
# Mow:Site^5  -0.449080   0.438299  -1.025  0.30761    
# Mow:Site^6         NA         NA      NA       NA    
# Mow:Site^7         NA         NA      NA       NA    
# Mow:Site^8         NA         NA      NA       NA

fit <- lm(X13CO2_G~Mow*Site, data=luicue)
summary(fit)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.5922 -0.5762 -0.0374  0.4728  3.5340 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.9530     0.4351  18.277  < 2e-16 ***
# Mow          -2.7257     0.5267  -5.175 9.35e-07 ***
# Site.L       -7.6378     1.0568  -7.227 5.12e-11 ***
# Site.Q       -2.8237     0.7489  -3.770 0.000255 ***
# Site.C        8.9105     1.5758   5.654 1.09e-07 ***
# Site^4       -1.2346     1.0479  -1.178 0.241054    
# Site^5       -1.0650     0.7465  -1.427 0.156284    
# Site^6       10.8010     1.9954   5.413 3.27e-07 ***
# Site^7      -12.2810     1.8924  -6.490 2.07e-09 ***
# Site^8        0.3951     0.6276   0.629 0.530262    
# Mow:Site.L   10.4172     2.4634   4.229 4.64e-05 ***
# Mow:Site.Q   -0.8902     0.6954  -1.280 0.202952    
# Mow:Site.C   -8.6234     1.8830  -4.580 1.15e-05 ***
# Mow:Site^4    8.6765     1.7756   4.886 3.24e-06 ***
# Mow:Site^5   -9.3113     2.2063  -4.220 4.79e-05 ***
# Mow:Site^6        NA         NA      NA       NA    
# Mow:Site^7        NA         NA      NA       NA    
# Mow:Site^8        NA         NA      NA       NA 

fit <- lm(X13CO2_G~Till*Site, data=luicue)
summary(fit)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.2047 -0.5900 -0.1340  0.4645  3.5553 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.88304    0.12285  47.887  < 2e-16 ***
# Till        -0.27434    0.07631  -3.595 0.000473 ***
# Site.L      -3.12266    0.38337  -8.145 4.27e-13 ***
# Site.Q      -0.33259    0.37542  -0.886 0.377452    
# Site.C       0.89081    0.36820   2.419 0.017062 *  
# Site^4       3.30591    0.38085   8.680 2.44e-14 ***
# Site^5       2.52626    0.36593   6.904 2.65e-10 ***
# Site^6       0.42365    0.37348   1.134 0.258933    
# Site^7      -3.57174    0.34476 -10.360  < 2e-16 ***
# Site^8      -1.66101    0.35484  -4.681 7.63e-06 ***
# Till:Site.L -0.10240    0.31604  -0.324 0.746498    
# Till:Site.Q  0.39372    0.27127   1.451 0.149299    
# Till:Site.C  0.03614    0.50397   0.072 0.942950    
# Till:Site^4 -0.26763    0.17251  -1.551 0.123470    
# Till:Site^5 -0.12932    0.42247  -0.306 0.760062    
# Till:Site^6       NA         NA      NA       NA    
# Till:Site^7       NA         NA      NA       NA    
# Till:Site^8       NA         NA      NA       NA

fit <- lm(X13CO2_N~Till*Site, data=luicue)
summary(fit)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.6460 -0.1270 -0.0160  0.1167  0.6573 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.741e-01  2.252e-02  25.493  < 2e-16 ***
# Till        -3.448e-02  1.403e-02  -2.457  0.01542 *  
# Site.L      -1.287e-01  7.046e-02  -1.827  0.07022 .  
# Site.Q      -3.166e-05  6.884e-02   0.000  0.99963    
# Site.C      -1.442e-01  6.755e-02  -2.135  0.03483 *  
# Site^4       2.094e-01  6.996e-02   2.994  0.00335 ** 
# Site^5      -6.652e-02  6.697e-02  -0.993  0.32257    
# Site^6      -9.685e-02  6.867e-02  -1.410  0.16100    
# Site^7      -1.340e-01  6.295e-02  -2.129  0.03533 *  
# Site^8      -2.787e-01  6.479e-02  -4.301 3.48e-05 ***
# Till:Site.L -1.489e-01  5.811e-02  -2.563  0.01162 *  
# Till:Site.Q  2.272e-01  4.988e-02   4.555 1.27e-05 ***
# Till:Site.C -3.511e-02  9.266e-02  -0.379  0.70542    
# Till:Site^4  3.787e-02  3.172e-02   1.194  0.23481    
# Till:Site^5 -6.062e-02  7.768e-02  -0.780  0.43670    
# Till:Site^6         NA         NA      NA       NA    
# Till:Site^7         NA         NA      NA       NA    
# Till:Site^8         NA         NA      NA       NA 

fit <- lm(X13MBC_N~Till*Site, data=luicue)
summary(fit)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.24100 -0.16766 -0.02444  0.11779  2.40400 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.7701587  0.0448043  17.189  < 2e-16 ***
# Till        -0.0230708  0.0290582  -0.794 0.428845    
# Site.L       0.6892455  0.1420822   4.851 3.86e-06 ***
# Site.Q      -0.3718018  0.1395616  -2.664 0.008819 ** 
# Site.C      -0.2860695  0.1356921  -2.108 0.037164 *  
# Site^4      -0.4533555  0.1385683  -3.272 0.001408 ** 
# Site^5      -1.0218979  0.1322144  -7.729 4.29e-12 ***
# Site^6      -0.8092100  0.1344955  -6.017 2.13e-08 ***
# Site^7       0.2206470  0.1240540   1.779 0.077919 .  
# Site^8       0.4347795  0.1276606   3.406 0.000907 ***
# Till:Site.L -0.0499950  0.1146164  -0.436 0.663507    
# Till:Site.Q  0.0006847  0.1017912   0.007 0.994645    
# Till:Site.C -0.0388617  0.1822947  -0.213 0.831560    
# Till:Site^4  0.1202888  0.0639359   1.881 0.062425 .  
# Till:Site^5  0.1538433  0.1525198   1.009 0.315229    
# Till:Site^6         NA         NA      NA       NA    
# Till:Site^7         NA         NA      NA       NA    
# Till:Site^8         NA         NA      NA       NA 

fit <- lm(X13MBC_G~Till*Site, data=luicue)
summary(fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.408 -2.248  0.079  1.586 11.863 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 12.19993    0.31656  38.539  < 2e-16 ***
# Till        -0.54464    0.19721  -2.762 0.006655 ** 
# Site.L      -2.12846    0.99037  -2.149 0.033630 *  
# Site.Q       4.77522    0.96764   4.935 2.61e-06 ***
# Site.C      -1.16403    0.94951  -1.226 0.222628    
# Site^4      -3.16387    0.98328  -3.218 0.001662 ** 
# Site^5       3.46514    0.94129   3.681 0.000349 ***
# Site^6       1.86197    0.96521   1.929 0.056083 .  
# Site^7      -5.34972    0.88480  -6.046 1.73e-08 ***
# Site^8      -3.27742    0.91062  -3.599 0.000465 ***
# Till:Site.L -0.18228    0.81677  -0.223 0.823784    
# Till:Site.Q -0.11390    0.70108  -0.162 0.871217    
# Till:Site.C  1.71015    1.30248   1.313 0.191691    
# Till:Site^4 -0.08464    0.44584  -0.190 0.849746    
# Till:Site^5 -0.56128    1.09183  -0.514 0.608143    
# Till:Site^6       NA         NA      NA       NA    
# Till:Site^7       NA         NA      NA       NA    
# Till:Site^8       NA         NA      NA       NA

fit3 <- aov(CUE_N~LUIwT+Site, data=luicue)
Anova(fit3, type="III")
summary.lm(fit3)

# removing non-sig variables - did not report these

# Anova Table (Type III tests)
# 
# Response: CUE_G
#             Sum Sq  Df   F value Pr(>F)    
# (Intercept) 20.1023   1 2890.6195 <2e-16 ***
# LUIwT        0.0002   1    0.0355 0.8508    
# Site         1.3724   8   24.6690 <2e-16 ***
# Residuals    0.8623 124

# Anova Table (Type III tests)
# 
# Response: CUE_N
#              Sum Sq  Df  F value Pr(>F)    
# (Intercept) 11.9478   1 533.9422 <2e-16 ***
# LUIwT        0.0043   1   0.1905 0.6633    
# Site         4.6802   8  26.1443 <2e-16 ***
# Residuals    2.7299 122

#posth=glht(fit3, linfct=mcp(Site="Tukey"))  ##gives the post-hoc Tukey analysis
#summary(posth) #lots of site diffs in CUE, but no LUI diffs, so this not neccessary

fit4 <- lm(CUE_G~LUIlevel, data=luicue)
summary(fit4)
#Residual standard error: 0.1305 on 131 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.001698,	Adjusted R-squared:  -0.01354 
# F-statistic: 0.1114 on 2 and 131 DF,  p-value: 0.8946

fit5 <- lm(CUE_N~LUIlevel, data=luicue)
summary(fit5)
#Residual standard error: 0.2396 on 129 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.00462,	Adjusted R-squared:  -0.01081 
# F-statistic: 0.2993 on 2 and 129 DF,  p-value: 0.7418

x <- as.numeric(luicue$CUE_G)
x2<- as.numeric(luicue$CUE_N)
x
range(x, na.rm=TRUE)
range(x2, na.rm=TRUE)
mean(x, na.rm=TRUE)
mean(x2, na.rm=TRUE)
t.test(x,x2)


fit4 <- aov(totC~LUIwT, data=luicue)
Anova(fit4, type="III")
summary.lm(fit4)

model.totc <- lm(totC ~ LUIwT, data = luicue)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.totc)
par(opar)

summary(luicue$totC)
luicue$totC <- log(luicue$totC)

fit6 <- lm(totC~LUIwT, data=luicue)
summary(fit6)
