setwd("/Users/katebuckeridge/OneDrive - University of Edinburgh/R/UGrass/cue")
setwd("C:/Users/kbuckeri/OneDrive - University of Edinburgh/R/UGrass/cue") ## at work


library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(lattice)
library(forcats)

##### Fig. 1 is conceptual figure ###

##### Fig.2a Exp design: the map ######

sites <- read.csv("UGrassMap.csv")   #my data for sampling sites, contains a column of "lat" and a column of "long" with GPS points in decimal degrees

cols2 <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6") #divergent 9 class

names <- c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")

UK <- map_data(map = "world", region = "UK") 
UGrass <- ggplot(data = UK, aes(x = long, y = lat)) + 
  geom_polygon(colour = "gray55", fill="white", aes(group=group)) +
  geom_point(data=sites, aes(LG,LT, colour=site), size = 3) +
  labs(x="Longitude",y="Latitude")+
  scale_colour_manual(values=cols2,
                      labels=names,
                      name="Site") +
  coord_map() + #controls distortion
  theme_bw() 
#cleanup

png(file="UGrassmap.png", units="in", width=4, height=4, res=300) 
UGrass
dev.off()

### Fig. 2b. the map side plot

lui <- read.csv("LUIlevel.csv")
lui$Site <- factor(lui$Site, levels=as.character(lui$Site))

library(ggalt)

cols2 <- c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6")

p <- ggplot(lui, aes(y=Site)) + 
  geom_dumbbell(aes(x=Low, xend=High),
                size=3, 
                color=rev(cols2), 
                colour_x = "#e3e2e1", 
                colour_xend = "#e3e2e1") +
  scale_x_continuous(limits = c(0, 12)) +
  coord_cartesian(xlim = c(-0.5, 12.5))  +
  #first removes all data points outside the given range and the second only adjusts the visible area
  geom_dumbbell(aes(x=Medium, xend=Medium),
                size=3, 
                color=cols2, 
                colour_x = "#e3e2e1", 
                colour_xend = "#e3e2e1") +
  geom_text(data=lui, 
            aes(x=Low, y=Site, label="Low"),
            color="gray55", 
            size=2.75, 
            vjust=2.5,
            hjust=1.0,
            family="Calibri") +
  geom_text(data=lui, 
            aes(x=Medium, y=Site, label="Mid"),
            color="gray55", 
            size=2.75, 
            vjust=2.5, 
            family="Calibri") +
  geom_text(data=lui, 
            aes(x=High, y=Site, label="High"),
            color="gray55", 
            size=2.75, 
            vjust=2.5,
            hjust=0.0,
            family="Calibri") +
  labs(x="Land use intensity index score", y="Site")+
  theme_bw()

png(file="UGrassmapLUI.png", units="in", width=5, height=4, res=300) 
p
dev.off()

##### Fig. 3: LUI not that important
###3a CUE-glucose ~ LUI 3b CUE-g ~ %C 3c CUE-necro ~ LUI 3d CUE-n ~ %C 

luicue <- read.csv("metaotu2018FILL.csv", header=TRUE, sep=",") 

fit1 <- lm(CUE_G~LUIwT, data=luicue)
summary(fit1)
# Residual standard error: 0.1286 on 128 degrees of freedom
# Multiple R-squared:  0.00221,	Adjusted R-squared:  -0.005585 
# F-statistic: 0.2836 on 1 and 128 DF,  p-value: 0.5953

luiG <- ggplot(luicue, 
               aes(y=CUE_G, x = LUIwT))+ 
  geom_point(shape = 19,
             size=3, 
             alpha = 0.7,
             colour="#e31a1c", 
             position=position_jitter(width=0.2))+
  scale_y_continuous(limits = c(0, 1)) +
  labs(y="CUE glucose (%)", x="Land use intensity index score") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
luiG

fit2 <- lm(CUE_N~LUIwT, data=luicue)
summary(fit2) 
# Residual standard error: 0.2405 on 128 degrees of freedom
# Multiple R-squared:  0.01099,	Adjusted R-squared:  0.003266 
# F-statistic: 1.423 on 1 and 128 DF,  p-value: 0.2352

luiN <- ggplot(luicue, 
               aes(y=CUE_N, x = LUIwT))+ 
  geom_point(shape = 19,
             size=3,
             alpha = 0.7,
             colour="#1f78b4", 
             position=position_jitter(width=0.2)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y="CUE necromass (%)", x="Land use intensity index score") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
luiN

fit3 <- lm(CUE_G~totC, data=luicue)
summary(fit3) 

# Residual standard error: 0.1145 on 128 degrees of freedom
# Multiple R-squared:  0.2098,	Adjusted R-squared:  0.2037 
# F-statistic: 33.99 on 1 and 128 DF,  p-value: 4.252e-08

BigC_G <- ggplot(luicue, 
               aes(y=CUE_G, x = totC))+ 
  geom_smooth(method="lm",
              formula=y~x, 
              col="black", 
              fill="gray70", 
              size=0.5) +
  geom_point(shape = 19,
             size=3, 
             alpha = 0.7,
             colour="#e31a1c", 
             position=position_jitter(width=0.2))+
  annotate("text",
           label = expression(Adj.~R^{"2"}~{"="}~{"0.20"}),
           x = 12,
           y = 0.1,
          size = 4,
          colour = "gray29") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = c(4,8,12)) +
  labs(y="CUE glucose (%)", x="Total Soil C (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
BigC_G

fit4 <- lm(CUE_N~totC, data=luicue)
summary(fit4) 
# Residual standard error: 0.2418 on 128 degrees of freedom
# Multiple R-squared:  0.0002772,	Adjusted R-squared:  -0.007533 
# F-statistic: 0.0355 on 1 and 128 DF,  p-value: 0.8509

BigC_N <- ggplot(luicue, 
               aes(y=CUE_N, x = totC))+ 
  geom_point(shape = 19,
             size=3,
             alpha = 0.7,
             colour="#1f78b4", 
             position=position_jitter(width=0.2)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = c(4,8,12)) +
  labs(y="CUE necromass (%)", x="Total Soil C (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
BigC_N

Fig3 <- ggarrange(luiG, BigC_G, luiN,  BigC_N,
                   ncol = 2, nrow = 2)
png(file="luiTotCcueplots.png", units="in", width=8, height=8, res=300)
Fig3
dev.off()


#### Fig 4. Why not? B/c LUI does not alter growth rate and CUE generally increases with GR

### venn diagrams from varpart - see cueLinReg.R
##growth rate on glucose and necro
##make scatterplot style

growth <- read.csv("metaotu2018.csv", header=TRUE, sep=",") 

log.fit0 <- lm(CUE_N~log(growthN), data=growth)
summary(log.fit0)
# Call:
# lm(formula = CUE_N ~ log(growthN), data = growth)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.43251 -0.10753 -0.00109  0.10762  0.51262 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.14497    0.05818  -2.492    0.014 *  
#   log(growthN)  0.21349    0.01780  11.997   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1592 on 126 degrees of freedom
# (7 observations deleted due to missingness)
# Multiple R-squared:  0.5332,	Adjusted R-squared:  0.5295 
# F-statistic: 143.9 on 1 and 126 DF,  p-value: < 2.2e-16

log.fit1 <- lm(MAP~log(growthN), data=growth)
summary(log.fit1)

# Call:
#   lm(formula = MAP ~ log(growthN), data = growth)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1203.55  -206.72   -49.74   214.41   674.93 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2119.56     103.48   20.48   <2e-16 ***
#   log(growthN)  -324.02      31.77  -10.20   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 306.2 on 127 degrees of freedom
# (6 observations deleted due to missingness)
# Multiple R-squared:  0.4502,	Adjusted R-squared:  0.4459 
# F-statistic:   104 on 1 and 127 DF,  p-value: < 2.2e-16

gN <- ggplot(growth, aes(y=CUE_N, x=growthN)) + 
  geom_smooth(method="lm",
              formula=y~log(x), 
              col="black", 
              fill="gray70", 
              size=0.5) +
  geom_point(shape = 19,
             size=3,
             alpha = 0.7,
             aes(colour = MAP)) + 
  labs(x=expression(Necromass~C~uptake~rate~(ng~C~g^{"-1"}~h^{"-1"})), 
       y="Necromass CUE (%)") +
  scale_color_gradient(low="#a6cee3",
                       high = "#1f78b4",
                       na.value = "#bdbdbd",
                       guide = "colourbar",
                       name = "MAP (mm)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(legend.position = c(0.8,0.3),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=1)) 
gN 

log.fit <- lm(CUE_G~log(growthG), data=growth)
summary(log.fit)
# # Call:
# lm(formula = CUE_G ~ log(growthG), data = growth)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.275027 -0.078852 -0.005421  0.064193  0.245410 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.05366    0.14166  -0.379    0.705    
# log(growthG)  0.11793    0.02309   5.107 1.12e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1189 on 132 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.165,	Adjusted R-squared:  0.1586 
# F-statistic: 26.08 on 1 and 132 DF,  p-value: 1.121e-06

# these values are on the growth rate that uses average MB values - but CUE does not use these values so I have left old numbers and plots in ms and need to change back the data set.

log.fit2 <- lm(pH~log(growthG), data=growth)
summary(log.fit2)

# Call:
#   lm(formula = pH ~ log(growthG), data = growth)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.3481 -0.6060 -0.3025  0.3075  1.8106 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   11.5258     1.0471  11.008  < 2e-16 ***
#   log(growthG)  -0.8246     0.1706  -4.835 3.63e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8854 on 133 degrees of freedom
# Multiple R-squared:  0.1495,	Adjusted R-squared:  0.1431 
# F-statistic: 23.37 on 1 and 133 DF,  p-value: 3.627e-06

gG <- ggplot(growth, aes(y=CUE_G, x=growthG)) + 
  geom_smooth(method="lm",
              formula=y~log(x), 
              col="black", 
              fill="gray70", 
              size=0.5) +
  geom_point(shape = 19,
             size=3,
             alpha = 0.7,
             aes(colour = pH)) + 
  labs(x=expression(Glucose~C~uptake~rate~(ng~C~g^{"-1"}~h^{"-1"})), 
       y="Glucose CUE (%)") +
  scale_color_gradient(low= "#fff5f0", 
                       high = "#e31a1c", 
                       na.value = "#bdbdbd",
                       guide = "colourbar",
                       name = "pH") +
  scale_y_continuous(limits = c(0, 1)) +
   theme_bw() +
  theme(legend.position = c(0.8,0.3),
      legend.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=1)) 
gG 

Figx2 <- ggarrange(gG, gN,
                  ncol = 2, nrow = 1)
png(file="growthPlots.png", units="in", width=8, height=4, res=300)
Figx2
dev.off()

png(file="NecroGrowth.png", units="in", width=4.5, height=4, res=300)
gN
dev.off()



#### Fig 5. What does control growth rate? Differs by sub

### diff microbes using diff subs - PLFA-SIP
#### Fig 5a, b, c, d. PLFA-SIP

library(vegan)
library(MASS)
library(ggrepel)

ord1 <- read.csv("plfa_13cORDplus.csv", header=TRUE, sep=",", row.names = 1)
meta1 <- read.csv("plfa_13cORDmeta.csv", header=TRUE, sep=",")

plfa.dis <- vegdist(ord1, binary=FALSE)#Bray-Curtis matrix, use this, Binary performs p/a standardization first

plfa <- cmdscale(plfa.dis)

PLFA <- data.frame(PCOA1 = plfa[,1], PCOA2 = plfa[,2])
p1 <- ggplot(data = PLFA, aes(PCOA1, PCOA2)) + 
  geom_point(shape = 21,
             size=3, 
             colour="black", 
             aes(fill = meta1$Sub)) +
  scale_fill_manual(name = "Substrate",
                    labels=c("Glucose","Necromass"),
                    values = c("white","black")) +
  theme_bw() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black")) 
p1

p3 <- ggplot(data = PLFA, aes(PCOA1, PCOA2)) + 
  geom_point(shape = 21,
             size=3, 
             colour="black", 
             aes(fill = meta1$Farm)) +
  scale_fill_grey(start=0, end=1,
                  name = "Farm",
                  labels=c("Knapwell","Shotts","Crainlarich","Preston","Salisbury")) +
  theme_bw() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black")) 
p3

Figx3 <- ggarrange(p1, p3,
                   ncol = 2, nrow = 1, align = "h")
png(file="SIPplots.png", units="in", width=11, height=4, res=300)
Figx3
dev.off()

####### c and d with lipid overlays

metaG <- meta1[c(1:9,19:27,37:45,55:62,69:77),]
metaN <- meta1[c(10:18,28:36,46:54,63:68,78:84),]

ordG <- ord1[c(1:9,19:27,37:45,55:62,69:77),]
ordN <- ord1[c(10:18,28:36,46:54,63:68,78:84),]

plfaG.mds <- metaMDS(ordG, trace = FALSE)
plfaG.mds

plfaN.mds <- metaMDS(ordN, trace = FALSE)
plfaN.mds

### necromass use alone
NMDSn = data.frame(MDS1 = plfaN.mds$points[,1], MDS2 = plfaN.mds$points[,2])

vecN.sp<-envfit(plfaN.mds$points, ordN, perm=1000)
vecN.sp.df<-as.data.frame(vecN.sp$vectors$arrows*sqrt(vecN.sp$vectors$r))
vecN.sp.df$species<-rownames(vecN.sp.df)

p.mdsN <- ggplot(data = NMDSn, aes(MDS1, MDS2)) + 
  geom_point(shape = 19,
             size=3,
             alpha = 0.7,
             aes(colour=metaN$MAP)) +
  scale_colour_gradient(low="#a6cee3",
                        high = "#1f78b4",
                        na.value = "#bdbdbd",
                        guide = "colourbar",
                        name = "MAP (mm)") +
  geom_segment(data=vecN.sp.df,
               aes(x=0,
                   xend=MDS1,
                   y=0,
                   yend=MDS2), 
               arrow=arrow(length=unit(0.25, "cm")),
               colour="grey") +
  geom_text_repel(data=vecN.sp.df,
                  aes(x=MDS1,
                      y=MDS2,
                      label=species),
                  size=3) +
  annotate("text",
           label = expression(Stress~{"="}~{"0.12"}),
           x = 0.75,
           y = -0.9,
           size = 3,
           colour = "gray29") +
coord_fixed() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black")) 

p.mdsN

png(file="plfa13cNecro.png", units="in", width=5, height=4, res=300)
p.mdsN

dev.off()

### glucose use alone
NMDSg = data.frame(MDS1 = plfaG.mds$points[,1], MDS2 = plfaG.mds$points[,2])

vecG.sp<-envfit(plfaG.mds$points, ordG, perm=1000)
vecG.sp.df<-as.data.frame(vecG.sp$vectors$arrows*sqrt(vecG.sp$vectors$r))
vecG.sp.df$species<-rownames(vecG.sp.df)

p.mdsG <- ggplot(data = NMDSg, aes(MDS1, MDS2)) + 
  geom_point(shape = 19,
             size=3, 
             alpha = 0.7,
             aes(colour=metaG$pH)) +
  scale_colour_gradient(low= "#fff5f0", 
                        high = "#e31a1c",
                        na.value = "#bdbdbd",
                        guide = "colourbar",
                        name = "pH") +
  geom_segment(data=vecG.sp.df,
               aes(x=0,
                   xend=MDS1,
                   y=0,
                   yend=MDS2), 
               arrow=arrow(length=unit(0.25, "cm")),
               colour="grey") +
  geom_text_repel(data=vecG.sp.df,
                  aes(x=MDS1,
                      y=MDS2,
                      label=species),
                  size=3) +
  annotate("text",
           label = expression(Stress~{"="}~{"0.04"}),
           x = -0.4,
           y = -0.7,
           size = 3,
           colour = "gray29") +
  coord_fixed() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black")) 
p.mdsG

Figx2 <- ggarrange(p.mdsG, p.mdsN,
                   ncol = 2, nrow = 1, align = "h")
png(file="SIPPlots.png", units="in", width=11, height=4, res=300)
Figx2
dev.off()

Figx3 <- ggarrange(p1, p3,
                   ncol = 2, nrow = 1, align = "h")
png(file="SIPplots.png", units="in", width=11, height=4, res=300)
Figx3
dev.off()



######## Supp F1 Corr plot  ########
#### supp fig corr plot (supp f1)

cue <- read.csv("metaotu2018FILL.csv", header=T, sep=",")


cue2 <- cue[,c(4:31)] #factors removed
summary(cue2)

cue3 <- as.data.frame(scale(cue2))
colMeans(cue3)

library(corrplot)
M <- cor(cue3)
corrplot(M, method="circle", order="FPC") # same result when scaled


cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(cue2, 0.95)
res2 <- cor.mtest(cue2, 0.99)


CP <- corrplot(M, method="circle", order="hclust", p.mat = res1[[1]], sig.level=0.05, insig="blank")

png(file="FigS1.png", units="in", width=6, height=4, res=300)
CP
dev.off()

########## Supp F2. Microbial physiological response to glucose and microbial necromass as substrate: biomass-specific respiration 


# log.fit0 <- lm(CUE_N~log(growthN), data=growth)
# summary(log.fit0)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=F,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=F) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

growth <- read.csv("metaotu2018.csv", header=TRUE, sep=",") 

gqresp <- summarySE(growth, measurevar="qCO2G", groupvars="Site", na.rm=T)

GqCO2 <- ggplot(growth, 
                aes(y=qCO2G, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=gqresp, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=gqresp, aes(ymin=qCO2G-se, ymax=qCO2G+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  # scale_y_continuous(limits = c(0, 100)) +
  labs(y=expression(Glucose~qCO[2]~(mg~C~gMBC^{"-1"}~h^{"-1"})), x="") +
 scale_y_log10() +
#  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  expand_limits(y=0.1) +
  theme_bw() +
  theme(axis.text.x = element_blank(), #text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
GqCO2 

nqresp <- summarySE(growth, measurevar="qCO2N", groupvars="Site", na.rm=T)
growth$Site <- factor(growth$Site, levels = growth$Site)
NqCO2 <- ggplot(growth, 
                aes(y=qCO2N, x = Site)) + 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=nqresp, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=nqresp, aes(ymin=qCO2N-se, ymax=qCO2N+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  #scale_y_continuous(limits = c(0, 20)) +
  labs(y=expression(Necromass~qCO[2]~(mg~C~gMBC^{"-1"}~h^{"-1"})), x="") +
  scale_y_log10() +
 # scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  #expand_limits(y=0.01) +
  theme_bw() +
  theme(axis.text.x = element_blank(), #text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
NqCO2 

tog <- summarySE(growth, measurevar="TOG", groupvars="Site", na.rm=T)
TOG <- ggplot(growth, 
                aes(y=TOG, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=tog, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=tog, aes(ymin=TOG-se, ymax=TOG+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(y=expression(Glucose~turnover~(g~C~gMBC^{"-1"}~y^{"-1"})), x="") +
 scale_y_log10() +
#  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), #text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
TOG

ton <- summarySE(growth, measurevar="TON", groupvars="Site", na.rm=T)
TON <- ggplot(growth, 
              aes(y=TON, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=ton, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=ton, aes(ymin=TON-se, ymax=TON+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(y=expression(Necromass~turnover~(g~C~gMBC^{"-1"}~y^{"-1"})), x="") +
  scale_y_log10() +
#  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), #text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
TON

GqB <- summarySE(growth, measurevar="qBiomassC", groupvars="Site", na.rm=T)
Gqbio <- ggplot(growth, 
              aes(y=qBiomassC, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=GqB, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=GqB, aes(ymin=qBiomassC-se, ymax=qBiomassC+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(y=expression(Glucose~qUptake~(g~C~gMBC^{"-1"}~d^{"-1"})), x="") +
 # scale_y_log10() +
  #  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), #text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
Gqbio

NqB <- summarySE(growth, measurevar="qBiomassN", groupvars="Site", na.rm=T)
Nqbio <- ggplot(growth, 
                aes(y=qBiomassN, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=NqB, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=NqB, aes(ymin=qBiomassN-se, ymax=qBiomassN+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(y=expression(Necromass~qUptake~(g~C~gMBC^{"-1"}~d^{"-1"})), x="") +
 # scale_y_log10() +
  #  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), #text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
Nqbio

gncue <- summarySE(growth, measurevar="CUEG.CUEN", groupvars="Site", na.rm=T)
GNcue <- ggplot(growth, 
              aes(y=CUEG.CUEN, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=gncue, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=gncue, aes(ymin=CUEG.CUEN-se, ymax=CUEG.CUEN+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(y=expression(Glucose~CUE:Necromass~CUE~ratio), x="") +
  #scale_y_log10() +
  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
GNcue

gngr <- summarySE(growth, measurevar="CNGrowthR", groupvars="Site", na.rm=T)
GNgr <- ggplot(growth, 
                aes(y=CNGrowthR, x = Site))+ 
  geom_point(shape = 19,
             size=3,
             position=position_jitter(width=0.2),
             aes(colour = Site, alpha = 0.2)) + 
  geom_bar(data=gngr, 
           stat="identity", 
           aes(fill = Site),
           alpha =0.3) +
  geom_errorbar(data=gngr, aes(ymin=CNGrowthR-se, ymax=CNGrowthR+se),
                size=.3,                     # thinner lines
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(y=expression(Glucose:Necromass~growth~rate~ratio), x="") +
  scale_y_log10() +
  scale_x_discrete(labels=c("Crainlarich","Shotts","Chapel-le-Dale","Preston","Wymondham","Aberystwyth","Knapwell","Salisbury","Okehampton")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) 
GNgr

Figxs <- ggarrange(GqCO2, NqCO2, TOG, TON, GNcue,GNgr,
                   ncol = 2, nrow = 3, align = "h")
png(file="suppPlotslog.png", units="in", width=8, height=12, res=300)
Figxs
dev.off()

