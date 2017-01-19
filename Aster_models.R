#These code performs aster model analysis to examine the response of fitness in 14 willow and poplar species to a hydrologic gradient. 
#The data was collected from a field experiment, in which cuttings of the species were transplanted into 40 common gardens (located in 10 sites) established across a natural hydrologic gradient in the field. 
#Two individuals per species were planted in each garden.One of the two individuals recieved an insect exclusion cage treatment; its conspecific counterpart recieved a sham cage treatment.
#The dataset included survival and basal area of the experimental plants measured at the end of the second growing season, as well as water table depth and soil nitrogen availability in the common garden plots.

#Load package
library(aster)

#Import data.
data<-read.csv("salicaceae.csv",row.names=1)
str(data)
#'data.frame':	989 obs. of  13 variables:
#$ site       : int  1 1 1 1 1 1 1 1 1 1 ...
#$ species    : int  1 1 2 3 13 3 3 5 8 14 ...
#$ indiv.     : int  1 2 56 6 23 5 58 62 13 79 ...
#$ wt.max     : num  -55.3 -55.3 -71.6 -55.3 -55.3 -55.3 -71.6 -71.6 -55.3 -71.6 ...
#$ wt.min     : num  -107 -107 -110 -107 -107 ...
#$ sur12f     : int  1 1 0 1 0 1 1 0 1 0 ...
#$ bs12f      : num  0.1146 0.0674 0 0.9898 0 ...
#$ s.wt.max   : num  -0.756 -0.756 -1.199 -0.756 -0.756 ...
#$ s.wt.min   : num  -0.821 -0.821 -0.917 -0.821 -0.821 ...
#$ s.wt.max.sq: num  0.572 0.572 1.438 0.572 0.572 ...
#$ s.wt.min.sq: num  0.674 0.674 0.841 0.674 0.674 ...
#$ s.lnN.res.M: num  0.42 0.42 0.42 0.42 0.42 ...
#$ s.lnN.res.m: num  0.2 0.2 0.2 0.2 0.2 ...

#Each row in the dataframe represents an experimental plant. The columns are:
#site= common garden sites; 
#species=species; 
#r.s.= cage treatment (0=real cage and 1= sham cage); 
#wt.max=growing season maximum water table depth;
#wt.min=growing season minimum water table depth;
#sur12f=survival of the plant (0=dead and 1=alive);
#bs12f=basal area of the plant;
#s.wt.max=centered and scaled growing season maximum water table depth;
#s.wt.min=centered and scaled growing season manimum water table depth;
#s.wt.max.sq=squared s.wt.max;
#s.wt.min.sq=squared s.wt.min;
#s.lnN.res.M=centered and scaled residual of soil N availability after regressed against growing season maximum water table depth;
#s.lnN.res.m=centered and scaled residual of soil N availability after regressed against growing season minimum water table depth.


data$site<-as.factor(data$site)
data$species<-as.factor(data$species)
data$r.s.<-as.factor(data$r.s.)

#Reshape the data so that each row represents one fitness component (i.e. survival or basal area) of an experimental plant (i.e. two rows per plant).
vars <- c("sur12f", "bs12f")
redata <- reshape(data, varying = list(vars), direction = "long",
                  timevar = "varb", times = as.factor(vars), v.names = "resp")
str(redata)
#'data.frame':	1978 obs. of  14 variables:
#$ site       : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ species    : Factor w/ 14 levels "1","2","3","4",..: 1 1 2 3 13 3 3 5 8 14 ...
#$ r.s.       : Factor w/ 2 levels "0","1": 1 2 2 2 1 1 2 2 1 1 ...
#$ wt.max     : num  -55.3 -55.3 -71.6 -55.3 -55.3 -55.3 -71.6 -71.6 -55.3 -71.6 ...
#$ wt.min     : num  -107 -107 -110 -107 -107 ...
#$ s.wt.max   : num  -0.756 -0.756 -1.199 -0.756 -0.756 ...
#$ s.wt.min   : num  -0.821 -0.821 -0.917 -0.821 -0.821 ...
#$ s.wt.max.sq: num  0.572 0.572 1.438 0.572 0.572 ...
#$ s.wt.min.sq: num  0.674 0.674 0.841 0.674 0.674 ...
#$ s.lnN.res.M: num  0.42 0.42 0.42 0.42 0.42 ...
#$ s.lnN.res.m: num  0.2 0.2 0.2 0.2 0.2 ...
#$ varb       : Factor w/ 2 levels "bs12f","sur12f": 2 2 2 2 2 2 2 2 2 2 ...
#$ resp       : num  1 1 0 1 0 1 1 0 1 0 ...
#$ id         : int  1 2 3 4 5 6 7 8 9 10 ...
#- attr(*, "reshapeLong")=List of 4
#..$ varying:List of 1
#.. ..$ : chr  "sur12f" "bs12f"
#..$ v.names: chr "resp"
#..$ idvar  : chr "id"
#..$ timevar: chr "varb"

#Note that redata has twice as many rows as data. It does not have columns sur12f and bs12f but has three new columns:
#varb=a categorical variable that indicates whether the row is survival or basal area (rows 1-989 are survival data and rows 990-1978 are basal area data);
#response=survival or basal area;
#id=an id number of the experimental plant, ranges from 1 to 989.

#Add root
redata <- data.frame(redata, root = 1)

#Add numerical indicators of the fitness components
redata$fit <- as.integer(as.character(redata$varb) == "bs12f")
redata$sur <- as.integer(as.character(redata$varb) == "sur12f")

#Calculate standard deviation of basal area, excluding the zeros (i.e. the plants that were dead).
nzero.bs12f<-c()
for (i in 1:989){
  if(data$bs12f[i]>0){
    nzero.bs12f<-c(nzero.bs12f,data$bs12f[i])
  }
}
sd<-sd(nzero.bs12f)

#Create a vector that indicates the precedent node of each fitness component.
pred <- c(0, 1)

#Create vectors that indicate the statistical distribution of each fitness component.
famlist<-list(fam.bernoulli(),fam.normal.location(sd))
fam <- c(1, 2)

#Fit four aster models that represent alternative hypotheses about the resposne of species fitness to the hydrologic gradient, 
#using growing season maximum water table depth in the common garden plots as a predictor of fitness.
max<-aster(resp ~ varb+species+s.wt.max + s.wt.max.sq+species:s.wt.max+species:s.wt.max.sq
           + site + s.lnN.res.M + species:r.s.,
           pred,fam,varb,id,root,famlist=famlist,data=redata)
max.red<-aster(resp ~ varb +species +s.wt.max + s.wt.max.sq
               + site + s.lnN.res.M + species:r.s.,
               pred,fam,varb,id,root,famlist=famlist,data=redata)
max.red2<-aster(resp ~ varb+species +s.wt.max+species:s.wt.max
                + site + s.lnN.res.M + species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)
max.red3<-aster(resp ~ varb+species +s.wt.max
                + site + s.lnN.res.M + species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)


#Fit four aster models represent alternative hypotheses about the response of species fitness to the hydrologic gradient,
#using growing season minimum water table depth as a predictor of fitness.
min<-aster(resp ~ varb+species+s.wt.min + s.wt.min.sq+species:s.wt.min+species:s.wt.min.sq
           +site + s.lnN.res.m+ species:r.s.,
           pred,fam,varb,id,root,famlist=famlist,data=redata)
min.red<-aster(resp ~ varb + species+s.wt.min + s.wt.min.sq
               + site + s.lnN.res.m+ species:r.s.,
               pred,fam,varb,id,root,famlist=famlist,data=redata)
min.red2<-aster(resp ~ varb+species +s.wt.min+species:s.wt.min
                + site + s.lnN.res.m+ species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)
min.red3<-aster(resp ~ varb+species + s.wt.min
                + site +s.lnN.res.m+ species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)

#Compare models using Likelihood ratio tests
anova(max.red,max)
anova(max.red2,max)
anova(max.red3,max)
anova(min.red,min)
anova(min.red2,min)
anova(min.red3,min)

#The results of the likelihood ratio tests showed that the most complex models(max, min) fitted the data better than the other models. 

#Test the significance of each effect in the best-fitted models using likelihood ratio tests. 
#First create reduced models in which the effects to be tested are dropped from the model formulas. 
#If the effect is included in interactions (i.e. species, s.wt.max), then both the main effects and the interactions are dropped in the reduced models.
#Create reduced models for each effect in the best-fitted model using growing season maximum water table depth as a predictor of fitness:
max.sp<-aster(resp ~ varb+s.wt.max + s.wt.max.sq+
                + site + s.lnN.res.M,
              pred,fam,varb,id,root,famlist=famlist,data=redata)
max.wt<-aster(resp ~ varb+species
              + site + s.lnN.res.M + species:r.s.,
              pred,fam,varb,id,root,famlist=famlist,data=redata)
max.int<-aster(resp ~ varb+species+s.wt.max + s.wt.max.sq
               + site + s.lnN.res.M + species:r.s.,
               pred,fam,varb,id,root,famlist=famlist,data=redata)
max.site<-aster(resp ~ varb+species+s.wt.max + s.wt.max.sq+species:s.wt.max+species:s.wt.max.sq
                + s.lnN.res.M + species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)
max.soil<-aster(resp ~ varb+species+s.wt.max + s.wt.max.sq+species:s.wt.max+species:s.wt.max.sq
                + site + species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)
max.rs<-aster(resp ~ varb+species+s.wt.max + s.wt.max.sq+species:s.wt.max+species:s.wt.max.sq
              + site + s.lnN.res.M,
              pred,fam,varb,id,root,famlist=famlist,data=redata)

#Create reduced models for each effect in the best-fitted model using growing season minimum water table depth as a predictor of fitness:
min.sp<-aster(resp ~ varb+s.wt.min + s.wt.min.sq+
                + site + s.lnN.res.m,
              pred,fam,varb,id,root,famlist=famlist,data=redata)
min.wt<-aster(resp ~ varb+species
              + site + s.lnN.res.m + species:r.s.,
              pred,fam,varb,id,root,famlist=famlist,data=redata)
min.int<-aster(resp ~ varb+species+s.wt.min + s.wt.min.sq
               + site + s.lnN.res.m + species:r.s.,
               pred,fam,varb,id,root,famlist=famlist,data=redata)
min.site<-aster(resp ~ varb+species+s.wt.min + s.wt.min.sq+species:s.wt.min+species:s.wt.min.sq
                + s.lnN.res.m + species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)
min.soil<-aster(resp ~ varb+species+s.wt.min + s.wt.min.sq+species:s.wt.min+species:s.wt.min.sq
                + site + species:r.s.,
                pred,fam,varb,id,root,famlist=famlist,data=redata)
min.rs<-aster(resp ~ varb+species+s.wt.min + s.wt.min.sq+species:s.wt.min+species:s.wt.min.sq
              + site + s.lnN.res.m,
              pred,fam,varb,id,root,famlist=famlist,data=redata)

#Test the significance of each effect in the best-fit model using growng season maximum water table depth as a predictor of fitness.
anova(max.sp,max)
anova(max.wt,max)
anova(max.int,max)
anova(max.site,max)
anova(max.soil,max)
anova(max.rs,max)

#Test the significance of each effect in the best-fit model using growng season minimum water table depth as a predictor of fitness.
anova(min.sp,min)
anova(min.wt,min)
anova(min.int,min)
anova(min.site,min)
anova(min.soil,min)
anova(min.rs,min)

#Make prediction of species fitness based on the best-fit models.
#First create a dataframe for prediction. 
#The prediction dataframe has the same format as the original dataframe used for fitting aster models. 
#It contains dependent variables of "imaginery plants" whose fitness will be predicted based on the best-fitted models.
#Specificially, these "imaginery plants" are 20 individuals per species (280 individuals total), 
#planted across the full range of the water table depth gradient in even spacing,
#recieving a real cage treatment, 
#under a site effect equivalent to the effect of site 1 on plant fitness in the field experiment, 
#and in soils with a N availability equal to the means N avaialbility of all gardens.
#Put in another word, all the effects in the best models, except for species and water table depth, are equal among the imaginery plants.

#Create two vectors of water table depth (for growing season maximum and minimum water table depth, respectively) that cover the full range of the experimental water table depth gradients in even spacing.
n<-20
wtmax.m<-min(data$wt.max)
wtmax.M<-max(data$wt.max)
grid<-(wtmax.M-wtmax.m)/(n-1)
wt.max.p<-grid*c(0:(n-1))+wtmax.m

wtmin.m<-min(data$wt.min)
wtmin.M<-max(data$wt.min)
grid2<-(wtmin.M-wtmin.m)/(n-1)
wt.min.p<-grid2*c(0:(n-1))+wtmin.m

#Center and scale the water table depth vectors 
#The transformation uses the same mean and standard deviation of the water table depth columns in the original dataframe ("data") used for fitting the aster models.
mean.wtmax<-mean(data$wt.max)
sd.wtmax<-sd(data$wt.max)

mean.wtmin<-mean(data$wt.min)
sd.wtmin<-sd(data$wt.min)

s.wtmax.p<-(wt.max.p-mean.wtmax)/sd.wtmax
s.wtmin.p<-(wt.min.p-mean.wtmin)/sd.wtmin

#Create a prediction dataframe named "newdata".
newdata<-data.frame(
  species=as.factor(rep(c(1:14),each=n)),
  wt.max=wt.max.p,
  s.wt.max=s.wtmax.p,
  wt.min=wt.min.p,
  s.wt.min=s.wtmin.p,
  s.wt.max.sq=s.wtmax.p^2,
  s.wt.min.sq=s.wtmin.p^2,
  site=unique(data$site)[1],
  r.s.=unique(data$r.s.)[1],
  s.lnN.res.M=mean(data$s.lnN.res.M),
  s.lnN.res.m=mean(data$s.lnN.res.m),
  sur12f=1,
  bs12f=1,
  root=1)

#Reshape newdata.
renewdata <- reshape(newdata, varying = list(vars), direction = "long",
                     timevar = "varb", times = as.factor(vars), v.names = "resp")

renewdata$fit <- as.integer(as.character(renewdata$varb) == "bs12f")
renewdata$sur <- as.integer(as.character(renewdata$varb) == "sur12f")

#Make two sets of predictions based on the best-fit models using growing season maximum and minimum water table depth as predictors of fitness, respectively.
pred.max<-predict(max,newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE)
pred.min<-predict(min,newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE)

#Create a new dataframe, which includes species identity and water table depths of the imaginery plants and their predicted fitness.
plot<-data.frame(species=renewdata$species,wt.max=renewdata$wt.max,
                 wt.min=renewdata$wt.min,varb=renewdata$varb,pred.max=pred.max$fit,pred.min=pred.min$fit)
str(plot)
#'data.frame':	560 obs. of  6 variables:
#  $ species : Factor w/ 14 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ wt.max  : num  -120.1 -112.4 -104.8 -97.1 -89.5 ...
#$ wt.min  : num  -163 -153 -144 -135 -126 ...
#$ varb    : Factor w/ 2 levels "bs12f","sur12f": 2 2 2 2 2 2 2 2 2 2 ...
#$ pred.max: num  0.268 0.28 0.307 0.345 0.39 ...
#$ pred.min: num  0.536 0.539 0.54 0.54 0.538 ...
#The plot dataframe has 560 rows. 
#Rows 1-280 are predicted survial  and rows 281-560 are predicted basal area, as indicated by the values of the column varb.


#Calculate two metrics for each species based on the predictions, which are
#a) the water table depth optimum of the species, i.e., the water table depth at which the species had highest predicted fitness and 
#b) the width of the species' hydrologic niche, i.e., the range of water table depth gradient in which the species had predicted fitness greater than zero.

#Create an empty dataframe to store water table depth optima and widths of hydrologic niches of the species.
opt.ran<-data.frame(sp.num=c(1:14), optima.w=numeric(14),optima.d=numeric(14),
               range.w=numeric(14),range.d=numeric(14))

#Calculate water table dephth optima based on the best-fit aster model with maximum growing season water table depth as a predictor of fitness.
for (i in 1:14){
  x<-plot[280+(i-1)*20+1,5]
  y<-min(plot[,2])
  for (j in 2:20){
    if (plot[(280+(i-1)*20+j),5]>x){
      x<-plot[(280+(i-1)*20+j),5]
      y<-plot[(280+(i-1)*20+j),2]
    }
  }     
  opt.ran$optima.w[i]<-y
}

#Calculate water table dephth optima based on the best-fit aster model with miniimum growing season water table depth as a predictor of fitness.
for (i in 1:14){
  x<-plot[280+(i-1)*20+1,6]
  y<-min(plot[,3])
  for (j in 2:20){
    if (plot[(280+(i-1)*20+j),6]>x){
      x<-plot[(280+(i-1)*20+j),6]
      y<-plot[(280+(i-1)*20+j),3]
    }
  }     
  opt.ran$optima.d[i]<-y
}

#Calculate widths of hydrologic niches based on the best-fit aster model with maximum growing season water table depth as a predictor of fitness.
for(i in 1:14){
  range<-c()
  for (j in 1:20){
    if(plot[280+(i-1)*20+j,5]>0){
      range<-c(range,plot[280+(i-1)*20+j,2])
    }
  }
  opt.ran$range.w[i]<-max(range)-min(range)
}

#Calculate widths of hydrologic niches based on the best-fit aster model with minimum growing season water table depth as a predictor of fitness.
for(i in 1:14){
  range<-c()
  for (j in 1:20){
    if(plot[280+(i-1)*20+j,6]>0){
      range<-c(range,plot[280+(i-1)*20+j,3])
    }
  }
  opt.ran$range.d[i]<-max(range)-min(range)
}




