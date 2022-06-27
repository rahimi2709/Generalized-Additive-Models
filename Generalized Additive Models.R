# Histogram plot of Risk of TB = Number of TB cases/population of each region
par(mfrow=c(1,1))
hist(TBdata$TB/TBdata$Population,
     xlab= "Risk of TB", ylab="Density", main="Risk of TB Distribution", prob=TRUE,
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

# Plot for Data Exploration
library("GGally")
df_new <- cbind(TBdata, TBdata$TB/TBdata$Population)
ggpairs(columns=c("Indigenous","Illiteracy","Urbanisation","Density",
"Poverty","Poor_Sanitation","Unemployment","Timeliness"
,"TBdata$TB/TBdata$Population","Year","lon","lat"),data=df_new
,title="Exploration Data")


#Fit the first model
library(mgcv)
GAM1 <- gam(TB/Population ~ s(Indigenous,k=20)+s(Illiteracy,k=20)
            +s(Urbanisation,k=20)+s(Density,k=20)+s(Poverty,k=20)
            +s(Poor_Sanitation,k=20)+s(Unemployment,k=20)+s(Timeliness,k=20)+s(Population,k=20)
            ,select=TRUE,data = TBdata)
par(mfrow=c(2,2))
gam.check(GAM1)
summary(GAM1)
plot(GAM1,pages=1,cex.axis=1.5,cex.lab=1.5)

AIC(GAM1)

#Adding other covariates and different interactions
library(mgcv)
GAM2 <-  bam(TB/Population ~ 
              # s(Indigenous)
              # +s(Illiteracy)
              # +s(Urbanisation)
              # +s(Density)
              # +s(Poverty)
              # +s(Poor_Sanitation)
            +s(Unemployment)
              # +s(Timeliness)
            +s(Population,k=20)

            +ti(Indigenous,lon,lat,d=c(1,2),bs=c('cs','ds'))
            +ti(Illiteracy,lon,lat,d=c(1,2),bs=c('cs','ds'))
            +ti(Urbanisation,lon,lat,d=c(1,2),bs=c('cs','ds'))
            +ti(Density,lon,lat,d=c(1,2),bs=c('cs','ds'))
            +ti(Poverty,lon,lat,d=c(1,2),bs=c('cs','ds'))
              # +ti(Poor_Sanitation,lon,lat,d=c(1,2),bs=c('cs','ds'))
            +ti(Unemployment,lon,lat,d=c(1,2),bs=c('cs','ds'))################################################
            +ti(Timeliness,lon,lat,d=c(1,2),bs=c('cs','ds'))####################################################
            +ti(Population,lon,lat,d=c(1,2),bs=c('cs','ds'))####################################################

              # +ti(Indigenous,Year,bs="fs",k=3)
              # +ti(Illiteracy,Year,bs="fs",k=3)
              # +ti(Urbanisation,Year,bs="fs",k=3)
              # +ti(Density,Year,bs="fs",k=3)
              # +ti(Poverty,Year,bs="fs",k=3)
              # +ti(Poor_Sanitation,Year,bs="fs",k=3)
              # +ti(Unemployment,Year,bs="fs",k=3)
              # +ti(Timeliness,Year,bs="fs",k=3)
              # +ti(Population,Year,bs="fs",k=3)

            +s(lon,lat,k=150,bs='sos') # te for both effects of spatial

            
              # +ti(lon,lat,Year,d=c(2,1),bs=c('ds','fs'),k=c(100,3))

            +te(Indigenous,Illiteracy)
            +ti(Indigenous,Urbanisation)
              # +ti(Indigenous,Density)
              # +ti(Indigenous,Poverty)
              # +ti(Indigenous,Poor_Sanitation)
            +ti(Indigenous,Unemployment)
              # +ti(Indigenous,Timeliness)
            +ti(Illiteracy,Urbanisation)
              # +ti(Illiteracy,Density)
              # +ti(Illiteracy,Poverty)
              # +ti(Illiteracy,Poor_Sanitation)
              # +ti(Illiteracy,Unemployment)
              # +ti(Illiteracy,Timeliness)
              # +ti(Urbanisation,Density)
            +te(Urbanisation,Poverty)
              # +ti(Urbanisation,Poor_Sanitation)
              # +ti(Urbanisation,Unemployment)
              # +ti(Urbanisation,Timeliness)
              # +ti(Density,Poverty)
            +te(Density,Poor_Sanitation)
              # +ti(Density,Unemployment)
            +ti(Density,Timeliness)
              # +ti(Poverty,Poor_Sanitation)
            +ti(Poverty,Unemployment)
              # +ti(Poverty,Timeliness)
              # +ti(Poor_Sanitation,Unemployment)
              # +ti(Poor_Sanitation,Timeliness)
            +ti(Unemployment,Timeliness)
            
              # +ti(Population,Indigenous)
            +ti(Population,Illiteracy)
              # +ti(Population,Urbanisation)
              # +ti(Population,Density)
              # +ti(Population,Poverty)
              # +ti(Population,Poor_Sanitation)
              # +ti(Population,Unemployment)
              # +ti(Population,Timeliness)

            ,data = TBdata,method="fREML",family=gaussian
           ,nthreads=16,discrete=TRUE
            )
  
par(mfrow=c(2,2))
gam.check(GAM2)


