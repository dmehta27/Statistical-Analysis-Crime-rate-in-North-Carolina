#Read the file
crime1 <- read.csv(file.choose(), header=T) 

#Joining the county file
county<-read.csv("C:\\Users\\Hetvi Pasad\\Documents\\Homework\\Statistics\\ProjectData\\NC_Counties.csv",1 )
View(county)
county<-na.omit(county)
View(county)
crimenc<- merge(crime1,county,by.x="county", by.y="County.Code")
crimenc$X <- NULL
View(crimenc)
colnames(crimenc)[25] <- "CountyName"



qplot(y=crmrte, x=prbarr, data=crime_out, main="CrimeRate and Probability of Arrest",geom=c("point", "smooth"))
qplot(y=crmrte, x=prbpris, data=crime_out, main="CrimeRate and Probability of Prison",geom=c("point", "smooth"))
qplot(x=crmrte, y=pctymle, data=crime_out, main="CrimeRate and Percentage of male",geom=c("point", "smooth"))
qplot(y=crmrte, x=taxpc, data=crime_out, main="CrimeRate and Tax per Capita",geom=c("point", "smooth"))


#CrimeRate and PolicePerCapita

plot(density(crimenc$crmrte))
plot(density(crime_out$crmrte))
qplot(y=crmrte, x=polpc, data=crime_out, main="PolicePerCapita and CrimeRate",geom=c("point", "smooth"))

cor(crime_out$crmrte,crime_out$polpc)

crime_polpc<- t.test(crime_out$crmrte,crime_out$polpc, conf.level = 0.95)
crime_polpc
barplot(crime_byyear)
crime_byyear<- aggregate(crmrte ~ year, data=crimeout, FUN="mean")
crime_byyear$year <-  as.factor(crime_byyear$year)
barplot(crime_byyear$year,crime_byyear$crmrte)
polpc_byyear<- aggregate(polpc ~ year, data=crime_out, FUN="mean")
plot(crime_byyear$crmrte,polpc_byyear$polpc)
polpc_byyear$polpc
class(crime_byyear$year)

west <- crime_out[crime_out$region=="west",]
central <- crime_out[crime_out$region=="central",]
other <- crime_out[crime_out$region=="other",]
crmrteagwest <- aggregate(crmrte ~ year, data= west, FUN = "mean")
crmrteagcentral <- aggregate(crmrte ~ year, data= central, FUN = "mean")
crmrteagother <- aggregate(crmrte ~ year, data= other, FUN = "mean")

polpcagwest <- aggregate(polpc ~ year, data= west, FUN = "mean")
polpcagcentral <- aggregate(polpc ~ year, data= central, FUN = "mean")
polpcagother <- aggregate(polpc ~ year, data= other, FUN = "mean")

plot(crmrteagwest$crmrte, polpcagwest$polpc)
cor(crmrteagwest$crmrte, polpcagwest$polpc)

plot(crmrteagcentral$crmrte, polpcagcentral$polpc)
cor(crmrteagcentral$crmrte, polpcagcentral$polpc)

plot(crmrteagother$crmrte, polpcagother$polpc)
cor(crmrteagother$crmrte, polpcagother$polpc)


cor(crime_byyear$crmrte,polpc_byyear$polpc)


qplot(y=crmrte,x=year, data=crime_byyear, ylab="CrimeRate", xlab="PolicePerCapita", main="Relationship between CrimeRate and PolicePerCapita, by year", facets=~region,geom=c("point", "smooth"))


#CrimeRate and Density

qplot(y=crmrte, x=density, data=crime_out,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , geom=c("point", "smooth"))
qplot(y=crmrte, x=density, data=crime_out,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , facets=~year, geom=c("point", "smooth"))
qplot(y=crmrte, x=density, data=crime_out,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , facets=~smsa, geom=c("point", "smooth"))
qplot(y=crmrte, x=density, data=crime_out,xlab="Density",ylab="Crime Rate", main="Crime Rate and Density" , facets=~region, geom=c("point", "smooth"))


#NUll Hypothesis -> There is no relation between Crime Rate and Density
#Alternate Hypothesis -> There is a relation between Crime Rate and Density
mod_density<- t.test(crime_out$crmrte, crime_out$density, conf.level = 0.95)
mod_density

#Since the P-value is less than 0.05 we reject the null the null and accept the alternate hypothesis.

#Co-relation between Crime rate and density
cor(crime_out$crmrte,crime_out$density)


#CrimeRate and Wages
privwageavg<- (crime_out$wcon 
               + crime_out$wfir
               + crime_out$wtuc
               + crime_out$wtrd
               + crime_out$wser)/7

govwageavg<- (crime_out$wloc
              + crime_out$wsta
              + crime_out$wfed)/3

crime_rate_carolina["PrivAvgWage"] <- privwageavg
crime_rate_carolina["GovAvgWage"] <- govwageavg

ln <- length(crime_out$X1)
rw <- c(90.9/90.9, 90.9/96.5, 90.9/99.6, 90.9/103.9, 90.9/107.6, 90.9/109.6, 90.9/113.6)
real<- rep(rw, length.out = ln)


rprivwageavg <- crime_out$PrivAvgWage*real
crime_out["RealPrivAvgWage"] <- rprivwageavg

rgovwageavg <- crime_out$GovAvgWage*real
crime_out["RealGovAvgWage"] <- rgovwageavg


modsmden<- lm(crmrte ~ density+polpc, data=crime_out)
summary(modsmden)

