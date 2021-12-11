library(readxl)
data <- read_excel("IPM.xlsx", col_types = c("text", 
                                            "numeric", "numeric", "numeric"))
View(data)
summary(data)
#####SPLINE UNIVARIABEL############
##Scatterplot
attach(data)
plot( AHH,IPM,  main="Scatter Plot IPM dan AHH", xlab="AHH", ylab="IPM")
plot( KP,IPM,  main="Scatter Plot IPM dan KP", xlab="KP", ylab="IPM")

#AHH Regresi Polinomial 
ml <- lm(IPM ~ AHH,data = data)
m2 <- lm(IPM ~ poly(AHH,2) + AHH, data = data)
plot(IPM ~AHH, data)
lines(data$AHH, predict(m2), col ='red')
data$X_bar <- ifelse(data$AHH>70,1,0)
data$diff <- data$AHH-70
data$X <- data$diff*data$X_bar
reg <- lm(IPM ~ AHH + X , data = data)
plot(IPM~ AHH, data)
lines(data$AHH, predict(reg), col='blue')

#KP Regresi Polinomial 
ml <- lm(IPM ~ KP,data = data)
m2 <- lm(IPM ~ poly(KP,2) + KP, data = data)
plot(IPM ~KP, data)
lines(data$KP, predict(m2), col ='red')
data$X_bar <- ifelse(data$KP>2000,1,0)
data$diff <- data$KP-2000
data$X <- data$diff*data$X_bar
reg <- lm(IPM ~ KP + X , data = data)
plot(IPM~ KP, data)
lines(data$KP, predict(reg), col='blue')

##Nilai GCV
attach(data)

library(mgcv)
AHH2=AHH^2
AHH3=AHH^3

X1<-ifelse(AHH>71,1,0)
X2<-ifelse(AHH>71.5,1,0)
X3<-ifelse(AHH>71.8,1,0)

##Multivariabel 1 titik knot
##linier spline

##Angka Harapan Hidup
knot71<-(AHH-71)*X1
knot71.5<-(AHH-71.5)*X2
knot71.8<-(AHH-71.8)*X3
#71
working1<-data.frame(IPM, AHH,knot71)
GCV1<-gam(IPM~AHH+knot71)
GCV1
Estimasi<-lm(IPM~AHH+knot71, data=working1)
summary(Estimasi)

#71.5
working2<-data.frame(IPM,AHH,knot71.5)
GCV2<-gam(IPM~AHH+knot71.5, data=working2)
GCV2
Estimasi2<-lm(IPM~AHH+knot71.5, data=working2)
summary(Estimasi2)

#71.8
working3<-data.frame(IPM,AHH,knot71.8)
GCV3<-gam(IPM~AHH+knot71.8, data=working3)
GCV3
Estimasi3<-lm(IPM~AHH+knot71.8, data=working3)
summary(Estimasi3)

##Kepadatan Penduduk
attach(data)
library(mgcv)
KP2=KP^2
KP3=KP^3

X1<-ifelse(KP>2500,1,0)
X2<-ifelse(KP>3000,1,0)
X3<-ifelse(KP>3500,1,0)

knot25<-(KP-2500)*X1
knot30<-(KP-3000)*X2
knot35<-(KP-3500)*X3

#2500
working1<-data.frame(IPM,KP,knot25)
GCV1<-gam(IPM~KP+knot25)
GCV1
Estimasi1<-lm(IPM~KP+knot25, data=working1)
summary(Estimasi1)

#3000
working2<-data.frame(IPM,KP,knot30)
GCV2<-gam(IPM~KP+knot30, data=working2)
GCV2
Estimasi2<-lm(IPM~KP+knot30, data=working2)
summary(Estimasi2)

#3500
working3<-data.frame(IPM,KP,knot35)
GCV3<-gam(IPM~KP+knot35, data=working3)
GCV3
Estimasi3<-lm(IPM~KP+knot35, data=working3)
summary(Estimasi3)

##Nilai GCV (71, 2500)
working1<-data.frame(IPM,AHH,knot71,KP,knot25)
GCV1<-gam(IPM~AHH+knot71+KP+knot25)
GCV1
Estimasi1<-lm(IPM~AHH+knot71+KP+knot25, data=working1)
summary(Estimasi1)

##Nilai GCV (71.5, 3000)
working2<-data.frame(IPM,AHH,knot71.5,KP,knot30)
GCV2<-gam(IPM~AHH+knot71.5+KP+knot30)
GCV2
Estimasi2<-lm(IPM~AHH+knot71.5+KP+knot30, data=working2)
summary(Estimasi2)

##Nilai GCV (71.8, 3500)
working3<-data.frame(IPM,AHH,knot71.8,KP,knot35)
GCV3<-gam(IPM~AHH+knot71.8+KP+knot35)
GCV3
Estimasi3<-lm(IPM~AHH+knot71.8+KP+knot35, data=working3)
summary(Estimasi3)

#Kesimpulan: nilai GCV paling rendah adalah: 71 dan 2500

#model Multivariat
multivariat<-lm(IPM~AHH+knot71+KP+knot25)
multivariat
summary(multivariat)
##GCV 
#Ypredict
Ypredict<-predict(multivariat)
#nilai MSE
a<-(IPM-Ypredict)^2
MSE<-1/38*(sum(a))
#membuat matriks X
Xmat<-cbind(1,AHH+knot71+KP+knot25)
#mencari nilai trace(I-Ak)
m1<-t(Xmat)
m2<-as.matrix(Xmat)
m3<-solve(m1%*%m2)
Ak<-m2%*%m3%*%m1
I<-diag(1,nrow=38,ncol=38)
m4<-diag(I-Ak)
trA=sum(m4)
penyebut<-(trA/38)^2
#Nilai GCV
GCV<-MSE/penyebut
GCV

##Pengujian asumsi 
#asumsi normal
library(nortest)
lillie.test(multivariat$residuals)
#Uji Identik
library(lmtest)
bptest(multivariat, data=data)
#Pemeriksaan asumsi residual independen
library(car)
durbinWatsonTest(multivariat)

##Multivariabel 2  titik knot

##Angka Harapan Hidup
#71, 71.5
working1<-data.frame(IPM,AHH,knot71,knot71.5)
GCV1<-gam(IPM~AHH+knot71+knot71.5, data=working1)
GCV1
Estimasi1<-lm(IPM~AHH+knot71+knot71.5, data=working1)
summary(Estimasi1)

#71.5,71.8
working2<-data.frame(IPM,AHH,knot71,knot71.8)
GCV2<-gam(IPM~AHH+knot71+knot71.8, data=working2)
GCV2
Estimasi2<-lm(IPM~AHH+knot71+knot71.8, data=working2)
summary(Estimasi2)

##Kepadatan Penduduk

#2500, 3000
working1<-data.frame(IPM,KP,knot25,knot30)
GCV1<-gam(IPM~KP+knot25+knot30, data=working1)
GCV1
Estimasi1<-lm(IPM~KP+knot25+knot30, data=working1)
summary(Estimasi1)

#3000, 3500
working2<-data.frame(IPM,KP,knot30,knot35)
GCV2<-gam(IPM~KP+knot30+knot35, data=working2)
GCV2
Estimasi2<-lm(IPM~KP+knot30+knot35, data=working2)
summary(Estimasi2)

##Nilai GCV (71,71.5 dan 2500,3000)
working1<-data.frame(IPM,AHH,knot71,knot71.5,KP,knot25,knot30)
GCV1<-gam(IPM~AHH+knot71+knot71.5+KP+knot25+knot30, data=working1)
GCV1
Estimasi1<-lm(IPM~AHH+knot71+knot71.5+KP+knot25+knot30, data=working1)
summary(Estimasi1)

##Nilai GCV (71.5,71.8 dan 3000,3500)
working2<-data.frame(IPM,AHH,knot71.5,knot71.8,KP,knot30,knot35)
GCV2<-gam(IPM~AHH+knot71.5+knot71.8+KP+knot30+knot35, data=working2)
GCV2
Estimasi2<-lm(IPM~AHH+knot71.5+knot71.8+KP+knot30+knot35, data=working2)
summary(Estimasi2)

#Kesimpulan: nilai GCV paling rendah adalah: 71.5,71.8 dan 3000, 3500

#model Multivariat
multivariat2<-lm(IPM~AHH+knot71.5+knot71.8+KP+knot30+knot35)
multivariat2
summary(multivariat2)

##GCV 
#Ypredict
Ypredict2<-predict(multivariat2)
#nilai MSE
a2<-(IPM-Ypredict2)^2
MSE2<-1/38*(sum(a2))
#membuat matriks X
Xmat2<-cbind(1,AHH+knot71.5+knot71.8+KP+knot30+knot35)
#mencari nilai trace(I-Ak)
m1<-t(Xmat2)
m2<-as.matrix(Xmat2)
m3<-solve(m1%*%m2)
Ak<-m2%*%m3%*%m1
I<-diag(1,nrow=38,ncol=38)
m4<-diag(I-Ak)
trA=sum(m4)
penyebut<-(trA/38)^2
#Nilai GCV
GCV<-MSE2/penyebut
GCV

##Pengujian asumsi 
#asumsi normal
library(nortest)
lillie.test(multivariat2$residuals)
#Uji Identik
library(lmtest)
bptest(multivariat2, data=data)
#Pemeriksaan asumsi residual independen
library(car)
durbinWatsonTest(multivariat2)
