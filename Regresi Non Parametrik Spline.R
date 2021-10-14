library(readxl)

data<- read_excel("D:/STATISTIKA/MATA KULIAH/KAMPUS MERDEKA/PERTUKARAN MAHASISWA/UNIVERSITAS NEGERI MAKASSAR/TUGAS/TUGAS 5/UR.xlsx", 
                col_types = c("numeric", "numeric"))
View(data)
summary(data)

#Scatterplot
attach(data)
plot( Umur,Rasio,  main="Scatter Plot Rasio dan Umur", xlab="Umur", ylab="Rasio")

#Regresi Polinomial
ml <- lm(Rasio ~ Umur,data = data)
m2 <- lm(Rasio ~ poly(Umur,2) + Umur, data = data)
plot(Rasio ~Umur, data)
lines(data$Umur, predict(m2), col ='red')
data$X_bar <- ifelse(data$Umur>10,1,0)
data$diff <- data$Umur-10
data$X <- data$diff*data$X_bar
reg <- lm(Rasio ~ Umur + X , data = data)
plot(Rasio~ Umur, data)
lines(data$Umur, predict(reg), col='blue')

##Nilai GCV
library(mgcv)
attach(data)

head(data)
X2= Umur^2


XA=ifelse(Umur>12.5, 1,0)
XB=ifelse(Umur> 17.5,1,0)

#Linier Spline 2 titik knot
Knot1<-(Umur-12.5)*XA
Knot2<-(Umur-17.5)*XB

##satu titik knot
#12.5
EVM<-data.frame(Rasio, Umur,Knot1)
GCV1<-gam(Rasio~Umur+Knot1, data=EVM)
GCV1
Estimasi1<-lm(Rasio~Umur+Knot1, data=EVM)
summary(Estimasi1)
#17.5
EVM11<-data.frame(Rasio, Umur,Knot2)
GCV11<-gam(Rasio~Umur+Knot2, data=EVM11)
GCV11
Estimasi11<-lm(Rasio~Umur+Knot2, data=EVM11)
summary(Estimasi11)

##dua titik knot
EVM1<-data.frame(Rasio, Umur,Knot1,Knot2)
GCV2<-gam(Rasio~Umur+Knot1+Knot2, data=EVM1)
GCV2
Estimasi2<-lm(Rasio~Umur+Knot1+Knot2, data=EVM1)
summary(Estimasi2)

#Kuadratik spline 1 titik knot
#12.5
Knot1<-(Umur-12.5)^2*XA
EVM2<-data.frame(Rasio,Umur,X2,Knot1)
GCV3<-gam(Rasio~Umur+X2+Knot1, data=EVM2)
GCV3
Estimasi3<-lm(Rasio~Umur+X2+Knot1, data=EVM2)
summary(Estimasi3)

#17.5
Knot12<-(Umur-17.5)^2*XB
EVM22<-data.frame(Rasio,Umur,X2,Knot12)
GCV32<-gam(Rasio~Umur+X2+Knot12, data=EVM22)
GCV32
Estimasi32<-lm(Rasio~Umur+X2+Knot12, data=EVM22)
summary(Estimasi32)


