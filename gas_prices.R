#Natural gas prices
rm(list=ls());gc();dev.off()
library(tseries)
library(car)
library(lmtest)
library(nortest)
library(olsrr)
library(glmnet)
setwd("C:\\Users\\sewer\\Desktop\\Projekt Eko\\R")
#data <- read.csv("Natural_gas.csv",sep = ";",dec = ",")
load("gas_prices_data.rData")
#Obrobka danych
{
#ustawienie nazw panstw jako nazwa obserwacji
data <- dane.all
rownames(data) <- data$Country
data <- data[,-1]
#diagnostyka danych
summary(data)
# w 3 zmiennych mamy braki danych, z czego 1 to Liechtenstein

#Eco
data$Eco[data$Eco==0] <- "Average"
data$Eco <- as.factor(data$Eco)

#HDD
a <- which(data$HDD==0)
nazwy <- rownames(data[a,])
#ustawienie wartosci HDD dla braku danych jako srednia tej wartosci
#dla panstw sasiadujacych
data[nazwy[1],"HDD"] <- mean(data["Croatia","HDD"])
data[nazwy[7],"HDD"] <- mean(c(data["Bulgaria","HDD"],data["Greece","HDD"]))
data[nazwy[2],"HDD"] <- mean(data["Turkey","HDD"])
data[nazwy[8],"HDD"] <- mean(c(data["Poland","HDD"],data["Romania","HDD"],
                               data["Slovakia","HDD"],data["Hungary","HDD"]))
data[nazwy[3],"HDD"] <- mean(data["Austria","HDD"])
data[nazwy[6],"HDD"] <- mean(data["Croatia","HDD"])
data[nazwy[4],"HDD"] <- mean(c(data["Bulgaria","HDD"],data["Greece","HDD"],
                               data["Serbia","HDD"]))
data[nazwy[5],"HDD"] <- mean(c(data["Romania","HDD"],data["Ukraine","HDD"]))

#Nuclear plant jako factor
data$Nuclear_plant <- as.factor(data$Nuclear_plant)

#Dependency
data$Dependency_import[data$Dependency_import==0] <- NA

#zostalo 1 NA i jest to Liechtenstein zatem go wyrzucam
summary(data)
which(is.na(data$Dependency_import)==TRUE)
which(data$GDP>100000)
data <- data[-c(17),]
summary(data)
View(data)
attach(data)
#dane zostaly wyczyszczone oraz uzupelnione
}


#Budowa wyjsciowego modelu
{
gas_1 <- lm(Natural_gas_price~.,data = data)
summary(gas_1)
#patrzac na podstawowe statystyki model jest zly
test_1 <- lm(Natural_gas_price~Eco)
summary(test_1)

test_2 <- lm(Natural_gas_price~Dependency_import)
summary(test_2)

test_3 <- lm(Natural_gas_price~Dependency_import+I(Dependency_import^2))
summary(test_3)

test_4 <- lm(Natural_gas_price~Nuclear_plant)
summary(test_4)

test_5 <- lm(Natural_gas_price~GDP)
summary(test_5)
}

#sensowny model, testujemy dalej (diagnoza overfit)
{
test_6 <- lm(Natural_gas_price~GDP+I(GDP^2)+HDD)
summary(test_6)

test_7 <- lm(Natural_gas_price~I(GDP^3)+I(GDP^2))
summary(test_7)


#Rysunek 1
#narysowanie wykresu
coefs <- test_7$coefficients
coefs
x <- seq(17000,130000,by=1)
plot(GDP,Natural_gas_price,pch=19)
lines(x,coefs[1]+coefs[2]*x^3+coefs[3]*x^2,col="blue",lw=2)
which(data$GDP>100000)
data[c(14,18),]
points(data[14,3],data[14,1],col="red",pch=19)
points(data[18,3],data[18,1],col="red",pch=19)
#ewidentny overfit


data_2 <- data[-c(14,18),]
test_8 <- lm(Natural_gas_price~I(GDP^3)+I(GDP^2),data = data_2)
summary(test_8)
#byl overfit i jednak model nie jest taki dobry

test_9 <- lm(Natural_gas_price~HDD)
summary(test_9)

test_10 <- lm(Natural_gas_price~I(HDD^3))
summary(test_10)

test_10 <- lm(Natural_gas_price~I(HDD^3)+HDD)
summary(test_10)


#Rysunek 2
#taki sam wykres co poprzednio
coefs <- test_10$coefficients
coefs
x <- seq(500,5500,by=0.1)
plot(HDD,Natural_gas_price,pch=19)
lines(x,coefs[1]+coefs[2]*x^3+coefs[3]*x,lw=2,col="blue")
which(data$HDD>4500)
data[29,]
points(data[29,2],data[29,1],col="red",pch=19)


data_3 <- data[-29,]
test_11 <- lm(Natural_gas_price~I(HDD^3)+HDD,data = data_3)
summary(test_11)
#ta sama sytuacja z overfitem
}

#nowe dane bez overfita z GDP i HDD i testowanie modelu na nowych danych
{
data_4 <- data[-c(14,18,29),]
test_12 <- lm(Natural_gas_price~.,data = data_4)
summary(test_12)

test_13 <- lm(Natural_gas_price~HDD+GDP+Dependency_import+Eco,data = data_4)
summary(test_13)

test_14 <- lm(Natural_gas_price~HDD+GDP+Dependency_import,data = data_4)
summary(test_14)

test_15 <- lm(Natural_gas_price~HDD+GDP+Eco,data = data_4)
summary(test_15)

test_16 <- lm(Natural_gas_price~HDD+GDP,data = data_4)
summary(test_16)

test_17 <- lm(Natural_gas_price~HDD+GDP+I(GDP^2),data = data_4)
summary(test_17)
}

#koncowy model ktory wybieram i bede sprawdzal zalozenia
{
test_18 <- lm(Natural_gas_price~HDD+I(GDP^2),data = data_4)
summary(test_18)
#plot(test_18)
e_1 <- test_18$residuals
fit_t <- test_18$fitted.values
#testy normalnosci

#Rysunek 3a
qqPlot(e_1)

shapiro.test(e_1)
jarque.bera.test(e_1)
ad.test(e_1)
lillie.test(e_1)

#bledy nie sa z rozkladu normalnego (nie przechodza wszystkich testow)
}


#dane bez Turcji i Belgii
{
data_5 <- data_4[-c(2,27),]
test_19 <- lm(Natural_gas_price~HDD+I(GDP^2),data = data_5)
summary(test_19)
e_2 <- test_19$residuals
fit_t_2 <- test_19$fitted.values
X <- model.matrix(test_19)
}

#podstawowe statystyki modelu
{
summary(test_19)$r.squared
summary(test_19)$adj.r.squared
summary(test_19)$sigma
RSS <- t(e_2)%*%e_2;RSS
MSE <- RSS/length(data_5$Natural_gas_price);MSE
}

#Diagnostyka modelu:

#testowanie normalnosci bledow
{
#graficznie:
l <- seq(-15,15,by=0.01)
#RSE
RSS <- t(e_2)%*%e_2 
SSE <- RSS/(26-3);SSE 
RSE <- sqrt(SSE);RSE

#Rysunek 3b
qqPlot(e_2)

#Rysunek 4a
#plot gestosci
plot(density(e_2),main="Porownanie gestosci")
lines(l,dnorm(l,sd=RSE),col="red")
legend("topleft",legend=c("Gestosc z probki", "Gestosc rozkladu normalnego")
       ,fill = c("black","red"),cex = 0.8)

#Rysunek 4b
#plot dystrybuant
plot(ecdf(e_2),main="Porownanie dystrybuant")
lines(l,pnorm(l,sd=RSE),col="red")
legend("topleft",legend=c("Empiryczna dystrybuanta", "Dystrybuanta rozkladu normalnego")
       ,fill = c("black","red"),cex = 0.8)
    
                      
#testami statystycznymi

shapiro.test(e_2)
jarque.bera.test(e_2)
ad.test(e_2)
lillie.test(e_2)
#tutaj bledy "przechodza" testy normalnosci
}

#testowanie liniowej zaleznosci
{

#Rysunek 5a
#fitted vs zaobserwowane
plot(data_5[,1],fit_t_2,xlab = "Wartosci zaobserwowane",ylab = "Wartosci dopasowane",pch=19)
x_3 <- seq(1,30,by=0.01)
lines(x_3,x_3,col="red")
legend("topleft",legend=c("Obserwacje", "Prosta y=x")
       ,fill = c("black","red"),cex = 0.8)
#widac trend liniowy, wartosci dopasowane rozproszone sa wokol prostej y=x


#Rysunek 5b
#residua vs fitted
plot(fit_t_2,e_2,xlab = "Wartosci dopasowane",ylab = "Residua",pch=19)
abline(h=0,col="red")
legend("bottomleft",legend=c("Obserwacje", "Prosta y=0")
       ,fill = c("black","red"),cex = 0.8)
#nie widac zadnego trendu, residua dosyc rownomiernie rozproszone wokol prostej y=0


#Rainbow test
raintest(test_19)
#nie mamy podstaw do odrzucenia hipotezy zerowej, co sugeruje nam, ze zaleznosc
#jaka badamy modelem faktycznie jest liniowa

#Harvey-Collier test
harvtest(test_19)
#podobnie w przypadku Harvey-Collier testu

#wnioskujemy, ze nasz model faktycznie opisuje zaleznosc liniowa
}


#Sprawdzanie wspolliniowosci
{
  GDP_2 <- data_5$GDP^2
View(cor(cbind(data_5[,-c(3,4,6,5)],GDP_2)))
#wektor tolerancji
T <- 1-summary(lm(HDD~I(GDP^2),data = data_5))$r.squared;T
#w przypadku durgiego modelu R^2 bedzie takie samo, zatem ten wspolczynnik nie
#wskazuje na wspolliniowosc (T<0.1 lub T<0.01 wskazuje problemy)
vif(test_19)
#podobnie wspolczynnik vif jest bliski 1
}

#Niezaleznosc bledow
{
#uporzadkowanie zmiennych rosnaco po gas_price
data_6 <- data_5[order(data_5$Natural_gas_price),]
#model na uporzadkowanych danych
test_22 <- lm(Natural_gas_price~HDD+I(GDP^2),data=data_6)
e_4 <- test_22$residuals
fit_t_4 <- test_22$fitted.values
X <- model.matrix(test_19)
summary(test_22)

#Rysunek 6a
#residua vs numer obserwacji
plot(e_4,xlab = "Numer obserwacji",ylab = "Residua",pch=19)
#nie widac zadnych trendow

#bez posortowania
#plot(e_2,xlab = "Numer obserwacji",ylab = "Residua",pch=19)


#Rysunek 6b
#autokorelacja
acf(e_4)

#bez posortowania
#acf(e_2)


#testy
durbinWatsonTest(test_22)
durbinWatsonTest(test_19)
#nie ma podstaw do odrzucenia H_0 ze wspol autokor rho=0
Box.test(e_4,type = "Ljung",lag = 3)
#podobna sytuacja


#SPATIAL CORRELATION?
}

#Homoskedastycznosc
{
  
#Rysunek 5b jeszcze raz
#residua vs fitted (znow)


#testy statystczyne
#Breuscha-Pagana
bptest(test_19)
#nie ma podstaw do odrzucenia hipotezy zerowej o rownosci wariancji bledow


#Goldfielda-Quanta
#moze byc problem z g-q test bo jest malo danych
gqtest(test_19)
#nie ma podstaw do odrzucenia hipotezy zerowej o rownosci wariancji bledow
}

#Obserwacje odstajace
{
  
#diagonala macierzy projekcji (dzwignia)
H <- diag(X%*%solve(t(X)%*%X,tol = 2.00517e-21)%*%t(X))
  
#Rysunek 7a
barplot(H,ylim = c(0,0.3),main = "Dzwignia")
abline(6/26,0,col="red")


order(H,decreasing = T)


#tyle musi wynosic
dim(X)
3/26;mean(H)
H[H>6/26]
#brak obserwacji odstajacych ze wzgledu na dzwignie
#(przypominam ze juz outliery wyrzucilismy przy dopasowywaniu modelu z GDP i HDD)
#w ogolnosci by tak nie musialo byc


#Cook's distance
#Rysunek 7b
ols_plot_cooksd_bar(test_19)
data_5[c(9,20),]
}


#LASSO
{
#stworzenie macierzy danych i zmiennej objasnianej
y_1 <- data_5$Natural_gas_price
x_1 <- data.matrix(data_5[,2:6])
#utowrzenie modelu lasso
cv_model <- cv.glmnet(x_1,y_1,alpha=1)
cv_model
#wyciagniecie lambdy minimalizujacej mse
best_lambda <- cv_model$lambda.min;best_lambda

#Rysunek 8
plot(cv_model)


#stworzenie ostatecznego modelu lasso z najlepsza lambda
best_model <- glmnet(x_1,y_1,alpha = 1,lambda = best_lambda);best_model
coef(best_model)
# reczne obliczenie r^2
y_predicted <- predict(best_model,s=best_lambda,newx = x_1)
sst <- sum((y_1 - mean(y_1))^2)
sse <- sum((y_predicted - y_1)^2)
r_squared <- 1-sse/sst;r_squared 
#sprawdzenie wspolliniowosci
vif(gas_1)
}


#Walidacja krzyzowa
{
#to jest mozliwe dlatego ze dane sa nieskorelowane czasowo
#stworzenie zbiorow testowych
cross_data1 <- data_5[1:6,]
cross_data2 <- data_5[7:12,]
cross_data3 <- data_5[13:19,]
cross_data4 <- data_5[20:26,]
#zbiory uczace
train_1 <- data_5[7:26,];train_1
train_2 <- data_5[c(1:6,13:26),];train_2
train_3 <- data_5[c(1:12,20:26),];train_3
train_4 <- data_5[1:19,];train_4

#tworzenie modeli ze zmienianymi zbiorami uczacymi
#1
cross_model1 <- lm(Natural_gas_price~HDD+I(GDP^2),
                   data = train_1)
summary(cross_model1)
y_hat1 <- predict(cross_model1,cross_data1);y_hat1
MSE_test1 <- sum((y_hat1-cross_data1[,1])^2)/length(y_hat1);MSE_test1
RMSE_test1 <- sqrt(MSE_test1);RMSE_test1

#2
cross_model2 <- lm(Natural_gas_price~HDD+I(GDP^2),
                   data = train_2)
summary(cross_model2)
y_hat2 <- predict(cross_model2,cross_data2);y_hat2
MSE_test2 <- sum((y_hat2-cross_data2[,1])^2)/length(y_hat2);MSE_test2
RMSE_test2 <- sqrt(MSE_test2);RMSE_test2

#3
cross_model3 <- lm(Natural_gas_price~HDD+I(GDP^2),
                   data = train_3)
summary(cross_model3)
y_hat3 <- predict(cross_model3,cross_data3);y_hat3
MSE_test3 <- sum((y_hat3-cross_data3[,1])^2)/length(y_hat3);MSE_test3
RMSE_test3 <- sqrt(MSE_test3);RMSE_test3

#4
cross_model4 <- lm(Natural_gas_price~HDD+I(GDP^2),
                   data = train_4)
summary(cross_model4)
y_hat4 <- predict(cross_model4,cross_data4);y_hat4
MSE_test4 <- sum((y_hat4-cross_data4[,1])^2)/length(y_hat4);MSE_test4
RMSE_test4 <- sqrt(MSE_test4);RMSE_test4

#obliczenie ko?cowych statystyk
MSE_test_all <- (MSE_test1+MSE_test2+MSE_test3+MSE_test4)/4
MSE_test_all
RMSE_test_all <- (RMSE_test1+RMSE_test2+RMSE_test3+RMSE_test4)/4
RMSE_test_all

#mse_training
RSS <- t(e_2)%*%e_2;RSS
MSE <- RSS/length(data_5$Natural_gas_price);MSE
RMSE <- sqrt(MSE);RMSE
}













