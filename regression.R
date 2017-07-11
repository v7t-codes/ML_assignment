#mean
setosa_mean_1 <- c(mean(iris $ Sepal.Length[iris$Species=="setosa"]),mean(iris $ Sepal.Width[iris$Species=="setosa"]),mean(iris $ Petal.Length[iris$Species=="setosa"]),mean(iris $ Petal.Width[iris$Species=="setosa"]))
setosa_mean_1

virginica_mean_1 <- c(mean(iris $ Sepal.Length[iris$Species=="virginica"]),mean(iris $ Sepal.Width[iris$Species=="virginica"]),mean(iris $ Petal.Length[iris$Species=="virginica"]),mean(iris $ Petal.Width[iris$Species=="virginica"]))
virginica_mean_1

versicolor_mean_1 <- c(mean(iris $ Sepal.Length[iris$Species=="versicolor"]),mean(iris $ Sepal.Width[iris$Species=="versicolor"]),mean(iris $ Petal.Length[iris$Species=="versicolor"]),mean(iris $ Petal.Width[iris$Species=="versicolor"]))
versicolor_mean_1

setosa_data <- matrix(c(iris $ Sepal.Length[iris$Species=="setosa"],iris $ Sepal.Width[iris$Species=="setosa"],iris $ Petal.Length[iris$Species=="setosa"],iris $ Petal.Width[iris$Species=="setosa"]),ncol=4)
setosa_data

setosa_mean_petal <- c(mean(iris $ Petal.Length[iris$Species=="setosa"]),mean(iris $ Petal.Width[iris$Species=="setosa"]))
setosa_mean_petal

virginica_mean_petal <- c(mean(iris $ Petal.Length[iris$Species=="virginica"]),mean(iris $ Petal.Width[iris$Species=="virginica"]))
virginica_mean_petal

versicolor_mean_petal <- c(mean(iris $ Petal.Length[iris$Species=="versicolor"]),mean(iris $ Petal.Width[iris$Species=="versicolor"]))
versicolor_mean_petal


#covariance
cov_setosa<-cov(setosa_data)

virginica_data <- matrix(c(iris $ Sepal.Length[iris$Species=="virginica"],iris $ Sepal.Width[iris$Species=="virginica"],iris $ Petal.Length[iris$Species=="virginica"],iris $ Petal.Width[iris$Species=="virginica"]),ncol=4)
virginica_data
cov(virginica_data)

versicolor_data <- matrix(c(iris $ Sepal.Length[iris$Species=="versicolor"],iris $ Sepal.Width[iris$Species=="versicolor"],iris $ Petal.Length[iris$Species=="versicolor"],iris $ Petal.Width[iris$Species=="versicolor"]),ncol=4)
versicolor_data
cov(versicolor_data)

#subset
setosa_data_subset <- matrix(c(iris $ Petal.Length[iris$Species=="setosa"],iris $ Petal.Width[iris$Species=="setosa"]),ncol=2)
cov_setosa_petal <- cov(setosa_data_subset)

virginica_data_subset <- matrix(c(iris $ Petal.Length[iris$Species=="virginica"],iris $ Petal.Width[iris$Species=="virginica"]),ncol=2)
cov_virginica_petal <- cov(virginica_data_subset)

versicolor_data_subset <- matrix(c(iris $ Petal.Length[iris$Species=="versicolor"],iris $ Petal.Width[iris$Species=="versicolor"]),ncol=2)
cov_versicolor_petal <- cov(versicolor_data_subset)

#plot
plot(iris$Petal.Length,iris$Petal.Width,pch=21,bg=c("red","green","blue")[unclass(iris$Species)],main="Iris Data")
plot(iris$Sepal.Length,iris$Sepal.Width,pch=23,bg=c("red","green","blue")[unclass(iris$Species)],main="Iris Data")

#gaussian
x <-seq(0,7,0.1)
y <-seq(0,7,0.1)
gaussian_contour <-matrix(NA,nrow=length(x),ncol=length(y))
for(i in 1:length(x)){ for (j in 1:length(y)){ gaussian_contour[i,j]=1/(2*pi*sqrt(det(cov_setosa_petal)))*exp(-0.5*(t(c(x[i],x[j])-setosa_mean_petal)%*%(solve(cov_setosa_petal))%*%(c(x[i],y[j])-setosa_mean_petal)))    }  }
contour(x,y,gaussian_contour)
contour(x,y,gaussian_contour,add=TRUE)

x<-seq(0,1,length=10)
y<-sin(2*pi*x)
set.seed(0)
noisy.y <-y+rnorm(10,sd=0.3)
plot(x,noisy.y)
lines(x,y)

lm9=lm(noisy.y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9))
coef(lm9)
summary(lm9)

lm8=lm(noisy.y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8))
coef(lm8)
summary(lm8)

lm7=lm(noisy.y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7))
coef(lm7)
summary(lm7)

lm6=lm(noisy.y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
coef(lm6)
summary(lm6)

lm5=lm(noisy.y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5))
coef(lm5)
summary(lm5)

lm4=lm(noisy.y~x+I(x^2)+I(x^3)+I(x^4))
coef(lm4)
summary(lm4)

lm3=lm(noisy.y~x+I(x^2)+I(x^3))
coef(lm3)
summary(lm3)

lm2=lm(noisy.y~x+I(x^2))
coef(lm2)
summary(lm2)

lm1=lm(noisy.y~x)
coef(lm1)
summary(lm1)

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm1,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="orange")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm2,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="green")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm3,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="pink")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm9,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="red")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm4,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="green3")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm5,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="blue")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm6,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="purple")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm7,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="yellow")

xplot<-seq(0,1,length=100)
ypredict1<-predict(lm8,newdata=data.frame(x=xplot))
lines(xplot,ypredict1,col="magenta")

lms=list(lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9)
train=test=rep(0,9)
noisy.y <-y+rnorm(100,sd=0.3)

for(i in 1:9)
{
  train[i]=var(lms[i][[1]]$residual)
}

for(j in 1:9)
{
  test[j]=(1/100)*sum((noisy.y-predict(lms[j][[1]],newdata=data.frame(xplot)))^2)
}

plot(1:9,train)
points(1:9,test,pch="x")
lines(1:9,train,col="blue")
lines(1:9,test,col="green")