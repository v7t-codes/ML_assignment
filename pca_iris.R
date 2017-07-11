irn = iris[,1:4]
pc = prcomp(irn,retx=TRUE, scale. =TRUE)
plot(pc,type="l")
pr = predict(pc,irn)
pr = as.data.frame(pr)
pr[5]= iris[5]
pr = as.data.frame(pr)
plot(pr$PC1,pr$PC2,pch=21,bg=c('red','green','blue')[unclass(pr$Species)],main="IRIS DATA")
a1= mean(pr$PC1[pr$Species=='setosa'])
a2= mean(pr$PC2[pr$Species=='setosa'])
a = c(a1,a2)
b1 = mean(pr$PC1[pr$Species=='virginica'])
b2 = mean(pr$PC2[pr$Species=='virginica'])
b = c(b1,b2)
c1 = mean(pr$PC1[pr$Species=='versicolor'])
c2 = mean(pr$PC2[pr$Species=='versicolor'])
c = c(c1,c2)
pc1 = subset(pr, pr$Species=='setosa')
pc1 = pc1[1:2]
pc2 = subset(pr, pr$Species=='versicolor')
pc2 = pc2[1:2]
cv1 = cov(pc1,pc1)
cv2=cov(pc2,pc2)
x<-seq(-7,7,0.1)
y<-seq(-7,7,0.1)
gc1<-matrix(NA,nrow=length(x),ncol=length(y))
for(i in 1:length(x))
  for(j in 1:length(y))
    gc1[i,j]=1/(2*pi*sqrt(det(cv1)))*exp(-0.5*t(c(x[i],y[j])-a)%*%solve(cv1)%*%(c(x[i],y[j])-a))
contour(x,y,gc1,add=TRUE)



gc2<-matrix(NA,nrow=length(x),ncol=length(y))
for(i in 1:length(x))
  for(j in 1:length(y))
    gc2[i,j]=1/(2*pi*sqrt(det(cv2)))*exp(-0.5*t(c(x[i],y[j])-b)%*%solve(cv2)%*%(c(x[i],y[j])-b))
contour(x,y,gc2,add=TRUE)

contour(x,y,log(gc1)-log(gc2),add=TRUE,level=0)