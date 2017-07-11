x<-data.frame(c(iris$Petal.Length[1:100]),c(iris$Petal.Width[1:100]));
y<-data.frame(c(iris$Species[1:100]))
eta<-1;
niter<-10
plot(x$c.iris.Petal.Length.1.100..,x$c.iris.Petal.Width.1.100..)

perceptron <- function(x, y, eta, niter)
{
  # initialize weight vector
  weight <-rep(0, dim(x)[2] + 1)
  errors <-rep(0, niter)
  # initialize decision boundary
  boundary_x <-seq(-1,max(irissubdf[,1]),0.1)
  boundary_y <-rep(0,length(boundary_x)) 
  # loop over number ofiterations
  for (jj in 1:niter) 
  {
    # loop through training data set
    for (ii in 1:length(y)) 
    {
      # Predict binary label using Heaviside activationfunction 
      z<-sum(weight[2:length(weight)] * as.numeric(x[ii, ])) + weight[1]
      if(z < 0) ypred <--1
      else if (z > 0) ypred <-1
      if (z == 0) { 
        if (y[ii] == -1) ypred <-1
        else ypred <--1
      }
      # Change weight-the formula doesn't do anything if the predicted value is correct
      weightdiff <-eta * 0.5* (y[ii]-ypred) * c(1, as.numeric(x[ii, ]))
      weight <-weight + weightdiff
      print(weight)
      # Update error function and draw decision boundary
      if ((y[ii]-ypred) != 0.0) {
        errors[jj] <-errors[jj]+ 1
        for (i in 1:length(boundary_x)) {
          boundary_y[i] <-(1/weight[3])*(-weight[1]-weight[2]*boundary_x[i])
        }
        lines(boundary_x, boundary_y,col=10*ii+50*jj) 
      }
    }
  }
  # weight to decide between the two species 
  return(list(err=errors,wt=weight))
}

set.seed(0)
temp_vc <-iris[51:100, c(3,4,5)];
temp_vi <-iris[100:150, c(3,4,5)];
select_vc <-sample(nrow(temp_vc),40)
select_vi <-sample(nrow(temp_vi),40)
train_vc <-temp_vc[select_vc,]
train_vi <-temp_vi[select_vi,]
test_vc <-temp_vc[-select_vc,]
test_vi <-temp_vi[-select_vi,]
iris_train <-rbind.data.frame(train_vc,train_vi)
iris_test <-rbind.data.frame(test_vc,test_vi)

iris2<-iris[51:150,c(3,4,5)]
iris2[,4]<-1
iris2[iris2$Species=="virginica",4]<--1

x<-iris2[1:2]
y<-iris2[,4]


plot(iris2$Petal.Length,iris2$Petal.Width,pch=23,bg=c("green","red", "blue")[unclass(iris2$Species)],main="petal
     percepton")
err<-perceptron(x,y,1,25)
