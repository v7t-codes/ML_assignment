versicolor_mean_sl <- c(mean(iris$Sepal.Length[iris$Species== 'versicolor']))
virginica_mean_sl <- c(mean(iris$Sepal.Length[iris$Species== 'virginica']))
setosa_mean_sl <- c(mean(iris$Sepal.Length[iris$Species== 'setosa']))
#Petal.Length MEAN
versicolor_mean_pl <- c(mean(iris$Petal.Length[iris$Species== 'versicolor']))
virginica_mean_pl <- c(mean(iris$Petal.Length[iris$Species== 'virginica']))
setosa_mean_pl  <- c(mean(iris$Petal.Length[iris$Species== 'setosa']))
#Sepal.Width MEAN
versicolor_mean_sw <- c(mean(iris$Sepal.Width[iris$Species== 'versicolor']))
virginica_mean_sw <- c(mean(iris$Sepal.Width[iris$Species== 'virginica']))
setosa_mean_sw <- c(mean(iris$Sepal.Width[iris$Species== 'setosa']))
#Petal.Width MEAN
versicolor_mean_pw <- c(mean(iris$Petal.Width[iris$Species== 'versicolor']))
virginica_mean_pw <- c(mean(iris$Petal.Width[iris$Species== 'virginica']))
setosa_mean_pw <- c(mean(iris$Petal.Width[iris$Species== 'setosa']))

# TO FIND THE COVARIANCE OF THE FEATURES
#############################
setosa_feats<-matrix(c(iris$Sepal.Length[iris$Species== 'setosa'],
                       iris$Petal.Length[iris$Species== 'setosa'],
                       iris$Sepal.Width[iris$Species== 'setosa'],
                       iris$Petal.Width[iris$Species== 'setosa']), ncol=4)

cov(setosa_feats)
#############################
versicolor_feats<-matrix(c(iris$Sepal.Length[iris$Species== 'virginica'],
                           iris$Petal.Length[iris$Species== 'virginica'],
                           iris$Sepal.Width[iris$Species== 'virginica'],
                           iris$Petal.Width[iris$Species== 'virginica']), ncol=4)
cov(virginica_feats)
############################
versicolor_feats<-matrix(c(iris$Sepal.Length[iris$Species== 'versicolor'],
                           iris$Petal.Length[iris$Species== 'versicolor'],
                           iris$Sepal.Width[iris$Species== 'versicolor'],
                           iris$Petal.Width[iris$Species== 'versicolor']), ncol=4)
cov(versicolor_feats)
#############################
#last two features of the species
setosa_feats2d<-matrix(c(iris$Pepal.Length[iris$Species== 'setosa'],
                         iris$Petal.Width[iris$Species== 'setosa']), ncol=2)
c_set<-cov(setosa_feats2d)
############################
virginica_feats2d<-matrix(c(iris$Petal.Length[iris$Species== 'virginica'],
                            iris$Petal.Width[iris$Species== 'virginica']), ncol=2)
c_vir<-cov(viriginica_feats2d)
###########################
versicolor_feats2d<-matrix(c(iris$Petal.Length[iris$Species== 'versicolor'],
                             iris$Petal.Width[iris$Species== 'versicolor']), ncol=2)
c_ver<-cov(versicolor_feats2d)
###############################
plot(iris$Petal.Length,iris$Petal.Width,pch = 21,bg = c("red","green","blue")[unclass(iris$Species)],main = "Petal Data")

set_pet_mean = c(mean(iris$Petal.Length[iris$Species=="setosa"]),mean(iris$Petal.Width[iris$Species=="setosa"]))
x = seq(0, 7, 0.1)
y =seq(0,7, 0.1)
gaussian_contour1<- matrix(NA,nrow=length(x), ncol= length(y))
for(i in 1:length(x)){for(j in 1:length(y)){gaussian_contour1[i,j] = 1/(2*pi*(sqrt(det(cov(setosa_feats2d)))))*exp(-0.5*(t(c(x[i],y[j])-set_pet_mean)%*%(solve(cov(setosa_feats2d)))%*%(c(x[i],y[j])-set_pet_mean)))}}
#contour(x,y,gaussian_contour1,add= TRUE)


set_pet_mean = c(mean(iris$Petal.Length[iris$Species=="virginica"]),mean(iris$Petal.Width[iris$Species=="virginica"]))
x = seq(0, 7, 0.1)
y =seq(0,7, 0.1)
gaussian_contour2<- matrix(NA,nrow=length(x), ncol= length(y))
for(i in 1:length(x)){for(j in 1:length(y)){gaussian_contour2[i,j] = 1/(2*pi*(sqrt(det(cov(virginica_feats2d)))))*exp(-0.5*(t(c(x[i],y[j])-set_pet_mean)%*%(solve(cov(virginica_feats2d)))%*%(c(x[i],y[j])-set_pet_mean)))}}
#contour(x,y,gaussian_contour2,add= TRUE)
super1 = gaussian_contour2-gaussian_contour1
contour(x,y,super1, add = TRUE)

set_pet_mean = c(mean(iris$Petal.Length[iris$Species=="versicolor"]),mean(iris$Petal.Width[iris$Species=="versicolor"]))
x = seq(0, 7, 0.01)
y =seq(0,7, 0.01)
gaussian_contour3<- matrix(NA,nrow=length(x), ncol= length(y))
for(i in 1:length(x)){for(j in 1:length(y)){gaussian_contour3[i,j] = 1/(2*pi*(sqrt(det(cov(versicolor_feats2d)))))*exp(-0.5*(t(c(x[i],y[j])-set_pet_mean)%*%(solve(cov(versicolor_feats2d)))%*%(c(x[i],y[j])-set_pet_mean)))}}
contour(x,y,gaussian_contour3,add= TRUE)
super2 = gaussian_contour3 - gaussian_contour2
super3 = gaussian_contour3 - gaussian_contour1
contour(x,y,super2,add= TRUE)
contour(x,y,super3,add= TRUE)