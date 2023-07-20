#---IMPORTING LIBRARIES---
install.packages("gridExtra")
library(MASS)
library(GGally)
library(robustbase)
library(rrcov)
library(gridExtra)
library(ggplot2)
library(nnet)

#--READING THE DATA

x=read.csv("breastcancer.csv")
xB=read.csv("Breastcancer(B).csv")
xM=read.csv("Breastcancer(M).csv")
x$diagnosis = as.factor(x$diagnosis)
print(head(x))
x_subset=x[,3:12]
x_subsetB=xB[,3:12]
x_subsetM=xM[,3:12]
#x_subsetB_scaled= scale(x_subsetB)
#x_subsetM_scaled=scale(x_subsetM)
print(head(x_subsetB))
print(head(x_subsetM))


#--CHECKING FOR N/A VALUES

print(anyNA(x_subsetB))
print(anyNA(x_subsetM))

#--SUMMARY OF THE DATA

print(summary(x_subsetB))
print(summary(x_subsetM))

#--CREATING THE GGPAIRS SCATTERPLOTS

ggpairs(x_subsetB)
#quartz()
windows()
ggpairs(x_subsetM)

#quartz()
windows()

#--Normality validation using qq plots
qqplot_all_columns = function(df){
  par(mfrow=c(2,5))
  for (i in 1:10){
    qqnorm(df[, i], main = names(df[i]))
    qqline(df[, i])
  }
  
}

qqplot_all_columns(x_subset) 

qqplot_all_columns(x_subsetB) 
qqplot_all_columns(x_subsetM)

#---APPLYING LOGARITHMIC TRANSFORMATION TO THE VARAIBLES---
#x_subset1=x_subset
#x_subset1=x_subset1+abs(min(x_subset1))+1

x_subsetB1=x_subsetB
x_subsetM1=x_subsetM



#---Transformation B class----

x_subsetB1=scale(x_subsetB1)

x_subsetB1=as.data.frame(x_subsetB1)

x_subsetB1$texture_mean=x_subsetB1$texture_mean+abs(min(x_subsetB1$texture_mean))+1

x_subsetB1$compactness_mean=x_subsetB1$compactness_mean+abs(min(x_subsetB1$compactness_mean))+1

x_subsetB1$concavity_mean=x_subsetB1$concavity_mean+abs(min(x_subsetB1$concavity_mean))+1

x_subsetB1$concave.points_mean=x_subsetB1$concave.points_mean+abs(min(x_subsetB1$concave.points_mean))+1

x_subsetB1$symmetry_mean=x_subsetB1$symmetry_mean+abs(min(x_subsetB1$symmetry_mean))+1

x_subsetB1$fractal_dimension_mean=x_subsetB1$fractal_dimension_mean+abs(min(x_subsetB1$fractal_dimension_mean))+1


x_subsetB1$texture_mean=log(x_subsetB1$texture_mean)

x_subsetB1$compactness_mean=log(x_subsetB1$compactness_mean)

x_subsetB1$concavity_mean=log(x_subsetB1$concavity_mean)

x_subsetB1$concave.points_mean=log(x_subsetB1$concave.points_mean)

x_subsetB1$symmetry_mean=log(x_subsetB1$symmetry_mean)

x_subsetB1$fractal_dimension_mean=log(x_subsetB1$fractal_dimension_mean)



qqplot_all_columns(x_subsetB1)
#--Transformation for M class---

qqplot_all_columns(x_subsetM)

x_subsetM1=scale(x_subsetM1)

x_subsetM1=as.data.frame(x_subsetM1)

x_subsetM1$texture_mean=x_subsetM1$texture_mean+abs(min(x_subsetM1$texture_mean))+1

x_subsetM1$compactness_mean=x_subsetM1$compactness_mean+abs(min(x_subsetM1$compactness_mean))+1

x_subsetM1$concavity_mean=x_subsetM1$concavity_mean+abs(min(x_subsetM1$concavity_mean))+1

x_subsetM1$concave.points_mean=x_subsetM1$concave.points_mean+abs(min(x_subsetM1$concave.points_mean))+1

x_subsetM1$symmetry_mean=x_subsetM1$symmetry_mean+abs(min(x_subsetM1$symmetry_mean))+1

x_subsetM1$fractal_dimension_mean=x_subsetM1$fractal_dimension_mean+abs(min(x_subsetM1$fractal_dimension_mean))+1

x_subsetM1$perimeter_mean=x_subsetM1$perimeter_mean+abs(min(x_subsetM1$perimeter_mean))+1

x_subsetM1$area_mean=x_subsetM1$area_mean+abs(min(x_subsetM1$area_mean))+1

x_subsetM1$smoothness_mean=x_subsetM1$smoothness_mean+abs(min(x_subsetM1$smoothness_mean))+1

x_subsetM1$radius_mean=x_subsetM1$radius_mean+abs(min(x_subsetM1$radius_mean))+1



x_subsetM1=log(x_subsetM1)

qqplot_all_columns(x_subsetM1) 


#---Transformation for the whole dataset

x_subset1=x_subset


x_subset1=scale(x_subset1)

x_subset1=as.data.frame(x_subset1)



x_subset1$texture_mean=x_subset1$texture_mean+abs(min(x_subset1$texture_mean))+1

x_subset1$compactness_mean=x_subset1$compactness_mean+abs(min(x_subset1$compactness_mean))+1

x_subset1$concavity_mean=x_subset1$concavity_mean+abs(min(x_subset1$concavity_mean))+1

x_subset1$concave.points_mean=x_subset1$concave.points_mean+abs(min(x_subset1$concave.points_mean))+1

x_subset1$symmetry_mean=x_subset1$symmetry_mean+abs(min(x_subset1$symmetry_mean))+1

x_subset1$fractal_dimension_mean=x_subset1$fractal_dimension_mean+abs(min(x_subset1$fractal_dimension_mean))+1

x_subset1$perimeter_mean=x_subset1$perimeter_mean+abs(min(x_subset1$perimeter_mean))+1

x_subset1$area_mean=x_subset1$area_mean+abs(min(x_subset1$area_mean))+1

x_subset1$smoothness_mean=x_subset1$smoothness_mean+abs(min(x_subset1$smoothness_mean))+1

x_subset1$radius_mean=x_subset1$radius_mean+abs(min(x_subset1$radius_mean))+1


x_subset1$texture_mean=log(x_subset1$texture_mean)
x_subset1$symmetry_mean=log(x_subset1$symmetry_mean)
x_subset1$compactness_mean=log(x_subset1$compactness_mean)
x_subset1$concavity_mean=log(x_subset1$concavity_mean)
x_subset1$concave.points_mean=log(x_subset1$concave.points_mean)
x_subset1$fractal_dimension_mean=log(x_subset1$fractal_dimension_mean)
x_subset1$perimeter_mean=log(x_subset1$perimeter_mean)
x_subset1$area_mean=log(x_subset1$area_mean)
x_subset1$smoothness_mean=log(x_subset1$smoothness_mean)
x_subset1$radius_mean=log(x_subset1$radius_mean)


qqplot_all_columns(x_subset1) 

#--Fisher Linear Discriminant Analysis--Internal Validation
flda=function(x,class) {
  cat("Fisher Linear Discriminant")
  cat("\nInternal validation:")
  a = lda(x, class); d = predict(a) 
  t=table(class, d$class); print(t) 
  er=100*(sum(t)-sum(diag(t)))/nrow(x) 
  cat("Error Rate =",er,"%\n")
  return(d) 
}

result=flda(x_subset1,x[,2])


#--External VALIDATION (LEAVE ONE OUT METHOD)----
loo=function(x,class){
  cat("Fisher Linear Discriminant")
  cat("\nExternal validation:")
  prediction = lda(x,class,CV=T)
  cm = table(class,prediction$class)
  print(cm)
  er=100*(sum(cm)-sum(diag(cm)))/nrow(x) 
  cat("Error Rate =",er,"%\n")
  return(prediction)
}

result=loo(x_subset1,x[,2])

# Fisher Linear Discriminant Analysis; p = 2 and k = 2
flda2=function(x, class) { 
  if(ncol(x)!=2) {cat("Data should be 2-dimensional\n" ); return()}
  t=factor(class);   level=levels(t);   
  if(length(level)!=2) {cat("Data should have only two groups\n" ); return()}
  y=x[class==level[1],];     x=x[class==level[2],]
  n1=nrow(x);   n2=nrow(y); n=n1+n2;  xcenter=colMeans(x);  ycenter=colMeans(y)
  xcov=cov(x); ycov=cov(y);   sp=(n1-1)*xcov+(n2-1)*ycov; sp=sp/(n-2)
  d=xcenter-ycenter; m=(xcenter+ycenter)/2;  a=solve(sp)%*%d;  
  class=c(rep(1,n1),rep(2,n2));      p=1;      
  z=rbind(x,y);       pred=z-matrix(m,ncol=2,nrow=n, byrow=T); pred=as.matrix(pred)
  pred=(pred%*%a)<log(p);    C=(class!=pred+1);     ce=sum(C)
  cat("--------------------------------------------------\n")
  cat("           Correct         Incorrect\n Class  Classification   Classification    Total\n")  
  cat("--------------------------------------------------\n")
  cd1=n1-sum(C[1:n1]);         cat("  1          ",cd1,"             ", n1-cd1,"            ",n1,"\n")
  cd2=n2-sum(C[(n1+1):n]);  cat("  2          ",cd2,"             ", n2-cd2,"            ",n2,"\n")
  cat("--------------------------------------------------\n")
  cat(" Total:     ",cd1+cd2,"             ", n-(cd1+cd2),"           ",n,"\n")  
  cat("--------------------------------------------------\n")
  cat("Error Rate = ",100*(ce/n),"%\n");  const=(sum(a*m)+log(p))/a[2]; slope=-a[1]/a[2]
  z=rbind(x,y);   print(rbind(xcenter[1:2],ycenter[1:2]))
  plot(z[,c(1,2)],col=class,pch=19);  abline(const,slope, col = 'blue'); 
  points(rbind(xcenter[1:2],ycenter[1:2]),pch=19,col=3,cex=1.5); 
  segments(xcenter[1], xcenter[2], ycenter[1], ycenter[2],col=3)
  list(xcenter=xcenter[2:1],ycenter=ycenter[2:1],xcov=xcov,ycov=ycov,sp=sp,a=a,slope=slope,
       const=const,ce=ce,m=m,z=z) }



a=flda2(x_subset1[,c(2,9)],x[,2])

#---Projection FLDA
a = lda(x_subset1, x[,2])
d = predict(a) 
plot(d$x,pch=19,col=x[,2] )
abline(h = mean(range(d$x)), col = "black", lty = 2)

#---APPLYING MULTINOMIAL WITH INTERNAL VALIDATION---

library(nnet)
mn = multinom(class ~., data = x_subset)
results = predict(mn)
t1=table(x_subset_class$diagnosis, results)
cat("Multinomial")
cat("\nInternal validation:\n")
print(t1) 
er1=100*(sum(t1)-sum(diag(t1)))/nrow(x_subset_class) 
cat("Error Rate =",er1,"%\n")

## ----APPLYING MULTINOMIAL WITH EXTERNAL VALIDATION----

looMN=function(x_subset,class){
  n=length(class)
  rslt={}
  for(i in 1:n){
    y=x_subset[-i,]
    a = multinom(class[-i]~. , x_subset[-i,])
    b = predict(a,x_subset[i,])
    rslt[i]=b
  }
  return(rslt)
}
rslt=looMN(x_subset,class)
cat("Multinomial")
cat("\nExternal validation:\n")
print(table(class,rslt))
t2= table(class,rslt)
er2=100*(sum(t2)-sum(diag(t2)))/nrow(x_subset_class)
cat("Error Rate =",er2,"%\n")
