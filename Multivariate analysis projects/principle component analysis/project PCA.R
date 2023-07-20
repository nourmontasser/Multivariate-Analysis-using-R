#---the Classical PCA part---

library(MASS)
library(GGally)
library(robustbase)
library(rrcov)
library(gridExtra)
library(ggplot2)

df= read.csv("breastcancer.csv")
head(df)
dim(df)
df=df[,3:12]

# ----PREVIEWING DATA----
library(GGally)
ggpairs(df, aes(alpha = 0.5), 
        upper = list(continuous = wrap("cor", size = 3)))
windows()


# ----STANDARDIZING DATA----
x=scale(df, center=TRUE, scale=TRUE)

#----PERFORMING CLASSICAL PCA----
pc=princomp(x, cor=T)
summary(pc,loadings=T)
plot(pc)

v=pc$sd^2 
sv=sum(v)
cs=cumsum(v)/sv
cs
m=length(cs[cs<0.85])
m
pc$scores
pc$sd
pc$loadings

#---the Robust PCA Part---



x=read.csv("breastcancer.csv")
x$diagnosis = as.factor(x$diagnosis)
print(head(x))
x_subset=x[,3:12]
print(head(x_subset))

ggpairs(x_subset)


#b=mvBACON(x_subset)
#x_subset[!b$subset,]

library(robustX);


#--Checking outliers
b=mvBACON(x_subset) ## Error due to singularity 

plot(b$dis,pch=19); 
abline(h=b$limit); 
id=1:length(b$dis); 
out=id[b$dis>b$limit]
points(id[out],b$dis[out],pch=19,col="red")
d=diag(b$cov)^-0.5; 
D=diag(d); 
R=D%*%b$cov%*%D 

#--PCA after removing outliers

x_new=x_subset[b$subset,]

pc=princomp(x_new, cor=T); summary(pc,loadings=T)
plot(pc) # Scree plot;
v=pc$sd^2; sv=sum(v); cs=cumsum(v)/sv; m=length(cs[cs<0.95])
if(m>1) {
  quartz()
  pairs(pc$scores[,1:m])
  }; v=pc$sd^2; sv=sum(v)
rslt=round(cbind(v,v/sum(v),cumsum(v)/sv),3)
colnames(rslt)=c("Variance","Prop. of Var.","Cum. Prop.") 
print(rslt)



