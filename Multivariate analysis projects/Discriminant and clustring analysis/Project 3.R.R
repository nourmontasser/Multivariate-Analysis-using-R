#---IMPORTING LIBRARIES---
install.packages("clue")
library(MASS)
library(GGally)
library(robustbase)
library(rrcov)
library(gridExtra)
library(ggplot2)
library(nnet)

#--READING THE DATA

x=read.csv("breastcancer.csv")
print(head(x))
x_subset=x[,3:12]
print(head(x_subset))
class=x[,2]
#--CHECKING FOR N/A VALUES

print(anyNA(x_subset))

#--SUMMARY OF THE DATA

print(summary(x_subset))

#--CREATING THE GGPAIRS SCATTERPLOTS

ggpairs(x_subset)
#quartz()
windows()


# ----DECLARING R2 FUNCTION----
R2=function(x,clusters,k=3){
  n=nrow(x); tss=var(x); tss=(n-1)*sum(diag(tss));
  wss=0
  for(j in 1:k){
    cj=x[clusters==j,]; nj=nrow(cj); vj=var(cj); wssj=0
    if(is.matrix(cj)) 
      wssj=(nj-1)*sum(diag(vj)); wss=wss+wssj
  }
  r2=1-wss/tss; cat("R2 = ",r2,"\n") 
  return(r2)
}


minWeightBipartiteMatching <- function(clusteringA, clusteringB) {
  require(clue)
  idsA <- unique(clusteringA)  # distinct cluster ids in a
  idsB <- unique(clusteringB)  # distinct cluster ids in b
  nA <- length(clusteringA)  # number of instances in a
  nB <- length(clusteringB)  # number of instances in b
  if (length(idsA) != length(idsB) || nA != nB) {
    stop("number of clusters or number of instances do not match")
  }
  
  nC <- length(idsA)
  tupel <- c(1:nA)
  
  # computeing the distance matrix
  assignmentMatrix <- matrix(rep(-1, nC * nC), nrow = nC)
  for (i in 1:nC) {
    tupelClusterI <- tupel[clusteringA == i]
    solRowI <- sapply(1:nC, function(i, clusterIDsB, tupelA_I) {
      nA_I <- length(tupelA_I)  # number of elements in cluster I
      tupelB_I <- tupel[clusterIDsB == i]
      nB_I <- length(tupelB_I)
      nTupelIntersect <- length(intersect(tupelA_I, tupelB_I))
      return((nA_I - nTupelIntersect) + (nB_I - nTupelIntersect))
    }, clusteringB, tupelClusterI)
    assignmentMatrix[i, ] <- solRowI
  }
  
  # optimization
  result <- solve_LSAP(assignmentMatrix, maximum = FALSE)
  attr(result, "assignmentMatrix") <- assignmentMatrix
  return(result)
}





# ----STANDARDIZING DATA TO AVOID SCALE INVARIANCE ISSUES----
x_scale=scale(x_subset, center=TRUE, scale=TRUE)
x_scale=as.data.frame(x_scale)

# ----HIERARCHICAL CLUSTERING----
# Euclidean and Ward Method
d = dist(x_scale, method = "euclidean")
hc = hclust(d, method="ward.D2")
plot(hc)
rect.hclust(hc, k=2, border="red")
clusters=cutree(hc, k=2)
clusters = as.factor(clusters)
match.label=minWeightBipartiteMatching(clusters,class)
r2=R2(x_scale,clusters,2)
table(clusters,class)
t=table(clusters,class)
er2=100*(sum(t)-sum(diag(t)))/nrow(x)
cat("Error Rate =",er2,"%\n")


# Euclidean and Complete Linkage 

d = dist(x_scale, method = "euclidean")
hc = hclust(d, method="complete")
plot(hc)
rect.hclust(hc, k=2, border="red")
clusters=cutree(hc, k=2)
clusters = as.factor(clusters)
match.label=minWeightBipartiteMatching(clusters,class)
r2=R2(x_scale,clusters,2)
table(clusters,class)
t=table(clusters,class)
er2=100*(sum(t)-sum(diag(t)))/nrow(x)
cat("Error Rate =",er2,"%\n")

# manhattan and Ward Method 

d = dist(x_scale, method = "manhattan")
hc = hclust(d, method="ward.D2")
plot(hc)
rect.hclust(hc, k=2, border="red")
clusters=cutree(hc, k=2)
clusters = as.factor(clusters)

r2=R2(x_scale,clusters,2)
table(clusters,class)
table(clusters)
match.label=minWeightBipartiteMatching(clusters,class)
t=table(clusters,class)
er2=100*(sum(t)-sum(diag(t)))/nrow(x)
cat("Error Rate =",er2,"%\n")


# manhattan and Complete Linkage 

d = dist(x_scale, method = "manhattan")
hc = hclust(d, method="complete")
plot(hc)
rect.hclust(hc, k=2, border="red")
clusters=cutree(hc, k=2)
clusters = as.factor(clusters)
match.label=minWeightBipartiteMatching(clusters,class)
r2=R2(x_scale,clusters,2)
table(clusters)
t=table(clusters,class)
er2=100*(sum(t)-sum(diag(t)))/nrow(x)
cat("Error Rate =",er2,"%\n")




# ----K-MEANS----
# L-Curve
wss = (nrow(x_scale)-1)*sum(apply(x_scale,2,var))
for (i in 2:15) {
  wss[i] = sum(kmeans(x_scale,centers=i)$withinss)} 
plot(wss, type="b", pch=19, xlab="k", ylab="WSS", main="The L-Curve")




k=5; 
kmc = kmeans(x_scale, k);
clusters=kmc$cluster
plot(x_scale, col = kmc$cluster)
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1)
clusters;
table(clusters,class)
cat("Cluster Centers:\n"); 
kmc$centers
r2=R2(x_scale, clusters, k)
t=table(clusters,class)
er2=100*(sum(t)-sum(diag(t)))/nrow(x)
cat("Error Rate =",er2,"%\n")

