#---IMPORTING LIBRARIES---

library(MASS)
library(GGally)
library(robustbase)
library(rrcov)

#---READING THE DATA---

x=read.csv("breastcancer.csv")
print(head(x))
x_subset=x[,3:12]
print(head(x_subset))

#---CHECKING FOR N/A VALUES---

print(anyNA(x_subset))

#---SUMMARY OF THE DATA---

print(summary(x_subset))

#---CREATING THE GGPAIRS SCATTERPLOTS---

ggpairs(x_subset)
windows()

#---IMPLEMENTING THE BACON ALGORITHM---
install.packages("robustX")
require(robustX)
matrix_x=as.matrix(x_subset)
output= mvBACON(matrix_x)

#---CONSTRUCTING A DATASET WITHOUT THE OUTLIERS---

x_subset2 = (x[output$subset, ])
x_subset_df <- as.data.frame(x_subset2)
write.csv(x_subset_df, file = "outputWithoutoutt.csv")


#----TESTING HOTELING'S T2 TEST WITH OUTLIERS---#

#---DIVIDING THE DATASET INTO TWO GROUPS---#


Mbreatcancer<-read.csv("Breastcancer(M).csv") 
Bbreatcancer<-read.csv("Breastcancer(B).csv")


#---APPLYING LOGARITHMIC TRANSFORMATION TO THE VARAIBLES---
Mbreatcancer_log=log(Mbreatcancer[, 3:12])
Bbreatcancer_log=log(Bbreatcancer[, 3:12])

#---EXTRACTING THE TEXTURE AND SYMMETRY VARIABLES---

x=Mbreatcancer_log[,c(2,9)]
y=Bbreatcancer_log[,c(2,9)]

x <- as.matrix(Mbreatcancer_log[, c(2, 9)])
y <- as.matrix(Bbreatcancer_log[, c(2, 9)])

#---APPLYING THE HOTELLING T2 ROBUST TEXT---
out1=T2.test(x,y)

#----TESTING HOTELING'S T2 TEST WITHOUT OUTLIERS---#
Mbreatcancer_without<-read.csv("DataWithoutoutlier(M).csv") 
Bbreatcancer_without<-read.csv("DataWithoutoutliers(B).csv")

#---APPLYING LOGARITHMIC TRANSFORMATION TO THE VARAIBLES---
Mbreatcancer_without_log=log(Mbreatcancer_without[, 3:12])
Bbreatcancer_without_log=log(Bbreatcancer_without[, 3:12])

#---HISTOGRAM OF THE NORMAL TEXTURE AD SYMMETRY---#
hist(Mbreatcancer_log[,2], main = paste("Texture"))
hist(Mbreatcancer_log[,9], main = paste("Symmetry"))

#---EXTRACTING THE TEXTURE AND SYMMETRY VARIABLES---
z=Mbreatcancer_without_log[,c(2,9)]
k=Bbreatcancer_without_log[,c(2,9)]

z <- as.matrix(Mbreatcancer_without_log[, c(2, 9)])
k <- as.matrix(Bbreatcancer_without_log[, c(2, 9)])

#---APPLYING THE HOTELLING T2 ROBUST TEXT---
out2=T2.test(z,k)