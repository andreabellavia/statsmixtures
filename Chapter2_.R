
# Required packages and open data
Packages <- c("readxl", "qgraph", "corrplot", "factoextra","dplyr")
lapply(Packages, library, character.only = TRUE)
data2 <- read_excel("your_path/dataset2xls.xls")
 
# Figure 2.1
par("mfcol"=c(1, 2))
hist(exp(data2$x1), col="orange", main="",xlab="X1")
hist(data2$x1, col="green3", main="",xlab="log(X1)")


# Figure 2.2
par("mfcol"=c(2, 1))
boxplot(data2$x1, data2$x2,data2$x3,data2$x4, names=c('X1', 'X2', 'X3', 'X4'))
data3<-data2
data3[,3:17]<-scale(data3[,3:17])
boxplot(data3$x1, data3$x2,data3$x3,data3$x4, names=c('X1', 'X2', 'X3', 'X4'))



# Figure 2.3

par("mfcol"=c(1, 3))
plot(data2$x3, data2$x5, main = "",
     xlab = "X5", ylab = "X3")

plot(data2$x1, data2$x5, main = "",
     xlab = "X5", ylab = "X1")
plot(data2$x13, data2$x12, main = "",
     xlab = "X12", ylab = "X13")


# Table 2.1
cor.matrix <- cor(data2[,3:16], method="spearman")
cor.matrix

# Figure 2.4
par("mfcol"=c(1, 1))


corrplot(cor.matrix,
         method="circle",
         order = "hclust",
         addrect =10,
         tl.pos = "l",
         tl.col = "black", tl.cex=0.95, col=COL2("RdYlBu"))


# Figure 2.5
qgraph(cor.matrix, graph = "pcor", layout = "spring")
  
###
###
# PCA

set.seed(123)   
data2<- read_excel("your_path/dataset2xls.xls")

# define exposures and outcome
data2[,3:16] <- scale(data2[,3:16])

X<-as.matrix(data2[,3:16])
Y<-data2$y

# fit PCA
fit <- prcomp(X, center=TRUE, scale=TRUE)

# Table 2.2

a<-summary(fit)
b<-as.data.frame(t(a$importance))
b[,1]<-sqrt(b[,1])
names(b)[names(b) == "Standard deviation"] <- "Eigenvalues"
b[,2:3]

# Figure 2.8
plot(fit,type="lines",main="")

# Table 2.3
rawLoadings_3<-fit$rotation[,1:3]
rotatedLoadings_3<- a<-varimax(rawLoadings_3) $loadings
a<-as.vector(rotatedLoadings_3)
b<-as.data.frame(round(cbind(a[1:14],a[15:28],a[29:42]),3))
colnames(b)<-c("PC1","PC2","PC3")
rownames(b)<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9",
               "X10","X11","X12","X13","X14")
b
