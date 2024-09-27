# Load packages and open data
Packages <- c("readxl","glmnet","dplyr","car")
lapply(Packages, library, character.only = TRUE)
data2<- read_excel("your_path/dataset2xls.xls")

# Single regression (EWAS)
lm1 <- lm(y ~ x1 + z1 + z2 + z3, data = data2)
lm2 <- lm(y ~ x2 + z1 + z2 + z3, data = data2)
lm3 <- lm(y ~ x3 + z1 + z2 + z3, data = data2)
lm4 <- lm(y ~ x4 + z1 + z2 + z3, data = data2)
lm5 <- lm(y ~ x5 + z1 + z2 + z3, data = data2)
lm6 <- lm(y ~ x6 + z1 + z2 + z3, data = data2)
lm7 <- lm(y ~ x7 + z1 + z2 + z3, data = data2)
lm8 <- lm(y ~ x8 + z1 + z2 + z3, data = data2)
lm9 <- lm(y ~ x9 + z1 + z2 + z3, data = data2)
lm10 <- lm(y ~ x10 + z1 + z2 + z3, data = data2)
lm11 <- lm(y ~ x11 + z1 + z2 + z3, data = data2)
lm12 <- lm(y ~ x12 + z1 + z2 + z3, data = data2)
lm13 <- lm(y ~ x13 + z1 + z2 + z3, data = data2)
lm14 <- lm(y ~ x14 + z1 + z2 + z3, data = data2)

l3<-as.vector(round(summary(lm3)$coefficients[2,c(1,4)],3))
l4<-as.vector(round(summary(lm4)$coefficients[2,c(1,4)],3))
l5<-as.vector(round(summary(lm5)$coefficients[2,c(1,4)],3))
l12<-as.vector(round(summary(lm12)$coefficients[2,c(1,4)],3))
l13<-as.vector(round(summary(lm13)$coefficients[2,c(1,4)],3))

# Table 3.1
res<-as.data.frame(rbind(l3,l4,l5,l12,l13))
rownames(res)<-c("x3","x4","x5", "x12","x13")
colnames(res)<-c("Estimate","p.value")

res

# Multiple regression

lmall <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14 + z1 + z2 + z3, data = data2)

# Table 3.2

res_sing<-c(round(summary(lm1)$coefficients[2,1],3),
            round(summary(lm2)$coefficients[2,1],3),
            round(summary(lm3)$coefficients[2,1],3),
            round(summary(lm4)$coefficients[2,1],3),
            round(summary(lm5)$coefficients[2,1],3),
            round(summary(lm6)$coefficients[2,1],3),
            round(summary(lm7)$coefficients[2,1],3),
            round(summary(lm8)$coefficients[2,1],3),
            round(summary(lm9)$coefficients[2,1],3),
            round(summary(lm10)$coefficients[2,1],3),
            round(summary(lm11)$coefficients[2,1],3),
            round(summary(lm12)$coefficients[2,1],3),
            round(summary(lm13)$coefficients[2,1],3),
            round(summary(lm14)$coefficients[2,1],3))
psing<-c(round(summary(lm1)$coefficients[2,4],3),
         round(summary(lm2)$coefficients[2,4],3),
         round(summary(lm3)$coefficients[2,4],3),
         round(summary(lm4)$coefficients[2,4],3),
         round(summary(lm5)$coefficients[2,4],3),
         round(summary(lm6)$coefficients[2,4],3),
         round(summary(lm7)$coefficients[2,4],3),
         round(summary(lm8)$coefficients[2,4],3),
         round(summary(lm9)$coefficients[2,4],3),
         round(summary(lm10)$coefficients[2,4],3),
         round(summary(lm11)$coefficients[2,4],3),
         round(summary(lm12)$coefficients[2,4],3),
         round(summary(lm13)$coefficients[2,4],3),
         round(summary(lm14)$coefficients[2,4],3))

lall<-as.matrix(round(summary(lmall)$coefficients[2:15,c(1,4)],3))

res<-as.data.frame(cbind(lall,res_sing,psing))
rownames(res)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12","x13","x14")
colnames(res)<-c("Estimate - multiple","p.value - multiple", "Estimate - single","p.value - single")
res


# Table 3.3
vif(lmall)

###
### 
# Penalized regression

set.seed(123)   
X<-as.matrix(data2[,3:16])
Y<-data2$y
lambdas_to_try <- 10^seq(-3, 3, length.out = 100)

# Ridge

# Figure 3.5
res <- glmnet(X, Y, alpha = 0, lambda = lambdas_to_try, standardize = TRUE)
plot(res, xvar = "lambda", xlab="log(lambda)")
legend("bottomright", lwd = 1, col = 1:14, legend = colnames(X), cex = .7)

ridge_cv <- cv.glmnet(X, Y, alpha = 0, 
                      lambda = lambdas_to_try,
                      standardize = TRUE, 
                      nfolds = 1000)


# Figure 3.6
plot(ridge_cv, xlab="log(lambda)")

# lowest lambda
lambda_cv_min <- ridge_cv$lambda.min
# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.1se


# Table 3.4
model_cv <- glmnet(X, Y, alpha = 0, 
                   lambda = lambda_cv, standardize = TRUE)


res<-as.matrix(round(model_cv$beta,3))
colnames(res)<-"Estimate"
res

# LASSO

# Figure 3.7
res_lasso <- glmnet(X, Y, alpha = 1, lambda = lambdas_to_try, standardize = TRUE)
plot(res_lasso, xvar = "lambda", xlab="log(lambda)")
legend("bottomright", lwd = 1, col = 1:14, legend = colnames(X), cex = .7)

# Figure 3.8
lasso_cv <- cv.glmnet(X, Y, alpha = 1, 
                      lambda = lambdas_to_try,
                      standardize = TRUE, 
                      nfolds = 1000)
plot(lasso_cv, xlab="log(lambda)")

# lowest lambda
lambda_cv_min_lasso <- lasso_cv$lambda.min
# Best cross-validated lambda
lambda_cv_lasso <- lasso_cv$lambda.1se


# Table 3.5
model_cv_lasso <- glmnet(X, Y, alpha = 1, 
                         lambda = lambda_cv_lasso, 
                         standardize = TRUE)


res<-as.matrix(round(model_cv_lasso$beta,3))
colnames(res)<-"Estimate"
res


# Elastic Net

# Table 3.6
enet_cv <- cv.glmnet(X, Y, alpha = 0.7, 
                     lambda = lambdas_to_try,
                     standardize = TRUE, 
                     nfolds = 1000)

# Best cross-validated lambda
lambda_cv_enet <- enet_cv$lambda.1se

res<-as.matrix(round(model_cv_enet$beta,3))
colnames(res)<-"Estimate"
res


# Example: how to adjust for covariates
enet_cv_adj <- cv.glmnet(X, Y, alpha = 0.7, 
                         lambda = lambdas_to_try,
                         standardize = TRUE, nfolds = 1000, 
                         penalty.factor=c(rep(1,ncol(X)-3),0,0,0))

model_cv_enet_adj <- glmnet(X, Y, alpha = 0.7, lambda = lambda_cv_enet, standardize = TRUE,penalty.factor=c(rep(1,ncol(X) - 3),0,0,0))




