# Required packages and open data

Packages <- c("readxl","bkmr","ggplot2","car","rms","mgcv")
lapply(Packages, library, character.only = TRUE)

# Non-linearities
set.seed(12345)
cov <- data.frame(id = 1:1000,
                  x1 = rnorm(1000, 1, 3),
                  x2 = rnorm(1000, 3, 4))
cov$y <- (cov$x1^2)+cov$x2 + runif(1000, -0.3, 0.3)

cov$x1cat<-ntile(cov$x1,4)
lin<-glm(y~x1,data=cov)
cat<-glm(y~as.factor(x1cat),data=cov)
rcs<-glm(y~rcs(x1,3),data=cov)


cov$predcat<-predict.glm(cat, type="response")
cov$predlin<-predict.glm(lin, type="response")
cov$predsp<-predict.glm(rcs, type="response")

# Figure 5.1
ggplot(cov, aes(x=x1)) + 
  geom_point(aes(y = y))+
  geom_line(aes(y = predcat), color = "red", size=1.2) + 
  geom_line(aes(y = predlin), color="blue", size=1.2) + 
  geom_line(aes(y = predsp), color="darkgreen", size=1.2) +ylab("Predicted Y")+xlab("X1")+
  theme_bw() 


###
###
###
# BKMR

data2<- read_excel("your_path/dataset2xls.xls")
data2[,3:16] <- scale(data2[,3:16])


mixture<-as.matrix(data2[,3:16])
y<-data2$y
covariates<-as.matrix(data2[,17:19])

set.seed(10)
knots100  <- fields::cover.design(mixture, 
                                  nd = 50)$design



temp <-  kmbayes(y=y, Z=mixture, X=covariates, 
                 iter=1000, verbose=FALSE, 
                 varsel=TRUE, knots=knots100)



# Figure 5.2
sel<-seq(0,1000,by=1)
TracePlot(fit = temp, par = "beta", sel=sel, 
          xlab = "Iteration", ylab = "Parameter Value")

#Figure 5.3
sel<-seq(100,1000,by=1)
TracePlot(fit = temp, par = "beta", sel=sel, 
          xlab = "Iteration", ylab = "Parameter Value")

# Table 5.1
ExtractPIPs(temp)

# Table 5.2
hier <-  kmbayes(y=y, Z=mixture, X=covariates, 
                 iter=1000, verbose=FALSE, varsel=TRUE,  
                 knots=knots100, 
                 groups=c(1,1,2,2,2,1,1,1,1,1,1,1,1,1))
ExtractPIPs(hier)

# Generate cross-sections to plot
pred.resp.univar <- PredictorResponseUnivar(fit = temp, sel=sel, 
                                            method="approx")

pred.resp.bivar  <- PredictorResponseBivar(fit = temp,  min.plot.dist = 1, sel=sel, 
                                           method="approx")

pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, 
                                                       Z = mixture, both_pairs = TRUE, qs = c(0.25, 0.5, 0.75))

risks.overall <- OverallRiskSummaries(fit = temp, qs = seq(0.25, 0.75, by = 0.05), 
                                      q.fixed = 0.5, method = "approx",sel=sel)

risks.singvar <- SingVarRiskSummaries(fit = temp, qs.diff = c(0.25, 0.75),
                                      q.fixed = c(0.25, 0.50, 0.75), method = "approx")

risks.int <- SingVarIntSummaries(fit = temp, qs.diff = c(0.25, 0.75),
                                 qs.fixed = c(0.25, 0.75))

# Figure 5.4
ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, 
                             ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + ylab("h(x)") +xlab("x") + theme_bw()+ facet_wrap(~ variable) 

# Figure 5.5
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd,  
                          ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_hline(aes(yintercept=0), linetype="dashed", color="gray60") +  theme_bw()+
  geom_pointrange(position = position_dodge(width = 1)) +
  coord_flip() + theme(legend.position="none")+scale_x_discrete(name="") +
  scale_y_continuous(name="Estimate") 

# Figure 5.6
ggplot(pred.resp.bivar[pred.resp.bivar$variable1 =='x6' 
                       & pred.resp.bivar$variable2 =='x12', ], aes(z1, z2, fill = est)) + 
  geom_raster() + 
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"), name="Estimate") +  theme_bw()+
  xlab("X6") +
  ylab("X12")  

# Figure 5.7
pred.resp.bivar.levels6_12<-pred.resp.bivar.levels[pred.resp.bivar.levels$variable1 =='x6' 
                                                   & pred.resp.bivar.levels$variable2 =='x12', ,] 
 

ggplot(pred.resp.bivar.levels6_12, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity",fill=NA) + 
  scale_color_manual(labels = c("0.25", "0.5","0.75"), 
                     values = c("red", "darkgreen","blue")) +
  labs(title = "", x = "X6", y = "Estimate", color = "Quantiles of X12")+
  theme_bw()+theme(
    legend.position = c(.95, .05),
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.background = element_rect(size = 0.5, colour = 1)
  )

# Figure 5.8
ggplot(risks.int, aes(variable, est, ymin = est - 1.96*sd, 
                      ymax = est + 1.96*sd)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  geom_hline(yintercept = 0, lty = 2, col = "brown") + coord_flip()+  theme_bw()+
  xlab("Variable") +
  ylab("Estimate") 

# Figure 5.9
ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, 
                          ymax = est + 1.96*sd)) +  
  geom_hline(yintercept=00, linetype="dashed", color="gray60") + 
  geom_pointrange() + scale_y_continuous(name="estimate") +  theme_bw()+
  xlab("Quantile") +
  ylab("Estimate") 



###
###
###
# GAM


data2<- read_excel("your_path/dataset2xls.xls")

library(mgcv)
library(visreg)

mod_lm <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+
                s(x8)+s(x9)+s(x10)+s(x11)+s(x12)+s(x13)+s(x14) + z1 + z2 + z3, data = data2,select = TRUE)
summary(mod_lm)

mod_lm1 <- gam(y ~ s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+
                 s(x8)+s(x9)+s(x10)+s(x11)+s(x12)+s(x13)+s(x14) + z1 + z2 + z3, data = data2)
a<-as.data.frame(
  cbind(
    round(summary(mod_lm1)$s.table[,4],3),
    round(summary(mod_lm)$s.table[,4],3)
  )
)

# Table 5.3
colnames(a)<-c("p","p2")
a

# Figure 5.10
plot(mod_lm, shade=TRUE, select=6, scale=0, xlab="X6",ylab="Estimate")


ggplot()+ylab("Estimated coefficient")+xlab("Corr (X1,X2)")+
  theme_bw()+scale_x_continuous(limits = c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(limits = c(-1.5, 1.5),breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5))


