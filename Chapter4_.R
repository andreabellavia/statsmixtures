# Load packages and open data


Packages <- c("readxl", "gWQS", "qgcomp","dplyr")
lapply(Packages, library, character.only = TRUE)
data2<- read_excel("your_path/dataset2xls.xls")

exposure<- names(data2[,3:16])



results1_adj<-gwqs(y ~ wqs+z1+z2+z3, 
                 mix_name = exposure, data = data2, 
                 q = 4, validation = 0.6, b = 100, 
                 b1_pos = T,rh = 100, 
                 family = "gaussian", seed = 123)

# Overall effect
summary(results1_adj)
# Figure 4.2
gwqs_barplot(results1_adj, tau=NULL, xlab="Weights")

# Book version
w_ord <- order(results1_adj$final_weights$Estimate)

mean_weight <- results1_adj$final_weights$Estimate[w_ord]
mix_name <- factor(results1_adj$final_weights$mix_name[w_ord], 
                   levels = results1_adj$final_weights$mix_name[w_ord])
data_plot <- data.frame(mean_weight = mean_weight, 
                        mix_name = mix_name)
ggplot(data_plot, aes(x = mix_name, y = mean_weight)) + 
  geom_bar(stat = "identity", color = "black") + theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(color='black',size = 11),
        axis.text.x = element_text(color='black',size = 11),
        legend.position = "none") + coord_flip() + ylab("Weight")+ xlab("") 

# Figure 4.3
a<-gwqs_boxplot(results1_adj)
a+theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(color='black',size = 11),
        axis.text.x = element_text(color='black',size = 11),
        legend.position = "none") +  ylab("Weight")+ xlab("") 
###
###
# qgcomp
qc <- qgcomp(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+
               x11+x12+x13+x14+z1+z2+z3,
             expnms=exposure, data2, 
             family=gaussian(), q=4)

# Figure 4.4
plot(qc)

qc.boot <- qgcomp.boot(y ~ x1+x2+x3+x4+x5+x6+x7+
                         x8+x9+x10+x11+x12+x13
                       +x14+z1+z2+z3,
                       expnms=exposure, data2, 
                       family=gaussian(), q=4, 
                       B=200, seed=123)
# Overall effect

summary(qc.boot)

# Figure 4.5
plot(qc.boot, legend=FALSE)


