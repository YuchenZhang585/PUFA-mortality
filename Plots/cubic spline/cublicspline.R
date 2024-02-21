library(smoothHR)
library(survival)
library(rms)
library(Hmisc) 
library(dplyr)
library(data.table)

new_mydata <- fread("/Users/yuchen/Desktop/UKBiobank/Data/update/data_f.csv", header=TRUE, sep=",")
new_mydata = new_mydata %>% filter(omega_ratio <40)
#new_mydata = new_mydata %>% select(-c(Lost_follow_up, Death_record_origin))
new_mydata= new_mydata %>% select(ID, omega_ratio, age, TDI, eth_group_new, sex, center, BMI, Smoking_status, Alcohol_status, IPAQ_activity, 
                                  comorb, time_to_death_months, death, death_cancer, death_cvd)

new_mydata$Alcohol_status = as.factor(new_mydata$Alcohol_status)
new_mydata$Smoking_status =as.factor(new_mydata$Smoking_status)
new_mydata$IPAQ_activity = as.factor(new_mydata$IPAQ_activity)
new_mydata$sex = as.factor(new_mydata$sex)
new_mydata$center = as.factor(new_mydata$center)
new_mydata$comorb = as.factor(new_mydata$comorb)

# Determine the optimal number of nodes knots,

for (i in 3:7) {
    fit <- cph(Surv(time_to_death_months, death) ~ rcs(omega_ratio,i) + age + TDI + eth_group_new + sex + center + BMI + 
                   Smoking_status + Alcohol_status + IPAQ_activity + comorb, data = new_mydata, x=TRUE)
    tmp <- extractAIC(fit)
    if(i == 3) {AIC = tmp[2]; nk = 3}
    if(tmp[2] < AIC) {AIC = tmp[2]; nk = i} 
}

# The above code is based on AIC Criteria filter minimum AIC Corresponding knots. The results show that the optimal knots by 4.

################ All cause  #####################

# Start fitting the model

fit = cph(Surv(time_to_death_months, death) ~ rcs(omega_ratio,4) + age + TDI + eth_group_new + sex + center + BMI + 
              Smoking_status + Alcohol_status + IPAQ_activity + comorb, data = new_mydata, x=TRUE)
anova(fit)
# The results show nonlinear P value = 0.0183, It indicates that there is nonlinearity .

## p-non-linear
p <-round(anova(fit)[,3],3)

# define the reference number
refvalue <- 4

# Package the data , And specify the reference value .
ddist<-datadist(new_mydata)
ddist$limits$omega_ratio[2]<-refvalue
options(datadist="ddist")

pred_HR<-Predict(fit,omega_ratio,ref.zero=TRUE,fun=exp)


tiff(file.path("/Users/yuchen/Desktop/all_cubic.tiff"), units="in", width=4.5, height=6, res=300)

#  Sets the background color of the density curve 
grey <- "#D3D3D3"
#  Draw the left and right double axes baseplot
par(mar = c(5, 4, 4, 4) + 0.3)
par(xpd=NA)

ylim.bot <- min(pred_HR$lower)
ylim.top <- max(pred_HR$upper)

#  Draw a density chart first to avoid obscuring the line chart below 
dens <- density(new_mydata$omega_ratio) #  Calculate density 
plot(dens$x,dens$y, col=ggplot2::alpha(grey,0.5), type="l", xlab = "", ylab = "",xaxt="n",yaxt="n")
polygon(dens$x,dens$y,col = ggplot2::alpha(grey,0.5),border = ggplot2::alpha(grey,0.5)) #  Color transparent anti covering line 
axis(side=4, at = pretty(range(dens$y))[-length(pretty(range(dens$y)))])
mtext("Fraction of population (Density)", side=4, line=2)


par(new=TRUE) #  New canvas 
plot(pred_HR$omega_ratio,pred_HR$yhat,
     xlab = "Ratio of omega-6/omega-3 PUFAs",ylab = "Hazard Ratio (ref = 4)",
     type = "l", ylim = c(0.5,5.1),
     col="red",lwd=2) 
lines(pred_HR$omega_ratio,pred_HR$lower,lty=2,lwd=1.5)
lines(pred_HR$omega_ratio,pred_HR$upper,lty=2,lwd=1.5)
lines(x=range(pred_HR$omega_ratio),y=c(1,1),lty=3,col="grey40",lwd=1.3) 
points(as.numeric(refvalue),1,pch=16,cex=1)
title(main = "All-cause Mortality")

#  Draw a legend , Pay attention to nonlinearity p The value is in the variable p Position in 
#legend("topright",
#       paste0("P-overall ",
#              ifelse(anova(fit)[1,3] < 0.001,"< 0.001",round(anova(fit)[1,3],3)),
#              "\nP-non-linear = ",
#              ifelse(anova(fit)[2,3] < 0.001,"< 0.001",round(anova(fit)[2,3],3))),
#       bty="n",cex=0.8)

legend("topright",lty = c(1,2),col = c("red","black"),
       c("Estimation","95% CI"),
       bty="n",cex=0.8)
dev.off()


################ Cancer caused  #####################

# Start fitting the model

fit2 = cph(Surv(time_to_death_months, death_cancer) ~ rcs(omega_ratio,4) + age + TDI + eth_group_new + sex + center + BMI + 
               Smoking_status + Alcohol_status + IPAQ_activity + comorb, data = new_mydata, x=TRUE)
anova(fit2)
# The results show nonlinear P value = 0.1219, It indicates that there is no nonlinearity .

## p-non-linear
p <-round(anova(fit2)[,3],3)

# omega_ratio      17.47     3   0.0006
# Nonlinear        5.60     2   0.0609

# define the reference number
refvalue <- 4

# Package the data , And specify the reference value .
ddist<-datadist(new_mydata)
ddist$limits$omega_ratio[2]<-refvalue
options(datadist="ddist")

pred_HR2<-Predict(fit2,omega_ratio,ref.zero=TRUE,fun=exp)


tiff(file.path("/Users/yuchen/Desktop/cancer_cubic.tiff"), units="in", width=4.5, height=6, res=300)
#  Sets the background color of the density curve 
grey <- "#D3D3D3"
#  Draw the left and right double axes baseplot
par(mar = c(5, 4, 4, 4) + 0.3)
par(xpd=NA)

ylim.bot <- min(pred_HR2$lower)
ylim.top <- max(pred_HR2$upper)

#  Draw a density chart first to avoid obscuring the line chart below 
dens <- density(new_mydata$omega_ratio) #  Calculate density 
plot(dens$x,dens$y, col=ggplot2::alpha(grey,0.5), type="l", xlab = "", ylab = "",xaxt="n",yaxt="n")
polygon(dens$x,dens$y,col = ggplot2::alpha(grey,0.5),border = ggplot2::alpha(grey,0.5)) #  Color transparent anti covering line 
axis(side=4, at = pretty(range(dens$y))[-length(pretty(range(dens$y)))])
mtext("Fraction of population (Density)", side=4, line=2)


par(new=TRUE) #  New canvas 
plot(pred_HR2$omega_ratio,pred_HR2$yhat,
     xlab = "Ratio of omega-6/omega-3 PUFAs",ylab = "Hazard Ratio (ref = 4)",
     type = "l", ylim = c(0.5,5.1),
     col="red",lwd=2) 
lines(pred_HR2$omega_ratio,pred_HR2$lower,lty=2,lwd=1.5)
lines(pred_HR2$omega_ratio,pred_HR2$upper,lty=2,lwd=1.5)
lines(x=range(pred_HR2$omega_ratio),y=c(1,1),lty=3,col="grey40",lwd=1.3) 
points(as.numeric(refvalue),1,pch=16,cex=1)
title(main = "Cancer Mortality")

#  Draw a legend , Pay attention to nonlinearity p The value is in the variable p Position in 
#legend("topright",
#       paste0("P-overall ",
#              ifelse(anova(fit2)[1,3] < 0.001,"< 0.001",round(anova(fit)[1,3],3)),
#              "\nP-non-linear = ",
#              ifelse(anova(fit2)[2,3] < 0.001,"< 0.001",round(anova(fit)[2,3],3))),
#       bty="n",cex=0.8)

legend("topright",lty = c(1,2),col = c("red","black"),
       c("Estimation","95% CI"),
       bty="n",cex=0.8)
dev.off()

################ Cvd caused  #####################

# Start fitting the model

fit3 = cph(Surv(time_to_death_months, death_cvd) ~ rcs(omega_ratio,4) + age + TDI + eth_group_new + sex + center + BMI +
               Smoking_status + Alcohol_status + IPAQ_activity + comorb, data = new_mydata, x=TRUE)
anova(fit3)
# The results show nonlinear P value = 0.130, It indicates that there is no nonlinearity .

## p-non-linear
p <-round(anova(fit3)[,3],3)

#omega_ratio     26.94      3   <.0001
#Nonlinear       8.63      2   0.0134

# define the reference number
refvalue <- 4

# Package the data , And specify the reference value .
ddist<-datadist(new_mydata)
ddist$limits$omega_ratio[2]<-refvalue
options(datadist="ddist")

pred_HR3<-Predict(fit3,omega_ratio,ref.zero=TRUE,fun=exp)


tiff(file.path("/Users/yuchen/Desktop/cvd_cubic.tiff"), units="in", width=4.5, height=6, res=300)
#  Sets the background color of the density curve 
grey <- "#D3D3D3"
#  Draw the left and right double axes baseplot
par(mar = c(5, 4, 4, 4) + 0.3)
par(xpd=NA)

ylim.bot <- min(pred_HR3$lower)
ylim.top <- max(pred_HR3$upper)

#  Draw a density chart first to avoid obscuring the line chart below 
dens <- density(new_mydata$omega_ratio) #  Calculate density 
plot(dens$x,dens$y, col=ggplot2::alpha(grey,0.5), type="l", xlab = "", ylab = "",xaxt="n",yaxt="n")
polygon(dens$x,dens$y,col = ggplot2::alpha(grey,0.5),border = ggplot2::alpha(grey,0.5)) #  Color transparent anti covering line 
axis(side=4, at = pretty(range(dens$y))[-length(pretty(range(dens$y)))])
mtext("Fraction of population (Density)", side=4, line=2)


par(new=TRUE) #  New canvas 
plot(pred_HR3$omega_ratio,pred_HR3$yhat,
     xlab = "Ratio of omega-6/omega-3 PUFAs",ylab = "Hazard Ratio (ref = 4)",
     type = "l", ylim = c(0.5,5.1),
     col="red",lwd=2) 
lines(pred_HR3$omega_ratio,pred_HR3$lower,lty=2,lwd=1.5)
lines(pred_HR3$omega_ratio,pred_HR3$upper,lty=2,lwd=1.5)
lines(x=range(pred_HR3$omega_ratio),y=c(1,1),lty=3,col="grey40",lwd=1.3) 
points(as.numeric(refvalue),1,pch=16,cex=1)
title(main = "CVD Mortality")

#  Draw a legend , Pay attention to nonlinearity p The value is in the variable p Position in 
#legend("topright",
#       paste0("P-overall ",
#              ifelse(anova(fit)[1,3] < 0.001,"< 0.001",round(anova(fit)[1,3],3)),
#              "\nP-non-linear = ",
#              ifelse(anova(fit)[2,3] < 0.001,"< 0.001",round(anova(fit)[2,3],3))),
#       bty="n",cex=0.8)

legend("topright",lty = c(1,2),col = c("red","black"),
       c("Estimation","95% CI"),
       bty="n",cex=0.8)


dev.off()


