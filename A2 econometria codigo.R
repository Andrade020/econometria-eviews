library(wooldridge)
library(dplyr)
library(ggplot2)
library(plyr)
library("lmtest")
library(sandwich)
library(car)

###############
###              ##
##                          # Criado por Lucas Andrade
##  # #  ##  ###   ##
##    # # #   #  #  #
#######       ##  ##  ####   #
##  ###             ###
#    



data("mroz")

#a) 
dwtest(lwage~educ+age+kidslt6+ nwifeinc+ city,data=mroz)
bptest(m2)
vet= cbind(mroz$educ, mroz$age, mroz$kidslt6, mroz$nwifeinc, mroz$city)
summary(lm(lwage~exper+ exper**2,data=mroz))

X1= cbind(mroz$age, mroz$kidslt6, mroz$nwifeinc, mroz$city, mroz$educ)
X2= cbind(mroz$exper, mroz$exper**2)
Y1= cbind(mroz$lwage)
Y2= cbind(mroz$educ)
Y2= NULL

#ols regressao
summary(lm(Y1~Y2+X1))
#2sls 
summary(ivreg(Y1~X1| X1+X2))




##item f: 
X1= cbind(mroz$age, mroz$kidslt6, mroz$nwifeinc, mroz$city, mroz$fatheduc, mroz$motheduc)
X2= cbind(mroz$exper, mroz$exper**2)
Y1= cbind(mroz$lwage)
Y2= cbind(mroz$educ)
#2sls 
summary(ivreg(Y1~Y2+X1| X1+X2))


m1<-lm(lwage~educ,data=mroz)
m2<-lm(lwage~educ+age+kidslt6+ nwifeinc+ city,data=mroz)





m3<-lm(lwage~educ+roe+pcroe,data=mroz)
m4<-lm(lwage~educ+age+finance+consprod+roe+pcroe,data=mroz)

m1_se <- sqrt(diag(vcovHC(m1, type = "HC1")))
m2_se <- sqrt(diag(vcovHC(m2, type = "HC1")))
m3_se <- sqrt(diag(vcovHC(m3, type = "HC1")))
m4_se <- sqrt(diag(vcovHC(m4, type = "HC1")))

stargazer(m1,m2,m3,m4, se=list(m1_se,m2_se,m3_se,m4_se),
          omit.stat=c("adj.rsq","ser"),df = FALSE, no.space=TRUE, type="html", out="/Users/leonerd/Downloads/regq3.htm" )







abil2= htv$abil^2
regressaoHTV= lm(htv$educ~htv$motheduc+htv$fatheduc+ htv$abil+abil2, data=htv)
summary(regressaoHTV)
linearHypothesis(regressaoHTV, c("htv$motheduc= htv$fatheduc", "abil2=0.4"))
#install.packages("car")

linearHypothesis(regressaoHTV, c("htv$abil= 8*abil2", "htv$motheduc=0.2"))
data("gpa2")
View(gpa2)
sum(gpa2$sat*gpa2$black)/length(isTRUE(gpa2$sat==0))
data("ceosal1")
View(ceosal1)
?ceosal1
executivos= ceosal1$finance*ceosal1$salary
outros=ceosal1$salary*(1-ceosal1$finance)
View(cbind(executivos, outros))
uf=sum(executivos)/46
unf=sum(outros)/163 
sfin= sd(executivos)
snfin= sd(outros)
length(which(outros != 0))
regressaoCEO= lm(ceosal1$salary~finance, data=ceosal1)
summary(regressaoCEO)
bptest(regressaoCEO)
ceosal1$resi<-regressaoCEO$residuals
var.func <- lm(resi^2 ~ finance, data = ceosal1)
coeftest(regressaoCEO, vcov = vcovHC(regressaoCEO, "HC1"))

regs1=lm(ceosal1$lsalary~ceosal1$lsales)
summary(regs1)
bptest(regs1)
ceosal1$resi1<-regs1$residuals
var1.func <- lm(resi1^2 ~ lsales, data = ceosal1)
coeftest(regs1, vcov = vcovHC(regs1, "HC1"))


regs2=lm(ceosal1$lsalary~ceosal1$lsales+ ceosal1$indus, data=ceosal1)
summary(regs2)
bptest(regs2)
ceosal1$resi2<-regs2$residuals
var1.func <- lm(resi1^2 ~ lsales+ceosal1$indus , data = ceosal1)
coeftest(regs2, vcov = vcovHC(regs2, "HC1"))


regs3=lm(ceosal1$lsalary~ceosal1$lsales+ ceosal1$roe+ceosal1$pcroe , data=ceosal1)
summary(regs3)
bptest(regs3)
ceosal1$resi3<-regs3$residuals
var.func <- lm(resi1^2 ~ lsales+ roe+ pcroe, data = ceosal1)
coeftest(regs3, vcov = vcovHC(regs3, "HC1"))

regs4=lm(ceosal1$lsalary~ceosal1$lsales+ ceosal1$indus+ ceosal1$roe+ceosal1$pcroe , data=ceosal1)
summary(regs4)
bptest(regs4)
ceosal1$resi4<-regs4$residuals
var.func <- lm(resi1^2 ~ lsales+ indus+  roe+ pcroe, data = ceosal1)
coeftest(regs4, vcov = vcovHC(regs4, "HC1"))
cor(ceosal1$pcroe, ceosal1$roe)

x=ceosal1$pcroe
n=length(x)
sqrt((sd(x))^2*n/(n-1))
##################################################
   ###############                
###              ##
               ##
            ##  # #  ##  ###   ##
          ##    # # #   #  #  #
    #####       ##  ##  ####   #
        ##  ###             ###
          #    
##################################################




