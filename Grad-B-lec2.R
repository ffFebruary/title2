#setwd("C:/Users/stakagi/OneDrive/LectureFiles/GradB/np-dens-reg")
setwd("/Users/45tkg/metrics/OneDrive/LectureFiles/GradB/np-dens-reg")
Data <- read.csv("wage.csv", header=TRUE, na.string=".")


################################################
# Nonparametric Regression Function Estimation
################################################
library(KernSmooth)
library(np)

Data <- read.csv("wage.csv", header=TRUE, na.string=".")
Data <- subset( Data, complete.cases( Data ) )


y <- Data$lwage
x <- Data$educ
#
reg.1 <- lm(y~x)
reg.2 <- lm(y~x+I(x^2))
#
# Fitted values of the quadratic regression
#
x2 <- seq(min(x),max(x),length=30)
y2 <- reg.2$coef[1] + x2*reg.2$coef[2] + x2*x2 * reg.2$coef[3]
#
h      <- dpill(x, y)                              # Direct Prug-in Estimate
ll.1   <- locpoly(x, y, drv = 0, degree=1, bandwidth = h)
ll.2   <- locpoly(x, y, drv = 0, degree=1, bandwidth = h*10)
ll.3   <- locpoly(x, y, drv = 0, degree=1, bandwidth = h*.40)
#
par(mfrow=c(1,1))
plot(y~x, pch =1, cex=0.3, ylim = c(6.2,7.4))
abline(reg.1, lwd = 3, lty=3, col = "blue")   # parametric
lines(x2, y2, lwd = 3, lty=3,  col="magenta") # parametric
lines(ll.1, lwd = 3, lty=1, col = "red")      # nonparametric
lines(ll.2, lwd = 3, lty=1, col = "purple")   # nonparametric
lines(ll.3, lwd = 2, lty=2, col = "black")    # nonparametric
#
#
##########################################
# Estimates with confidence intervals
##########################################
h      <- dpill(Data$educ, Data$lwage )   # Direct Prug-in Estimate
#
bw0 <- npreg(lwage~educ,data=Data,  regtype="ll",bws = h)
bw1 <- npreg(lwage~educ,data=Data,  regtype="ll",bws = h*0.2)
bw2 <- npreg(lwage~educ,data=Data,  regtype="ll",bws = h*1.5)
bw3 <- npreg(lwage~educ,data=Data,  regtype="ll",bws = h*100.0)
#
par(mfrow=c(2,2))
plot(bw0, 
     plot.errors.method="bootstrap", 
     plot.errors.center="bias-corrected", 
     plot.errors.type="quantiles", 
     plot.errors.quantiles = c(0.025,0.975),
     plot.par.mfrow = FALSE, col = "red",ylim=c(6.0,7.2))
plot(bw1, 
     plot.errors.method="bootstrap", 
     plot.errors.center="bias-corrected", 
     plot.errors.type="quantiles", 
     plot.errors.quantiles = c(0.025,0.975),
     plot.par.mfrow = FALSE, col = "red",ylim=c(6.0,7.2))
plot(bw2, 
     plot.errors.method="bootstrap", 
     plot.errors.center="bias-corrected", 
     plot.errors.type="quantiles", 
     plot.errors.quantiles = c(0.025,0.975),
     plot.par.mfrow = FALSE, col = "red",ylim=c(6.0,7.2))
plot(bw3, 
     plot.errors.method="bootstrap", 
     plot.errors.center="bias-corrected", 
     plot.errors.type="quantiles", 
     plot.errors.quantiles = c(0.025,0.975),
     plot.par.mfrow = FALSE, col = "red",ylim=c(6.0,7.2))
#
##########################################
# Smoothing-out effect of Crossvalidation
##########################################
library(np)
set.seed(3864)
nobs <- length(Data$lwage)
Data$dx   <- rbeta(nobs,1,2)
Data$D    <- ((1:nobs)%%20==0)
#Data$D    <- runif(nobs) > .5
#Data$D    <- summary(lm(D~lwage,Data))$resid
bw.wage <- npregbw(lwage~factor(D)+educ+dx,data=Data, tol=.1, ftol=.1)
bw.wage


bw.wage2 <- npregbw(lwage~factor(black)+factor(south)+factor(sibs)+factor(brthord)+meduc+feduc+IQ+educ+tenure+hours,data=Data, tol=.1, ftol=.1)
bw.wage2


##########################################
# Consistent Specification Test of the Linear Model
##########################################
#
#m1 <- lwage~factor(black)+factor(south)+meduc+IQ+educ+tenure
# ols.r <- lm(m1, x=TRUE, y=TRUE, data=Data)
# X <- data.frame(factor(black), factor(south), meduc, IQ, educ, tenure)
# npcmstest(model=ols.r, xdat = X, ydat = lwage, tol=.1, ftol=.1)
#
# Result: Test Statistic �e Jn �f: -0.3096014 P Value: 0.19048
# Not Rejected ! The linear model is "a correct specification"!
#
attach(Data)
ols.0 <- lm(lwage~black+educ, x=TRUE, y=TRUE, data=Data)
X     <- data.frame(factor(black), educ)
npcmstest(model=ols.0, xdat = X, ydat = lwage, tol=.01, ftol=.01)



##########################################
# Significant Test of black dummy
##########################################
#
# bw.b <- npregbw(m1, tol=.1, ftol=.1)
# npsigtest(bws=bw.b, index=1)
#
# Significance Tests
# P Value:
# factor(black) < 2.22e-16 ***
# factor(south) < 2.22e-16 ***
# meduc 0.070175 .
# IQ < 2.22e-16 ***
# educ < 2.22e-16 ***
# tenure 0.002506 **
#
# To see Estimation Results
#
# r.np <- npreg(m1, bws=bw.b)
# summary(r.np)
#
bw.s <- npregbw(lwage~factor(black)+factor(south)+meduc+IQ+educ+tenure)#, tol=.1, ftol=.1)
npsigtest(bws=bw.s, index=2)
npsigtest(bws=bw.s, index=1)
npsigtest(bws=bw.s)
r.np <- npreg(lwage~black+educ, bws=bw.s)
summary(r.np)
npplot(bws=bw.s, plot.errors.boot.num = 99, plot.errors.method="bootstrap")
npplot(bws=bw.s, plot.errors.boot.num = 99, plot.errors.method="bootstrap",view = "fixed",theta=0,phi=10)
r.lm <- lm(lwage~black+educ, Data)
summary(r.lm)

library(np)
bw.s1 <- npregbw(lwage~educ)#, tol=.1, ftol=.1)
npplot(bws=bw.s1)
npplot(bws=bw.s1, plot.errors.boot.num = 99, plot.errors.method="bootstrap")

##########################################################
#  Growth Empirics
##########################################################
#
#install.packages("np")
library(np)
data("oecdpanel")
attach(oecdpanel)
summary(oecdpanel)
#
#
#
##########################################################
# Growth rate densities over sample periods
##########################################################
#
par(mfrow=c(1,1))
fhat <- npcdens(growth ~ year, tol = 0.1, ftol = 0.1, data = oecdpanel)
summary(fhat)
plot(fhat, view = "fixed", main = "", theta = 300, phi = 50)
plot(fhat, view = "rotate", main = "", theta = 250, phi = 30)
#
##########################################################
# Growth rate density conditional on the initial GDP
##########################################################
#
fhat <- npcdens(growth ~ initgdp, tol = 0.1, ftol = 0.1, data = oecdpanel)
summary(fhat)
plot(fhat, view = "fixed", main = "", theta = 300, phi = 50)
plot(fhat, view = "rotate", main = "", theta = 250, phi = 30)
#
############################################################
# Growth rate densities: OECD members v.s. non-OECD members
############################################################
#
g.nono <- subset(oecdpanel$growth, oecdpanel$oecd==0)
g.oecd <- subset(oecdpanel$growth, oecdpanel$oecd==1)
plot(density(g.nono), xlim = c(-0.2,0.2), ylim=c(0,25), lty=1, col="red")
par(new=TRUE)
plot(density(g.oecd), xlim = c(-0.2,0.2), ylim=c(0,25), lty=3, col="blue")
#
#
#
#
#
#
m1 <- growth~factor(oecd)+factor(year)+popgro+inv+initgdp+humancap
m2 <- growth~factor(oecd)+factor(year)+popgro+inv+initgdp+I(initgdp^2)+I(initgdp^3)+I(initgdp^4)+humancap+I(humancap^2)+I(humancap^3)
#
##########################################################
# Linear Model
##########################################################
#
ols.r <- lm(m2,x=TRUE,y=TRUE)
#
#
##########################################################
# Consitent Specifiction Test of the Linear Model
##########################################################
#
X <- data.frame(factor(oecd),factor(year),popgro,inv,initgdp,humancap)
npcmstest(model=ols.r, xdat = X, ydat = growth, tol=.1, ftol=.1)
#
# Reult: Test Statistic �eJn�f: 2.486947 P Value: < 2.22e-16 ***
#
#
#
#
##########################################################
# Significant Test of OECD dummy 
#(This test is free from specification errors on the regression functional form)##########################################################
#
bw <- npregbw(m1, tol=.1, ftol=.1)
npsigtest(bws=bw, index=1)     # Significant test for the first variable only
npsigtest(bws=bw)              # Significant test for all variables
#
#
# To see Estimation Results of Nonparametric Regression
#
r.np <- npreg(m1, bws=bw)
summary(r.np)
#
#
#
#
##########################################################
# Partially Linear Model
##########################################################
#
bw <- npplregbw(growth~factor(oecd)+factor(year)+popgro+inv|initgdp+humancap, tol=.1, ftol=.1)
#
# To see Estimation Results Graphically
#
npplot(bws=bw)
npplot(bws=bw, plot.errors.boot.num = 99,plot.errors.method="bootstrap") # Warning: this command is extremely time-consuming.
#
# To see Estimation Results
#
r.plm <- npplreg(growth~factor(oecd)+factor(year)+popgro+inv|initgdp+humancap, bws=bw)
summary(r.plm)
#
# To see Estimation Results of Parametric Parts
#
coef(r.plm)
coef(r.plm, errors = TRUE)
#
#
#
#
##########################################################
# Consitent Specifiction Test of the Partially Linear Model
##########################################################
#
ols.r <- lm(m2,y=TRUE,x=TRUE)
uhat <- resid(ols.r)
X <- data.frame(initgdp,humancap)
npcmstest(model=ols.r, xdat=X, ydat=uhat, tol=.1, ftol=.1)
#
# Test Statistic �eJn�f: 1.311450 P Value: 0.015038 *
#
detach(oecdpanel)



(2p+2)/(2p+k+2) = (2*1+2)/(2*1+1+2) = 4/5
(2p+2)/(2p+k+2) = (2*1+2)/(2*1+2+2) = 4/6
(2p+2)/(2p+k+2) = (2*1+2)/(2*1+3+2) = 4/7
(2p+2)/(2p+k+2) = (2*1+2)/(2*1+4+2) = 4/8

n^{-1} versus n^{-4/5} k=1
n^{-1} versus n^{-4/6} k=2
n^{-1} versus n^{-4/7} k=3
n^{-1} versus n^{-4/8} k=4


x  <- c(1:10000)
y0 <- log10(x)
y1 <- log10(x^(4/5))
y2 <- log10(x^(4/6))
y3 <- log10(x^(4/7))
y4 <- log10(x^(4/8))

matplot(x,cbind(y0,y1,y2,y3,y4),type="l")
abline(h=2,col="black")
abline(h=log10(500),col="black")
abline(h=log10(1000),col="black")



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
Data <- read.csv("wage.csv", header=TRUE, na.string=".")
#
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

###################################################
Data  <- Data[,c("educ","exper","tenure","KWW","IQ","black","age","south","urban","lwage")]
model <- lwage~educ+exper+tenure+KWW+IQ+factor(black)+age+factor(south)+factor(urban)
###################################################
tree.rp <- train(model, data = Data, method = 'rpart', tuneGrid = expand.grid(cp = seq(0.01, 0.05, 0.001)))

###################################################
# Regression Tree
###################################################
# minsplit (20):  the minimum number of observations in a node when spliting
# minbucket (20/3): the minimum number of obs. in any terminal <leaf> node.
# cp    (0.01):    complexity parameter (small=complex model)
r0.1    <- rpart(model, minsplit=30,  Data)
rpart.plot(r0.1, uniform=T)
r0.2    <- rpart(model, minbucket=5, Data)
rpart.plot(r0.2, uniform=T)
r0.3    <- rpart(model, cp=0.005, Data)
rpart.plot(r0.3, uniform=T)

r1    <- rpart(model, Data)
rpart.plot(r1, uniform=T)                         # --> Figure 4
#r1$cptable
#print(r1)
r1    <- rpart(model, cp=0.032, Data)
rpart.plot(r1, uniform=T)                         
###################################################
library(partykit)
plot(as.party(r1))                                # --> Figure 5
###################################################
# Prediction by Regression Tree
###################################################
point1 <- data.frame(KWW=30, educ=12, tenure=3, black=0,
                       exper=3, IQ=105, age=30, south=1,  urban=1)
predict(r1, newdata=point1)
###################################################
# Regression Tree: Estimation and Prediction
###################################################
#trn <- Data[1:800,]            # training data (used for estimation)
#tst <- Data[801:935,]          # test data (used for out-of-sample prediction)
set.seed(3864)
idx <- sample(1:935,800,replace=F)
trn <- Data[idx,]              # training data (used for estimation)
tst <- Data[-idx,]             # test data (used for out-of-sample prediction)
#
# cp selection (smaller cp value = more complex model)
#
tree.rp <- train(model, data = trn, method = 'rpart', tuneGrid = expand.grid(cp = seq(0.01, 0.05, 0.001)))
#
rtp <- rpart(model, data = trn)
tf  <- predict(rtp, newdata = trn)  # in-sample prediction
tp  <- predict(rtp, newdata = tst)  # out-of-sample prediction
#
rtp1 <- rpart(model, cp=0.026, data = trn)
tf1  <- predict(rtp1, newdata = trn)  # in-sample prediction
tp1  <- predict(rtp1, newdata = tst)  # out-of-sample prediction
#
rtp2 <- rpart(model, cp=0.001, data = trn)
tf2  <- predict(rtp2, newdata = trn)  # in-sample prediction
tp2  <- predict(rtp2, newdata = tst)  # out-of-sample prediction
#
rlp <- lm(model, data = trn)
lf  <- predict(rlp, newdata = trn)  # in-sample
lp  <- predict(rlp, newdata = tst)  # out-of-sample
#
cor(trn$lwage, tf)     # R^2 of regression tree for in-sample prediction
cor(trn$lwage,tf1)     # R^2 of regression tree for in-sample prediction
cor(trn$lwage,tf2)     # R^2 of regression tree for in-sample prediction
cor(trn$lwage, lf)     # R^2 of linear model for in-sample prediction
#
cor(tst$lwage,tp)      # R^2 of regression tree for out-of-sample prediction
cor(tst$lwage,tp1)     # R^2 of regression tree for out-of-sample prediction
cor(tst$lwage,tp2)     # R^2 of regression tree for out-of-sample prediction
cor(tst$lwage,lp)      # R^2 of linear model for out-of-sample prediction


#############################################################
#############################################################
#
#                   Random Forest
#
#############################################################
#############################################################
set.seed(3864)
idx <- sample(1:935, 800, replace = FALSE)
trn <- Data[idx,]                 # training data (used for estimation)
tst <- Data[-idx,]                # test data (used for out-of-sample prediction
# Partitioning the explanatory variables and the dependent variable
exp.var <- trn[,1:9]
dep.var <- trn[,10]

# nodesize: Minimum size of terminal nodes .
# ntree:    Number of trees to grow 
# mtry:     Number of variables randomly sampled as candidates at each split

set.seed(3864)
rf.trn <- train(model, data = trn, tuneLength=6, method = 'rf')

set.seed(3864)
result1 <- randomForest(x=exp.var, y=dep.var, importance=TRUE, ntree=500, mtry=2)
set.seed(3864)
result2 <- randomForest(x=exp.var, y=dep.var, importance=TRUE, ntree=500, mtry=6)
set.seed(3864)
result3 <- randomForest(x=exp.var, y=dep.var, importance=TRUE, nodesize=3, ntree=500, mtry=2)
set.seed(3864)
result4 <- randomForest(x=exp.var, y=dep.var, importance=TRUE, maxnode=50, ntree=500, mtry=2)
#



print(result1)
print(result2)
print(result3)
print(result4)
# Importance of explanatory variables
print( importance( result1 ))
print( importance( result2 ))
print( importance( result3 ))
print( importance( result4 ))



rf1.in   <- predict(result1, newdata = trn)  # in-sample prediction
rf1.out  <- predict(result1, newdata = tst)  # out-of-sample prediction
rf2.in   <- predict(result2, newdata = trn)  # in-sample prediction
rf2.out  <- predict(result2, newdata = tst)  # out-of-sample prediction
rf3.in   <- predict(result3, newdata = trn)  # in-sample prediction
rf3.out  <- predict(result3, newdata = tst)  # out-of-sample prediction
rf4.in   <- predict(result4, newdata = trn)  # in-sample prediction
rf4.out  <- predict(result4, newdata = tst)  # out-of-sample prediction

cor(trn$lwage,rf1.in)   # R^2 of regression tree for in-sample prediction
cor(trn$lwage,rf2.in)   # R^2 of regression tree for in-sample prediction
cor(trn$lwage,rf3.in)   # R^2 of regression tree for in-sample prediction
cor(trn$lwage,rf4.in)   # R^2 of regression tree for in-sample prediction
cor(tst$lwage,rf1.out)  # R^2 of regression tree for out-of-sample prediction
cor(tst$lwage,rf2.out)  # R^2 of regression tree for out-of-sample prediction
cor(tst$lwage,rf3.out)  # R^2 of regression tree for out-of-sample prediction
cor(tst$lwage,rf4.out)  # R^2 of regression tree for out-of-sample prediction


############################################################
rn.trn <- train(model, data = trn, tuneLength=6, method = 'ranger')
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5), splitrule = "extratrees", min.node.size = c(1, 5, 10, 15, 20))
rn.trn <- train(model, data = trn, tuneGrid=rf_grid, method = 'ranger')

result <- ranger(lwage~., data=trn, mtry=2, min.node.size=10, write.forest=TRUE, importance = "impurity")
importance(result)
################################################################






##############################################################################
##############################################################################
library(hdm)
data(pension)

#Y <- pension$tw
#D <- pension$p401
Y <- pension$net_tfa
D <- pension$e401
# Only main effects (toy example)
X <- model.matrix(~ -1 + i2 + i3 + i4 + i5 + i6 + i7 + a2 + a3 + a4 + a5 +
                fsize + hs + smcol + col + marr + twoearn + db + pira + hown, 
                data = pension)
#X <- model.matrix(~ -1 + i2 + i3 + i4 + i5 + i6 + i7, data = pension)
X <- model.matrix(~ -1 + inc + age +
                fsize + hs + smcol + col + marr + twoearn + db + pira + hown, 
                data = pension)





thetahat <- rep(0,3)
 
  # = OLS estimate = #
  OLS <- coef(lm(Y~D+X))[2]
  thetahat[1] <- OLS
 
  # = Naive DML = #
   # = Compute ghat = #
    model <- randomForest(X,Y,mtry=2) #maxnodes = 20)
    K     <- predict(model,X)
   # = Compute mhat = #
    modeld <- randomForest(X,factor(D),mtry=2) #,maxnodes = 20)
    H      <- predict(modeld,X)
   # = compute vhat as the residuals of the second model = #
    V <- D-(as.numeric(H)-1)
   # = Compute DML theta = #
    theta_nv <- mean(V*(Y-K))/mean(V*D)
    thetahat[2] <- theta_nv
 
  # = Cross-fitting DML = #
  # = Split sample = #
  N  <- length(Y)
  I  <- sort(sample(1:N,N/2))
  IC <- setdiff(1:N,I)

#  rf.IC <- train(X[IC,],Y[IC], tuneLength=6, method = 'rf')
#  rf.I  <- train(X[I,],Y[I], tuneLength=6, method = 'rf')

  # = compute ghat on both sample = #
  model1 <- randomForest(X[IC,],Y[IC],mtry=2)
  model2 <- randomForest(X[I,],Y[I],mtry=2)
  G1 <- predict(model1,X[I,])
  G2 <- predict(model2,X[IC,])
 
  # = Compute mhat and vhat on both samples = #
  modeld1 <- randomForest(X[IC,],factor(D[IC]), mtry=2)#,maxnodes = 10)
  modeld2 <- randomForest(X[I,],factor(D[I]), mtry=2)#,maxnodes = 10)
  M1 <- predict(modeld1,X[I,])
  M2 <- predict(modeld2,X[IC,])
  V1 <- D[I]-(as.numeric(M1)-1)
  V2 <- D[IC]-(as.numeric(M2)-1)
 
  # = Compute Cross-Fitting DML theta
  theta1 <- mean(V1*(Y[I]-G1))/mean(V1*D[I])
  theta2 <- mean(V2*(Y[IC]-G2))/mean(V2*D[IC])
  theta_cf <- mean(c(theta1,theta2))
  thetahat[3] <- theta_cf



mDv <- randomForest(X,factor(D), mtry=2)#,maxnodes = 10)
mYv <- randomForest(X,Y, mtry=2)#, maxnodes = 10)
MDv <- predict(mDv,X)
Gv  <- predict(mYv,X)
Vv  <- D - (as.numeric(MDv)-1)
Wv  <- Y - Gv

#theta_cf
Uv <- (Wv-D*theta_cf-Vv)
#
V2 <- sum(Vv^2)
UV <- sum(Uv^2*Vv^2)
#
SE.theta <- sqrt(UV/V2/V2)
thetahat
SE.theta
##############################################################################
##############################################################################






DBML <- function(nvar){

thetahat <- rep(0,3)
 
  #####################
  # (1)  OLS estimate 
  #####################
  OLS <- coef(lm(Y~D+X))[2]
  thetahat[1] <- OLS
 
  ###################
  # (2) Naive DML  
  ###################
   ##################
   #  Compute khat  
   ##################
    model <- randomForest(X,Y,mtry=nvar) #maxnodes = 20)
    K     <- predict(model,X)
   ##################
   #  Compute mhat  
   ##################
    modeld <- randomForest(X,factor(D),mtry=nvar) #,maxnodes = 20)
    H      <- predict(modeld,X)
   ####################################################
   # Compute vhat as the residuals of the second model 
   ####################################################
    V <- D-(as.numeric(H)-1)
   ####################################################
   #  Compute DML theta 
   ####################################################
    theta_nv <- mean(V*(Y-K))/mean(V*D)
    thetahat[2] <- theta_nv
  
  ############################
  # (3) Cross-fitting DML 
  ############################
  #  Split sample 
  ############################
  N  <- length(Y)
  I  <- sort(sample(1:N,N/2))
  IC <- setdiff(1:N,I)

   # Tuning the "mtry" parameter
   #  rf.IC <- train(X[IC,],Y[IC], tuneLength=6, method = 'rf')
   #  rf.I  <- train(X[I,],Y[I], tuneLength=6, method = 'rf')

  #######################################
  #  compute khat on both sample 
  #######################################
   model1 <- randomForest(X[IC,],Y[IC],mtry=nvar)
   model2 <- randomForest(X[I,], Y[I], mtry=nvar)
   G1 <- predict(model1,X[I,])
   G2 <- predict(model2,X[IC,])
 
  ##########################################
  #  Compute mhat and vhat on both samples 
  ##########################################
   modeld1 <- randomForest(X[IC,],factor(D[IC]), mtry=nvar)#,maxnodes = 10)
   modeld2 <- randomForest(X[I,], factor(D[I]),  mtry=nvar)#,maxnodes = 10)
   M1 <- predict(modeld1,X[I,])
   M2 <- predict(modeld2,X[IC,])
   V1 <- D[I]-(as.numeric(M1)-1)
   V2 <- D[IC]-(as.numeric(M2)-1)
 
  ##########################################
  #  Compute Cross-Fitting DML theta
  ##########################################
   theta1 <- mean(V1*(Y[I]-G1))/mean(V1*D[I])
   theta2 <- mean(V2*(Y[IC]-G2))/mean(V2*D[IC])
   theta_cf <- mean(c(theta1,theta2))
  thetahat[3] <- theta_cf

  ##########################################
  # Standard error estimation of theta
  ##########################################
   mDv <- randomForest(X,factor(D), mtry=nvar)#,maxnodes = 10)
   mYv <- randomForest(X,Y, mtry=nvar)#, maxnodes = 10)
   MDv <- predict(mDv,X)
   Gv  <- predict(mYv,X)
   Vv  <- D - (as.numeric(MDv)-1)
   Wv  <- Y - Gv
    #
   Uv  <- (Wv-D*theta_cf-Vv)
    #
   V2  <- sum(Vv^2)
   UV  <- sum(Uv^2*Vv^2)
    #
   SE.theta <- sqrt(UV/V2/V2)
  return(list(Theta=thetahat,SE=SE.theta))
}
##############################################################################
##############################################################################



apply(matrix(2:10,1,9), 2, DBML)



##############################################################################
##############################################################################
mtry =  6
nvar <- 6
DBML(nvar)

> thetahat
[1]  9122.092  7125.294 11792.611
> SE.theta
[1] 1075.961

##############################################################################
##############################################################################
mtry =  2
nvar <- 2
DBML(nvar)
$Theta
[1] 9122.092 6545.180 5936.201

$SE
[1] 1193.569
##############################################################################
##############################################################################
mtry =  10
nvar <- 10
DBML(nvar)

$Theta
[1]  9122.092  7744.282 10738.193

$SE
[1] 1238.971
