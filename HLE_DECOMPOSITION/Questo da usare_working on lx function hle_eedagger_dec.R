
# Approccio combinato fatto ad Aarhus
hle.plus_edagg = function (rates,age=seq(start.age,open.age,5), sex='f') {
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  
  n <- c(diff(age), 1)
  ax <- 0.5 * n
  
  #---------------------------------------------------------
  if (age[1] == 0) {
    if (sex == 'm') {
      ax[1] <- ifelse(mx[1] >= 0.08307,0.29915,
                      ifelse(mx[1] < 0.023,
                             0.14929 - 1.99545 * mx[1],
                             0.02832 + 3.26021 * mx[1] ))}
    if (sex == 'f') {
      ax[1] <- ifelse(mx[1] >= 0.06891,0.31411,
                      ifelse(mx[1] < 0.01724,
                             0.14903 - 2.05527 * mx[1],
                             0.04667 + 3.88089 * mx[1] ))} }
  
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  # e0 <- sum(Lx)/lx[1] questa è life exp ed ottengo lo stesso valore 18.87073 della funzione usata nel paper con Josè
  
  Lx.health <- Lx*(1-wx)
  ex.health <- sum(Lx.health)/lx[1]
  
  #####################
  # e-dagger: 
  # see, Permanyer et al. Population Health Metrics (2022); paragraph Estimating health distributions.
  # The author suggest - Multiplying the lx column of the life table (showing the number of survivors at
  # age x ) by 1 − wx (the percent of population at age x not limited to carry out daily activities) 
  # we obtain lx' : the   number of healthy survivors at age x . This is the so-called  ‘morbidity curve’. 
  # From the lx and lx' columns we derive the standard dx and dx' distributions
  #####################
  
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  lx.health <- lx*(1-wx)
  dx.health <- lx.health * qx
  
  # Healty e-dagger at age x
  ed.health <- sum(dx.health*ex.health)/lx[1] 
  return(c(ex.health,ed.health))
}


################################



hle.fun = function (rates,age=seq(start.age,open.age,5), sex='f') {
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
 
  n <- c(diff(age), 1)
  ax <- 0.5 * n

  #---------------------------------------------------------
  if (age[1] == 0) {
    if (sex == 'm') {
      ax[1] <- ifelse(mx[1] >= 0.08307,0.29915,
                      ifelse(mx[1] < 0.023,
                             0.14929 - 1.99545 * mx[1],
                             0.02832 + 3.26021 * mx[1] ))}
    if (sex == 'f') {
      ax[1] <- ifelse(mx[1] >= 0.06891,0.31411,
                      ifelse(mx[1] < 0.01724,
                             0.14903 - 2.05527 * mx[1],
                             0.04667 + 3.88089 * mx[1] ))} }

  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  # e0 <- sum(Lx)/lx[1] questa è life exp ed ottengo lo stesso valore 18.87073 della funzione usata nel paper con Josè
  
  Lx.health <- Lx*(1-wx)
  ex.health <- sum(Lx.health)/lx[1]
  return(ex.health)
}



# sulla HLE lavoro sulla funzione di nopomuceno, su e-dagger lavoro sulla curva lx disability free come l'approccio di Iniaki, valutare cosa fare

H_edagger.fun = function (rates,age=seq(start.age,open.age,5), sex='f') {
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  
  n <- c(diff(age), 1)
  ax <- 0.5 * n
  
  #---------------------------------------------------------
  if (age[1] == 0) {
    if (sex == 'm') {
      ax[1] <- ifelse(mx[1] >= 0.08307,0.29915,
                      ifelse(mx[1] < 0.023,
                             0.14929 - 1.99545 * mx[1],
                             0.02832 + 3.26021 * mx[1] ))}
    if (sex == 'f') {
      ax[1] <- ifelse(mx[1] >= 0.06891,0.31411,
                      ifelse(mx[1] < 0.01724,
                             0.14903 - 2.05527 * mx[1],
                             0.04667 + 3.88089 * mx[1] ))} }
  
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  lx.health <- lx*(1-wx)
  dx.health <- lx.health * qx
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx.health[i+1]*n[i] + ax[i]*dx.health[i] }
  Lx[length(mx)] <- lx.health[length(mx)]/mx[length(mx)]
  Lx.health <- Lx
  
  ex.health <- sum(Lx.health)/lx.health[1]
  ed.health <- sum(dx.health*ex.health)/lx[1] # check and consider if this might be /lx.health[1]
  
  return(ed.health)
}



# Defining the variables for the start of the first interval, the start of the open-aged interval, and the years
start.age = 65
open.age = 85
year1 = 1990; year2 = 2019
# To extract data from the HMD, you first need to register at www.mortality.org. Replace your username and password here:


library(HMDHFDplus)
library(tidyverse)

load("LTF.RData")
load("Dx.RData")
load("Ex.RData")


mx1 <- filter(LTF,Year==1990 & Age >=65)$mx
mx2 <- filter(LTF,Year==2019 & Age >=65)$mx

Dx1 <- filter(Dx,Year==1990)$Total
Dx2 <- filter(Dx,Year==2019)$Total
Nx1 <- filter(Nx,Year==1990)$Total
Nx2 <- filter(Nx,Year==2019)$Total
# y is the last position in the mx1 and mx2 vectors
y = length(seq(start.age,open.age,5))
mx1[y] <- sum(Dx1[19:24])/sum(Nx1[19:24])
mx2[y] <- sum(Dx2[19:24])/sum(Nx2[19:24])
mx1 <- mx1[1:y]
mx2 <- mx2[1:y]

# wx1 <- c(7.79, 8.76, 9.17, 7.84, 6.80)/100
# wx2 <- c(9.87, 11.58, 12.73, 12.13, 9.73)/100

#########################################################################################################
#########################################################################################################
##
## ATTENZIONE: Nel mio esempio dei dati qui su, ho usato la prevalenza solo di una malattia
## invece si usa la prevalenza della disabilità che solitamente ha tassi piu grandi
## qui sotto infatti uso quelli USA dall'esempio Nepomuceno Vaan Raalte.
## In questo modo e-dagger da valori che sembrano ok
##
#########################################################################################################
#########################################################################################################

wx1 <- c(0.3000,0.3657,0.4552, 0.5285,
         0.6822)
wx2 <- c(0.3056,0.3831,0.4552, 0.5424,
         0.6441)



mxwx1 <- c(mx1,wx1)
mxwx2 <- c(mx2,wx2)

hle.plus_edagg_fun_oct_22(rates=mxwx1,age =seq(start.age,open.age,5), sex='f')
hle.fun(rates=mxwx1,age =seq(start.age,open.age,5), sex='f' )
H_edagger.fun(rates=mxwx1,age =seq(start.age,open.age,5), sex='f' )

#install.packages("DemoDecomp")
library(DemoDecomp)


HE_Decomp_Cont <- horiuchi(
  func=hle.fun,
  pars1 = mxwx1,
  pars2 = mxwx2,
  N=20)

library(reshape2)
library(data.table)



HE_cont <- matrix(HE_Decomp_Cont,nrow=(length(HE_Decomp_Cont)/2),ncol=2,
                  byrow=F)
 colnames(HE_cont) <- c("Total Mortality","Cancer Morbidity")
# 2) Creating a data frame with the matrices and adding
# a column with the beginning of the age interval
HE_cont_df <- mutate(as.data.frame(HE_cont),Age=c(seq(start.age,
                                                      open.age,5)))


HE_cont_res <- melt(HE_cont_df,id.vars="Age")
colnames(HE_cont_res) <- c("Age","type","Contribution")

Age=seq(start.age,open.age,5)

ggplot(data=HE_cont_res, aes(x=as.factor(Age),
                                        y=Contribution, fill =type))+
  geom_bar(stat = "identity", position = "stack")




HE_Decomp_Cont <- horiuchi(
  func=H_edagger.fun,
  pars1 = mxwx1,
  pars2 = mxwx2,
  N=20)



HE_cont <- matrix(HE_Decomp_Cont,nrow=(length(HE_Decomp_Cont)/2),ncol=2,
                  byrow=F)
colnames(HE_cont) <- c("Total Mortality","Cancer Morbidity")
# 2) Creating a data frame with the matrices and adding
# a column with the beginning of the age interval
HE_cont_df <- mutate(as.data.frame(HE_cont),Age=c(seq(start.age,
                                                      open.age,5)))


HE_cont_res <- melt(HE_cont_df,id.vars="Age")
colnames(HE_cont_res) <- c("Age","type","Contribution")

Age=seq(start.age,open.age,5)

ggplot(data=HE_cont_res, aes(x=as.factor(Age),
                             y=Contribution, fill =type))+
  geom_bar(stat = "identity", position = "stack")

