Sullivan.fun = function (rates,age=seq(start.age,open.age,5), sex='f') {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) and age-specific prevalence of disability (wx)
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  # 2) Calculating period life table functions
  # ax
  n <- c(diff(age), 1)
  ax <- 0.5 * n
  # This part of the code is only for calculations when the
  # starting age is zero.
  # Formulas are from Andreev & Kingkade (2015)
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
  #---------------------------------------------------------
  # probability of dying (qx) and surviving (px)
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
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  # 3) Person-years lived without disability
  Lx.health <- Lx*(1-wx)
  # 4) Healthy Life expectancy at age 0
  ex.health <- sum(Lx.health)/lx[1]
  return(ex.health)
}





# Defining the variables for the start of the first interval, the start of the open-aged interval, and the years
start.age = 65
open.age = 85
year1 = 1990; year2 = 2019
# To extract data from the HMD, you first need to register at www.mortality.org. Replace your username and password here:
  
  
library(HMDHFDplus)
library(tidyverse)

myusername <- "andrea.nigri@student.unisi.it"
mypassword <- "Dottorato17"
# Preparing the inputs for the arguments rates, pars1 and pars2
# 1) Selecting the country
country <- "ITA"
# 2) Extracting Female Life Tables from HMD
LTF<- readHMDweb (CNTRY = country,
                  item = "fltper_5x1",
                  username = myusername,
                  password = mypassword,
                  fixup = TRUE)
save(LTF, file = "LTF.RData")

# 3) Extracting Number of Deaths from HMD
Dx<- readHMDweb (CNTRY = country,
                 item = "Deaths_5x1",
                 username = myusername,
                 password = mypassword,
                 fixup = TRUE)
save(Dx, file = "Dx.RData")

# 4) Extracting Exposures from HMD
Nx<- readHMDweb(CNTRY = country,
                item = "Exposures_5x1",
                username = myusername,
                password = mypassword,
                fixup = TRUE)
save(Nx, file = "Nx.RData")

# 5) Getting the female death rates for ages >=65 in 1970 and 1990
mx1 <- filter(LTF,Year==1990 & Age >=65)$mx
mx2 <- filter(LTF,Year==2019 & Age >=65)$mx
# 6) We considered 85+ as the open age interval to match the disability data
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

wx1 <- c(7.79, 8.76, 9.17, 7.84, 6.80)/100
wx2 <- c(9.87, 11.58, 12.73, 12.13, 9.73)/100



# Making a single vector of mx followed
by wx - we need these as input for either
horiuchi or stepwise_replacement
mxwx1 <- c(mx1,wx1)
mxwx2 <- c(mx2,wx2)


Sullivan.fun(rates=mxwx1,age =seq(start.age,open.age,5), sex='f' )

#install.packages("DemoDecomp")
library(DemoDecomp)

HE_Decomp_sw <- stepwise_replacement(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2)


HE_Decomp_Cont <- horiuchi(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2,
  N=20)

library(reshape2)
library(data.table)


# 1) Making a matrix of the vectors with the age-specific
# mortality and morbidity contributions
HE_sw <- matrix(HE_Decomp_sw,nrow=(length(HE_Decomp_sw)/2),ncol=2,
                byrow=F)
HE_cont <- matrix(HE_Decomp_Cont,nrow=(length(HE_Decomp_Cont)/2),ncol=2,
                  byrow=F)
colnames(HE_sw) <- colnames(HE_cont) <- c("Mortality","Morbidity")
# 2) Creating a data frame with the matrices and adding
# a column with the beginning of the age interval
HE_sw_df <- mutate(as.data.frame(HE_sw),Age=c(seq(start.age,
                                                  open.age,5)))
HE_cont_df <- mutate(as.data.frame(HE_cont),Age=c(seq(start.age,
                                                      open.age,5)))
# We will construct barplots by using the package called
# "ggplot2". This package is included in the "tidyverse"
# package previously installed. As input, "ggplot2" requires # data in
#the long format. To change our data frames
# "HE_sw_df" and "HE_cont_df" from the wide to long format, # we use the function "melt" in the package "data.table"
# 3) Making the long data format
library(reshape2)
library(data.table)

HE_sw_res <- melt(HE_sw_df,id.vars="Age")
HE_cont_res <- melt(HE_cont_df,id.vars="Age")
colnames(HE_sw_res) <- c("Age","type","Contribution")
colnames(HE_cont_res) <- c("Age","type","Contribution")
# 4) Checking if the data is in the long format
head(HE_sw_res)
# Age type Contribution
# 65 Mortality 0.18388350
# 70 Mortality 0.21375435
# 75 Mortality 0.22878292
# 80 Mortality 0.16562088
# 85 Mortality 0.14367055
# 65 Morbidity -0.02677929
# 5) Creating the barplots
# create a vector with the start of first interval,
# the start of open-aged interval, & the length of interval
Age=seq(start.age,open.age,5)
# Fig.7.2-a) Barplot with the stepwise decomposition
Fig7.2a <- ggplot(data=HE_sw_res, aes(x=as.factor(Age), y=Contribution,
                                      fill=type))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(values=c("grey30", "grey60"))+
  scale_x_discrete(labels=c("65-69", "70-74", "75-79", "80-84", "85+"))+
  geom_hline(yintercept=0, linetype= "dashed", color = "gray50",
             size=0.5)+
  scale_y_continuous(breaks=seq(-0.1,0.3,by=0.05))+
  xlab("Age-group")+annotate("text", x=0.5, y=0.27, label= "a",
                             fontface =2)+ labs(fill = "Effect", size=8)+ theme_minimal()+
  theme(axis.text.x = element_text(size=8),axis.text.y = element_text
        (size=8), legend.text=element_text(size=8))+
  theme(axis.title.x = element_text(size=8),axis.title = element_text
        (size=8), legend.title=element_text(size=8))
# Fig.7.2- b) Barplot with the continuous change decomposition
Fig7.2b <- ggplot(data=HE_cont_res, aes(x=as.factor(Age),
                                        y=Contribution, fill =type))+
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_manual(values=c("grey30", "grey60"))+
  scale_x_discrete(labels=c("65-69", "70-74", "75-79", "80-84", "85+"))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray50", size=0.5)+
  scale_y_continuous(breaks=seq(-0.1,0.3,by=0.05))+
  xlab("Age-group")+annotate("text", x=0.5, y=0.27, label= "b",
                             fontface =2)+
  labs(fill = "Effect", size=8)+theme_minimal()+
  theme(axis.text.x = element_text(size=8),axis.text.y = element_text
        (size=8),legend.text=element_text(size=8))+
  theme(axis.title.x = element_text(size=8),axis.title = element_text
        (size=8),legend.title=element_text(size=8))
Fig7.2a
Fig7.2b
