# 
# install.packages('DemoDecomp', repos =
#                    'http://cran.us.r-project.org')

library('HMDHFDplus')
library('tidyverse')
library('data.table')
library(tidyverse)
library(reshape2)

load("Final.RData")

unique(Final$CNT)

Sullivan.fun = function (rates,age=AGE, sex=Gender) {
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
    if (sex == 'Male') {
      ax[1] <- ifelse(mx[1] >= 0.08307,0.29915,
                      ifelse(mx[1] < 0.023,
                             0.14929 - 1.99545 * mx[1],
                             0.02832 + 3.26021 * mx[1] ))}
    if (sex == 'Female') {
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




#only if ages are not equal spaced
start.age <- 60
open.age <- 85

library(tidyverse)
library(dplyr)


AGE=c(seq(start.age,open.age,5))


year1 = 2005; year2 = 2016;G="Male";country="ITA"

str(Final)
DEC_RES=function(start.age, open.age, Gender,
                 year1, year2, country){

  filter(Final,Year==year1 & Ageclass >=start.age & Gender == G & CNT == country)
  Final$Gender <- as.factor(Final$Gender)
  Final$Gender
  # 5) Getting the female death rates for ages >=65 in 1970 and 1990
mx1 <- filter(Final,Year==year1 & Ageclass >=start.age & Gender == G & CNT == country)$mx
mx2 <- filter(Final,Year==year2 & Ageclass >=start.age & Gender == G & CNT == country)$mx
# 6) We considered 85+ as the open age interval to match the disability data


wx1 <- filter(Final,Year==year1 & Ageclass >=start.age & Gender == G & CNT == country)$Prevalence
wx2 <- filter(Final,Year==year2 & Ageclass >=start.age & Gender == G & CNT == country)$Prevalence
# 6) We considered 85+ as the open age interval to match the disability data


# Making a single vector of mx followed by wx - we need these as input for either horiuchi or stepwise_replacement
mxwx1 <- c(mx1,wx1)
mxwx2 <- c(mx2,wx2)


library(DemoDecomp)
HE_Decomp_Cont <- horiuchi(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2,
  sex=Gender,  
  N=100)


HE_cont <- matrix(HE_Decomp_Cont,nrow=(length(HE_Decomp_Cont)/2),ncol=2,
                  byrow=F)
colnames(HE_cont) <- c("Mortality","Disability")
# 2) Creating a data frame with the matrices and adding
# a column with the beginning of the age interval

HE_cont_df <- mutate(as.data.frame(HE_cont),Age=AGE)

# 3) Making the long data format
HE_cont_res <- melt(HE_cont_df,id.vars="Age")
colnames(HE_cont_res) <- c("Age","type","Contribution")
# 4) Checking if the data is in the long format
HE_cont_res$CNT <- country
return(HE_cont_res)}


AGE=c(seq(start.age,open.age,5))

 DEC_RES(start.age =60,
         open.age = 85,year1 = year1,
         year2 = year2, Gender=G,country="ITA")

# 
  country <- c("AUT","BEL","DNK","EST","FIN","FRACNP","GRC",
               "IRL","ITA","LUX","PRT","ESP","SWE","CZE",
               "DEUTNP","HUN","LVA","LTU", "NLD","POL","SVK",
               "SVN","GBR_NP","BGR")
 

#  
#  country <- c("AUT","BEL","DNK","FIN","FRACNP",
#               "DEUTNP","GRC","IRL","ITA","LUX",
#               "NLD", "PRT","ESP", "SWE", "GBR_NP")
# # 


Res <- list()
for (i in 1:length(country)) {
  cnt <- country[i]
  
  
  Res[[cnt]] <-  DEC_RES(start.age =start.age,
                         open.age = open.age,year1 =year1 ,
                         year2 = year2, Gender=G,country=cnt)

}

HE_cont_res <-  do.call(rbind.data.frame, Res)
rownames(HE_cont_res) <- NULL



head(HE_cont_res)
unique(HE_cont_res$CNT)
TOT <- ggplot(data=HE_cont_res %>% filter(CNT!="GBR_NP"), aes(x=as.factor(Age),
                             y=Contribution, fill =type))+
  geom_bar(stat = "identity", position = "stack")+coord_flip()+facet_wrap(~CNT)+
  #scale_fill_manual(values=c("grey30", "grey60"))+
  scale_x_discrete(labels=c("60-64","65-69","70-74","75-79","80-84","85+"))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray50", size=0.5)+ggtitle("Male population") + xlab("Age")+
  #scale_y_continuous(breaks=seq(-0.1,0.3,by=0.05))+
 # xlab("Age-group")+annotate("text", x=3, y=0.2, 
  #                            label=paste("HLE", year1,"=", round(Sullivan.fun(rates=mxwx1,sex = Gender),2)),fontface =1)+
  #annotate("text", x=5, y=0.2, 
  #        label=paste("HLE", year2,"=", round(Sullivan.fun(rates=mxwx2,sex = Gender),2)),fontface =1)+
  
  labs(fill = "Effect", size=10)+theme_minimal()+
  theme(axis.text.x = element_text(size=10),axis.text.y = element_text
        (size=10),legend.text=element_text(size=10))+
  theme(axis.title.x = element_text(size=10),axis.title = element_text
        (size=10),legend.title=element_text(size=10))+ theme(text = element_text(size = 16))   



TOT 
ggsave("hle65_male.pdf",TOT, width = 15, height = 10)

hle_M <- HE_cont_res

save(hle_M, file = "hle_M.RData")


###############
# aggr ages
###############

library(tidyverse)

load("hle_M.RData")

head(hle_M)
hle_M$Ageclass2 <- NA
hle_M$Ageclass2[hle_M$Age%in%c(60,65)] <- "60-69"
hle_M$Ageclass2[hle_M$Age%in%c(70,75)] <- "70-79"
hle_M$Ageclass2[hle_M$Age>=80] <- "80+"


hle_M_Age2 <- aggregate(hle_M$Contribution,by = list(hle_M$Ageclass2,hle_M$CNT, hle_M$type),FUN=sum) %>% 
  rename(Ageclass=Group.1,CNT=Group.2, Type=Group.3, Contribution=x) 

head(hle_M_Age2)
save(hle_M_Age2,file = "hle_M_Age2.RData")


load("hle_F_Age2.RData")

hle_F_Age2$Contribution-hle_M_Age2$Contribution

