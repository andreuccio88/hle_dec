library(tidyverse) 
library(HMDHFDplus)

country <- getHMDcountries()


myusername <- "user"
mypassword <- "pass"


country <- c("AUT","BEL","DNK","EST","FIN","FRACNP","GRC",
             "IRL","ITA","LUX","PRT","ESP","SWE","CZE",
             "DEUTNP","HUN","LVA","LTU","NLD","POL","SVK",
             "SVN","GBR_NP","BGR")


  Ex <- list()
  for (i in 1: length(country)) {
    cnt <- country[i]
    Ex[[cnt]] <- readHMDweb(cnt, "Exposures_1x1",
                            myusername, mypassword)
    
    # let's print the progress
    paste(i,'out of',length(country)) 
  }

#load("Ex.RData")

Ex
Ex <- do.call(rbind.data.frame, Ex)
Ex$CNT <- rownames(Ex) 
Ex$CNT<- gsub('[0-9.]', '', Ex$CNT)
Ex
rownames(Ex) <- NULL
head(Ex)
tail(Ex)

Ex2 <- Ex %>% filter(Age>15)

Ex2$Ageclass <- NA
Ex2$Ageclass[Ex2$Age%in%c(16,17,18,19)] <- "16-19"
Ex2$Ageclass[Ex2$Age%in%c(20:24)] <- "20-24"
Ex2$Ageclass[Ex2$Age%in%c(25:29)] <- "25-29"
Ex2$Ageclass[Ex2$Age%in%c(30:34)] <- "30-34"
Ex2$Ageclass[Ex2$Age%in%c(35:39)] <- "35-39"
Ex2$Ageclass[Ex2$Age%in%c(40:44)] <- "40-44"
Ex2$Ageclass[Ex2$Age%in%c(45:49)] <- "45-49"
Ex2$Ageclass[Ex2$Age%in%c(50:54)] <- "50-54"
Ex2$Ageclass[Ex2$Age%in%c(55:59)] <- "55-59"
Ex2$Ageclass[Ex2$Age%in%c(60:64)] <- "60-64"
Ex2$Ageclass[Ex2$Age%in%c(65:69)] <- "65-69"
Ex2$Ageclass[Ex2$Age%in%c(70:74)] <- "70-74"
Ex2$Ageclass[Ex2$Age%in%c(75:79)] <- "75-79"
Ex2$Ageclass[Ex2$Age%in%c(80:84)] <- "80-84"
Ex2$Ageclass[Ex2$Age>84] <- "85+"


Ex2 <- gather(Ex2, key = "Gender", value = "measurement",
       Female,Male)
Ex2 <- aggregate(Ex2$measurement,by = list(Ex2$Ageclass,Ex2$Year,Ex2$CNT,Ex2$Gender),FUN=sum)%>% 
  rename(Ageclass=Group.1,Year=Group.2, CNT=Group.3, Gender= Group.4, Expo=x)

head(Ex2)
tail(Ex2)

###########################
# Dx
#########################

 Dx <- list()
 for (i in 1: length(country)) {
   cnt <- country[i]
   Dx[[cnt]] <- readHMDweb(cnt, "Deaths_1x1",
                           myusername, mypassword)
   
   # let's print the progress
   paste(i,'out of',length(country)) 
 }

#load("Dx.RData")
Dx
Dx <- do.call(rbind.data.frame, Dx)
Dx$CNT <- rownames(Dx) 
Dx$CNT<- gsub('[0-9.]', '', Dx$CNT)
Dx
rownames(Dx) <- NULL
head(Dx)
tail(Dx)


Dx2 <- Dx %>% filter(Age>15)

Dx2$Ageclass <- NA
Dx2$Ageclass[Dx2$Age%in%c(16,17,18,19)] <- "16-19"
Dx2$Ageclass[Dx2$Age%in%c(20:24)] <- "20-24"
Dx2$Ageclass[Dx2$Age%in%c(25:29)] <- "25-29"
Dx2$Ageclass[Dx2$Age%in%c(30:34)] <- "30-34"
Dx2$Ageclass[Dx2$Age%in%c(35:39)] <- "35-39"
Dx2$Ageclass[Dx2$Age%in%c(40:44)] <- "40-44"
Dx2$Ageclass[Dx2$Age%in%c(45:49)] <- "45-49"
Dx2$Ageclass[Dx2$Age%in%c(50:54)] <- "50-54"
Dx2$Ageclass[Dx2$Age%in%c(55:59)] <- "55-59"
Dx2$Ageclass[Dx2$Age%in%c(60:64)] <- "60-64"
Dx2$Ageclass[Dx2$Age%in%c(65:69)] <- "65-69"
Dx2$Ageclass[Dx2$Age%in%c(70:74)] <- "70-74"
Dx2$Ageclass[Dx2$Age%in%c(75:79)] <- "75-79"
Dx2$Ageclass[Dx2$Age%in%c(80:84)] <- "80-84"
Dx2$Ageclass[Dx2$Age>84] <- "85+"



Dx2 <- gather(Dx2, key = "Gender", value = "measurement",
              Female,Male)
Dx2 <- aggregate(Dx2$measurement,by = list(Dx2$Ageclass,Dx2$Year,Dx2$CNT,Dx2$Gender),FUN=sum)%>% 
  rename(Ageclass=Group.1,Year=Group.2, CNT=Group.3, Gender= Group.4, Death=x)

head(Dx2)
tail(Dx2)

All <- merge(Dx2,Ex2)

All$mx <- All$Death/All$Expo
str(All)

All %>% filter(Year==2010)%>% ggplot(aes(Ageclass,log(mx),color=Gender))+geom_point()+facet_wrap(~CNT)

save(All,file = "All.RData")


