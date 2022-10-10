dis<-read.csv("SILC.csv")
head(dis)


colnames(dis) <- c("CNT","Year","Gender","Ageclass","Prevalence")
head(dis)

unique(dis$CNT)
unique(All$CNT)

dis$CNT[dis$CNT=="Austria"] <- "AUT"
dis$CNT[dis$CNT=="Belgium"] <- "BEL"
dis$CNT[dis$CNT=="Denmark"] <- "DNK"
dis$CNT[dis$CNT=="Finland"] <- "FIN"
dis$CNT[dis$CNT=="France"] <- "FRACNP"
dis$CNT[dis$CNT=="Greece"] <- "GRC"
dis$CNT[dis$CNT=="Ireland"] <- "IRL"
dis$CNT[dis$CNT=="Italy"] <- "ITA"
dis$CNT[dis$CNT=="Luxembourg"] <- "LUX"
dis$CNT[dis$CNT=="Portugal"] <- "PRT"
dis$CNT[dis$CNT=="Spain"] <- "ESP"
dis$CNT[dis$CNT=="Sweden"] <- "SWE"
dis$CNT[dis$CNT=="Germany"] <- "DEUTNP"
dis$CNT[dis$CNT=="Netherlands"] <- "NLD"
dis$CNT[dis$CNT=="United Kingdom"] <- "GBR_NP"


tail(dis)
dis$Gender[dis$Gender=="Women"] <- "Female"
dis$Gender[dis$Gender=="Men"] <- "Male"
head(dis)
head(All)

str(All)
str(dis)


load("All.RData")

unique(All$CNT)
Final <- merge(All,dis)

unique(Final$CNT)

save(Final,file = "Final.RData")
