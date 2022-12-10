# get disability data

dis<-read.csv("Severely Limited_27.csv")
colnames(dis) <- c("CNT","Year","Gender","Ageclass","Prevalence")

dis$CNT[dis$CNT=="Austria"] <- "AUT"
dis$CNT[dis$CNT=="Belgium"] <- "BEL"
dis$CNT[dis$CNT=="Denmark"] <- "DNK"
dis$CNT[dis$CNT=="Estonia"] <- "EST"
dis$CNT[dis$CNT=="Finland"] <- "FIN"
dis$CNT[dis$CNT=="France"] <- "FRACNP"
dis$CNT[dis$CNT=="Greece"] <- "GRC"
dis$CNT[dis$CNT=="Ireland"] <- "IRL"
dis$CNT[dis$CNT=="Italy"] <- "ITA"
dis$CNT[dis$CNT=="Luxembourg"] <- "LUX"
dis$CNT[dis$CNT=="Portugal"] <- "PRT"
dis$CNT[dis$CNT=="Spain"] <- "ESP"
dis$CNT[dis$CNT=="Sweden"] <- "SWE"
dis$CNT[dis$CNT=="Czech Republic"] <- "CZE"
dis$CNT[dis$CNT=="Germany"] <- "DEUTNP"
dis$CNT[dis$CNT=="Hungary"] <- "HUN"
dis$CNT[dis$CNT=="Latvia"] <- "LVA"
dis$CNT[dis$CNT=="Lithuania"] <- "LTU"
dis$CNT[dis$CNT=="Netherlands"] <- "NLD"
dis$CNT[dis$CNT=="Poland"] <- "POL"
dis$CNT[dis$CNT=="Slovakia"] <- "SVK"
dis$CNT[dis$CNT=="Slovenia"] <- "SVN"
dis$CNT[dis$CNT=="United Kingdom"] <- "GBR_NP"
dis$CNT[dis$CNT=="Bulgaria"] <- "BGR"


dis$Gender[dis$Gender=="Women"] <- "Female"
dis$Gender[dis$Gender=="Men"] <- "Male"

str(All)
str(dis)

# Load Mortality data
load("All.RData")

# merge data and get final database
Final <- merge(All,dis)
save(Final,file = "Final.RData")
