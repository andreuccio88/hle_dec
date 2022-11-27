
install.packages('DemoDecomp', repos =
                   'http://cran.us.r-project.org')

help(package = 'DemoDecomp')
install.packages('HMDHFDplus', repos =
                   'http://cran.us.r-project.org')
install.packages('tidyverse', repos =
                   'http://cran.us.r-project.org')
install.packages('data.table', repos =
                   'http://cran.us.r-project.org')
library('HMDHFDplus')
library('tidyverse')
library('data.table')


Sullivan.fun = function (rates,age=seq(start.age,open.age,5), sex='f') {
  # 1) First, we split from our single vector 'rates' the set of age-specific
  death rates (mx) and age-specific prevalence of disability (wx)
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
year1 = 1970; year2 = 1990
# To extract data from the HMD, you first need to register at
www.mortality.org. Replace your username and password here:
  myusername <- "andrea.nigri@student.unisi.it"
mypassword <- "Dottorato17"
# Preparing the inputs for the arguments rates, pars1 and pars2
# 1) Selecting the country
country <- "USA"
# 2) Extracting Female Life Tables from HMD
LTF<- readHMDweb (CNTRY = country,
                  item = "fltper_5x1",
                  username = myusername,
                  password = mypassword,
                  fixup = TRUE)
# 3) Extracting Number of Deaths from HMD
Dx<- readHMDweb (CNTRY = country,
                 item = "Deaths_5x1",
                 username = myusername,
                 password = mypassword,
                 fixup = TRUE)
# 4) Extracting Exposures from HMD
Nx<- readHMDweb(CNTRY = country,
                item = "Exposures_5x1",
                username = myusername,
                password = mypassword,
                fixup = TRUE)
# 5) Getting the female death rates for ages >=65 in 1970 and 1990
mx1 <- filter(LTF,Year==1970 & Age >=65)$mx
mx2 <- filter(LTF,Year==1990 & Age >=65)$mx
# 6) We considered 85+ as the open age interval to match the
disability data
Dx1 <- filter(Dx,Year==1970)$Female
Dx2 <- filter(Dx,Year==1990)$Female
Nx1 <- filter(Nx,Year==1970)$Female
Nx2 <- filter(Nx,Year==1990)$Female
# y is the last position in the mx1 and mx2 vectors
y = length(seq(start.age,open.age,5))
mx1[y] <- sum(Dx1[19:24])/sum(Nx1[19:24])
mx2[y] <- sum(Dx2[19:24])/sum(Nx2[19:24])
mx1 <- mx1[1:y]
mx2 <- mx2[1:y]





mx1 <- c(0.0204, 0.0325, 0.0533, 0.0867,
         0.1640)
mx2 <- c(0.0161, 0.0246, 0.0384, 0.0646,
         0.1410)

wx1 <- c(0.3000,0.3657,0.4552, 0.5285,
         0.6822)
wx2 <- c(0.3056,0.3831,0.4552, 0.5424,
         0.6441)


# Making a single vector of mx followed by wx - we need these as input for either horiuchi or stepwise_replacement
mxwx1 <- c(mx1,wx1)
mxwx2 <- c(mx2,wx2)


# The DFLE in 1970 was
Sullivan.fun(rates=mxwx1)
## [1] 9.691796
# In 1990 it was
Sullivan.fun(rates=mxwx2)



HE_Decomp_sw <- stepwise_replacement(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2)
HE_Decomp_Cont <- horiuchi(
  func=Sullivan.fun,
  pars1 = mxwx1,
  pars2 = mxwx2,
  N=20)