
hle.plus.edagg = function (rates,age=seq(start.age,open.age,5), sex='f') {
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


# Data from example A. A. van Raalte and M. R. Nepomuceno
# Defining the variables for the start of the first interval, the start of the open-aged interval, and the years
start.age = 65
open.age = 85
year1 = 1970; year2 = 1990

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

hle.plus.edagg(rates=mxwx1)
hle.plus.edagg(rates=mxwx2)



