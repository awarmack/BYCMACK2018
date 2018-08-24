

fromNorth <- function(bearing){
  bearing <- normbear(bearing)  #first convert to a normal bearing
  
  fromNorth <- ifelse(bearing > 180, bearing-360, bearing)
  fromNorth <- ifelse(fromNorth < -180, 360+fromNorth, fromNorth)
  return(fromNorth)
}



twd <- seq(0, by=120, length.out=3)
btm <- twd

test <- expand.grid(btm, twd)
names(test) <- c("twd", "btm")

test$correct <- c(0, 120, -120, -120, 0, 120, 120, -120, 0)

test$cTWD <- fromNorth(test$twd)
test$cBTM <- fromNorth(test$btm)

test$BTMminTWD <- test$btm - test$twd
test$TWDminBTM <- test$twd - test$btm

test$convBMTminTWD <- fromNorth(test$TWDminBTM)

test$right <- test$correct == test$convBMTminTWD



getTWA <- function(btm, twd){
  fromNorth(twd-btm)
  
}

test$getTWA <- getTWA(test$btm, test$twd)





test$TWA <- getTwa(test$twd, test$btm)


correct <- matrix(test$correct, nrow=4, ncol=4, dimnames = list(twd, btm))

correct == matrix(test$cTWDmincBTM, nrow=4, ncol=4, dimnames = list(twd, btm))

correct - matrix(test$TWDminBTM, nrow=4, ncol=4, dimnames = list(twd, btm))
