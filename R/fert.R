fert <- function(dat, years = c(1950, 1951)){
    ## 'dat' is a data frame like 'obs'
    ## We will calculate 'number of births per woman'.
    ## For a period. Given by years. (1 Jan).
    
    if (is.null(dat$event)){
        dat$event <- !is.na(dat$sluttyp) & dat$sluttyp == 2
    }
    ## First, births between 'years':
    born <- dat[with(dat, enter + birthdate > years[1] &
                        enter + birthdate <= years[2] &
                         starttyp == 2), ]
    ##cat("dim(born) =", dim(born), "\n")
    indx <- match(born$id, per$id)
    born$mid <- per$mid[indx]
    born <- born[!is.na(born$mid), ]
    indx <- match(born$mid, per$id)
    born$m.birthdate <- toTime(per$foddat[indx])
    born$m.age <- born$birthdate - born$m.birthdate
    ##cat("sum(is.na(born$mid)) = ", sum(is.na(born$mid)))
    born <- born[, c("id", "mid", "m.age")]
    
    ## Then, 'women-years':
    fem <- dat[dat$sex == "female", c("id", "enter", "exit", "event", "birthdate")]
    ##cat("dim(fem) = ", dim(fem), "\n")
    #return(fem)
    #fem <- cal.window(fem, years)
    ##fem <- age.window(fem, c(15, 50))
    oo <- with(fem, perstat(cbind(enter, exit, event, birthdate), age = seq(15, 50, by = 1), period = years))$exposure
    bb <- numeric(35)
    mage <- floor(born$m.age)
    for (i in 1:35){
        bb[i] <- sum(mage == 14 + i)
    }
    sum(bb / oo)
}