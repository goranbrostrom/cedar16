remYears <- function(dat, ages = c(0, 100), years){
    ## 'dat' is a data frame with variables enter, exit, event, birthdate.
    ## 'ages' is a vector of length 2, ages[1] < ages[2].
    ## 'years' is a vector of length 2, years[1] < years[2].

    ## the result is expected remaining years after ages[1], given that
    ## death occurs before age[2] (Intensity = Inf after age[2]).

    ## One-year age intervals: If exposure is zero in an interval,
    ## not the last, then intensity = 0 there. Last interval: = Inf.

    oldop <- options(warn = -1)
    on.exit(options(oldop))
    cuts <- seq(ages[1], ages[2], length = (ages[2] - ages[1]) / 5)
    res <- with(dat, perstat(cbind(enter, exit, event, birthdate),
                             period = years, age = cuts))
    rate <- res$intensity

    n <- length(cuts) - 1
    goOn <- TRUE
    for (i in n:1){
        if (goOn){
            if (is.nan(rate[i])){
                rate[i] <- Inf
            }else{
                goOn <- FALSE
            }
        }else{
            if (is.nan(rate[i])){
                rate[i] <- 0
            }
        }
    }

    if (ages[1] > 0){
        rate <- c(0, rate, Inf)
    }else{
        cuts <- cuts[-1]
        rate <- c(rate, Inf)
    }
    ##return(rate)
    res <- integrate(ppch, 0, 100, lower.tail = FALSE,
                     cuts = cuts, levels = rate)$value
    invisible(list(res = res, cuts = cuts, rate = rate))
}
