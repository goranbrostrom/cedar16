pop3 <- function(dat, years = 1801:1950){
    ## 'dat' is in fact 'observations'

    dat <- dat[(!is.na(dat$startdat)) & (!is.na(dat$slutdat)), ]
    dat <- dat[dat$startdat > 0 & dat$slutdat > 0, ]
    yrs <- years * 10000 + 501
    n <- length(years)
    out <- numeric(n)
    for (i in 1:n){
        out[i] <- with(dat, sum(startdat < yrs[i] & slutdat >= yrs[i]))
    }
    names(out) <- years
    out
}
