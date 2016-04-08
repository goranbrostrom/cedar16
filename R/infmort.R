infmort <- function(dat, byears){
    ## 'byears' is a (integer) sequence of years for which we want
    ## infant mortality in a 'cohort' way.
    ##
    ## We do it by five-year averages: byears should have
    ## length as a multiple of five, eg 1901:1950.

    if (is.null(dat$event)){
        dat$event <- with(dat, !is.na(sluttyp) & sluttyp == 2)
    }
    n <- length(byears)
    n.groups <- n %/% 5
    imat <- matrix(byears[1:(n.groups * 5)], nrow = 5)
    dat$birthdate <- floor(dat$birthdate)
    dat <- dat[dat$birthdate %in% byears,
               c("birthdate", "enter", "exit", "event")]
    dat <- age.window(dat, c(0, 1))
    qu <- with(dat, enter >= exit)
    dat$enter[qu] <- dat$exit[qu] - 0.001
    out <- numeric(n.groups)
    for (i in 1:n.groups){
        fit <- coxph(Surv(enter, exit, event) ~ 1,
                     data = dat[dat$birthdate %in% imat[, i], ])
        out[i] <- 100 * (1 - min(summary(survfit(fit))$surv))
    }
    out <- rep(out, each = 5)
    names(out) <- byears[1:(n.groups * 5)]
    ##plot(names(out), out, type = "l", ylim = c(0, max(out)))
    out
}
