remYears <- function(dat, ages = c(0, 100), years){
    dat <- age.window(dat, ages)
    dat <- cal.window(dat, years)
    faul <- dat$enter >= dat$exit
    dat$enter[faul] <- dat$exit[faul] - 0.001
    fit <- coxph(Surv(enter, exit, event) ~ 1, data = dat)
    y <- summary(survfit(fit), rmean = ages[2])
    y$table["*rmean"] - ages[1]
}
