pop2 <- function(dat, 
                 years = 1901:1951, 
                 ages = c(0, 1, seq(5, 100, by = 5))){
    ## 'dat' is data frame containing 'enter, exit, event, birthdate'
    ## plus covariates of interest
    nage <- length(ages) - 1
    nyr <- length(years) - 1
    dat <- age.window(dat, c(ages[1], ages[nage + 1]))
    dat <- cal.window(dat, c(years[1], years[nyr + 1]))
    dat$year <- 0
    dat$age <- 0
    pop <- matrix(0, nrow = nage, ncol = nyr)
    out <- dat[1, ]
    for (y in 1:nyr){
        ##cat("Year = ", years[y], "\n")
        pp <- cal.window(dat, c(years[y], years[y + 1]))
        for (a in 1:nage){
            pa <- age.window(pp, c(ages[a], ages[a + 1]))
            if (!is.null(pa)){
                pop[a, y] <- sum(pa$exit - pa$enter)
                pa$year <- years[y]
                pa$age <- ages[a]
                out <- rbind(out, pa)
                ##cat("Age = ", ages[a], "\n")
            }
        }
    }
    out <- out[-1, ]
    invisible(list(out = out, pop = pop))
}
