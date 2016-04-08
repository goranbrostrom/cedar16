infske <- function(dat, byear = 1801:1950){
    ## Infant mortality by socBranch
    farming <- with(dat, !is.na(socBranch) & socBranch == "worker")
    bonde <- infmort(dat[farming, ], byear)
    fru <- infmort(dat[!farming, ], byear)
    ymax <- max(fru, bonde)
    library(forecast)
    fru <- ma(fru, order = 11)
    bonde <- ma(bonde, order = 11)
    plot(byear, fru, type = "l", ylim = c(0, ymax))
    lines(byear, bonde, type = "l", col = "red")
}
