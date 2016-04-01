addPer <- function(dat, name = "period", cuts = c(1851, 1876, 1901,
                                 1911, 1921, 1931, 1941, 1951)){
    n <- length(cuts)
    start <- cal.window(dat, c(cuts[1], cuts[2]))
    start[, "period"] <- 1
    if (n > 2){
        for (i in (3:n)){
            tmp <- cal.window(dat, c(cuts[i - 1], cuts[i]))
            tmp[, "period"] <- i - 1
            start <- rbind(start, tmp)
        }
    }
    levs <- character(n - 1)

    for (i in 1:(n - 1)){
        levs[i] <- paste(cuts[i], "-", cuts[i + 1] - 1, sep = "")
    }
    start[, "period"] <- factor(start[, "period"], labels = levs)
    start <- start[order(start$id, start$enter), ]
    start <- rc(start)
    start
}

