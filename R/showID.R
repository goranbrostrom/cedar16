showID <- function(id, dat){
    ## 'dat' must be a data frame ala 'obs' in 'skum'.
    ## Plot in calendar time.

    guy <- dat[dat$id == id, c("enter", "exit", "birthdate",
                               "starttyp", "sluttyp", "foddat",
                               "socBranch", "civst", "sex")]
    n <- NROW(guy)
    if (n <= 0) stop("No person with that id!")
    sev <- as.integer(guy$socBranch)
    farg <- c("blue", "darkgreen", "black", "red")
    colo <- character(n)
    for (i in 1:n){
        colo[i] <- farg[sev[i]]
    }
    ltyp <- numeric(n)
    for (i in 1:n){
        if (guy$civst[i] == "unmarried"){
            ltyp[i] <- 2
        }else if (guy$civst[i] == "married"){
            ltyp[i] <- 1
        }else{
            ltyp[i] <- 4
        }
    }
    start <- guy$starttyp
    slut <- guy$sluttyp
    start[start %in% c(0, 3, 99)] <- 1
    slut[slut %in% c(0, 3, 99)] <- 1
    start <- start + (0:(n-1)) / (2 * n)
    slut <- slut + (0:(n-1)) / (2 * n)
    
    enter <- guy$enter + guy$birthdate
    exit <- guy$exit + guy$birthdate
    ats <- c(guy$birthdate[1], exit)
    ##cat("ats = ", ats, "\n")
    labs <- c(as.character(guy$foddat[1]),
              as.character(round(guy$exit[n], digits = 2)))
    ##cat("labs = ", labs, "\n")
    
    plot(c(enter[1], exit[1]), c(1, 1),
         ylim = c(0, n + 1), xlim = c(min(guy$birthdate), max(exit)),
         col = colo[1], type = "l", axes = FALSE, lwd = 2, lty = ltyp[1],
         xlab = "Birthdate, age", ylab = "Interval No.",
         main = paste("ID = ", id, " (", guy$sex[1], ")", sep = ""))
    axis(1, at = c(guy$birthdate[1], max(exit)),
         labels = labs, las = 1, cex.axis = 1)
    axis(2)
    box()
    ##abline(h = c(1, 2, 4:9), lty = 3, col = "darkgreen")
    points(guy$birthdate[1], 0, col = "blue", cex = 2)
    lines(c(enter[1], enter[1]), c(0, 1), lty = 3)
    if (n > 1){
        for (i in 2:n){
            ##cat("i = ", i, "\n")
            lines(c(enter[i], exit[i]), c(i, i), lty = ltyp[i],
                  col = colo[i], type = "l", lwd = 2)
            lines(c(enter[i], enter[i]), c(i, i - 1), lty = 3)
        }
    }
    if (guy$sluttyp[n] == 2){
        points(exit[n], n + 1, pch = "+", cex = 2)
    }

    
    invisible(cbind(enter, exit, start, guy$socBranch, guy$civst))
}
