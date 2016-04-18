id463315 <- function(dat = observations, birthdate = 1795.385){
    enter <- tillTid(dat$startdat[dat$id == 463315]) - birthdate
    exit <- tillTid(dat$slutdat[dat$id == 463315]) - birthdate
    n <- length(exit)
    ord <- order(enter, -exit)
    enter <- enter[ord]
    exit <- exit[ord]
    plot(c(enter[1], exit[1]), c(1, 1), type = "l", xlab = "Age",
         ylim = c(0, 36), xlim = c(min(enter), max(exit)),
         col = "blue", axes = FALSE, ylab = "interval No.", lwd = 1.2)
    axis(1)
    wid <- c(1, 5, 10, 15, 20, 25, 30, 34)
    axis(2, at = wid)
    box()
    ## abline(h = wid, col = "darkgreen", lty = 3) 
    clo <- rep("blue", n)
    for (i in 2:n){
        incl <- c(4:9, 13, 16, 20, 22, 23)
        clo[incl] <- "red"
        lines(c(enter[i], exit[i]), c(i, i), type = "l", col = clo[i],
              lwd = 1.2)
    }
    text(13, 18, "red = 'included' intervals", col = "red")
}
