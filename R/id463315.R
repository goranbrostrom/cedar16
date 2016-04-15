id463315 <- function(dat = observations, birthdate = 1795.385){
    enter <- tillTid(dat$startdat[dat$id == 463315]) - birthdate
    exit <- tillTid(dat$slutdat[dat$id == 463315]) - birthdate
    n <- length(exit)
    plot(c(enter[1], exit[1]), c(1, 1), type = "l", xlab = "Age",
         ylim = c(0, 36), xlim = c(min(enter), max(exit)))
    for (i in 2:n){
        lines(c(enter[i], exit[i]), c(i, i), type = "l")
    }
}
