pop1 <- function(enter, exit, birthdate,
                 years = c(1851, 1951)){
    pick <- (!is.na(enter)) &
        (!is.na(exit)) &
        (!is.na(birthdate)) &
        (exit >= enter)
    enter <- enter[pick] + birthdate[pick]
    exit <- exit[pick] + birthdate[pick]
    ch <- enter >= exit
    enter[ch] <- exit[ch] + 0.001
    from <- years[1]
    to <- years[2]
    len <- to - from + 1
    pop <- numeric(len)
    j <- 0
    for (i in from:to){
        j <- j + 1
        pop[j] <- sum(enter < i + 0.5 & exit >= i + 0.5)
    }
    names(pop) <- from:to
    pop
}
