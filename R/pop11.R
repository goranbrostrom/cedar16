pop11 <- function(enter, exit, birthdate,
                 years = c(1901, 1951), per.year = 1){
    ## Like 'pop1', but with shorter intervals than yearly:
    ## per.year == 1 <==> pop11 = pop1
    pick <- (!is.na(enter)) & (!is.na(exit)) & (!is.na(birthdate)) & (exit > enter)
    enter <- enter[pick] + birthdate[pick]
    exit <- exit[pick] + birthdate[pick]
    from <- years[1]
    to <- years[2]
    len <- per.year * (to -from + 1)
    pop <- numeric(len)
    tid <- seq(from, to, length = len)
    j <- 0
    for (i in 1:len){
        
        pop[i] <- sum(enter <= tid[i] & exit >= tid[i])
    }
    
    list(tid = tid, pop = pop)
}
