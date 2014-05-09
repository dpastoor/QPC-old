p_eq <- function(sim_dat, real_dat) {
  within(sim_dat, {p25 <- ifelse(twentyfive > real_dat[["twentyfive"]], 1, 0)
                   p50 <- ifelse(fifty > real_dat[["fifty"]], 1, 0)
                   p75 <- ifelse(seventyfive > real_dat[["seventyfive"]], 1, 0)
                   eq25 <- log(twentyfive)/log(real_dat[["twentyfive"]])
                   eq50 <- log(fifty)/log(real_dat[["fifty"]])
                   eq75 <- log(seventyfive)/log(real_dat[["seventyfive"]])
                   
  })
}