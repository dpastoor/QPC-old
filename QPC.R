QPC <- function(data, simulation, modfile, AUC_list){

  
  # load libraries ---------------------------------------------
  require(ggplot2)
  require(gridExtra)
  require(PKPDmisc)
  require(dplyr)
  
  
  # read data ------------------------------------------------------
  sim_dat <- read.table("data//run001QPC")
  
  #update names to read in outputs from modfile
  names(sim_dat) <- c("ID", "REP", "TIME", "DV","IPRED", "PRED", "AMT", "DOSE", "ISM", "MDV")
  
  real_dat <- read.csv("data//data1.csv")
  
  ### Helper functions -----------------------------------------
  EC_check  <- function(vec) {
    counter <- ifelse(vec > 0.8 & vec < 1.25, 1, 0) 
    sum(counter)/length(counter)
  }
  
  
  
  ### Cmax ----------------------------------------------
  cmax_sim <- sim_dat %.% 
    filter(MDV != 1) %.% 
    group_by(REP, ID) %.%
    summarise(cmax = max(DV)) %.%
  group_by(REP, add = FALSE) %.%
  summarise(twentyfive = quantile(cmax, probs = 0.25),
            fifty = quantile(cmax, probs = 0.50),
            seventyfive = quantile(cmax, probs = 0.75))

  cmax_real <- real_dat %.% 
  filter(MDV != 1) %.% 
  group_by(ID) %.%
  summarise(cmax = max(DV)) %.%
    ungroup() %.%
  summarise(twentyfive = quantile(cmax, probs = 0.25),
            fifty = quantile(cmax, probs = 0.50),
            seventyfive = quantile(cmax, probs = 0.75))

  
#   sum_statscmax <- ddply(cmax, .(REP), summarize, twentyfive = quantile(cmax, probs = 0.25),
#                          fifty = quantile(cmax, probs = 0.50),
#                          seventyfive = quantile(cmax, probs = 0.75))
#   r25 <- quantile(cmaxreal$cmax, probs = 0.25)
#   r50 <- quantile(cmaxreal$cmax, probs = 0.50)
#   r75 <- quantile(cmaxreal$cmax, probs = 0.75)
  
  ### Predictive p-values and equivalence criteria
  sum_stats_cmax <- within(cmax_sim, {p25 <- ifelse(twentyfive > cmax_real[["twentyfive"]], 1, 0)
                                          p50 <- ifelse(fifty > cmax_real[["fifty"]], 1, 0)
                                          p75 <- ifelse(seventyfive > cmax_real[["seventyfive"]], 1, 0)
                                     eq25 <- log(twentyfive)/log(cmax_real[["twentyfive"]])
                                     eq50 <- log(fifty)/log(cmax_real[["fifty"]])
                                     eq75 <- log(seventyfive)/log(cmax_real[["seventyfive"]])
  })

  PPinf <- unlist(lapply(sum_stats_cmax[,c("p25", "p50", "p75")], function(x) sum(x)/length(x)))
  EQinf <- unlist(lapply(sum_stats_cmax[,c("eq25", "eq50", "eq75")], EC_check))
  
### cmax plots #############################
## 25 percentile
gg25 <- ggplot(sum_stats_cmax, aes(x = twentyfive)) + geom_histogram(color = "black", fill = "white") + 
  geom_vline(xintercept = cmax_real[["twentyfive"]], size = 2, color = "red") + theme_bw() + base_theme() + 
  xlab("25th Percentile Cmax") + 
  annotate("text", x = cmax_real[["twentyfive"]], y = 18, label = paste0("PredP =", PPinf[[1]]), hjust = -0.1, size = 5) +
  annotate("text", x = cmax_real[["twentyfive"]], y = 20, label = paste0("EqCrit =", EQinf[[1]]), hjust = -0.1, size = 5)

## 50% percentile
gg50 <- ggplot(sum_stats_cmax, aes(x = fifty)) + geom_histogram(color = "black", fill = "white") + 
  geom_vline(xintercept = cmax_real[["fifty"]], size = 2, color = "red") + theme_bw() + base_theme() +
  xlab("50th Percentile Cmax")+ 
  annotate("text", x = cmax_real[["fifty"]], y = 18, label = paste0("PredP =", PPinf[[2]]), hjust = -0.1, size = 5) +
  annotate("text", x = cmax_real[["fifty"]], y = 20, label = paste0("EqCrit =", EQinf[[2]]), hjust = -0.1, size = 5)


## 75th percentile
gg75 <- ggplot(sum_stats_cmax, aes(x = seventyfive)) + geom_histogram(color = "black", fill = "white") + 
  geom_vline(xintercept = cmax_real[["seventyfive"]], size = 2, color = "red") + theme_bw() + base_theme() + 
  xlab("75th Percentile Cmax")+ 
  annotate("text", x = cmax_real[["seventyfive"]], y = 18, label = paste0("PredP =", PPinf[[3]]), hjust = -0.1, size = 5) +
  annotate("text", x = cmax_real[["seventyfive"]], y = 20, label = paste0("EqCrit =", EQinf[[3]]), hjust = -0.1, size = 5)

grid.arrange(gg25, gg50, gg75, ncol = 3)


### AUC slices

## testing how to set up time subsets for AUC function

time_subsets <- list(c(0, 2), c(0, 5), c(2, 8), c(1, 10))

ts1 <- time_subsets[[1]]

subset_time <- function(ts, dat) dat %.% filter(TIME >= ts[1], TIME <= ts[2])

subset_time(ts1, dat)
list_subsets <- lapply(time_subsets, subset_time, dat )

data <- list_subsets[[1]]


mean_calc3 <- function(df,...) {
  df %.% group_by(...) %.%
    summarise(mean(CONC))
}



lapply(list_subsets, mean_calc3, ID)

}
  
