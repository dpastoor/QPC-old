#' plots QPC results
#' @param sim_sum summary simulated outcomes
#' @param rAUC_summary real AUC summary statistics
#' @param time_list list of times to check
#' @return prints the 3 ggplots for 25/50/75th percentiles
plot_QPC <- function(sim_sum, rAUC_summary, time_list) {
  PPinf <- unlist(lapply(sim_sum[,c("p25", "p50", "p75")], function(x) sum(x)/length(x)))
  EQinf <- unlist(lapply(sim_sum[,c("eq25", "eq50", "eq75")], EC_check))
  #plotting ---------------------------
  gg25 <- suppressWarnings(ggplot(sim_sum, aes(x = twentyfive)) + geom_histogram(color = "black", fill = "white") + 
                             geom_vline(xintercept = rAUC_summary[["twentyfive"]], size = 2, color = "red") + theme_bw() + base_theme() + 
                             xlab(paste0("25th Percentile AUC", time_list[1], "-", time_list[2])) + 
                             annotate("text", x = -Inf, y = Inf, label = paste0("PredP = ", PPinf[[1]]), hjust = -0.1, vjust =2, size = 5) +
                             annotate("text", x = -Inf, y = Inf, label = paste0("EqCrit = ", EQinf[[1]]), hjust = -0.1, vjust =4, size = 5))
  
  ## 50% percentile
  gg50 <- suppressWarnings(ggplot(sim_sum, aes(x = fifty)) + geom_histogram(color = "black", fill = "white") + 
                             geom_vline(xintercept = rAUC_summary[["fifty"]], size = 2, color = "red") + theme_bw() + base_theme() +
                             xlab(paste0("50th Percentile AUC", time_list[1], "-", time_list[2]))+ 
                             annotate("text", x = -Inf, y = Inf, label = paste0("PredP = ", PPinf[[2]]), hjust = -0.1, vjust =2, size = 5) +
                             annotate("text", x = -Inf, y = Inf, label = paste0("EqCrit = ", EQinf[[2]]), hjust = -0.1, vjust =4, size = 5))
  
  
  ## 75th percentile
  gg75 <- suppressWarnings(ggplot(sim_sum, aes(x = seventyfive)) + geom_histogram(color = "black", fill = "white") + 
                             geom_vline(xintercept = rAUC_summary[["seventyfive"]], size = 2, color = "red") + theme_bw() + base_theme() + 
                             xlab(paste0("75th Percentile AUC", time_list[1], "-", time_list[2]))+ 
                             annotate("text", x = -Inf, y = Inf, label = paste0("PredP = ", PPinf[[3]]), hjust = -0.1, vjust =2, size = 5) +
                             annotate("text", x = -Inf, y = Inf, label = paste0("EqCrit = ", EQinf[[3]]), hjust = -0.1, vjust =4, size = 5))
  suppressWarnings(print(gg25))
  suppressWarnings(print(gg50))
  suppressWarnings(print(gg75)) 
  #grid.arrange(gg25, gg50, gg75, nrow = 3) 
  return(list(gg25, gg50, gg75))  
} 