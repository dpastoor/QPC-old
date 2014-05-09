AUC_summary <- function(df,...) {  
  df %.% group_by(..., add = FALSE) %.%
    summarise(
      twentyfive = quantile(AUC, probs = 0.25),
      fifty = quantile(AUC, probs = 0.50),
      seventyfive = quantile(AUC, probs = 0.75),
      T1 = min(unique(T1)),
      T2 = max(unique(T2)))
}