eq_check  <- function(vec) {
  counter <- ifelse(vec > 0.8 & vec < 1.25, 1, 0) 
  sum(counter)/length(counter)
}
