percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digit = digits, ...), "%")
}

domain_string <- function(data, domains) {
  string <- ""
  for(i in domains) {
    name <- if_else(i == "mean_SRI", "SRI",
                    if_else(i == "mean_score1a", "Domain 1A",
                            if_else(i == "mean_score1b", "Domain 1B",
                                    if_else(i == "mean_score2", "Domain 2",
                                            if_else(i == "mean_score3", "Domain 3",
                                                    if_else(i == "mean_score4", "Domain 4",
                                                            if_else(i == "mean_score5", "Domain 5",
                                                                    if_else(i == "mean_score6", "Domain 6",
                                                                            if_else(i == "mean_score7", "Domain 7",
                                                                                    if_else(i == "mean_score8", "Domain 8",
                                                                                            if_else(i == "mean_score9", "Domain 9",
                                                                                                    if_else(i == "mean_score10", "Domain 10",
                                                                                                            if_else(i == "mean_score11", "Domain 11",
                                                                                                                    if_else(i == "mean_score12a", "Domain 12A",
                                                                                                                            if_else(i == "mean_score12b", "Domain 12B", "")))))))))))))))
    
    statistic <- data %>% 
      select(any_of(i)) %>% 
      unlist(use.names = FALSE)
    
    new_string <- str_c("<br/>", name, ": ", statistic)
    string <- str_c(string, new_string)
  }
  string
}