split_stack <- function(sep_col = "Data", delimiter = ",") {
  
  library(tidyverse)
  library(magrittr)
  library(purrr)
  
  split_file <- read_csv(file.choose())
  
  tmp <-
    split_file[which(colnames(split_file) == sep_col)] %>% as.list() %>%
    map(str_split, pattern = ",", simplify = T) %>% 
    map(trimws)
  
  tmp <- tmp[[1]] %>% as_data_frame()
  
  names(tmp) <-
    paste("split_vlaue", 1:ncol(tmp), sep = "_")
  
  result <-
    cbind(split_file, tmp)
  
  stop <- ncol(result)
  start <- which(names(result) == "split_value_1")
  
  result %<>%
    gather(data = ., split_type, split_value, start:stop) %>%
    dplyr::select(-c(split_type)) %>%
    unique %>%
    dplyr::filter(split_value != "")
  
  write_csv(result, "split-email-results.csv")
  
}

split_stack(sep_col = "Data", delimiter = ",")
