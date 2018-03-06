library(rvest)
library(dplyr)
library(stringr)
library(data.table)


# just text

url <- "https://www.sec.gov/Archives/edgar/data/16988/000119312515205169/d930282dsd.htm"


read_html(url) %>% 
  html_nodes("td, p, b") %>% 
  html_text() %>% 
  unlist %>% 
  paste(collapse = ' ')



#lots of links

url <- "http://www.yelp.com/search?find_loc=New+York,+NY,+USA"
  
new_urls <- read_html(url) %>% html_nodes(".biz-name") %>% html_attr('href') %>% paste0("http://www.yelp.com", .)


details <- list()
for (i in new_urls){
  
  details[[i]] <- read_html(i) %>% 
    html_nodes(".biz-hours") %>% 
    html_text()
  
  # details[[i]] %<>% gsub(., pattern = "\n+", replacement = "") %>% gsub(., pattern = "[[:space:]]+", replacement = " ")
  
  print(i)
  
}
details %>% bind_rows() %>% t() %>% View

