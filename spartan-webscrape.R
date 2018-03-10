library(rvest)
library(dplyr)
library(stringr)
library(data.table)


to_be_visited <- "https://fivebooks.com/best-books/adam-smith-dennis-rasmussen/"


visited <- c()

fivebooks_data <- list()
i <- 1



while (length(to_be_visited) > 0 ) {
  
  # 1. Select the next page in to_be_visited
  current_url <- to_be_visited[1]
  
  # 2. Gather the information we need from the interview
  page <- read_html(current_url)
  
  category <- page %>%
    html_node(".interview-heading") %>%
    html_text
  
  subject <- page %>%
    html_node(".subject") %>%
    html_text
  
  book <- page %>%
    html_nodes(".interview-page-bookshelf .title") %>%
    html_text
  
  author <- page %>%
    html_nodes(".interview-page-bookshelf .book-title") %>%
    html_text %>%
    str_replace(".* \n    by ", "")
  
  # 3. Put this information in a data.frame and insert it into fivebooks_data
  
  
  fivebooks_data[[i]] <- data.frame(category,
                                    subject,
                                    book,
                                    author,
                                    url = current_url,
                                    stringsAsFactors = F)
  i <- i + 1
  # 4. Find the recommended links and add them to to_be_visited
  recommended <- page %>%
    html_nodes(".related-item a") %>%
    html_attr("href")
  to_be_visited <- c(to_be_visited, recommended)
  
  # 5. Remove the page from to_be_visited and add it to visited
  visited <- c(visited, current_url)
  to_be_visited <- setdiff(to_be_visited, visited)
  
  message(subject,
          " - ",
          length(visited), " pages visited",
          " - ",
          length(to_be_visited), " pages left to visit")
  
}


fivebooks_data <- rbindlist(fivebooks_data)
