library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)
library(magrittr)
library(here)

metals <- c("tin", "tantalum", "gold")

setwd(here::here())

urls <- paste(
  "http://b5.caspio.com/dp.asp?AppKey=0c4a3000da14f615f362417c9bab&Metal=",
  metals,
  "&cbNewPageSize=250",
  sep = ""
)

urls %<>% as.list

urls[[4]] <- c("tin","tantalum", "gold")

stacked <- list()

for (i in 1:(length(urls) - 1)) {
  dat <- read_html(urls[[i]][1])
  tables <- html_table(x = dat, fill = T)
  scraped <- tables[3] %>% as.data.frame
  scraped$metals <- urls$metals[i]
  stacked[[i]] <- scraped
  stacked[[i]]$Metal <- urls[[4]][i]
  
}


dat <- read_html("http://b5.caspio.com/dp.asp?AppKey=0c4a3000e065bf537203451a94e0")
tables <- html_table(x = dat, fill = T)
scraped <- tables[3] %>% as.data.frame
scraped$Metal <- "tungsten"

stacked[[4]] <- scraped


# attempt to get the URLS from the website for that spreadsheet - not working well
# dat <- read_html(urls[[2]][1])
# links <- html_nodes(x = dat, css = "a") %>% html_attr("href")

stacked %<>% dplyr::bind_rows(stacked)
rm(scraped)

stacked$Valid.Until <-
  stacked$Last.Audit.Date %>% mdy() + ((substr(stacked$Audit.Cycle, 1,1) %>% as.numeric()) *365.25)+60

colnames(stacked) %<>%
  gsub(x = ., "[[:punct:]]", "_") %>%
  gsub(., pattern = "__", replacement = "_", fixed = T) %>%
  tolower()

stacked %<>% dplyr::select(-c(var_1,
                              company_website_with_cm_policy,
                              public_reports,
                              lbma_rg,
                              rjc,
                              location,
                              country,
                              group_company,
                              cm_policy,
                              ti_cmc)) %>% unique %>% as_data_frame()

stacked %>% write_csv( "CFSI_Web_Scrape.csv")

# stacked <- stacked[1,]
stacked %>% write_csv( "CFSI_Web_Scrape.csv")



# ##########################################
# Actives
# ##########################################

actives <-
  read_html("https://b5.caspio.com/dp/0c4a3000883e95e25bcb42efb0c3") %>%
  html_table(fill = T) %>%
  magrittr::extract(3)

actives <- actives[[1]][,2:ncol(actives[[1]])]




actives %>% write_csv("CFSI_Active_Smelters.csv")

