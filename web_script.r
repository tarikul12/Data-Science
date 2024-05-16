installed.packages("rvest")
installed.packages("dplyr")

library(rvest)
library(dplyr)
link = "https://www.imdb.com/search/title/?moviemeter=1,25000&sort=user_rating,asc"
page = read_html(link)

page

name = page %>% html_nodes(".ipc-title__text") %>% html_text()
year <- page %>% 
  html_nodes(".sc-b189961a-8.kLaxqf.dli-title-metadata-item:first-child") %>% 
  html_text()
rating = page %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text()
description = page %>% html_nodes(".ipc-html-content-inner-div") %>% html_text()

index <- intersect(intersect(which(!is.na(name)), which(!is.na(year))), intersect(which(!is.na(rating)), which(!is.na(description))))


movies <- data.frame(name[index], year[index], rating[index], description[index], stringsAsFactors = FALSE)


write.csv(movies, file = "C:\\Users\\tarik\\Desktop\\10 semester\\Data_Science_Lab\\Data-Science\\movies.csv")

