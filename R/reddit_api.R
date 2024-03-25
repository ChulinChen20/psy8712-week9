# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)
library(httr)
library(DBI)
library(jsonlite)

# Data Import and Cleaning
# download and convert JSON data to an R object and extract the table from it
rstats_original_tbl <-fromJSON("https://www.reddit.com/r/rstats/.json",flatten=T)$data$children

# select the required variables and rename them
rstats_tbl <- rstats_original_tbl %>%
  select(data.title,data.ups,data.num_comments) %>%
  rename(post=data.title, upvotes=data.ups,comments=data.num_comments) 

# Visualization  
# use scatterplot to visualize the relationship
rstats_tbl %>%
  ggplot(aes(upvotes,comments)) +
  geom_point()

# Analysis
# perform and store the result of cor.test
vote_comment_corr <- cor.test(
  rstats_tbl$upvotes,
  rstats_tbl$comments
)

vote_comment_corr

# Publication
"The correlation between upvotes and comments was r(23) = .20, p = .33. This test was not statistically significant."

# paste strings and the dynamic values together
paste("The correlation between upvotes and comments was r(", vote_comment_corr$parameter,") = ",
      str_remove(format(round(vote_comment_corr$estimate,2),nsmall = 2),"^0"),
      ", p = ",str_remove(format(round(vote_comment_corr$p.value,2),nsmall = 2),"^0"),
      ". This test ", ifelse(vote_comment_corr$p.value < 0.05, "was","was not"),
      " statistically significant.", sep="")


