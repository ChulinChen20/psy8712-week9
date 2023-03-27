# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)
library(httr)
library(DBI)
library(jsonlite)

# Data Import and Cleaning
# download file from the link
rstats_list <- GET("https://www.reddit.com/r/rstats/.json",
                   user_agent("UMN Researcher achen6496@umn.edu"),
                   query = list(format="csv"))
# convert JSON data to an R object and extract the table from it
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
# create two new variables from result of cor.test
correlation <- rstats_tbl %>%
  summarize(coef = cor.test(upvotes,comments)$estimate,
            p_value = cor.test(upvotes,comments)$p.value)

# Display correlation
correlation

# combine the rounded coefficient and p-value and the degrees of freedom
corr <- c(round(correlation,2),cor.test(rstats_tbl$upvotes,rstats_tbl$comments)$parameter)

# remove leading zero
corr <-  sub("^0+", "", corr) 

# Publication
# paste strings and the dynamic values together
paste("The correlation between upvotes and comments was r(", corr[3],") = ",corr[1],", p = ",corr[2],". This test was not statistically significant.", sep="")

rstats_html <- GET("https://old.reddit.com/r/rstats/")
content(rstats_html)

rstats_tbl <- rstats_html %>%
  html_elements(xpath = "//div")
