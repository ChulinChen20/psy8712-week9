# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)
library(httr)
library(DBI)

# Data Import and Cleaning
rstats_list <- GET("https://www.reddit.com/r/rstats/.json")
rstats_original_tbl <- dbGetQuery(rstats_list)


rstats_html <- GET("https://old.reddit.com/r/rstats/")
content(rstats_html)

rstats_tbl <- rstats_html %>%
  html_elements(xpath = "//body")