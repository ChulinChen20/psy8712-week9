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

# paste strings and the dynamic values together
paste("The correlation between upvotes and comments was r(", corr[3],") = ",corr[1],", p = ",corr[2],". This test was not statistically significant.", sep="")

# Publication
"The correlation between upvotes and comments was r(23) = .17, p = .42. This test was not statistically significant."


# Data Import and Cleaning
# download html data from the link
rstats_html <- read_html("https://old.reddit.com/r/rstats/")

# scrap titles
post <- rstats_html %>% 
  html_elements(css='.self .title.may-blank , .title.outbound , .self .title.may-blank') %>%
  html_text()

# scrap upvotes
upvotes <- rstats_html %>% 
  html_elements(css='.even .score , .odd .score') %>%
  html_text() %>%
  as.numeric() 

# the real number of upvotes seems to by every three elements start from position 2
upvotes<- upvotes[seq(2,length(upvotes),3)]

# scrap number of comments
comments <- rstats_html %>% 
  html_elements(css='.comments') %>%
  html_text() %>%
  str_remove_all("\\D") %>%
  as.numeric()

# combine the three variables into a tibble
rstats_tbl <-tibble(post,upvotes,comments)

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

# paste strings and the dynamic values together
paste("The correlation between upvotes and comments was r(", corr[3],") = ",corr[1],", p = ",corr[2],". This test was not statistically significant.", sep="")

# Publication
"The correlation between upvotes and comments was r(21) = c(cor = 0.16), p = .48. This test was not statistically significant."



