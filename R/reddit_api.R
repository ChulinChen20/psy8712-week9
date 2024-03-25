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

# combine the rounded coefficient and p-value and the degrees of freedom
corr <- c(round(correlation,2),cor.test(rstats_tbl$upvotes,rstats_tbl$comments)$parameter)

# remove leading zero
corr <-  sub("^0+", "", corr) 

# paste strings and the dynamic values together
paste("The correlation between upvotes and comments was r(", vote_comment_corr$parameter,") = ",
      str_remove(format(round(vote_comment_corr$estimate,2),nsmall = 2),"^0"),
      ", p = ",str_remove(format(round(vote_comment_corr$p.value,2),nsmall = 2),"^0"),
      ". This test ", ifelse(vote_comment_corr$p.value <= 0.05, "was","was not"),
      " statistically significant.", sep="")

# Publication
"The correlation between upvotes and comments was r(23) = .20, p = .33. This test was not statistically significant."

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

# scrap number of comments, extract number, replace na with o
comments <- rstats_html %>% 
  html_elements(css='.comments') %>%
  html_text() %>%
  str_remove_all("\\D") %>%
  as.numeric() %>%
  replace_na(0)

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
"The correlation between upvotes and comments was r(23) = c(cor = 0.16), p = .45. This test was not statistically significant."



