# Script Settings and Resources
library(tidyverse)
library(rvest)

# Data Import and Cleaning
# download html data from the link
cnbc_html <- read_html("https://www.cnbc.com")

# define a function to scrape headlines and calculate length for one section
get_headlines <- function(section_url, source) {
  section_html <- read_html(section_url)
  headline <- section_html %>% 
    html_elements(css='.TrendingNowItem-title , .Card-title') %>%
    html_text()
  length = str_count(headline, "\\S+")
  
  # combine the three variables into a tibble
  cnbc_tbl <-tibble(headline,length,source)
  
  return(cnbc_tbl)
}

# Define sections
sections <- c("business", "investing", "tech", "politics")

# create an empty tibble
cnbc_tbl <-NULL

for (section in sections) {
  # get section urls
  section_url <- paste0("https://www.cnbc.com","/",section)
  # scrape headlines from the given section
  section_tbl <- get_headlines(section_url,section)
  cnbc_tbl <-bind_rows(cnbc_tbl,section_tbl)
}

# Visualization  
# use scatterplot to visualize the relationship
cnbc_tbl %>%
  ggplot(aes(x=source,y=length)) +
  geom_point()

# Run ANOVA
anova_result <- aov(length ~ source, data = cnbc_tbl)

anova_summary <- summary(anova_result)

# Publication
"The results of an ANOVA comparing lengths across sources was F(3, 150) = 2.26, p = .08. This test was not statistically significant."

# paste strings and the dynamic values together
paste("The results of an ANOVA comparing lengths across sources was F(", 
      summary(anova_result)[[1]]$"Df"[1], ", ", 
      summary(anova_result)[[1]]$"Df"[2], ") = ", 
      str_remove(format(round(anova_summary[[1]]$`F value`[1], 2),nsmall = 2),"^0"),
      ", p = ", str_remove(format(round(anova_summary[[1]]$"Pr(>F)"[1], 2),nsmall = 2),"^0"), 
      ". This test ", ifelse(anova_summary[[1]]$"Pr(>F)"[1] < 0.05, "was","was not"),
      " statistically significant.", sep = "")

