#------------------Load packages--------------
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("hablar")

#library(ggplot2)
#library(dplyr)
#library(hablar)
#library(magrittr)
#library(tidyverse)
#library(knitr)
#library(forcats)

#------------------Read in data--------------
books <- read.csv("Books - Sheet2.csv")

#------------------Change column names--------------
colnames(books) <- c("book", "author", "year", "pages", "genre", "sub.genre")

#------------------Explore dataset--------------
nrow(books)
ncol(books)
head(books)
str(books)

colnames(books) <- c("book", "author", "year", "pages", "genre", "sub.genre")

#------------------Create visualizations--------------

#Bar chart of genre % over all years (https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/)

ordered <- books %>% 
  dplyr::count(sub.genre, sort=TRUE) %>% 
  dplyr::mutate(
    sub.genre=forcats::fct_rev(forcats::fct_inorder(sub.genre))
  )

ordered <- ordered %>% 
  dplyr::mutate(perc = paste0(sprintf("%4.1f", n/sum(n)*100), "%"),
                perc = if_else(row_number() == 1, paste(perc, "of all books"), perc))

ordered <-
  ordered %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "goldenrod1",
      row_number() == 2 ~ "mediumpurple1",
      row_number() == 3 ~ "coral2",
      row_number() == 4 ~ "seagreen3",
      TRUE ~ "gray80"
    ))

ggplot(ordered, aes(x=n, y=sub.genre, fill=color)) +
  geom_col() +
  geom_label(
    aes(label=perc),
    hjust=1, nudge_x=-.1, 
    size=4, fontface="bold", 
    fill="white", label.size=0
  ) +
  scale_x_continuous(expand=c(.01,.01)) +
  scale_fill_identity(guide="none") +
  theme_void() +
  theme(axis.text.y=element_text(size=12, hjust=1),
        plot.margin=margin(rep(15, 4)))





