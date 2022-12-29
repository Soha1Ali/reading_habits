#------------------Load packages--------------
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("hablar")
#install.packages("tidyr")
#install.packages("reshape2")

#library(ggplot2)
#library(dplyr)
#library(hablar)
#library(magrittr)
#library(tidyverse)
#library(tidyr)
#library(knitr)
#library(reshape2)

#------------------read in data--------------
# read in 
books <- read.csv("books12:27.csv")
# convert to local data frame
books_l <- tibble::as_tibble(books)
# change column names
colnames(books_l) <- c("book", "author", "year", "pages", "genre", "sub.genre")

#------------------explore data--------------
# for each year, # of books read 
book.count <- books_l %>%
  group_by(year) %>%
  summarise(book_count=n()) 

ggplot(book.count, aes(x=year, y=book_count)) +
  geom_col()

#...add count lables to the df. this is pretty hacky and i'm sure there's a better way
count.pages <- c("3970 pages", "4733 pages", "6173 pages", "2377 pages", "3746 pages read")
book.count.1 <- cbind(book.count, count.pages)

#...define colors 
book.count.1 <-
  book.count.1 %>% 
  mutate(
    col.colors = case_when(
      row_number() == 1 ~ "darkseagreen2",
      row_number() == 2 ~ "darkseagreen2",
      row_number() == 3 ~ "darkseagreen2",
      row_number() == 4 ~ "darkseagreen2",
      row_number() == 5 ~ "seagreen3"
    ))

#...base viz
ggplot(book.count.1, aes(x=year, y=book_count, fill=col.colors)) +
  geom_col()

#...viz
ggplot(book.count.1, aes(x=year, y=book_count, fill=col.colors)) +
  geom_col() +
  
  geom_text(data=book.count.1, aes(x=year, y=book_count, label=count.pages), hjust=1.15) + # add labels to bars
  
  coord_flip() +
  
  scale_y_continuous(n.breaks=20) +
  
  scale_fill_identity(guide="none") + #honestly don't know what this is but it makes the color thing work
  
  xlab("") + #labels
  ylab("# of Books")+
  labs(title="# of Pages and # of Books Read Between 2018 and 2022") +
  
  theme_light() + #setting a base theme and getting rid of stuff i don't want
  theme(
    panel.grid.major=element_blank(), 
    panel.border=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    line=element_blank()) +
  
  theme( #more specific theme stuff
    axis.text.y=element_text(size=15, hjust=-.5, margin=margin(r=-25), face="bold"),
    plot.margin=margin(rep(15, 4))) +
  theme(
    axis.text.x=element_text(size=12),
    plot.margin=margin(rep(15, 4))) +
  theme(
    axis.title.x=element_text(vjust=-3, face="bold", size=12)) + 
  theme(
    legend.position="none") +
  theme(plot.title=element_text(size=15, hjust=.5)) 

# for each year, # of pages read
page.count <- books_l %>%
  group_by(year) %>%
  summarise(pages_sum = sum(pages))

#...add count lables to the df. this is pretty hacky and i'm sure there's a better way
count <- c("10 books", "15 books", "16 books", "6 books", "    11 books read")
page.count.1 <- cbind(page.count, count)

#...define colors 
page.count.1 <-
  page.count.1 %>% 
  mutate(
    col.colors = case_when(
      row_number() == 1 ~ "darkseagreen2",
      row_number() == 2 ~ "darkseagreen2",
      row_number() == 3 ~ "darkseagreen2",
      row_number() == 4 ~ "darkseagreen2",
      row_number() == 5 ~ "seagreen3"
    ))

#...base viz
ggplot(page.count.1, aes(x=year, y=pages_sum, fill=col.colors)) +
  geom_col()

#...viz
ggplot(page.count.1, aes(x=year, y=pages_sum, fill=col.colors)) +
  geom_col() +
  coord_flip() + #flip x and y axes 
  
  geom_text(data=page.count.1, aes(x=year, y=pages_sum + 200, label=count), hjust=.3) + # add labels to bars
  
  scale_y_continuous(n.breaks=20) + #make more y axis ticks
  
  scale_fill_identity(guide="none") + #honestly don't know what this is but it makes the color thing work
  
  xlab("") + #labels
  ylab("# of Pages")+
  labs(title="# of Pages and # of Books Read Between 2018 and 2022") +
  
  theme_light() + #setting a base theme and getting rid of stuff i don't want
  theme(
    panel.grid.major=element_blank(), 
    panel.border=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    line=element_blank()) +
  
  theme( #more specific theme stuff
    axis.text.y=element_text(size=12, hjust=5, margin=margin(r=-25), face="bold"),
    plot.margin=margin(rep(15, 4))) +
  theme(
    axis.text.x=element_text(size=12),
    plot.margin=margin(rep(15, 4))) +
  theme(
    axis.title.x=element_text(vjust=-3, face="bold", size=12)) + 
  theme(
    legend.position="none") +
  theme(plot.title=element_text(size=15, hjust=.5))

# across all years, average # of books read per year
books_l %>%
  summarise(book_count=n()/n_distinct(year)) 

# across all years, # of fiction and non-fiction read
genre.count <- books_l %>% 
  group_by(genre) %>%
  summarise(book_count=n()) 

ggplot(genre.count, aes(x=genre, y=book_count)) +
  geom_col() 

# across all years, # of books in each sub genre and ordered from most to least
books_l %>% 
  group_by(sub.genre) %>%
  summarise(book_count=n()) %>%
  arrange(desc(book_count))

# faster way of across all years, # of books in each sub genre and ordered from most to least
books_l %>%
  count(sub.genre, sort=TRUE)

# across all years, % of books in each genre
ordered <- books_l %>% 
  dplyr::count(sub.genre, sort=TRUE) %>% 
  dplyr::mutate(
    sub.genre=forcats::fct_rev(forcats::fct_inorder(sub.genre))
  )

ordered <- ordered %>% 
  dplyr::mutate(perc = paste0(sprintf("%4.1f", n/sum(n)*100), "%"))

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
        plot.margin=margin(rep(15, 4))) +
  
  labs(title="% of Books Read by Genre Between 2018 and 2022") +
  theme(plot.title=element_text(size=15, hjust=.4, vjust=4))



