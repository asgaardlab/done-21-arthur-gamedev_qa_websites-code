suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(viridis))
suppressMessages(library(arrow))

load_tables <- function(dir, table_names) {
    get_table <- function(table_name) {
        path <- paste(dir, '/', table_name, '.csv', sep="")
        read.csv(file = path)
    }
  
  tables <- map(table_names, get_table)
  names(tables) <- table_names
  
  tables
}

data_transform <- function(database) {
  factor_topics <- function(tibble) {
      tibble %>%
      mutate(topic_label = fct_rev(fct_infreq(as.factor(topic_label))))
  }

  # Remove time from dates, get post month
  fix_dates <- function(tibble) {
    tibble %>%
      mutate(post_date = as.Date(post_date)) %>% 
      mutate(post_month = floor_date(post_date, "months"))
  }
  
  # Creates a table with all of the posts (questions, comments, answers)
  make_posts <- function(database) {
   database$posts <- database$questions %>%
    select(id, post_date, post_month, author_id, topic, topic_label, category, len_text) %>% 
    mutate(
      type = "question",
      question_id = id
      ) %>% 
    bind_rows(
      database$answers %>%
      select(question_id, id, post_date, post_month, author_id, topic, topic_label, category, len_text, has_code) %>% 
      mutate(type = "answer") %>% 
        bind_rows(
        database$comments %>%
        select(question_id, id, post_date, post_month, author_id, topic, topic_label, category, len_text, has_code) %>% 
        mutate(type = "comment")))
  }
  
  # Gets unique months (useful for plotting histograms)
  month_breaks <- function(database) {
    database$month_breaks <- database$posts %>%
    filter(!is.na(post_month)) %>% 
    filter(post_month != max(post_month)) %>% 
    distinct(post_month) %>% 
    arrange(post_month) %>%
    pull(post_month)  %>% 
    append(max(.)+months(1))
  }
  
  # Calc user experience (number of posts in each month)
  user_experience <- function(database) {
    database$posts %>% 
      filter(!is.na(post_date)) %>% 
      filter(post_month != max(post_month)) %>% 
      arrange(post_month) %>% 
      group_by(author_id, post_month) %>% 
      summarise(num_posts = n(), .groups='drop_last') %>% 
      ungroup() %>% 
      group_by(author_id) %>% 
      mutate(total_posts = cumsum(num_posts)) %>% 
      ungroup()
  }
  
  # table for posts per month
  ppm <- function(database) {
    database$posts %>% 
      group_by(post_month) %>% 
      summarise(n_posts = n()) %>% 
      ungroup  
  }
  
  # table for active users per month
  aupm <- function(database) {
    database$posts %>% 
      distinct(author_id, post_month) %>% 
      group_by(post_month) %>% 
      summarise(n_users = n()) %>% 
      ungroup  
  }
  
  # Calc percentages of answered, resolved + effectiveness per month
  percentages <- function(database) {
    database$questions %>% 
      mutate(post_month = floor_date(post_date, "months")) %>% 
      filter(!is.na(post_month)) %>% 
      filter(post_month != max(post_month)) %>% 
      group_by(post_month) %>% 
      summarise(
        answered = mean(is_answered),
        resolved = mean(is_resolved)
      ) %>% 
      inner_join(
        database$answers %>% 
          group_by(post_month) %>% 
          summarise(
            answer_effectiveness = mean(is_accepted)
          ),
        by = "post_month"
      )  
  }
  
  database$questions <- database$questions %>% factor_topics %>% fix_dates
  database$answers <- database$answers %>% factor_topics %>% fix_dates
  database$comments <- database$comments %>% factor_topics %>% fix_dates
  database$posts <- database %>% make_posts %>% factor_topics
  database$month_breaks <- database %>% month_breaks
  database$user_experience <- database %>%  user_experience
  database$ppm <- database %>% ppm
  database$aupm <- database %>% aupm
  database$percentages <- database %>% percentages
  
  database
}

load_ue4 <- function(path) {
  table_names <- c('questions', 'answers', 'comments', 'users')  
  ue4   <- load_tables(path, table_names)
  
  ue4$releases <- tribble(
    ~release, ~date, ~num,
    "v4.0/Public engine release", ymd("2014-03-19"), 11,
    "Marketplace release", ymd("2014-09-03"), 12,
    "Free engine release", ymd("2015-03-02"), 13
  )
  
  ue4$versions <- tribble(
    ~version, ~date,
    "4.1",  ymd("2014-04-22"),
    "4.2",  ymd("2014-06-04"),
    "4.3",  ymd("2014-07-16"),
    "4.4",  ymd("2014-08-14"),
    "4.5",  ymd("2014-10-14"),
    "4.6",  ymd("2014-12-03"),
    "4.7",  ymd("2015-02-24"),
    "4.8",  ymd("2015-06-10"),
    "4.9",  ymd("2015-08-31"),
    "4.10", ymd("2015-11-06"),
    "4.11", ymd("2016-03-31"),
    "4.12", ymd("2016-06-01"),
    "4.13", ymd("2016-09-01"),
    "4.14", ymd("2016-11-14"),
    "4.15", ymd("2017-02-14"),
    "4.16", ymd("2017-05-24"),
    "4.17", ymd("2017-08-03"),
    "4.18", ymd("2017-10-23"),
    "4.19", ymd("2018-03-14"),
    "4.20", ymd("2018-07-17"),
    "4.21", ymd("2018-11-05"),
    "4.22", ymd("2019-03-25"),
    "4.23", ymd("2019-08-29"),
    "4.24", ymd("2019-12-09"),
    "4.25", ymd("2020-05-05"),
  )
  
  ue4$events <- ue4$releases %>% 
    rename(event=release) %>% 
    bind_rows(ue4$versions %>% 
                mutate(
                  num = 0,
                  event = "Minor update"
                ) %>% 
                select(event, date, num))
    
    ue4 %>%
        data_transform
}

load_unity <- function(path) {
  table_names <- c('questions', 'answers', 'comments', 'users')  
  unity <- load_tables(path, table_names)
  
  unity$releases <- tribble(
    ~release, ~date, ~num,
    "Free release"       , ymd("2009-10-28"), 1,
    "Asset store release", ymd("2014-03-19"), 2,
    "v3.0"               , ymd("2010-10-15"), 3,
    "v3.5"               , ymd("2012-02-14"), 4,
    "v4.0"               , ymd("2012-11-13"), 5,
    "v5.0"               , ymd("2015-03-15"), 6,
    "v2017.1"            , ymd("2017-07-10"), 7,
    "v2018.1"            , ymd("2018-05-02"), 8,
    "v2019.1"            , ymd("2019-04-15"), 9,
    "v2020.1"            , ymd("2020-07-22"), 10
  )
  
  unity$versions <- tribble(
    ~version, ~date,
    "2.6", ymd("2009-10-28"),
    "3.2", ymd("2011-02-10"),
    "3.4", ymd("2011-07-26"),
    "4.1", ymd("2013-03-13"),
    "4.2", ymd("2013-07-22"),
    "4.3", ymd("2013-11-12"),
    "4.5", ymd("2014-05-27"),
    "4.6", ymd("2014-11-25"),
    "4.7", ymd("2015-12-17"),
    "5.1", ymd("2015-06-09"),
    "5.2", ymd("2015-09-08"),
    "5.3", ymd("2015-12-08"),
    "5.4", ymd("2016-07-28"),
    "5.5", ymd("2016-11-30"),
    "2017.2", ymd("2017-10-12"),
    "2017.3", ymd("2017-12-19"),
    "2017.4", ymd("2018-04-24"),
    "2018.2", ymd("2018-07-10"),
    "2018.3", ymd("2018-12-07"),
    "2018.4", ymd("2019-05-10"),
    "2019.2", ymd("2019-07-30"),
    "2019.3", ymd("2019-01-27"),
    "2019.4", ymd("2020-06-22")
  )
  
  unity$events <- unity$releases %>% 
    rename(event=release) %>% 
    bind_rows(unity$versions %>% 
                mutate(
                  num = 0,
                  event = "Minor update"
                ) %>% 
                select(event, date, num)) 
  unity %>%
    data_transform
}

load_stackoverflow <- function(path) {
  table_names <- c('questions', 'answers', 'comments', 'users')  
  so <- load_tables(path, table_names)
  so %>%
    data_transform
}

load_gamedev_se <- function(path) {
  table_names <- c('questions', 'answers', 'comments', 'users')
  se <- load_tables(path, table_names)
  se %>%
    data_transform
}