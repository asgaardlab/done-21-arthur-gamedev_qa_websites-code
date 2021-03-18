library(RColorBrewer)
suppressMessages(library(scales))

default_theme <- function() {
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x=element_text( size=20,family=""),
    axis.text.y=element_text( size=20,family=""),
    axis.title.x=element_text(size=20,family=""),
    axis.title.y=element_text(size=20,family=""),
    axis.ticks.length=unit(.25, "cm"),
    legend.text=element_text( size=20, family=""),
    legend.key = element_rect(
      fill = "white",
      color="white",
      size=2),
    legend.position = "bottom")
}

plot_events <- function(plot, database, max_y) {
  events <- levels(fct_relevel(fct_reorder(database$events$event, database$events$date), "Minor update", after = Inf))
  
  color_values <- c(viridis(length(events)-1), "black")
  names(color_values) <- events
  
  shapes <- letters[min(database$releases$num):max(database$releases$num)]
  names(shapes) <- head(events, -1)
  
  plot +
    geom_point(data=database$events %>% filter(event != "Minor update"), aes(x=date, shape=event), size=7, y=max_y*1.1, show.legend = FALSE) +
    geom_vline(data=database$events %>% filter(event != "Minor update"), aes(xintercept=date, color=event), show.legend = FALSE) +
    geom_vline(data=database$events %>% filter(event == "Minor update"), aes(xintercept=date, color=event, linetype=event), show.legend = FALSE) +
    scale_shape_manual("", breaks=head(events, -1), values = shapes) +
    scale_color_manual("", breaks=events, values = color_values, guide="none") +
    scale_linetype_manual("", breaks=c("Minor update"), values=c("dotted")) +
    coord_cartesian(clip="off") +
    default_theme()
}

plot_hist <- function(tibble, database, breaks, y_limits=NULL) {
  bs <- breaks %>% 
    Filter(f=function(d) month(d)==1)
    
  labs <- bs %>%
    .[c(TRUE, FALSE)] %>% 
    sapply(year) %>% 
    rbind('') %>%
    as.vector 
    
  tibble %>% 
    ggplot(aes(post_month)) + 
    geom_histogram(breaks=database$month_breaks, closed = "left", color="grey35") +
    scale_y_continuous(limits = y_limits, labels = label_number_si()) +
    scale_x_date(
      limits = c(min(breaks), max(breaks)),
      breaks = bs,
      labels = labs) +
    default_theme()
}

hist_posts_per_month_events <- function(database, breaks) {
  max_y <- 25000
  
  database$posts %>% 
    filter(!is.na(post_month)) %>% 
    filter(post_month != max(post_month)) %>% 
    plot_hist(database, breaks, c(0, max_y)) %>% 
    plot_events(database, max_y) + 
    labs(
      x = "Date", y="Posts per month",
      color="",
      shape="",
      linetype="") +
    guides(shape = guide_legend(nrow =4, order=1)) +
    theme(plot.margin = unit(c(1.5,0.6,0.6,0.6), "lines")) 
}

hist_posts_per_month <- function(database, breaks) {
  max_y <- 22000
  
  database$posts %>% 
    filter(!is.na(post_month)) %>% 
    filter(post_month != max(post_month)) %>% 
    plot_hist(database, breaks, c(0, max_y)) +
    labs(
      x = "Date", y="Posts per month",
      color="",
      shape="",
      linetype="") +
    guides(shape = guide_legend(nrow =5, order=1)) +
    theme(plot.margin = unit(c(1.5,0.6,0.6,0.6), "lines")) 
}

hist_active_users_per_month <- function(database, breaks) {
  max_y <- 5000
  
  database$posts %>%
    filter(!is.na(post_date)) %>% 
    filter(post_month != max(post_month)) %>% 
    distinct(author_id, post_month) %>% 
    plot_hist(database, breaks, c(0, max_y)) +
    labs(
      x = "Date", y="Active users",
      color="",
      linetype="",
      shape="") +
    guides(shape = guide_legend(nrow = 4, order=1)) +
    theme(plot.margin = unit(c(1.5,0.6,0.6,0.6), "lines")) 
}

hist_active_users_per_month_events <- function(database, breaks) {
  max_y <- 5000
  
  database$posts %>%
    filter(!is.na(post_date)) %>% 
    filter(post_month != max(post_month)) %>% 
    distinct(author_id, post_month) %>% 
    plot_hist(database, breaks, c(0, max_y)) %>% 
    plot_events(database, max_y) +
    labs(
      x = "Date", y="Active users",
      color="",
      linetype="",
      shape="") +
    guides(shape = guide_legend(nrow = 4, order=1)) +
    theme(plot.margin = unit(c(1.5,0.6,0.6,0.6), "lines")) 
}

smooth_percentages <- function(database, breaks) {
  plot_line <- function(plot, aes, data=NULL) {
      if (is.null(data)) {
          plot +
          geom_smooth(aes,
                      formula=y ~ s(x, bs = "cs"),
                      method="gam",
                      se=FALSE,
                      method.args=list(family=binomial))
      } else {
          plot +
          geom_smooth(aes,
                      data=data,
                      formula=y ~ s(x, bs = "cs"),
                      method="gam",
                      se=FALSE,
                      method.args=list(family=binomial))
      }
  }
  
  plot_points <- function(plot, aes, data=NULL) {
      if (is.null(data)) {
          plot +
          geom_point(aes,
                     size=3,
                     stat="smooth",
                     n=n_points%/%2,
                     formula=y ~ s(x, bs = "cs"),
                     method="gam",
                     method.args=list(family=binomial))
     } else {
          plot +
          geom_point(aes,
                     data=data,
                     size=3,
                     stat="smooth",
                     n=n_points%/%2,
                     formula=y ~ s(x, bs = "cs"),
                     method="gam",
                     method.args=list(family=binomial))
     }
  }
    
  bs <- breaks %>% 
    Filter(f=function(d) month(d)==1)
    
  labs <- bs %>%
    .[c(TRUE, FALSE)] %>% 
    sapply(year) %>% 
    rbind('') %>%
    as.vector 
           
  n_points <- database$posts %>% 
    mutate(post_year = floor_date(post_month, "year")) %>% 
    .$post_year %>% 
    unique() %>% 
    length
           
  eff <- database$answers %>%
           filter(!is.na(post_month)) %>% 
           filter(post_month != max(post_month))
           
  database$questions %>% 
    filter(!is.na(post_date)) %>% 
    mutate(
      post_month = floor_date(post_date, "months"),
      has_answer = as.numeric(n_answers>0)) %>% 
    filter(!is.na(post_month)) %>% 
    filter(post_month != max(post_month)) %>%  
    ggplot(aes(x=post_month)) %>% 
    plot_line(aes(y=is_resolved, color="Resolved questions")) %>% 
    plot_points(aes(y=is_resolved, color="Resolved questions", shape='Resolved questions')) %>% 
    plot_line(aes(y=has_answer, color="Answered questions")) %>% 
    plot_points(aes(y=has_answer, color="Answered questions", shape='Answered questions')) %>% 
    plot_line(aes(x=post_month, y=is_accepted, color="Answer effectiveness"), data = eff) %>% 
    plot_points(aes(x=post_month, y=is_accepted, color="Answer effectiveness", shape='Answer effectiveness'), data = eff) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent, limits = c(0, 1.1)) +
    scale_x_date(
        limits = c(min(breaks), max(breaks)),
        breaks=bs,
        labels = labs) +
    scale_color_manual("", values=c("#1F78B4", "#33A02C", "#E31A1C"), labels=c("Resolved questions", "Answered questions", "Answer effectiveness")) +
    scale_shape_manual("", values=c(15, 16, 18), labels=c("Resolved questions", "Answered questions", "Answer effectiveness")) +
    default_theme() +
    guides(color = guide_legend(nrow = 2, order=1), shape=guide_legend(nrow = 2, order=1)) +
    labs(
      x = "Date",
      y="",
      color="",
      linetype="") +
    theme(plot.margin = unit(c(0.1,1.5,1,0.6), "lines"))
}

boxplot_user_experience <- function(database, breaks) {
  bs <- breaks %>% 
    Filter(f=function(d) month(d)==1)
    
  labs <- bs %>%
    .[c(TRUE, FALSE)] %>% 
    sapply(year) %>% 
    rbind('') %>%
    as.vector 

  database$user_experience %>% 
    filter(!is.na(post_month)) %>% 
    mutate(post_quarter = floor_date(post_month, "halfyear")) %>% 
    group_by(post_quarter) %>% 
    mutate(log10_posts = log10(total_posts)) %>% 
    mutate(outlier = log10_posts > quantile(log10_posts, .75) + 1.50*IQR(log10_posts)) %>% 
    ungroup %>% 
    ggplot(aes(post_quarter, total_posts)) +
    geom_boxplot(aes(group=post_quarter), outlier.shape = NA) +
    geom_point(data = function(x) filter(x, outlier), position = position_jitter(w = 20, h = 0), shape=".", alpha=0.8) +
    scale_x_date(
        limits = c(min(breaks), max(breaks)),
        breaks=bs,
        labels = labs) +
    scale_y_log10(limits = c(1, 100000), labels=scales::comma) +
    annotation_logticks() +
    default_theme() +
    labs(
      x = "Date",
      y="Experience") +
    theme(plot.margin = unit(c(0.1,1.5,1,0.6), "lines"))
}
               
topic_dist <- function(tibble) {
    colors <- c("#F8766D", "#00BA38", "#619CFF")
    names(colors) <- c("General software development", "Bugs, crashes, and errors", "Game development")
    
    tibble %>% 
        filter(!is.na(topic)) %>% 
        ggplot(aes(y=topic_label, x=(..count..)/sum(..count..), fill=category)) +
        geom_bar() +
        scale_x_continuous(position="top", labels = scales::percent_format(accuracy = 1), limits=c(0, 0.3)) +
        default_theme() +
        theme(legend.position = c(0.7, 0.1)) +
        scale_fill_manual(values=colors) +
        labs(y="", x="Percentage of posts", fill="") +
        guides(fill=guide_legend(ncol=1))
}
               
colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep("plain", length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- "bold"                                  # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

bar_access_frequencies <- function(tibble) {
  tibble %>% 
  mutate(type=fct_inorder(type)) %>% 
  ggplot(aes(x=type, y=number, fill=category)) +
  geom_col(position="dodge") +
  scale_y_continuous(limits=c(0, 115)) +
  coord_flip() +
  scale_fill_brewer(palette="Paired") +
  theme(
    legend.position = "bottom") +
  default_theme() +
  labs(
    x="Access frequency",
    y="Respondents",
    fill="") +
  theme(plot.margin = unit(c(0.1,1,1,0.6), "lines"))
}

save <- function(plot, filename, w=2250, h=1500) {
  plot + ggsave(filename, width=w/300, height=h/300, dpi="print")
}