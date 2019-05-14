library(tidyverse)
library(babynames)
library(gganimate)

sexinterest = 'F'

initial_gif = function(sexinterest){

  sexfull = ifelse(sexinterest == 'F', 'Girls', 'Boys')
  mytitle = str_c('Distribution of First Letters of U.S. ', sexfull, ' Names over Time')
    
  
  gif = babynames %>%
    filter(sex == sexinterest) %>%
    mutate(firstletter = stringr::str_sub(name, 1, 1)) %>%
    group_by(year, firstletter) %>%
    summarize(letter_count = sum(n)) %>%
    mutate(letter_prop = letter_count / sum(letter_count), 
           rank = min_rank(-letter_prop) * 1) %>%
    ungroup() %>% 
    ggplot(aes(x = firstletter,
               y = letter_prop,
               group = firstletter,
               fill = factor(firstletter),
               color = factor(firstletter))) +
    geom_col(alpha = 0.8) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    guides(color = FALSE, fill = FALSE) +
    labs(title = mytitle,
         subtitle  = '{closest_state}',
         x = "", y = "Names Starting in letter",
         caption = "Data: US Social Security Administration. @liuminzhao") +
    theme(plot.title = element_text(size = rel(2)),
          plot.subtitle = element_text(size = rel(3)),
          plot.caption = element_text(size = rel(2)),
          axis.text.x = element_text(face = "bold", size = rel(3)),
          axis.text.y = element_text(size = rel(3)),
          axis.title.y = element_text(size = rel(2))) +
    transition_states(year, transition_length = 4, state_length = 1) +
    ease_aes('cubic-in-out')
  
  
  animate(gif, duration = 60, fps = 30,  width = 1200, height = 1000, end_pause = 30,
          renderer = gifski_renderer(str_c(sexfull, ".gif"))) 
  
  animate(gif, duration = 60, fps = 30,  width = 1200, height = 1000, end_pause = 30, 
          renderer = ffmpeg_renderer()) -> for_mp4
  
  anim_save(str_c(sexfull, ".mp4"), animation = for_mp4)

}

initial_gif(sexinterest = 'M')
