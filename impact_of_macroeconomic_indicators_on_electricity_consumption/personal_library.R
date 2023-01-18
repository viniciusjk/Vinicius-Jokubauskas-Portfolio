libs <- c('kableExtra', 'tidyverse')
          
      

sapply(libs[!libs %in% installed.packages()], install.packages, character=T)
sapply(libs[libs %in% installed.packages()], require, character=T)

pretty_sample_show <- function(data_, sample_size=10){
  data_ %>% 
    slice_sample(n=sample_size) %>% 
    kable() %>% 
    kable_classic(c('striped', 'hover', 'responsive'),
                  full_width = FALSE,
                  position='left') %>% 
    scroll_box(width="100%")
}

pretty_head_show <- function(data_, head_size=10){
  data_ %>% 
    head(head_size) %>% 
    kable() %>% 
    kable_classic(c('striped', 'hover', 'responsive'),
                  full_width = FALSE,
                  position='left') %>% 
    scroll_box(width="100%")
}