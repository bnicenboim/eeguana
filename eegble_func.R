as_eegble <- function(x, ...){
  UseMethod("as_eegble")
}

as_eegble.eeg_data <- function(x, .id = as.character(Sys.time()), verbose = TRUE) {
  eegble <- list(data = NULL, chan_info = NULL, gral_info = NULL  )
  eegble$chan_info <- x$chan_info
  eegble$gral_info <- list(srate = x$srate, 
               reference = x$reference)
  eegble$data <- bind_cols(select(x$timings,time),x$signals) %>%
        left_join(select(test_eeg$events,event = event_type, time=event_time),
                         by = "time") %>%
        mutate(event=as_factor(as.character(event)), `.id` = `.id`) %>%
        # move channels to the end
        select(-one_of(eegble$chan_info$labels), one_of(eegble$chan_info$labels)) %>%
        #group_by id so that the transformations are not done across files (subjects)
        group_by(`.id`)
  eegble <- unclass(eegble)
  class(eegble) <- "eegbl"
  if(verbose) print(paste("Object size",capture.output(pryr::object_size(eegble))))
  eegble
}

# file_io
read_vhdr <- function(x, ...) {

}

read_edf <- function(x, ...) {
  
}

# Info

nchan <- function(eegble){
  length(eegble$chan_info$labels)
}

chan_names <- function(eegble){
    #also rename them with <-
  c(eegble$chan_info$labels)
}

chan_names <- function(eegble){
    #also rename them with <-
  c(eegble$chan_info$labels)
}

event_names <- function(eegble){
    #also rename them with <-
  levels(eegble$data$event)
}


chan_info <- function(eegble){
  eegble$chan_info
}

gral_info <- function(eegble){
  eegble$gral_info
}

# print.eggbl <- function(eegble,...){
# }

summary.eggbl <- function(eegble,...){
}


rename_chan <- function(eegble){
}


# data wrangling
select_chan <- function(eegble, ...) {
}

filter.eegbl <- function(eegble, ...){ 
}

mutate.eegbl <- function(eegble, ...){ 
}

transmute.eegbl <- function(eegble, ...){ 
}

mutate_chan <- function(eegble, ...){ 
}

transmute_chan <- function(eegble, ...){ 
}


segment <- function(x, ...){ 
   UseMethod("segment")
}

slice <- function(x, ...){
  UseMethod("slice")
}

slice.eegbl <- function(eegble, from = NULL, to = NULL){
#add support for :

  eegble$data <- eegble$data   %>%  
        {if(is.character(from)) { 
            filter(.,time >= time[event %in% from])
         } else if(is.numeric(from)){
                 filter(.,time >= from)
         } else if(is.null(from)){ 
          .
         }  %>%  
        {if(is.character(to)) { 
            filter(.,time <= time[event %in% to])
         } else if(is.numeric(to)){
             filter(.,time <= to)
         }} else if(is.null(to)){
             .
         }}  %>% 
                 mutate(time = time - first(time)) # start from 0
  eegble
}


segment.eegbl <- function(eegble, events, lim = c(-1,1)){
                      
tsteps_lead <- round(lim[1] * eegble$gral_info$srate)
tsteps_lag <- round(lim[2] * eegble$gral_info$srate)

window <- seq(tsteps_lead, tsteps_lag)

eegble$data <- eegble$data %>% 
              mutate(event_id = ifelse(event %in% events,1,0))  %>%
              mutate(event_id = ifelse(event_id == 1, cumsum(event_id),0))%>% 
              select(-one_of(eegble$chan_info$labels), 
                      one_of(eegble$chan_info$labels)) 

  # Old for loop 
  # for(i in which(eegble$data$event_id!=0)){
  #     eegble$data[i + window, ]$time <- eegble$data[i + window, ]$time - eegble$data[i, ]$time
  # }

  decimals <- function(x) match(TRUE, round(x, 1:20) == x)

  event_rows <- expand.grid(which(eegble$data$event_id!=0),window) %>%
                .[order(.$Var1),] 
  # new times need to be rounded
  eegble$data[rowSums(event_rows), ]$time <- round(eegble$data[rowSums(event_rows), ]$time - eegble$data[event_rows[,1], ]$time,  decimals(1/eegble$gral_info$srate)) 

  eegble$data[rowSums(event_rows), ]$event_id <- eegble$data[event_rows[,1], ]$event_id  

  eegble$data <- eegble$data %>% filter(event_id != 0) 

  eegble$data %>% group_by(.id, event_id) %>%
             summarize(n = n()) %>% 
             {unique(.$n)} %>% 
             length() %>% 
             if(. > 1) warning("Segments have inequal size.")

  eegble
}


bind <- function(eegble, ...){

}