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



# Info

nchan <- function(eegble){
  length(eegble$chan_info$labels)
}

chan_names <- function(eegble){
  c(eegble$chan_info$labels)
}


# event_names <- function(eegble){
#     #also rename them with <-
#   levels(eegble$data$event)
# }


chan_info <- function(eegble){
  eegble$chan_info
}

gral_info <- function(eegble){
  eegble$gral_info
}

srate <- function(eegble){
  eegble$gral_info$srate
}



duration <- function(eegble){
  map_dbl(eegble$data, ~ nrow(.x$signals) ) / srate(eegble) 
}

summary.eegbl <- function(eegble, ...){
  message(paste0("# EEG data (eegble) from ", nchan(eegble), " channels:"))
  # message(paste0( chan_names(eegble), collapse = ", "))
  message(paste0("# Sampling rate: ", srate(eegble), " Hz."))
  message(paste0("# Duration: ", duration(eegble), " ms."))
  # message(paste0("# Events (markers/triggers): ", 
  #                   paste0(event_names(eegble),collapse =", "), "."))
  message(paste0("# Size in memory: ", capture.output(pryr::object_size(eegble)), "."))

  map_dfr(eegble$data, ~ .x$signals ) %>% summary()

  # print(eegble$data, ...)
}

# summary.eegbl <- function(eegble,...){
# }


`chan_names<-` <-  function(eegble, value){

  eegble$chan_info$labels <- value
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

slice.eegbl <- function(x, ...) {
  dots <- rlang::enquos(...) #NSE
  x$data <- map(x$data, ~ list(signals= slice(.x$signals, !!!dots), 
                              event = .x$event))
  x
}

filter.eggbl <- function(.data, ...) {
  
}

slice_time <- function(x, from = NULL, to = NULL){

from <- if_else(is.null(from), -Inf, from)
to <- if_else(is.null(to), Inf, to)
  x$data <- map(x$data, ~ list(signals= filter(.x$signals, 
                sample >= from * srate(x),
                sample <= to * srate(x) ), 
                              events = .x$events))

  # x$data <- map(x$data, ~ filter(.x$signals, 
  #               if(!is.null(from)) {sample >= from * srate(x)} else {},
  #               if(!is.null(to)) {sample <= to * srate(x)} else {} ))
  x
}


# s <- function(from=description=="s10"){

# from <- rlang::enquo(from)
# heavy_eeg$data$autoocularICA1.vmrk %>% {filter(.$signals, sample >= 
#                   min(filter(.$events, !!from)$sample ))}
  
# }
# s()

slice_from_event <- function(x, ...){

  from <- rlang::enquos(...)
  x$data <- map(x$data, ~ list(signals= filter(.x$signals, 
                sample >= min(filter(.$events, !!!from)$sample)), 
                              events = .x$events))
  # x$data <- map(x$data, ~ list(signals= filter(.x$signals, 
  #               sample >= min(filter(.$events, !!!from)$sample)), 
  #                             event = .x$event))
  x
}

slice_to_event <- function(x, ...){

  to <- rlang::enquos(...)
  x$data <- map(x$data, ~ list(signals= filter(.x$signals, 
                sample <= max(filter(.$events, !!!to)$sample)), 
                              events = .x$events))
  x
}


drop_events <- function(x) {

   x$data <- map(x$data, ~ 
                    list(signals = .x$signals,
                         events = filter(.x$events, 
                          sample >= min(.x$signals$sample) - size, 
                                  sample <= max(.x$signals$sample))))
  x 
}

# Bad Interval

thewhat <- tibble(sample = 1:10L, y= 1.0, z =2.0)
thewhere <- tibble(cond = replicate (10,sample(c("a","b","c"),1)),
     sample= 1:10L, 
     duration = replicate (10,sample(1:3L,1)), where = replicate (10,sample(c(NA,"y","z"),1)))

NAs <- filter(thewhere, cond=="a") %>% 
      mutate( s = map2(sample,sample + duration - 1,seq)) %>% unnest

mutate(thewhat, y = if_else(sample %in% NAs$s,  NA_real_, y  ))
mutate(thewhat, y = if_else(sample %in% NAs$s[NAs$where =="y"],  
        NA_real_, y  ))

map2_dfc(NAs$where, NAs$s, ~ 
      mutate(a,  y = 
        if_else(sample == .y,  NA_integer_,sample  )))



NAify_samples <- function(x, ...){
  to <- rlang::enquos(...)

   x$data <- map(x$data, ~ 
                    list(signals = .x$signals[.x$signals$], Fp1 = 
                      case_when(between(sample, ) )  
                         events = filter(.x$events, 
                          sample >= min(.x$signals$sample) - size, 
                                  sample <= max(.x$signals$sample))))
  x 
}


# heavy_eeg$data$autoocularICA1.vmrk$events 

# slice_it <- function(eegble, from = NULL, to = NULL){
# #add support for :

#   eegble$data <- eegble$data   %>%  group_by(.id) %>%
#         {if(is.character(from)) { 
#             filter(.,time >= time[event %in% from])
#          } else if(is.numeric(from)){
#                  filter(.,time >= from)
#          } else if(is.null(from)){ 
#           .
#          }  %>%  
#         {if(is.character(to)) { 
#             filter(.,time <= time[event %in% to])
#          } else if(is.numeric(to)){
#              filter(.,time <= to)
#          }} else if(is.null(to)){
#              .
#          }}  %>% 
#                  mutate(time = time - first(time)) # start from 0
#   eegble
# }


segment.eegbl <- function(eegble, events, lim = c(-1,1)){
tsteps_lead <- round(lim[1] * eegble$gral_info$srate)
tsteps_lag <- round(lim[2] * eegble$gral_info$srate)

window <- seq(tsteps_lead, tsteps_lag)

eegble$data <- eegble$data %>% group_by(.id) %>%
              mutate(.seg_id = ifelse(event %in% events,1,0))  %>%
              mutate(.seg_id = ifelse(.seg_id == 1, cumsum(.seg_id),0))%>% 
              select(-one_of(eegble$chan_info$labels), 
                      one_of(eegble$chan_info$labels)) 

  # Old for loop 
  # for(i in which(eegble$data$.seg_id!=0)){
  #     eegble$data[i + window, ]$time <- eegble$data[i + window, ]$time - eegble$data[i, ]$time
  # }

  decimals <- function(x) match(TRUE, round(x, 1:20) == x)

  event_rows <- expand.grid(which(eegble$data$.seg_id!=0),window) %>%
                .[order(.$Var1),] 
  # new times need to be rounded
  eegble$data[rowSums(event_rows), ]$time <- round(eegble$data[rowSums(event_rows), ]$time - eegble$data[event_rows[,1], ]$time,  decimals(1/eegble$gral_info$srate)) 

  eegble$data[rowSums(event_rows), ]$.seg_id <- eegble$data[event_rows[,1], ]$.seg_id  

  eegble$data <- eegble$data %>% filter(.seg_id != 0) 

  eegble$data %>% group_by(.id, .seg_id) %>%
             summarize(n = n()) %>% 
             {unique(.$n)} %>% 
             length() %>% 
             if(. > 1) warning("Segments have inequal size.")

  eegble
}


bind <- function(eegble, ...){

}


plot_chan <- function(eegble, chans, ...) {
  #? allow formulas as chans? Pz + Fz + mean(Fz)
  eegble_long <-  eegble$data %>% gather(key = chan, value = ampl, one_of(chans))
  ggplot(eegble_long, aes(x = time, y = ampl, group=chan)) +
            scale_x_continuous(name = "Time (ms)") + 
            scale_y_continuous(name = "Amplitude") + 
            geom_line(aes(group = .seg_id), alpha = .5)
}
