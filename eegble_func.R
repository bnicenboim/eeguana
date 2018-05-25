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
  levels(eegble$chan_info$labels)
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


# `chan_names<-` <-  function(eegble, value){

#   eegble$chan_info$labels <- value
# }


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

# # Bad Interval

# thewhat <- tibble(sample = 1:10L, y= 1.0, z =2.0)
# thewhere <- tibble(cond = replicate (10,sample(c("a","b","c"),1)),
#      sample= 1:10L, 
#      duration = replicate (10,sample(1:3L,1)), where = replicate (10,sample(c(NA,"y","z"),1)))

# NAs <- filter(thewhere, cond=="a") %>% 
#       mutate( s = map2(sample,sample + duration - 1,seq)) %>% unnest

# mutate(thewhat, y = if_else(sample %in% NAs$s,  NA_real_, y  ))
# mutate(thewhat, y = if_else(sample %in% NAs$s[NAs$where =="y"],  
#         NA_real_, y  ))
# mutate_at(thewhat, y = if_else(sample %in% NAs$s[NAs$where =="y"],  
#         NA_real_, y  ))

# thewhat$sample %in% NAs$sample 

# map_dfc(select(thewhat,-sample), 
#   ~ if_else(thewhat$sample %in% NAs$sample, NA_real_,.x))

# thewhat$sample %in% NAs$sample
# thewhat$sample %in% (NAs$sample * ("y" == NAs$where))

# map_dfc(select(thewhat,-sample), 
#   ~ names(.x))



# map2_dfc(NAs$where, NAs$s, ~ 
#       mutate(thewhat,  y = 
#         if_else(sample == .y,  NA_integer_,sample  )))

#   NAs <- map(heavy_eeg$data, ~ filter(.x$events, description == "Bad Min-Max") %>% 
#       mutate( sample = map2(sample, sample + size - 1,seq)) %>% 
#       unnest %>%
#       select(channel, sample))


# map(heavy_eeg$data, ~ mutate_at(.x$signals,chan_names(heavy_eeg),
#   funs(if_else(sample %in% 
#     transmute(filter( .x$events,description == "Bad Min-Max", is.na(channel)),sample), NA_real_,. )) ))
# myN400_CONG$data$cong4003.vmrk$events

# ll <- filter(myN400_CONG$data$cong4003.vmrk$events, description== "Bad Min-Max") %>% 
# select(sample, size, channel)

#  # pmap(ll, function(s,z,c) rowwise(myN400_CONG$data$cong4003.vmrk$signals) %>% 
#  #  mutate_at(
#  #  one_of(as.character(c)), funs(if_else(sample %in% seq(s, s + z -1),
#  #            NA_real_, .  ))))

# pmap(ll, function(s,z,c) mutate(myN400_CONG$data$cong4003.vmrk$signals,
#    Pz = if_else(sample %in% 1:10,
#             NA_real_, Pz  )))

# rowwise(myN400_CONG$data$cong4003.vmrk$signals) %>% mutate( Pz = 
#   if_else(sample %in% 1:10, NA_real_, Pz  ))

NAify_samples <- function(x, ...){
  dots <- rlang::enquos(...)

  # NAs <- map(x$data, ~ filter(.x$events, !!!dots) %>% 
  #     mutate( sample = map2(sample, sample + size - 1,seq)) %>% 
  #     unnest %>%
  #     select(channel, sample)

   # x$data <- map(x$data, ~ 
   #                  list(signals = .x$signals[.x$signals$], Fp1 = 
   #                    case_when(between(sample, ) )  
   #                       events = filter(.x$events, 
   #                        sample >= min(.x$signals$sample) - size, 
   #                                sample <= max(.x$signals$sample))))

  # x$data <- map(x$data, ~ mutate_at(.x,chan_names(.x),
  # funs(if_else(sample %in% select(filter( NAs, is.na(where)),sample), NA_real_,. )) )


  # x 
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

# times0 <- heavy_eeg$data$autoocularICA1.vmrk$events %>% filter(description == "s10") %>%
# select(sample) %>% unlist
# names(times0) <- NULL
# dfs <- map_dfr(times0, 
#   ~ heavy_eeg$data$autoocularICA1.vmrk$signal %>% 
#   filter(sample >= .x - 1 * 500, sample <= .x + 1 * 500 ) %>% 
#    mutate(time = sample/500 -.x) %>% select(sample, time, everything()),
#    .id = "segment"
#    )

# dfl <- map(times0, 
#   ~ heavy_eeg$data$autoocularICA1.vmrk$signal %>% 
#   filter(sample >= .x - 1 * 500, sample <= .x + 1 * 500 ) %>% 
#    mutate(time = sample/500 -.x) %>% select(sample, time, everything())
#    )

# pryr::object_size(dfs)
# pryr::object_size(dfl)


#   times0 <- map(heavy_eeg$data, ~.x$events %>% 
# filter(description == "s10") %>% select(segment = sample) %>% unlist)


# map2(.x= times0, .y= heavy_eeg$data,
#   function(t0 , df) map(.x=t0, ~ df$signals %>%
#     filter(sample >= .x - 1 * 500, sample <= .x + 1 * 500 ) %>%
#     mutate(time = sample/500 -.x) %>% select(sample, time, everything())
#     ) %>% {list(signals = .,  events = df$events)} )




decimals <- function(x) match(TRUE, round(x, 1:20) == x)

segment.eegbl <- function(eegble, ..., lim = c(-1,1)){

  dots <- rlang::enquos(...)

  # creates a list of vector with the times zero for each "file"
  times0 <- map(eegble$data, ~.x$events %>% 
    filter(!!!dots) %>% select(segment = sample) %>% unlist)
   
  # for each vector of times zero (t0) associated with the data of each file   
  eegble$data <- map2(.x= times0, .y= eegble$data,
  # iterate over the vector of times zeros (in sample)
  function(t0 , df) map(.x=t0, ~ df$signals %>%
    # filter the relevant samples
    filter(sample >= .x + lim[1] * srate(eegble), 
           sample <= .x + lim[2] * srate(eegble)) %>%
    # add a time column
    mutate(time = round(sample/srate(eegble) -.x/srate(eegble), 
      decimals(1/srate(eegble)))) %>% 
    # order the signals df:
    select(sample, time, everything())
    # reconstruct
    ) %>% {list(signals = .,  events = df$events)} )

eegble
}


bind <- function(eegble1, eegble2){

#validate
eegble1$data <- c(eegble1$data,eegble2$data) 
eegble1
}


as_long_dataframe <- function(eegble, chans) {
  chan_rv <- setdiff(chan_names(eegble),chans)
  
  map(eegble$data, ~ 
    map_dfr(.x$signals , .id = "segment", ~ .x) %>% 
    select(-one_of(chan_rv)) %>%
    gather(key = channel, value = amplitude, one_of(chans)) ) %>%
  map_dfr(.id = "file", ~ .x)

    
}

# plot_chan <- function(eegble, chans, ...) {
#   #? allow formulas as chans? Pz + Fz + mean(Fz)
  
#   chan_rv <- setdiff(chan_names(eegble),chans)
  
#   eegble_long <-  map(eegble$data, ~ 
#     map_dfr(.x$signals , .id = "segment", ~ .x) %>% 
#     select(-one_of(chan_rv)) %>%
#     gather(key = channel, value = amplitude, one_of(chans)) )

    


#   ggplot(eegble_long[[1]], aes(x = time, y = amplitude, group=channel)) +
#             scale_x_continuous(name = "Time (ms)") + 
#             scale_y_continuous(name = "Amplitude") + 
#             geom_line(aes(group = segment), alpha = .5)
# }
