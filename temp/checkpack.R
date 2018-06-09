devtools::load_all("/home/bruno/Documents/Working Papers/eegble")
library(tidyverse)
N400_CONG <- read_vhdr(file = "02_N400_CONG.vhdr")
N400_INC <- read_vhdr(file = "02_N400_INC.vhdr")


names(N400_CONG$data[[1]])

duration(N400_CONG)
summary(N400_CONG)



names(N400_CONG)

n400_paul <- bind(N400_CONG,N400_INC)

n400_paul

print(n400_paul,n=12, nsegs=1, nfiles = 1)

N400_c <- c("Cz", "CP1", "CP2", "P3", "Pz", "P4", "POz")

avsegs_df <- as_tibble(n400_paul, chans = N400_c) %>% 
  separate(id, c("subj","exp","cond","ext")) %>%
    group_by(time,channel,cond) %>% 
    summarize(A = mean(amplitude))


ggplot(avsegs_df, aes(x=time, y = A, color= cond)) + 
facet_wrap(~channel) + geom_line() + scale_y_reverse() +
coord_cartesian(ylim = c(-4, 4)) + geom_vline(xintercept=0, linetype = "dotted") +
ggtitle("one subj of Paul")

# ggsave(paul,file = "pauls.jpg")

#############################

av_faces <- read_vhdr(file = "faces/mariefacesasaold-avfaces.vhdr")
names(av_faces$data[[1]])

as_tibble(av_faces, chans = "Fp1")


seg_faces <- read_vhdr(file = "faces/mariefacesasaold-segmented.vhdr")
names(seg_faces$data[[1]])

seg_faces$data[[1]]$events %>% {unique(.$description)}

seg_faces_r <- event_to_NA(seg_faces, type == "Bad Interval")
seg_faces_r2 <- event_to_NA(seg_faces, type == "Bad Interval", 
                ind_channel=FALSE, entire_seg = TRUE)

seg_faces_r2$data[[1]]$signals

as_tibble(seg_faces_r2, chans = "Fp1") %>% 
        group_by(time,channel) %>% 
    summarize(A = mean(amplitude, na.rm=TRUE))



"mariefacesasaold-segmented-nobase.dat"


full_faces <- read_vhdr(file = "faces/mariefacesasaold-filtered-corrected.vhdr")

full_faces$data[[1]]$events %>% select(type) %>% unique
full_faces$data[[1]]$events %>% filter(type=="UserDefined")
#what does BV do with the blinks?

full_faces$data[[1]]$events %>% 
      dplyr::filter(type == "Time 0" ) %>% .$sample %>% length

full_faces$data[[1]]$events %>% 
      dplyr::filter(type == "Stimulus" ) 

x <- full_faces
sfull_faces <- segment(full_faces, description == "s70",  lim=c(-.2,.5))


bvsfull_faces <- read_vhdr(file ="faces/mariefacesasaold-segmented-nobase.vhdr")

sfull_faces$data[[1]]$signals[[1]]
bvsfull_faces$data[[1]]$signals[[1]]
bvsfull_faces$data[[1]]$events %>% select(description) %>% unique
sfull_faces$data[[1]]$events %>% select(description) %>% unique
sfull_faces$data[[1]]$events %>% filter(description=="Multiple") %>% unique

full_faces$data[[1]]$events %>% filter(type=="Stimulus")
#-36.16 in segmentation menu
#-35.73
sfull_faces$data[[1]]$signals[[1]] %>% filter(time==0) %>% select(Fp1) %>% unlist()
bvsfull_faces$data[[1]]$signals[[1]] %>% filter(sample==101) %>% select(Fp1) %>% unlist()

full_faces$data[[1]]$signals[[1]] %>% filter(sample == 40885)
