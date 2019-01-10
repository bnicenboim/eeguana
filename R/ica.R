# http://arnauddelorme.com/ica_for_dummies/

# Makeig, S., Bell, A. J., Jung, T.-P., & Sejnowski, T. J. (1996).
# Independent component analysis of electroencephal- ographic data. In D.
# Touretzky, M. Mozer & M. Hasselmo (Eds.), Advances in Neural Information
# Processing Systems 8 (pp. 145-151). Cambridge, MA: MIT Press.



# And I believe this is the first published use of ICA for EEG artifact
# correction:

# Jung, T.-P., Makeig, S., Humphries, C., Lee, T. W., McKeown, M. J.,
# Iragui, V. J., et al. (2000). Removing electroencephalographic artifacts
# by blind source separation. Psychophysiology, 37(2), 163-178.


# http://www.fieldtriptoolbox.org/example/use_independent_component_analysis_ica_to_remove_eog_artifacts

# eeglab:
#https://sccn.ucsd.edu/wiki/Chapter_09:_Decomposing_Data_Using_ICA

#another tutorial:
#https://www.nbtwiki.net/doku.php?id=tutorial:how_to_use_ica_to_remove_artifacts#.XBEabxBRdhE

# fast ica:

# http://research.ics.aalto.fi/ica/icademo/

# http://cnl.salk.edu/~tony/ica.html


#cpp code:


# https://github.com/ptillet/hf-ica/blob/master/lib/ica.cpp

# https://github.com/fieldtrip/fieldtrip/blob/master/external/eeglab/runica.m
#fieldtrip defaults:
# MAX_WEIGHT           = 1e8;       % guess that weights larger than this have blown up
# DEFAULT_STOP         = 0.000001;  % stop training if weight changes below this
# DEFAULT_ANNEALDEG    = 60;        % when angle change reaches this value,
# DEFAULT_ANNEALSTEP   = 0.90;      %     anneal by multiplying lrate by this
# DEFAULT_EXTANNEAL    = 0.98;      %     or this if extended-ICA
# DEFAULT_MAXSTEPS     = 512;       % ]top training after this many steps 
# DEFAULT_MOMENTUM     = 0.0;       % default momentum weight

# DEFAULT_BLOWUP       = 1000000000.0;   % = learning rate has 'blown up'
# DEFAULT_BLOWUP_FAC   = 0.8;       % when lrate 'blows up,' anneal by this fac
# DEFAULT_RESTART_FAC  = 0.9;       % if weights blowup, restart with lrate
#                                   % lower by this factor
# MIN_LRATE            = 0.000001;  % if weight blowups make lrate < this, quit
# MAX_LRATE            = 0.1;       % guard against uselessly high learning rate
# DEFAULT_LRATE        = 0.00065/log(chans); 
#                                   % heuristic default - may need adjustment
#                                   %   for large or tiny data sets!
# % DEFAULT_BLOCK        = floor(sqrt(frames/4));  % heuristic default 
# DEFAULT_BLOCK          = ceil(min(5*log(frames),0.3*frames)); % heuristic 
#                                   % - may need adjustment!
# % Extended-ICA option:
# DEFAULT_EXTENDED     = 0;         % default off
# DEFAULT_EXTBLOCKS    = 1;         % number of blocks per kurtosis calculation
# DEFAULT_NSUB         = 1;         % initial default number of assumed sub-Gaussians
#                                   % for extended-ICA
# DEFAULT_EXTMOMENTUM  = 0.5;       % momentum term for computing extended-ICA kurtosis
# MAX_KURTSIZE         = 6000;      % max points to use in kurtosis calculation
# MIN_KURTSIZE         = 2000;      % minimum good kurtosis size (flag warning)
# SIGNCOUNT_THRESHOLD  = 25;        % raise extblocks when sign vector unchanged
#                                   % after this many steps
# SIGNCOUNT_STEP       = 2;         % extblocks increment factor 

# DEFAULT_SPHEREFLAG   = 'on';      % use the sphere matrix as the default
#                                   %   starting weight matrix
# DEFAULT_INTERUPT     = 'off';     % figure interuption
# DEFAULT_PCAFLAG      = 'off';     % don't use PCA reduction
# DEFAULT_POSACTFLAG   = 'off';     % don't use posact(), to save space -sm 7/05
# DEFAULT_VERBOSE      = 1;         % write ascii info to calling screen
# DEFAULT_BIASFLAG     = 1;         % default to using bias in the ICA update rule
# DEFAULT_RESETRANDOMSEED = true;   % default to reset the random number generator to a 'random state'


# channels of
# the data set must not be linearly dependent.

# If you have data containing a reference
# channel, you should exclude this channel from the calculation in order to obtain usable re-
# sults.

# .eeg_lst <- multiplexed_bin_bv2 %>% select(-M1, -M2)


# https://www.nmr.mgh.harvard.edu/mne/dev/generated/mne.preprocessing.infomax.html


# https://rstudio-pubs-static.s3.amazonaws.com/93614_be30df613b2a4707b3e5a1a62f631d19.html


# https://labeling.ucsd.edu/tutorial/labels

eeg_ica.eeg_lst <- function(.eeg_lst, 
                    ...,
          method = "infomax",
          configuration = 
            if(method=="infomax") {
              list(algorithm = "gradient",
              fun = "logistic",
              learning_rate = 0.00065/log(nchannels(.eeg_lst)),
              annealing_rate_angle = 60,
              annealing_rate_step = 0.90,
              max_iterations = 512) 
           } else if(method== "fastica"){
              list(algorithm = "parallel",
              fun = "logcosh",
              alpha = 1.0,
              tolerance = 1e-04,
              max_iterations = 512)
             }
                            )
{

  ncomponents = nchannels(.eeg_lst)
  dots <- rlang::enquos(...)
  signal_raw <- dplyr::select(.eeg_lst$signal, !!!dots)
  
  # rest of the channels and cols of signal for the later reconstruction
  non_ica_signal <- .eeg_lst$signal[,setdiff(colnames(.eeg_lst$signal), colnames(signal_raw)), with = FALSE]
  
  if(stringr::str_to_lower(method) %in% c("imax","infomax","im")){

    #     infomax <- ica::icaimax(signal_raw,
    #           nc = ncomponents,
    #            rate = configuration$learning_rate,
    #            rateanneal= c(configuration$annealing_rate_angle,configuration$annealing_rate_step),
    #           maxit = configuration$max_iterations, 
    #           alg  = configuration$algorithm,
    #           fun = configuration$fun)
    # S_ICA <- data.table::as.data.table(infomax$S)
    # ncomponents <- nrow(infomax$W)
    # mixing_matrix <- ica_res$M 
    # unmixing_matrix <- ica_res$W 

  } else if(stringr::str_to_lower(method) %in% c("fi","fastica","fica")){

    ica_res <- fastICA::fastICA(signal_raw, n.comp = ncomponents, alg.typ = configuration$algorithm,
        fun = configuration$fun, alpha = configuration$alpha, method = "C", #c("R","C"),
        row.norm = FALSE, maxit = configuration$max_iterations, tol = configuration$tolerance, verbose = FALSE,
        w.init = NULL)
    S_ICA <- ica_res$S
    ncomponents <- nrow(ica_res$W)
    mixing_matrix <- ica_res$A 
    
  }

  component_names <- paste0("C",seq_len(ncomponents))
  
  intercept <- colMeans(signal_raw)

#col_info instead of channel_info and without any obligatory attribute...
#ica_lst (signal, events, segments, mixing, unmixing with the channels info)

  reconstr <- (S_ICA %*% mixing_matrix + intercept) 
  message("Absolute difference between original data and reconstructed from independent sources: ", 
    mean(abs(reconstr - as.matrix(signal_raw))) %>% signif(2))

  # S <- cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
  # A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
  # X <- S %*% A
  # 
  # a <- fastICA::fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
  #              method = "R", row.norm = FALSE, maxit = 200,
  #              tol = 0.0001, verbose = TRUE)
  # a$S %*% a$A + colMeans(X)
  # X
  
  S_ICA <- data.table::as.data.table(S_ICA)
  data.table::setnames(S_ICA, component_names)
  S_ICA[,lapply(component, .SD)]
  
  # S_ICA[,.id := .eeg_lst$signal$.id][
  #                ,.sample_id := .eeg_lst$signal$.sample_id]
  
  
  mixing_tbl <- mixing_tbl(mixing_matrix = mixing_matrix,
                components = component_names, 
                channel_info = channels_tbl(signal_raw)) 

    ica_lst(signal = cbind(non_ica_signal, S_ICA), 
            mixing = mixing_tbl, 
            events = .eeg_lst$events, 
            segments = .eeg_lst$segments)

    #signal_out <- as.matrix(signal_raw) -  (dplyr::select(S_ICA,C2) %>% as.matrix() %*% mixing_matrix[,2])
  #  mean(abs(signal_out - as.matrix(signal_matrix)))

  
# if(0){
# unmixing_matrix <- unmixing_matrix %>% #Sometimes called W
#     `rownames<-`(component_names) %>%
#     `colnames<-`(channel_names(.eeg_lst))
# mixing_matrix %>%
#     dplyr::as_tibble()  %>%
#     dplyr::mutate(channel=rownames(mixing_matrix))  %>% 
#     tidyr::gather(component, amplitude, -channel) %>% 
#     dplyr::left_join(channels_tbl(.eeg_lst), by = "channel")  %>% 
#     group_by(component) %>% interpolate_tbl() %>% plot_topo() + facet_grid(~component)
#   }




}
