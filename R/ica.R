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

eeg_ica <- function(.eeg_lst, 
          ncomponents = nchannels(.eeg_lst),
          method = "imax",
          algorithm = "gradient",
          squashing = "logistic",
          learning_rate = 0.00065/log(nchannels(.eeg_lst)),
          annealing_rate_angle = 60,
          annealing_rate_step = 0.90,
          max_iterations = 512)
{

  signal_raw <-select(.eeg_lst$signal, -.id, -.sample_id)

  ica_res <- ica::icaimax(signal_raw,
              nc = ncomponents,
              rate = learning_rate,
              rateanneal= c(annealing_rate_angle,annealing_rate_step),
              maxit = max_iterations, 
              alg  = "gradient",
              fun = "log")
 
  signal_ICA <- new_signal_tbl(signal_matrix = ica_res$S,ids = s_id,
                      sample_ids = sample_id, 
                      channel_info = channel_info)
#col_info instead of channel_info and without any obligatory attribute...
#ica_lst (signal, events, segments, mixing, unmixing with the channels info)
}
