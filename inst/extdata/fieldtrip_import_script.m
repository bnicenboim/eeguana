addpath /usr/local/MATLAB/R2016b/toolbox/fieldtrip-20181003/

cfg = [];
cfg.dataset = '~/Desktop/asa_export/asalab_export_bv.eeg';
cfg.trialdef.eventtype = 'Stimulus'; % if not sure what this should be called, run just first 4 lines with '?' here incl. ft_definetrial and check output.
cfg.trialdef.eventvalue = {'s10'}; % ungram 1P 
cfg.trialdef.prestim = 0.2;
cfg.trialdef.poststim = 0.5;
cfg = ft_definetrial(cfg);

prepro = ft_preprocessing(cfg);