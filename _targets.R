# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(
  packages = c(
    "here",
    # "readxl",
    # "janitor",
    "tidyverse",
    "base",
    "lme4",
    "lmerTest",
    # "faux",
    # "furrr",
    # "scales",
    # "ggtext",
    "ggh4x",
    # "zoo",
    "performance",
    # "see",
    # "rstan",
    # "brms",
    # "bayesplot",
    # "marginaleffects",
    "broom.mixed",
    "patchwork",
    # "kableExtra",
    "knitr",
    "quarto"
    # "officer",
    # "officedown",
  )
)


list(
  # Read and prepare datasets
  tar_target(data_file, here("data","Overall.csv"), format = "file"),
  tar_target(data, read_data(data_file)),
  tar_target(sit_reach_data, prepare_sit_reach_data(data)),
  tar_target(lactate_data, prepare_lactate_data(data)),
  tar_target(borg_rpe_data, prepare_borg_rpe_data(data)),
  tar_target(heart_rate_data, prepare_heart_rate_data(data)),
  tar_target(cmj_data, prepare_cmj_data(data)),
  tar_target(sprint_data, prepare_sprint_data(data)),
  tar_target(imtp_data, prepare_imtp_data(data)),
  
  # Fit models
  tar_target(sit_reach_model, fit_sit_reach_model(sit_reach_data)),
  tar_target(lactate_model, fit_lactate_model(lactate_data)),
  tar_target(borg_rpe_model, fit_borg_rpe_model(borg_rpe_data)),
  tar_target(heart_rate_model, fit_heart_rate_model(heart_rate_data)),
  tar_target(cmj_model, fit_cmj_model(cmj_data)),
  tar_target(sprint_model, fit_sprint_model(sprint_data)),
  tar_target(imtp_model, fit_imtp_model(imtp_data)),
  
  # Tidy models
  tar_target(sit_reach_model_tidy, tidy(sit_reach_model, conf.int = TRUE)),
  tar_target(lactate_model_tidy, tidy(lactate_model, conf.int = TRUE)),
  tar_target(borg_rpe_model_tidy, tidy(borg_rpe_model, conf.int = TRUE)),
  tar_target(heart_rate_model_tidy, tidy(heart_rate_model, conf.int = TRUE)),
  tar_target(cmj_model_tidy, tidy(cmj_model, conf.int = TRUE)),
  tar_target(sprint_model_tidy, tidy(sprint_model, conf.int = TRUE)),
  tar_target(imtp_model_tidy, tidy(imtp_model, conf.int = TRUE)), 
  
  # Make plots
  tar_target(sit_reach_plot, plot_sit_reach(sit_reach_data,sit_reach_model)),
  tar_target(lactate_plot, plot_lactate(lactate_data,lactate_model)),
  tar_target(borg_rpe_plot, plot_borg_rpe(borg_rpe_data,borg_rpe_model)),
  tar_target(heart_rate_plot, plot_heart_rate(heart_rate_data,heart_rate_model)),
  tar_target(cmj_plot, plot_cmj(cmj_data,cmj_model)),
  tar_target(sprint_plot, plot_sprint(sprint_data,sprint_model)),
  tar_target(imtp_plot, plot_imtp(imtp_data,imtp_model)),
  
  # Combine plots for manuscript
  tar_target(warmup_plot, combine_warmup_plots(heart_rate_plot, lactate_plot, borg_rpe_plot)),
  tar_target(performance_plot, combine_performance_plots(sit_reach_plot, cmj_plot, sprint_plot, imtp_plot)),
  
  # Save plots as tiffs for manuscript
  tar_target(warmup_tiff, ggsave(plot = warmup_plot, filename = "plots/warmup_plot.tiff",
                                 dpi = 300, w = 10, h = 10)),
  tar_target(performance_tiff, ggsave(plot = performance_plot, filename = "plots/performance_plot.tiff",
                                 dpi = 300, w = 10, h = 10)),
  
  
  
  # 
  # # Model checks
  # tar_target(model_checks, make_model_checks_tiff(model)),
  # 
  # # # Diagnostic plots
  # tar_target(rhat_model, make_rhat_plot(model)),
  # tar_target(trace_model, make_trace_plots(model)),
  # tar_target(pp_check_model, make_pp_check(model)),
  # 
  # # Calculate thresholds and their agreement
  # tar_target(thresholds, calculate_thresholds(data)),
  # tar_target(thresholds_agree, calculate_thresholds_agree(thresholds)),
  # 
  # # Make and save plots
  # tar_target(individual_data_plot, plot_individual_data(data)),
  # tar_target(individual_data_plot_tiff, make_individual_data_plot_tiff(individual_data_plot)),
  # 
  # tar_target(model_plot, plot_model(data, model)),
  # tar_target(model_plot_tiff, make_model_plot_tiff(model_plot)),
  # 
  # tar_target(individual_preds_plot, plot_individual_preds(data, model)),
  # tar_target(individual_preds_plot_tiff, make_individual_preds_plot_tiff(individual_preds_plot)),
  # 
  # tar_target(main_plot, combine_plots(individual_data_plot, individual_preds_plot, model_plot, thresholds_agree_plot)),
  # 
  # tar_target(thresholds_agree_plot, plot_thresholds_agree(thresholds, thresholds_agree)),
  # tar_target(thresholds_agree_plot_tiff, make_thresholds_agree_plot_tiff(thresholds_agree_plot))

  ##### Reporting
  # Render the report
  # tar_quarto(report, "report.qmd")
  
  # Render the supplementary material
  tar_quarto(model_structures_checks_diagnostics, "model_structures_checks_diagnostics.qmd")


)