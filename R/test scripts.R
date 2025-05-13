library(tidyverse)
library(lme4)
library(patchwork)

targets::tar_load(c(
  sit_reach_data,
  lactate_data,
  borg_rpe_data,
  heart_rate_data,
  cmj_data,
  sprint_data,
  imtp_data,
  sit_reach_model,
  lactate_model,
  borg_rpe_model,
  heart_rate_model,
  cmj_model,
  sprint_model,
  imtp_model,
  sit_reach_model_tidy,
  lactate_model_tidy,
  borg_rpe_model_tidy,
  heart_rate_model_tidy,
  cmj_model_tidy,
  sprint_model_tidy,
  imtp_model_tidy,
  sit_reach_plot,
  lactate_plot,
  borg_rpe_plot,
  heart_rate_plot,
  cmj_plot,
  sprint_plot,
  imtp_plot
))

library(marginaleffects)

avg_comparisons(sit_reach_model)

plot_comparisons(sit_reach_model, condition = "condition", variables = "timepoint")

library(modelbased)


estimate_slopes(lactate_model, trend = "timepoint", at = "condition")


estimate_means(sprint_model)

estimate_contrasts(borg_rpe_model)
