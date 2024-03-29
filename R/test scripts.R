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

sit_reach_plot + 
  guides(color = "none")

(sit_reach_plot + 
lactate_plot +
borg_rpe_plot +
heart_rate_plot +
cmj_plot +
sprint_plot +
imtp_plot)

((heart_rate_plot + guides(color = "none")) / (lactate_plot  | borg_rpe_plot))  +
  plot_layout(guides = "collect")  +
  plot_annotation(
    tag_levels = "A",
    title = "Effects of Warmup on Physiological and Perceptual Outcomes"
  ) & 
  theme(legend.position = "bottom")


ggsave("warmup.tiff",
       device = "tiff",
       dpi = 300,
       w = 10,
       h = 10)


((sit_reach_plot + guides(color = "none")) + cmj_plot + sprint_plot + imtp_plot) +  
plot_layout(guides = "collect")  +
  plot_annotation(
    tag_levels = "A",
    title = "Effects of Warmup on Performance Outcomes"
  ) & 
  theme(legend.position = "bottom")

ggsave("perform.tiff",
       device = "tiff",
       dpi = 300,
       w = 10,
       h = 10)





ggsave("sit_reach_plot.tiff",
       device = "tiff",
       dpi = 300,
       w = 5,
       h = 2.5)




