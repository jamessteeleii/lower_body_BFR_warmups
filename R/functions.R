##### Functions for targets

### Read and prepare datasets

read_data <- function(file) {
  read_csv(file) |>
    janitor::clean_names()
}

prepare_sit_reach_data <- function(data) {
  sit_reach_data <- data |>
    select(subject_number, contains("sit_reach")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "x", "y", "timepoint"),
             sep = "_") |>
    select(-x, -y) |>
    separate(timepoint,
             into = c("timepoint", "measurement"),
             sep = "(?<=[A-Za-z])(?=[0-9])") |>
    mutate(timepoint = as.numeric(case_when(
      timepoint == "pre" ~ 0,
      timepoint == "post" ~ 1
    ))) |>
    mutate(
      condition = factor(condition, levels = c("no", "low", "high")),
      measurement = as.numeric(measurement)
    )
}

prepare_lactate_data <- function(data) {
  lactate_data <- data |>
    select(subject_number, contains("lact")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "timepoint", "x"),
             sep = "_") |>
    select(-x)  |>
    mutate(timepoint = as.numeric(case_when(
      timepoint == "pre" ~ 0,
      timepoint == "post" ~ 1
    ))) |>
    mutate(condition = factor(condition, levels = c("no", "low", "high")))
}

prepare_borg_rpe_data <- function(data) {
  borg_rpe_data <- data |>
    select(subject_number, contains("rpe")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "x", "y"),
             sep = "_") |>
    select(-x, -y)  |>
    mutate(condition = factor(condition, levels = c("no", "low", "high")))
}


prepare_heart_rate_data <- function(data) {
  heart_rate_data <- data |>
    select(subject_number, contains("hr")) |>
    select(-contains("mean")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "minute"),
             sep = "_") |>
    separate(minute,
             into = c("x", "minute"),
             sep = "(?<=[A-Za-z])(?=[0-9])") |>
    select(-x) |>
    mutate(condition = factor(condition, levels = c("no", "low", "high")),
           minute = as.numeric(minute))
}

prepare_cmj_data <- function(data) {
  cmj_data <- data |>
    select(subject_number, contains("cmj")) |>
    select(-contains("mean")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "measurement"),
             sep = "_") |>
    separate(measurement,
             into = c("x", "measurement"),
             sep = "(?<=[A-Za-z])(?=[0-9])") |>
    select(-x) |>
    mutate(condition = factor(condition, levels = c("no", "low", "high")))
}

prepare_sprint_data <- function(data) {
  sprint_data <- data |>
    select(subject_number, contains("m_")) |>
    select(-contains("mean")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "distance", "measurement"),
             sep = "_") |>
    mutate(condition = factor(condition, levels = c("no", "low", "high"))) |>
    mutate(distance = as.numeric(
      case_when(
        distance == "10m" ~ 10,
        distance == "20m" ~ 20,
        distance == "30m" ~ 30
      )
    ))
}

prepare_imtp_data <- function(data) {
  imtp_data <- data |>
    select(subject_number, contains("imtp")) |>
    select(-contains("mean")) |>
    pivot_longer(
      cols = !subject_number,
      names_to = "name",
      values_to = "value"
    ) |>
    separate("name",
             into = c("condition", "x", "measurement"),
             sep = "_") |>
    select(-x) |>
    separate(measurement,
             into = c("x", "measurement"),
             sep = "(?<=[A-Za-z])(?=[0-9])") |>
    select(-x)  |>
    mutate(condition = factor(condition, levels = c("no", "low", "high")))
}

### Fit models for each outcome

fit_sit_reach_model <- function(data) {
  sit_reach_model <-
    lmer(
      value ~ condition * timepoint + measurement + (condition * timepoint + measurement |
                                                       subject_number),
      data = data,
      REML = TRUE,
      control = lmerControl(calc.derivs = FALSE)
    ) # to enable convergence with condition and condition:minute random slopes
}

fit_lactate_model <- function(data) {
  lactate_model <-
    lmer(log(value) ~ condition * timepoint + (1 | subject_number),
         data = data,
         REML = TRUE)
}

fit_borg_rpe_model <- function(data) {
  borg_rpe_model <- lmer(value ~ condition + (1 | subject_number),
                         data = data,
                         REML = TRUE)
}

fit_heart_rate_model <- function(data) {
  heart_rate_model <-
    lmer(
      value ~ condition * minute + (condition * minute | subject_number),
      data = data,
      REML = TRUE,
      control = lmerControl(calc.derivs = FALSE)
    ) # to enable convergence with condition and condition:minute random slopes
}

fit_cmj_model <- function(data) {
  cmj_model <- lmer(value ~ condition + (condition | subject_number),
                    data = data,
                    REML = TRUE)
}

fit_sprint_model <- function(data) {
  sprint_model <-
    lmer(
      value ~ condition * distance + (condition + distance |
                                        subject_number),
      data = data,
      REML = TRUE
    ) # no interaction random slopes due to singularity
  
}

fit_imtp_model <- function(data) {
  imtp_model <- lmer(value ~ condition + (condition | subject_number),
                     data = data,
                     REML = TRUE)
}

### Create plots for each model

plot_sit_reach <- function(data, model) {
  sit_reach_pred <- tibble(
    ggeffects::ggpredict(
    model, 
    terms = c(
      "timepoint [0,1]", "condition"
    )
  )
  ) |>
    rename("timepoint" = x,
           "condition" = group) |>
    mutate(timepoint = factor(
      case_when(
        timepoint == 0 ~ "Pre",
        timepoint == 1 ~ "Post"
      ),
      levels = c("Pre", "Post")
    ),
    condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  data <- data |>
    mutate(timepoint = factor(
      case_when(
        timepoint == 0 ~ "Pre",
        timepoint == 1 ~ "Post"
      ),
      levels = c("Pre", "Post")
    ),
    condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  sit_reach_pred |>
    ggplot(aes(x=factor(timepoint), color=condition)) +
    geom_line(data = data,
              aes(y=value, group=interaction(subject_number, condition, measurement)),
              position = position_jitter(w=0.05, h=0),
              alpha = 0.5) +
    geom_line(aes(x=factor(timepoint), y = predicted, group = condition),
              position = position_nudge(x = c(-0.1,-0.1,0,0,0.1,0.1)),
              size = 1.5, color = "black") +
    geom_line(aes(x=factor(timepoint), y = predicted, group = condition),
              position = position_nudge(x = c(-0.1,-0.1,0,0,0.1,0.1)),
              size = 1) +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low-0.1,
                        ymax = conf.high+0.1),
                    position = position_nudge(x = c(-0.1,0,0.1)),
                    size = 0.7, linewidth = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low,
                        ymax = conf.high),
                    position = position_nudge(x = c(-0.1,0,0.1)),
                    size = 0.5, linewidth = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Timepoint",
      y = "Sit and Reach (cm)",
      color = "Condition"
    ) +
    theme_bw()
}

plot_lactate <- function(data, model) {
  lactate_pred <- tibble(
    ggeffects::predict_response(
    model, 
    terms = c(
      "timepoint [0,1]", "condition"
    )
  )
  ) |>
    rename("timepoint" = x,
           "condition" = group) |>
    mutate(timepoint = factor(
      case_when(
        timepoint == 0 ~ "Pre",
        timepoint == 1 ~ "Post"
      ),
      levels = c("Pre", "Post")
    ),
    condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  data <- data |>
    mutate(timepoint = factor(
      case_when(
        timepoint == 0 ~ "Pre",
        timepoint == 1 ~ "Post"
      ),
      levels = c("Pre", "Post")
    ),
    condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  lactate_pred |>
    ggplot(aes(x=factor(timepoint), color=condition)) +
    geom_line(data = data,
              aes(y=value, group=interaction(subject_number, condition)),
              position = position_jitter(w=0.05, h=0),
              alpha = 0.5) +
    geom_line(aes(x=factor(timepoint), y = predicted, group = condition),
              position = position_nudge(x = c(-0.1,-0.1,0,0,0.1,0.1)),
              size = 1.5, color = "black") +
    geom_line(aes(x=factor(timepoint), y = predicted, group = condition),
              position = position_nudge(x = c(-0.1,-0.1,0,0,0.1,0.1)),
              size = 1) +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low-0.01,
                        ymax = conf.high+0.01),
                    position = position_nudge(x = c(-0.1,0,0.1)),
                    size = 0.7, linewidth = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low,
                        ymax = conf.high),
                    position = position_nudge(x = c(-0.1,0,0.1)),
                    size = 0.5, linewidth = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Timepoint",
      y = bquote("Blood Lactate (mmol."~L^-1~")"),
      color = "Condition"
    ) +
    theme_bw()
}

plot_borg_rpe <- function(data, model) {
  borg_rpe_pred <- tibble(
    ggeffects::predict_response(
    model, 
    terms = c(
      "condition"
    )
  )
  ) |>
    rename("condition" = x) |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    ) |>
    filter(!is.na(condition))
  
  data <- data |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  borg_rpe_pred |>
  ggplot(aes(x=condition, color=condition)) +
    geom_line(data = data,
              aes(y=value, group = subject_number),
              position = position_jitter(w=0.05, h=0),
              alpha = 0.5, color = "black") +
    geom_line(aes(y = predicted, group = 1),
              size = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low-0.05,
                        ymax = conf.high+0.05),
                    size = 0.7, linewidth = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low,
                        ymax = conf.high),
                    size = 0.5, linewidth = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Condition",
      y = "Rating of Perceived Exertion (6-20 scale)",
    ) +
    scale_y_continuous(limits = c(6,20), breaks = 6:20) +
    guides(
      color = "none"
    ) +
    theme_bw()
}

plot_heart_rate <- function(data, model) {
  heart_rate_pred <- tibble(
    ggeffects::ggpredict(
      model, 
      terms = c(
        "minute [1:18]", "condition"
      )
    )
  ) |>
    rename("minute" = x,
           "condition" = group) |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  data <- data |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  heart_rate_pred |>
    ggplot(aes(x=minute, color=condition)) +
    geom_line(data = data,
              aes(y=value, group=interaction(subject_number, condition)),
              alpha = 0.5) +
    geom_ribbon(aes(x=minute, ymin = conf.low, ymax = conf.high, fill = condition),
                size = 1, alpha = 0.5, color = NA) +
    geom_line(aes(x=minute, y = predicted, group = condition),
              size = 1.5, color = "black") +
    geom_line(aes(x=minute, y = predicted, group = condition),
              size = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    scale_fill_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Time (minutes)",
      y = bquote("Heart Rate (beats.minute"^-1~")"),
      color = "Condition"
    ) +
    guides(
      fill = "none"
    ) +
    theme_bw()
}

plot_cmj <- function(data, model) {
  cmj_pred <- tibble(
    ggeffects::ggpredict(
      model, 
      terms = c(
        "condition"
      )
    )
  ) |>
    rename("condition" = x) |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  data <- data |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  cmj_pred |>
    ggplot(aes(x=condition, color=condition)) +
    geom_line(data = data,
              aes(y=value, group=interaction(subject_number, measurement)),
              position = position_jitter(w=0.05, h=0),
              alpha = 0.5, color = "black") +
    geom_line(aes(y = predicted, group = 1),
              size = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low-0.1,
                        ymax = conf.high+0.1),
                    size = 0.7, linewidth = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low,
                        ymax = conf.high),
                    size = 0.5, linewidth = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Condition",
      y = "Countermovement Jump Height (cm)",
    ) +
    guides(
      color = "none"
    ) +
    theme_bw()
}

plot_sprint <- function(data, model) {
  sprint_pred <- tibble(
    ggeffects::ggpredict(
      model, 
      terms = c(
        "distance [10,20,30]", "condition"
      )
    )
  ) |>
    rename("distance" = x,
           "condition" = group) |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  data <- data |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  sprint_pred |>
    ggplot(aes(x=distance, color=condition)) +
    geom_line(data = data,
              aes(y=value, group=interaction(subject_number, condition, measurement)),
              position = position_jitter(w=0.05, h=0),
              alpha = 0.25) +
    geom_line(aes(x=distance, y = predicted, group = condition),
              position = position_nudge(x = c(-1,-1,-1,0,0,0,1,1,1)),
              size = 1, color = "black") +
    geom_line(aes(x=distance, y = predicted, group = condition),
              position = position_nudge(x = c(-1,-1,-1,0,0,0,1,1,1)),
              size = 0.75) +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low-0.01,
                        ymax = conf.high+0.01),
                    position = position_nudge(x = c(-1,0,1)),
                    size = 0.5, linewidth = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low,
                        ymax = conf.high),
                    position = position_nudge(x = c(-1,0,1)),
                    size = 0.25, linewidth = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Distance (meters)",
      y = "Time (seconds)",
      color = "Condition"
    ) +
    guides(
      fill = "none"
    ) +
    theme_bw()
}

plot_imtp <- function(data, model) {
  imtp_pred <- tibble(
    ggeffects::ggpredict(
      model, 
      terms = c(
        "condition"
      )
    )
  ) |>
    rename("condition" = x) |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  data <- data |>
    mutate(condition = factor(
      case_when(
        condition == "no" ~ "Control",
        condition == "low" ~ "Low BFR",
        condition == "high" ~ "High BFR"
      ),
      levels = c("Control", "Low BFR", "High BFR")
    )
    )
  
  imtp_pred |>
    ggplot(aes(x=condition, color=condition)) +
    geom_line(data = data,
              aes(y=value, group=interaction(subject_number, measurement)),
              position = position_jitter(w=0.05, h=0),
              alpha = 0.5, color = "black") +
    geom_line(aes(y = predicted, group = 1),
              size = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low-0.1,
                        ymax = conf.high+0.1),
                    size = 0.7, linewidth = 1.5, color = "black") +
    geom_pointrange(aes(y = predicted,
                        ymin = conf.low,
                        ymax = conf.high),
                    size = 0.5, linewidth = 1) +
    scale_color_manual(values = c("#009E73", "#0072B2", "#D55E00" )) +
    labs(
      x = "Condition",
      y = "Isometric Mid-Thigh Pull Force (N)",
    ) +
    guides(
      color = "none"
    ) +
    theme_bw()
}

### Combine plots for manuscript

combine_warmup_plots <- function(heart_rate_plot, lactate_plot, borg_rpe_plot) {
  ((heart_rate_plot + guides(color = "none")) / (lactate_plot  | borg_rpe_plot))  +
    plot_layout(guides = "collect")  +
    plot_annotation(
      tag_levels = "A",
      title = "Effects of Warmup on Physiological and Perceptual Outcomes"
    ) & 
    theme(legend.position = "bottom")
}

combine_performance_plots <- function(sit_reach_plot, cmj_plot, sprint_plot, imtp_plot) {
  ((sit_reach_plot + guides(color = "none")) + cmj_plot + sprint_plot + imtp_plot) +  
    plot_layout(guides = "collect")  +
    plot_annotation(
      tag_levels = "A",
      title = "Effects of Warmup on Performance Outcomes"
    ) & 
    theme(legend.position = "bottom")
}