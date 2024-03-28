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
