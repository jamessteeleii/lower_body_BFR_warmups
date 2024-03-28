library(tidyverse)
library(lme4)


data <- read_csv("data/Overall.csv") |>
  janitor::clean_names() 

sit_reach_data <- data |>
  select(subject_number, contains("sit_reach")) |>
  pivot_longer(cols = !subject_number, names_to = "name", values_to = "value") |>
  separate("name", into = c("condition", "x", "y", "timepoint"), sep = "_") |>
  select(-x,-y) |>
  separate(timepoint, 
           into = c("timepoint", "measurement"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  ) |>
  mutate(
    timepoint = as.numeric(
      case_when(
        timepoint == "pre" ~ 0,
        timepoint == "post" ~1
      )
    )
  ) |>
  mutate(condition = factor(condition, levels= c("no", "low", "high")),
         measurement = as.numeric(measurement))

lactate_data <- data |>
  select(subject_number, contains("lact")) |>
  pivot_longer(cols = !subject_number, names_to = "name", values_to = "value") |>
  separate("name", into = c("condition", "timepoint", "x"), sep = "_") |>
  select(-x)  |>
  mutate(
    timepoint = as.numeric(
      case_when(
        timepoint == "pre" ~ 0,
        timepoint == "post" ~1
      )
    )
  ) |>
  mutate(condition = factor(condition, levels= c("no", "low", "high")))

heart_rate_data <- data |>
  select(subject_number, contains("hr")) |>
  select(-contains("mean")) |>
  pivot_longer(cols = !subject_number, names_to = "name", values_to = "value") |>
  separate("name", into = c("condition", "measurement"), sep = "_") |>
  separate(measurement, 
           into = c("x", "measurement"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  ) |>
  select(-x) |>
  mutate(condition = factor(condition, levels= c("no", "low", "high")),
         measurement = as.numeric(measurement))

cmj_data <- data |>
  select(subject_number, contains("cmj")) |>
  select(-contains("mean")) |>
  pivot_longer(cols = !subject_number, names_to = "name", values_to = "value") |>
  separate("name", into = c("condition", "measurement"), sep = "_") |>
  separate(measurement, 
           into = c("x", "measurement"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  ) |>
  select(-x) |>
  mutate(condition = factor(condition, levels= c("no", "low", "high")))


sprint_data <- data |>
  select(subject_number, contains("m_")) |>
  select(-contains("mean")) |>
  pivot_longer(cols = !subject_number, names_to = "name", values_to = "value") |>
  separate("name", into = c("condition", "distance", "measurement"), sep = "_") |>
  mutate(condition = factor(condition, levels= c("no", "low", "high")))

imtp_data <- data |>
  select(subject_number, contains("imtp")) |>
  select(-contains("mean")) |>
  pivot_longer(cols = !subject_number, names_to = "name", values_to = "value") |>
  separate("name", into = c("condition", "x", "measurement"), sep = "_") |>
  select(-x) |> 
  separate(measurement, 
           into = c("x", "measurement"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  ) |>
  select(-x)  |>
  mutate(condition = factor(condition, levels= c("no", "low", "high")))


### Fit models for each outcome

# sit and reach

sit_reach_model <- lmer(value ~ condition*timepoint + (1 | subject_number),
                        data = sit_reach_data,
                        REML = TRUE)

sjPlot::plot_model(sit_reach_model, type = "pred", terms = c("condition", "timepoint"),
                   show.data = TRUE, jitter = TRUE)

sjPlot::plot_model(sit_reach_model)

# lactate
lactate_model <- lmer(value ~ condition*timepoint + (1 | subject_number),
                        data = lactate_data,
                        REML = TRUE)


sjPlot::plot_model(lactate_model, type = "pred", terms = c("condition", "timepoint"),
                   show.data = TRUE, jitter = TRUE)

sjPlot::plot_model(lactate_model)

# heart rate
heart_rate_model <- lmer(value ~ condition*measurement + (measurement | subject_number),
                      data = heart_rate_data,
                      REML = TRUE)


sjPlot::plot_model(heart_rate_model, type = "pred", terms = c("measurement", "condition"),
                   show.data = TRUE, jitter = TRUE)

sjPlot::plot_model(heart_rate_model)

# cmj
cmj_model <- lmer(value ~ condition + (1 | subject_number),
                         data = cmj_data,
                         REML = TRUE)


sjPlot::plot_model(cmj_model, type = "pred", terms = "condition",
                   show.data = TRUE, jitter = TRUE)

sjPlot::plot_model(cmj_model)

# sprint
sprint_model <- lmer(value ~ condition*distance + (1 | subject_number),
                  data = sprint_data,
                  REML = TRUE)


sjPlot::plot_model(sprint_model, type = "pred", terms = c("distance", "condition"),
                   show.data = TRUE, jitter = TRUE)

sjPlot::plot_model(sprint_model)

# imtp
imtp_model <- lmer(value ~ condition + (1 | subject_number),
                  data = imtp_data,
                  REML = TRUE)


sjPlot::plot_model(imtp_model, type = "pred", terms = "condition",
                   show.data = TRUE, jitter = TRUE)

sjPlot::plot_model(imtp_model)
