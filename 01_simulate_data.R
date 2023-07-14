# simulating school panel data for estimating intervention effect
# last edited 20230628 by @vankesteren

library(tidyverse)
library(truncnorm)

# parameters
N_schools         <- 1000
N_intervened      <- 30
years             <- 2011:2020
intervention_year <- 2017 # the first year post-intervention

# intervention school properties (selection effects)
# i schools perform worse on outcome
i_school_result_delta <- -5
i_school_result_sd <- 0.5
# i schools have more poverty
i_school_poverty_delta <- 0.3
i_school_poverty_sd <- 0.07
# i schools have similar switchrate
i_school_switchrate_delta <- 0
i_school_switchrate_sd <- 0

# true average causal effect of intervention
intervention_effect <- 3
intervention_sd     <- 0.707

# set seed
set.seed(45)

# school id is a random hexadecimal string
generate_hexadecimal_ids <- function(N, id_len = 12) {
  sample_space <- c(0:9, letters[1:6])
  ids <- character(N)
  len <- 0L
  rej <- 0L
  while (len < N) {
    # simple rejection sampling
    id <- paste0(sample(sample_space, id_len, replace = TRUE), collapse = "")
    if (id %in% ids) {
      rej <- rej + 1
      cat("reject", rej, "\r")
      next
    }
    len <- len + 1
    ids[len] <- id
  }
  return(ids)
}

schools_df <- tibble(school_id = generate_hexadecimal_ids(N_schools, 12))

# add time-constant school info
schools_df <-
  schools_df |>
  mutate(
    intervened     = sample(c(rep(TRUE, N_intervened), rep(FALSE, N_schools - N_intervened))),
    avg_students   = as.integer(rnbinom(N_schools, size = 12, mu = 333)),
    avg_poverty    = rnorm(N_schools),
    avg_switchrate = rbeta(N_schools, 2, 9),
    avg_result     = as.integer(round(rtruncnorm(N_schools, a = 0, b = 550, mean = 530, sd = 10)))
  )

# add years and expand
schools_df <-
  schools_df |>
  mutate(peiljaar = list(years)) |>
  unnest_longer(peiljaar)

# create yearly variation and retain only yearly data
schools_df <-
  schools_df |>
  mutate(
    students = as.integer(round(rnorm(n(), mean = avg_students, sd = 30))),
    poverty  = rnorm(n(), mean = avg_poverty, sd = 0.05),
    switchrate = rtruncnorm(n(), a = 0, b = 1, mean = avg_switchrate, sd = 0.1),
    result     = rtruncnorm(n(), a = 0, b = 550, mean = avg_result, sd = 3)
  ) |>
  select(-starts_with("avg"))

# Adjust characteristics of intervention schools
intervention_schools <- schools_df |> filter(intervened)

# Edit intervention school results
intervention_schools$result <- as.integer(round(rtruncnorm(
  n = nrow(intervention_schools),
  a = 0,
  b = 550,
  mean = intervention_schools$result + i_school_result_delta,
  sd = i_school_result_sd
)))

# Edit intervention school poverty
intervention_schools$poverty <- rnorm(
  n = nrow(intervention_schools),
  mean = intervention_schools$poverty + i_school_poverty_delta,
  sd = i_school_poverty_sd
)

# Edit intervention school switchrate
intervention_schools$switchrate <- rnorm(
  n = nrow(intervention_schools),
  mean = intervention_schools$switchrate + i_school_switchrate_delta,
  sd = i_school_switchrate_sd
)


schools_df <-
  schools_df |>
  rows_update(intervention_schools, by = c("school_id", "peiljaar"))

# Actually make the intervention effect

# the intervention has a positive effect
post_intervention <- schools_df |> filter(intervened, peiljaar >= intervention_year)
post_intervention$result <- as.integer(round(rtruncnorm(
  n = nrow(post_intervention),
  a = 0,
  b = 550,
  mean = post_intervention$result + intervention_effect,
  sd = intervention_sd
)))


schools_df <-
  schools_df |>
  rows_update(post_intervention, by = c("school_id", "peiljaar"))

# write the dataset
write_rds(schools_df, "processed_data/schools_df.rds")

# plot the effect
schools_df |>
  filter(intervened) |>
  ggplot(aes(x = factor(peiljaar), y = result, group = school_id)) +
  geom_line(alpha = 30/N_schools, data = schools_df |> filter(!intervened)) +
  geom_point(alpha = 0.6) +
  geom_line(alpha = 0.6) +
  geom_vline(xintercept = intervention_year - min(years) + 0.5, linetype = 2, alpha = 0.6) +
  theme_minimal() +
  labs(
    x = "School year",
    y = "School result (average test score)",
    title = "Simulated test score of intervention schools",
    subtitle = paste("Average causal effect of", intervention_effect, "points starting in", intervention_year)
  )

ggsave("img/simulated_effect.png", width = 9, height = 6, bg = "white")


