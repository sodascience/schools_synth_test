# estimating simulated intervention effect using synthetic control method
# last edited 20230628 by @vankesteren
library(tidyverse)
library(tidysynth)
library(furrr)
plan(multisession, workers = 10)

# load data
schools_df <- read_rds("processed_data/schools_df.rds")
intervention_year <- 2017 # the first year post-intervention

# get intervention schools
intervened_ids <- schools_df |> filter(intervened) |> pull(school_id) |> unique()

# make schools df smaller for testing purposes
# non_intervened_ids <- schools_df |> pull(school_id) |> setdiff(intervened_ids) |> head(50)
# schools_df <- schools_df |> filter(school_id %in% c(intervened_ids, non_intervened_ids))

# for each intervened unit, do synthetic control excluding the other intervened units
create_synth_obj <- function(i_school) {
  schools_df |>
    filter(!school_id %in% setdiff(intervened_ids, i_school)) |>
    synthetic_control(
      outcome = result,
      unit = school_id,
      time = peiljaar,
      i_unit = i_school,
      i_time = intervention_year - 1,
      generate_placebos = FALSE
    )
}

# first, create a list of synth objects
synth_list <- map(intervened_ids, create_synth_obj, .progress = TRUE)
names(synth_list) <- intervened_ids

# create pre-intervention student number and poverty indicators
# for synthetic control matching
synth_list <-
  map(
    .x = synth_list,
    .f = generate_predictor,
    time_window = 2011:2016,
    students = mean(students),
    poverty = mean(poverty),
    result = mean(result),
    .progress = TRUE
  )

# create 2013 results indicator
synth_list <-
  map(
    .x = synth_list,
    .f = generate_predictor,
    time_window = 2013,
    result_2013 = result,
    .progress = TRUE
  )

# create 2016 results indicator
synth_list <-
  map(
    .x = synth_list,
    .f = generate_predictor,
    time_window = 2016,
    result_2016 = result,
    .progress = TRUE
  )

# estimate weights
synth_list <-
  future_map(
    .x = synth_list,
    .f = generate_weights,
    Margin.ipop = .02,
    Sigf.ipop = 7,
    Bound.ipop = 6,
    .progress = TRUE
  )

synth_list <-
  map(
    .x = synth_list,
    .f = generate_control,
    .progress = TRUE
  )


write_rds(synth_list, "processed_data/synth_list.rds")


iwalk(synth_list, \(s, n) {
    fn <- paste0("img/trend_plots/trend_", n, ".png")
    plt <- plot_trends(s) + ggtitle(paste("Intervention trend for school", n))
    ggsave(fn, plt, width = 9, height = 6, bg = "white")
  },
  .progress = TRUE
)

# inspect how far way the synth is from the true
balance_table <-
  synth_list |>
  map(\(s) grab_balance_table(s) |> select(-variable, -donor_sample), .progress = TRUE) |>
  bind_cols() |>
  mutate(variable = grab_predictors(synth_list[[1]])$variable) |>
  pivot_longer(-variable, names_to = "school_id") |>
  mutate(
    synthetic = factor(str_detect(school_id, "synthetic"),
                       levels = c(TRUE, FALSE),
                       labels = c("synthetic", "real")),
    school_id = str_remove(school_id, "synthetic_")
  )

balance_table |>
  ggplot(aes(x = synthetic, y = value, group = school_id)) +
  geom_line(colour = "grey") +
  geom_point(aes(fill = synthetic), colour = "white", pch = 21, size = 1.9) +
  facet_wrap(vars(variable), scales = "free") +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "black"), guide = "none") +
  labs(x = "", y = "", title = "Covariate balance for schools' synthetic controls",
       subtitle = "How successful was the matching procedure?")

ggsave("img/balance_plot.png", width = 9, height = 6, bg = "white")

compute_ace <- function(s) {
  s |>
    grab_synthetic_control() |>
    mutate(diff = real_y-synth_y) |>
    filter(time_unit >= intervention_year) |>
    pull(diff) |>
    mean()
}

aces <- map_dbl(synth_list, compute_ace, .progress = TRUE)
mean(aces)
sd(aces)


# there is a danger with coverage criterion: correlation between pre-intervention result and ACE
pre_result <-
  schools_df |>
  filter(school_id %in% intervened_ids, peiljaar >= intervention_year) |>
  summarise(mu = mean(result), .by = school_id) |>
  pull(mu)

tibble(pre_result, aces) |>
  ggplot(aes(x = pre_result, y = aces)) +
  geom_point() +
  theme_minimal() +
  labs(x)
