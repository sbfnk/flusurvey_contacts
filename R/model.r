library("dplyr")
library("magrittr")
library("lubridate")
library("brms")
library("docopt")
library("here")

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

doc <- "Script for modelling flusurvey contact data.

Usage: contact_model.r [options]

Options:
  -t --type=<contact.type>             contact type
  -m --model=<model>                   model to use (base, variate, individual)
  -h --help                            print this help message
"

if (!interactive()) {
  opts <- docopt(doc)
  if (opts[["help"]]) {
    print(opts)
    exit()
  }
} else {
  opts <- list(
    type = "contacts",
    model = "random"
  )
}

type <- opts[["type"]]
model <- opts[["model"]]

contacts <- readRDS(here::here("data", "contacts.rds"))

dt_back_contacts <- contacts |>
  mutate(
    season_month = month(date) - 6,
    month_name = as.character(month(date, label = TRUE, abbr = FALSE))
  ) |>
  mutate(
    season_month = if_else(
      season_month > 0, season_month, season_month + 12
    )
  ) |>
  mutate(weekday = (day.of.week %in% 1:5))

colnames(dt_back_contacts) <- gsub("\\.", "_", colnames(dt_back_contacts))

participants <-
  data.frame(global_id=unique(dt_back_contacts$global_id)) |>
  mutate(new_participant_id=1:n())

contacts <- dt_back_contacts |>
  left_join(participants, by = "global_id") |>
  select(-global_id, contacts = !!sym(type)) |>
  rename(global_id = new_participant_id) |>
  filter(!is.na(contacts))

options <- list(
  family = negbinomial,
  data = contacts,
  chains = 2,
  cores = 2,
  iter = 4000
)

variables <- c(
  "agegroup", "weekday", "season_month", "month_name", "gender", "nb_household",
  "nb_household_children", "main_activity", "occupation", "highest_education",
  "current_symptoms", "urban_rural", "work_urban_rural", "enclosed_indoor_space",
  "public_transport"
)

if (model == "random") {
  formula <- 1
} else if (model == "variate") {
  formula <- paste(c(1, variables), collapse = "+")
} else if (model == "individual") {
  formula <- paste(c("(1 | global_id)", variables), collapse = "+")
} else {
  stop("Unknown model")
}

options[["formula"]] <- list(
  formula = formula(paste(type, formula, sep = "~"))
)

fit <- do.call(brm, options)

dir.create(here::here("res"), showWarnings = FALSE)
saveRDS(fit, here::here("res", paste0(paste(type, model, sep = "_"), ".rds")))
