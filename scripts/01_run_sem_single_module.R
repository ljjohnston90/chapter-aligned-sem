# scripts/01_run_sem_single_module.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/prep_sem_input.R")
source("R/sem_compute.R")

# ---- user inputs ----
module_year <- "MYMODULE_2324"# must end in 4 digits like 2324 for 2023-24 academic year

path_to_csv <- "path/to/your/MoodleLogsMarks_anon.csv"

# NOTE: keys must match the year code extracted from module_year (e.g., "2324")
start_dates <- c(
  "2223" = as.Date("2022-08-29"), #example
  "2324" = as.Date("2023-08-28"),
  "2425" = as.Date("2024-08-26")
)

# Read your export
raw <- readr::read_csv(path_to_csv, show_col_types = FALSE)

# Prepare data
dat <- prepare_sem_input(
  dat = raw,
  start_dates_named = start_dates,
  module_year = module_year,
  col_time = Time,
  col_event_context = "Event context",
  col_event_name = "Event name",
  col_component = Component,
  col_user = User,
  time_format = "%d/%m/%y, %H:%M:%S",
  max_chapter = 10,
  course_start_date = as.Date("2022-09-26"),
  course_end_date   = as.Date("2022-12-19"),
  override_fun = NULL   # optional hook
)

# ---- OPTIONAL: cohort filters (keep out of core method) ----
min_events <- 1
counts <- dat %>% count(User, name = "n_events")
dat <- dat %>% semi_join(counts %>% filter(n_events >= min_events), by = "User")

# ---- Choose which weeks to compute over ----
weeks_to_include <- sort(unique(dat$univ_week))


# Optional safety: keep only positive weeks
weeks_to_include <- weeks_to_include[weeks_to_include >= 1]

# ---- Run SEM ----
res <- compute_sem_over_weeks(
  dat = dat,
  weeks_to_include = weeks_to_include,
  prop_users_min = 0.05,
  chapter_ahead  = 2,
  w_freq = 1, w_div = 1, w_imm = 1,
  verbose = TRUE
)

# ---- Save outputs ----
out_dir <- file.path("output", "sem_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(res$idf_minmax_by_week, file.path(out_dir, paste0(module_year, "_idf_minmax_by_week.rds")))
saveRDS(res$idf_norm01_by_week, file.path(out_dir, paste0(module_year, "_idf_norm01_by_week.rds")))
saveRDS(res$sem_norm01_by_week, file.path(out_dir, paste0(module_year, "_sem_norm01_by_week.rds")))

message("Done. Saved outputs to: ", out_dir)

# ---- Sanity check: rowSums(IDF norm01) == SEM norm01 ----
idf_sem <- res$idf_norm01_by_week %>%
  mutate(sem_from_idf = rowSums(across(-c(User, academic_year, week)))) %>%
  select(User, academic_year, week, sem_from_idf)

chk <- idf_sem %>%
  left_join(res$sem_norm01_by_week, by = c("User","academic_year","week")) %>%
  mutate(abs_err = abs(sem_from_idf - sem))

print(summary(chk$abs_err))
cat("Max abs err:", max(chk$abs_err, na.rm = TRUE), "\n")


