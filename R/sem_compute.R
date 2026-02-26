# R/sem_compute.R
# Core SEM computation using the SAME naming convention as the pipeline:
#   User, academic_year, univ_week, date, Session_ID, session_chap,
#   Event.context, Event.name, Component
#
# Returns:
#   - sem_norm01_by_week: user-week SEM in [0,1]
#   - idf_minmax_by_week: per-user-week chapter contribution table (raw weighted)
#   - idf_norm01_by_week: contribution table normalised so rowSums == sem_norm01
#   - chapters_keep_by_week / chapters_avail_by_week: diagnostics

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

minmax_scale_vec <- function(x) {
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  if (!is.finite(mn) || !is.finite(mx) || mx == mn) return(rep(0, length(x)))
  (x - mn) / (mx - mn)
}

# Convert per-chapter scaled components -> wide contribution table (one row per User-year-week)
make_idf_wide <- function(dat_scaled, week, w_freq, w_div, w_imm) {
  dat_scaled %>%
    mutate(
      week = week,
      freq_contrib = w_freq * freq_01,
      div_contrib  = w_div  * div_01,
      imm_contrib  = w_imm * imm_01
    ) %>%
    pivot_wider(
      id_cols     = c(User, academic_year, week),
      names_from  = Chapter,
      values_from = c(freq_contrib, imm_contrib, div_contrib),
      names_glue  = "{.value}..c.{sprintf('%02d', as.numeric(Chapter))}"
    ) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
}

compute_sem_for_week <- function(dat,
                                 week_val,
                                 prop_users_min = 0.05,
                                 chapter_ahead  = 2,
                                 w_freq = 1,
                                 w_div  = 1,
                                 w_imm  = 1) {
  
  req <- c("User","academic_year","univ_week","date","Session_ID","session_chap",
           "Event.context","Event.name","Component")
  stopifnot(all(req %in% names(dat)))
  
  dat_week <- dat %>%
    mutate(
      univ_week     = as.numeric(univ_week),
      date          = as.Date(date),
      User          = as.character(User),
      academic_year = as.character(academic_year),
      Chapter       = as.integer(session_chap)
    ) %>%
    filter(univ_week <= week_val, !is.na(Chapter))
  
  # cohort size per year in this time window
  cohort_by_year <- dat_week %>%
    distinct(academic_year, User) %>%
    count(academic_year, name = "cohort_users")
  
  # chapter prevalence + inclusion rules
  chapters_tbl <- dat_week %>%
    distinct(academic_year, Chapter, User) %>%
    group_by(academic_year, Chapter) %>%
    summarise(n_users = n_distinct(User), .groups = "drop") %>%
    left_join(cohort_by_year, by = "academic_year") %>%
    mutate(
      prop_users = n_users / cohort_users,
      keep_prop  = prop_users >= prop_users_min,
      keep_week  = Chapter <= (week_val + chapter_ahead),
      keep       = keep_prop & keep_week
    )
  
  chapters_keep <- chapters_tbl %>%
    filter(keep) %>%
    select(academic_year, Chapter)
  
  chapters_avail <- chapters_keep %>%
    group_by(academic_year) %>%
    summarise(n_chapters_avail = n(), .groups = "drop") %>%
    mutate(n_chapters_avail = if_else(is.na(n_chapters_avail) | n_chapters_avail <= 0, 1L, n_chapters_avail))
  
  # If nothing kept, return zeros safely
  if (nrow(chapters_keep) == 0) {
    all_students <- dat_week %>% distinct(User, academic_year)
    
    dat_scaled_empty <- all_students %>%
      mutate(Chapter = 1L, freq_01 = 0, imm_01 = 0, div_01 = 0) %>%
      select(User, academic_year, Chapter, freq_01, imm_01, div_01)
    
    sem_norm01 <- all_students %>%
      mutate(week = week_val, sem = 0)
    
    idf_minmax <- make_idf_wide(dat_scaled_empty, week = week_val, w_freq = w_freq, w_div = w_div, w_imm = w_imm)
    
    denom <- (w_freq + w_div + w_imm) * 1
    contrib_cols <- setdiff(names(idf_minmax), c("User","academic_year","week"))
    idf_norm01 <- idf_minmax %>%
      mutate(across(all_of(contrib_cols), ~ .x / denom))
    
    return(list(
      sem_norm01 = sem_norm01,
      idf_minmax = idf_minmax,
      idf_norm01 = idf_norm01,
      chapters_keep  = chapters_keep,
      chapters_avail = chapters_avail,
      chapters_drop  = chapters_tbl %>% filter(!keep)
    ))
  }
  
  # Restrict to kept chapters
  dat_week <- dat_week %>%
    semi_join(chapters_keep, by = c("academic_year","Chapter"))
  
  # Chapter-level components
  dat_metrics <- dat_week %>%
    arrange(User, Chapter, date) %>%
    group_by(User, Chapter, academic_year) %>%
    summarise(
      first_session = min(date, na.rm = TRUE),
      frequency     = n_distinct(Session_ID),
      .groups = "drop"
    )
  
  earliest_dates <- dat_week %>%
    group_by(Chapter, academic_year) %>%
    summarise(earliest_date = min(date, na.rm = TRUE), .groups = "drop")
  
  dat_metrics <- dat_metrics %>%
    left_join(earliest_dates, by = c("Chapter","academic_year")) %>%
    mutate(immediacy = as.numeric(first_session - earliest_date))
  
  diversity_metrics <- dat_week %>%
    group_by(User, academic_year, Chapter) %>%
    summarise(
      diversity = n_distinct(paste(Event.context, Event.name, Component, sep = "_")),
      .groups = "drop"
    )
  
  dat_metrics <- diversity_metrics %>%
    left_join(dat_metrics, by = c("User","academic_year","Chapter"))
  
  # Scale within (year, chapter)
  dat_scaled <- dat_metrics %>%
    group_by(academic_year, Chapter) %>%
    mutate(
      freq_01 = minmax_scale_vec(frequency),
      div_01  = minmax_scale_vec(diversity),
      imm_01  = 1 - minmax_scale_vec(immediacy)
    ) %>%
    ungroup() %>%
    select(User, academic_year, Chapter, freq_01, imm_01, div_01)
  
  # Contribution tables
  idf_minmax <- make_idf_wide(dat_scaled, week = week_val, w_freq = w_freq, w_div = w_div, w_imm = w_imm)
  
  # SEM raw then norm01
  sem_raw <- dat_scaled %>%
    mutate(idf = w_freq * freq_01 + w_imm * imm_01 + w_div * div_01) %>%
    group_by(User, academic_year) %>%
    summarise(sem_raw = sum(idf, na.rm = TRUE), .groups = "drop")
  
  denom_tbl <- chapters_avail %>%
    mutate(denom = (w_freq + w_div + w_imm) * n_chapters_avail)
  
  sem_norm01 <- sem_raw %>%
    left_join(denom_tbl, by = "academic_year") %>%
    transmute(User, academic_year, week = week_val, sem = sem_raw / denom)
  
  # Normalise the contribution table so rowSums == sem_norm01
  contrib_cols <- setdiff(names(idf_minmax), c("User","academic_year","week"))
  idf_norm01 <- idf_minmax %>%
    left_join(chapters_avail %>% mutate(week = week_val), by = c("academic_year","week")) %>%
    mutate(divisor = (w_freq + w_div + w_imm) * n_chapters_avail) %>%
    mutate(across(all_of(contrib_cols), ~ .x / divisor)) %>%
    select(-n_chapters_avail, -divisor)
  
  # ensure IDF tables have one row per (user, year, week) (even if all zeros)
  all_students <- dat_week %>% distinct(User, academic_year) %>% mutate(week = week_val)
  
  idf_minmax <- all_students %>%
    left_join(idf_minmax, by = c("User","academic_year","week")) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  idf_norm01 <- all_students %>%
    left_join(idf_norm01, by = c("User","academic_year","week")) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  list(
    sem_norm01 = sem_norm01,
    idf_minmax = idf_minmax,
    idf_norm01 = idf_norm01,
    chapters_keep  = chapters_keep,
    chapters_avail = chapters_avail,
    chapters_drop  = chapters_tbl %>% filter(!keep)
  )
}

compute_sem_over_weeks <- function(dat,
                                   weeks_to_include,
                                   prop_users_min = 0.05,
                                   chapter_ahead  = 2,
                                   w_freq = 1, w_div = 1, w_imm = 1,
                                   verbose = TRUE) {
  
  req <- c("User","academic_year","univ_week","date","Session_ID","session_chap",
           "Event.context","Event.name","Component")
  stopifnot(all(req %in% names(dat)))
  
  all_students <- dat %>% distinct(User, academic_year)
  
  sem_by_week <- vector("list", length(weeks_to_include))
  idf_minmax_by_week <- vector("list", length(weeks_to_include))
  idf_norm01_by_week <- vector("list", length(weeks_to_include))
  
  chapters_keep_list  <- vector("list", length(weeks_to_include))
  chapters_avail_list <- vector("list", length(weeks_to_include))
  
  nm <- as.character(weeks_to_include)
  names(sem_by_week) <- names(idf_minmax_by_week) <- names(idf_norm01_by_week) <- nm
  
  for (wk in weeks_to_include) {
    res <- compute_sem_for_week(
      dat = dat,
      week_val = wk,
      prop_users_min = prop_users_min,
      chapter_ahead  = chapter_ahead,
      w_freq = w_freq, w_div = w_div, w_imm = w_imm
    )
    
    if (isTRUE(verbose)) {
      cat("\nweek ", wk, " | chapters kept by year:\n", sep = "")
      print(
        res$chapters_keep %>%
          arrange(academic_year, Chapter) %>%
          group_by(academic_year) %>%
          summarise(chapters = paste(sort(unique(Chapter)), collapse = ", "), .groups = "drop"),
        n = Inf
      )
    }
    
    wk_keys <- res$idf_norm01 %>%
      distinct(User, academic_year, week)
    
    sem_by_week[[as.character(wk)]] <- wk_keys %>%
      left_join(res$sem_norm01, by = c("User","academic_year","week")) %>%
      mutate(sem = replace_na(sem, 0)) %>%
      select(User, academic_year, week, sem)
    
    idf_minmax_by_week[[as.character(wk)]] <- res$idf_minmax
    idf_norm01_by_week[[as.character(wk)]] <- res$idf_norm01
    
    chapters_keep_list[[as.character(wk)]]  <- res$chapters_keep  %>% mutate(week = wk)
    chapters_avail_list[[as.character(wk)]] <- res$chapters_avail %>% mutate(week = wk)
  }
  
  sem_out <- bind_rows(sem_by_week)
  
  idf_minmax_out <- bind_rows(idf_minmax_by_week) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  idf_norm01_out <- bind_rows(idf_norm01_by_week) %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  chap_keep_out  <- bind_rows(chapters_keep_list)
  chap_avail_out <- bind_rows(chapters_avail_list)
  
  list(
    sem_norm01_by_week = sem_out,
    idf_minmax_by_week = idf_minmax_out,
    idf_norm01_by_week = idf_norm01_out,
    chapters_keep_by_week  = chap_keep_out,
    chapters_avail_by_week = chap_avail_out
  )
}