# R/sem_plot.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

theme_sem_pres <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# ---- helpers ----
sem_filter_inputs <- function(dat, res, plot_users, ay_pick = NULL) {
  dat_use <- dat %>%
    mutate(
      User = as.character(User),
      academic_year = as.character(academic_year),
      date = as.Date(date),
      univ_week = as.integer(univ_week)
    ) %>%
    filter(User %in% plot_users)
  
  sem_use <- res$sem_norm01_by_week %>%
    mutate(User = as.character(User), academic_year = as.character(academic_year)) %>%
    filter(User %in% plot_users)
  
  idf_use <- res$idf_norm01_by_week %>%
    mutate(User = as.character(User), academic_year = as.character(academic_year)) %>%
    filter(User %in% plot_users)
  
  if (!is.null(ay_pick)) {
    dat_use <- dat_use %>% filter(academic_year == ay_pick)
    sem_use <- sem_use %>% filter(academic_year == ay_pick)
    idf_use <- idf_use %>% filter(academic_year == ay_pick)
  }
  
  list(dat = dat_use, sem = sem_use, idf = idf_use)
}

sem_make_calendar <- function(dat_use) {
  cal <- dat_use %>%
    filter(!is.na(date), !is.na(univ_week)) %>%
    distinct(date, univ_week)
  
  week_starts <- cal %>%
    group_by(univ_week) %>%
    summarise(week_start = min(date), .groups = "drop") %>%
    arrange(univ_week)
  
  list(cal = cal, week_starts = week_starts)
}

# ---- plots ----
plot_sem_daily_activity <- function(dat_use, plot_users, title = "Daily VLE activity") {
  cal_out <- sem_make_calendar(dat_use)
  cal <- cal_out$cal
  week_starts <- cal_out$week_starts
  
  daily <- dat_use %>%
    filter(User %in% plot_users) %>%
    count(User, date, name = "n_events") %>%
    group_by(User) %>%
    complete(
      date = seq(min(date), max(date), by = "day"),
      fill = list(n_events = 0)
    ) %>%
    ungroup() %>%
    left_join(cal, by = "date")
  
  ggplot(daily, aes(date, n_events, colour = User, group = User)) +
    geom_line(linewidth = 1.2, lineend = "round") +
    scale_x_date(
      breaks = week_starts$week_start,
      labels = week_starts$univ_week,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    labs(
      title = title,
      x = "Teaching week",
      y = "Log events per day",
      colour = "User"
    ) +
    theme_sem_pres()
}

plot_sem_weekly <- function(sem_use, title = "Weekly SEM trajectory") {
  ggplot(sem_use, aes(week, sem, colour = User, group = User)) +
    geom_line(linewidth = 1.3, lineend = "round") +
    geom_point(size = 2.2) +
    scale_x_continuous(breaks = sort(unique(sem_use$week))) +
    labs(
      title = title,
      x = "Teaching week",
      y = "SEM (normalised)",
      colour = "User"
    ) +
    theme_sem_pres()
}

plot_sem_components <- function(idf_use, title = "SEM components over time") {
  freq_cols <- names(idf_use)[str_detect(names(idf_use), "^freq_contrib")]
  imm_cols  <- names(idf_use)[str_detect(names(idf_use), "^imm_contrib")]
  div_cols  <- names(idf_use)[str_detect(names(idf_use), "^div_contrib")]
  
  comp_week <- idf_use %>%
    mutate(
      Frequency = rowSums(across(all_of(freq_cols)), na.rm = TRUE),
      Immediacy = rowSums(across(all_of(imm_cols)),  na.rm = TRUE),
      Diversity = rowSums(across(all_of(div_cols)),  na.rm = TRUE)
    ) %>%
    select(User, week, Frequency, Immediacy, Diversity) %>%
    pivot_longer(cols = c(Frequency, Immediacy, Diversity),
                 names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("Frequency","Immediacy","Diversity")))
  
  ggplot(comp_week, aes(week, value, colour = User, group = User)) +
    geom_line(linewidth = 1.1, lineend = "round") +
    geom_point(size = 2) +
    facet_wrap(~ component, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(comp_week$week))) +
    labs(
      title = title,
      x = "Teaching week",
      y = NULL,
      colour = "User"
    ) +
    theme_sem_pres()
}

plot_idf_chapter_trajectories <- function(idf_use, chapters_show = 1:10,
                                          title = "Chapter contributions over time") {
  idf_long <- idf_use %>%
    pivot_longer(
      cols = matches("^(freq_contrib|imm_contrib|div_contrib)"),
      names_to = "var",
      values_to = "value"
    ) %>%
    mutate(chapter = as.integer(str_extract(var, "(?<=\\.\\.c\\.)\\d+"))) %>%
    filter(!is.na(chapter), chapter %in% chapters_show)
  
  chap_week <- idf_long %>%
    group_by(User, week, chapter) %>%
    summarise(idf_kt = sum(value, na.rm = TRUE), .groups = "drop")
  
  ggplot(chap_week, aes(week, idf_kt, colour = User, group = User)) +
    geom_line(linewidth = 1.0, lineend = "round") +
    geom_point(size = 1.6) +
    facet_wrap(~ chapter, ncol = 5, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(chap_week$week))) +
    labs(
      title = title,
      subtitle = "IDF(k,t) = Frequency + Immediacy + Diversity within chapter",
      x = "Teaching week (t)",
      y = "IDF(k,t)",
      colour = "User"
    ) +
    theme_sem_pres(base_size = 14)
}

plot_idf_components_by_chapter <- function(idf_use,
                                           chapters_show = 1:10,
                                           facet_style = c("grid", "wrap"),
                                           title = "Component contributions by chapter over time") {
  facet_style <- match.arg(facet_style)
  
  idf_long <- idf_use %>%
    pivot_longer(
      cols = matches("^(freq_contrib|imm_contrib|div_contrib)"),
      names_to = "var",
      values_to = "value"
    ) %>%
    mutate(
      component = case_when(
        str_detect(var, "^freq_contrib") ~ "Frequency",
        str_detect(var, "^imm_contrib")  ~ "Immediacy",
        str_detect(var, "^div_contrib")  ~ "Diversity",
        TRUE ~ NA_character_
      ),
      chapter = as.integer(str_extract(var, "(?<=\\.\\.c\\.)\\d+"))
    ) %>%
    filter(!is.na(component), !is.na(chapter), chapter %in% chapters_show) %>%
    mutate(
      component = factor(component, levels = c("Frequency", "Immediacy", "Diversity")),
      chapter = factor(chapter, levels = sort(unique(chapter)))
    )
  
  p <- ggplot(idf_long, aes(x = week, y = value, colour = User, group = User)) +
    geom_line(linewidth = 0.95, lineend = "round") +
    geom_point(size = 1.4) +
    scale_x_continuous(breaks = sort(unique(idf_long$week))) +
    labs(
      title = title,
      subtitle = "Raw per-chapter component contributions (from the IDF tables)",
      x = "Teaching week (t)",
      y = "Contribution",
      colour = "User"
    ) +
    theme_sem_pres(base_size = 12)
  
  if (facet_style == "grid") {
    p <- p + facet_grid(component ~ chapter, scales = "free_y")
  } else {
    p <- p + facet_wrap(component ~ chapter, scales = "free_y", ncol = length(chapters_show))
  }
  
  p
}

# One plot per component
plot_idf_one_component_by_chapter <- function(idf_use,
                                              component_name = c("Frequency","Immediacy","Diversity"),
                                              chapters_show = 1:10,
                                              title = NULL) {
  component_name <- match.arg(component_name)
  if (is.null(title)) title <- paste0(component_name, " contributions by chapter over time")
  
  prefix <- switch(component_name,
                   "Frequency" = "^freq_contrib",
                   "Immediacy" = "^imm_contrib",
                   "Diversity" = "^div_contrib")
  
  idf_long <- idf_use %>%
    pivot_longer(
      cols = matches(prefix),
      names_to = "var",
      values_to = "value"
    ) %>%
    mutate(
      chapter = as.integer(str_extract(var, "(?<=\\.\\.c\\.)\\d+"))
    ) %>%
    filter(!is.na(chapter), chapter %in% chapters_show) %>%
    mutate(chapter = factor(chapter, levels = sort(unique(chapter))))
  
  ggplot(idf_long, aes(x = week, y = value, colour = User, group = User)) +
    geom_line(linewidth = 1.0, lineend = "round") +
    geom_point(size = 1.5) +
    facet_wrap(~ chapter, ncol = 5, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(idf_long$week))) +
    labs(
      title = title,
      x = "Teaching week (t)",
      y = "Contribution",
      colour = "User"
    ) +
    theme_sem_pres(base_size = 13)
}

# ---- main wrapper: makes + saves plots ----
save_sem_plots <- function(dat, res, module_year,
                           plot_users = NULL,
                           ay_pick = NULL,
                           chapters_show = 1:10,
                           week_focus = NULL,
                           out_dir = file.path("output", "figures"),
                           width = 12, height = 6.75, dpi = 300) {
  
  if (is.null(plot_users)) {
    plot_users <- dat %>% distinct(User) %>% slice_head(n = 2) %>% pull(User)
  }
  
  filt <- sem_filter_inputs(dat, res, plot_users = plot_users, ay_pick = ay_pick)
  dat_use <- filt$dat
  sem_use <- filt$sem
  idf_use <- filt$idf
  
  if (nrow(sem_use) == 0) stop("No SEM rows after filtering. Check plot_users / ay_pick.")
  if (is.null(week_focus)) week_focus <- max(sem_use$week, na.rm = TRUE)
  
  fig_dir <- file.path(out_dir, module_year)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  
  p1 <- plot_sem_daily_activity(dat_use, plot_users)
  p2 <- plot_sem_weekly(sem_use)
  p3 <- plot_sem_components(idf_use)
  p4 <- plot_idf_chapter_trajectories(idf_use, chapters_show = chapters_show)
  p5 <- plot_idf_components_by_chapter(idf_use, chapters_show = chapters_show)
  

  ggsave(file.path(fig_dir, "01_daily_activity.png"), p1, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(file.path(fig_dir, "02_sem_weekly.png"),     p2, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(file.path(fig_dir, "03_sem_components.png"), p3, width = width, height = 8.5,   dpi = dpi, bg = "white")
  ggsave(file.path(fig_dir, "04_idf_chapter_traj.png"), p4, width = width, height = 7.5, dpi = dpi, bg = "white")
  ggsave(file.path(fig_dir, "05_idf_components_bychapter_grid.png"), p5, width = 16, height = 9, dpi = dpi, bg = "white")

  invisible(list(
    plot_users = plot_users,
    week_focus = week_focus,
    fig_dir = fig_dir,
    plots = list(daily = p1, sem = p2, components = p3, idf_traj = p4, idf_comp_bychapter = p5)
  ))
}