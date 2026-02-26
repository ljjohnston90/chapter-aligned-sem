# scripts/02_plot_sem_single_module.R
source("R/sem_plot.R")

save_sem_plots(
  dat = dat,
  res = res,
  module_year = module_year,
  plot_users = NULL,        # defaults to first 2 users
  ay_pick = NULL,
  chapters_show = 1:10,
  week_focus = NULL
)