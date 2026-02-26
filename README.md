# Chapter-aligned SEM from VLE logs (with plots)

This repository computes a chapter-aligned Student Engagement Metric (SEM) from Moodle/VLE log exports, using only data observed up to each teaching week.

It produces:

- Weekly SEM per student (normalised to [0, 1])
- Chapter-level contribution tables (raw and normalised)
- Diagnostics showing which chapters are “available” each week
- Publication-ready plots (daily activity, weekly SEM, component trajectories, chapter breakdowns)

The workflow is:

1. Prepare raw logs  
2. Compute SEM week by week  
3. Generate plots for selected users  

---

## Repository structure

R/  
- prep_sem_input.R → prepares raw logs into SEM-ready format  
- sem_compute.R → core SEM computation  
- sem_plot.R → plotting functions and save_sem_plots()

scripts/  
- 01_run_sem_single_module.R → end-to-end SEM run for one module  
- 02_plot_sem_single_module.R → generates plots from computed SEM  

data/ → place your CSV export here (do not commit)  
output/ → created automatically (do not commit)

---

## Input data requirements

Your CSV must include:

- Time (timestamp)  
- User (student identifier)  
- Event context  
- Event name  
- Component  

prepare_sem_input() converts these into:

- Time, date  
- User  
- Event.context, Event.name, Component  
- academic_year  
- univ_week  
- Session_ID  
- session_chap  

---

## Quick start

### 1) Compute SEM

Open:

scripts/01_run_sem_single_module.R

Edit:

- module_year  
- path_to_csv  
- start_dates  
- optional: course_start_date / course_end_date  
- optional: min_events  
- weeks_to_include  

Then run in R:

source("scripts/01_run_sem_single_module.R")

Outputs are saved to:

output/sem_outputs/

Files created:

- *_sem_norm01_by_week.rds  
- *_idf_minmax_by_week.rds  
- *_idf_norm01_by_week.rds  

A sanity check confirms:

rowSums(IDF_norm01) == SEM_norm01

---

### 2) Generate plots

After computing SEM, run:

source("scripts/02_plot_sem_single_module.R")

This saves figures to:

output/figures/<module_year>/

By default it plots the first two users.  
You can specify users manually:

save_sem_plots(
  dat = dat,
  res = res,
  module_year = module_year,
  plot_users = c("user_12", "user_47"),
  chapters_show = 1:10
)

---

## Plots produced

1. Daily VLE activity (events per day aligned to teaching weeks)  
2. Weekly SEM trajectory  
3. SEM component trajectories (Frequency, Immediacy, Diversity)  
4. Chapter contribution trajectories  
   IDF(k,t) = F + I + D within each chapter  
5. Component-by-chapter breakdown (raw F/I/D contributions)

All plots are saved as PNG.

---

## Method summary

For each week t:

1. Use only logs with univ_week ≤ t  
2. Keep chapters that:  
   - are accessed by a minimum proportion of students  
   - are within the look-ahead window  
3. Compute per-chapter indicators:  
   - Frequency = distinct sessions  
   - Immediacy = time from cohort first access (scaled and reversed)  
   - Diversity = distinct resource interactions  
4. Min–max scale within (academic year, chapter)  
5. Combine:

IDF_{k,t}^{(i)} = w_F F + w_I I + w_D D

6. Sum across available chapters and normalise:

SEM_{t}^{(i)} = sum_k IDF_{k,t}^{(i)} / ((w_F + w_I + w_D) * K_t)

where K_t = number of available chapters at week t.

The normalised chapter table is scaled so that:

rowSums(IDF_norm01) == SEM_norm01

---

## Dependencies

Required R packages:

- dplyr  
- tidyr  
- stringr  
- lubridate  
- purrr  
- ggplot2  
- readr  

Install with:

install.packages(c("dplyr","tidyr","stringr","lubridate","purrr","ggplot2","readr"))

---

## Notes

- Do not commit raw student data  
- data/ and output/ should be in .gitignore  
- Scripts are written for single-module runs but can be looped  

---

## Author

Laura Johnston  
PhD Researcher in Learning Analytics and Educational Data Mining  
University College London