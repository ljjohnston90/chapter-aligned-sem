# R/prep_sem_input.R

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lubridate)
})

prepare_sem_input <- function(dat,
                              start_dates_named,
                              module_year,
                              col_time,
                              col_event_context,
                              col_event_name,
                              col_component,
                              col_user,
                              time_format = "%d/%m/%y, %H:%M:%S",
                              chapter_regex = "Chapter\\s*([0-9]+)",
                              max_chapter = 15,
                              session_gap_mins = 30,
                              course_start_date = NULL,
                              course_end_date   = NULL,
                              override_fun = NULL) {
  
  # standardise column names
  dat <- dat %>%
    rename(
      Time = {{ col_time }},
      Event.context = {{ col_event_context }},
      Event.name = {{ col_event_name }},
      Component = {{ col_component }},
      User = {{ col_user }}
    )
  
  # timestamp
  dat <- dat %>%
    mutate(
      Time = as.POSIXct(Time, format = time_format, tz = "UTC"),
      date = as.Date(Time)
    )
  
  # Optional: restrict logs to a specific course window (recommended)
  if (!is.null(course_start_date)) {
    course_start_date <- as.Date(course_start_date)
    dat <- dat %>% filter(date >= course_start_date)
  }
  if (!is.null(course_end_date)) {
    course_end_date <- as.Date(course_end_date)
    dat <- dat %>% filter(date <= course_end_date)
  }
  
  # academic year from module_year
  year_code <- stringr::str_extract(module_year, "\\d{4}$")
  start_date <- start_dates_named[[year_code]]
  
  if (is.null(start_date)) {
    stop("No start_date provided for year_code = ", year_code)
  }
  
  dat <- dat %>%
    mutate(
      academic_year = year_code,
      univ_week = as.integer(floor(as.numeric(date - start_date) / 7) + 1)
    )
  
  # normalise text fields for pattern matching
  dat <- dat %>%
    mutate(
      Event.context = Event.context %>%
        stringr::str_to_lower() %>%
        stringr::str_squish(),
      Event.name = Event.name %>%
        stringr::str_to_lower() %>%
        stringr::str_squish()
    )
  
  # chapter extraction from event context
  extract_chapter_fallback <- function(x, max_chapter) {
    if (is.na(x) || x == "") return(NA_integer_)
    
    # strip obvious non-chapter numbers (times, years, ids)
    x2 <- x %>%
      stringr::str_replace_all("\\b\\d{1,2}:\\d{2}\\b", " ") %>%       # 14:48
      stringr::str_replace_all("\\b\\d{2}/\\d{2}\\b", " ") %>%         # 22/23
      stringr::str_replace_all("\\bid\\s*'\\d+'\\b", " ") %>%          # id '3738529'
      stringr::str_replace_all("\\b\\d{6,}\\b", " ")                   # very long ids
    
    nums <- suppressWarnings(as.integer(stringr::str_extract_all(x2, "\\b\\d{1,2}\\b")[[1]]))
    nums <- nums[!is.na(nums) & nums >= 1 & nums <= max_chapter]
    if (length(nums) == 0) return(NA_integer_)
    nums[1]
  }
  
  chapter_regex <- "\\bchapter\\s*([0-9]{1,2})\\b"
  week_regex    <- "\\bweek\\s*([0-9]{1,2})\\b"
  
  dat <- dat %>%
    mutate(
      chap_ctx = suppressWarnings(as.integer(stringr::str_match(Event.context, chapter_regex)[, 2])),
      chap_nam = suppressWarnings(as.integer(stringr::str_match(Event.name,    chapter_regex)[, 2])),
      wk_ctx   = suppressWarnings(as.integer(stringr::str_match(Event.context, week_regex)[, 2])),
      wk_nam   = suppressWarnings(as.integer(stringr::str_match(Event.name,    week_regex)[, 2])),
      
      Chapter = dplyr::coalesce(chap_ctx, chap_nam, wk_ctx, wk_nam),
      
      # final fallback: any number 1..max_chapter anywhere in text
      Chapter = dplyr::if_else(
        is.na(Chapter),
        purrr::map_int(paste(Event.context, Event.name, sep = " | "),
                       extract_chapter_fallback,
                       max_chapter = max_chapter),
        Chapter
      ),
      
      Chapter = dplyr::if_else(!is.na(Chapter) & Chapter > max_chapter, NA_integer_, Chapter)
    ) %>%
    select(-chap_ctx, -chap_nam, -wk_ctx, -wk_nam)
  
  # optional course-specific fixes
  if (!is.null(override_fun)) {
    dat <- override_fun(dat, module_year)
  }
  
  # sort
  dat <- dat %>%
    arrange(User, Time)
  
  # sessionisation
  dat <- dat %>%
    group_by(User) %>%
    mutate(
      timediff = as.numeric(difftime(Time, lag(Time), units = "mins")),
      new_session = if_else(is.na(timediff) | timediff > session_gap_mins, 1, 0),
      Session_ID = cumsum(new_session)
    ) %>%
    ungroup()
  
  # session_chap = last known chapter in session
  fill_forward <- function(x) {
    last <- NA
    for (i in seq_along(x)) {
      if (!is.na(x[i])) last <- x[i]
      x[i] <- ifelse(is.na(x[i]), last, x[i])
    }
    if (!all(is.na(x))) {
      first_known <- which(!is.na(x))[1]
      x[1:(first_known - 1)] <- x[first_known]
    }
    x
  }
  
  dat <- dat %>%
    group_by(User, Session_ID) %>%
    mutate(session_chap = fill_forward(Chapter)) %>%
    ungroup()
  
  dat
}