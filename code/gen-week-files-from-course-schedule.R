library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(glue)
library(yaml)
library(lubridate)
plan <- read_yaml("schedule.yaml")

plandf <- plan$schedule |>
  purrr::map_dfr(~ as_tibble(.)) |>
  mutate(start = ymd(start), end = ymd(end))

glue_na <- function(data, var, gluestr, .envir = parent.frame()) {
  var <- enquo(var)
  if_else(
    is.na(unlist(select(data, !!var))),
    "",
    glue_data(data, gluestr, .envir = .envir)
  )
}

library(glue)
glue_df <- tibble(
  week = glue_data(plandf, "# Week {week}: {name}\n\n\n"),
  date = glue_data(plandf, '{format(start, "%B %d")}-{format(end, "%d, %Y")}'),
  reading = glue_na(plandf, reading, "## 📖 Reading\n\n{reading}"),
  prepare = glue_na(plandf, prepare, "## 🥣 Prepare for class\n\n{prepare}"),
  class1 = glue_na(plandf, class1, "## 🌮 Tuesday\n\n{class1}"),
  class2 = glue_na(plandf, class2, "## 🌩️ Thursday 🔨 \n\n{class2}"),
  assignments = glue_na(plandf, assignments, "##  🏋 Practice Your Skills\n\n{assignments}")
)

md <- glue_df |>
  apply(1, as.list) |>
  map_chr(~ paste(., collapse = "\n\n"))

md <- set_names(md, sprintf("weeks/week-%02d.qmd", plandf$week))

walk2(md, names(md), ~ writeLines(.x, con = .y))
