library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(glue)

plan <- read_xls("course-schedule.xls", "Week-plan")

week_template <- "

# Week {Week}: {Title}

## 📖 Reading

{Reading}

### 🎯 Check your understanding

{Reading_Quiz}

## 🥣 Prepare for class

{Prepare}

## ☕ Monday

{Monday_Class}

## 🐪 Wednesday

{Wednesday_Class}

##  🏋️ Practice your skills

{Assignments}


"

week_template_no_prep <- "

# Week {Week}: {Title}

## 📖 Reading

{Reading}

### 🎯 Check your understanding

{Reading_Quiz}

## ☕ Monday

{Monday_Class}

## 🐪 Wednesday

{Wednesday_Class}

##  🏋️ Practice your skills

{Assignments}


"

md <- if_else(is.na(plan$Prepare), glue_data(plan, week_template_no_prep), glue_data(plan, week_template))

md <- set_names(md, sprintf("weeks/week-%02d.qmd", plan$Week))

walk2(md, names(md), ~writeLines(.x, con = .y))
