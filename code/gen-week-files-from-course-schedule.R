library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(glue)

plan <- read_xls("course-schedule.xlsx", "Week-plan")


plan_bits <- c(
  Week = "# Week {Week}:",
  Date_First_Class = "",
  Title = " {Title}
  
", 
  Reading = "## 📖 Reading

  {Reading}

", 
  Reading_Quiz = "### 🎯 Check your understanding

{Reading_Quiz}

", 
  Prepare = "## 🥣 Prepare for class

{Prepare}

",
  Monday_Class = "## ☕ Monday

{Monday_Class}
  
",
  Wednesday_Class = "## 🐪 Wednesday

{Wednesday_Class}

",
  Exam = "## 🧪 Exam
  
  {Exam}
  
",
  Assignments = "##  🏋️ Practice your skills

{Assignments}

"
)

templates <- purrr::map(split(plan, 1:nrow(plan)), ~paste(plan_bits[names(.)[!is.na(.)]], collapse = "") )


md <- map2_chr(split(plan, 1:nrow(plan)), templates, glue_data)

md <- set_names(md, sprintf("weeks/week-%02d.qmd", plan$Week))

walk2(md, names(md), ~writeLines(.x, con = .y))


# 
# week_template <- "
# 
# # Week {Week}: {Title}
# 
# ## 📖 Reading
# 
# {Reading}
# 
# ### 🎯 Check your understanding
# 
# {Reading_Quiz}
# 
# ## 🥣 Prepare for class
# 
# {Prepare}
# 
# ## ☕ Monday
# 
# {Monday_Class}
# 
# ## 🐪 Wednesday
# 
# {Wednesday_Class}
# 
# ##  🏋️ Practice your skills
# 
# {Assignments}
# 
# 
# "
# 
# week_template_no_prep <- "
# 
# # Week {Week}: {Title}
# 
# ## 📖 Reading
# 
# {Reading}
# 
# ### 🎯 Check your understanding
# 
# {Reading_Quiz}
# 
# ## ☕ Monday
# 
# {Monday_Class}
# 
# ## 🐪 Wednesday
# 
# {Wednesday_Class}
# 
# ##  🏋️ Practice your skills
# 
# {Assignments}
# 
# 
# "

