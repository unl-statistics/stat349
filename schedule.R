library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(forcats)
library(ggplot2)
library(readxl)

# Create a calendar for your syllabus 
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

semester_info <- read_xlsx("course-schedule.xlsx", sheet = "SemesterDates")

date_seq <- function(tbl) {
  stopifnot("Start" %in% names(tbl))
  stopifnot("End" %in% names(tbl))
  
  if(is.na(tbl$End)) {
    ymd(tbl$Start)
  } else {
    seq(ymd(tbl$Start), ymd(tbl$End), by = 1)
  }       
}

# Weekday(s) of class
class_wdays <- semester_info |>
  filter(Description == "Class") |>
  select(Days) |>
  mutate(Days = str_split(Days, ",", simplify = F)) |>
  pluck("Days") |>
  unlist() |>
  str_trim()


# What are the full dates of the semester? Monday of week 1 - end of finals
semester_dates <- semester_info |>
  filter(Description == "Semester") |>
  date_seq()

# Dates that are school holidays/no class by university fiat
not_here_dates <- semester_info |>
  filter(Description == "Holiday") |>
  select(Start, End) |>
  mutate(id = 1:n()) |>
  nest(data = -id) |>
  mutate(dates = purrr::map(data, date_seq)) |>
  unnest(dates) |>
  pluck("dates")

project_dates <- read_xlsx("course-schedule.xlsx", "due-dates") |>
  mutate(date=ymd(date))

exam_week <-  semester_info |>
  filter(Description == "Exams") |>
  select(Start, End) |>
  mutate(id = 1:n()) |>
  nest(data = -id) |>
  mutate(dates = purrr::map(data, date_seq)) |>
  unnest(dates) |>
  pluck("dates")

# Custom function for treating the first day of the month as the first week 
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = seq(floor_date(min(semester_dates), "month"), ceiling_date(max(semester_dates), "month") - days(1), by=1))  %>%
  mutate(mon = lubridate::month(date, label=T, abbr=F), # get month label
         wkdy = weekdays(date, abbreviate=T), # get weekday label
         wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu","Fri","Sat"), # make sure Sunday comes first
         semester = date %in% semester_dates, # is date part of the semester?
         due = date %in% project_dates$date, # is it a due date?
         not_here = date %in% not_here_dates, # is it a day off?
         exam_wk = date %in% exam_week,
         day = lubridate::mday(date), # get day of month to add later as a label
         # Below: our custom wom() function
         week = wom(date),
         sem_week = pmin(17, pmax(0, epiweek(date) - 3)) - if_else(date > ymd(20250316), 1, 0))

# Create a category variable, for filling in squares colorwise

Cal <- Cal %>%
  mutate(category = case_when(
    due ~ "Due date",
    not_here ~ "UNL holiday",
    exam_wk ~ "Finals",
    semester & wkdy %in% class_wdays ~ "Class",
    semester ~ "Semester",
    TRUE ~ "NA"
  )) |>
  left_join(project_dates, by = c("date", "category"))

class_cal <- ggplot(Cal, aes(wkdy, week)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(1,0), 
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  # geom_tile and facet_wrap will do all the heavy lifting
  geom_tile(alpha=0.8, aes(fill=category), color="black", linewidth=.45) +
  facet_wrap(~mon, scales = "free", ncol=3) +
  # fill in tiles to make it look more "calendary" (sic)
  geom_text(aes(label=day, color = semester&(!not_here))) +
  # put your y-axis down, flip it, and reverse it
  scale_y_reverse(breaks=NULL) +
  # manually fill scale colors to something you like...
  scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "black"), guide = "none") + 
  scale_fill_manual(values=c("Class"="purple", 
                             "Due date"="orange",
                             "Semester"="white",
                             "Finals" = "grey70",
                             "UNL holiday" = "grey10",
                             "NA" = "white" # I like these whited out...
                             ),
                    #... but also suppress a label for a non-class semester day
                    breaks=c("Semester", "UNL holiday", "Due date", "Class", "Finals"))
# class_cal

topics <- read_excel("course-schedule.xlsx",  sheet = "Week-plan") |>
  rename(sem_week=Week, topic = Title) |>
  select(sem_week, topic)


duedates <- filter(Cal, category == "Due date") |>
  mutate(important = paste(topic, ": ", format.Date(date, "%b %d"), sep = "")) |>
  select(sem_week, important)

schedule <- topics |>
  full_join(duedates) |>
  arrange(sem_week) |>
  rename("Week" = sem_week, "Topic" = topic, "Important Dates" = important)

# schedule
