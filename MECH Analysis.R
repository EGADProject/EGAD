library(tidyr)
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(stringr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(knitr)
library(sparkline)
library(htmlwidgets)

# ########
# TO-DO
#
# 1. Many courses contain duplicate measures.
# 2. For those frames, melt all columns
# 3. rename duplicates (often having a .1 or .2),
# 4. cast back out with mean as an aggregate function.
# 5. Bind all frames together
#
# ########


# Build function to read sheets, letting sheet be the variable to get passed via lapply ----
read.files <- . %>%
{
  read_excel(mech.file,.,skip = 1)
}

# Set working directory and file paths ----
setwd("~/ownCloud/Faculty Outcomes Work/Department Data/MECH")

## Path to recent MECH data file
mech.file <-
  "~/ownCloud/Faculty Outcomes Work/Department Data/MECH/Course Data/2014-2015/2015 MECH Outcomes.xlsx"

# Get sheet names from files and extract outcomes reports ----
mech.sheets <- excel_sheets(mech.file)
mech.sheets <-
  mech.sheets[grep("Outcomes R", mech.sheets, ignore.case = TRUE)]

# Extract courses, indicators and descriptives from MECH sheets ----

## Extract course titles
mech.courses <-
  lapply(mech.sheets, str_extract, pattern = "([A-Z])+ ([0-9])+")

## Extract all indicator and information
mech.indicators <- read_excel(mech.file, 3, col_names = TRUE) %>%
  set_names(tolower(names(.)))

## Create mech course table
mech.courses <- mech.indicators %>%
  select(term, prog,year, `course(s)`) %>%
  rename(
    Semester = term, DEPT = prog , Program_year = year, course_code = `course(s)`
  ) %>%
  left_join(data.frame(course_code = unlist(mech.courses)),.) %>%
  unique

## Add missing information
mech.courses[mech.courses$course_code == "CIVL 230", c(2:4)] <-
  c("F","CIVL",2)
mech.courses[mech.courses$course_code == "MECH 396", c(2:4)] <-
  c("F","MECH",3)
mech.courses[mech.courses$course_code == "MECH 216", c(2:4)] <-
  c("W","MECH",2)

mech.courses %<>%
  na.omit

# Process all MECH course sheets, correcting information where needed ----
##Read and join all outcomes measures from MECH Courses: outputs a list
mech.outcomes.data <- lapply(mech.sheets, read.files) %>%
  set_names(mech.courses$course_code)

## Process each course in the list, bind the columns with descriptive info pulled from the course table
mech.outcomes.data <-
  lapply(seq(length(mech.outcomes.data)), function(i) {
    cbind(
      mech.outcomes.data[[i]],
      COURSE_CODE = mech.courses$course_code[[i]],
      ACADEMIC_YEAR = "2014-2015",
      SEMESTER = mech.courses$Semester[[i]],
      DEPT = mech.courses$DEPT[[i]],
      PROGRAM_YEAR = mech.courses$Program_year[[i]]
    )
  })

## Bind the elements of the list together, re-order the columns, outputs a data frame
mech.outcomes.data %<>%
  rbind.fill() %>%
  rename(STUDENT_ID = Student_ID) %>%
  select(STUDENT_ID, ACADEMIC_YEAR, SEMESTER, COURSE_CODE, DEPT, PROGRAM_YEAR, everything()) %>%
  filter(DEPT == "MECH")

# Read in previous years MECH file ----
prev.mech <-
  "~/ownCloud/Faculty Outcomes Work/Department Data/Faculty-wide/Outcomes Data/Queen's Engineering Master.xlsx" %>%
  read_excel(sheet = "MECH") %>%
  select(-(ASSESSMENT:ASSESSMENT_DATE))


# Rename last years indicators to 2014-2015 designation ----
## Create indicator name lookup-table
lookup <- mech.indicators %>%
  select(
    old.indicator = `2013/14 designator`, indicator = `indicator code`, description = `indicator short description`,  course = `course(s)`
  ) %>%
  filter(course %in% mech.courses$course_code) %>%
  na.omit %>%
  #filter(grepl("MECH",indicator)) %>%
  slice(-9)

## Select just the indicator columns in last years data that are continued in this years
prev.mech.outcomes1 <- prev.mech %>%
  select(.,one_of(intersect(lookup$old.indicator, colnames(.)))) %>%
  set_colnames(.,lookup$indicator[match(colnames(.),lookup$old.indicator)])

prev.mech.outcomes2 <- prev.mech %>%
  select(.,-one_of(intersect(lookup$old.indicator, colnames(.))))

prev.mech <- bind_cols(prev.mech.outcomes2,prev.mech.outcomes1)


# Bind the renamed previous years with this years list----
full.mech.data <- rbind.fill(mech.outcomes.data,prev.mech)

full.mech.data %<>%
  set_names(.,str_trim(names(.)))

m.mech <-
  melt(full.mech.data, c(1:6), c(-1:-6), "indicator",  na.rm = TRUE)

m.mech %<>%
  set_names(.,tolower(names(.))) %>%
  mutate(p.level = cut(
    value, c(0,1,2,3,4,5), labels = c(
      "Not Demonstrated",
      "Marginal",
      "Meets Expectations",
      "High Quality",
      "Mastery"
    ), include.lowest = TRUE
  )) %>%
  mutate(attribute = str_extract(m.mech$indicator, "-[A-Z][A-Z]-") %>%
           str_sub(., 2, 3))
  

ind.lookup <- mech.indicators %>%
  select(indicator = `indicator code`, description = `indicator short description`)

m.mech$description <- ind.lookup$description[match(m.mech$indicator,ind.lookup$indicator)]

na.m.mech <- m.mech %>% 
  filter(is.na(description)>=1) %>% 
  mutate(description = indicator)

m.mech %<>%
  filter(!is.na(description)>=1) %>% 
  bind_rows(.,na.m.mech)

m.mech$p.level %<>% str_wrap(width = 5)


# m.mech %>% 
#   filter(academic_year == "2014-2015", attribute == "KB", program_year == 2) %>%
#   select(value) %>%  
#   sparkline(type = 'bar')



# Dashboard ----
p.ypc <- function (df)
{
  ggplot(df, aes(x = attribute, y = p.level)) +
    geom_point(
      aes(
        color = factor(attribute), group = indicator, size = 8, alpha = 0.5
      ), position = "jitter", stat = 'summary', fun.y = mean
    ) +
    theme_bw() +
    theme(legend.position = "none") +
    xlab("Attribute") +
    ylab("") +
    facet_grid(program_year ~ academic_year) +
    xlim(c(
      "KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"
    )) +
    ylim(str_wrap(
      c(
        "Not Demonstrated", "Marginal", "Meets Expectations", "High Quality", "Mastery"
      ),width = 5
    )) +
    theme(text = element_text(size = 18))
}

m.mech %>%
  p.ypc()

## KB ----

ga.hist <- function (df)
{
  ggplot(df, aes(x = p.level, fill = academic_year)) +
    geom_bar() +
    theme_bw() +
    xlab("Performance") +
    ylab("Count") +
    facet_grid(description ~ academic_year, labeller = label_wrap_gen(width =
                                                                        10)) +
    xlim(str_wrap(
      c(
        "Not Demonstrated", "Marginal", "Meets Expectations", "High Quality", "Mastery"
      ),width = 5
    )) +
    theme(legend.position = "none") +
    theme(strip.text.y = element_text(angle = 0)) +
    scale_fill_brewer(palette = "Set1")
}
