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

# Build function to read sheets, letting sheet be the variable to get passed via lapply ----
read.files <- . %>%
{
  read_excel(mech.file,.,skip = 1)
}

# Set directories and file paths ----

##Project Dir
R <- "~/ownCloud/Projects/R/Projects/EGAD/R"

## Working Directory
out.dir <- "~/ownCloud/Faculty Outcomes Work/Department Data/MECH/Reports/2014-2015"

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
  plyr::rename(c("term" = "Semester", "prog"= "DEPT" , "year"= "Program_year","course(s)"="course_code")) %>%
  left_join(data.frame(course_code = unlist(mech.courses)),.) %>%
  unique

## Add missing information
mech.courses[mech.courses$course_code == "CIVL 230", c(2:4)] <-
  c("F","CIVL",2)
mech.courses[mech.courses$course_code == "MECH 398", c(2:4)] <-
  c("F","MECH",3)
mech.courses[mech.courses$course_code == "MECH 216", c(2:4)] <-
  c("W","MECH",2)

mech.courses %<>%
  na.omit

# Get sheet names from files and extract course asssessment information ----
info.sheets <- excel_sheets(mech.file)
info.sheets <-
  info.sheets[grep("Table 1", info.sheets, ignore.case = TRUE)]

mech.course.data <- lapply(info.sheets, read.files) %>% 
  set_names(mech.courses %>% 
              filter(DEPT == "MECH") %>% 
              select(course_code) %>%
              unlist)

mech.course.data %<>%
  rbind.fill() %>% 
  select(c(1,2,4:7)) %>% 
  set_names(c("course_code",
              "indicator",
              "assessment",
              "assessor",
              "date_assessed",
              "instructor_comments"))



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
  plyr::rename(c("Student_ID" = "STUDENT_ID")) %>%
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

## String wrap the p.level for nice plotting
m.mech$p.level %<>% str_wrap(width = 5)

## Arrange the melted dataset 
m.mech %<>%
  arrange(academic_year,
          course_code,
          attribute,
          indicator,
          description) 

## Factor the Attribute Column and reorder according to CEAB order
m.mech$attribute %<>%
  factor() %>% 
  gdata::reorder.factor(new.order=c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"))

## Capture the arranged description, factor description (makes it alphabetic), and return it to original order

sorted.description <- m.mech$description

m.mech$description %<>%
  factor() %>% 
  gdata::reorder.factor((new.order=sorted.description))



##  plyr::dlply() call on the melted frame to produce plots for all courses
#p.mech <- dlply(m.mech, .(course_code), function(x) cb.hist %+% x + ggtitle(paste(x$course_code, "Outcomes Report",sep=" ")))