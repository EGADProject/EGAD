#'---
#' title: "Graduate Attribute Course Report: <%= course %>"
#' author: "Jake Kaupp"
#' date: "May 22nd, 2015"
#' output: pdf_document
#'---

#+ library, include = F
library(knitr)
library(dplyr)

#' ## Course Mapping Tables 
#+ course mapping1, echo = FALSE, warning = FALSE, message = FALSE, results='asis' 
n_assessed <- m.mech %>%
  filter(course_code == "<%= course %>", academic_year == "2014-2015") %>%
  group_by(indicator) %>%
  select(description) %>% 
  mutate('Number of Students Assessed' = n()) %>% 
  unique
#'\footnotesize
#+ course mapping2, echo = FALSE, warning = FALSE, message = FALSE,results='asis' 
panderOptions('table.split.table', Inf)
mech.course.data %>%
  filter(course_code == "<%= course %>", !(indicator %in% na.m.mech$indicator)) %>%
  left_join(n_assessed, by = "indicator") %>%
  select(course_code, indicator, description, assessment, assessor, date_assessed, instructor_comments, `Number of Students Assessed`) %>%
  set_names(
    c(
      "Course",
      "Indicator",
      "Short Description",
      "Assessment",
      "Assessor",
      "Date Assessed",
      "Instructor Comments",
      "Number of Students Assessed"
    )
  ) %>% 
  pander

#' ****
#' ## Performance Histograms
#+ histograms, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 12, fig.height = 15,
m.mech %>% 
  filter(course_code=="<%= course %>", !(indicator %in% na.m.mech$indicator)) %>% 
  ga.hist
 

#' ****
#' ## Instructor Feedback/Interpretation
#+ feedback, echo = FALSE, results='asis'
#' Part of CEAB's requirements is to demonstrate the use of outcomes assessment data to make improvements to the course, program and process.  To help prepare future accreditation reports, please write any interpretations about the data or factors affecting student performance, comments and feedback you have the report, and any possible recommendations you have about the course drawn from the data.  If you don't feel comfortable writing the comments down on this sheet, you can also email them to Jake Kaupp (jake.kaupp@queensu.ca) 
