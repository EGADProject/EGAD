library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(stringr)

course_report <- function(course_code, ...) {
# Usage: 
# 
# course_report("CIVL 220") or lapply over an array/list of course names 
# courses <- m.mech %>% filter(academic_year=="2014-2015") %>% 
# select(course_code) %>% unique() %>% unlist
# 
# lapply(courses, course_report)
# 
  # Template is currently hardcoded
   template <-
    "~/ownCloud/Projects/R/Projects/EGAD/R/course_report_template.R"
  
  # Brewed_template to not overwrite original template, a literal programming R
  # script used for knitr::spin
  brewed_template <- paste(course_code,"Report.R")
  
  # Pull course from the parameters, format it to a list for brew
  course <- list(course = course_code)
  
  # Use brew to expand the parameterized template, output the expanded template
  brew::brew(template, brewed_template, envir = list2env(course))
  
  # Use knitr::spin to convert the template to .Rmd 
  knitr::spin(brewed_template, knit = FALSE)
  
  # Change the brewed_template extension to Rmd
  md.report <- gsub("\\.R",".Rmd",brewed_template)
  
  # Use rmarkdown::render to produce a pdf report
  rmarkdown::render(md.report)
  
  # Clean up the unnnecessary files
  file.remove(c(brewed_template, md.report))
}



# Dashboard function ----
p.ypc <- function (df)
{
  ggplot(df, aes(x = attribute, y = p.level)) +
    geom_point(
      aes(
        color = factor(attribute), group = indicator, alpha = 0.5
      ), size = 8, position = "jitter", stat = 'summary', fun.y = mean
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
    theme(text = element_text(size = 18)) +
    scale_colour_tableau()
}

## GA Histogram function----

ga.hist <- function (df)
{
  ggplot(df, aes(x = p.level, fill = academic_year)) +
    geom_bar() +
    theme_bw() +
    xlab("Performance") +
    ylab("Count") +
    facet_grid(attribute + indicator + description ~ academic_year, labeller = label_wrap_gen(width =
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


## Sparktable/sparklines exploration ----
library(sparkTable)

content <- list ()

content [['Distribution']] <- newSparkHist(vMax=c(5),barWidth = 1)
content [[ ' Action ' ]] <- function (x) { psych::skew(x) }
  

varType <- c("value", "value")

m.mech %>%
  filter(course_code == "MECH 460", academic_year == "2014-2015", !(indicator %in% na.m.mech$indicator)) %>%
  select(indicator,value) %>% 
  reshapeExt(., idvar="indicator", varying=list(2)) %>% 
  as.data.frame %>% 
  newSparkTable(.,content, varType) %>% 
  showSparkTable()



