require(plyr)
require(dplyr)
require(reshape2)
require(magrittr)
require(readr)
require(ggplot2)
require(ggthemes)
require(data.table)

rubric.levels <- . %>% 
{
  rlevels <- magrittr::set_names(.,tolower(names(.))) %>% 
    select(.,`criterion`) %>% 
    unique %>% 
    lapply(as.character) %>% 
    unlist %>% 
    as.vector() %>% 
    c("course code","rubric name","org defined id",.)
  
  df <- dcast(., `course code` + `rubric name` + `org defined id` ~ `criterion`, value.var = "p.score")
  df <- df[,rlevels]
}

civl.dir <- "~/ownCloud/Faculty Outcomes Work/Department Data/CIVL/Course Data/2014-2015/"
civl.file <- "civl_443_d2l_rubric_output.csv"

civl.443 <- paste0(civl.dir,civl.file) %>% 
  read_csv() %>% 
  set_names(.,tolower(names(.)))

# Task 2a CIVL-4-IN-Gx+2 ----
task2a <- civl.443 %>% 
  filter(`rubric name`=="Task 2a Rubric",
         criterion!="Cover letter provided and overall quality of report") %>% 
  data.table %>% 
  setkeyv(c("org defined id","criterion")) %>% 
  unique

task2a$level %<>%
  factor(c("Level 1", "Level 2", "Level 3", "Level 4"))

task2a$p.score <- task2a$level %>% 
  as.numeric()

task2a %<>% 
  rubric.levels

task2a$`CIVL-4-IN-Gx+2`<-rowSums(task2a[,c(4:9)])/ncol(task2a[,c(4:9)])

task2a %<>%
  select(`course code`,`rubric name`,`org defined id`,`CIVL-4-IN-Gx+2`)

# Task 2b CIVL-4-IN-Gx+2----
task2b <- civl.443 %>% 
  filter(`rubric name`=="Task 2b Rubric",
         criterion!="Borehole names are located on cross sections and easy to find" &
           criterion!="All layers are clearly and distinctly identified" &
           criterion!="Covering Memo") %>% 
  data.table %>% 
  setkeyv(c("org defined id","criterion")) %>% 
  unique

task2b$level %<>%
  factor(c("Incomplete", "Level 2", "Level 3", "Level 4", "Level 5"))

task2b$level %<>%
mapvalues(from = c("Incomplete", "Level 2", "Level 3", "Level 4", "Level 5"), to = c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4"))

task2b$p.score <- task2b$level %>% 
  as.numeric() %>% 
  mapvalues(from = c(1,2,3,4,5), to = c(0,1,2,3,4))

task2b %<>% 
  rubric.levels

task2b$`CIVL-4-IN-Gx+2`<-rowSums(task2b[,c(4:ncol(task2b))])/ncol(task2b[,c(4:ncol(task2b))])

task2b %<>%
  select(`course code`,`rubric name`,`org defined id`,`CIVL-4-IN-Gx+2`)

# Task 2c CIVL-4-IN-Gx+2----
task2c <- civl.443 %>% 
  filter(`rubric name`=="Task 2c Rubric",
         criterion!="Quality of plans and clarity with which borehole names and locations are shown" &
           criterion!="Covering memo and comments on how the additional information altered their geological model") %>% 
  data.table %>% 
  setkeyv(c("org defined id","criterion")) %>% 
  unique

task2c$level %<>%
  factor(c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4"))

task2c$p.score <- task2c$level %>% 
  as.numeric() %>% 
  mapvalues(from = c(1,2,3,4,5), to = c(0,1,2,3,4))

task2c %<>% 
  rubric.levels

task2c$`CIVL-4-IN-Gx+2`<-rowSums(task2c[,c(4:ncol(task2c))])/ncol(task2c[,c(4:ncol(task2c))])

task2c %<>%
  select(`course code`,`rubric name`,`org defined id`,`CIVL-4-IN-Gx+2`)

task2c<-task2c[c(1:48),]

# Join all CIVL-4-IN-Gx+2 ----

in.gx2 <- bind_cols(task2a,task2b,task2c) 

in.gx2 <- in.gx2[,c(1:4,8,12)]

in.gx2 %<>%
  melt(c(1:3))

in.gx2$variable <- "CIVL-4-IN-Gx2"

in.gx2$value %<>% as.numeric

civl.4.in.gx2 <- dcast(in.gx2,`org defined id` ~ variable, fun.aggregate = mean)

civl.4.in.gx2$`CIVL-4-IN-Gx2` %<>%
  trunc %>% 
  factor(c(0:5),c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))
  

# Data for Gx+2 time plot
time.gax2<-bind_rows(task2a,task2b,task2c) 
time.gax2 %<>%
  set_names(c("course code","rubric","student.id","indicator"))

time.gax2$indicator %<>%
  trunc %>% 
  factor(c(0:5),c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))



# Task 4.1 CIVL-4-PA-Gx ----
task4.1 <- civl.443 %>% 
  filter(`rubric name`=="Task 4 Rubric",
         criterion=="Case 1: Va calculation") %>% 
  data.table %>% 
  setkeyv(c("org defined id","criterion")) %>% 
  unique

task4.1$level %<>%
  factor(c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))

task4.1$p.score <- task4.1$level %>% 
  as.numeric() %>% 
  mapvalues(from = c(1,2,3,4,5,6), to = c(0,1,2,3,4,5))

task4.1 %<>% 
  rubric.levels

task4.1 %<>%
  plyr::rename( c("Case 1: Va calculation" = "CIVL-4-PA-Gx"))

civl.4.pa.gx <- task4.1[,c(3:4)]

civl.4.pa.gx$`CIVL-4-PA-Gx` %<>% 
  factor(c(0:5),c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))


# Task 4.2 CIVL-4-ET-Gx+1 ----
task4.2 <- civl.443 %>% 
  filter(`rubric name`=="Task 4 Rubric",
         criterion=="Case 1: Plot of Cl concentration in aquifer" |
           criterion=="Case 2: Plot of Cl concentration in aquifer")%>% 
  data.table %>% 
  setkeyv(c("org defined id","criterion")) %>% 
  unique

task4.2$level %<>%
  factor(c("Incomplete", "Level 2", "Level 4", "Level 6", "Level 8", "Level 10"))

task4.2$p.score <- task4.2$level %>% 
  as.numeric() %>% 
  mapvalues(from = c(1,2,3,4,5,6), to = c(0,1,2,3,4,5))

task4.2 %<>% 
  rubric.levels

task4.2$`CIVL-4-ET-Gx+1`<-rowSums(task4.2[,c(4:ncol(task4.2))])/ncol(task4.2[,c(4:ncol(task4.2))])

task4.2 %<>%
  select(`course code`,`rubric name`,`org defined id`,`CIVL-4-ET-Gx+1`)

civl.4.et.gx1 <- task4.2[,c(3:4)] 

civl.4.et.gx1$`CIVL-4-ET-Gx+1` %<>%
  trunc %>% 
  factor(c(0:5),c("Incomplete", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))


civl.levels <- civl.4.pa.gx$`CIVL-4-PA-Gx` %>% levels

