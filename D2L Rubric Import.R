library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(readr)

civl.dir <- "~/ownCloud/Faculty Outcomes Work/Department Data/CIVL/Course Data/2014-2015/"
civl.file <- "civl_443_d2l_rubric_output.csv"

civl.443 <- paste0(civl.dir,civl.file) %>% 
  read_csv() %>% 
  set_names(.,tolower(names(.)))


rubric.levels <- function(df){
  
  rlevels <- select(df,criterion) %>% 
    unique %>% 
    lapply(as.character) %>% 
    unlist %>% 
    as.vector() %>% 
    c("course.code","rubric.name","org.defined.id",.) 
  
  df <- dcast(df,course.code + rubric.name + org.defined.id ~ criterion)
  
  df <- df[,rlevels]
  
  return(df)
}

rubric.order <- civl.443 %>% 
  dlply(.(`rubric name`),rubric.levels)

mech.216 %>% 
  select(course.code,org.defined.id,rubric.name,criterion,level) %>% 
  dlply(.(rubric.name),rubric.levels) %>% 
  l_ply(write.xlsx,"MECH_216_Rubric_level.xlsx", sheetName=.(rubric.name), append=TRUE)

mech.216 %>% 
  select(course.code,org.defined.id,rubric.name,criterion,points) %>% 
  dlply(.(rubric.name),rubric.levels) %>% 
  l_ply(write.xlsx,"MECH_216_Rubric_points.xlsx", sheetName=.(rubric.name), append=TRUE)



