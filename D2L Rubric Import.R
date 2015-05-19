require(plyr)
require(dplyr)
require(reshape2)
require(magrittr)
require(readr)

rubric.levels <- . %>% 
{
  rlevels <- set_names(.,tolower(names(.))) %>% 
    select(.,`criterion`) %>% 
    unique %>% 
    lapply(as.character) %>% 
    unlist %>% 
    as.vector() %>% 
    c("course code","rubric name","org defined id",.)
  
  df <- dcast(., `course code` + `rubric name` + `org defined id` ~ `criterion`, value.var = `level`,)
  df <- df[,rlevels]
}

bsImport <- . %>% 
{
#   Description:
#   This function takes an input data frame, in D2L export format and exports it
#   into a tidy data frame in the queen's outcomes format 
  
  # transform column names to lowercase
  . %<>%
    set_names(.,tolower(names(.)))
  
  # call 
  . %>% 
    select(`course code`,org.defined.id,rubric.name,criterion,level) %>% 
    dlply(.(rubric.name),rubric.levels) %>% 
    l_ply(write.xlsx,"MECH_216_Rubric_level.xlsx", sheetName=.(rubric.name), append=TRUE)
}


  
  
civl.dir <- "~/ownCloud/Faculty Outcomes Work/Department Data/CIVL/Course Data/2014-2015/"
civl.file <- "civl_443_d2l_rubric_output.csv"

civl.443 <- paste0(civl.dir,civl.file) %>% 
  read_csv() 

rubric.order <- civl.443 %>% 
  dlply(.(`rubric name`),rubric.levels)

mech.216 %>% 

mech.216 %>% 
  select(course.code,org.defined.id,rubric.name,criterion,points) %>% 
  dlply(.(rubric.name),rubric.levels) %>% 
  l_ply(write.xlsx,"MECH_216_Rubric_points.xlsx", sheetName=.(rubric.name), append=TRUE)



