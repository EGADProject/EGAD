## update case study figures to v3 ----

m.qeng %>% 
  filter(academic_year=="2013-2014", attribute %in% c("PA","DE","CO","LL")) %>%
  p.ypc

apsc.100 <- m.qeng %>%
  filter(attribute %in% c("PA","DE","CO","LL"), course_code=="APSC 100 M1", assessment=="MEA2")

apsc.100$p.level %<>% str_wrap(width=5)

indicators <- read_excel(mech.file, 6, col_names = TRUE, skip = 1) %>%
  set_names(tolower(names(.)))

indicators$indicator %<>% str_replace_all("-", ".")

apsc.100$description <- indicators$short[match(apsc.100$indicator,indicators$indicator)]

apsc.100$indicator %<>% str_replace_all("[.]", "-")

apsc.100$academic_year %<>%
  reorder.factor(new.order = c("2012-2013","2013-2014","2013-14"))

apsc.100 %>%  
  filter(academic_year=="2013-2014", !indicator %in% c("APSC-1-DE-2","APSC-1-DE-4","APSC-1-LL-3")) %>% 
  ga.hist