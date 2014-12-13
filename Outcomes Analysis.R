# Setting working directory and establishing data frames ----
setwd("/Users/Jake/ownCloud/Faculty Outcomes Work/Department Data/Faculty-wide/Outcomes Data/")

# Setting output path to direct saving reports to proper areas ----
output.path <- "/Users/Jake/ownCloud/Faculty Outcomes Work/Department Data"

# Adding required libraries ----
library(gdata)
library(ggplot2)
library(plyr)
library(reshape2)
library(data.table)
library(gridExtra)


# Read in departmental data to individual data frames from the master sheet ----
mech <- read.xls("Queen's Engineering Master.xlsx", sheet = "MECH", na.strings = c("", "NA"))
mthe <- read.xls("Queen's Engineering Master.xlsx", sheet = "MTHE", na.strings = c("", "NA"))
eece <- read.xls("Queen's Engineering Master.xlsx", sheet = "EECE", na.strings = c("", "NA"))
chee <- read.xls("Queen's Engineering Master.xlsx", sheet = "CHEE", na.strings = c("", "NA"))
apsc <- read.xls("Queen's Engineering Master.xlsx", sheet = "APSC", na.strings = c("","#N/A", "NA"))

# Create a list of all data frames
d.list <- list(mech,mthe,eece,chee,apsc)

# Place all indicator columns in alphabetical order ----
d.list <- llply(d.list, function (x) x[c(1:8,(order(names(x[c(9:ncol(x))]))+8))])

mech<-mech[c(1:8,(order(names(mech[c(9:ncol(mech))]))+8))]
mthe<-mthe[c(1:8,(order(names(mthe[c(9:ncol(mthe))]))+8))]
eece<-eece[c(1:8,(order(names(eece[c(9:ncol(eece))]))+8))]
chee<-chee[c(1:8,(order(names(chee[c(9:ncol(chee))]))+8))]
apsc<-apsc[c(1:8,(order(names(apsc[c(9:ncol(apsc))]))+8))]


# Melt all the data frames ----
# keeping the all columns that aren't indicators as IDs, then all outcomes columns as variables
# The majority of the data can be plotted from these melted frames
m.mech <- melt(mech, c(1:8), c(-1:-8), "indicator",  na.rm = TRUE)
m.mthe <- melt(mthe, c(1:8), c(-1:-8), "indicator",  na.rm = TRUE)
m.eece <- melt(eece, c(1:8), c(-1:-8), "indicator",  na.rm = TRUE)
m.chee <- melt(chee, c(1:8), c(-1:-8), "indicator",  na.rm = TRUE)
m.apsc <- melt(apsc, c(1:8), c(-1:-8), "indicator",  na.rm = TRUE)

# Convert all header names to lowercase ----
setnames(m.mech,names(m.mech),tolower(names(m.mech)))
setnames(m.mthe,names(m.mthe),tolower(names(m.mthe)))
setnames(m.eece,names(m.eece),tolower(names(m.eece)))
setnames(m.chee,names(m.chee),tolower(names(m.chee)))
setnames(m.apsc,names(m.apsc),tolower(names(m.apsc)))


# Factor the scores for each data frame ----

# APSC Cut is different for 2012-2013 leagcy scores being 4 level, not 5.
m.apsc1<-m.apsc[m.apsc$academic_year=="2012-2013",]
m.apsc2<-m.apsc[m.apsc$academic_year=="2013-2014",]

# Values into Performance Level Scores (Rubric levels)
m.mech$p.level <-cut(m.mech$value,c(0,1,2,3,4,5),labels=c("Not Demonstrated","Marginal","Meets Expectations","High Quality","Mastery"), include.lowest=TRUE)
m.mthe$p.level <-cut(m.mthe$value,c(0,1,2,3,4,5),labels=c("Not Demonstrated","Marginal","Meets Expectations","High Quality","Mastery"), include.lowest=TRUE)
m.eece$p.level <-cut(as.numeric(m.eece$value),c(0,1,2,3,4,5),labels=c("Not Demonstrated","Marginal","Meets Expectations","High Quality","Mastery"), include.lowest=TRUE)
m.chee$p.level <- cut(m.chee$value,c(0,1,2,3,4,5),labels=c("Not Demonstrated","Marginal","Meets Expectations","High Quality","Mastery"), include.lowest=TRUE)
m.apsc1$p.level <- cut(m.apsc1$value,c(0,2,4,6,8),labels=c("Not Demonstrated","Marginal","Meets Expectations","Mastery"), include.lowest=TRUE)
m.apsc2$p.level <- cut(m.apsc2$value,c(0,3,4,5,6,8),labels=c("Not Demonstrated","Marginal","Meets Expectations","High Quality","Mastery"), include.lowest=TRUE)


# Values to target scores
m.mech$t.score <- cut(m.mech$value,c(0,2,3,5),labels=c("Below Target","Target","Above Target"), include.lowest=TRUE)
m.mthe$t.score <- cut(m.mthe$value,c(0,2,3,5),labels=c("Below Target","Target","Above Target"), include.lowest=TRUE)
m.eece$t.score <- cut(as.numeric(m.eece$value),c(0,2,3,5),labels=c("Below Target","Target","Above Target"), include.lowest=TRUE)
m.chee$t.score <- cut(m.chee$value,c(0,2,3,5),labels=c("Below Target","Target","Above Target"), include.lowest=TRUE)
m.apsc1$t.score <- cut(m.apsc1$value,c(0,4,6,8),labels=c("Below Target","Target","Above Target"), include.lowest=TRUE)
m.apsc2$t.score <- cut(m.apsc2$value,c(0,4,5,8),labels=c("Below Target","Target","Above Target"), include.lowest=TRUE)

# Bind APSC years into one frame
m.apsc <- rbindlist(list(m.apsc1,m.apsc2))

# Create data tables of the melted frames ----
m.mech <- data.table(m.mech)
m.mthe <- data.table(m.mthe)
m.eece <- data.table(m.eece)
m.chee <- data.table(m.chee)
m.apsc <- data.table(m.apsc)

# NA.OMIT all tables ----
m.mech$p.level<-na.omit(m.mech$p.level)
m.mthe$p.level<-na.omit(m.mthe$p.level)
m.eece$p.level<-na.omit(m.eece$p.level)
m.chee$p.level<-na.omit(m.chee$p.level)
m.apsc$p.level<-na.omit(m.apsc$p.level)


# Process melted frames to add an attribute column ----
m.mech<-m.mech[, attribute := substr(m.mech$indicator, 6,7)]
m.mthe<-m.mthe[, attribute := substr(m.mthe$indicator, 6,7)]
m.eece<-m.eece[, attribute := substr(m.eece$indicator, 6,7)]
m.chee<-m.chee[, attribute := substr(m.chee$indicator, 6,7)]
m.apsc<-m.apsc[, attribute := substr(m.apsc$indicator, 8,9)]


#Create the Master Queen's Engineering data table ----
m.qeng<-rbindlist(list(m.mech,m.mthe,m.eece,m.chee,m.apsc))

# Process melted frames to add frequency counts for each indicator -----
# Might be able to access this over stat.summary
# m.mech<-m.mech[, indicator.freq := .N, by = list(indicator,value)]
# m.mthe<-m.mthe[, indicator.freq := .N, by = list(indicator,value)]
# m.eece<-m.eece[, indicator.freq := .N, by = list(indicator,value)]
# m.chee<-m.chee[, indicator.freq := .N, by = list(indicator,value)]
# m.apsc<-m.apsc[, indicator.freq := .N, by = list(indicator,value)]
# m.qeng<-m.qeng[, indicator.freq := .N, by = list(indicator,value)]
# 
# # By attribute
# m.apsc<-m.apsc[, ga.freq := .N, by = list(attribute,value)]
# 
# # By assessment
# m.apsc<-m.apsc[, assess.ind.freq := .N, by = list(assessment,indicator,value)]



b.apsc2$attribute<-factor(b.apsc2$attribute,levels = c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"))

# Program Attibute Target Plots Function ----
p.hist.atp <- function (df) {
 
  # Summarize the input data table by the items in the first list, and create prop, a percetnage measure detailed by the second list
  temp.df<-df[, .N, by = list(attribute, indicator, t.score)][, prop := N/sum(N)*100, by=list(attribute,indicator)]
  
  temp.df$attribute<-factor(temp.df$attribute,levels = c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"))
  
  # Produce the plot, a histogram by attribute of program target distributions
  plot<- ggplot(temp.df, aes(x = t.score)) +  
    geom_bar(aes(y = prop, fill=t.score), stat="identity") + 
    xlab("") +
    ylab("Percent of Students") +
    facet_grid(~attribute) +
    ggtitle("Program Attribute Targets") +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))+
    theme(text = element_text(size=20)) 
  
  return(plot)
}
  

# Program Attribute Targets by Indicator Scatter Plot Function----
p.sp.atp_i <- function (df) {
  
  # Summarize the input data table by the items in the first list, and create prop, a percetnage measure detailed by the second list
  temp.df <- df[, .N, by = list(attribute, indicator, t.score)][, prop := N/sum(N)*100, by = list(attribute, indicator)]
  
  temp.df$attribute<-factor(temp.df$attribute,levels = c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL"))
  
  # Produce the plot, a scatter plot of program target distributions, detailed by indicator
  plot <- ggplot(temp.df, aes(x = t.score, y = prop)) +  
    geom_point(aes(group = indicator, shape = indicator, color = indicator), size = 4, alpha = 0.5) + 
    xlab("") +
    ylab("Percent of Students") +
    facet_grid(~attribute) +
    ggtitle("Program Attribute Targets by Indicator") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))+
    theme(text = element_text(size = 20)) +
    scale_shape_manual(values = c(1:25)) 
  
  return(plot)
}
  
 

# Course-based Histogram --------------------------------------------------

cb.hist <- ggplot(m.apsc, aes(x = p.level)) + 
  geom_bar(aes(fill=p.level)) + 
  ylab("Count") + 
  facet_wrap(~indicator, scales = "fixed", ncol=3) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0)) +
  #ggtitle("APSC 100:M1 Course Indicator Report") +
  theme(text = element_text(size=20)) +
  xlab("Performance") + 
  xlim(c("Not Demonstrated","Marginal","Meets Expectations","High Quality", "Mastery")) +
  scale_fill_brewer()  
  

# Course-based Line graph. Week Assessed by Aggregate Mean, by each indicator for each attribute ----
cb.idl <- ggplot(m.apsc, aes(x = assessment_date, y = p.level)) + 
  geom_line(aes(colour = indicator, group = indicator), size = 1, stat = 'summary', fun.y = mean) + 
  geom_point(aes(colour = indicator, shape = indicator, group = indicator), stat = 'summary', fun.y = mean) + 
  scale_shape_manual(values = c(0:25)) + 
  facet_wrap(~attribute, scales="fixed") + 
  xlab("Academic Week") + 
  ylab("") + 
  xlim(1,24) +
  theme(text = element_text(size=20))

# Program Year pannel chart ----
p.ypc <- ggplot(m.qeng, aes(x=attribute, y=p.level)) + 
  geom_point(aes(color = attribute, group = indicator, alpha=0.5), size=6, stat = 'summary', fun.y = mean) + 
  theme(legend.position="none") + 
  xlab("Attribute") + 
  ylab("") + 
  facet_grid(program_year~.) + 
  ggtitle(expression(atop("Queen's Engineering Attribute Performance", atop(italic("Each dot represents mean aggregate student performance on an indicator"), "")))) +
  #ggtitle("Queen's Engineering Attribute Performance") +
  xlim(c("KB","PA","IN","DE","ET","TW","CO","PR","IM","EE","EC","LL")) +
  theme(text = element_text(size=20))

# Course based overlaid histogram ----

cb.ohist<- ggplot(mea.apsc.2013b, aes(x=value, fill=assessment)) +
  geom_bar(alpha = 0.5) +
  xlab("Performance") + 
  ylab("Count") +  
  ggtitle("Indicator Comparison between Assessments") +
  theme(text = element_text(size=20)) +
  facet_grid(indicator~assessment) +
  xlim(c("Not Demonstrated","Marginal","Meets Expectations","High Quality", "Mastery")) +
  theme(legend.position="none")
  
# Historical overliad histogram ----

cb.h_ohist<- ggplot(temp, aes(x=p.level, fill=academic_year)) +
  geom_bar() +
  xlab("Performance") + 
  ylab("Count") +  
  ggtitle("Indicator Comparison to Previous Years") +
  theme(text = element_text(size=10)) +
  facet_grid(indicator~academic_year) +
  xlim(c("Not Demonstrated","Marginal","Meets Expectations","High Quality", "Mastery")) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_fill_brewer(palette="Set1")

# Course-based line plot aggregate indicator performance ----

cb.ln <- ggplot(mea.apsc.2013b, aes(x=assessment, y=raw_score)) +
  geom_line(aes(group=indicator, color=indicator), size=2, alpha=0.5, stat = 'summary', fun.y = mean) +
  geom_point(aes(colour = indicator, group = indicator, shape=indicator), size=5, stat = 'summary', fun.y = mean) +
  xlab("Assessment") + 
  ylab("Performance") +  
  ggtitle("Aggregate Indicator Performance between Assessments") +
  theme(text = element_text(size=18)) +
  coord_cartesian(ylim = c(3, 5)) + 
  scale_y_continuous(breaks=c(3,4,5), labels=c("Meets Expectations", "High Quality", "Mastery"))
 



# Use dlply to plot the course based histogram for each department ----
p.mech <- dlply(m.mech, .(course_code), function(x) cb.hist %+% x + ggtitle(paste(x$course_code, "Outcomes Report",sep=" ")))
p.mthe <- dlply(m.mthe, .(course_code), function(x) cb.hist %+% x + ggtitle(paste(x$course_code, "Outcomes Report",sep=" ")))
p.eece <- dlply(m.eece, .(course_code), function(x) cb.hist %+% x + ggtitle(paste(x$course_code, "Outcomes Report",sep=" ")))
p.chee <- dlply(m.chee, .(course_code), function(x) cb.hist %+% x + ggtitle(paste(x$course_code, "Outcomes Report",sep=" ")))
p.apsc <- dlply(m.apsc, .(course_code, academic_year), function(x) cb.hist %+% x + ggtitle(paste(x$course_code, "Outcomes Report",sep=" ")))
p.apsc <- dlply(temp.apsc2, .(course_code, academic_year), function(x) cb.hist %+% x + ggtitle("EDPS 101 Indicator Profile"))


# Use dlply to plot the Course-based line graph for each department ----
# Have to use an explicit function caller do.call to get the right functionality out of lapply.
plots <- dlply(m.apsc[attribute==c("CO","DE","LL","PA"),], .(academic_year, attribute), function(x) cb.idl %+% x)
do.call(grid.arrange, lapply((plots), ggplotGrob)) 





# Program Health Beta-Chart ----
# Essentially what I would like is to display a programs health on a single grid
# Apparently it's called a waffle chart.  Think of a squared pie-chart.
# I can display it on a 5 color scale, or lump it to be above/below expectations
# Either that or a dot plot
# ph.chart <- ggplot(m.chee,aes(x=value, weight=))

# Program report beta test-ing ----
# program.chee <-dlply(m.chee, .(program_year), function(x) plot %+% x + ggtitle(paste(x$program_year, "Year Program Report",sep=" ")))
# temp<-melt(chee, id=c(1:6), measure.vars=grep("kb", colnames(chee)), "indicator",  na.rm = TRUE)

# Beta-version of the code to produce a comparison of plotting indicator reults over time across a course as a line ----
# p<-ggplot(apsc.test2, aes(x=assessment_date, y=value, group=1, colour=assessment)) + geom_point(stat='summary', fun.y=mean) + stat_summary(fun.y=mean, geom="line")
# p+facet_wrap(academic_year~attribute, scales = "fixed")

#beta plot of repeated histogram measures across a course, grouped by attribute and in a grid of variable(indicator) by assessment
#q<-ggplot(dt,aes(x=value))+geom_bar()+facet_grid(variable~assessment)
#p.apsc<-dlply(dt, .(attribute), function(x) q %+% x)

# Saving plots to pngs ----------------------------------------------------

ggsave(filename = paste(course.code, ".png", sep=""), path=paste(output.path, "/", dept.code, sep = ""), dpi = 600, width = 8.5, height = 11, units = "in")




