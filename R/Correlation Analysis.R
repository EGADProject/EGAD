#given a melted program frame, dcast into matrix of indicator raw scores with student id and GA as id columns
library(RColorBrewer)
library(corrplot)

# Program attribute correlation tables
# Custom function to get correlations using plyr
p.a.cor <- function (df) {
  t.df <- dcast(df,student_id + attribute ~ indicator, mean, fill = 0)
  t.cor <- cor(t.df[-1:-2])
  return(t.cor)
}

# Call to dlply to take a department frame, split by attribute and get correlations on the respective indicators
temp<-dlply(m.apsc, "attribute", p.a.cor)

# Set the colorscale for the correlation plot
colorscale <- colorRampPalette(brewer.pal(10, "RdBu"))(10)

# Produce the Correlation Plot
corrplot(temp[[8]],method=c("color"),col=colorscale,tl.col = "blue", tl.cex = 0.8, cl.lim = c(0,1))


p.a.efa <- function (df) {
  t.df <- dcast(df,student_id + attribute ~ indicator, mean, fill= 0)
  fa.parallel(t.df)
  
  t.fit <- factanal(df[-1:-2], )
}

fa.parallel(c.temp[7:11])
fit <- factanal(c.temp[3:6], 1, rotation="none")