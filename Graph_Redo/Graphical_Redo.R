#Load Tidy Verse...Of Course
library(tidyverse)
library(scales)
library(ggplot2)
#Create Data Frame
TimeFrame <- c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017")
Total_Papers <- c(727203,759829,793113,827858,854283, 879859)
Total_Citations <- c(4452645,4882752,5410158,5963893,6453814,6892757)
Average_Papers <- c(181800.75,189957.25,198278.25,206964.5,213570.75,219964.75)
Average_Citations <- c(1113161.25,1220688,1352539.5,1490973.25,1613453.5,1723189.25)
df <- data.frame(TimeFrame, Total_Papers, Total_Citations, Average_Papers, Average_Citations)

p1 <- ggplot(df) +
  geom_col(aes(TimeFrame,Total_Papers,fill ="gray")) +
  geom_col(aes(TimeFrame,Average_Papers, fill ="orange")) +
  labs(title = "Publications and Average Publications\n over Rolling Four-Year Time Periods",
       x ="Rolling Four-Year Time Periods",
       y = "Publications",
       col = "") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("gray", "orange"), labels = c("Total Papers","Average Papers"), drop = FALSE) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
                
p2 <- ggplot(df) +
  geom_col(aes(TimeFrame,Total_Citations,fill ="gray")) +
  geom_col(aes(TimeFrame,Average_Citations,fill ="orange")) +
  labs(title = "Citations and Average Citations\n over Rolling Four-Year Time Periods",
       x ="Rolling Four-Year Time Periods",
       y = "Citations") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("gray", "orange"), labels = c("Total Citations","Average Citations"), drop = FALSE) +
  theme_classic() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2)
