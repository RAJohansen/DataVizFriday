#### PLOTLY TO MAKE INTERACTIVE VIDEO
library(plotly)
library(tidyverse)
library(lubridate)
library(zoo)

#Import Data
df <- read.csv("C:/R_Packages/DataVizFriday/FantasyFootball2019/RegularSeason2019.csv")

#test static plot
df_cumsum <- df %>% group_by(Player) %>% mutate(cumsum = cumsum(Wins))
ggplot(df_cumsum, aes(Week,cumsum, color = Player)) + geom_line(aes(size = Points)) + theme_classic()

df_points <- df %>% group_by(Player) %>% mutate(cumsum = cumsum(Points))
ggplot(df_points, aes(Week,cumsum, color = Player)) + geom_line() + theme_classic()


#Acculumate by Function
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

#Remove excess attributes from data processing
df_cumsum<- as.data.frame(df_cumsum)
df_points<- as.data.frame(df_points)

#Create new data set with accumulation variable (used for frames in plotly)
df_plotly <- df_cumsum %>%
  accumulate_by(~Week)

df_plotly2 <- df_points %>%
  accumulate_by(~Week)

#Create Interactive Plot
p <- plot_ly(data = df_plotly,
             x = ~Week,
             y = ~cumsum) %>%
  add_trace(x = ~Week, y = ~ cumsum, color=~Player, frame = ~frame, type = 'scatter', mode = 'lines+markers', line = list(shape = "spline")) %>%
  layout(xaxis = list(title = "Week",
                      range = c(min(df_plotly$Week),max(df_plotly$Week)),
                      zeroline = F),
         yaxis = list(title = "Wins",
                      range = c(0,10),
                      zeroline = F)) %>%
  animation_opts(frame = 250,
                 transition = 0,
                 redraw = FALSE) %>%
  animation_slider(hide = F) %>%
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")

#Create Interactive Plot
p1 <- plot_ly(data = df_plotly2,
             x = ~Week,
             y = ~cumsum) %>%
  add_trace(x = ~Week, y = ~ cumsum, color=~Player, frame = ~frame, type = 'scatter', mode = 'lines+markers', line = list(shape = "spline")) %>%
  layout(xaxis = list(title = "Week",
                      range = c(min(df_plotly$Week),max(df_plotly$Week)),
                      zeroline = F),
         yaxis = list(title = "Points",
                      range = c(0,2000),
                      zeroline = F)) %>%
  animation_opts(frame = 250,
                 transition = 0,
                 redraw = FALSE) %>%
  animation_slider(hide = F) %>%
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")


#View before sending to plotly!
p 
p1
#Set plotly API information
Sys.setenv("plotly_username"="RAJohansen")
Sys.setenv("plotly_api_key"="OkJPXLleytoFqrshkmrB")

#Send plot to https://plot.ly/~RAJohansen/
api_create(p1)
