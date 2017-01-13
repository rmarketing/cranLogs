library(cranlogs)
library(plotly)
library(zoo)

# Get data
df <- cran_downloads(packages = c("RAdwords", "RSmartlyIO", "RCriteo"), 
                     from = "2015-01-01", to = "2017-01-12")

# Convert dates
df$date <- as.Date(df$date)

# Make data frame
df <- data.frame(date = unique(df$date),
                 count.radwords = subset(df, package == "RAdwords")$count,
                 count.rsmartlyio = subset(df, package == "RSmartlyIO")$count,
                 count.rcriteo = subset(df, package == "RCriteo")$count)

# Smooth data 
# 5 day moving averages
nWidth <- 5
df <- zoo(df[,-1], order.by = df[,1])
df.smooth <- rollapply(df, width = nWidth, FUN = mean)

df <- data.frame(date = index(df.smooth),
                 count.radwords = round(df.smooth[,1],0),
                 count.rsmartlyio = round(df.smooth[,2],0),
                 count.rcriteo = round(df.smooth[,3],0))

plot_ly(df, x = ~date) %>% 
  add_lines(y = ~count.radwords, fill = "tozeroy", line = list(shape = "spline"), name = "RAdwords") %>% 
  add_lines(y = ~count.rsmartlyio, fill = "tozeroy", line = list(shape = "spline"), name = "RSmartlyIO", yaxis = "y2", xaxis = "x2") %>%
  add_lines(y = ~count.rcriteo, fill = "tozeroy", line = list(shape = "spline"), name = "RCriteo", yaxis = "y3", xaxis = "x3") %>% 
  
  layout(yaxis = list(domain = c(0.70,1), title = "Count", anchor = "xaxis",
                      tickfont = list(color = "#595959", size = 12)),
         
         yaxis2 = list(domain = c(0.35, 0.65), title = "Count", anchor = "xaxis2",
                       tickfont = list(color = "#595959", size = 12)),
         
         yaxis3 = list(domain = c(0, 0.30), title = "Count", anchor = "xaxis3",
                       tickfont = list(color = "#595959", size = 12)),
         
         xaxis = list(anchor = "free", title = "Date"),
         
         xaxis2 = list(anchor = "free", title = "Date"),
         xaxis3 = list(anchor = "free", title = "Date"),
         
         annotations = list(
           
           list(x = 0.05, y = 1, xref = "paper", yref = "paper",
                showarrow = FALSE,
                text = "<b>RAdwords downloads</b>",
                font = list(size = 17)),
           
           list(x = 0.05, y = 0.65, xref = "paper", yref = "paper",
                showarrow = FALSE,
                text = "<b>RSmartlyIO downloads</b>",
                font = list(size = 17)),
           
           list(x = 0.05, y = 0.30, xref = "paper", yref = "paper",
                showarrow = FALSE,
                text = "<b>RCriteo downloads</b>",
                font = list(size = 17))))