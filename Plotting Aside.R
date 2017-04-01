library(plotly)
library(MASS)

p <- plot_ly(player_stats_season_pg, x = ~mpg_avg, y = ~real_salary, type = 'scatter', mode = 'markers', 
             marker = list(size = ~ppg_avg, opacity = .7), color = ~team, text = ~paste("Player: ", name)) %>%
  layout(
    title = "Player Minutes per Game & Salary",
    xaxis = list(title = "Average Minutes per Game"),
    yaxis = list(title = "Annual Salary [USD]"),
    updatemenus = list(
      list(
      y = 15000000,
      buttons = list(
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "Scatter"),
        
        list(method = "restyle",
             args = list("type", "histogram2d"),
             label = "2D Histogram")))
      )
  )

p

