library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(DT)

players <- readRDS("C:/Users/chery/OneDrive/Desktop/RStudio/FPL Shiny App/data/players.rds")
pos_cols <- c(GKP = "#8A8F98", DEF = "#2F6DB5", MID = "#1F9D78", FWD = "#D9822B")

#container for my pages
ui <- page_navbar(
  id = "nav",
  title = "FPL (Fantasy Premiere League) Insights",
  
  sidebar = sidebar(
    sliderInput("min_minutes", "Minimum minutes played",
                min = 0, max = 900, value = 180, step = 30),
    checkboxGroupInput("pos", "Positions",
                       choices  = c("GKP", "DEF", "MID", "FWD"),
                       selected = c("GKP", "DEF", "MID", "FWD")),
    conditionalPanel(
      "input.nav == 'diff'",
      selectInput("metric", "Measure of output",
                  choices = c("Form" = "form",
                              "Points per game" = "points_per_game",
                              "Total points" = "total_points")),
      sliderInput("own_max", "Owned by at most (%)", min = 1, max = 100, value = 15)
    )
  ),
  nav_panel("Value", value = "value",
            card(plotlyOutput("value_plot")),
            card(card_header("Most points per £m"), DTOutput("value_table"))
  ),
  nav_panel("Over/underperformers", plotlyOutput("perf_plot")),
  nav_panel("Differentials", value = "diff", plotlyOutput("diff_plot"))
)

server <- function(input, output, session) {
  #reactive filter
  filt <- reactive({
    players %>% filter(minutes >= input$min_minutes, position %in% input$pos)
  })
  
  #plot of value per player
  output$value_plot <- renderPlotly({
    req(input$pos)
    d <- filt()
    p <- ggplot(d, aes(price, total_points, colour = position, text = paste0(web_name, " (", team_short, ")", "\n £", price, "m, ", total_points, " pts \n", minutes, " mins"))) +
      geom_point() +
      labs(x = "Price (£m)", y = "Total points") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
    })
  #table of value per player
  output$value_table <- renderDT({
    req(input$pos)
    filt() %>% mutate(pts_per_m = round(total_points/price,2))%>%
      arrange(desc(pts_per_m))%>%
      transmute(Player = web_name, Team = team_short, Pos = position,
                `Price £m` = price, Points = total_points,
                Minutes = minutes, `Pts per £m` = pts_per_m) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })
  
  #plot of performance
  output$perf_plot <- renderPlotly({
    req(input$pos)
    #remove goal keepers
    d <- filt() %>% filter( position != "GKP") %>%
      mutate(ga = goals_scored + assists,
             xgi = expected_goal_involvements, 
             diff = ga - xgi)
    p <- ggplot(d, aes(xgi, ga, colour = diff, text = paste0(web_name, " (", team_short, ")", "\n goals + assists: ", ga, "\n xGI:", round(xgi, 2)))) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point() +
      scale_colour_gradient2(low = "#D9822B", mid = "grey80", high = "#1F9D78", midpoint = 0, name = "G+A minus xGI") +
      labs(x = "Expected Goal Involvements (xGI)", y = "Actual Goals + Assists") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  #plot of differentials
  output$diff_plot <- renderPlotly({
    req(input$pos)
    d <- filt() %>% filter(selected_by_percent > 0, selected_by_percent <= input$own_max) %>%
    mutate(y = .data[[input$metric]])
    p <- ggplot(d, aes(selected_by_percent, y, colour = position, text = paste0(web_name, " (", team_short, ")", "\n selected by: ", selected_by_percent, "% of managers \n", input$metric, ": ", y))) +
      geom_point() +
      scale_colour_manual(values = pos_cols) +
      scale_x_log10()
      labs(x = "Ownership (%, log-adjusted)", y = input$metric) +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)