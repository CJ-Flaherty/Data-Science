library(tidyverse)
library(shiny)
library(alphavantager)
library(shinycssloaders)
library(data.table)
articles1 <- fread("articles1.csv")
articles2 <- fread("articles2.csv")
articles3 <- fread("articles3.csv")

articles <- rbind(articles1, articles2, articles3)

av_api_key("92RPFSEMT0EQHQIK")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Markets vs. Headlines"),
      mainPanel(
        h4("By: C. Flaherty, J. Kagan, P. Mackenzie, & P.
           Schmidt"),
        h4("Line graph key:"),
        h5("red = daily open"),
        h5("purple = daily close"),
        h5("yellow = daily low "),
        h5("black = article release date")),
      selectizeInput(inputId = "selection",
                     label ="Select a New York Times headline:",
                     choices = NULL),
      selectInput(inputId = "Time",
                  label = "Select a time period:",
                  choices = c("Week", "Month","Year")),
      actionButton(inputId = "button1",
                   label = "View stock data!"),
      checkboxGroupInput(inputId = "Measure",
                         label = "What would you like to view?",
                         choices = c("Daily Open", "Daily Close", "Daily Low"))
      
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Information", htmlOutput("backinfo")),
        tabPanel("S&P 500", withSpinner(plotOutput(outputId = "spplot"))),
        tabPanel("DOW Jones", withSpinner(plotOutput(outputId = "dowplot"))),
        tabPanel("CBOE VIX", withSpinner(plotOutput(outputId = "vixplot")))
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$backinfo <- renderUI({
    HTML(paste("This app is designed to aid in the exploration of the relationship is between major events (represented by New York Times headlines) and the state of the market (represented by the S&P 500, the Dow, and the CBOE Volatility Index).",
               "",
               "The S&P 500 is an index that is based on the market performance of 500 large companies that are listed on the NYSE or the NASDAQ. The S&P 500 can be used to analyze general stock market performance because the companies factored into the index take up a significant portion of total value in the market, making their performance a proxy for overall market performance. The companies with the highest weights in the S&P 500 are Apple, Alphabet, Microsoft, Amazon, and Facebook.",
               "",
               "The Dow Jones Industrial Average is a price-weighted average of 30 significant stocks that are traded on the NYSE or the NASDAQ. It is an index used to gauge the general state of the market on a given day. Some of the 30 companies in the Dow include: ExxonMobil, General Electric, American Express, and Goldman Sachs.",
               "",
               "The CBOE Volatility Index represents the market's expectation for 30-day volatility and is often used as a measure of market risk using different index options from the S&P 500. Values greater than 30 usually mean a great deal of volatility, whereas values under 20 mean smoother times in the market.",
               sep="<br/>"))
  })
  
  #computation for selectize input  
  updateSelectizeInput(session, 'selection', choices = articles$title, server = TRUE) 
  
  observeEvent( input$button1,  {  
    
    sandp <- av_get(symbol = "^GSPC", av_fun = "TIME_SERIES_DAILY", outputsize = "full")
    
    dow <- av_get(symbol = "DJI", av_fun = "TIME_SERIES_DAILY", outputsize= "full")
    
    vix <- av_get(symbol = "VIX", av_fun = "TIME_SERIES_DAILY", outputsize = "full")
    
    output$spplot <-  renderPlot({
      
      date1 <- as.Date(articles %>%
                         filter(title == input$selection) %>%
                         .$date)
      
      
      
      #get data from alphavantage
      # sandp <- av_get(symbol = "^GSPC", av_fun = "TIME_SERIES_DAILY", outputsize = "full")
      
      
      
      #crop data
      if (input$Time == "Week") {
        sandpcrop <- sandp %>%
          filter(timestamp > date1 -4 & timestamp < date1 +4)
      }
      if (input$Time == "Month") {
        sandpcrop <- sandp %>%
          filter(timestamp > date1 -15 & timestamp < date1 +15)
      }
      if (input$Time == "Year") {
        sandpcrop <- sandp %>%
          filter(timestamp > date1 -182 & timestamp < date1 +182)
      }
      
      
      
      
      #plot
      
      p <- sandpcrop %>%
        ggplot() +
        geom_vline(xintercept = date1, col = "black") +
        labs(title = "S&P 500", x = "Date", y = "Points", caption = paste("article released on:" ,date1, sep = " "))
      
      if ("Daily Open" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = open), col = "red")
      }
      
      if("Daily Close" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = close), col = "purple")
      }
      
      if("Daily Low" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = low), col = "green") + scale_color_manual(values = "green")
      }
      
      
      p
      
      
    })
    
    output$dowplot <-  renderPlot({
      
      date1 <- as.Date(articles %>%
                         filter(title == input$selection) %>%
                         .$date)
      
      
      
      dow <- av_get(symbol = "DJI", av_fun = "TIME_SERIES_DAILY", outputsize= "full")
      
      
      
      if (input$Time == "Week") {
        dowcrop <- dow %>%
          filter(timestamp > date1 -4 & timestamp < date1 +4)
      }
      if (input$Time == "Month") {
        dowcrop <- dow %>%
          filter(timestamp > date1 -15 & timestamp < date1 +15)
      }
      if (input$Time == "Year") {
        dowcrop <- dow %>%
          filter(timestamp > date1 -182 & timestamp < date1 +182)
      }
      
      p <- dowcrop %>%
        ggplot() +
        geom_vline(xintercept = date1, color = "black") +
        labs(title = "Dow Jones Industrial Average", x = "Date", y = "Points", caption = paste("article released on:" ,date1, sep = " "))
      
      if ("Daily Open" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = open), col = "red")
      }
      
      if("Daily Close" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = close), col = "purple")
      }
      
      if("Daily Low" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = low), col = "green")
      }
      
      p      
    })
    
    
    output$vixplot <-  renderPlot({
      
      date1 <- as.Date(articles %>%
                         filter(title == input$selection) %>%
                         .$date)
      
      
      # vix <- av_get(symbol = "VIX", av_fun = "TIME_SERIES_DAILY", outputsize = "full")
      
      
      
      if (input$Time == "Week") {
        vix <- vix %>%
          filter(timestamp > date1 -4 & timestamp < date1 +4)
      }
      if (input$Time == "Month") {
        vix <- vix %>%
          filter(timestamp > date1 -15 & timestamp < date1 +15)
      }
      if (input$Time == "Year") {
        vix <- vix %>%
          filter(timestamp > date1 -182 & timestamp < date1 +182)
      }
      
      p <- vix %>%
        ggplot() +
        geom_vline(xintercept = date1, col = "black") +
        labs(title = "Volatility Index (CBOE VIX)", x = "Date", y = "Points", caption = paste("article released on:" ,date1, sep = " "))
      
      if ("Daily Open" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = open), col = "red")
      }
      
      if("Daily Close" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = close), col = "purple")
      }
      
      if("Daily Low" %in% input$Measure) {
        p <- p + geom_line(aes(x = timestamp, y = low), col = "green")
      }
      
      p      
    })
    
  }
  )}


shinyApp(ui = ui, server = server)

