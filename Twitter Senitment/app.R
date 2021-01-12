library(shinydashboard)
library(shiny)
library(dplyr)
library(data.table)
library(highcharter)
library(DT)


ui <- dashboardPage(
  dashboardHeader(title = "Sentiment App",
      dropdownMenu(type= "tasks", badgeStatus = "success", 
                   taskItem(value = 50, color = "yellow", "Documentation"),
                   taskItem(value = 70, color = "yellow", "Algorithm Development"),
                   taskItem(value = 90, color = "green", "App Development")),
      dropdownMenu(type = "messages", badgeStatus = "success",
                   messageItem(
                     from = "Shinyapps.io",
                     icon = icon("cloud"),
                     message = "Last deployed date: 25/09/2017"
                   ),
                   messageItem(
                     from = "Sentiment App Update",
                     icon = icon("cubes"),
                     message = "Included description for graphs"
                   )),
      dropdownMenu(type = "notifications",
                   notificationItem(
                     text = "Include correlations between n-grams and pos-neg words",
                     icon("cube"),
                     status = "info"
                   ),
                   notificationItem(
                     text = "Fix donut chart tooltip",
                     icon("cube"),
                     status = "info"
                   ),
                   notificationItem(
                     text = "Finish the documentation",
                     icon("cube"),
                     status = "info"
                   ),
                   notificationItem(
                     text = "Include descriptive dashboard",
                     icon("cube"),
                     status = "info"
                   ))
      ),
        
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Sentiment Results", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Reactive Input", tabName = "widgets", icon = icon("gears"),
                 sliderInput("slider", "Number of tweets:", 10, 50, 10), 
                 radioButtons("sentiment", "Sentiment", c("Positive","Negative"), "Positive"),
                 selectInput("classification", "Classification", c("Correct Classification","Incorrect Classification"))),
        menuItem("Information", tabName = "Documentation", icon = icon("table"))
      )
    ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("count"),
                valueBoxOutput("users"),
                valueBoxOutput("accuracy")
                
              ),
              fluidRow(
                column(width = 4, align ="center",  highchartOutput("donut", width = "100%")),
                column(width = 8, align ="center", highchartOutput("tweets_score", width = "100%"))
              ),
              fluidRow(
                column(width = 4, background = "light-blue",
                    textOutput("text1")),
                column(width = 2),
                column(width = 4, background = "light-blue",
                    textOutput("text2"))
              
      )),
      
      # Documentation tab content
      tabItem(tabName = "Documentation",
              fluidRow(
                column(width = 4,
                       tags$div(
                         h1("Twitter US Airline",
                            tags$br(),
                            "Sentiment App"))),
                column(width = 5),
                column(width = 3, img(src = "twitterbird_RGB.png",width = "150", height = "100"))
              ),
              fluidRow(
                column(width = 4, tags$div(tags$br(), tags$hr())),
                column(width = 8, tags$div(tags$br(), tags$hr()))
              ),
              fluidRow(
                column(width = 4,
                         tags$div(
                           tags$ol(
                           tags$li(tags$h5("Kaggle hosted a competition with The Main question to analyse how travelers
                                   in February 2015 expressed their feelings in Twitter for US Airlines.")),
                           tags$li(tags$h5("This Application uses only the text to predict the Sentiment for each tweet.")),
                           tags$li(tags$h5("If you want to read more about the competition you can
                                            find more information",tags$a(href = "https://www.kaggle.com/crowdflower/twitter-airline-sentiment", "here"),"at the official Kaggle website.")))
                         )
                       ),
                column(width = 8, 
                       dataTableOutput("table_doc"))
              )
              
      )
    )
  )
)

server <- function(input, output) {
  original_tweets <- read.csv("Tweets.csv")
  original_tweets <- original_tweets[,c("airline_sentiment", "airline", "text")]
  results <- read.csv("twitter_results.csv")
  Sentiments <- c('Positive','Negative')
  
  dataset_class <- reactive({
    switch(input$classification,
           "Correct Classification" = filter(results, actual == predicted),
           "Incorrect Classification" = filter(results, actual != predicted))
  })
  
  dataset_sentiment <- reactive({
    data_cla <- dataset_class()
    switch(input$sentiment,
           "Positive" = filter(data_cla, score_sentiment == "positive"),
           "Negative" = filter(data_cla, score_sentiment == "negative"))
  })

  
  bar_chart_colour <- reactive({
    switch(input$sentiment,
           "Positive" = "#1A5276",
           "Negative" = "#E74C3C")
  })

  dataset_sorting <- reactive({
    data <- dataset_sentiment()
    switch(input$sentiment,
           "Positive" = head(data[order(-data$Score),], input$slider),
           "Negative" = head(data[order(data$Score),], input$slider))
  })
  
  output$table_doc <- renderDataTable(original_tweets,
                                      options = list(lengthMenu = c(5, 10, 15), pageLength = 5))

  output$text1 <- renderText({paste("The donut chart displays all ", input$classification,
                                    "positive and negative tweets. ")})
  
  output$text2 <- renderText({paste("The bar chart displays", input$slider,input$classification, input$sentiment,
                                    "tweets. The tweets are ordered based on a 
                                    sentiment score.")})
  

  output$count <- renderValueBox({
    valueBox(
      value = 15000, 
      subtitle = "Total number of tweets",
      icon = icon("download"),
      color = "light-blue"
    )
  })

  
  output$users <- renderValueBox({
    valueBox(
      value = 2000,
      subtitle = "Total number Users",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$accuracy <- renderValueBox({
    valueBox(
      value = "86%",
      subtitle = "Accuracy",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$donut <- renderHighchart({
    datac <- dataset_class()
    datac_pos <- filter(datac, score_sentiment == "positive")
    datac_neg <- filter(datac, score_sentiment == "negative")
    Values1 <- c(dim(datac_pos)[1], dim(datac_neg)[1])
    Values2 <- c(dim(datac)[1], dim(datac)[1])
    table <- data.frame(Sentiments, Values1)
    highchart() %>%
      hc_chart(backgroundColor = "#ECF0F8") %>%
      hc_title(text = "Succesfull Predictions") %>%
      hc_add_series_labels_values(1, 1, type = "pie", size = '60%',color ="#FDFEFE",
                                  dataLabels = FALSE) %>%
      hc_add_series_labels_values(table$Sentiments, table$Values1, type = "pie",  size = '100%',
                                  innerSize = "60%") %>%
      hc_colors(c("#1A5276", "#E74C3C"))
  })
  
  output$tweets_score <- renderHighchart({
    colour <- bar_chart_colour()
    dataset <- dataset_sorting()
    t <- dataset$text
    t_pos <- dataset$Score
    
    highchart() %>% 
      hc_chart(backgroundColor = "#ECF0F8") %>%
      hc_chart(type = "bar") %>% 
      hc_title(text = "Positive Successful Tweets") %>% 
      hc_plotOptions(column = list(
        dataLabels = list(enabled = TRUE),
        stacking = "column",
        colorByPoint = TRUE,
        enableMouseTracking = FALSE)) %>%
      hc_xAxis(categories = head(t, input$slider)) %>% 
      hc_add_series(data = head(t_pos, input$slider),
                    name = "Sentiment Score", color = colour)
  })
  
}

shinyApp(ui, server)