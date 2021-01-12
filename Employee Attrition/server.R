library(shiny)
library(highcharter)
library(shinythemes)
print("server is working")



shinyServer(function(input, output) {
  
  output$table <- renderHighchart({
    
    if (input$input_label == "Left the Company"){
      hr_data <- hr_data[hr_data$left == 1]}
    
    else if (input$input_label == "Still in the company"){
      hr_data <- hr_data[hr_data$left == 0]}

    highchart(height = 150, width = 150) %>%
      hc_xAxis(title = list(text = input$show_vars,
                            opposite = TRUE)) %>%
      hc_yAxis(title = list(text = "Density",
                            opposite = TRUE)) %>%
    
    hc_add_series(name = "high", density(salary_function("high", hr_data[sample(nrow(hr_data),input$sample_size,replace = TRUE),])[,input$show_vars]), type = "area") %>%
    hc_add_series(name = "medium", density(salary_function("medium",hr_data[sample(nrow(hr_data),input$sample_size,replace = TRUE),])[,input$show_vars]), type = "area") %>%
    hc_add_series(name = "low", density(salary_function("low",hr_data[sample(nrow(hr_data),input$sample_size,replace = TRUE),])[,input$show_vars]), type = "area")
  
  })
  
  output$graph <- renderHighchart({
    
    highchart(height = 250, width = 300) %>% 
      hc_chart(polar = TRUE, type = "line") %>% 
      hc_credits(align = "left") %>%
      hc_xAxis(categories = as.vector(hr_data_avg$sales),
               tickmarkPlacement = "on",
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = "polygon",
               lineWidth = 0,
               min = 0) %>% 
      hc_series(
        list(
          name = "Satisfaction",
          data = as.vector(hr_data_avg$satisfaction_average),
          pointPlacement = "on"
        ),
        list(
          name = "Frequency",
          data = as.vector(hr_data_avg$Frequency),
          pointPlacement = "on"
        ),
        list(
          name = "Number of Projects",
          data = as.vector(hr_data_avg$average_monthly_hours),
          pointPlacement = "on"
        ),
        list(
          name = "Monthly Hours",
          data = as.vector(hr_data_avg$average_monthly_hours),
          pointPlacement = "on"
        ),
        list(
          name = "Time Spend in Company",
          data = as.vector(hr_data_avg$time_spend_company_average),
          pointPlacement = "on"
        ),
        list(
          name = "Left",
          data = as.vector(hr_data_avg$left_average),
          pointPlacement = "on"
        )
      )
  })
  
  output$renderTable <- renderDataTable({
    hr_data_intro[sample(nrow(hr_data_intro),30,replace = TRUE)]
  })
  
  output$cluster_chart <- renderHighchart({

        if (input$number_clusters == "2"){
      ff <- head(clustering2, input$slider_clustering)
    }
    else if (input$number_clusters == "3"){
      ff <- head(clustering3, input$slider_clustering)
    }
    else if (input$number_clusters == "4"){
      ff <- head(clustering4, input$slider_clustering)
    }
    

    ff <- head(ff, input$slider_clustering)
    
    highchart(height = "automatic", width = "automatic") %>% 
      hc_add_series_scatter(round(ff$PC1,2),
                            round(ff$PC2,2),
                            color = ff$clusters,
                            dataLabels = list(
                              enabled = TRUE,
                              format = "{point.label}"
                            )) %>% 
      hc_chart(zoomType = "xy") %>% 
      hc_xAxis(title = list(text = "Factor Blend",
                            opposite = TRUE)) %>%
      hc_yAxis(title = list(text = "Factor Blend 1",
                            opposite = TRUE)) %>%
      hc_tooltip(useHTML = TRUE,
                 headerFormat = "<table>",
                 pointFormat = paste("<tr><th colspan=\"1\"><b>{point.label}</b></th></tr>",
                                     "<tr><th>Component x:</th><td>{point.x} </td></tr>",
                                     "<tr><th>Component y:</th><td>{point.y} </td></tr>",
                                     "<tr><th>Cluster Group:</th><td> Cluster {point.valuecolor}</td></tr>"),
                 footerFormat = "</table>")
  })
  
  output$text1 <- renderText({paste(input$show_vars,
                      "compared to salary")
  })
  
    
  output$comments_cluster1 <- renderText({
    paste("The green cluster are a group 
          of employees with the lowest satisfaction level
          and the largest number of projects. As it seems
          this group are mature employees with no promotions for the last 5 years")
  })
  
  output$comments_cluster2 <- renderText({
    paste("The blue cluster is a group
          of employees with very good satisfaction level
          and an average number of projects. This is the best cluster.")
  })
  
  output$comments_cluster3 <- renderText({
    paste("The yellow cluster received the most promotions and 
          has a very good satisfaction level with a medium
          number of projects.")
  })
  
  output$comments_cluster4 <- renderText({
    paste("The purple cluster is the most dangerous group. The satisfaction
          level is very low with very low number of projects. The employees that
          are still in the company are very likely to leave")
  })
  
  output$cluster_desc_chart <- renderHighchart({
    highchart(height = "automatic", width = "automatic") %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = colnames(aggr_data)[2:5]) %>%
      hc_yAxis(title = list(text = "Value")) %>%
      hc_plotOptions(bar = list(
        dataLabels = list(enabled = TRUE),
        enableMouseTracking = FALSE)) %>%
      hc_series(
        list(
          name = "Cluster1",
          data = round(preparing_the_desc_clu_data(1),2), color = "#4A235A" 
        ),
        list(
          name = "Cluster2",
          data = round(preparing_the_desc_clu_data(2),2), color = "#154360" 
        ),
        list(
          name = "CLuster3",
          data = round(preparing_the_desc_clu_data(3),2), color = "#229954"
        ),
        list(
          name = "CLuster4",
          data = round(preparing_the_desc_clu_data(4),2), color = "#F4D03F"
        )) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th colspan=\"1\"><b>{point.label}</b></th></tr>",
                                   "<tr><th>Value:</th><td>{point.y} </td></tr>",
                                   "<tr><th>Cluster:</th><td> {point.series.name}</td></tr>"),
               footerFormat = "</table>") 
      
  })
  
})
