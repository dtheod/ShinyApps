

shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  navbarPage("Human Resources App",
                             tabPanel("App Introduction",
                
                             fluidRow(column(6,
                                             fluidRow(
                                               column(10,
                                                      h3("Why are the best and most experienced employees leaving early?"),
                                                      hr(),
                                                      h5("This Application provides an introducion to clustering and how it can be used to segment customers. 
                                                         The dataset used for the analysis was taken from Kaggle.You can find more information",
                                                         a(href = 'https://www.kaggle.com/ludobenistant/hr-analytics', strong('here'))),
                                                      hr(),
                                                      tags$ol(
                                                        tags$li(h5("Number of Projects:",style = "margin-bottom:0px"),HTML("How many projects the employee participated.")),
                                                        tags$li(h5("Satisfaction Level:",style = "margin-bottom:0px"),HTML("How satisfied the employee in the company.")),
                                                        tags$li(h5("Salary:",style = "margin-bottom:0px"),HTML("Three categories, low, medium and high indicating salary.")),
                                                        tags$li(h5("Average Monthly Hours:",style = "margin-bottom:0px"),HTML("Average Hours worked during a month.")),
                                                        tags$li(h5("Left:",style = "margin-bottom:0px"),HTML("Employee left the company.")),
                                                        tags$li(h5("Profession:",style = "margin-bottom:0px"),HTML("Profession of the employee.")),
                                                        tags$li(h5("Promotion last 5 years:",style = "margin-bottom:0px"),HTML("If the employee got a promotions"))
                                                        
                                                      )
                                                      ),                
                                                      column(2
                                                      )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h4(),
                                                      h4(),
                                                      tags$img(height = 60, width = 140, src = "Kaggle_logo.png")
                                                      )
                                             )), column(6,
                                                        dataTableOutput("renderTable")
                                                        )
                             )),
                  tabPanel("Descriptive",
                  sidebarLayout(sidebarPanel(
                    radioButtons('show_vars', 
                                       'Features', 
                                       choices = colnames(hr_data)[-c(7,9,10)],
                                       selected = "time_spend_company"
                                       ),selectInput('input_label', 
                                                     'Outcome',
                                                     choices = c("Left the Company", "Still in the Company"),
                                                     selected = c("Left the Company")),
                    sliderInput("sample_size",
                                "Sample size", 1,15000,1000), style = "position: fixed; overlow: visible;"
                    ),
                      mainPanel = mainPanel(
                      h4(textOutput("text1")),
                      hr(),
                      highchartOutput("table", "auto", "auto"),
                      h4("all factors compared to profession"),
                      hr(),
                      highchartOutput("graph","auto", "auto")
                      )
                  )
                  ),
                  tabPanel("Clustering",
                           sidebarLayout(sidebarPanel(
                             sliderInput("slider_clustering",
                                         "Head Size", 1000,5000,1000),
                             selectInput('number_clusters', 
                                         'Number of Clusters',
                                         choices = c("2", "3", "4"),
                                         selected = c("4")),
                             br(),
                             h4("Comments"),
                             hr(),
                             textOutput("comments_cluster1"),
                             br(),
                             textOutput("comments_cluster2"),
                             br(),
                             textOutput("comments_cluster3"),
                             br(),
                             textOutput("comments_cluster4")
                           ),
                           mainPanel(fluidRow(
                             h4("Employees seperated in groups."),
                             hr(),
                             highchartOutput("cluster_chart","auto", "auto"),
                             h4("Characteristics for each group."),
                             hr(),
                             highchartOutput("cluster_desc_chart", "auto", "auto")
                             
                           )
                           )
                           ))
                  )
))