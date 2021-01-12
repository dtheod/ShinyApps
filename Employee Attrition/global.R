library(shiny)
library(highcharter)
library(data.table)
library(dplyr)
library(tibble)
library(shinythemes)
library(DT)
library(dplyr)
library(caret)
library(cluster)
library(e1071)


print("global is working")
hr_data<- read.csv("Human_Resources.csv")
clustering2 <- read.csv("clustering2")
clustering3 <- read.csv("clustering3")
clustering4 <- read.csv("clustering4")

hr_data <- setDT(hr_data)
clustering2 <- setDT(clustering2)
clustering3 <- setDT(clustering3)
clustering4 <- setDT(clustering4)

hr_data_avg <- hr_data%>%
  group_by(sales) %>%
  summarise(satisfaction_average = mean(satisfaction_level),
            number_of_projects_average  = mean(number_project),
            time_spend_company_average = mean(time_spend_company),
            average_last_promotions = mean(promotion_last_5years),
            average_monthly_hours = mean(average_montly_hours),
            left_average = mean(left),
            Frequency = n()
  )


salary_function <- function(salary_measure, df){
  hr_data1 = df %>%
    filter(salary  == salary_measure)
  return(hr_data1)
}

aggr_data <- clustering4 %>%
  group_by(clusters) %>%
  summarise(statisfaction_average = mean(satisfaction_level),
            number_project_average  = mean(number_project),
            promotion_average = sum(promotion_last_5years),
            left_average = mean(left)
  )

  

preparing_the_desc_clu_data <- function(cluster_name_index){
  c1 <- tbl_df(aggr_data) %>%
    slice(cluster_name_index) %>% 
    unlist(., use.names=FALSE)
  c1 <- c1[2:5]
  return(c1)
}

label_naming <- function(raw_data){
  raw_data$left <- as.character(raw_data$left)
  new_label <- recode(raw_data$left, 
                      "1" = "Left the Company",
                      "0" = "Still in the Company"
  )
  raw_data <- raw_data[, c("left") := new_label]
  return(raw_data)
}

hr_data_intro1 <- head(hr_data[hr_data$left == 1],25)
hr_data_intro0 <- head(hr_data[hr_data$left == 0],25)
hr_data_intro <- rbind(hr_data_intro0, hr_data_intro1)
hr_data_intro <- hr_data_intro[,c("left","satisfaction_level","number_project", "salary"), with = FALSE]

hr_data_intro <-label_naming(hr_data_intro)

