#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(class)    
library(e1071)
library(dplyr)
library(RCurl)
library(ggplot2)
library(caret)
library(dplyr)
library(magrittr)
library(tidyverse)
library(reshape2)
library(ggcorrplot)
library(cowplot) 
library(shiny)
library(plotly)
library(htmlwidgets)
library(ggthemes)
# Read in employee data from AWS S3 location
job_df <- read.table(textConnection(getURL(
  "https://ddsproject1.s3.us-east-2.amazonaws.com/CaseStudy2-data.csv"
)), sep=",", header=TRUE)

#Assigning another data frame
cs2df =job_df

#Ordering the columns so that all numerical features are together
cs2df = cs2df %>% 
  select('ID','Attrition','BusinessTravel','DailyRate','Department','EducationField','EmployeeCount',
         'EmployeeNumber','Gender','HourlyRate','JobRole','MaritalStatus','Over18','OverTime',
         'StandardHours','TrainingTimesLastYear','DistanceFromHome','Education','MonthlyRate',
         'PercentSalaryHike','PerformanceRating','YearsAtCompany','YearsInCurrentRole',
         'YearsSinceLastPromotion','YearsWithCurrManager','WorkLifeBalance','JobInvolvement',
         'JobSatisfaction','JobLevel','MonthlyIncome','Age','EnvironmentSatisfaction',
         'NumCompaniesWorked','RelationshipSatisfaction','StockOptionLevel','TotalWorkingYears')

#Employee Attrition Count & Percentage
EmpAttrCnP <- cs2df %>% group_by(Attrition) %>% summarise(Count=n()) %>%
  mutate(pct=round(prop.table(Count),2) * 100) %>% 
  mutate(cntpct=paste(Count, "(",pct,"%)")) %>% 
  ggplot(aes(y=Attrition, x=Count)) + geom_bar(stat="identity", fill="turquoise4", color="orange") + 
  ggthemes::theme_solarized() + 
  geom_text(aes(y=Attrition, x=Count-200, label= cntpct),
            hjust=-0.8, vjust=0, size=4, 
            colour="white", fontface="bold",
            angle=360) + labs(title="Employee Attrition Count & Percentage", y="Employee Attrition",x="Count") +
  theme(text = element_text(size = 12)) 

#Assigning another data frame
cs2dfa = cs2df
cs2dfa$Attrition = ifelse(cs2dfa$Attrition == 'Yes',1,0)
cs2dfa$OverTime = ifelse(cs2dfa$OverTime == 'Yes',1,0)
min_max <- function(x) {
  res <- (x - min(x))/(max(x) - min(x))
  return(res)
}
cs2dfa <-  cbind(cs2dfa[,1:16],sapply(cs2dfa[,17:36], min_max))
# Categorical variables to numeric
cs2dfa <- cbind(cs2dfa, model.matrix(~ BusinessTravel - 1, cs2dfa))
cs2dfa <- cbind(cs2dfa, model.matrix(~ Department - 1, cs2dfa))
cs2dfa <- cbind(cs2dfa, model.matrix(~ MaritalStatus - 1, cs2dfa))
cs2dfa <- cbind(cs2dfa, model.matrix(~ JobRole - 1, cs2dfa))

#Analysis on these 3 Top Factors

#Overtime
AttrByOverTime <- cs2df %>%
  ggplot(aes(y= OverTime, fill= Attrition )) +   geom_bar(position = "fill") + scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange3", "turquoise4")) + ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Over Time") + xlab("Percentage( % )")+ ylab("Over Time") +
  theme(text = element_text(size = 12)) 

#TotalWorkingYears
AttrByTotalWorkingYears <- cs2df %>% filter(TotalWorkingYears <30) %>%
  ggplot(aes(y= TotalWorkingYears, fill= Attrition )) +   geom_bar(position = "fill", color = "white" ) + scale_x_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("orange3", "turquoise4")) + ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Total Working Years") + xlab("Percentage( % )")+ ylab("Total Working Years") +
  theme(text = element_text(size = 12)) 

#Job Involvement
AttrByJobInvolvement <- cs2df %>%
  ggplot(aes(y= JobInvolvement, fill= Attrition )) +   geom_bar(position = "fill") + scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange3", "turquoise4")) + ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Job Involvement") + xlab("Percentage( % )")+ ylab("Job Involvement") +
  theme(text = element_text(size = 12)) 


IncCat <- job_df %>% mutate(IncomeCategory=cut(job_df$MonthlyIncome,
                                     breaks=c(1080, 3000, 7000, 12000, 17000, 19999),
                                     labels=c('Very Low(<$3k)', 'Low($3k-$7K)', 'Medium($7k-$12K)', 
                                              'High($12k-$17K)', 'Very High($17k-$20K)'))) %>%
  group_by(IncomeCategory)  %>% summarise(Count=n()) %>%
  ggplot(aes(x=IncomeCategory, y= Count)) + 
  geom_bar(stat = "identity", color = "orange", fill="turquoise2") +
  ggthemes::theme_solarized() +
  geom_text(aes(x=IncomeCategory, y=Count-30, label= Count),
            hjust=.7, vjust=-1, size=4, 
            colour="black", fontface="bold",
            angle=360) +   theme(axis.text.x = element_text(angle = 45, size = 13, hjust=1, vjust=1),
                                 axis.text.y = element_text( size = 13))  +
  labs(title="Income Category & Counts", y="",x="Employee Income Category") +  
  theme(text = element_text(size = 14)) 


#Correlogram of Employee Features and their relationships
cs2dfnums <- select_if(cs2dfa[,20:54], is.numeric)
corr <- round(cor(cs2dfnums), 1)
Correlogram <- ggcorrplot(corr, 
                          type = "lower", 
                          lab = TRUE, 
                          lab_size = 2, 
                          method="circle", 
                          colors = c("tomato2", "white", "#01A9DB"), 
                          title="Correlogram to find feature relationships", 
                          ggtheme=theme_minimal()) + theme(text = element_text(size = 8)) +
  theme(axis.text.x = element_text( size = 6, angle=90 , hjust=1, vjust=1),
        axis.text.y = element_text( size = 6))

#Analysis on these 3 Top Factors

#Mean Monthly Income for Job Levels
JobLevelMMI <- cs2df %>% group_by(JobLevel) %>%
  summarize(meanMonInc = mean(MonthlyIncome)) %>% 
  mutate(meanwdol=paste("$",round(meanMonInc,2))) %>%
  ggplot(aes(x= as.factor(JobLevel)  , y= meanMonInc )) +
  geom_bar(stat = "identity", fill="turquoise2")  + 
  ggthemes::theme_solarized() +
  geom_text(aes(x=JobLevel, y=meanMonInc-1600, label= meanwdol),
            hjust=.5, vjust=-1, size=4, 
            colour="black", fontface="bold",
            angle=360) +   theme(axis.text.x = element_text( size = 13),
                                 axis.text.y = element_text( size = 13)) +
  ggtitle("Mean Monthly Income for Job Levels") +
  xlab("Job Level")+ ylab("Monthly Income") +
  theme(text = element_text(size = 12))

#Mean Monthly Income for Total Working Years
TotWrkYrsMMI <- cs2df %>% filter(TotalWorkingYears <36) %>%
  group_by(TotalWorkingYears) %>%
  summarize(meanMonInc = mean(MonthlyIncome)) %>% 
  mutate(meanwdol=paste("$",round(meanMonInc,0))) %>%
  ggplot(aes(x= as.factor(TotalWorkingYears)  , y= meanMonInc )) +
  geom_bar(stat = "identity", fill="turquoise2") +
  ggthemes::theme_solarized() +
  geom_text(aes(x=TotalWorkingYears, y=meanMonInc, label= meanwdol),
            hjust=1, vjust=1.8, size=4, 
            colour="black", fontface="bold",
            angle=90) +
  theme(axis.text.x = element_text( size = 13),
        axis.text.y = element_text( size = 13)) +
  ggtitle("Mean Monthly Income for Total Working Years") + 
  xlab("Total Working Years")+ ylab("Monthly Income") +
  theme(text = element_text(size = 12))

#Mean Monthly Income for Job Roles
JobRoleMMI <- cs2df %>% group_by(JobRole) %>% 
  summarize(meanMonInc = mean(MonthlyIncome)) %>% 
  mutate(meanwdol=paste("$",round(meanMonInc,0))) %>%
  ggplot(aes(x= JobRole  , y= meanMonInc )) +
  geom_bar(stat = "identity", fill="turquoise2") +
  ggthemes::theme_solarized() +
  geom_text(aes(x=JobRole, y=meanMonInc, label= meanwdol),
            hjust=.8, vjust=.5, size=4, 
            colour="black", fontface="bold",
            angle=90) +
  theme(axis.text.x = element_text( size = 13, angle=90 , hjust=1, vjust=1),
        axis.text.y = element_text( size = 13)) +
  ggtitle("Mean Monthly Income for Job Roles") +
  xlab("Job Role")+ ylab("Monthly Income") +
  theme(text = element_text(size = 12))

#Job Role Vs Years at Company
JobRoleTrends <- job_df %>% group_by(JobRole, YearsAtCompany)  %>%
  ggplot2::ggplot(aes(x=YearsAtCompany, color=JobRole)) +
  ggthemes::theme_solarized() + 
  geom_line(stat = "count", linetype = "solid")+
  facet_wrap(~JobRole) +
  labs(title="Job Role Vs Years at Company", y="Count",x="Years At Company") +
  theme(text = element_text(size = 14)) 
#Other Trends
#Employees who travel frequently as part of job are leaving the company more often
BizTrAttr <- cs2df %>% 
  ggplot2::ggplot(aes(y=BusinessTravel, fill = Attrition)) +
  geom_bar(position = "fill") + 
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange3", "turquoise4")) + 
  ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Business Travel") + 
  xlab("Percentage( % )")+ 
  ylab("Business Travel") +
  theme(axis.text.y = element_text( size = 10, angle=45 , hjust=1, vjust=1)) +
  theme(text = element_text(size = 12)) 
#Employees who have no stock options tend to  leave the company more often
StoOptLvlAttr <- cs2df %>% 
  ggplot2::ggplot(aes(y=StockOptionLevel, fill = Attrition)) +
  geom_bar(position = "fill") + 
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange3", "turquoise4")) + 
  ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Stock Option Level") + 
  xlab("Percentage( % )")+ 
  ylab("Stock Option Level") +
  theme(text = element_text(size = 12)) 
#Employees who have less job satisfaction tend to  leave the company more often
JobSatAttr <- cs2df %>% 
  ggplot2::ggplot(aes(y=JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") + 
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange3", "turquoise4")) + 
  ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Job Satisfaction") + 
  xlab("Percentage( % )")+ 
  ylab("Job Satisfaction") +
  theme(text = element_text(size = 12)) 
#Employees who have less work life balance tend to  leave the company more often
WrkLifBalAttr <- cs2df %>% ggplot2::ggplot(aes(y=WorkLifeBalance, fill = Attrition)) +
  geom_bar(position = "fill") + 
  scale_x_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange3", "turquoise4")) + 
  ggthemes::theme_solarized() + 
  ggtitle("Attrition Percentage by Work Life Balance") + 
  xlab("Percentage( % )")+ 
  ylab("Work Life Balance") +
  theme(text = element_text(size = 12)) 


ui <- fluidPage(
  
  # Application title
  titlePanel("Employee Data and plots"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("select", "Select from dropdown list", 
                  c("View Employee Data"="viewdata", "Correlogram"="Correl","Attrition Analysis" = "TFAttr","Monthly Income Analysis" = "TFMonInc", "Job Role related Trends" = "JobRoleT", "Other Trends" =  "OthrT"), 
                  selected = 1),
      conditionalPanel(
        condition = "input.select == 'TFAttr'",
        radioButtons("radioa","Select an option",
                     choices = list("Attrition Percentage & Count" = 1, "Attrition Percentage By Over Time" = 2,
                                    "Attrition Percentage By Job Involvement" = 3, "Attrition Percentage By Total Working Years" = 4), 
                     selected = 1),
      ),
      conditionalPanel(
        condition = "input.select == 'TFMonInc'",
        radioButtons("radiob","Select an option",
                     choices = list("Income Category" = 1, "Mean Monthly Income by Job Levels" = 2,
                                    "Mean Monthly Income by Job Roles" = 3, "Mean Monthly Income by Total Working Years" = 4), 
                     selected = 1),
      ),
      conditionalPanel(
        condition = "input.select == 'OthrT'",
        radioButtons("radioc","Select an option",
                     choices = list("Attrition Percentage by Business Travel" = 1, "Attrition Percentage by Stock Option Level" = 2,
                                    "Attrition Percentage by  Job Satisfaction" = 3, "Attrition Percentage by Work Life Balance" = 4),
                     selected = 1)
      )),
    mainPanel(
      # plotOutput(outputId = "distPlot"),
      # textOutput("Employee Data"),
      # fluidRow(
      #   column(12,
      #          dataTableOutput('table')
      #   )),
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Table", fluidRow(
          column(12,
                 dataTableOutput('table')
          )))
      )
    ))
)

server <-  function(input,output) {

  
  output$distPlot <- renderPlot({

    if(input$select == "viewdata"){
      output$table <- renderDataTable(job_df,
                                      options = list(
                                        pageLength = 25)
      )
    } else if(input$select == "Correl"){
      Correlogram
      } else if(input$select == "TFAttr" && input$radioa == 1){
        EmpAttrCnP
      } else if(input$select == "TFAttr" && input$radioa == 2){
        AttrByOverTime
      } else if(input$select == "TFAttr" && input$radioa == 3){
        AttrByJobInvolvement
      } else if(input$select == "TFAttr" && input$radioa == 4){
        AttrByTotalWorkingYears
      } else if(input$select == "TFMonInc" && input$radiob == 1){
        IncCat
      } else if(input$select == "TFMonInc" && input$radiob == 2){
        JobLevelMMI
      } else if(input$select == "TFMonInc" && input$radiob == 3){
        JobRoleMMI
      } else if(input$select == "TFMonInc" && input$radiob == 4){
        TotWrkYrsMMI
      } else if(input$select == "JobRoleT"){
        JobRoleTrends
      } else if(input$select == "OthrT" && input$radioc == 1){
        BizTrAttr
      } else if(input$select == "OthrT" && input$radioc == 2){
        StoOptLvlAttr
      } else if(input$select == "OthrT" && input$radioc == 3){
        JobSatAttr
      } else if(input$select == "OthrT" && input$radioc == 4){
        WrkLifBalAttr
    }
  })
}

shinyApp(ui = ui, server = server)
