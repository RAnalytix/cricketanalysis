#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)

data <- read.csv("cricketdata.csv",stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    .irs-bar{
      height:8px;
      top: 25px;
      border-top: 1px solid #B22726;
      border-bottom: 1px solid #B22726;
      background: #B22726;
    }
     .irs-bar-edge{
      height:8px;
      top: 25px;
      border: 1px solid #B22726;
      border-right: 0;
      background: #B22726;
      border-radius: 16px 0 0 16px;
     }
.irs-from, .irs-to, .irs-single {
      color: #fff;
      font-size: 11px;
      line-height:1.333;
      text-shadow: none;
      padding: 1px 3px;
      background: #B22726;
      border-radius: 3px;
}
.well {
  background-color:#FFFEAD;
}
.selectize-input.focus{
  border-color:#B22726;
}
                    "))
  ),
   
   # Application title
   titlePanel("Exploration of Test Cricket boundaries"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("batsmen",
                     "Number of Batsmen:",
                     min = 1,
                     max = 30,
                     value = 15),
         sliderInput("runs",
                     "Minimum Number of Runs:",
                     min = 1,
                     max = max(data$TotalRuns),
                     value = 1000),
         selectInput("measure",
                     "Select measure:",
                     c("Total Runs from Boundaries"="runs",
                       "% of Runs from Boundaries"="prop"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(

         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
   
   output$distPlot <- renderPlot({
     if (input$measure!="runs"){
        data %>% filter(TotalRuns>=input$runs) %>% arrange(-PropFromBoundaries) %>% top_n(input$batsmen) %>%
         mutate(order=((input$batsmen+1)-(1:n())))%>% 
         select(order,Player,PropFrom4s,PropFrom6s,TotalRuns,PropFromBoundaries) %>%
         mutate(PropFromOther=1-(PropFrom4s+PropFrom6s)) %>%
         gather(key="key",value="value",-c(order,Player,TotalRuns,PropFromBoundaries)) %>%
         mutate(TotalRuns=if_else(key=="PropFrom4s",paste0(comma(TotalRuns)," runs"),NULL)) %>% 
         mutate(PropFromBoundariesLabel=if_else(key=="PropFromOther",percent(PropFromBoundaries),NULL)) %>%
         ggplot(aes(y=value,x=reorder(Player,order))) +
         geom_col(aes(fill=key),position=position_stack(reverse=TRUE)) +
         geom_text(aes(y=1.15,label=TotalRuns),colour="grey50") +
         geom_text(aes(y=PropFromBoundaries,label=PropFromBoundariesLabel),colour="grey50",hjust=0,nudge_y=0.01) +
         coord_flip() +
         scale_y_continuous("",labels=scales::percent,limits=c(0,1.30),breaks=c(0,.20,.40,.60,.80,1.00)) +
         scale_x_discrete("") +
         scale_fill_manual("Source of batsmen's runs",values=c("#B22726","#FF7675","#FFFEAD"), labels=c("Fours","Sixes","Non-boundary")) +
         theme_minimal() +
         theme(text = element_text(colour="grey50"),
               panel.grid = element_blank(),
               title = element_text(hjust=1,size=10)
         ) +   
         labs(caption="Data from ESPNcricinfo - Design by @stevejburr") 
     }else{
       data %>% filter(TotalRuns>=input$runs) %>% arrange(-RunsFromBoundaries) %>% 
         top_n(input$batsmen,RunsFromBoundaries) %>%
         mutate(order=((input$batsmen+1)-(1:n())))%>% 
         mutate(RunsFromOther=TotalRuns-RunsFromBoundaries) %>%
         group_by(Player) %>%
         gather(key="key",value="value",-c(order,Player,RunsFromBoundaries,TotalRuns)) %>%
         filter(key %in% c("RunsFrom4s","RunsFrom6s","RunsFromOther")) %>%
         arrange(order) %>%
         mutate(RunsFromBoundariesLabel=if_else(key=="RunsFromOther",paste0(comma(RunsFromBoundaries)," / ",comma(TotalRuns)),NULL)) %>%
         ggplot(aes(y=value,x=reorder(Player,order))) +
         geom_col(aes(fill=key),position =position_stack(reverse=TRUE)) +
         geom_text(aes(y=RunsFromBoundaries,label=RunsFromBoundariesLabel),colour="grey50",hjust=0,nudge_y=0.01) +
         coord_flip() +
         theme_minimal() +
         theme(text = element_text(colour="grey50"),
               panel.grid = element_blank()
         )+
         scale_x_discrete("") +
         scale_y_continuous("Test runs",labels=scales::comma ) +
         scale_fill_manual("Source of batsmen's runs",values=c("#B22726","#FF7675","#FFFEAD"), labels=c("Fours","Sixes","Non-boundary"))+   
         labs(caption="Data from ESPNcricinfo - Design by @stevejburr") 
       
     }

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

