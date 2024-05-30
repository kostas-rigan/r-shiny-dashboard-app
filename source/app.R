library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

data <- read.csv('sales_data.csv')

ui <- navbarPage(
  'E-Commerce Dashboard App',
  
  tabPanel('Dashboard',
           
           fluidPage(
             fluidRow(
               column(plotOutput('plot1'), width = 4),
               
               column(plotOutput('plot2'), width = 5)
             )
           )
           
           
  ),
  
  tabPanel('Data',
           
           DT::dataTableOutput('dataTable')
          )
  
)

server <- function(input, output, session) {
  
  output$dataTable <- renderDataTable(
    {
      DT::datatable(data)
    }
  )
  
  output$plot1 <- renderPlot(
    {
      sub.df <- data[, c('is_gift', 'revenue')]
      grouped <- sub.df %>%
        group_by(is_gift) %>%
        summarise(rev = sum(revenue))
      grouped %>% ggplot(aes(y = as.factor(is_gift), x = rev)) + geom_col()
    }
  )
  
  output$plot2 <- renderPlot(
    {
      grouped <- data %>%
        group_by(as.Date(order_date)) %>%
        summarise(rev = sum(revenue))
      
      colnames(grouped)[1] <- 'order_date'
      grouped %>% ggplot(aes(x = order_date, y = rev)) + geom_line()
    }
  )
  
}

shinyApp(ui, server)