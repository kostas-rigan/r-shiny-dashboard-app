library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
options(shiny.maxRequestSize=30*1024^2)

ui <- navbarPage(
  'E-Commerce Dashboard App',
  tabPanel('Data',
           tabsetPanel(
             tabPanel('Input',
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          width = 3,
                          
                          # Input: Select a file ----
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          # Input: Select quotes ----
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          width = 9,
                          
                          # Output: Data file ----
                          tableOutput("preview")
                          
                        )
                      
                    ),
             
             tabPanel('Data Table',
                      DT::dataTableOutput('dataTable'))
           )
          ),
  
  tabPanel('Dashboard',
           
           h1('Hello World'),
           
           plotOutput('plot')
           )
  
)

server <- function(input, output, session) {
  
  data = reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    return(df)
  })
  
  output$preview <- renderTable({
    req(input$file1)

    return(head(data(), 10))
  })
  
  output$dataTable <- renderDataTable(
    {
      req(input$file1)
      DT::datatable(data())
    }
  )
  
  output$plot <- renderPlot(
    {
      df <- data()
      sub.df <- df[, c('is_gift', 'revenue')]
      sub.df %>% ggplot(aes(y = (is_gift), x = revenue)) + geom_col()
    }
  )
  
}

shinyApp(ui, server)