library(shiny)
options(shiny.maxRequestSize=30*1024^2)

ui <- navbarPage(
  'E-Commerce Dashboard App',
  fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv"))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)