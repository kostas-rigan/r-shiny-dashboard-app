library(shiny)
library(shinythemes)
library(bslib)
library(bsicons)
library(ggplot2)
library(forcats)
library(dplyr)
library(DT)
library(stringr)
library(lubridate)

data <- read.csv('sales_data.csv')
world <- map_data('world')

ui <- navbarPage(
  'E-Commerce Dashboard App',
  
  
  tabPanel('Dashboard',

           fluidPage(
             shinythemes::themeSelector(),
             
             # FIRST ROW - PAGE FILTERS
             fluidRow(
               layout_column_wrap(
                 width = 1/6,
                 selectInput('category', 'Category', choices = unique(data$category), multiple = T),
                 selectInput('is_gift', 'Is Gift', choices = unique(data$is_gift), multiple = T),
                 selectInput('publisher', 'Publisher', choices = unique(data$publisher), multiple = T),
                 selectInput('narrator', 'Narrator', choices = unique(data$narrator), multiple = T),
                 selectInput('book', 'Book', choices = unique(data$product_name), multiple = T),
                 dateRangeInput('dates',
                                'Dates',
                                start = '2020-01-01',
                                end = '2024-06-01')
               )
             ),
             textOutput('text'),
             
             # SECOND ROW - CARD VISUALS
             fluidRow(
               layout_column_wrap(
                 width = 1/4,
                 height = '100px',
                 fixed_width = T,
                 gap = 50,
                 value_box(
                   title = p('Total Revenue', style = 'font-weight: bold; font-size: 24px'), 
                   value = textOutput('revenue'),
                   theme = value_box_theme(bg = "#1AC765", fg = "#F5F5F5"),
                   showcase = bsicons::bs_icon("graph-up"), showcase_layout = "left center",
                   full_screen = FALSE, fill = TRUE, height = NULL
                 ),
                 value_box(
                   title = p('Total Cost', style = 'font-weight: bold; font-size: 24px'), 
                   value = textOutput('cost'),
                   theme = value_box_theme(bg = "#E04A4A", fg = "#F5F5F5"),
                   showcase = bsicons::bs_icon("graph-down"), showcase_layout = "left center",
                   full_screen = FALSE, fill = TRUE, height = NULL
                 ),
                 value_box(
                   title = p('Products Sold', style = 'font-weight: bold; font-size: 24px'), 
                   value = textOutput('quantity_sold'),
                   theme = value_box_theme(bg = "#7846EB", fg = "#F5F5F5"),
                   showcase = bsicons::bs_icon("graph-up"), showcase_layout = "left center",
                   full_screen = FALSE, fill = TRUE, height = NULL
                 )
               )
               ),
             

             # THIRD ROW - PLOTS
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(6, 6),
                              
                              plotOutput('timeline'),
                              plotOutput('bar')
                              ))
             ),
             
             fluidRow(
               page_fillable(
                 plotOutput('map')
               )
             )

           )
           
           
  ),
  
  tabPanel('Data',
           
           DT::dataTableOutput('dataTable')
          ),
  
)

server <- function(input, output, session) {
  output$text <- renderText(
    {
      as.character(input$dates[1])
      
    }
  )
  
  output$dataTable <- renderDataTable(
    {
      DT::datatable(data)
    }
  )
  
  output$revenue <- renderText(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      filtered <- data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2)
      total_revenue <- sum(filtered$revenue)
      euro(total_revenue)
    }
  )
  
  output$cost <- renderText(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      filtered <- data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2)
      total_cost <- sum(filtered$commissions)
      euro(total_cost)
    }
  )
  
  output$quantity_sold <- renderText(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      filtered <- data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2)
      total_quantity <- sum(filtered$quantity)
      scales::label_number(big.mark = '.', decimal.mark = ',')(total_quantity)
    }
  )
  
  output$map <- renderPlot(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      filtered <- data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2)
      by.country <- group_by(filtered, country) %>%
        summarise(revenue = sum(revenue),
                  cost = sum(commissions),
                  profit = sum(profit),
                  quantity = sum(quantity),
                  orders = n())
      
      country.agg <- merge(world, by.country, by.x = 'region', by.y = 'country', all.x=T)
      country.agg <- arrange(country.agg, region, group, order)
      ggplot(country.agg, aes(x = long, y = lat, group = group, fill = revenue)) + 
        geom_polygon() + 
        coord_quickmap() +
        theme_void()
    }
  )
  
  
  output$timeline <- renderPlot(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      filtered <- data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2)
      grouped <- filtered %>%
        group_by(as.Date(order_date)) %>%
        summarise(rev = sum(revenue))
      
      colnames(grouped)[1] <- 'order_date'
      grouped %>% ggplot(aes(x = order_date, y = rev)) + geom_line()
    }
  )
  
  output$bar <- renderPlot(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      filtered <- data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2)
      grouped <- filtered %>%
        group_by(category) %>%
        summarise(rev = sum(revenue))
    
      grouped <- grouped[!is.na(grouped[, 1]), ]
      
      grouped <- head(grouped)
    
      ggplot(grouped, aes(x = rev, y = fct_reorder(factor(category), rev, na.rm = F))) + geom_col()
    }
  )
  
}

euro <- scales::label_currency(
  largest_with_fractional = 1000000,
  accuracy = 2,
  prefix = '',
  suffix = '\U20AC',
  big.mark = '.',
  decimal.mark = ','
)

shinyApp(ui, server)