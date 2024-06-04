library(shiny)
library(shinythemes)
library(bslib)
library(bsicons)
library(ggplot2)
library(forcats)
library(dplyr)
library(DT)
library(stringr)

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
                 dateRangeInput('dates', 'Dates')
               )
             ),
             
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
                              layout_columns(
                                col_widths = c(12, 12),
                                plotOutput('plot2'),
                                plotOutput('plot3'),
                              ),
                              plotOutput('plot1')))
             )

           )
           
           
  ),
  
  tabPanel('Data',
           
           DT::dataTableOutput('dataTable')
          ),
  
)

server <- function(input, output, session) {
  
  output$dataTable <- renderDataTable(
    {
      DT::datatable(data)
    }
  )
  
  output$revenue <- renderText(
    {
      total_revenue <- sum(data$revenue)
      euro(total_revenue)
    }
  )
  
  output$cost <- renderText(
    {
      total_cost <- sum(data$commissions)
      euro(total_cost)
    }
  )
  
  output$quantity_sold <- renderText(
    {
      total_quantity <- sum(data$quantity)
      scales::label_number(big.mark = '.', decimal.mark = ',')(total_quantity)
    }
  )
  
  output$plot1 <- renderPlot(
    {
      by.country <- group_by(data, country) %>%
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
  
  
  output$plot2 <- renderPlot(
    {
      grouped <- data %>%
        group_by(as.Date(order_date)) %>%
        summarise(rev = sum(revenue))
      
      colnames(grouped)[1] <- 'order_date'
      grouped %>% ggplot(aes(x = order_date, y = rev)) + geom_line()
    }
  )
  
  output$plot3 <- renderPlot(
    {
      grouped <- data %>%
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