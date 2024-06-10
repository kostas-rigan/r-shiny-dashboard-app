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
library(fastDummies)
library(shinycssloaders)
library(factoextra)
library(cluster)

euro <- scales::label_currency(
  largest_with_fractional = 1000000,
  accuracy = 2,
  prefix = '',
  suffix = '\U20AC',
  big.mark = '.',
  decimal.mark = ','
)

card_visual <- function(card, title, color) {
  return(value_box(
    title = p(title, style = 'font-weight: bold; font-size: 24px'), 
    value = textOutput(card),
    theme = value_box_theme(bg = color, fg = "#F5F5F5"),
    showcase = bsicons::bs_icon("graph-up"), showcase_layout = "left center",
    full_screen = FALSE, fill = TRUE, height = NULL
  ))
}

aggregator_select_input <- function(id) {
  return(selectInput(id,
                     label = 'Aggregator',
                     choices = list('Sum' = 'sum',
                                    'Average' = 'mean',
                                    'Median' = 'median',
                                    'Count' = 'n')))
}

metric_select_input <- function(id) {
  return(selectInput(id,
                     label = 'Metric',
                     choices = list('Revenue' = 'revenue',
                                    'Cost' = 'commissions',
                                    'Profit' = 'profit')))
}

group_and_summarize <- function(dataframe, by_variable, input_agg_func) {
  agg_func <- match.fun(input_agg_func)
  
  if (input_agg_func == 'n') {
    grouped <- group_by(dataframe, !!by_variable) %>%
      summarise(agg_metric = agg_func())
  } else {
    grouped <- group_by(dataframe, !!by_variable) %>%
      summarise(agg_metric = agg_func(metric))
  }
  
  return (grouped)
}

sales_data <- as_tibble(read.csv('sales_data.csv'))
world <- map_data('world')
world <- filter(world, region != 'Antarctica')

ui <- navbarPage(
  'E-Commerce Dashboard App',
  
  
  tabPanel('Dashboard',

           fluidPage(
             shinythemes::themeSelector(),
             
             # FIRST ROW - PAGE FILTERS
             fluidRow(
               layout_column_wrap(
                 width = 1/6,
                 selectInput('category', 
                             'Category', 
                             choices = c('All', unique(sales_data$category)), 
                             selected = 'All'),
                 selectInput('is_gift', 
                             'Is Gift', 
                             choices = c('All', unique(sales_data$is_gift))),
                 selectInput('publisher', 
                             'Publisher', 
                             choices = c('All', unique(sales_data$publisher))),
                 selectInput('narrator', 
                             'Narrator', 
                             choices = c('All', unique(sales_data$narrator))),
                 selectInput('country', 
                             'Country', 
                             choices = c('All', unique(sales_data$country))),
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
                 card_visual('revenue', 'Total Revenue', '#1AC765'),
                 card_visual('cost', 'Total Cost', "#E04A4A"),
                 card_visual('profit', 'Total Profit', "#6492EF"),
                 card_visual('quantity_sold', 'Products Sold', "#7846EB")
               )
               ),
             

             # THIRD ROW - PLOTS
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(6, 6),
                              
                              layout_columns(
                                col_widths = c(6, 6, 12),
                                metric_select_input('timeline_metric'),
                                aggregator_select_input('timeline_agg_func'),
                                plotOutput('timeline')
                              ),
                              layout_columns(
                                col_widths = c(4, 4, 4, 12),
                                selectInput('bar_categorical',
                                            label = 'Categorical',
                                            choices = list('Category' = 'category',
                                                           'Book' = 'product_name',
                                                           'Is Gift' = 'is_gift',
                                                           'Publisher' = 'publisher',
                                                           'Narrator' = 'profit')),
                                metric_select_input('bar_metric'),
                                aggregator_select_input('bar_agg_func'),
                                plotOutput('bar')
                              )
                              ))
             ),
             
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(3, 9),
                                page_fillable(
                                  h1('Parameters'),
                                  metric_select_input('map_metric'),
                                  aggregator_select_input('map_agg_func'),
                                  checkboxInput('map_exclude_greece', 
                                                'Exclude Greece')
                                ),
                                plotOutput('map'))
               )
             ),
             
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(3, 9),
                                page_fillable(
                                  h1('Parameters'),
                                  selectInput('time_bar_interval',
                                              label = 'Interval',
                                              choices = list('By Day of Week' = 'wday',
                                                             'By Month' = 'month',
                                                             'By Hour of Day' = 'hour')),
                                  metric_select_input('time_bar_metric'),
                                  aggregator_select_input('time_bar_agg_func')
                                ),
                                
                                plotOutput('time_bar'))
                
               )
             )

           )
           
           
  ),
  
  tabPanel('Data',
           
           DT::dataTableOutput('dataTable')
          ),
  
  tabPanel('Clustering',
           sidebarLayout(
             sidebarPanel(
               h1('Parameters'),
               p(tags$i('Note: In this version all tests are conducted from k = 1 to 10')),
               br(),
               sliderInput('sample_size', 'Sample Size (%):', value = 0.5, min = 0, max = 1, step = 0.01),
               selectInput('segmentation_var', label = 'Segment:', choices = c('Customers' = 'user_id', 'Orders' = 'order_id')),
               selectInput('clustering_var', label = "By Variable:", choices = c('Category' = 'category', 'Narrator' = 'narrator')),
               actionButton('run_tests', 'Run Test!'),
               br(),
               numericInput('k', 'k', value = 2, min = 2, max = 10, step = 1),
               actionButton('run_kmeans', 'Run K-Means!')
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel('Elbow',
                          h1('Hello'),
                          plotOutput('elbow') %>% withSpinner(color="#0dc5c1")),
                 tabPanel('Silhouette',
                          h1('Kalispera'),
                          numericInput('k_sil', 'k', value = 2, min = 2, max = 10, step = 1),
                          plotOutput('silhouette') %>% withSpinner(color="#0dc5c1")),
                 tabPanel('Results',
                          h1('Iaso kokla'),
                          DT::dataTableOutput('clustering'))
               )
             )
           )
           
           )
  
)

server <- function(input, output, session) {
  
  filtered <- reactive(
    {
      date1 <- input$dates[1]
      date2 <- input$dates[2]
      df <- sales_data %>%
        dplyr::filter(order_date >= date1 & order_date <= date2 &
                        (input$category == 'All' | input$category == category) &
                        (input$is_gift == 'All' | input$is_gift == is_gift) &
                        (input$publisher == 'All' | input$publisher == publisher) &
                        (input$narrator == 'All' | input$narrator == narrator) &
                        (input$country == 'All' | input$country == country))
      return(df)
    }
  )
  
  output$dataTable <- renderDataTable(
    {
      DT::datatable(sales_data)
    }
  )
  
  output$revenue <- renderText(
    {
      df <- filtered()
      total_revenue <- sum(df$revenue)
      euro(total_revenue)
    }
  )
  
  output$cost <- renderText(
    {
      df <- filtered()
      total_cost <- sum(df$commissions)
      euro(total_cost)
    }
  )
  
  output$profit <- renderText(
    {
      df <- filtered()
      total_profit <- sum(df$profit)
      euro(total_profit)
    }
  )
  
  output$quantity_sold <- renderText(
    {
      df <- filtered()
      total_quantity <- sum(df$quantity)
      scales::label_number(big.mark = '.', decimal.mark = ',')(total_quantity)
    }
  )
  
  output$map <- renderPlot(
    {
      df <- filtered()[, c('country', input$map_metric)]
      
      lookup = c(metric = input$map_metric)
      
      df <- rename(df, all_of(lookup))
      by.country <- group_and_summarize(df, quo(country), input$map_agg_func)
      
      if (input$map_exclude_greece) {
        by.country <- filter(by.country, country != 'Greece')
      }
      
      country.agg <- merge(world, by.country, by.x = 'region', by.y = 'country', all.x=T)
      country.agg <- arrange(country.agg, region, group, order)
      ggplot(country.agg, aes(x = long, y = lat, group = group, fill = agg_metric)) + 
        geom_polygon() + 
        coord_quickmap() +
        theme_void()
    }
  )
  
  
  output$timeline <- renderPlot(
    {
      df <- filtered()[, c('order_date', input$timeline_metric)]
      lookup = c(metric = input$timeline_metric)
      
      df <- rename(df, all_of(lookup))
      df$order_date <- as.Date(df$order_date)
      
      by.date <- group_and_summarize(df, quo(order_date), input$timeline_agg_func)
      
      
      by.date %>% ggplot(aes(x = order_date, y = agg_metric)) + geom_line()
    }
  )
  
  output$bar <- renderPlot(
    {
      df <- filtered()
      grouped <- df %>%
        group_by(category) %>%
        summarise(rev = sum(revenue))
    
      grouped <- grouped[!is.na(grouped[, 1]), ]
      
      grouped <- head(grouped)
    
      ggplot(grouped, aes(x = rev, y = fct_reorder(factor(category), rev, na.rm = F))) + geom_col()
    }
  )
  
  output$time_bar <- renderPlot(
    {
      df <- filtered()[, c('order_date', input$time_bar_metric)]
      lookup = c(metric = input$time_bar_metric)
      
      df <- rename(df, all_of(lookup))
      
      interval_type_fun <- match.fun(input$time_bar_interval)
      
      if (input$time_bar_interval != 'hour') {
        df$interval <- interval_type_fun(df$order_date, label = T, abbr = F)
        just <- 0.5
      } else {
        df$interval <- ymd_hms(df$order_date) %>% interval_type_fun()
        just <- 0
      }
      
      grouped_date <- group_and_summarize(df, quo(interval), input$time_bar_agg_func)
      ggplot(grouped_date, aes(x = interval, y = agg_metric)) + 
        geom_col(just = just)
    }
  )
  
  ## CLUSTERING
  dummified <- function() {
    df <- sales_data[, c(input$segmentation_var, input$clustering_var)]
    lookup = c(seg = input$segmentation_var, cluvar = input$clustering_var)
    
    df <- rename(df, all_of(lookup))
    
    dummy <- dummy_columns(df, select_columns = 'cluvar', remove_selected_columns = T)
    dummy <- replace(dummy, is.na(dummy), 0)
    
    dummy[, -1] <- scale(dummy[, -1])
    
    return(dummy)
  }
  
  cluster_test_data <- eventReactive(input$run_tests, {
    dummy <- dummified()
    slice_sample(dummy, prop = input$sample_size)
  })
  
  cluster_run_data <- eventReactive(input$run_kmeans, dummified())

  cluster_tests <- reactive(
    {
      req(input$run_tests)
      sample <- cluster_test_data()

      lower_k <- 1
      higher_k <- 10

      elb <- data.frame(matrix(nrow = 0, ncol = 2))

      for (k in lower_k : higher_k) {
        results <- kmeans(sample[, -1], k)
        elb <- rbind(elb, c(k, results$tot.withinss))
      }
      colnames(elb) <- c('k', 'twss')
      return(elb)
    }
  )
  
  output$elbow <- renderPlot(
    {
      results <- cluster_tests()
      ggplot(results, aes(x = k, y = twss)) + geom_line() + geom_point()
    }
  )
  
  output$silhouette <- renderPlot(
    {
      req(input$run_tests)

      sample <- cluster_test_data()
      results <- kmeans(sample[, -1], input$k_sil)
      sil <- silhouette(results$cluster, dist(sample[, -1]))
      fviz_silhouette(sil)
    }
  )
  
  output$clustering <- renderDataTable(
    {
      req(input$run_kmeans)
      df <- cluster_run_data()
      results <- kmeans(df, centers = input$k)
      df_clu <- as_tibble(data.frame(id = df$seg, cluster = results$cluster))

      temp <- merge(sales_data, df_clu, by.x = input$segmentation_var, by.y = 'id', all.x=T) %>%
        arrange(X) %>%
        filter(duplicated(X) == FALSE)

      DT::datatable(temp,
                    extensions = 'Buttons',
                    options = list(scrollX = TRUE,
                                   paging = TRUE,
                                   searching = TRUE,
                                   fixedColumns = TRUE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'tB',
                                   buttons = c('copy', 'csv', 'excel')))
    }
  )
}

shinyApp(ui, server)