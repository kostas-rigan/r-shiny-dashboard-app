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
library(maps)

euro <- scales::label_currency(
  largest_with_fractional = 1000000,
  accuracy = 2,
  prefix = '',
  suffix = '\U20AC',
  big.mark = '.',
  decimal.mark = ','
)

card_visual <- function(card, title, bgcolor, icon, fgcolor) {
  return(value_box(
    title = p(title, style = 'font-weight: bold; font-size: 24px'), 
    value = textOutput(card),
    theme = value_box_theme(bg = bgcolor, fg = fgcolor),
    showcase = bsicons::bs_icon(icon), showcase_layout = "left center",
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

metric_mapper <- list('sum' = 'Total', 'mean' = 'Avg', 'median' = 'Median', 'n' = 'Number of')

sales_data <- as_tibble(read.csv('sales_data.csv'))
world <- map_data('world')
world <- filter(world, region != 'Antarctica')


ui <- navbarPage(
  id = 'my-navbar',
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  title = 'E-Commerce Dashboard App',

  tabPanel('Dashboard',

           fluidPage(
             
             
             # FIRST ROW - PAGE FILTERS
             fluidRow(
               layout_column_wrap(
                 width = 1/6,
                 div(
                   class = 'border rounded-2 p-2',
                   style = 'background-color: #f8f8f8',
                   selectInput('category', 
                               'Category', 
                               choices = c('All', unique(sales_data$category)), 
                               selected = 'All')
                 ),
                 div(
                   class = 'border rounded-2 p-2',
                   style = 'background-color: #f8f8f8',
                   selectInput('is_gift', 
                               'Is Gift', 
                               choices = c('All', unique(sales_data$is_gift))),
                 ),
                 
                 div(
                   class = 'border rounded-2 p-2',
                   style = 'background-color: #f8f8f8',
                   selectInput('publisher', 
                               'Publisher', 
                               choices = c('All', unique(sales_data$publisher)))
                 ),
                 div(
                   class = 'border rounded-2 p-2',
                   style = 'background-color: #f8f8f8',
                   selectInput('narrator', 
                               'Narrator', 
                               choices = c('All', unique(sales_data$narrator)))
                 ),
                 div(
                   class = 'border rounded-2 p-2',
                   style = 'background-color: #f8f8f8',
                   selectInput('country', 
                               'Country', 
                               choices = c('All', unique(sales_data$country)))
                 ),
                 div(
                   class = 'border rounded-2 p-2',
                   style = 'background-color: #f8f8f8',
                   dateRangeInput('dates',
                                  'Dates',
                                  start = '2020-01-01',
                                  end = '2024-06-01')
                 )
                 
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
                 card_visual('revenue', 'Total Revenue', 'grey90', 'graph-up', '#4CAF50'),
                 card_visual('cost', 'Total Cost', "grey90", 'graph-down', '#F44336'),
                 card_visual('profit', 'Total Profit', "grey90", 'currency-dollar', '#2196F3'),
                 card_visual('quantity_sold', 'Products Sold', "grey90", 'box-seam', '#FF9800')
               )
               ),
             

             # THIRD ROW - PLOTS
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(6, 6),
                              
                              layout_columns(
                                class = 'border rounded',
                                col_widths = c(12, 12),
                                div(class = 'border-bottom  p-2',
                                    style = 'background-color:#f8f8f8',
                                    popover(
                                      bs_icon('gear'),
                                      metric_select_input('timeline_metric'),
                                      aggregator_select_input('timeline_agg_func'),
                                      textInput('period_breaks', 'Break by (period)', value = '3 months', placeholder = '3 months'),
                                      title = 'Input controls'
                                    )),
                                div(class = 'p-1',
                                    plotOutput('timeline') %>% withSpinner(color="#5b5b5b"))
                                
                              ),
                              layout_columns(
                                class = 'border rounded',
                                col_widths = c(12, 12),
                                
                                div(class = 'border-bottom  p-2',
                                    style = 'background-color:#f8f8f8',
                                    popover(
                                    bs_icon('gear'),
                                      selectInput('bar_categorical',
                                                  label = 'Categorical',
                                                  choices = list('Category' = 'category',
                                                                 'Book' = 'product_name',
                                                                 'Publisher' = 'publisher',
                                                                 'Narrator' = 'profit')),
                                      metric_select_input('bar_metric'),
                                      aggregator_select_input('bar_agg_func'),
                                      numericInput('number_of_items', 'Items to Show', value = 5, min = 1, max = 10, step = 1),
                                      placement = 'right'
                                  )
                                ),
                                div(class = 'p-1',
                                    plotOutput('bar') %>% withSpinner(color="#5b5b5b")
                                )
                              )
                              ))
             ),
             
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(3, 9),
                                class = 'border rounded',
                                div(class = 'border-end p-3 h-100',
                                    style = 'background-color:#f8f8f8',
                                    page_fillable(
                                      metric_select_input('map_metric'),
                                      aggregator_select_input('map_agg_func'),
                                      checkboxInput('map_exclude_greece', 
                                                    'Exclude Greece')
                                    )
                                ),
                                div(class = 'pt-2',
                                  plotOutput('map') %>% 
                                    withSpinner(color="#5b5b5b"))
                                )
               )
             ),
             
             fluidRow(
               page_fillable(
                 layout_columns(col_widths = c(3, 9),
                                class = 'border rounded',
                                div(
                                  style = 'background-color:#f8f8f8',
                                  class = 'h-100 w-100',
                                  div(class = 'border-end ps-3 pt-3',
                                      
                                      page_fillable(
                                        selectInput('time_bar_interval',
                                                    label = 'Interval',
                                                    choices = list('By Day of Week' = 'wday',
                                                                   'By Month' = 'month',
                                                                   'By Hour of Day' = 'hour')),
                                        metric_select_input('time_bar_metric'),
                                        aggregator_select_input('time_bar_agg_func')
                                      )
                                  )
                                ),
                                
                                div(class = 'pt-2',
                                    plotOutput('time_bar') %>% 
                                      withSpinner(color="#5b5b5b"))
                                )
                                
                
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
               class = 'h-100',
               style = 'background-color: #f8f8f8',
               h1('K-Means Parameters'),
               p(tags$i('Note: In this version all tests are conducted from k = 1 to 10')),
               hr(),
               h3('General Options'),
               p(tags$i('These options are applied to both testing and running k-means')),
               selectInput('segmentation_var', label = 'Segment:', choices = c('Customers' = 'user_id', 'Orders' = 'order_id')),
               selectInput('clustering_var', label = "By Variable:", choices = c('Category' = 'category', 'Narrator' = 'narrator')),
               hr(),
               h3('Testing Options'),
               p(tags$i('These options apply only in testing k-means with Elbow and Silhouette Method')),
               sliderInput('sample_size', 'Sample Size (%):', value = 0.5, min = 0, max = 1, step = 0.01),
               div(
                 class="d-flex justify-content-center",
                 actionButton('run_tests', 'Run Test!', class = 'custom-button')
               ),
               hr(),
               h3('k Options'),
               p(tags$i('These options only affect non-testing execution of k-means')),
               numericInput('k', 'k', value = 2, min = 2, max = 10, step = 1),
               div(
                 class="d-flex justify-content-center",
                 actionButton('run_kmeans', 'Run K-Means!', class = 'custom-button')
               )
               
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel('Elbow',
                          plotOutput('elbow') %>% withSpinner(color="#5b5b5b")),
                 tabPanel('Silhouette',
                          numericInput('k_sil', 'k', value = 2, min = 2, max = 10, step = 1),
                          plotOutput('silhouette') %>% withSpinner(color="#5b5b5b")),
                 tabPanel('Results',
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
      
      if (input$map_agg_func == 'n') {
        metr <- 'Orders'
      } else {
        metr <- str_to_title(input$map_metric)
      }
      
      country.agg <- merge(world, by.country, by.x = 'region', by.y = 'country', all.x=T)
      country.agg <- arrange(country.agg, region, group, order)
      ggplot(country.agg, aes(x = long, y = lat, group = group, fill = agg_metric)) + 
        geom_polygon() + 
        coord_quickmap() +
        labs(x = NULL,
             y = NULL,
             title = paste(metric_mapper[input$map_agg_func],
                           metr,
                           'by Country')) +
        scale_fill_gradient(low = '#bcbcbc', high = '#3d85c6', labels = scales::label_number()) +
        theme_void() +
        theme(legend.title = element_blank(),
              plot.title = element_text(face = 'bold', size = 17, hjust = 0.5))
    }
  )
  
  
  output$timeline <- renderPlot(
    {
      df <- filtered()[, c('order_date', input$timeline_metric)]
      lookup = c(metric = input$timeline_metric)
      
      df <- rename(df, all_of(lookup))
      df$order_date <- as.Date(df$order_date)
      
      by.date <- group_and_summarize(df, quo(order_date), input$timeline_agg_func)
      
      if (input$timeline_agg_func == 'n') {
        metr <- 'Orders'
      } else {
        metr <- str_to_title(input$timeline_metric)
      }
      
      by.date %>% 
        ggplot(aes(x = order_date, y = agg_metric)) + 
        geom_line(color = '#0b5394') +
        labs(x = NULL, 
             y = NULL,
             title =
               paste(metric_mapper[input$timeline_agg_func], 
                     metr,
                     'per Day')
             ) +
        scale_x_date(date_breaks = input$period_breaks) +
        theme_minimal() +
        theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5),
              axis.text = element_text(size = 12))
    }
  )
  
  output$bar <- renderPlot(
    {
      df <- filtered()[, c(input$bar_categorical, input$bar_metric)]
      lookup = c(categorical = input$bar_categorical, metric = input$bar_metric)
      
      df <- rename(df, all_of(lookup))
      grouped <- group_and_summarize(df, quo(categorical), input$bar_agg_func)
      
      grouped <- grouped[!is.na(grouped[, 1]), ]
      
      grouped <- arrange(grouped, desc(agg_metric))

      grouped <- head(grouped, input$number_of_items)
      
      if (input$bar_agg_func == 'n') {
        metr <- 'Orders'
      } else {
        metr <- str_to_title(input$bar_metric)
      }
      
      if (input$number_of_items > 1) {
        if (str_ends(input$bar_categorical, 'y')) {
          plural <- paste0(str_sub(input$bar_categorical, start = 1, end = -2), 'ies')
        } else {
          plural <- paste0(input$bar_categorical, 's')
        }
        separator <- paste(input$number_of_items, plural)
      } else {
        separator <- input$bar_categorical
      }
    
      ggplot(grouped, 
             aes(x = agg_metric, 
                 y = fct_reorder(factor(categorical), agg_metric, na.rm = T), 
                 fill = agg_metric)) + 
        geom_col() + 
        labs(x = NULL,
             y = NULL,
             title = paste('Top', 
                           str_to_title(separator), 
                           'by', 
                           metric_mapper[input$bar_agg_func], 
                           metr)) +
        scale_fill_gradient(low = '#bcbcbc', 
                            high = '#3d85c6', 
                            guide = NULL) +
        theme_minimal() +
        theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5),
              axis.text = element_text(size = 12))
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
        
        if (input$time_bar_interval == 'month') {
          inteval_lbl <- 'Month'
        } else {
          inteval_lbl <- 'Day of Week'
        }
        
      } else {
        df$interval <- ymd_hms(df$order_date) %>% interval_type_fun()
        just <- 0
        inteval_lbl <- 'Hour of Day'
      }
      
      grouped_date <- group_and_summarize(df, quo(interval), input$time_bar_agg_func)
      
      if (input$time_bar_agg_func == 'n') {
        metr <- 'Orders'
      } else {
        metr <- str_to_title(input$time_bar_metric)
      }
      
      ggplot(grouped_date, aes(x = interval, y = agg_metric)) + 
        geom_col(just = just, fill = '#3d85c6') +
        labs(x = NULL,
             y = NULL,
             title = paste(metric_mapper[input$time_bar_agg_func], 
                           metr,
                           'by',
                           inteval_lbl)) +
        theme_minimal() +
        theme(plot.title = element_text(face = 'bold', size = 17, hjust = 0.5),
              axis.text = element_text(size = 12))
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