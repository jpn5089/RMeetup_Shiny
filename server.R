library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(plotly)

# revenue is in billions

media_franchises <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

server <- function(input, output) {
  
  output$data_table <- DT::renderDataTable({
    
    print(input$revenue_category)
    
    table <- media_franchises %>% 
      filter(revenue_category %in% input$revenue_category)
    
    DT::datatable(table, rownames = FALSE,  
              filter="top", selection="multiple", escape=FALSE, 
              options = list(sDom  = '<"top">flrt<"bottom">ip')
    )
    
  })
  
  output$scatter_plot <- renderPlot({
    
    pdata <- media_franchises %>% 
      filter(revenue_category %in% input$revenue_category) 
    
    ggplot(pdata, aes(year_created, revenue, col = revenue_category)) + geom_point() +
      labs(y = "Revenue (in billions)", x = "Year Created")
    
  })
  
  #eventReactive creates a reactive value that changes based on the trigger event 
  #In this case, us clicking on the plot button
  
  media_plot_data <- eventReactive(input$plot_media_data, {
      
      media_franchises %>% 
        filter(original_media %in% input$original_media) 
  
  })
  
  output$plot_original_media <- renderPlotly({
    
    p <- ggplot(media_plot_data(), aes(year_created, revenue, col = franchise)) + geom_point() +
        labs(y = "Revenue (in billions)", x = "Year Created")
    
    ggplotly(p)
    
  })
  
  output$media_plot_description <- renderText({
    
    "Plot displaying Revenue by Original Media Type"
    
  })

  output$grouped_by_plot <- renderPlot({
    
    pdata <- media_franchises %>% 
      dplyr::group_by_(input$group_by_var) %>%
      # dplyr::group_by(year_created) %>%
      summarise(total_rev = sum(revenue)) %>% 
      ungroup() %>% 
      arrange(desc(total_rev)) %>% 
      mutate(rank = row_number())
    
    print(head(pdata))
    
    ggplot(pdata, aes(rank, total_rev)) + geom_bar(stat = "identity") +
      scale_x_continuous(breaks = 1:nrow(pdata), labels = pull(pdata[,1])) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Total Revenue (in billions)", x = "")
    
  })
  
  output$grouped_plot_description <- renderText({
    
    "Plot displaying Cumulative Revenue"
    
  })
}

