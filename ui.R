library(shiny) 
library(DT)
library(dplyr)
library(plotly)

media_franchises <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

ui <- fluidPage(
  titlePanel("Shiny App to analyze Media Franchise Revenue"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput(inputId  = 'revenue_category', label = "Revenue Category", choices = unique(media_franchises$revenue_category),
                             selected = unique(media_franchises$revenue_category)[1], multiple = T)
    ),
    mainPanel(
      plotOutput('scatter_plot'),
      br(), # line break
      br(),
      DT::dataTableOutput('data_table')
    )

  )


)

# ui <- navbarPage(
#   "Shiny App to analyze Media Franchise Revenue",
#   tabPanel("EDA",
#            fluidPage(
#              sidebarLayout(
#                sidebarPanel(width = 2,
#                             selectizeInput(inputId  = 'revenue_category', label = "Revenue Category", choices = unique(media_franchises$revenue_category), 
#                                         selected = unique(media_franchises$revenue_category)[1], multiple = T)
#                ),
#                mainPanel(
#                  plotOutput('scatter_plot'),
#                  br(),
#                  br(),
#                  DT::dataTableOutput('data_table')
#                )
#              )
#            )
#   ),
#   tabPanel("Action Button",
#     fluidPage(
#       #Rows can be a TOTAL width of 12 units
#       fluidRow(
#         column(width = 2,
#                selectInput(inputId = 'original_media', label = "Original Media", choices = unique(media_franchises$original_media),
#                            selected = unique(media_franchises$original_media)[1], multiple = F)),
#         column(width = 2, offset = 1, 
#                actionButton(inputId = "plot_media_data", label = "Plot", style="background-color: #41ab5d; border-color: #41ab5d"))
#       ),
#       mainPanel(width = 12,
#                 textOutput('media_plot_description'),
#                 plotlyOutput("plot_original_media")
#       )
#     )
#   ),
#   tabPanel("Cumulative Revenue by Group",
#            fluidPage(
#              #Rows can be a TOTAL width of 12 units
#              fluidRow(
#                column(width = 2,
#                       selectInput(inputId = 'group_by_var', label = "Group By Variable", 
#                                   choices = c("Year Created" = "year_created", "Franchise" = "franchise",
#                                               "Creators" = "creators", "Owners" = "owners",
#                                               "Revenue Category" = "revenue_category", "Original Media" = "original_media"),
#                                   selected = unique(media_franchises$original_media)[1], multiple = F))
#                             ),
#              mainPanel(width = 12,
#                        textOutput('grouped_plot_description'),
#                        plotOutput("grouped_by_plot", height = "700px")
#              )
#            )
#   )
# )
#   