library(shiny)
pacman::p_load("ggplot2","dplyr","plotly","ggsci","shinythemes","scales")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

my_data_raw <- read.csv("FES_2019_responses_collapsed_forApp.csv", row.names = 1, header = TRUE)
my_data_raw %>% names() %>% smart_name() -> niceNames
colnames(my_data_raw) <- niceNames
# UI
ui <- fluidPage(theme = shinytheme("journal"),
  
  # App title 
  titlePanel("Faculty Equity Survey"),
  
  # Sidebar layout with input and output definitions
  
  sidebarLayout(
    
    # Input: Selector for choosing the dataset
    sidebarPanel(
      
      # DropSelect a school 
      selectInput(inputId = "dataset",
                  label = "Choose a category:",
                  choices = c("Affiliate" = "affiliate", 
                              "Fulltime" = "fulltime",
                              "All" = "all"),
                  selected = "my_data_affiliate"),
      
      uiOutput("question_one_selector"),
      
      uiOutput("question_two_selector")
      
    ),
    
    # Output:
    mainPanel(
      
      # Output: HTML table
      #verbatimTextOutput("data"), ## changed by Dr. Ribble
      
      tags$style(HTML('#data {
                        background: whitesmoke; 
                      color: black; 
                      font-family: Arial, Helvetica, sans-serif;
                      font-size: 16px
                      }
                      ')),
      
      #plotOutput("plot") ## added by James
      
      tabsetPanel(type = "tabs",
                  tabPanel("Table", verbatimTextOutput("data")),
                  tabPanel("Plot", plotlyOutput("plot")),
                  tabPanel("Test Results"))
      
    )
  )
)

# Define server logic to view selected dataset
server <- function(input, output) {
  
  dataset_input <- reactive({
    
    if(input$dataset == "affiliate") {
      
      my_data_affiliate_raw <- my_data_raw[my_data_raw$Category == input$dataset, ]
      
      my_data_affiliate <- subset(my_data_affiliate_raw, select = -c(1, 17:18, 20:21, 30:31, 48:54, 89:91, 114:115, 123))
      
    } else if(input$dataset == "fulltime") {
      
      my_data_fulltime_raw <- my_data_raw[my_data_raw$Category == input$dataset, ]
      
      my_data_fulltime <- subset(my_data_fulltime_raw, select = -c(1, 28, 33:47, 88, 97, 99:113))
      
    } else if(input$dataset == "all") {
      
      my_data_all <- my_data_raw
      
    }
    
  })
  
  output$question_one_selector <- renderUI({
    
    # DropSelect the first question
    selectInput(inputId = "question_one",
                label = "Choose the Question for Rows:", ## changed by Dr. Ribble
                choices = names(dataset_input())[names(dataset_input()) %in% names(my_data_raw)[c(87:91, 97:121)]]) ## changed by Dr. Ribble
  }) 
  
  output$question_two_selector <- renderUI({
    
    # DropSelect the second question
    selectInput(inputId = "question_two",
                label = "Choose the Question for Columns:", ## changed by Dr. Ribble
                choices = names(dataset_input())[names(dataset_input()) %in% names(my_data_raw)[-c(87:91, 97:121)]]) ## changed by Dr. Ribble
  }) 
  
  output$data <- renderPrint({ ## changed by Dr. Ribble
    
    with(dataset_input(), table(get(req(input$question_one)), get(input$question_two)))   ## changed by James

    
  })
  
  output$plot <- renderPlotly({ ## Added by James

    my_table_int <- with(dataset_input(), table(get(req(input$question_two)), get(req(input$question_one))))

    my_table_pct <- scale(my_table_int, FALSE, colSums(my_table_int)) * 100

    par(mar = c(6, 12, 6, 2))
    
    if(nrow(my_table_pct) == 2) {
      data.frame(my_table_pct/100) %>% 
        rename(Response=Var1, q1=Var2 ,perc=Freq) %>%
        ggplot() + geom_col(aes(q1,perc,fill=Response)) + coord_flip() +
        scale_y_continuous(labels=percent, "Percent")+ theme_bw() + xlab("") +
        scale_fill_d3("category10") -> plot1 
      ggplotly(plot1)
    } else {
      data.frame(my_table_pct/100) %>% 
        ggplot() + geom_col(aes(Var2,Freq,fill=Var1)) + coord_flip() +
        scale_y_continuous(labels=percent, "Percent") + 
        facet_wrap(facets=~Var1)+ theme_bw() + xlab("") +
        scale_fill_d3("category10") -> p 
      ggplotly(p)
    }

  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

