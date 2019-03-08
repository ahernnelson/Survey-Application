# Wrap shinymaterial apps in material_page
pacman::p_load("shiny","shinymaterial","ggplot2","plotly","dplyr","scales")
source("setup.R")
### UI #########################
ui <- material_page(
  title = "MSU Denver Faculty Equity Survey",
  # Place side-nav in the beginning of the UI
  material_side_nav(
    fixed = F,
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Diversity" = "diversity_tab",
        "Discrimination" = "discrim_tab",
        "Campus Climate" = "climate_tab"
      ),
      icons = c("pie_chart","insert_chart","insert_chart")
    )
  ),
### Diversity Tab ####################
  material_side_nav_tab_content(
    side_nav_tab_id = "diversity_tab",
    material_tabs(
      tabs = c(
      "Religion" = "religion_tab",
      "Race" = "race_tab",
      "Age" = "age_tab",
      "Gender" = "gender_tab",
      "Sexual Orientation" = "sex_tab",
      "College of" = "college_tab",
      "Faculty Appointment" = "faculty_tab"
      )
    ),
    material_tab_content(
      tab_id = "religion_tab",
      plotlyOutput("religion_plot")
    ),
    material_tab_content(
      tab_id = "race_tab",
      plotlyOutput("race_plot")
    ),
    material_tab_content(
      tab_id = "age_tab",
      plotlyOutput("age_plot")
    ),
    material_tab_content(
      tab_id = "gender_tab",
      plotlyOutput("gender_plot")
    ),
    material_tab_content(
      tab_id = "sex_tab",
      plotlyOutput("sex_plot")
    ),
    material_tab_content(
      tab_id = "college_tab",
      plotlyOutput("college_plot")
    ),
    material_tab_content(
      tab_id = "faculty_tab",
      plotlyOutput("faculty_plot")
    )
  ),
### Discrimination Tab ###############
  material_side_nav_tab_content(
    side_nav_tab_id = "discrim_tab",
    material_tabs(
      tabs = c(
        "Types of Discrimination" = "first_tab",
        "Reporting Discrimination" = "second_tab",
        "Comfort Sharing Identity" = "third_tab"
      )
    ),
    # Define tab content
    material_tab_content(
      tab_id = "first_tab",
      tags$h1("Discrimination on Campus"),
      plotlyOutput("discrim_plot")
  
    ),
    material_tab_content(
      tab_id = "second_tab",
      tags$h1("Percent Respond Likely to Report"),
      plotlyOutput("report_plot")
    ),
    material_tab_content(
      tab_id = "third_tab",
      tags$h1("Percent Comfortable Disclosing Identity"),
      plotlyOutput("comfort_plot")
    )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "climate_tab",
    material_tabs(
      tabs = c(
        "Value On Campus" = "value_tab",
        "Fair Compensation" = "pay_tab",
        "Effectivenes of Groups" = "group_tab"
      )
    ),
    material_tab_content(
      tab_id = "value_tab",
      plotlyOutput("value_plot")
    ),
    material_tab_content(
      tab_id = "pay_tab",
      plotlyOutput("pay_plot")
    ),
    material_tab_content(
      tab_id = "group_tab",
      plotlyOutput("group_plot")
    )
  )
)
### Server ########################
server <- function(input, output) {
### Diversity Server ##############
  output$religion_plot <- renderPlotly({
    py %>% config(displayModeBar=F)
  })
  output$race_plot <- renderPlotly({
    py_race %>% config(displayModeBar=F)
  })
  output$age_plot <- renderPlotly({
    py_age %>% config(displayModeBar=F)
  })
  output$gender_plot <- renderPlotly({
    py_gender %>% config(displayModeBar=F)
  })
  output$sex_plot <- renderPlotly({
    py_sex %>% config(displayModeBar=F)
  })
  output$college_plot <- renderPlotly({
    py_college %>% config(displayModeBar=F)
  })
  output$faculty_plot <- renderPlotly({
    py_faculty %>% config(displayModeBar=F)
  })
### Discimination Server ########## 
  output$discrim_plot <- renderPlotly({
    ggplotly(p,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$report_plot <- renderPlotly({
    ggplotly(p1,height = 600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$comfort_plot <- renderPlotly({
    ggplotly(p2,height = 600,tooltip="text") %>% config(displayModeBar=F)
  })

### Campus Climate Server ###########
  output$value_plot <- renderPlotly({
    ggplotly(value_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$pay_plot <- renderPlotly({
    ggplotly(pay_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$group_plot <- renderPlotly({
    ggplotly(group_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
}
shinyApp(ui = ui, server = server)