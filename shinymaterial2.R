# Wrap shinymaterial apps in material_page
pacman::p_load("shiny","shinymaterial","DT")
source("setup.R")
### UI #########################
ui <- material_page(
  title = "MSU Denver Faculty Equity Survey",
  # Place side-nav in the beginning of the UI
  material_side_nav(
    fixed = F,
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Introduction" = "intro_tab",
        "Demographics" = "diversity_tab",
        "Discrimination on Campus" = "discrim_tab",
        "Campus Climate" = "climate_tab",
        "Retention and Affiliate Benefits" = "affil_tab"
      ),
      icons = c("mood","pie_chart","insert_chart","insert_chart", "insert_chart")
    )
  ),
### Introduction tabb ###########
  material_side_nav_tab_content(
    side_nav_tab_id = "intro_tab",
    material_card(
      includeMarkdown("include.md")
    )
  ),
### Diversity Tab ####################
  material_side_nav_tab_content(
    side_nav_tab_id = "diversity_tab",
    material_tabs(
      tabs = c(
        "Bar Charts" = "bar_tab",
        "Table of counts" = "table_tab"
      )
    ),
    material_tab_content(
      tab_id = "bar_tab",
      tags$h2("Survey Demographics"),
      material_row(
        width = 1.2,
        height = 1,
        material_column(plotlyOutput("college_bar")),
        material_column(plotlyOutput("faculty_bar"))
      ),
      material_row(
        width = 1.2,
        height = 2,
        material_column(plotlyOutput("age_bar")),
        material_column(plotlyOutput("gender_bar"))
      ),
      material_row(
        width=1.2,
        height = 1.2,
        material_column(plotlyOutput("religion_bar")),
        material_column(plotlyOutput("sex_bar"))
      ),
      material_row(
        width=1.2,
        height = 1.2,
        material_column(plotlyOutput("veteran_bar")),
        material_column(plotlyOutput("disability_bar"))
      ),
      material_row(
        width=1.2,
        height = 1.2,
        material_column(plotlyOutput("race_bar"))
      )
    ),
    material_tab_content(
      tab_id = "table_tab",
      tags$h2("Tables of Survey Demographics"),
      material_row(
        width = 1.2,
        height = 1,
        material_column(material_card(title="College",DTOutput("college_table"))),
        material_column(material_card(title="Faculty",DTOutput("faculty_table")))
      ),
      material_row(
        width = 1.2,
        height = 2,
        material_column(material_card(title = "Age",DTOutput("age_table"))),
        material_column(material_card(title="Gender",DTOutput("gender_table")))
      ),
      material_row(
        width=1.2,
        height = 1.2,
        material_column(
          material_card(title="Religion",DTOutput("religion_table"))),
        material_column(
          material_card(title = "Sexual Orientation",DTOutput("sex_table")))
      ),
      material_row(
        width=1.2,
        height = 1.2,
        material_column(
          material_card(
            title = "Veteran",
            DTOutput("veteran_table")
          )
        ),
        material_column(material_card(title="Disabled",DTOutput("disability_table")))
      ),
      material_row(
        width=1.2,
        height = 1.2,
        material_column(material_card(title="Race",DTOutput("race_table")))
      )
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
      material_row(
        plotlyOutput("discrim_plot")
      ),
      material_parallax(image_source = "cat1.gif"),
      material_row(
        material_column(material_card(DTOutput("discrim_table"))),
        material_column(material_card(DTOutput("witness_table")))
      )
    ),
    material_tab_content(
      tab_id = "second_tab",
      tags$h1("Percent Respond Likely to Report"),
      material_row(
        plotlyOutput("report_plot")
      ),
      material_parallax(image_source = "cat2.gif"),
      material_row(
       material_card(DTOutput("report_table"))
      )
    ),
    material_tab_content(
      tab_id = "third_tab",
      tags$h1("Percent Comfortable Disclosing Identity"),
      material_row(
        plotlyOutput("comfort_plot")
      ),
      material_parallax(image_source = "cat3.gif"),
      material_row(
        material_card(DTOutput("comfort_table"))
      )
    )
  ),
### Campus Climate Tab ###################
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
      material_row(plotlyOutput("value_plot")),
      material_parallax(image_source = "cat4.gif"),
      material_row(material_card(DTOutput("value_table")))
    ),
    material_tab_content(
      tab_id = "pay_tab",
      material_row(plotlyOutput("pay_plot")),
      material_parallax(image_source = "cat5.gif"),
      material_row(material_card(DTOutput("pay_table")))
    ),
    material_tab_content(
      tab_id = "group_tab",
      material_row(plotlyOutput("group_plot")),
      material_parallax(image_source = "cat6.gif"),
      material_row(material_card(DTOutput("group_table")))
    )
  ),
### Faculty tab ################
  material_side_nav_tab_content(
    side_nav_tab_id = "affil_tab",
    material_tabs(
      tabs = c(
        "Faculty Retention" = "retention_tab",
        "Affiliate Benefits" = "affil_b_tab"
      )
    ),
    material_tab_content(
      tab_id = "retention_tab",
      material_row(plotlyOutput("retention_plot")),
      material_parallax(image_source = "cat7.gif"),
      material_row(material_card(DTOutput("retention_table")))
    ),
    material_tab_content(
      tab_id = "affil_b_tab",
      material_row(plotlyOutput("affil_plot")),
      material_parallax(image_source = "cat8.gif"),
      material_row(material_card(DTOutput("affil_table")))
    )
  )
)
### Server ########################
server <- function(input, output) {
### Diversity Server ##############
  ## bars ##############
  output$religion_bar <- renderPlotly({
    bar_religion %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  output$race_bar <- renderPlotly({
    bar_race %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  output$age_bar <- renderPlotly({
    bar_age %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  output$gender_bar <- renderPlotly({
    bar_gender %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  output$sex_bar <- renderPlotly({
    bar_sex %>% ggplotly(tooltip="text")  %>% config(displayModeBar=F)
  })
  output$college_bar <- renderPlotly({
    bar_college %>% ggplotly(tooltip="text")  %>% config(displayModeBar=F)
  })
  output$faculty_bar <- renderPlotly({
    bar_faculty %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  output$veteran_bar <- renderPlotly({
    bar_veteran %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  output$disability_bar <- renderPlotly({
    bar_disable %>% ggplotly(tooltip="text") %>% config(displayModeBar=F)
  })
  ## tables ########
  output$religion_table <- renderDT({
    select(df_religion,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$race_table <- renderDT({    
    select(df_race,-plot_text) %>% datatable(options = list(dom = 't'))
    })
  output$age_table <- renderDT({
    select(df_age,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$gender_table <- renderDT({
    select(df_gender,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$sex_table <- renderDT({
    select(df_sex,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$college_table <- renderDT({
   select(df_college,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$faculty_table <- renderDT({
    select(df_faculty,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$veteran_table <- renderDT({
    select(df_veteran,-plot_text) %>% datatable(options = list(dom = 't'))
  })
  output$disability_table <- renderDT({
    select(df_disable,-plot_text) %>% datatable(options = list(dom = 't'))
  })
### Discimination Server ########## 
  ### PLots #################
  output$discrim_plot <- renderPlotly({
    ggplotly(p,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$report_plot <- renderPlotly({
    ggplotly(p1,height = 600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$comfort_plot <- renderPlotly({
    ggplotly(p2,height = 600,tooltip="text") %>% config(displayModeBar=F)
  })
  
  ### Tables ##################
  output$discrim_table <- renderDT(
    discrim_table %>% datatable(options = list(dom='t'))
  )
  output$witness_table <- renderDT(
    witness_table %>% datatable(options = list(dom='t'))
  )
  output$report_table <- renderDT(
    report_table %>% datatable(options = list(dom='t'))
  )
  output$comfort_table <- renderDT(
    comfort_table %>% datatable(options = list(dom='t'))
  )
### Campus Climate Server ###########
  ### plots ############
  output$value_plot <- renderPlotly({
    ggplotly(value_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$pay_plot <- renderPlotly({
    ggplotly(pay_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$group_plot <- renderPlotly({
    ggplotly(group_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  ### tables ###################
  output$value_table <- renderDT({
    value_table %>% datatable(options = list(dom='t'))
  })
  output$pay_table <- renderDT({
    pay_table %>% datatable(options = list(dom='t'))
  })
  output$group_table <- renderDT({
    group_table %>% datatable(options = list(dom='t'))
  })
### Faculty Server ##############
  ### plots ################
  output$retention_plot <- renderPlotly({
    ggplotly(retention_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  output$affil_plot <- renderPlotly({
    ggplotly(affil_plot,height=600,tooltip="text") %>% config(displayModeBar=F)
  })
  ### tables #############
  output$retention_table <- renderDT({
    retention_table %>% datatable(options = list(dom='t'))
  })
  output$affil_table <- renderDT({
    affil_table %>% datatable(options = list(dom='t'))
  })
}
shinyApp(ui = ui, server = server)