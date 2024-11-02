#-------------------------------------------------------------------------------
# Shiny App for Budget Visaulatation Tool for Nigeria's 2024 NSP update 
#  App displays: 
#               - cost input data used for analysis (unit cost - no line list)
#               - National budget summary  
#               - State budget summary 
#               - LGA budget summary 
#               - Cost alteration tool - pre-programmed 
#               - Cost alteration tool - manually programmed (on hold)
#               - Text summary of methods and calculations
#-------------------------------------------------------------------------------              

# load packages-----------------------------------------------------------------
source("global.R")  # loads data
source("helpers.R") # functions for server outputs

#-SHINY UI----------------------------------------------------------------------
ui <- navbarPage(
    
    # SETTNG SOME STYLING 
  tags$head(
    tags$style(HTML("
      .info-box { font-size: 16px; text-align: left; }
      .info-icon { font-size: 50px; display: block; margin: 0 auto; text-align: left; }
      .info-container { color: grey50; font-weight: bold;font-size: 1.2em; }
      .info-section { display: flex; justify-content: space-between; width: 100%; padding: 20px; margin-top: 20px; }
      .final-info-container {  color: green; font-weight: bold;font-size: 1.2em;  }
      .chart-container { width: 33.33%; padding: 15px; box-sizing: border-box; margin-bottom: 20px; }
      .row-container { display: flex; flex-wrap: wrap; justify-content: space-evenly; }
      .navbar { background-color: #008000; }
      .bb-title { margin-bottom: 5px !important; }
     .container-fluid { width: 100% !important; max-width: none !important; }
     .navbar-nav > li > a {
        padding-top: 20px !important;
        padding-bottom: 20px !important;
        font-size: 18px;
      }
      .navbar-brand {
        padding-top: 20px !important;
        padding-bottom: 20px !important;
        font-size: 22px !important;
        height: auto;
      }
      .navbar {
        min-height: 70px !important;
      }
    "))
  ),
    
    title = div("Nigeria Budget Allocation Visualisation Tool"), # Main title
    theme = shinytheme("yeti"),
    h4("TOOL IS IN ACTIVE DEVELOPMENT AND BEING UPDATED CONTINUALLY - DATA SUBJECT TO CHANGE"),
    # Define custom CSS to change colour of navbar
    header = tags$style(HTML("
    .navbar {
      background-color: #008000;
    }
  ")),
  
  # LANDING PAGE IS NATIONAL BUDGET SUMMARY 
  tabPanel("National Summary of Planned Interventions and Costs", 
           h4("This page provides a summary of the costs associated with the 2025 prioritised intervention package at the National level."),
           fluidRow(
             column(
               width = 12,
               # Wrapper div with grey border and padding
               div(
                 style = "border: 1px solid grey; padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
                 radioButtons(
                   "currency_option", 
                   "Currency Selection:", 
                   choices = c("USD", "Naira"), 
                   selected = "USD",
                   inline = TRUE, # Makes the radio buttons display horizontally
                   width = "100%"
                 ) %>% tagAppendAttributes(style = "font-size: 20px;")
               )
             )
           ),
           hr(), # A horizontal line separator
             mainPanel(
                     width = 12,
                     fluidRow(
                               column(4, leafletOutput("map1", height = "550px")),
                               column(4, leafletOutput("map_interactive", height = "550px")),
                               column(4, leafletOutput("map2", height = "550px"))
                           ),
                     uiOutput("info_section"), 
                     hr(),
                     fluidRow(
                       column(8, # Left column (half width)
                              uiOutput("table_title"),
                              DTOutput("budget_table"),
                              uiOutput("summary_text")
                       ),
                       column(4, # Right column (half width)
                              uiOutput("donut_title"),
                              billboarderOutput("donut_chart")
                       )
                     ),
                     hr(),  
                     fluidRow(
                       column(12, h3("Cost Analysis Visualizations")),
                       column(6, plotlyOutput("treemap_plot", height = "600px")),
                       column(6, plotlyOutput("stacked_bar_plot", height = "600px"))
                     ),
                     fluidRow(
                       column(6, plotlyOutput("lollipop_plot", height = "600px")),
                       column(6, plotlyOutput("prop_plot", height = "600px"))
                     ), 
                     hr(), 
                     fluidRow(
                       column(12, h3("Cost Distribution at the State Level")),
                       column(6, leafletOutput("total_cost_map", height = "600px")),
                       column(6, leafletOutput("cost_per_person_map", height = "600px"))
                     ),
                     hr()
                   
                   )
               ), 
  
  # SECOND TAB IS STATE LEVEL SUMMARY  
  tabPanel("State Summary of Planned Interventions and Costs", 
           h4("This page provides a summary of the costs associated with the 2025 prioritised intervention package at the State level."), 
           sidebarLayout(
             sidebarPanel(
               width = 12,  # Adjusted width to accommodate a horizontal layout if needed
               # Wrapper div with grey border and padding for the entire sidebar
               div(
                 style = "border: 1px solid grey; padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
                 selectizeInput(
                   "state", 
                   "Select State:", 
                   choices = c("", unique(state_outline$state)),
                   options = list(placeholder = "Select or type a state")
                 ),
                 br(),  # Line break for spacing between inputs
                 radioButtons(
                   "currency_option_state", 
                   "Currency Selection:", 
                   choices = c("USD", "Naira"), 
                   selected = "USD"
                 ),
                 br(),  # Line break for spacing between inputs
                 actionButton("clear_state_selection", "Clear Selection")
               )
             ),
             mainPanel(
               width = 12,
               fluidRow(
                 column(4, leafletOutput("state_map1", height = "550px")),
                 column(4, leafletOutput("state_map_interactive", height = "550px")),
                 column(4, leafletOutput("state_map2", height = "550px"))
               ),
               hr(),
               uiOutput("state_info_section"),   # Add the info section below the maps
               hr(),
               fluidRow(
                 column(8, # Left column (half width)
                        uiOutput("state_table_title"),
                        DTOutput("state_budget_table"),
                        uiOutput("state_summary_text")
                 ),
                 column(4, # Right column (half width)
                        uiOutput("state_donut_title"),
                        billboarderOutput("state_donut_chart")
                 ), 
               ), 
               hr(), 
               fluidRow(
                   column(12, uiOutput("state_cost_analysis_title")),
                   column(6, plotlyOutput("state_treemap_plot", height = "600px")),
                   column(6, plotlyOutput("state_stacked_bar_plot", height = "600px"))
                ),
                fluidRow(
                   column(6, plotlyOutput("state_lollipop_plot", height = "600px")),
                   column(6, plotlyOutput("state_prop_plot", height = "600px"))
                 ), 
                 hr(),
                 fluidRow(
                   column(12, h3("Cost Distribution at the State Level")),
                   column(6, leafletOutput("state_total_cost_map", height = "600px")),
                   column(6, leafletOutput("state_cost_per_person_map", height = "600px"))
                 ), 
               hr(),  
               h3("Cost Distribution at the LGA Level"),
               fluidRow(
                 column(6, leafletOutput("lga_total_cost_map", height = "600px")),
                 column(6, leafletOutput("lga_cost_per_person_map", height = "600px"))
               )
               )
             )
           ),

 
          
  # THIRD TAB IS LGA LEVEL SUMMARY 
  tabPanel("LGA Summary of Planned Interventions and Costs", 
           h4("This page provides a summary of the costs associated with the 2025 prioritised intervention package at the LGA level."),
           sidebarLayout(
             sidebarPanel(
               width = 12,
               div(
                 style = "border: 1px solid grey; padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
               # Spatial level selection (fixed to LGA for now)
               selectInput("level", "Select Spatial Level:", choices = c("LGA"), selected = "LGA"),
               
               # State selection dropdown
               selectInput("state_lga", "Select State:", choices = c("", unique(state_outline$state))),
               
               # LGA selection (rendered dynamically)
               uiOutput("lga"),  # Dynamic LGA selection dropdown
                # Currency selection radio buttons
               radioButtons("currency_option_lga", "Currency Selection:", 
                            choices = c("USD", "Naira"), selected = "USD"),
               
               # Clear Selection button
               actionButton("clear_selection", "Clear Selection")
               )
             ),
           mainPanel(
             width = 12,
             fluidRow(
               column(4, leafletOutput("lga_map1", height = "550px")),
               column(4, leafletOutput("lga_map_interactive", height = "550px")),
               column(4, leafletOutput("lga_map2", height = "550px"))
             ),
             hr(),
             uiOutput("lga_info_section"),  # LGA info section
             hr(),
             fluidRow(
               column(8, uiOutput("lga_table_title"), DTOutput("lga_budget_table")),
               column(4, uiOutput("lga_donut_title"), billboarderOutput("lga_donut_chart"))
             ),
             hr(),
             fluidRow(
               column(12, uiOutput("lga_cost_analysis_title")),
               column(6, plotlyOutput("lga_treemap_plot", height = "600px")),
               column(6, plotlyOutput("lga_stacked_bar_plot", height = "600px"))
             ),
             fluidRow(
               column(6, plotlyOutput("lga_lollipop_plot", height = "600px")),
               column(6, plotlyOutput("lga_prop_plot", height = "600px"))
             ),
             hr(),
             hr(),  
             h3("Cost Distribution at the LGA Level"),
             fluidRow(
               column(6, leafletOutput("lga_total_cost_map_lga", height = "600px")),
               column(6, leafletOutput("lga_cost_per_person_map_lga", height = "600px"))
             )
           )
  )
  ), 
  
  
  # FOURTH TAB IS BUDGET COMPARISONS 
  tabPanel("Plan Comparison",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 width=12,
                 h4("Select plans to compare with Baseline Costed Operational Plan"),
                 checkboxGroupInput(
                   inputId = "selected_plans",
                   label = NULL,
                   choices = setNames(unique_plans, plan_labels)
                 ),
                 radioButtons(
                   inputId = "currency_option_plan",
                   label = "Select Currency",
                   choices = c("USD", "Naira"),
                   selected = "USD"  # Default to USD
                 )
               ),
               mainPanel(
                 width=12,
                 fluidRow(
                   # Baseline Map
                   column(4, h4("Baseline Costed Operational Plan"), leafletOutput("baseline_map")),
                   
                   # Dynamic Maps for Selected Plans
                   uiOutput("selected_plans_maps")
                 ), 
                 # Cost Comparison Visualizations
                 h4("Cost Comparison Visualisations"), 
                 fluidRow(
                   column(6, plotlyOutput("cost_comparison_plot", height = "700px")),  # Adjust height here
                   column(6, plotlyOutput("cost_difference_plot", height = "700px"))  # Adjust height here
                 ), 
                 # Summary Data Tables
                 h4("Cost Summary Tables"), 
                 uiOutput("summary_data_tables")  # Add this output for the tables
               )
             )
           )
        ),
  

  
  # SIXTH TAB IS METHODOLOGY 
  tabPanel("Methodology", 
           h4(" "))
)

#-SHINY SERVER-------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
    
    # # color palette for the text values in 'intervention_mix'
    # color_pal <- reactive({
    #     colorFactor(
    #         palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Paired"))(10),
    #         domain = intervention_mix$intervention_summary
    #     )
    # })
    
    # Color palette for under-5 prevalence
    color_pal_prevalence <- reactive({
      colorNumeric(viridis(50, direction=-1), domain = prevalence_data$prev_u5_state)
    })
    
    #-NATIONAL SUMMARY TAB-------------------------------------------------------------------------------
    
    
    #-INTERVENTION MIX MAP------------------------------------
    output$map1 <- renderLeaflet({
      create_intervention_map_static(
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        intervention_mix = intervention_mix
      )
    })
    
    #-INTERACTIVE INTERVENTION MAP------------------------------
    # Create initial map
    output$map_interactive <- renderLeaflet({
      create_base_interactive_map(
        lga_outline = lga_outline,
        intervention_mix = intervention_mix
      ) %>%
        htmlwidgets::onRender(create_legend_js(intervention_mix = intervention_mix))
    })
    
    # Observer for legend selections
    observeEvent(input$selected_interventions, {
      selected_interventions <- input$selected_interventions
      
      # Join and prepare the interactive map data
      interactive_map <- left_join(lga_outline, intervention_mix) %>%
        mutate(unique_interventions = intervention_summary) %>%
        separate_rows(unique_interventions, sep = "\\+ ") %>%
        mutate(unique_interventions = trimws(unique_interventions))
      
      # Filter polygons based on the selected interventions
      highlighted_lgas <- interactive_map %>%
        filter(unique_interventions %in% selected_interventions)  # Use %in% for multiple selections
      
      # Update the map
      update_intervention_map(
        map_id = "map_interactive",
        highlighted_lgas = highlighted_lgas,
        state_outline = state_outline,
        country_outline = country_outline
      )
    })
    
    #-PREVALENCE MAP--------------------------------------------
    output$map2 <- renderLeaflet({
      create_prevalence_map(
        prevalence_data = prevalence_data,
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        color_pal_prevalence = color_pal_prevalence
      )
    })
    
    #-ICON SUMMARIES---------------------------------------------------
    output$info_section <- renderUI({
      create_icon_summaries(
        data = national_ribbon_data,
        currency_choice = input$currency_option
      )
    })
    
    #-TOTAL COST SUMMARY TABLE-----------------------------------------
    output$table_title <- renderUI({
      h3(paste("Intervention and Total Cost Breakdown"),
         style = "text-align: left; margin-top: 20px;")
      } 
    )
    
    observe({
      print(names(national_total_cost_summary))
    })
    
    # Reactive function to reduce data based on selected currency
    filtered_cost_data <- reactive({
      req(national_total_cost_summary)  # Ensure the dataset is loaded
      
      # Filter based on currency selection
      currency_display <- input$currency_option
      
      # Prepare formatted cost column based on selected currency
      national_total_cost_summary |> 
        filter(currency == currency_display) |> 
        filter(intervention != "IRS") |> 
        filter(total_cost != 0) |> 
        select(intervention_type, title, state_count, lga_count, target, target_value, total_cost, cost_per_target)  |> 
        arrange((intervention_type))
    
      })
     # Render the DataTable
    output$budget_table <- renderDT({
      datatable(
        filtered_cost_data(),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        colnames = c(
          "Category" = "intervention_type",
          "Item" = "title",
          "States Targeted" = "state_count",
          "LGAs Targeted" = "lga_count",
          "Target Denominator" = "target",
          "Target Population or Area" = "target_value",
          "Total Cost" = "total_cost",
          "Cost per Target" = "cost_per_target"
        )) %>%
        formatStyle(
          columns=1:8,
          fontSize = '14px'
         ) %>%
        formatCurrency(
          columns = "Total Cost",
          currency = if (input$currency_option == "USD") "$" else "₦",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>%
        formatCurrency(
          columns = "Cost per Target",
          currency = if (input$currency_option == "USD") "$" else "₦",
          interval = 3,
          mark = ",",
          digits = 2
        ) %>%
        formatCurrency(
          columns = "Target Population or Area",
          currency = "",  # No currency symbol
          interval = 3,   # Add comma separator
          mark = ",", 
          digits = 0      # No decimals
        ) 
    })
    
    # Text undeneath
    output$summary_text <- renderUI({
      HTML("<br>For more information on the costing assumptions see the methodology tab<br>")
    })
    
    
    #-ROPORTIONAL COST BREAKDOWN PLOTS------------------------------------------
    
    # Donut chart title 
    output$donut_title <- renderUI({
      h3(paste("Proportional Cost Breakdown"),
         style = "text-align: left; margin-top: 20px; margin-bottom: 50px;")
    } 
    )
    # Render the Main Donut Chart
    output$donut_chart <- renderBillboarder({
      create_cost_donut(
        data = national_total_cost_summary,
        currency_choice = input$currency_option
      )
    })
    
    #-COST BREAKDOWN PLOTS------------------------------------------------------
    output$treemap_plot <- renderPlotly({
      create_treemap_plot(national_intervention_chart_data, input$currency_option)
    })
    
    output$stacked_bar_plot <- renderPlotly({
      create_stacked_bar_plot(national_intervention_chart_data, input$currency_option)
    })
    
    output$lollipop_plot <- renderPlotly({
      create_lollipop_plot(national_intervention_chart_data, input$currency_option)
    })
    
    output$prop_plot <- renderPlotly({
      create_prop_plot(national_intervention_chart_data, input$currency_option)
    })
    
    #-STATE LEVEL COST MAP------------------------------------------------------
    # Reactive dataset filtered by currency selection
    state_filtered_data <- reactive({
      
      state_outline |> 
        left_join(state_data_total_cost) %>%
        filter(currency == input$currency_option) 
      
    })
    
    # Total cost map
    output$total_cost_map <- renderLeaflet({
      map_data_sf <- state_filtered_data()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "total",
        currency_option = input$currency_option
      )
    })
    
    # Cost per person map
    output$cost_per_person_map <- renderLeaflet({
      map_data_sf <- state_filtered_data()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "per_person",
        currency_option = input$currency_option
      )
    })

    
    
    
    
    
    #-STATE SUMMARY PAGE--------------------------------------------------------------------------------------
    
    # Reactive: Store selected state polygon
    selected_state_outline <- reactive({
      req(input$state)  # Only proceed if a state is selected
      filter(state_outline, state == input$state)
    })
    

    # Render intervention mix map on page load
    output$state_map1 <- renderLeaflet({
      create_intervention_map_static(
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        intervention_mix = intervention_mix
      )
    })
    
    # Render prevalence map on page load
    output$state_map2 <- renderLeaflet({
      create_prevalence_map(
        prevalence_data = prevalence_data,
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        color_pal_prevalence = color_pal_prevalence
      )
    })
    
    # Render interactive map on page load
    output$state_map_interactive <- renderLeaflet({
      create_base_interactive_map(
        lga_outline = lga_outline,
        intervention_mix = intervention_mix
      ) %>%
        htmlwidgets::onRender(create_legend_js(intervention_mix = intervention_mix))
    })
    
    # Highlight selected state across all maps
    observeEvent(input$state, {
      req(selected_state_outline())  # Ensure a valid state selection
      
      # Apply highlighting to all maps
      highlight_state("state_map1", selected_state_outline())
      highlight_state("state_map2", selected_state_outline())
      highlight_state("state_map_interactive", selected_state_outline())
      highlight_state("state_total_cost_map", selected_state_outline())
      highlight_state("state_cost_per_person_map", selected_state_outline())
      highlight_state("lga_total_cost_map", selected_state_outline())
      highlight_state("lga_cost_per_person_map", selected_state_outline())
    })
    
    # Observer for legend clicks on the interactive map
    observeEvent(input$selected_intervention, {
      selected_intervention <- input$selected_intervention
      
      # Filter LGA polygons for the selected intervention
      interactive_map <- 
        left_join(lga_outline, intervention_mix) |> 
        mutate(unique_interventions = intervention_summary) |>
        separate_rows(unique_interventions, sep = "\\+ ") |>
        mutate(unique_interventions = trimws(unique_interventions))
      
      highlighted_lgas <- interactive_map %>%
        filter(unique_interventions == selected_intervention)
      
      # Update the interactive map with the intervention polygons
      update_intervention_map(
        map_id = "state_map_interactive",
        highlighted_lgas = highlighted_lgas,
        state_outline = state_outline,
        country_outline = country_outline
      )
      
      # Reapply state highlight if a state is selected
      if (!is.null(input$state) && input$state != "") {
        highlight_state("state_map_interactive", selected_state_outline())
      }
    })
    
    # Reactive: Store the selected state data
    selected_state_data <- reactive({
      req(input$state)  # Ensure a state is selected
      filter(state_ribbon_data, state == input$state)
    })
    
    # Render the info section for the state tab
    output$state_info_section <- renderUI({
      req(selected_state_data())  # Ensure valid data exists
      create_icon_summaries(
        data = selected_state_data(),
        currency_choice = input$currency_option_state
      )
    })
    
    # Reactive: Filter the state data based on selection and currency
    filtered_state_total_data <- reactive({
      req(input$state, input$currency_option_state)  # Ensure both inputs are available
      state_total_cost_summary %>%
        filter(state == input$state, currency == input$currency_option_state) |> 
        filter(total_cost != 0) |> 
        select(intervention_type, title, lga_count, target, target_value, total_cost, cost_per_target)  |> 
        arrange((intervention_type))
    })
    
    # Reactive: Title for Total Cost Breakdown
    output$state_table_title <- renderUI({
      req(input$state)  # Ensure a state is selected
      h3(paste("Intervention and Total Cost Breakdown for", input$state, " State"),
         style = "text-align: left; margin-top: 20px;")
    })
    
    # Render Total Cost Breakdown Table
    output$state_budget_table <- renderDT({
      datatable(
        filtered_state_total_data(),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        colnames = c(
          "Category" = "intervention_type",
          "Item" = "title",
          "LGAs Targeted" = "lga_count",
          "Target Denominator" = "target",
          "Target Population or Area" = "target_value",
          "Total Cost" = "total_cost",
          "Cost per Target" = "cost_per_target"
        )) %>%
        formatStyle(
          columns=1:7,
          fontSize = '14px'
        ) %>%
        formatCurrency(
          columns = "Total Cost",
          currency = if (input$currency_option == "USD") "$" else "₦",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>%
        formatCurrency(
          columns = "Cost per Target",
          currency = if (input$currency_option == "USD") "$" else "₦",
          interval = 3,
          mark = ",",
          digits = 2
        ) %>%
        formatCurrency(
          columns = "Target Population or Area",
          currency = "",  # No currency symbol
          interval = 3,   # Add comma separator
          mark = ",", 
          digits = 0      # No decimals
        ) 
    })
    
    # Reactive: Title for Proportional Cost Breakdown
    output$state_donut_title <- renderUI({
      req(input$state)  # Ensure a state is selected
      h3(paste("Proportional Cost Breakdown for", input$state, "State"),
         style = "text-align: left; margin-top: 20px;")
    })
    
    # Render Proportional Cost Breakdown Donut Chart
    output$state_donut_chart <- renderBillboarder({
      create_cost_donut(
        data = filtered_state_total_data() |> mutate(currency = input$currency_option_state),
        currency_choice = input$currency_option_state
      )
    })
    
    # Render summary text below the table
    output$state_summary_text <- renderUI({
      HTML("<br>For more information on the costing assumptions, see the Methodology tab.<br>")
    })
    
    # Reactive: Title for the Cost Analysis Visualizations Section
    output$state_cost_analysis_title <- renderUI({
      req(input$state)  # Ensure a state is selected
      
      h3(paste("Cost Analysis Visualizations for", input$state, " State"),
         style = "text-align: left; margin-top: 20px;")
    })
    
    # Reactive: Filter the state-level data by selected state and currency
    filtered_state_chart_data <- reactive({
      req(input$state, input$currency_option_state)  # Ensure both inputs are available
      
      state_intervention_chart_data %>%
        filter(state == input$state, currency == input$currency_option_state)
    })
    
    # Render Treemap Plot for the Selected State
    output$state_treemap_plot <- renderPlotly({
      req(filtered_state_chart_data())  # Ensure valid data
      create_treemap_plot(filtered_state_chart_data(), input$currency_option_state)
    })
    
    # Render Stacked Bar Plot for the Selected State
    output$state_stacked_bar_plot <- renderPlotly({
      req(filtered_state_chart_data())  # Ensure valid data
      create_stacked_bar_plot(filtered_state_chart_data(), input$currency_option_state)
    })
    
    # Render Lollipop Plot for the Selected State
    output$state_lollipop_plot <- renderPlotly({
      req(filtered_state_chart_data())  # Ensure valid data
      create_lollipop_plot(filtered_state_chart_data(), input$currency_option_state)
    })
    
    # Render Proportion Plot for the Selected State
    output$state_prop_plot <- renderPlotly({
      req(filtered_state_chart_data())  # Ensure valid data
      create_prop_plot(filtered_state_chart_data(), input$currency_option_state)
    })
    
    # Reactive: Filter state-level data for the two final maps
    state_filtered_data_state <- reactive({
      req(input$state, input$currency_option_state)
      state_outline |> 
        left_join(state_data_total_cost) %>%
        filter(currency == input$currency_option_state)
    })
    
    # Total Cost Map
    output$state_total_cost_map <- renderLeaflet({
      map_data_sf <- state_filtered_data_state()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "total",
        currency_option = input$currency_option_state
      )
    })
    
    # Cost per Person Map
    output$state_cost_per_person_map <- renderLeaflet({
      map_data_sf <- state_filtered_data_state()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "per_person",
        currency_option = input$currency_option_state
      )
    })
    
    # Reactive: Filter LGA data by selected state and currency
    lga_filtered_data <- reactive({
      lga_outline |> 
        left_join(lga_data_total_cost) %>%
        filter(currency == input$currency_option_state)
    })
    
    # Total Cost Map
    output$lga_total_cost_map <- renderLeaflet({
      map_data_sf <- lga_filtered_data()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "total",
        currency_option = input$currency_option_state
      )
    })
    
    # Cost per Person Map
    output$lga_cost_per_person_map <- renderLeaflet({
      map_data_sf <- lga_filtered_data()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "per_person",
        currency_option = input$currency_option_state
      )
    })
    
    # Clear state selection and reset everything
    observeEvent(input$clear_state_selection, {
      # Reset state dropdown
      updateSelectizeInput(session, "state", selected = "")
      
      # Clear highlights from all maps
      leafletProxy("state_map1") %>% clearGroup("highlight")
      leafletProxy("state_map2") %>% clearGroup("highlight")
      leafletProxy("state_map_interactive") %>% clearGroup("highlight")
      leafletProxy("state_total_cost_map") %>% clearGroup("highlight")
      leafletProxy("state_cost_per_person_map") %>% clearGroup("highlight")
      leafletProxy("lga_total_cost_map") %>% clearGroup("highlight")
      leafletProxy("lga_cost_per_person_map") %>% clearGroup("highlight")
      
      # Clear other outputs
      output$state_info_section <- renderUI({ NULL })
      output$state_table_title <- renderUI({ NULL })
      output$state_budget_table <- renderDT({ NULL })
      output$state_donut_title <- renderUI({ NULL })
      output$state_donut_chart <- renderBillboarder({ NULL })
      output$summary_text <- renderUI({ NULL })
      output$state_cost_analysis_title <- renderUI({ NULL })
      
      # Clear cost visualization plots
      output$state_treemap_plot <- renderPlotly({ NULL })
      output$state_stacked_bar_plot <- renderPlotly({ NULL })
      output$state_lollipop_plot <- renderPlotly({ NULL })
      output$state_prop_plot <- renderPlotly({ NULL })
    })
    
    
    
    
    
    #-LGA SUMMARY TAB-----------------------------------------------------------------------------
    
    
    # Render dynamic LGA selection dropdown
    output$lga <- renderUI({
      selectizeInput("unit", "Select LGA:",
                     choices = if (is.null(input$state_lga) || input$state_lga == "") {
                       c("", unique(lga_outline$lga))  # All LGAs if no state selected
                     } else {
                       c("", unique(lga_outline$lga[lga_outline$state == input$state_lga]))  # Filter by state
                     },
                     selected = "",
                     options = list(placeholder = "Type or select an LGA")
      )
    })
    
    # Observe state selection to update the LGA dropdown dynamically
    observeEvent(input$state_lga, {
      updateSelectizeInput(session, "unit", choices = {
        if (input$state_lga == "") {
          c("", unique(lga_outline$lga))  # All LGAs if no state is selected
        } else {
          c("", unique(lga_outline$lga[lga_outline$state == input$state_lga]))  # Filter by state
        }
      })
    })
    
    
    # Render initial intervention mix map
    output$lga_map1 <- renderLeaflet({
      create_intervention_map_static(
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        intervention_mix = intervention_mix
      )
    })
    
    # Render interactive intervention map
    output$lga_map_interactive <- renderLeaflet({
      create_base_interactive_map(
        lga_outline = lga_outline, 
        intervention_mix = intervention_mix
      ) %>%
        htmlwidgets::onRender(create_legend_js(intervention_mix = intervention_mix))
    })
    
    # Render prevalence map
    output$lga_map2 <- renderLeaflet({
      create_prevalence_map(
        prevalence_data = prevalence_data,
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        color_pal_prevalence = color_pal_prevalence
      )
    })
    
    # Reactive: Store selected LGA polygon based on LGA dropdown
    selected_lga_outline <- reactive({
      req(input$unit)  # Ensure an LGA is selected
      filter(lga_outline, state == input$state_lga,  lga == input$unit)
    })
    
    # Highlight the selected LGA across all three maps
    observeEvent(input$unit, {
      req(selected_lga_outline())  # Ensure a valid LGA selection
      
      # Apply highlighting to all maps
      highlight_lga("lga_map1", selected_lga_outline())
      highlight_lga("lga_map2", selected_lga_outline())
      highlight_lga("lga_map_interactive", selected_lga_outline())
      highlight_lga("lga_total_cost_map_lga", selected_lga_outline())
      highlight_lga("lga_cost_per_person_map_lga", selected_lga_outline())
    })
    
    # Observer for legend clicks on the interactive map
    observeEvent(input$selected_intervention, {
      selected_intervention <- input$selected_intervention
      
      # Filter LGA polygons for the selected intervention
      interactive_map <- 
        left_join(lga_outline, intervention_mix) |> 
        mutate(unique_interventions = intervention_summary) |>
        separate_rows(unique_interventions, sep = "\\+ ") |>
        mutate(unique_interventions = trimws(unique_interventions))
      
      highlighted_lgas <- interactive_map %>%
        filter(unique_interventions == selected_intervention)
      
      # Update the interactive map with the intervention polygons
      update_intervention_map(
        map_id = "lga_map_interactive",
        highlighted_lgas = highlighted_lgas,
        state_outline = state_outline,
        country_outline = country_outline
      )
      
      # Reapply lga highlight if a lga is selected
      if (!is.null(input$unit) && input$unit != "") {
        highlight_lga("lga_map_interactive", selected_lga_outline())
      }
    })
    
    # Reactive: Store the selected lga data
    selected_lga_data <- reactive({
      req(input$unit)  # Ensure a lga is selected
      filter(lga_ribbon_data, state == input$state_lga, lga == input$unit)
    })
    
    # Render the info section for the lga tab
    output$lga_info_section <- renderUI({
      req(selected_lga_data())  # Ensure valid data exists
      create_icon_summaries(
        data = selected_lga_data(),
        currency_choice = input$currency_option_lga
      )
    })
    
    
    # Title values for sections  
    output$lga_table_title <- renderUI({
      req(input$unit)  # Ensure an LGA is selected
      h3(paste("Intervention and Total Cost Breakdown for", input$unit, " LGA in ", input$state_lga, " State"),
         style = "text-align: left; margin-top: 20px;")
    })
    
    output$lga_donut_title <- renderUI({
      req(input$unit)  # Ensure an LGA is selected
      h3(paste("Proportional Cost Breakdown for", input$unit, " LGA in ", input$state_lga, " State"),
         style = "text-align: left; margin-top: 20px;")
    })
    
    
    # Reactive: Filter the lga data based on selection and currency
    filtered_lga_total_data <- reactive({
      req(input$state_lga, input$unit, input$currency_option_state)  # Ensure both inputs are available
      lga_total_cost_summary %>%
        filter(state == input$state_lga, lga == input$unit, currency == input$currency_option_lga) |> 
        filter(total_cost != 0) |> 
        select(intervention_type, title, target, target_value, total_cost, cost_per_target)  |> 
        arrange((intervention_type))
    })
    
  
    # Render Total Cost Breakdown Table
    output$lga_budget_table <- renderDT({
      datatable(
        filtered_lga_total_data(),
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        colnames = c(
          "Category" = "intervention_type",
          "Item" = "title",
          "Target Denominator" = "target",
          "Target Population or Area" = "target_value",
          "Total Cost" = "total_cost",
          "Cost per Target" = "cost_per_target"
        )) %>%
        formatStyle(
          columns=1:7,
          fontSize = '14px'
        ) %>%
        formatCurrency(
          columns = "Total Cost",
          currency = if (input$currency_option == "USD") "$" else "₦",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>%
        formatCurrency(
          columns = "Cost per Target",
          currency = if (input$currency_option == "USD") "$" else "₦",
          interval = 3,
          mark = ",",
          digits = 2
        ) %>%
        formatCurrency(
          columns = "Target Population or Area",
          currency = "",  # No currency symbol
          interval = 3,   # Add comma separator
          mark = ",", 
          digits = 0      # No decimals
        ) 
    })
    
    
    # Render Proportional Cost Breakdown Donut Chart
    output$lga_donut_chart <- renderBillboarder({
      create_cost_donut(
        data = filtered_lga_total_data() |> mutate(currency = input$currency_option_state),
        currency_choice = input$currency_option_state
      )
    })
    
    
    # Reactive: Title for the Cost Analysis Visualizations Section
    output$lga_cost_analysis_title <- renderUI({
      req(input$state_lga, input$unit)  # Ensure a state is selected
      
      h3(paste("Cost Analysis Visualizations for", input$unit, " LGA in", input$state_lga, " State"),
         style = "text-align: left; margin-top: 20px;")
    })
    
    # Reactive: Filter the state-level data by selected state and currency
    filtered_lga_chart_data <- reactive({
      req(input$state_lga, input$unit, input$currency_option_lga)  # Ensure both inputs are available
      
      lga_intervention_chart_data %>%
        filter(state == input$state_lga, lga == input$unit, currency == input$currency_option_lga)
    })
    
    # Render Treemap Plot for the Selected State
    output$lga_treemap_plot <- renderPlotly({
      req(filtered_lga_chart_data())  # Ensure valid data
      create_treemap_plot(filtered_lga_chart_data(), input$currency_option_lga)
    })
    
    # Render Stacked Bar Plot for the Selected State
    output$lga_stacked_bar_plot <- renderPlotly({
      req(filtered_lga_chart_data())  # Ensure valid data
      create_stacked_bar_plot(filtered_lga_chart_data(), input$currency_option_lga)
    })
    
    # Render Lollipop Plot for the Selected State
    output$lga_lollipop_plot <- renderPlotly({
      req(filtered_lga_chart_data())  # Ensure valid data
      create_lollipop_plot(filtered_lga_chart_data(), input$currency_option_lga)
    })
    
    # Render Proportion Plot for the Selected State
    output$lga_prop_plot <- renderPlotly({
      req(filtered_lga_chart_data())  # Ensure valid data
      create_prop_plot(filtered_lga_chart_data(), input$currency_option_lga)
    })
    
    # Reactive: Filter LGA data by selected state and currency
    lga_filtered_data_lga <- reactive({
      lga_outline |> 
        left_join(lga_data_total_cost) %>%
        filter(currency == input$currency_option_state)
    })
    
    # Total Cost Map
    output$lga_total_cost_map_lga <- renderLeaflet({
      map_data_sf <- lga_filtered_data_lga()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "total",
        currency_option = input$currency_option_state
      )
    })
    
    # Cost per Person Map
    output$lga_cost_per_person_map_lga <- renderLeaflet({
      map_data_sf <- lga_filtered_data_lga()
      create_nigeria_cost_map(
        data = map_data_sf,
        map_type = "per_person",
        currency_option = input$currency_option_state
      )
    })
    
    # Clear all selections and reset everything on Clear Selection click
    observeEvent(input$clear_selection_lga, {
      # Reset the state and LGA dropdowns
      updateSelectInput(session, "state_lga", selected = "")
      updateSelectizeInput(session, "unit", selected = "")
      
      # Clear LGA highlights from all maps
      leafletProxy("lga_map1") %>% clearGroup("highlight")
      leafletProxy("lga_map2") %>% clearGroup("highlight")
      leafletProxy("lga_map_interactive") %>% clearGroup("highlight")
      leafletProxy("lga_total_cost_map_lga") %>% clearGroup("highlight")
      leafletProxy("lga_cost_per_person_map_lga") %>% clearGroup("highlight")
      
      # Clear any additional UI outputs
      output$lga_info_section <- renderUI({ NULL })
    })
    
    
    

    #-PLAN COMPARISON PAGE-------------------------------------------------------------------------
    
    # Baseline Map
    output$baseline_map <- renderLeaflet({
      create_intervention_map_static(
        lga_outline = lga_outline,
        state_outline = state_outline,
        country_outline = country_outline,
        intervention_mix = plan_comparison_mixes[plan_comparison_mixes$plan == "Baseline", ]
      )
    })
    
    # Dynamic Maps for Selected Plans
    output$selected_plans_maps <- renderUI({
      selected_plans <- input$selected_plans
      if (length(selected_plans) == 0) return(NULL)
      
      # Calculate the number of columns per row
      num_columns <- 3 #12 / min(length(selected_plans) + 1, 3)  # Adjust columns for side-by-side layout, up to 3 maps per row
      
      # Create a list of columns for each selected plan
      map_outputs <- lapply(seq_along(selected_plans), function(i) {
        plan <- selected_plans[i]
        description <- plan_descriptions[unique_plans == plan]
        
        # Create a column with a title and the leaflet map
        column(
          num_columns,
          h4(paste(plan, "-", description)),  # Add the title
          leafletOutput(outputId = paste0("map_", plan))
        )
      })
      
      # Return the map outputs in a fluidRow
      do.call(fluidRow, map_outputs)
    })
    
    # Generate Leaflet Maps for Each Selected Plan
    observe({
      selected_plans <- input$selected_plans
      if (length(selected_plans) == 0) return()
      
      lapply(selected_plans, function(plan) {
        output[[paste0("map_", plan)]] <- renderLeaflet({
          create_intervention_map_static(
            lga_outline = lga_outline,
            state_outline = state_outline,
            country_outline = country_outline,
            intervention_mix = plan_comparison_mixes[plan_comparison_mixes$plan == plan, ]
          )
        })
      })
    })
   
    
    # Reactive function to prepare cost data based on selected currency and plans
    prepare_cost_data <- reactive({
      req(input$currency_option_plan, input$selected_plans)  # Ensure inputs are available
      
      # Filter and summarize data based on the selected currency and plans
      total_cost_comparisons %>%
        filter(currency == input$currency_option_plan, 
               plan %in% c("Baseline", input$selected_plans))
    })
    
    # Cost Comparison Plot
    output$cost_comparison_plot <- renderPlotly({
      cost_data <- prepare_cost_data()
      if (nrow(cost_data) == 0) return(NULL)  # Return if no data
      
      currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
      
      # Divide the full_cost by 1 million to express in millions and round to remove decimals
      cost_data <- cost_data %>%
        mutate(full_cost_millions = round(full_cost / 1e6))  # Round to nearest whole number
      
      p <- ggplot(cost_data, aes(x = plan, y = full_cost_millions, fill = plan)) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_text(aes(label = paste0(currency_symbol, format(full_cost_millions, big.mark = ","), "M")),
                  vjust = -0.5, size = 4) +  # Add labels above the bars
        theme_minimal() +
        labs(y = paste("Total Cost (", input$currency_option_plan, " in Millions)"), x = "") +
        theme(text = element_text(size = 12)) +
        scale_y_continuous(labels = scales::comma) +
        guides(fill = "none")  # Remove the legend
      
      ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white"))
    })
    
    # Cost Difference Plot
    output$cost_difference_plot <- renderPlotly({
      cost_data <- prepare_cost_data()
      if (nrow(cost_data) == 0) return(NULL)  # Return if no data
      
      baseline_cost <- cost_data$full_cost[cost_data$plan == "Baseline"]
      if (length(baseline_cost) == 0) return(NULL)
      
      # Calculate differences for selected plans, convert to millions, and round to remove decimals
      diff_data <- cost_data %>%
        filter(plan != "Baseline") %>%
        mutate(
          difference_millions = round((full_cost - baseline_cost) / 1e6),  # Difference in millions, rounded
          percent_change = round((difference_millions * 1e6 / baseline_cost) * 100),  # Percentage change, rounded
          label = paste(plan, "vs Baseline")
        )
      
      currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
      
      p <- ggplot(diff_data, aes(x = difference_millions, y = label,
                                 text = paste0("Difference: ", ifelse(difference_millions >= 0, "+", ""), currency_symbol,
                                               format(difference_millions, big.mark = ","), "M<br>",
                                               "Change from Baseline: ", sprintf("%.0f%%", percent_change)))) +  # No decimals
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        geom_segment(aes(x = 0, xend = difference_millions, y = label, yend = label),
                     color = ifelse(diff_data$difference_millions >= 0, "#ED7D31", "#4472C4")) +
        geom_point(size = 4) +
        theme_minimal() +
        labs(x = paste("Change in Cost (", input$currency_option_plan, " in Millions)"), y = "") +
        theme(text = element_text(size = 12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_x_continuous(labels = scales::comma)
      
      ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white"))
    })
    
    # Reactive function to filter and format data for a given plan
    filter_and_format_plan_data <- function(plan) {
      cost_data <- plan_comparison_costs %>%
        filter(plan == !!plan, currency == input$currency_option_plan) %>%
        select(intervention_type, title, state_count, lga_count, target, target_value, total_cost) %>%
        arrange(intervention_type) 
      return(cost_data)
    }
    
    # Render the baseline table by default
    output$summary_data_tables <- renderUI({
      # Render the baseline table
      baseline_data <- filter_and_format_plan_data("Baseline")
      
      # Calculate the total cost for the baseline plan and format it
      total_cost_sum_baseline <- sum(baseline_data$total_cost, na.rm = TRUE)
      currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
      formatted_total_cost_baseline <- paste0(currency_symbol, format(total_cost_sum_baseline, big.mark = ","), "M")
      
      baseline_table <- fluidRow(
        h4(HTML(paste("Plan - Baseline Costed Operational Plan. Total Cost:", 
                      "<strong>", formatted_total_cost_baseline, "</strong>"))),
        DT::datatable(
          baseline_data |>
            filter(intervention_type == "Malaria Interventions") %>%
            mutate(
              target_value = format(round(target_value, 0), big.mark = ","),  # No decimals, comma separators
              total_cost = round(total_cost / 1e6, 0)  # Convert to millions and round to no decimals
            ),
          options = list(pageLength = 20, scrollX = TRUE),
          rownames = FALSE,
          colnames = c(
            "Category" = "intervention_type",
            "Item" = "title",
            "State Count" = "state_count",
            "LGA Count" = "lga_count",
            "Target Denominator" = "target",
            "Target Population or Area" = "target_value",
            "Total Cost (Millions)" = "total_cost"
          )
        )
      )
      
      # Generate tables for each selected plan
      selected_tables <- lapply(input$selected_plans, function(plan) {
        plan_data <- filter_and_format_plan_data(plan)
        plan_description <- unique(plan_comparison_costs$plan_description[plan_comparison_costs$plan == plan])
        total_cost_sum <- round(sum(plan_data$total_cost), 0) 
        formatted_total_cost <- paste0(currency_symbol, format(total_cost_sum, big.mark = ","), "M")
        
        fluidRow(
          h4(HTML(paste("Plan -", plan_description, ". Total Cost:",
                        "<strong>", formatted_total_cost, "</strong>"))),
          DT::datatable(
            plan_data |> filter(intervention_type == "Malaria Interventions") %>%
              mutate(
                target_value = format(round(target_value, 0), big.mark = ","),  # No decimals, comma separators
                total_cost = round(total_cost / 1e6, 0)  # Convert to millions and round to no decimals
              ),
            options = list(pageLength = 20, scrollX = TRUE),
            rownames = FALSE,
            colnames = c(
              "Category" = "intervention_type",
              "Item" = "title",
              "State Count" = "state_count",
              "LGA Count" = "lga_count",
              "Target Denominator" = "target",
              "Target Population or Area" = "target_value",
              "Total Cost (Millions)" = "total_cost"
            )
          )
        )
      })
      
      # Combine the baseline table with the selected plan tables
      do.call(tagList, c(list(baseline_table), selected_tables))
    })

}




#-RUN THE APPLICATION-----------------------------------------------------------
shinyApp(ui = ui, server = server)












