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
           h4("This page provides a summary of the costs associated with the 2025 prioritised intervention package at the National or State level. Select a spatial scale to initiate summaries"),
           # Horizontal sidebar at the top
           fluidRow(
             column(
               width = 12,
               div(
                 style = "border: 1px solid grey; padding: 10px; border-radius: 5px; background-color: #f8f9fa; display: flex; align-items: center; justify-content: space-between;",
                 
                 # Spatial level selection dropdown
                 div(
                   selectInput(
                     "spatial_level",
                     "Select Spatial Level:",
                     choices = c("", "National", "State"), # Blank as initial value
                     selected = "National"
                   )
                 ),
                 
                 # Conditionally rendered state selection dropdown
                 div(
                   conditionalPanel(
                     condition = "input.spatial_level == 'State'",
                     selectizeInput(
                       "state_selection",
                       "Select State of Interest:",
                       choices = NULL, # Populate dynamically in server
                       options = list(placeholder = "Type or select a state")
                     )
                   )
                 ),
                 
                 # Currency selection radio buttons
                 div(
                   radioButtons(
                     "currency_option",
                     "Currency Selection:",
                     choices = c("USD", "Naira"),
                     selected = "USD",
                     inline = TRUE
                   ) %>% tagAppendAttributes(style = "font-size: 20px;")
                 ),
                 
                 # Clear selection button
                 div(
                   actionButton(
                     "clear_selection",
                     "Clear Selection",
                     icon = icon("eraser")
                   )
                 )
               )
             )
           ),
           hr(), # A horizontal line separator
           mainPanel(
             width = 12,
             fluidRow(
               column(4, leafletOutput("map1", height = "550px")),
               # column(4, leafletOutput("map_interactive", height = "550px")),
               # column(4, leafletOutput("map2", height = "550px"))
             ),
             uiOutput("info_section"), 
             hr(),
             fluidRow(
               column(6, # Left column (half width)
                      uiOutput("table_title"),
                      DTOutput("budget_table"),
                      uiOutput("summary_text")
               ),
               column(6, # Right column (half width)
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
  

  
  # FOURTH TAB IS BUDGET COMPARISONS
  tabPanel(
    "Plan Comparison",
    fluidPage(
      # Sidebar
      sidebarLayout(
        sidebarPanel(
          width = 12,
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
        # Main Panel
        mainPanel(
          width = 12,
          # Baseline Map (Full Width)
          fluidRow(
            column(
              width = 12,
              h4("Baseline Costed Operational Plan", style = "text-align: center;"),
              leafletOutput("baseline_map", height = "600px")  # Full-width map
            )
          ),
          hr(),
          # Dynamic Comparison Maps (Max 2 per row)
          fluidRow(
            uiOutput("selected_plans_maps")
          ),
          hr(),
          # Cost Comparison Visualizations
          h4("Cost Comparison Visualizations"),
          fluidRow(
            column(6, plotlyOutput("cost_comparison_plot", height = "700px")),
            column(6, plotlyOutput("cost_difference_plot", height = "700px"))
          ),
          hr(),
          # Summary Data Tables
          h4("Cost Summary Tables"),
          uiOutput("summary_data_tables")
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
  
  #-HELPERS----------------------------------------------------------------------------------
  # Color palette for under-5 prevalence
  color_pal_prevalence <- reactive({
    colorNumeric(viridis(50, direction=-1), domain = prevalence_data$prev_u5_state)
  })
  
  # State names 
  observe({
    states <- c("", unique(state_outline$state)) # Add an empty string as the first option
    
    updateSelectizeInput(
      session,
      "state_selection",
      choices = states,
      server = TRUE
    )
  })
  
  # Reactive: Store the selected state polygon (only if spatial level is State)
  selected_state_outline <- reactive({
    req(input$spatial_level == "State", input$state_selection)  # Ensure spatial level is State and a state is selected
    
    # Filter the selected state from the state_outline dataset
    state_outline %>%
      filter(state == input$state_selection)  
  })
  
  #-SUMMARY TAB-------------------------------------------------------------------------------
  
  #-INTERVENTION MIX MAP------------------------------------
  output$map1 <- renderLeaflet({
    create_intervention_map_static(
      lga_outline = intervention_mix_map,
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix = intervention_mix
    )
  }) |> bindCache(input$spatial_level, input$state_selection)
  
  #-INTERACTIVE INTERVENTION MAP------------------------------
  # Create initial map
  # output$map_interactive <- renderLeaflet({
  #   create_base_interactive_map(
  #     interactive_map = interactive_map,
  #     intervention_mix = intervention_mix
  #   ) %>%
  #     htmlwidgets::onRender(create_legend_js(intervention_mix = intervention_mix))
  # }) |> bindCache(input$selected_intervention, input$spatial_level, input$state_selection)
  
  # Observer for legend clicks
  # observeEvent(input$selected_intervention, {
  #   selected_intervention <- input$selected_intervention
  #   
  #   # Filter polygons based on the selected intervention
  #   highlighted_lgas <- interactive_map %>%
  #     filter(unique_interventions == selected_intervention) |> 
  #     bindCache(input$selected_intervention)
  #   
  #   # Update the map
  #   update_intervention_map(
  #     map_id = "map_interactive",
  #     highlighted_lgas = highlighted_lgas,
  #     state_outline = state_outline,
  #     country_outline = country_outline
  #   ) |> 
  #     bindCache(input$selected_intervention, input$spatial_level, input$state_selection)
  # })
  
  
  #-PREVALENCE MAP--------------------------------------------
  # output$map2 <- renderLeaflet({
  #   create_prevalence_map(
  #     prevalence_data = prevalence_data,
  #     state_outline = state_outline,
  #     country_outline = country_outline,
  #     color_pal_prevalence = color_pal_prevalence
  #   )
  # })
  
  #-ICON SUMMARIES---------------------------------------------------
  # Reactive: Store either national or state data based on input
  icon_summary_data <- reactive({
    if (input$spatial_level == "National") {
      req(national_ribbon_data)  # Ensure national data exists
      national_ribbon_data
    } else if (input$spatial_level == "State") {
      req(input$state_selection)  # Ensure a state is selected
      filter(state_ribbon_data, state == input$state_selection)  # Filter state-level data
    } else {
      NULL  # Return NULL if no valid selection
    }
  })
  
  # Render Icons
  output$info_section <- renderUI({
    req(icon_summary_data())  # Ensure valid data exists
    
    create_icon_summaries(
      data = icon_summary_data(),  # Use the reactive data
      currency_choice = input$currency_option  # Use the selected currency
    )
  })
  
  #-TOTAL COST SUMMARY TABLE-----------------------------------------
  # Reactive: Generate dynamic table title
  table_title_text <- reactive({
    if (input$spatial_level == "National") {
      "Intervention and Total Cost Breakdown at the National Level"
    } else if (input$spatial_level == "State" && !is.null(input$state_selection) && input$state_selection != "") {
      paste("Intervention and Total Cost Breakdown at the State Level:", input$state_selection, "State")
    } else {
      "Intervention and Total Cost Breakdown"  # Fallback title
    }
  })
  
  output$table_title <- renderUI({
    h3(table_title_text(), style = "text-align: left; margin-top: 20px;")
  })
  
  # Reactive: Filter and prepare data for the summary table
  summary_table_data <- reactive({
    req(input$spatial_level)  # Ensure a spatial level is selected
    
    # Select the dataset based on spatial level
    if (input$spatial_level == "National") {
      # Filter national data
      national_total_cost_summary %>%
        filter(currency == input$currency_option, total_cost != 0) %>%
        select(intervention_type, title, state_count, lga_count, target, target_value, total_cost, cost_per_target) |> 
        arrange(intervention_type)
    } else if (input$spatial_level == "State" && !is.null(input$state_selection) && input$state_selection != "") {
      # Filter state-specific data
      state_total_cost_summary %>%
        filter(state == input$state_selection, currency == input$currency_option, total_cost != 0) %>%
        select(intervention_type, title, lga_count, target, target_value, total_cost, cost_per_target)  |> 
        arrange(intervention_type)
    } else {
      NULL  # Return NULL if no valid selection
    }
  })
  
  
  # Render the DataTable
  output$budget_table <- renderDT({
    req(summary_table_data())  # Ensure valid data exists
    
    # Get the filtered dataset
    data <- summary_table_data()
    
    # Define column names dynamically based on spatial level
    if (input$spatial_level == "National") {
      col_names <- c(
        "Category" = "intervention_type",
        "Item" = "title",
        "States Targeted" = "state_count",
        "LGAs Targeted" = "lga_count",
        "Target Denominator" = "target",
        "Target Population or Area" = "target_value",
        "Total Cost" = "total_cost",
        "Cost per Target" = "cost_per_target"
      )
    } else {
      col_names <- c(
        "Category" = "intervention_type",
        "Item" = "title",
        "LGAs Targeted" = "lga_count",
        "Target Denominator" = "target",
        "Target Population or Area" = "target_value",
        "Total Cost" = "total_cost",
        "Cost per Target" = "cost_per_target"
      )
    }
    
    # Render the DataTable
    datatable(
      data,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      colnames = col_names
    ) %>%
      formatStyle(
        columns = 1:ncol(data),
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
  # Reactive: Generate dynamic table title
  donut_title_text <- reactive({
    if (input$spatial_level == "National") {
      "Proportional Cost Breakdown at the National Level"
    } else if (input$spatial_level == "State" && !is.null(input$state_selection) && input$state_selection != "") {
      paste("Proportional Cost Breakdown at the State Level:", input$state_selection, "State")
    } else {
      "Proportional Cost Breakdown"  # Fallback title
    }
  })
  
  output$donut_title <- renderUI({
    h3(donut_title_text(), style = "text-align: left; margin-top: 20px; margin-bottom: 50px;")
  })

  # Render the Main Donut Chart
  donut_chart_data <- reactive({
    req(input$spatial_level)  # Ensure a spatial level is selected
    
    if (input$spatial_level == "National") {
      # Use national data
      national_total_cost_summary %>%
        filter(currency == input$currency_option, total_cost != 0)
    } else if (input$spatial_level == "State" && !is.null(input$state_selection) && input$state_selection != "") {
      # Use state-specific data
      state_total_cost_summary %>%
        filter(state == input$state_selection, currency == input$currency_option, total_cost != 0)
    } else {
      NULL  # Return NULL if no valid selection
    }
  })
  
  # Render the Main Donut Chart
  output$donut_chart <- renderBillboarder({
    req(donut_chart_data())  # Ensure valid data exists
    
    # Use the filtered data to create the donut chart
    create_cost_donut(
      data = donut_chart_data(),
      currency_choice = input$currency_option
    )
  })
  
  #-COST BREAKDOWN PLOTS------------------------------------------------------
  # Reactive: Prepare data for the cost visualization plots
  cost_visualization_data <- reactive({
    req(input$spatial_level)  # Ensure a spatial level is selected
    
    if (input$spatial_level == "National") {
      # Use national data
      national_intervention_chart_data %>%
        filter(currency == input$currency_option)
    } else if (input$spatial_level == "State" && !is.null(input$state_selection) && input$state_selection != "") {
      # Use state-specific data
      state_intervention_chart_data %>%
        filter(state == input$state_selection, currency == input$currency_option)
    } else {
      NULL  # Return NULL if no valid selection
    }
  })
  
  # render plots
  output$treemap_plot <- renderPlotly({
    # Create the treemap plot
    create_treemap_plot(cost_visualization_data(), input$currency_option)
  })
  
  output$stacked_bar_plot <- renderPlotly({
    # Create the stacked bar plot
    create_stacked_bar_plot(cost_visualization_data(), input$currency_option)
  })
  
  output$lollipop_plot <- renderPlotly({
    # Create top 10 plot
    create_lollipop_plot(cost_visualization_data(), input$currency_option)
  })
  
  output$prop_plot <- renderPlotly({
    # Create proportional cost bar plot
    create_prop_plot(cost_visualization_data(), input$currency_option)
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
  
  #-STATE HIGHLIGHT OPTIONS FOR ALL MAPS---------------------------------------------------------
  # Observe changes in the selected state and apply highlighting
  observeEvent(input$state_selection, {
    req(input$spatial_level == "State")  # Ensure spatial level is State
    req(selected_state_outline())       # Ensure the selected state outline exists
    
    # Highlight the selected state on map1
    highlight_state("map1", selected_state_outline())
    # highlight_state("map_interactive", selected_state_outline())
    # highlight_state("map2", selected_state_outline())
    highlight_state("total_cost_map", selected_state_outline())
    highlight_state("cost_per_person_map", selected_state_outline())
  })
  
  # remove highlights if national level is reselected 
  observeEvent(input$spatial_level, {
    req(input$spatial_level == "National")  # Ensure spatial level is State
   
    # Clear highlights from all maps
    leafletProxy("map1") %>% clearGroup("highlight")
    # leafletProxy("map2") %>% clearGroup("highlight")
    # leafletProxy("map_interactive") %>% clearGroup("highlight")
    leafletProxy("total_cost_map") %>% clearGroup("highlight")
    leafletProxy("cost_per_person_map") %>% clearGroup("highlight")
  })
  
  #-CLEAR SELECTIONS-----------------------------------------------------------------------------
  # Clear state selection and reset everything
  observeEvent(input$clear_selection, {
    # Reset spatial level and state dropdown
    updateSelectInput(session, "spatial_level", selected = "National")
    updateSelectizeInput(session, "state_selection", selected = "")
    updateRadioButtons(session, "currency_option", selected = "USD")  # Reset currency option to default
    
    # Clear highlights from all maps
    leafletProxy("map1") %>% clearGroup("highlight")
    # leafletProxy("map2") %>% clearGroup("highlight")
    # leafletProxy("map_interactive") %>% clearGroup("highlight")
    leafletProxy("total_cost_map") %>% clearGroup("highlight")
    leafletProxy("cost_per_person_map") %>% clearGroup("highlight")
    
    # Reset UI elements to National level
    output$info_section <- renderUI({
      create_icon_summaries(
        data = national_ribbon_data,  # Reset to national data
        currency_choice = "USD"
      )
    })
    
    output$table_title <- renderUI({
      h3("Intervention and Total Cost Breakdown at the National Level", style = "text-align: left; margin-top: 20px;")
    })
    
    output$budget_table <- renderDT({
      datatable(
        national_total_cost_summary %>%
          filter(currency == "USD", total_cost != 0) %>%
          select(intervention_type, title, state_count, lga_count, target, target_value, total_cost, cost_per_target),
        options = list(pageLength = 20, scrollX = TRUE),
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
        )
      )
    })
    
    output$donut_title <- renderUI({
      h3("Proportional Cost Breakdown at the National Level", style = "text-align: left; margin-top: 20px; margin-bottom: 50px;")
    })
    
    output$donut_chart <- renderBillboarder({
      create_cost_donut(
        data = national_total_cost_summary %>% filter(currency == "USD", total_cost != 0),
        currency_choice = "USD"
      )
    })
    
    # Reset cost visualization plots
    output$treemap_plot <- renderPlotly({
      create_treemap_plot(
        data = national_intervention_chart_data %>% filter(currency == "USD"),
        currency_choice = "USD"
      )
    })
    
    output$stacked_bar_plot <- renderPlotly({
      create_stacked_bar_plot(
        data = national_intervention_chart_data %>% filter(currency == "USD"),
        currency_choice = "USD"
      )
    })
    
    output$lollipop_plot <- renderPlotly({
      create_lollipop_plot(
        data = national_intervention_chart_data %>% filter(currency == "USD"),
        currency_choice = "USD"
      )
    })
    
    output$prop_plot <- renderPlotly({
      create_prop_plot(
        data = national_intervention_chart_data %>% filter(currency == "USD"),
        currency_choice = "USD"
      )
    })
  })


  #-PLAN COMPARISON PAGE ------------------------------------------------------------------------
  
  # Baseline Map
  output$baseline_map <- renderLeaflet({
    create_intervention_map_static(
      lga_outline = intervention_mix_map_plan_comp[intervention_mix_map_plan_comp$plan == "Baseline",],
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix = plan_comparison_mixes[plan_comparison_mixes$plan == "Baseline", ]
    ) 
  })
  
  # Dynamic Maps for Selected Plans
  output$selected_plans_maps <- renderUI({
    selected_plans <- input$selected_plans
    if (length(selected_plans) == 0) return(NULL)
    
    # Limit to 2 maps per row
    num_columns <- 6  # 12 divided by 2 maps per row
    
    # Create map outputs
    map_outputs <- lapply(seq_along(selected_plans), function(i) {
      plan <- selected_plans[i]
      description <- plan_descriptions[unique_plans == plan]
      
      column(
        width = num_columns,
        div(
          style = "padding: 10px;",
          h4(paste(plan, "-", description), style = "text-align: center;"),
          leafletOutput(outputId = paste0("map_", plan), height = "450px")
        )
      )
    })
    
    # Arrange maps in rows of 2
    rows <- split(map_outputs, ceiling(seq_along(map_outputs) / 2))
    lapply(rows, fluidRow) %>% tagList()
  })
  
  # Generate Leaflet Maps for Each Selected Plan
  observe({
    selected_plans <- input$selected_plans
    if (length(selected_plans) == 0) return()
    
    lapply(selected_plans, function(plan) {
      output[[paste0("map_", plan)]] <- renderLeaflet({
        create_intervention_map_static(
          lga_outline = intervention_mix_map_plan_comp[intervention_mix_map_plan_comp$plan == plan, ],
          state_outline = state_outline,
          country_outline = country_outline,
          intervention_mix = plan_comparison_mixes[plan_comparison_mixes$plan == plan, ]
        ) 
      })
    })
  })
  
  # Reactive function for Cost Comparison Data
  prepare_cost_data <- reactive({
    req(input$currency_option_plan, input$selected_plans)
    
    total_cost_comparisons %>%
      filter(currency == input$currency_option_plan, plan %in% c("Baseline", input$selected_plans))
  })
  
  # Cost Comparison Plot
  output$cost_comparison_plot <- renderPlotly({
    cost_data <- prepare_cost_data()
    req(nrow(cost_data) > 0)
    
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
    
    cost_data <- cost_data %>%
      mutate(full_cost_millions = round(full_cost / 1e6))
    
    p <- ggplot(cost_data, aes(x = plan, y = full_cost_millions, fill = plan)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(currency_symbol, format(full_cost_millions, big.mark = ","), "M")),
                vjust = -0.5, size = 4) +
      theme_minimal() +
      labs(y = paste("Total Cost (", input$currency_option_plan, " in Millions)"), x = "") +
      theme(text = element_text(size = 12)) +
      scale_y_continuous(labels = scales::comma) +
      guides(fill = "none")
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Cost Difference Plot
  output$cost_difference_plot <- renderPlotly({
    cost_data <- prepare_cost_data()
    req(nrow(cost_data) > 0)
    
    baseline_cost <- cost_data$full_cost[cost_data$plan == "Baseline"]
    req(length(baseline_cost) > 0)
    
    diff_data <- cost_data %>%
      filter(plan != "Baseline") %>%
      mutate(
        difference_millions = round((full_cost - baseline_cost) / 1e6),
        percent_change = round((difference_millions * 1e6 / baseline_cost) * 100),
        label = paste(plan, "vs Baseline")
      )
    
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
    
    p <- ggplot(diff_data, aes(x = difference_millions, y = label)) +
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












