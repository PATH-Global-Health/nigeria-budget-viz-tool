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
source("global-amen-version.R")  # loads data
source("helpers-amen-version.R") # functions for server outputs

# Disable scientific notation globally
options(scipen = 999)

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
           h4("This page provides a summary of the costs associated with the Scenario 1 intervention package (fully-scaled up plan) at the National level"),
           # Horizontal sidebar at the top
           fluidRow(
             column(
               width = 12,
               div(
                 style = "border: 1px solid grey; padding: 10px; border-radius: 5px; background-color: #f8f9fa; display: flex; align-items: center; justify-content: space-around;",
                
                 # Spatial level selection dropdown
                 div(
                   selectInput(
                     "spatial_level",
                     "Select Spatial Level:",
                     choices = c("National"), 
                     selected = "National"
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
                   )
                 )
                 )
               )
             ),
           hr(), # A horizontal line separator
           mainPanel(
             width = 12,
             fluidRow(
               column(6, leafletOutput("map1", height = "700px")),
               column(6, plotOutput("intervention_mix_facets", height = "700px"))
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
             hr()
           )
  ), 
  
  
  
  # FOURTH TAB IS BUDGET COMPARISONS
  tabPanel(
    "Plan Comparison",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 12,
          h4("Select plans to compare with Scenario 1"),
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
          width = 12,
          # Maps Layout
          fluidRow(
            uiOutput("maps_row")
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
          uiOutput("summary_data_tables"), 
          hr(), 
          # Add a section for the final plot
          fluidRow(
            column(12,
                   h4("Cost Distribution by Intervention and Scenario"),
                   plotOutput("final_cost_plot", height = "600px")
            )
          )
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

  #-SUMMARY TAB-------------------------------------------------------------------------------
  
  #-INTERVENTION MIX MAP------------------------------------
  output$map1 <- renderLeaflet({
    create_intervention_map_static(
      lga_outline = intervention_mix_map,
      state_outline = state_outline,
      country_outline = country_outline,
      intervention_mix = intervention_mix_map
    )
  })
  
  #-INTERVENTION FACET MAP----------------------------------
  output$intervention_mix_facets <- renderPlot({
    ggplot() +   
      geom_sf(data = lga_outline, fill = "grey90", col="grey", alpha=0.5)+
      geom_sf(data = intervention_mix_plot, fill = "#1B7339", color = "grey") +
      geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") + 
      facet_wrap(vars(mix)) +  
      theme_void(18) +  
      theme(legend.position = "none")  
  })
  
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
    
    int_names = c("ITN Campaign",
                  "ITN Routine Distribution",
                  "LSM",
                  "IRS",
                  "Entomological surveillance",
                  "Malaria Vaccine",
                  "SMC",
                  "PMC", 
                  "IPTp",
                  "Public Sector CM",
                  "Private Sector CM",
                  "Global Fund Warehousing & Distribution Activities",
                  "Capacity Building",
                  "Governance & Coordination",
                  "Monitoring & Evaluation",
                  "Resource Mobilisation",
                  "Social Behaviour Change")
    
    # Select the dataset based on spatial level
    if (input$spatial_level == "National") {
      # Filter national data
      national_total_cost_summary %>%
        filter(currency == input$currency_option, total_cost != 0) %>%
        select(intervention_type, title, state_count, lga_count, total_cost) |> 
        mutate(title = factor(title, levels = int_names)) %>%  # Set the custom order
        arrange(title)
      
    } else if (input$spatial_level == "State" && !is.null(input$state_selection) && input$state_selection != "") {
      # Filter state-specific data
      state_total_cost_summary %>%
        filter(state == input$state_selection, currency == input$currency_option, total_cost != 0) %>%
        select(intervention_type, title, lga_count, total_cost)  |> 
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
        "Total Cost" = "total_cost"
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
      national_total_cost_summary %>%
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

  
  
  #-PLAN COMPARISON PAGE ------------------------------------------------------------------------
  
  # Render UI for maps
  output$maps_row <- renderUI({
    selected_plans <- input$selected_plans
    all_plans <- c("Scenario 1", selected_plans) # Include the baseline map first
    
    # Create map outputs
    map_outputs <- lapply(seq_along(all_plans), function(i) {
      plan <- all_plans[i]
      title <- ifelse(plan == "Scenario 1", 
                      "Scenario 1 - Fully scaled-up plan", 
                      paste(plan, "-", plan_descriptions[unique_plans == plan]))
      
      column(
        width = 6,  # Two maps per row (12/6 = 2)
        div(
          h4(title, style = "text-align: center;"),
          leafletOutput(outputId = paste0("map_", plan), height = "500px")
        )
      )
    })
    
    # Wrap maps into rows of 2
    rows <- split(map_outputs, ceiling(seq_along(map_outputs) / 2))
    lapply(rows, fluidRow) %>% tagList()
  })
  
  # Generate Leaflet Maps for Each Plan
  observe({
    all_plans <- c("Scenario 1", input$selected_plans) # Include baseline map first
    
    lapply(all_plans, function(plan) {
      output[[paste0("map_", plan)]] <- renderLeaflet({
        create_intervention_map_static(
          lga_outline = plan_comparison_mixes[plan_comparison_mixes$plan == plan, ],
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
      filter(currency == input$currency_option_plan, plan %in% c("Scenario 1", input$selected_plans)) 
  })
  
  # Cost Comparison Plot
  output$cost_comparison_plot <- renderPlotly({
    cost_data <- prepare_cost_data()
    req(nrow(cost_data) > 0)
    
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
    
    cost_data <- cost_data %>%
      mutate(
        full_cost_millions = round(full_cost / 1e6),
        hover_text = paste0("Plan: ", plan, "<br>",
                            "Total Cost: ", currency_symbol, format(full_cost_millions, big.mark = ","), "M")
      )
    
    p <- ggplot(cost_data, aes(x = plan, y = full_cost_millions, fill = plan, text = hover_text)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(currency_symbol, format(full_cost_millions, big.mark = ","), "M")),
                vjust = -0.5, size = 4) +
      theme_minimal() +
      labs(y = paste("Total Cost (", input$currency_option_plan, " in Millions)"), x = "") +
      theme(text = element_text(size = 12)) +
      scale_y_continuous(labels = scales::comma) +
      guides(fill = "none") +
      scale_fill_manual(values = c("Scenario 1" = "#156082", 
                                   "Scenario 2" = "#A02B93", 
                                   "Scenario 3" = "#3B7D23", 
                                   "Scenario 4" = "#E97132"))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  
  # # Cost Difference Plot
  # output$cost_difference_plot <- renderPlotly({
  #   cost_data <- prepare_cost_data()
  #   req(nrow(cost_data) > 0)
  # 
  #   baseline_cost <- cost_data$full_cost[cost_data$plan == "Scenario 1"]
  #   req(length(baseline_cost) > 0)
  # 
  #   currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
  # 
  #   diff_data <- cost_data %>%
  #     filter(plan != "Scenario 1") %>%
  #     mutate(
  #       difference_millions = round((full_cost - baseline_cost) / 1e6),
  #       percent_change = round((difference_millions * 1e6 / baseline_cost) * 100),
  #       hover_text = paste0("Plan: ", plan, "<br>",
  #                           "Difference: ", ifelse(difference_millions >= 0, "+", ""), currency_symbol,
  #                           format(difference_millions, big.mark = ","), "M<br>",
  #                           "Change from Scenario 1: ", sprintf("%.0f%%", percent_change))
  #     )
  # 
  #   p <- ggplot(diff_data, aes(x = difference_millions, y = label, text = hover_text)) +
  #     geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  #     geom_segment(aes(x = 0, xend = difference_millions, y = label, yend = label),
  #                  color = ifelse(diff_data$difference_millions >= 0, "#ED7D31", "#4472C4")) +
  #     geom_point(size = 4) +
  #     theme_minimal() +
  #     labs(x = paste("Change in Cost (", input$currency_option_plan, " in Millions)"), y = "") +
  #     theme(text = element_text(size = 12),
  #           panel.grid.major.y = element_blank(),
  #           panel.grid.minor.y = element_blank()) +
  #     scale_x_continuous(labels = scales::comma)
  # 
  #   ggplotly(p, tooltip = "text") %>%
  #     layout(hoverlabel = list(bgcolor = "white"))
  # })
  
  # Cost Difference Plot
  output$cost_difference_plot <- renderPlotly({
    cost_data <- prepare_cost_data()
    req(nrow(cost_data) > 0)
    
    baseline_cost <- cost_data$full_cost[cost_data$plan == "Scenario 1"]
    req(length(baseline_cost) > 0)
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
    
    diff_data <- cost_data %>%
      filter(plan != "Scenario 1") %>%
      mutate(
        difference_millions = round((full_cost - baseline_cost) / 1e6),
        percent_change = round((difference_millions * 1e6 / baseline_cost) * 100),
        label = paste(plan, "vs Scenario 1"), 
        hover_text = paste0("Difference: ", ifelse(difference_millions >= 0, "+", ""), currency_symbol,
                            format(difference_millions, big.mark = ","), "M<br>",
                            "Change from Scenario 1: ", sprintf("%.0f%%", percent_change))
        
      )
    
    p <- ggplot(diff_data, aes(x = difference_millions, y = label, text = hover_text)) +
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
    int_names = c("ITN Campaign",
                  "ITN Routine Distribution",
                  "LSM",
                  "IRS",
                  "Entomological surveillance",
                  "Malaria Vaccine",
                  "SMC",
                  "PMC", 
                  "IPTp",
                  "Public Sector CM",
                  "Private Sector CM",
                  "Global Fund Warehousing & Distribution Activities",
                  "Capacity Building",
                  "Governance & Coordination",
                  "Monitoring & Evaluation",
                  "Resource Mobilisation",
                  "Social Behaviour Change")
    
    cost_data <- 
      plan_comparison_costs %>%
      filter(plan == !!plan, currency == input$currency_option_plan) %>%
      select(intervention_type, title, state_count, lga_count, total_cost) %>%
      mutate(title = factor(title, levels = int_names)) %>%  # Set the custom order
      arrange(title)
    
    return(cost_data)
  }
  
  # COST TABLES 
  output$summary_data_tables <- renderUI({
    # Render the baseline table
    baseline_data <- filter_and_format_plan_data("Scenario 1")
    
    # Calculate the total cost for the baseline plan and format it
    total_cost_sum_baseline <- sum(baseline_data$total_cost, na.rm = TRUE)
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
    formatted_total_cost_baseline <- paste0(currency_symbol, format(total_cost_sum_baseline, big.mark = ","), "M")
    
    baseline_table <- column(6, 
                             h4(HTML(paste("Scenario 1 - Fully scaled-up plan. Total Cost:", 
                                           "<strong>", formatted_total_cost_baseline, "</strong>"))),
                             DT::datatable(
                               baseline_data |>
                                 filter(intervention_type == "Malaria Interventions") %>%
                                 mutate(
                                   total_cost = round(total_cost / 1e6, 0)  # Convert to millions and round to no decimals
                                 ),
                               options = list(pageLength = 20, scrollX = TRUE),
                               rownames = FALSE,
                               colnames = c(
                                 "Category" = "intervention_type",
                                 "Item" = "title",
                                 "State Count" = "state_count",
                                 "LGA Count" = "lga_count",
                                 "Total Cost (Millions)" = "total_cost"
                               )
                             )
    )
    
    # Generate tables for each selected plan
    selected_tables <- lapply(input$selected_plans, function(plan) {
      plan_data <- filter_and_format_plan_data(plan)
      plan_description <- unique(plan_comparison_costs$plan_description[plan_comparison_costs$plan == plan])
      total_cost_sum <- round(sum(plan_data$total_cost, na.rm = TRUE), 0)
      formatted_total_cost <- paste0(currency_symbol, format(total_cost_sum, big.mark = ","), "M")
      
      column(6, 
             h4(HTML(paste(plan, " - ", plan_description, ". Total Cost:",
                           "<strong>", formatted_total_cost, "</strong>"))),
             DT::datatable(
               plan_data |> filter(intervention_type == "Malaria Interventions") %>%
                 mutate(
                   total_cost = round(total_cost / 1e6, 0)  # Convert to millions and round to no decimals
                 ),
               options = list(pageLength = 20, scrollX = TRUE),
               rownames = FALSE,
               colnames = c(
                 "Category" = "intervention_type",
                 "Item" = "title",
                 "State Count" = "state_count",
                 "LGA Count" = "lga_count",
                 "Total Cost (Millions)" = "total_cost"
               )
             )
      )
    })
    
    # Organize tables into rows with two tables per row
    tables <- c(list(baseline_table), selected_tables)
    table_rows <- split(tables, ceiling(seq_along(tables) / 2))  # Split into groups of two
    rows <- lapply(table_rows, function(row_tables) {
      fluidRow(row_tables)  # Wrap each pair of tables in a fluidRow
    })
    
    # Combine all rows into a tagList
    do.call(tagList, rows)
  })
  
  # final plot as in the slides
  reactive_plot_data <- reactive({
    # Include Scenario 1 in selected plans
    selected_plans <- unique(c("Scenario 1", input$selected_plans))
    
    int_names = c("ITN Campaign",
                  "ITN Routine Distribution",
                  "LSM",
                  "IRS",
                  "Entomological surveillance",
                  "Malaria Vaccine",
                  "SMC",
                  "PMC", 
                  "IPTp",
                  "Public Sector CM",
                  "Private Sector CM",
                  "Global Fund Warehousing & Distribution Activities",
                  "Capacity Building",
                  "Governance & Coordination",
                  "Monitoring & Evaluation",
                  "Resource Mobilisation",
                  "Social Behaviour Change")
    
    # Determine the currency symbol
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"
    
    # Process data for the selected plans
    plan_comparison_costs %>%
      filter(plan %in% selected_plans, currency == input$currency_option_plan) %>%
      mutate(
        intervention = factor(title, levels = rev(int_names)),
        plan = str_to_title(plan),  # Capitalize plan names
        total_cost = total_cost / 1e6,  # Convert to millions
        tc_print = case_when(
          currency == "Naira"  ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
          currency == "USD" & total_cost > 60 ~ paste0(currency_symbol, format(round(total_cost, 0), big.mark = ","), "m"),
          currency == "USD" & total_cost <= 60 & total_cost > 1 ~ paste0(currency_symbol, format(round(total_cost, 1), big.mark = ","), "m"),
          currency == "USD" & total_cost <= 1 ~ paste0(currency_symbol, format(round(total_cost, 2), big.mark = ","), "m"),
          is.na(total_cost) ~ paste0(currency_symbol, "0m")
        ),
        total_cost = ifelse(is.na(total_cost), 0, total_cost)
      )
    
  })
  

  output$final_cost_plot <- renderPlot({
    dat <- reactive_plot_data()
    req(nrow(dat) > 0)  # Ensure there is data to plot
    
    currency_symbol <- if (input$currency_option_plan == "Naira") "₦" else "$"  # Get the selected currency symbol
    
    ggplot(dat) +
      geom_col(aes(x = total_cost, y = intervention, fill = plan)) +
      facet_wrap(vars(plan), ncol = 4, scales = "fixed") +  # Adjust facets dynamically
      scale_fill_manual("", values = c("#156082", "#a02b93", "#3b7d23", "#e97132")) +
      labs(
        y = "",
        x = paste0("Total Cost (in ", currency_symbol, " Million)")
      ) +
      # xlim(c(0, 420)) +
      geom_text(aes(x = total_cost + 10, y = intervention, label = tc_print),
                size = 6, hjust = 0) +
      theme_bw(16) +
      theme(
        strip.text = element_text(size = 10, face = "bold"),  # Style for facet labels
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        legend.position = "none"
      ) 
  })
  
}




#-RUN THE APPLICATION-----------------------------------------------------------
shinyApp(ui = ui, server = server)












