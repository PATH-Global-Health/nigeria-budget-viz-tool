#-------------------------------------------------------------------------------
# Helper functions to streamline code in app 
#-------------------------------------------------------------------------------


#-code to create intervention mix map 1 - with no interactivity-----------------
create_intervention_map_static <- function(lga_outline, state_outline, 
                                           country_outline, intervention_mix,
                                           center_lng = 9, center_lat = 4, 
                                           zoom = 5.2) {
  
  # Join intervention mix data with LGA outline
  lga_outline <- left_join(lga_outline, intervention_mix, by = c("state","lga"))
  
  # Define color palette based on unique values in intervention_summary
  color_pal <- colorFactor(
    palette = colorRampPalette(RColorBrewer::brewer.pal(15, "Paired"))(length(unique(intervention_mix$intervention_summary))),
    domain = unique(intervention_mix$intervention_summary)
  )
  
  leaflet() %>%
    addTiles() %>%
    # Add LGA polygons with color based on intervention_summary
    addPolygons(
      data = lga_outline,
      fillColor = ~color_pal(lga_outline$intervention_summary),
      color = "grey",
      weight = 1,
      fillOpacity = 0.9,
      layerId = ~lga,
      highlightOptions = highlightOptions(
        weight = 3, 
        color = "black",
        fillOpacity = 1, 
        bringToFront = TRUE
      ),
      label = ~sprintf(
        "<strong>%s</strong><br>State: %s<br>Intervention mix: %s",
        lga, state, intervention_summary
      ) %>% lapply(htmltools::HTML),
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "10px",
        style = list("font-weight" = "normal", "padding" = "3px 8px"),
        sticky = TRUE
      ) 
    ) %>%
    # Add state boundaries
    addPolygons(
      data = state_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) %>%
    # Add national boundaries
    addPolygons(
      data = country_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) %>%
    # Add legend (pass the values argument to color_pal)
    addLegend(
      pal = color_pal,
      values = lga_outline$intervention_summary,
      title = "Intervention Mix",
      position = "bottomright",
      opacity = 1
    ) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
}
#-interactive intervention map--------------------------------------------------
# Function to create the base interactive map
#-interactive intervention map--------------------------------------------------
# Function to create the base interactive map
create_base_interactive_map <- function(lga_outline, intervention_mix, 
                                        center_lng = 9, center_lat = 4, zoom = 5.2) {
  
  interactive_map <-
    left_join(lga_outline, intervention_mix) |> 
    mutate(
      unique_interventions = intervention_summary,
    ) |>
    separate_rows(
      unique_interventions, sep = "\\+ "
    ) |>
    mutate(unique_interventions = trimws(unique_interventions))
  
  unique_interventions <- sort(unique(interactive_map$unique_interventions))
  
  leaflet(interactive_map) %>%
    addTiles() %>%
    addPolygons(
      fillColor = "transparent", 
      color = "grey", 
      weight = 1,
      fillOpacity = 0.7, 
      layerId = ~lga
    ) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
}

# Function to create the custom legend JavaScript
# Updated Function to create the custom legend JavaScript with checkboxes
create_legend_js <- function(intervention_mix) {
  
  unique_interventions <- intervention_mix %>%
    mutate(unique_interventions = intervention_summary) %>%
    separate_rows(unique_interventions, sep = "\\+ ") %>%
    mutate(unique_interventions = trimws(unique_interventions))
  
  unique_interventions <- sort(unique(unique_interventions$unique_interventions))
  
  glue("
        function(el, x) {{
            var legend = L.control({{ position: 'bottomright' }});

            legend.onAdd = function(map) {{
                var div = L.DomUtil.create('div', 'info legend');
                var interventions = {jsonlite::toJSON(unique_interventions, auto_unbox = TRUE)};

                div.innerHTML += '<h4>Select Interventions</h4>';
                interventions.forEach(function(intervention) {{
                    div.innerHTML +=
                        '<input type=\"checkbox\" class=\"legend-checkbox\" value=\"' + intervention + '\">' +
                        '<label style=\"cursor: pointer;\">' + intervention + '</label><br>';
                }});

                return div;
            }};

            legend.addTo(this);

            var map = this;

            // Handle changes on the checkboxes
            $('.legend-checkbox').on('change', function() {{
                var selectedInterventions = [];
                $('.legend-checkbox:checked').each(function() {{
                    selectedInterventions.push($(this).val().trim());
                }});
                Shiny.setInputValue('selected_interventions', selectedInterventions, {{priority: 'event'}});
            }});
        }}")
}

# Function to update map based on selection
update_intervention_map <- function(map_id, highlighted_lgas, state_outline, country_outline,
                                    fill_color = "#1B7339", center_lng = 9, center_lat = 4, zoom = 5.2) {
  leafletProxy(map_id) %>%
    clearShapes() %>%
    addPolygons(
      data = highlighted_lgas,
      fillColor = fill_color,  
      color = "grey", 
      weight = 2,
      fillOpacity = 0.9, 
      layerId = ~lga
    ) %>%
    addPolygons(
      data = state_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) %>%
    addPolygons(
      data = country_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE)
    ) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
}

#-Prevalence map with interactivity---------------------------------------------
#-Prevalence map with interactivity---------------------------------------------
create_prevalence_map <- function(prevalence_data, lga_outline, state_outline, country_outline, 
                                  color_pal_prevalence, center_lng = 9, center_lat = 4, 
                                  zoom = 5.2) {
  
  prev_outline <- 
    lga_outline |> 
    left_join(prevalence_data)
  
  # Ensure years are properly extracted and valid
  years <- try(sort(unique(prevalence_data$year)))
  req(years)
  req(length(years) > 0)
  
  # Convert years to character
  years <- as.character(years)
  
  # Initialize base map
  base_map <- leaflet() %>%
    addTiles() %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
  
  # Add polygons for each year
  base_map <- add_year_layers(base_map, years, prev_outline, color_pal_prevalence)
  
  # Add boundaries (moved to after year layers but before controls)
  base_map <- add_boundaries(base_map, state_outline, country_outline)
  
  # Add controls and legend
  base_map %>%
    add_controls_and_legend(years, prev_outline, color_pal_prevalence)
}

# Helper function to add year layers
add_year_layers <- function(base_map, years, prevalence_data, color_pal_prevalence) {
  for(yr in years) {
    yr_num <- as.numeric(yr)
    
    year_data <- prevalence_data %>% 
      filter(year == yr_num) %>%
      filter(!is.na(prev_u5_state))
    
    if(nrow(year_data) > 0) {
      base_map <- base_map %>%
        addPolygons(
          data = year_data,
          fillColor = ~color_pal_prevalence()(prev_u5_state),
          color = "grey",
          weight = 1,
          fillOpacity = 1,
          layerId = ~paste(lga, yr),
          group = yr,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "black",
            fillOpacity = 1,
            bringToFront = TRUE
          ),
          label = ~sprintf(
            "<strong>%s</strong><br>State: %s<br>Under 5 prevalence (%s): %.0f%%",
            lga, state, yr, prev_u5_state
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            direction = "auto",
            textsize = "12px",
            style = list("font-weight" = "normal", "padding" = "3px 8px"),
            sticky = TRUE
          )
        )
    }
  }
  base_map
}

# Helper function to add boundaries
add_boundaries <- function(base_map, state_outline, country_outline) {
  base_map %>%
    addPolygons(
      data = state_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE),
      group = "boundaries"  # Add this line
    ) %>%
    addPolygons(
      data = country_outline,
      fillColor = "transparent",
      color = "black",
      weight = 2,
      options = pathOptions(interactive = FALSE),
      group = "boundaries"  # Add this line
    )
}

# Helper function to add controls and legend
add_controls_and_legend <- function(base_map, years, prevalence_data, color_pal_prevalence) {
  base_map %>%
    addLayersControl(
      baseGroups = years,
      overlayGroups = c("boundaries"),  # Add this line
      options = layersControlOptions(
        collapsed = FALSE,
        position = "topright"
      )
    ) %>%
    # Show boundaries by default
    showGroup("boundaries") %>%  # Add this line
    addLegend(
      pal = color_pal_prevalence(),
      values = prevalence_data$prev_u5,
      title = "DHS Under-5 Prevalence",
      position = "bottomright",
      opacity = 1,
      labFormat = labelFormat(suffix = "%")
    )
}

#' Create Icon Summary Cards
#' @param data A dataframe containing the required columns
#' @param currency_choice Either "USD" or "NGN"
create_icon_summaries <- function(data, currency_choice = "USD") {
  
  # Determine currency settings
  data <- 
    data |> 
    filter(currency == currency_choice)
  
  currency_symbol <- if (currency_choice == "USD") "$" else "₦"
  
  # Create the UI
  div(class = "info-section",
      div(class = "info-container",
          icon("users", class = "info-icon"),
          h4("Total Population", class = "info-box"),
          h4(formatC(data$pop_2025_projected, 
                     format = "d", big.mark = ","), class = "info-box")
      ),
      div(class = "info-container",
          icon("children", class = "info-icon"),
          h4("Under 5 Population", class = "info-box"),
          h4(formatC(data$pop_number_children_u5, 
                     format = "d", big.mark = ","), class = "info-box")
      ),
      div(class = "info-container",
          icon("person-pregnant", class = "info-icon"),
          h4("Pregnant Women Population", class = "info-box"),
          h4(formatC(data$pop_number_pw, 
                     format = "d", big.mark = ","), class = "info-box")
      ),
      div(class = "info-container",
          icon("map", class = "info-icon"),
          h4("Number of States", class = "info-box"),
          h4(formatC(data$admin_number_states, 
                     format = "d", big.mark = ","), class = "info-box")
      ),
      div(class = "info-container",
          icon("map", class = "info-icon"),
          h4("Number of LGAs", class = "info-box"),
          h4(formatC(data$admin_number_lgas, 
                     format = "d", big.mark = ","), class = "info-box")
      ),
      div(class = "info-container",
          icon("map", class = "info-icon"),
          h4("Number of Wards", class = "info-box"),
          h4(formatC(data$admin_number_wards, 
                     format = "d", big.mark = ","), class = "info-box")
      ),
      div(class = "final-info-container",
          icon("money-bill-wave", class = "info-icon"),
          h4("Total Cost", class = "info-box", style = "font-weight: bold;"),  
          h4(paste0(currency_symbol, 
                    formatC(as.numeric(data$full_cost), 
                            format = "f", digits = 0, big.mark = ",")), 
             class = "info-box", 
             style = "font-weight: bold;")  
      ), 
      div(class = "info-container",
          icon("coins", class = "info-icon"),
          h4("Cost per Person", class = "info-box"),
          h4(paste0(currency_symbol, 
                    formatC(as.numeric(data$full_cost) / data$pop_2025_projected, 
                            format = "f", digits = 2, big.mark = ",")), 
             class = "info-box")
      )
  )
}


#' Create Cost Summary Donut Chart
#' @param data A dataframe containing at least 'title', 'total_cost', and 'currency' columns
#' @param currency_choice The selected currency ("USD" or "NGN")
#' @return A billboarder chart object
create_cost_donut <- function(data, currency_choice) {
  # Validate inputs
  required_cols <- c("title", "total_cost", "currency")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create the donut chart
  billboarder() |> 
    bb_donutchart(
      data = data |> 
        filter(currency == currency_choice) |>
        arrange(desc(total_cost)) |>  
        select(title, total_cost)
    ) |>
    bb_title(text = " ", position = "left") |> 
    bb_legend(position = "right") |> 
    bb_tooltip() 
}

#' Create Intervention Treemap Plot
#' @param data Dataframe containing intervention_prop_breakdown data
#' @param currency_choice Selected currency ("USD" or "NGN")
create_treemap_plot <- function(data, currency_choice) {
  currency_symbol <- if(currency_choice == "USD") "$" else "₦"
  
  intervention_totals <- data %>%
    filter(currency == currency_choice) %>%
    group_by(intervention) %>%
    summarise(total_value = sum(value)) %>%
    arrange(desc(total_value))
  
  plot_ly(
    data = intervention_totals,
    type = "treemap",
    labels = ~intervention,
    parents = "",
    values = ~round(total_value,0),
    textinfo = "label+value",
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      "Total Cost: ", currency_symbol, "%{value:,.0f}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(
        text = "Overall Cost Distribution by Intervention",
        font = list(size = 16)
      ),
      font = list(size = 14)
    )
}

#' Create Stacked Bar Plot
#' @param data Dataframe containing intervention_prop_breakdown data
#' @param currency_choice Selected currency ("USD" or "NGN")
create_stacked_bar_plot <- function(data, currency_choice) {
  currency_symbol <- if(currency_choice == "USD") "$" else "₦"
  
  proc_impl_split <- data %>%
    filter(currency == currency_choice) %>%
    mutate(
      category = case_when(
        grepl("Procurement", full_name) ~ "Procurement",
        grepl("Distribution|Campaign|Operational|EQA|Storage", full_name) ~ "Implementation",
        TRUE ~ "Support"
      )
    ) %>%
    group_by(intervention, category) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    group_by(intervention) %>%
    mutate(total_intervention_cost = sum(value)) %>%
    ungroup() %>%
    mutate(intervention = reorder(intervention, -total_intervention_cost))
  
  plot_ly(data = proc_impl_split) %>%
    add_bars(
      x = ~intervention,
      y = ~value,
      color = ~category,
      text = ~scales::dollar(value, prefix = currency_symbol),
      hoverinfo = "text"
    ) %>%
    layout(
      title = list(
        text = "Cost Breakdown by Category",
        font = list(size = 16)
      ),
      barmode = 'stack',
      xaxis = list(
        title = "",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = paste0("Cost (", currency_choice, ")"),
        tickfont = list(size = 12)
      ),
      legend = list(font = list(size = 12)),
      font = list(size = 14)
    )
}

#' Create Lollipop Plot
#' @param data Dataframe containing intervention_prop_breakdown data
#' @param currency_choice Selected currency ("USD" or "NGN")
#' Create Lollipop Plot
#' @param data Dataframe containing intervention_prop_breakdown data
#' @param currency_choice Selected currency ("USD" or "NGN")
create_lollipop_plot <- function(data, currency_choice) {
  currency_symbol <- if (currency_choice == "USD") "$" else "₦"
  
  # Get top costs and include intervention information
  top_costs <- data %>%
    filter(currency == currency_choice) %>%
    group_by(full_name, intervention) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    arrange(desc(value)) %>%
    head(15)
  
  plot_ly() %>%
    add_segments(
      data = top_costs,
      x = 0,
      xend = ~value,
      y = ~full_name,
      yend = ~full_name,
      line = list(color = "gray"),
      showlegend = FALSE
    ) %>%
    add_markers(
      data = top_costs,
      x = ~value,
      y = ~full_name,
      color = ~intervention,
      colors = "Set1", 
      marker = list(size = 12),
      text = ~paste0(
        intervention, "<br>",
        "Cost: ", scales::dollar(value, prefix = currency_symbol)
      ),
      hoverinfo = "text"
    ) %>%
    layout(
      title = list(
        text = "Top 15 Specific Cost Components",
        font = list(size = 16)
      ),
      xaxis = list(
        title = paste0("Cost (", currency_choice, ")"),
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 12),
        automargin = TRUE
      ),
      legend = list(
        title = list(text = "Intervention"),
        font = list(size = 12),
        orientation = "h",  # Horizontal legend
        x = 0.5,            # Center the legend horizontally
        xanchor = "center", # Anchor the legend to the center
        y = -0.2            # Position the legend below the plot
      )
    )
}

#' Create Proportion Plot
#' @param data Dataframe containing intervention_prop_breakdown data
#' @param currency_choice Selected currency ("USD" or "NGN")
create_prop_plot <- function(data, currency_choice) {
  prop_split <- data %>%
    filter(currency == currency_choice) %>%
    mutate(
      category = case_when(
        grepl("Procurement", full_name) ~ "Procurement",
        grepl("Distribution|Campaign|Operational", full_name) ~ "Implementation",
        TRUE ~ "Support"
      )
    ) %>%
    group_by(intervention, category) %>%
    summarise(total = sum(value)) %>%
    group_by(intervention) %>%
    mutate(proportion = total / sum(total)) %>%
    filter(category != "Support")
  
  plot_ly(data = prop_split) %>%
    add_bars(
      x = ~intervention,
      y = ~proportion,
      color = ~category,
      colors = c("Procurement" = "#fc8d62", "Implementation" = "#66c2a5"),
      text = ~scales::percent(proportion, accuracy = 0.1),
      hoverinfo = "text"
    ) %>%
    layout(
      title = list(
        text = "Proportion of Procurement vs Implementation Costs",
        font = list(size = 16)
      ),
      barmode = 'stack',
      xaxis = list(
        title = "",
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Proportion of Total Cost",
        tickfont = list(size = 12),
        tickformat = ".0%"
      ),
      legend = list(font = list(size = 12)),
      font = list(size = 14)
    )
}

#' Create color palette for maps
#' @param values Numeric vector of values to create palette for
create_map_palette <- function(values) {
  colorNumeric(
    palette = "RdBu",
    domain = values, 
    reverse = TRUE
  )
}

#' Format cost values for map labels
#' @param value Numeric cost value
#' @param currency_option Currency choice ("USD" or "NGN")
#' @param is_per_person Boolean indicating if value is per person
format_cost_label <- function(value, currency_option, is_per_person = FALSE) {
  currency_symbol <- if(currency_option == "USD") "$" else "₦"
  
  if(is_per_person) {
    paste0(currency_symbol, round(value, 2))
  } else {
    paste0(currency_symbol, format(round(value), big.mark = ","))
  }
}

#' Create Nigeria state-level cost map
#' @param data SF object with state polygons and cost data
#' @param map_type Type of map ("total" or "per_person")
#' @param currency_option Selected currency ("USD" or "NGN")
create_nigeria_cost_map <- function(data, map_type = "total", currency_option = "USD") {
  # Determine which values to map
  values <- if(map_type == "total") data$full_cost else data$cost_per_person
  title <- if(map_type == "total") "Total Cost" else "Cost per Person"
  
  # Create color palette
  pal <- create_map_palette(values)
  
  # Create map
  leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(values),
      weight = 2,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 1,
      label = ~paste0(
        state, ": ",
        format_cost_label(
          if(map_type == "total") full_cost else cost_per_person,
          currency_option,
          is_per_person = map_type == "per_person"
        )
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = values,
      title = paste(title, "(", currency_option, ")"),
      labFormat = labelFormat(
        prefix = if(currency_option == "USD") "$" else "₦"
      )
    )
}

# Helper: Highlight a single state on the map
highlight_state <- function(map_id, state_outline) {
  leafletProxy(map_id) %>%
    clearGroup("highlight") %>%
    addPolylines(
      data = state_outline,
      color = "red", 
      weight = 3, 
      opacity = 1, 
      group = "highlight"
    )
}

highlight_lga <- function(map_id, lga_outline) {
  leafletProxy(map_id) %>%
    clearGroup("highlight") %>%
    addPolylines(
      data = lga_outline,
      color = "red", 
      weight = 3, 
      opacity = 1, 
      group = "highlight"
    )
}

plan_colors <- function(plans) {
  # Generate a set of distinct colors for the plan names
  palette <- brewer.pal(min(length(plans), 9), "Set1")  # Use Set1 palette for up to 9 colors
  color_map <- setNames(palette[seq_along(plans)], plans)
  return(color_map)
}

# Helper Function to Sum Costs Across Interventions per Plan
# sum_costs_per_plan <- function(data, currency) {
#   data %>%
#     filter(currency == currency) %>%
#     group_by(plan) %>%
#     summarise(full_cost = sum(total_cost, na.rm = TRUE))
# }
# 
# # Helper Function to Create Cost Comparison Plot
# create_cost_comparison_plot <- function(data, selected_plans, currency_symbol) {
#   # Filter to include only baseline and selected plans
#   plot_data <- data %>%
#     filter(plan == "Baseline" | plan %in% selected_plans)
#   
#   p <- ggplot(plot_data, aes(x = plan, y = full_cost,
#                              text = paste0(
#                                "Plan: ", plan, "<br>",
#                                "Cost: ", currency_symbol, 
#                                format(full_cost, big.mark = ",")
#                              ))) +
#     geom_bar(stat = "identity", fill = c("#4472C4", rep("#ED7D31", length(selected_plans))), width = 0.6) +
#     geom_text(aes(label = paste0(currency_symbol, format(full_cost, big.mark = ","))),
#               vjust = -0.5) +
#     theme_minimal() +
#     labs(y = paste("Total Cost (", currency_symbol, ")"), x = "") +
#     theme(text = element_text(size = 12)) +
#     scale_y_continuous(labels = scales::comma)
#   
#   ggplotly(p, tooltip = "text") %>%
#     layout(hoverlabel = list(bgcolor = "white"))
# }
# 
# # Helper Function to Create Cost Difference Plot
# create_cost_difference_plot <- function(data, selected_plans, currency_symbol) {
#   baseline_cost <- data$full_cost[data$plan == "Baseline"]
#   if (length(baseline_cost) == 0) return(NULL)
#   
#   # Calculate cost differences for each selected plan
#   diff_data <- data %>%
#     filter(plan %in% selected_plans) %>%
#     mutate(
#       Value = full_cost - baseline_cost,
#       PercentChange = (Value / baseline_cost) * 100,
#       Label = paste(plan, "vs Baseline")
#     )
#   
#   p <- ggplot(diff_data, aes(x = Value, y = Label,
#                              text = paste0(
#                                "Difference: ", ifelse(Value >= 0, "+", ""), currency_symbol,
#                                format(Value, big.mark = ","), "<br>",
#                                "Change from Baseline: ", sprintf("%.1f%%", PercentChange)
#                              ))) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
#     geom_segment(aes(x = 0, xend = Value, y = Label, yend = Label),
#                  color = ifelse(diff_data$Value >= 0, "#ED7D31", "#4472C4")) +
#     geom_point(size = 4, color = ifelse(diff_data$Value >= 0, "#ED7D31", "#4472C4")) +
#     theme_minimal() +
#     labs(x = paste("Change in Cost (", currency_symbol, ")"), y = "") +
#     theme(text = element_text(size = 12),
#           panel.grid.major.y = element_blank(),
#           panel.grid.minor.y = element_blank()) +
#     scale_x_continuous(labels = scales::comma)
#   
#   ggplotly(p, tooltip = "text") %>%
#     layout(hoverlabel = list(bgcolor = "white"))
# }