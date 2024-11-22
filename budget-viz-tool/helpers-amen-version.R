#-------------------------------------------------------------------------------
# Helper functions to streamline code in app 
#-------------------------------------------------------------------------------


#-code to create intervention mix map 1 - with no interactivity-----------------
# Function to create static intervention map with a toggleable legend
create_intervention_map_static <- function(lga_outline, state_outline, 
                                           country_outline, intervention_mix,
                                           center_lng = 9, center_lat = 4, 
                                           zoom = 5.2) {
  
  # Define color palette based on unique values in intervention_summary
  color_pal <- colorFactor(
    palette = colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(intervention_mix$intervention_summary))),
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
      opacity = 0.7
    ) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
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
  
  intervention_totals <- 
    data %>%
    filter(currency == currency_choice) %>%
    arrange(desc(total_cost)) %>%
    mutate(
      formatted_cost = paste0(currency_symbol, format(round(total_cost, 0), big.mark = ",", scientific = FALSE))
    )
  
  plot_ly(
    data = intervention_totals,
    type = "treemap",
    labels = ~paste(title, "<br>", formatted_cost), 
    parents = "",
    values = ~total_cost,  # Keep numeric values for rendering
    textinfo = "label",  # Display only custom label
    hovertemplate = paste(
      "<b>%{label}</b><br>",
      
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
                "Capacity Building",
                "Governance & Coordination",
                "Monitoring & Evaluation",
                "Resource Mobilisation",
                "Social Behaviour Change")
  
  proc_impl_split <-
    data %>%
    filter(currency == currency_choice) |> 
    mutate(title = factor(title, levels = int_names)) %>%  # Set the custom order
    arrange(title)
  
  plot_ly(data = proc_impl_split) %>%
    add_bars(
      x = ~title,
      y = ~total_cost,
      color = ~intervention_type,
      text = ~scales::dollar(total_cost, prefix = currency_symbol),
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
