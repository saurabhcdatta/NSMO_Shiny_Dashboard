# server.R - Server Logic
# This file contains all reactive expressions and outputs

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE DATA FILTERS
  # =============================================================================
  
  # Executive Summary filtered data
  summary_data <- reactive({
    data_filtered <- df
    if (!is.null(input$sum_fico) && length(input$sum_fico) > 0) {
      data_filtered <- data_filtered %>% filter(FICO_category %in% input$sum_fico)
    }
    if (!is.null(input$sum_period) && length(input$sum_period) > 0) {
      data_filtered <- data_filtered %>% filter(COVID_period %in% input$sum_period)
    }
    if (!is.null(input$sum_first_mort) && length(input$sum_first_mort) > 0) {
      data_filtered <- data_filtered %>% filter(First_Mortgage_Label %in% input$sum_first_mort)
    }
    if (!is.null(input$sum_loan_type) && length(input$sum_loan_type) > 0) {
      data_filtered <- data_filtered %>% filter(Loan_Type %in% input$sum_loan_type)
    }
    if (!is.null(input$sum_term) && length(input$sum_term) > 0) {
      data_filtered <- data_filtered %>% filter(Term_Label %in% input$sum_term)
    }
    if (!is.null(input$sum_cashout) && length(input$sum_cashout) > 0) {
      data_filtered <- data_filtered %>% filter(Cashout_Label %in% input$sum_cashout)
    }
    if (!is.null(input$sum_race) && length(input$sum_race) > 0) {
      data_filtered <- data_filtered %>% filter(Race %in% input$sum_race)
    }
    if (!is.null(input$sum_gender) && length(input$sum_gender) > 0) {
      data_filtered <- data_filtered %>% filter(Sex_Label %in% input$sum_gender)
    }
    if (!is.null(input$sum_jumbo) && length(input$sum_jumbo) > 0) {
      data_filtered <- data_filtered %>% filter(Jumbo_Label %in% input$sum_jumbo)
    }
    data_filtered
  })
  
  # Survey Responses filtered data
  response_data <- reactive({
    data_filtered <- df
    if (input$resp_fico != "All") data_filtered <- data_filtered %>% filter(FICO_category == input$resp_fico)
    if (input$resp_period != "All") data_filtered <- data_filtered %>% filter(COVID_period == input$resp_period)
    if (input$resp_first_mort != "All") data_filtered <- data_filtered %>% filter(First_Mortgage_Label == input$resp_first_mort)
    if (input$resp_sex != "All") data_filtered <- data_filtered %>% filter(Sex_Label == input$resp_sex)
    if (input$resp_veteran != "All") data_filtered <- data_filtered %>% filter(Veteran == input$resp_veteran)
    data_filtered
  })
  
  # Loan Characteristics filtered data
  loan_data <- reactive({
    data_filtered <- df
    if (input$loan_fico != "All") data_filtered <- data_filtered %>% filter(FICO_category == input$loan_fico)
    if (input$loan_period != "All") data_filtered <- data_filtered %>% filter(COVID_period == input$loan_period)
    if (input$loan_type_filter != "All") data_filtered <- data_filtered %>% filter(Loan_Type == input$loan_type_filter)
    if (input$loan_gse != "All") data_filtered <- data_filtered %>% filter(GSE_Label == input$loan_gse)
    if (input$loan_term != "All") data_filtered <- data_filtered %>% filter(Term_Label == input$loan_term)
    if (input$loan_cashout != "All") data_filtered <- data_filtered %>% filter(Cashout_Label == input$loan_cashout)
    data_filtered
  })
  
  # Demographics filtered data
  demo_data <- reactive({
    data_filtered <- df
    if (input$demo_period != "All") data_filtered <- data_filtered %>% filter(COVID_period == input$demo_period)
    if (input$demo_race != "All") data_filtered <- data_filtered %>% filter(Race == input$demo_race)
    if (input$demo_work != "All") data_filtered <- data_filtered %>% filter(Work_Status == input$demo_work)
    if (input$demo_metro != "All") data_filtered <- data_filtered %>% filter(Metro == input$demo_metro)
    data_filtered
  })
  
  # Performance filtered data
  perf_data <- reactive({
    data_filtered <- df
    if (input$perf_period != "All") data_filtered <- data_filtered %>% filter(COVID_period == input$perf_period)
    if (input$perf_fico != "All") data_filtered <- data_filtered %>% filter(FICO_category == input$perf_fico)
    if (input$perf_forbear != "All") data_filtered <- data_filtered %>% filter(Forbearance == input$perf_forbear)
    if (input$perf_jumbo != "All") data_filtered <- data_filtered %>% filter(Jumbo_Label == input$perf_jumbo)
    data_filtered
  })
  
  # Explorer filtered data
  explorer_data <- reactive({
    data_filtered <- df
    if (!is.null(input$exp_years) && length(input$exp_years) > 0) {
      data_filtered <- data_filtered %>% filter(open_year %in% input$exp_years)
    }
    if (input$exp_performance != "All") data_filtered <- data_filtered %>% filter(Performance == input$exp_performance)
    if (input$exp_education != "All") data_filtered <- data_filtered %>% filter(Education_Label == input$exp_education)
    if (input$exp_age != "All") data_filtered <- data_filtered %>% filter(Age_Category == input$exp_age)
    if (input$exp_ltv != "All") data_filtered <- data_filtered %>% filter(LTV_Category == input$exp_ltv)
    if (input$exp_dti != "All") data_filtered <- data_filtered %>% filter(DTI_Category == input$exp_dti)
    data_filtered
  })
  
  # =============================================================================
  # TAB 1: EXECUTIVE SUMMARY
  # =============================================================================
  
  # Helper function for t-test significance on means
  test_mean_significance <- function(data, var, group_var, weight_var = "analysis_weight") {
    tryCatch({
      cu_data <- data %>% filter(get(group_var) == "Credit Union", !is.na(get(var)))
      noncu_data <- data %>% filter(get(group_var) == "Non-Credit Union", !is.na(get(var)))
      
      if (nrow(cu_data) < 10 || nrow(noncu_data) < 10) return(FALSE)
      
      # Weighted means
      cu_mean <- weighted.mean(cu_data[[var]], cu_data[[weight_var]], na.rm = TRUE)
      noncu_mean <- weighted.mean(noncu_data[[var]], noncu_data[[weight_var]], na.rm = TRUE)
      
      # Simple t-test on values (not perfect for weighted, but gives indication)
      test_result <- t.test(cu_data[[var]], noncu_data[[var]])
      return(test_result$p.value < 0.05)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  output$sum_total_loans_cu <- renderValueBox({
    cu_count <- sum(summary_data()$Institution_Type == "Credit Union")
    valueBox(format(cu_count, big.mark = ","), "CU Total Loans",
             icon = icon("home"), color = "blue")
  })
  
  output$sum_total_loans_noncu <- renderValueBox({
    noncu_count <- sum(summary_data()$Institution_Type == "Non-Credit Union")
    valueBox(format(noncu_count, big.mark = ","), "Non-CU Total Loans",
             icon = icon("home"), color = "red")
  })
  
  output$sum_rate_spread_cu <- renderValueBox({
    cu_data <- summary_data() %>% filter(Institution_Type == "Credit Union")
    avg <- weighted.mean(cu_data$Rate_Spread_Pct, cu_data$analysis_weight, na.rm = TRUE)
    sig <- test_mean_significance(summary_data(), "Rate_Spread_Pct", "Institution_Type")
    sig_star <- ifelse(sig, "*", "")
    valueBox(paste0(round(avg, 2), "%", sig_star), 
             "CU Avg Rate Spread",
             icon = icon("percent"), color = "blue")
  })
  
  output$sum_rate_spread_noncu <- renderValueBox({
    noncu_data <- summary_data() %>% filter(Institution_Type == "Non-Credit Union")
    avg <- weighted.mean(noncu_data$Rate_Spread_Pct, noncu_data$analysis_weight, na.rm = TRUE)
    valueBox(paste0(round(avg, 2), "%"), 
             "Non-CU Avg Rate Spread",
             icon = icon("percent"), color = "red")
  })
  
  output$sum_median_amt_cat_cu <- renderValueBox({
    cu_data <- summary_data() %>% filter(Institution_Type == "Credit Union", !is.na(loan_amount_cat))
    
    if(nrow(cu_data) > 0) {
      median_cat <- median(cu_data$loan_amount_cat, na.rm = TRUE)
      
      # Map median category to range
      range_label <- case_when(
        median_cat == 1 ~ "<$50K",
        median_cat == 2 ~ "$50-99K",
        median_cat == 3 ~ "$100-149K",
        median_cat == 4 ~ "$150-199K",
        median_cat == 5 ~ "$200-249K",
        median_cat == 6 ~ "$250-299K",
        median_cat == 7 ~ "$300-349K",
        median_cat == 8 ~ "$350-399K",
        median_cat == 9 ~ "$400K+",
        TRUE ~ "N/A"
      )
    } else {
      range_label <- "N/A"
    }
    
    valueBox(range_label, 
             "CU Median Loan Amount",
             icon = icon("dollar-sign"), color = "blue")
  })
  
  output$sum_median_amt_cat_noncu <- renderValueBox({
    noncu_data <- summary_data() %>% filter(Institution_Type == "Non-Credit Union", !is.na(loan_amount_cat))
    
    if(nrow(noncu_data) > 0) {
      median_cat <- median(noncu_data$loan_amount_cat, na.rm = TRUE)
      
      # Map median category to range
      range_label <- case_when(
        median_cat == 1 ~ "<$50K",
        median_cat == 2 ~ "$50-99K",
        median_cat == 3 ~ "$100-149K",
        median_cat == 4 ~ "$150-199K",
        median_cat == 5 ~ "$200-249K",
        median_cat == 6 ~ "$250-299K",
        median_cat == 7 ~ "$300-349K",
        median_cat == 8 ~ "$350-399K",
        median_cat == 9 ~ "$400K+",
        TRUE ~ "N/A"
      )
    } else {
      range_label <- "N/A"
    }
    
    valueBox(range_label, 
             "Non-CU Median Loan Amount",
             icon = icon("dollar-sign"), color = "red")
  })
  
  output$sum_avg_fico_cu <- renderInfoBox({
    cu_data <- summary_data() %>% filter(Institution_Type == "Credit Union")
    avg <- weighted.mean(cu_data$Score_Origin, cu_data$analysis_weight, na.rm = TRUE)
    sig <- test_mean_significance(summary_data(), "Score_Origin", "Institution_Type")
    sig_star <- ifelse(sig, "*", "")
    infoBox("CU Avg FICO", paste0(round(avg, 0), sig_star), 
            icon = icon("chart-line"), color = "blue", fill = TRUE)
  })
  
  output$sum_avg_fico_noncu <- renderInfoBox({
    noncu_data <- summary_data() %>% filter(Institution_Type == "Non-Credit Union")
    avg <- weighted.mean(noncu_data$Score_Origin, noncu_data$analysis_weight, na.rm = TRUE)
    infoBox("Non-CU Avg FICO", round(avg, 0), 
            icon = icon("chart-line"), color = "red", fill = TRUE)
  })
  
  output$sum_avg_ltv_cu <- renderInfoBox({
    cu_data <- summary_data() %>% filter(Institution_Type == "Credit Union")
    avg <- weighted.mean(cu_data$ltv, cu_data$analysis_weight, na.rm = TRUE)
    sig <- test_mean_significance(summary_data(), "ltv", "Institution_Type")
    sig_star <- ifelse(sig, "*", "")
    infoBox("CU Avg LTV", paste0(round(avg, 1), "%", sig_star), 
            icon = icon("home"), color = "light-blue")
  })
  
  output$sum_avg_ltv_noncu <- renderInfoBox({
    noncu_data <- summary_data() %>% filter(Institution_Type == "Non-Credit Union")
    avg <- weighted.mean(noncu_data$ltv, noncu_data$analysis_weight, na.rm = TRUE)
    infoBox("Non-CU Avg LTV", paste0(round(avg, 1), "%"), 
            icon = icon("home"), color = "orange")
  })
  
  output$sum_metrics_compare <- renderPlotly({
    # Calculate metrics with significance testing
    cu_data <- summary_data() %>% filter(Institution_Type == "Credit Union")
    noncu_data <- summary_data() %>% filter(Institution_Type == "Non-Credit Union")
    
    # Calculate each metric (removed Rate Spread and Interest Rate)
    metrics_df <- data.frame(
      Institution_Type = rep(c("Credit Union", "Non-Credit Union"), 2),
      Metric = rep(c("Avg DTI (%)", "Avg LTV (%)"), each = 2),
      Value = c(
        weighted.mean(cu_data$dti, cu_data$analysis_weight, na.rm = TRUE),
        weighted.mean(noncu_data$dti, noncu_data$analysis_weight, na.rm = TRUE),
        weighted.mean(cu_data$ltv, cu_data$analysis_weight, na.rm = TRUE),
        weighted.mean(noncu_data$ltv, noncu_data$analysis_weight, na.rm = TRUE)
      )
    )
    
    # Test significance for each metric
    sig_dti <- test_mean_significance(summary_data(), "dti", "Institution_Type")
    sig_ltv <- test_mean_significance(summary_data(), "ltv", "Institution_Type")
    
    # Add significance stars
    metrics_df$sig_star <- ""
    metrics_df$sig_star[metrics_df$Metric == "Avg DTI (%)" & metrics_df$Institution_Type == "Credit Union" & sig_dti] <- "*"
    metrics_df$sig_star[metrics_df$Metric == "Avg LTV (%)" & metrics_df$Institution_Type == "Credit Union" & sig_ltv] <- "*"
    
    # Create plot with legend at bottom
    p <- ggplot(metrics_df, aes(x = Metric, y = Value, fill = Institution_Type)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(aes(label = paste0(round(Value, 2), sig_star)), 
                position = position_dodge(width = 0.7), 
                vjust = -0.5, size = 4, fontface = "bold") +
      theme_minimal() +
      scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
      labs(x = "", y = "", 
           title = "Key Weighted Metrics Comparison (* = Statistically Significant)",
           fill = "") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 12, face = "bold"),
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 10)) +
      ylim(0, max(metrics_df$Value, na.rm = TRUE) * 1.25)
    
    ggplotly(p) %>% 
      layout(margin = list(b = 100, t = 40, l = 50, r = 50),
             legend = list(orientation = "h", x = 0.35, y = -0.2))
  })
  
  # Sex distribution pie charts
  output$sum_sex_pie <- renderPlotly({
    sex_data <- summary_data() %>%
      filter(Sex_Label %in% c("Male", "Female")) %>%
      group_by(Institution_Type, Sex_Label) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100)
    
    # Create two pie charts side by side
    cu_data <- sex_data %>% filter(Institution_Type == "Credit Union")
    noncu_data <- sex_data %>% filter(Institution_Type == "Non-Credit Union")
    
    plot_ly() %>%
      add_pie(data = cu_data, labels = ~Sex_Label, values = ~percentage,
              name = "Credit Union", domain = list(x = c(0, 0.48), y = c(0, 1)),
              marker = list(colors = c('#3498db', '#85c1e9')),
              textinfo = 'label+percent', textposition = 'inside',
              hoverinfo = 'text',
              text = ~paste(Sex_Label, '<br>', round(percentage, 1), '%')) %>%
      add_pie(data = noncu_data, labels = ~Sex_Label, values = ~percentage,
              name = "Non-Credit Union", domain = list(x = c(0.52, 1), y = c(0, 1)),
              marker = list(colors = c('#e74c3c', '#ec7063')),
              textinfo = 'label+percent', textposition = 'inside',
              hoverinfo = 'text',
              text = ~paste(Sex_Label, '<br>', round(percentage, 1), '%')) %>%
      layout(title = "Sex Distribution: CU (Left) vs Non-CU (Right)",
             showlegend = FALSE,
             annotations = list(
               list(x = 0.24, y = -0.1, text = "Credit Union", showarrow = FALSE, xref='paper', yref='paper'),
               list(x = 0.76, y = -0.1, text = "Non-Credit Union", showarrow = FALSE, xref='paper', yref='paper')
             ))
  })
  
  # FICO distribution donut charts
  output$sum_fico_heatmap_cu <- renderPlotly({
    # Define FICO order: Very Poor to Excellent
    fico_display_order <- c("Poor (<580)", "Fair (580-669)", "Good (670-739)", 
                           "Very Good (740-799)", "Exceptional (800+)")
    
    cu_data <- summary_data() %>%
      filter(Institution_Type == "Credit Union", FICO_category %in% fico_order) %>%
      group_by(FICO_category) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        percentage = weighted_n / sum(weighted_n) * 100,
        FICO_category = factor(FICO_category, levels = fico_display_order)
      ) %>%
      arrange(FICO_category)
    
    # Define colors: Very Poor=pinkish, Poor=purple, Fair=magenta, Good=blue, Excellent=dark blue
    fico_colors <- c(
      "Poor (<580)" = "#E8A0BF",              # Pinkish (Very Poor)
      "Fair (580-669)" = "#9B59B6",           # Purple (Poor)
      "Good (670-739)" = "#D946A6",           # Magenta (Fair)
      "Very Good (740-799)" = "#5DADE2",      # Blue (Good)
      "Exceptional (800+)" = "#2C3E50"        # Dark Blue (Excellent)
    )
    
    plot_ly(cu_data, 
            labels = ~FICO_category, 
            values = ~percentage,
            type = "pie",
            hole = 0.5,
            marker = list(colors = fico_colors[cu_data$FICO_category]),
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = 'white', size = 14, family = "Arial", weight = "bold"),
            hovertemplate = paste('<b>%{label}</b><br>',
                                 '%{percent}<br>',
                                 '<extra></extra>'),
            sort = FALSE) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          x = 0.5, 
          y = -0.15,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 10)
        ),
        annotations = list(
          list(
            text = "<b>Credit<br>Union<br>FICO</b>",
            x = 0.5, y = 0.5,
            font = list(size = 13, color = "#2c3e50", family = "Arial", weight = "bold"),
            showarrow = FALSE
          )
        ),
        margin = list(l = 20, r = 20, t = 20, b = 80)
      )
  })
  
  output$sum_fico_heatmap_noncu <- renderPlotly({
    # Define FICO order: Very Poor to Excellent
    fico_display_order <- c("Poor (<580)", "Fair (580-669)", "Good (670-739)", 
                           "Very Good (740-799)", "Exceptional (800+)")
    
    noncu_data <- summary_data() %>%
      filter(Institution_Type == "Non-Credit Union", FICO_category %in% fico_order) %>%
      group_by(FICO_category) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        percentage = weighted_n / sum(weighted_n) * 100,
        FICO_category = factor(FICO_category, levels = fico_display_order)
      ) %>%
      arrange(FICO_category)
    
    # Define colors: Very Poor=pinkish, Poor=purple, Fair=magenta, Good=blue, Excellent=dark blue
    fico_colors <- c(
      "Poor (<580)" = "#E8A0BF",              # Pinkish (Very Poor)
      "Fair (580-669)" = "#9B59B6",           # Purple (Poor)
      "Good (670-739)" = "#D946A6",           # Magenta (Fair)
      "Very Good (740-799)" = "#5DADE2",      # Blue (Good)
      "Exceptional (800+)" = "#2C3E50"        # Dark Blue (Excellent)
    )
    
    plot_ly(noncu_data, 
            labels = ~FICO_category, 
            values = ~percentage,
            type = "pie",
            hole = 0.5,
            marker = list(colors = fico_colors[noncu_data$FICO_category]),
            textposition = 'inside',
            textinfo = 'percent',
            insidetextfont = list(color = 'white', size = 14, family = "Arial", weight = "bold"),
            hovertemplate = paste('<b>%{label}</b><br>',
                                 '%{percent}<br>',
                                 '<extra></extra>'),
            sort = FALSE) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          x = 0.5, 
          y = -0.15,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 10)
        ),
        annotations = list(
          list(
            text = "<b>Non-CU<br>FICO</b>",
            x = 0.5, y = 0.5,
            font = list(size = 13, color = "#2c3e50", family = "Arial", weight = "bold"),
            showarrow = FALSE
          )
        ),
        margin = list(l = 20, r = 20, t = 20, b = 80)
      )
  })
  
  # Race composition donut charts
  output$sum_race_donut <- renderPlotly({
    race_data <- summary_data() %>%
      filter(Race %in% c("White", "Black", "Asian", "Other")) %>%
      group_by(Institution_Type, Race) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100)
    
    # Create two donut charts side by side
    cu_data <- race_data %>% filter(Institution_Type == "Credit Union")
    noncu_data <- race_data %>% filter(Institution_Type == "Non-Credit Union")
    
    # Define colors for races
    race_colors <- c("White" = "#3498db", "Black" = "#e74c3c", 
                     "Asian" = "#f39c12", "Other" = "#95a5a6")
    
    plot_ly() %>%
      add_pie(data = cu_data, labels = ~Race, values = ~percentage,
              name = "Credit Union", domain = list(x = c(0, 0.48), y = c(0, 1)),
              marker = list(colors = race_colors[cu_data$Race]),
              textinfo = 'label+percent', textposition = 'inside',
              hole = 0.4,
              hoverinfo = 'text',
              text = ~paste(Race, '<br>', round(percentage, 1), '%')) %>%
      add_pie(data = noncu_data, labels = ~Race, values = ~percentage,
              name = "Non-Credit Union", domain = list(x = c(0.52, 1), y = c(0, 1)),
              marker = list(colors = race_colors[noncu_data$Race]),
              textinfo = 'label+percent', textposition = 'inside',
              hole = 0.4,
              hoverinfo = 'text',
              text = ~paste(Race, '<br>', round(percentage, 1), '%')) %>%
      layout(title = list(text = "Race Composition: CU (Left) vs Non-CU (Right)", 
                         font = list(size = 14, face = "bold")),
             showlegend = TRUE,
             legend = list(orientation = "h", x = 0.2, y = -0.1),
             annotations = list(
               list(x = 0.24, y = 0.5, text = "<b>Credit<br>Union</b>", 
                    showarrow = FALSE, xref='paper', yref='paper', font = list(size = 12)),
               list(x = 0.76, y = 0.5, text = "<b>Non-Credit<br>Union</b>", 
                    showarrow = FALSE, xref='paper', yref='paper', font = list(size = 12))
             ))
  })
  
  # Performance metrics
  output$sum_performance_metrics <- renderPlotly({
    # Calculate forbearance rate
    forbear_data <- summary_data() %>%
      group_by(Institution_Type, Forbearance) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(Forbearance == "Yes") %>%
      select(Institution_Type, Forbearance_Rate = percentage)
    
    # Calculate 60-day delinquency rate
    delinq_60_data <- summary_data() %>%
      filter(Performance != "Unknown") %>%
      mutate(Delinq_60 = ifelse(Performance == "60 Days Delinquent", "Yes", "No")) %>%
      group_by(Institution_Type, Delinq_60) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(Delinq_60 == "Yes") %>%
      select(Institution_Type, Delinq_60_Rate = percentage)
    
    # Calculate bankruptcy rate
    bankruptcy_data <- summary_data() %>%
      filter(Performance != "Unknown") %>%
      mutate(Bankruptcy = ifelse(Performance == "Foreclosure/Bankruptcy", "Yes", "No")) %>%
      group_by(Institution_Type, Bankruptcy) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(Bankruptcy == "Yes") %>%
      select(Institution_Type, Bankruptcy_Rate = percentage)
    
    # Ensure we have both institution types
    all_institutions <- data.frame(Institution_Type = c("Credit Union", "Non-Credit Union"))
    
    # Combine all metrics with proper handling of missing data
    perf_data <- all_institutions %>%
      left_join(forbear_data, by = "Institution_Type") %>%
      left_join(delinq_60_data, by = "Institution_Type") %>%
      left_join(bankruptcy_data, by = "Institution_Type") %>%
      replace_na(list(Forbearance_Rate = 0, Delinq_60_Rate = 0, Bankruptcy_Rate = 0)) %>%
      pivot_longer(cols = -Institution_Type, names_to = "Metric", values_to = "Percentage") %>%
      mutate(Metric = case_when(
        Metric == "Forbearance_Rate" ~ "% in Forbearance",
        Metric == "Delinq_60_Rate" ~ "% 60 Day Delinquent",
        Metric == "Bankruptcy_Rate" ~ "% Foreclosure/Bankruptcy"
      ))
    
    # Create plot with legend at bottom
    p <- ggplot(perf_data, aes(x = Metric, y = Percentage, fill = Institution_Type)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(aes(label = paste0(round(Percentage, 2), "%")), 
                position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5, fontface = "bold") +
      theme_minimal() +
      scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
      labs(x = "", y = "", 
           title = "Performance Metrics: CU vs Non-CU",
           fill = "") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
            axis.text.y = element_text(size = 9),
            plot.title = element_text(size = 12, face = "bold"),
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 10)) +
      ylim(0, max(c(perf_data$Percentage, 1), na.rm = TRUE) * 1.35)
    
    ggplotly(p) %>% 
      layout(margin = list(b = 120, t = 40, l = 50, r = 50),
             legend = list(orientation = "h", x = 0.35, y = -0.25))
  })
  
  # =============================================================================
  # TAB 2: TREND ANALYSIS
  # =============================================================================
  
  # Trend Analysis filtered data
  trend_data <- reactive({
    data_filtered <- df
    if (!is.null(input$trend_fico) && length(input$trend_fico) > 0) {
      data_filtered <- data_filtered %>% filter(FICO_category %in% input$trend_fico)
    }
    if (!is.null(input$trend_period) && length(input$trend_period) > 0) {
      data_filtered <- data_filtered %>% filter(COVID_period %in% input$trend_period)
    }
    if (!is.null(input$trend_first_mort) && length(input$trend_first_mort) > 0) {
      data_filtered <- data_filtered %>% filter(First_Mortgage_Label %in% input$trend_first_mort)
    }
    if (!is.null(input$trend_loan_type) && length(input$trend_loan_type) > 0) {
      data_filtered <- data_filtered %>% filter(Loan_Type %in% input$trend_loan_type)
    }
    if (!is.null(input$trend_term) && length(input$trend_term) > 0) {
      data_filtered <- data_filtered %>% filter(Term_Label %in% input$trend_term)
    }
    if (!is.null(input$trend_cashout) && length(input$trend_cashout) > 0) {
      data_filtered <- data_filtered %>% filter(Cashout_Label %in% input$trend_cashout)
    }
    if (!is.null(input$trend_race) && length(input$trend_race) > 0) {
      data_filtered <- data_filtered %>% filter(Race %in% input$trend_race)
    }
    if (!is.null(input$trend_gender) && length(input$trend_gender) > 0) {
      data_filtered <- data_filtered %>% filter(Sex_Label %in% input$trend_gender)
    }
    if (!is.null(input$trend_jumbo) && length(input$trend_jumbo) > 0) {
      data_filtered <- data_filtered %>% filter(Jumbo_Label %in% input$trend_jumbo)
    }
    data_filtered
  })
  
  output$trend_variable_label <- renderText({
    get_variable_label(input$trend_var)
  })
  
  # Graph 1: Yearly Trend
  output$trend_yearly <- renderPlotly({
    var_binary <- paste0(input$trend_var, "_binary")
    
    yearly_data <- trend_data() %>%
      filter(!is.na(get(var_binary)), !is.na(open_year)) %>%
      group_by(open_year, Institution_Type, response = get(var_binary)) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(open_year, Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      select(open_year, Institution_Type, percentage)
    
    # Calculate appropriate Y-axis range
    min_val <- min(yearly_data$percentage, na.rm = TRUE)
    max_val <- max(yearly_data$percentage, na.rm = TRUE)
    range_val <- max_val - min_val
    
    # Set Y-axis to show a reasonable range around the data
    y_min <- max(0, min_val - range_val * 0.5)  # Start lower to show separation
    y_max <- min(100, max_val + range_val * 0.3)  # Add space for labels
    
    p <- ggplot(yearly_data, aes(x = open_year, y = percentage, color = Institution_Type, group = Institution_Type)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      theme_minimal() +
      scale_color_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
      labs(x = "Year", y = "% Positive Response (Weighted)", 
           title = "Yearly Trend",
           color = "") +
      theme(legend.position = "bottom") +
      ylim(y_min, y_max)
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  # Graph 2: Year-over-Year Change
  output$trend_yoy_change <- renderPlotly({
    var_binary <- paste0(input$trend_var, "_binary")
    
    yearly_data <- trend_data() %>%
      filter(!is.na(get(var_binary)), !is.na(open_year)) %>%
      group_by(open_year, Institution_Type, response = get(var_binary)) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(open_year, Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      select(open_year, Institution_Type, percentage) %>%
      arrange(Institution_Type, open_year) %>%
      group_by(Institution_Type) %>%
      mutate(yoy_change = percentage - lag(percentage)) %>%
      filter(!is.na(yoy_change))
    
    # Calculate appropriate vertical offset for labels
    y_range <- max(yearly_data$yoy_change, na.rm = TRUE) - min(yearly_data$yoy_change, na.rm = TRUE)
    label_offset <- y_range * 0.15  # 15% of range for better visibility
    
    p <- ggplot(yearly_data, aes(x = open_year, y = yoy_change, color = Institution_Type, group = Institution_Type)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      theme_minimal() +
      scale_color_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
      labs(x = "Year", y = "YoY Change (Percentage Points)", color = "") +
      theme(legend.position = "bottom") +
      expand_limits(y = c(min(yearly_data$yoy_change, na.rm = TRUE) - label_offset,
                         max(yearly_data$yoy_change, na.rm = TRUE) + label_offset))
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  # Graph 3: Net Difference
  output$trend_net_diff <- renderPlotly({
    var_binary <- paste0(input$trend_var, "_binary")
    
    yearly_data <- trend_data() %>%
      filter(!is.na(get(var_binary)), !is.na(open_year)) %>%
      group_by(open_year, Institution_Type, response = get(var_binary)) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(open_year, Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      select(open_year, Institution_Type, percentage) %>%
      pivot_wider(names_from = Institution_Type, values_from = percentage) %>%
      mutate(Net_Diff = `Credit Union` - `Non-Credit Union`,
             Color = ifelse(Net_Diff > 0, "CU Higher", "Non-CU Higher"))
    
    p <- ggplot(yearly_data, aes(x = open_year, y = Net_Diff, fill = Color)) +
      geom_col() +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_text(aes(label = paste0(ifelse(Net_Diff > 0, "+", ""), round(Net_Diff, 1), "%")), 
                vjust = ifelse(yearly_data$Net_Diff > 0, -0.5, 1.5), size = 3.5, fontface = "bold") +
      theme_minimal() +
      scale_fill_manual(values = c("CU Higher" = colors_positive, "Non-CU Higher" = colors_negative)) +
      labs(x = "Year", y = "Net Difference (CU - Non-CU)", fill = "") +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  # Graph 4: By COVID Period
  output$trend_by_period <- renderPlotly({
    var_binary <- paste0(input$trend_var, "_binary")
    
    period_data <- trend_data() %>%
      filter(!is.na(get(var_binary)), COVID_period %in% period_order) %>%
      group_by(COVID_period, Institution_Type, response = get(var_binary)) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(COVID_period, Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      mutate(COVID_period = factor(COVID_period, levels = period_order))
    
    p <- ggplot(period_data, aes(x = COVID_period, y = percentage, fill = Institution_Type)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_dodge(0.9), vjust = -0.5, size = 3.5, fontface = "bold") +
      theme_minimal() +
      scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
      labs(x = "COVID Period", y = "% Positive Response (Weighted)", fill = "") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1)) +
      ylim(0, max(period_data$percentage, na.rm = TRUE) * 1.15)
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  # Graph 5: Net Difference by Period
  output$trend_net_by_period <- renderPlotly({
    var_binary <- paste0(input$trend_var, "_binary")
    
    period_data <- trend_data() %>%
      filter(!is.na(get(var_binary)), COVID_period %in% period_order) %>%
      group_by(COVID_period, Institution_Type, response = get(var_binary)) %>%
      summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(COVID_period, Institution_Type) %>%
      mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
      filter(response == "Positive (Very/Somewhat)") %>%
      select(COVID_period, Institution_Type, percentage) %>%
      pivot_wider(names_from = Institution_Type, values_from = percentage) %>%
      mutate(Net_Diff = `Credit Union` - `Non-Credit Union`,
             Color = ifelse(Net_Diff > 0, "CU Higher", "Non-CU Higher"),
             COVID_period = factor(COVID_period, levels = period_order))
    
    # Test significance for each period
    sig_stars <- sapply(period_order, function(period) {
      period_data_test <- trend_data() %>% filter(COVID_period == period)
      if (nrow(period_data_test) < 10) return("")
      sig <- test_significance(period_data_test, input$trend_var, "Institution_Type")
      ifelse(sig$significant, "*", "")
    })
    
    period_data$sig_star <- sig_stars[as.character(period_data$COVID_period)]
    
    p <- ggplot(period_data, aes(x = COVID_period, y = Net_Diff, fill = Color)) +
      geom_col() +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_text(aes(label = paste0(ifelse(Net_Diff > 0, "+", ""), round(Net_Diff, 1), "%", sig_star)), 
                vjust = ifelse(period_data$Net_Diff > 0, -0.5, 1.5), size = 3.5, fontface = "bold") +
      theme_minimal() +
      scale_fill_manual(values = c("CU Higher" = colors_positive, "Non-CU Higher" = colors_negative)) +
      labs(x = "COVID Period", y = "Net Difference (CU - Non-CU)", fill = "",
           title = "Net Difference by Period (* = Statistically Significant)") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  # =============================================================================
  
  # =============================================================================
  # TAB 3: ML ANALYSIS - CU vs Non-CU (Simplified)
  # =============================================================================
  
  prepare_outcome <- function(df, outcome_type) {
    if (outcome_type == "satisfaction") {
      var_binary <- paste0(response_vars[1], "_binary")
      ml_data <- df %>% filter(!is.na(get(var_binary))) %>%
        mutate(outcome = ifelse(get(var_binary) == "Positive (Very/Somewhat)", 1, 0))
      list(data = ml_data, name = "Satisfaction", type = "binary")
    } else {
      ml_data <- df %>% filter(!is.na(Rate_Spread_Pct)) %>% mutate(outcome = Rate_Spread_Pct)
      list(data = ml_data, name = "Rate Spread (%)", type = "regression")
    }
  }
  
  train_xgb_model <- function(data, include_covid_period = FALSE) {
    # Select features - conditionally include COVID_period
    if (include_covid_period) {
      features <- data %>%
        select(FICO_category, COVID_period, ltv, dti, Amount_Borrowed, Interest_Rate,
               Age, Sex_Label, Race, First_Mortgage_Label, Loan_Type, analysis_weight) %>%
        mutate(across(where(is.character), as.factor))
    } else {
      features <- data %>%
        select(FICO_category, ltv, dti, Amount_Borrowed, Interest_Rate,
               Age, Sex_Label, Race, First_Mortgage_Label, Loan_Type, analysis_weight) %>%
        mutate(across(where(is.character), as.factor))
    }
    
    factor_cols <- names(features)[sapply(features, is.factor)]
    for (col in factor_cols) if (length(unique(features[[col]])) < 2) features[[col]] <- NULL
    features <- features %>% select(where(~!all(is.na(.)) && length(unique(na.omit(.))) > 1))
    X <- model.matrix(~ . - 1 - analysis_weight, data = features)
    params <- list(max_depth = 4, eta = 0.1,
                   objective = ifelse(mean(data$outcome %in% c(0,1)) == 1, "binary:logistic", "reg:squarederror"))
    xgb_model <- xgb.train(params = params, data = xgb.DMatrix(X, label = data$outcome, weight = data$analysis_weight),
                          nrounds = 50, verbose = 0)
    list(model = xgb_model, importance = xgb.importance(model = xgb_model, feature_names = colnames(X)))
  }
  
  ml_cu_results_val <- reactiveValues(data = NULL)
  observeEvent(input$reset_ml_cu, { ml_cu_results_val$data <- NULL })
  
  observeEvent(input$run_ml_cu, {
    tryCatch({
      outcome_data <- prepare_outcome(df, input$ml_cu_outcome)
      ml_data <- outcome_data$data
      
      # Determine if COVID period dummies should be used as features
      # Use COVID_period when ALL three periods are selected (analyzing across all time)
      all_periods_selected <- length(input$ml_cu_period) == 3 && 
                              all(period_order %in% input$ml_cu_period)
      
      # Filter data if specific periods are selected
      if (!is.null(input$ml_cu_period) && length(input$ml_cu_period) > 0 && !all_periods_selected) {
        ml_data <- ml_data %>% filter(COVID_period %in% input$ml_cu_period)
      }
      
      cu_data <- ml_data %>% filter(Institution_Type == "Credit Union")
      noncu_data <- ml_data %>% filter(Institution_Type == "Non-Credit Union")
      
      # Train models - include COVID_period as feature ONLY when all periods selected
      cu_model <- train_xgb_model(cu_data, include_covid_period = all_periods_selected)
      noncu_model <- train_xgb_model(noncu_data, include_covid_period = all_periods_selected)
      
      if (outcome_data$type == "binary") {
        test_result <- prop.test(c(sum(cu_data$outcome), sum(noncu_data$outcome)),
                                 c(nrow(cu_data), nrow(noncu_data)))
      } else {
        test_result <- t.test(cu_data$outcome, noncu_data$outcome)
      }
      
      period_text <- if (!is.null(input$ml_cu_period) && length(input$ml_cu_period) > 0) {
        if (all_periods_selected) {
          "All Periods (COVID used as feature)"
        } else {
          paste(input$ml_cu_period, collapse = ", ")
        }
      } else "All Periods"
      
      ml_cu_results_val$data <- list(
        cu_model = cu_model, noncu_model = noncu_model, outcome_name = outcome_data$name,
        cu_mean = mean(cu_data$outcome, na.rm = TRUE), noncu_mean = mean(noncu_data$outcome, na.rm = TRUE),
        p_value = test_result$p.value, period_text = period_text, success = TRUE
      )
    }, error = function(e) {
      ml_cu_results_val$data <- list(success = FALSE, error = as.character(e))
    })
  })
  
  output$ml_cu_performance <- renderUI({
    res <- ml_cu_results_val$data
    if (is.null(res)) return(HTML("<p style='text-align: center; color: #95a5a6;'>Select options and click 'Run Model'.</p>"))
    if (!res$success) return(HTML(paste0("<div class='alert alert-danger'>", res$error, "</div>")))
    sig_color <- ifelse(res$p_value < 0.05, "#27ae60", "#e74c3c")
    sig_text <- ifelse(res$p_value < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")
    HTML(sprintf("<h4>Period(s): %s</h4><div class='row'>
      <div class='col-md-3 text-center'><h3 style='color: %s;'>%.3f</h3><p>CU Mean</p></div>
      <div class='col-md-3 text-center'><h3 style='color: %s;'>%.3f</h3><p>Non-CU Mean</p></div>
      <div class='col-md-3 text-center'><h3>%.4f</h3><p>P-value</p></div>
      <div class='col-md-3 text-center'><h3 style='color: %s;'>%s</h3><p>Difference</p></div></div>",
      res$period_text, colors_cu, res$cu_mean, colors_noncu, res$noncu_mean, res$p_value, sig_color, sig_text))
  })
  
  output$ml_cu_importance_cu <- renderPlotly({
    res <- ml_cu_results_val$data
    if (is.null(res) || !res$success) return(NULL)
    top_10 <- as.data.frame(res$cu_model$importance) %>% head(10)
    # Convert to percentage
    top_10$Importance_Pct <- (top_10$Gain / sum(res$cu_model$importance$Gain)) * 100
    top_10$Feature <- factor(top_10$Feature, levels = rev(top_10$Feature))
    p <- ggplot(top_10, aes(x = Importance_Pct, y = Feature)) + geom_col(fill = colors_cu) +
      geom_text(aes(label = paste0(round(Importance_Pct, 1), "%")), hjust = -0.1, size = 3.5, fontface = "bold") +
      theme_minimal() + labs(x = "Importance (%)", y = "") + xlim(0, max(top_10$Importance_Pct) * 1.15)
    ggplotly(p)
  })
  
  output$ml_cu_importance_noncu <- renderPlotly({
    res <- ml_cu_results_val$data
    if (is.null(res) || !res$success) return(NULL)
    top_10 <- as.data.frame(res$noncu_model$importance) %>% head(10)
    # Convert to percentage
    top_10$Importance_Pct <- (top_10$Gain / sum(res$noncu_model$importance$Gain)) * 100
    top_10$Feature <- factor(top_10$Feature, levels = rev(top_10$Feature))
    p <- ggplot(top_10, aes(x = Importance_Pct, y = Feature)) + geom_col(fill = colors_noncu) +
      geom_text(aes(label = paste0(round(Importance_Pct, 1), "%")), hjust = -0.1, size = 3.5, fontface = "bold") +
      theme_minimal() + labs(x = "Importance (%)", y = "") + xlim(0, max(top_10$Importance_Pct) * 1.15)
    ggplotly(p)
  })
  
  output$ml_cu_interpretation <- renderUI({
    res <- ml_cu_results_val$data
    if (is.null(res)) return(HTML("<p style='text-align: center; color: #95a5a6;'>Run model to see findings.</p>"))
    if (!res$success) return(HTML("<div class='alert alert-danger'>Model failed.</div>"))
    diff <- res$cu_mean - res$noncu_mean
    sig_html <- if (res$p_value < 0.001) {
      sprintf("<h4 style='color: #27ae60;'>✓ HIGHLY SIGNIFICANT</h4><p>P < 0.001: CU differs by <strong>%.3f</strong> (%.1f%% %s).</p>",
              abs(diff), abs(diff/res$noncu_mean * 100), ifelse(diff > 0, "higher", "lower"))
    } else if (res$p_value < 0.05) {
      sprintf("<h4 style='color: #27ae60;'>✓ SIGNIFICANT</h4><p>P = %.3f: CU differs by <strong>%.3f</strong>.</p>", res$p_value, abs(diff))
    } else {
      sprintf("<h4 style='color: #e74c3c;'>✗ NOT SIGNIFICANT</h4><p>P = %.3f: No significant difference.</p>", res$p_value)
    }
    cu_imp <- as.data.frame(res$cu_model$importance)
    noncu_imp <- as.data.frame(res$noncu_model$importance)
    drivers_html <- sprintf("<hr><h5>🎯 Top Drivers:</h5><div class='row'>
      <div class='col-md-6'><p><strong>CU:</strong> %s (%.1f%%)</p></div>
      <div class='col-md-6'><p><strong>Non-CU:</strong> %s (%.1f%%)</p></div></div><p><em>%s</em></p>",
      cu_imp$Feature[1], cu_imp$Gain[1] * 100, noncu_imp$Feature[1], noncu_imp$Gain[1] * 100, res$period_text)
    HTML(paste0(sig_html, drivers_html))
  })
  
  source("server_tabs.R", local = TRUE)
}
