# server_tabs.R - Additional Server Logic for Remaining Tabs
# This file is sourced by server.R

# =============================================================================
# TAB 3: LOAN CHARACTERISTICS
# =============================================================================

output$loan_avg_amount_cu <- renderInfoBox({
  cu_data <- loan_data() %>% filter(Institution_Type == "Credit Union")
  avg <- weighted.mean(cu_data$Amount_Borrowed, cu_data$analysis_weight, na.rm = TRUE)
  infoBox("CU Avg Amount", paste0("$", format(round(avg/1000, 0), big.mark = ","), "K"),
          icon = icon("dollar-sign"), color = "blue", fill = TRUE)
})

output$loan_avg_amount_noncu <- renderInfoBox({
  noncu_data <- loan_data() %>% filter(Institution_Type == "Non-Credit Union")
  avg <- weighted.mean(noncu_data$Amount_Borrowed, noncu_data$analysis_weight, na.rm = TRUE)
  infoBox("Non-CU Avg Amount", paste0("$", format(round(avg/1000, 0), big.mark = ","), "K"),
          icon = icon("dollar-sign"), color = "red", fill = TRUE)
})

output$loan_avg_rate_cu <- renderInfoBox({
  cu_data <- loan_data() %>% filter(Institution_Type == "Credit Union")
  avg <- weighted.mean(cu_data$Interest_Rate, cu_data$analysis_weight, na.rm = TRUE)
  infoBox("CU Avg Rate", paste0(round(avg, 2), "%"),
          icon = icon("percent"), color = "light-blue")
})

output$loan_avg_rate_noncu <- renderInfoBox({
  noncu_data <- loan_data() %>% filter(Institution_Type == "Non-Credit Union")
  avg <- weighted.mean(noncu_data$Interest_Rate, noncu_data$analysis_weight, na.rm = TRUE)
  infoBox("Non-CU Avg Rate", paste0(round(avg, 2), "%"),
          icon = icon("percent"), color = "orange")
})

output$loan_metrics_compare <- renderPlotly({
  metrics_data <- loan_data() %>%
    group_by(Institution_Type) %>%
    summarise(
      `Amount ($K)` = weighted.mean(Amount_Borrowed/1000, analysis_weight, na.rm = TRUE),
      `Rate (%)` = weighted.mean(Interest_Rate, analysis_weight, na.rm = TRUE),
      `Payment ($)` = weighted.mean(Monthly_Payment, analysis_weight, na.rm = TRUE),
      `FICO Origin` = weighted.mean(Score_Origin, analysis_weight, na.rm = TRUE) / 10,
      `FICO Last` = weighted.mean(Score_Last, analysis_weight, na.rm = TRUE) / 10,
      `LTV (%)` = weighted.mean(ltv, analysis_weight, na.rm = TRUE),
      `DTI (%)` = weighted.mean(dti, analysis_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -Institution_Type, names_to = "Metric", values_to = "Value")
  
  p <- metrics_data %>%
    ggplot(aes(x = Metric, y = Value, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(Value, 1)), position = position_dodge(width = 0.9),
              vjust = -0.7, size = 2.5, fontface = "bold") +
    facet_wrap(~Metric, scales = "free_y", ncol = 4) +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "", y = "Value") +
    theme(legend.position = "none", axis.text.x = element_blank(),
          strip.text = element_text(face = "bold", size = 9))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$loan_ltv_dist <- renderPlotly({
  ltv_order <- c("≤80%", "80-90%", "90-95%", ">95%")
  
  dist_data <- loan_data() %>%
    group_by(LTV_Category, Institution_Type) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(LTV_Category = factor(LTV_Category, levels = ltv_order))
  
  p <- ggplot(dist_data, aes(x = LTV_Category, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "LTV Category", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$loan_dti_dist <- renderPlotly({
  dti_order <- c("≤36%", "36-43%", "43-50%", ">50%")
  
  dist_data <- loan_data() %>%
    group_by(DTI_Category, Institution_Type) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(DTI_Category = factor(DTI_Category, levels = dti_order))
  
  p <- ggplot(dist_data, aes(x = DTI_Category, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "DTI Category", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$loan_score_compare <- renderPlotly({
  score_data <- loan_data() %>%
    group_by(Institution_Type) %>%
    summarise(
      `FICO at Origination` = weighted.mean(Score_Origin, analysis_weight, na.rm = TRUE),
      `FICO Last Reported` = weighted.mean(Score_Last, analysis_weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -Institution_Type, names_to = "Score_Type", values_to = "Score")
  
  p <- ggplot(score_data, aes(x = Score_Type, y = Score, fill = Institution_Type, group = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = round(Score, 0)), position = position_dodge(0.9),
              vjust = -0.7, size = 3.5, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Score Type", y = "Average FICO Score") +
    theme(legend.position = "none")
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

# =============================================================================
# TAB 4: DEMOGRAPHICS
# =============================================================================

output$demo_age_dist <- renderPlotly({
  age_order <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
  
  dist_data <- demo_data() %>%
    filter(Age_Category != "Unknown") %>%
    group_by(Age_Category, Institution_Type) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Age_Category = factor(Age_Category, levels = age_order))
  
  p <- ggplot(dist_data, aes(x = Age_Category, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Age Category", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$demo_sex_dist <- renderPlotly({
  dist_data <- demo_data() %>%
    filter(Sex_Label != "Unknown") %>%
    group_by(Sex_Label, Institution_Type) %>%
    summarise(n = n(), .groups = "drop")
  
  p <- ggplot(dist_data, aes(x = Sex_Label, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3.5, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Sex", y = "Count") +
    theme(legend.position = "none")
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$demo_race_dist <- renderPlotly({
  dist_data <- demo_data() %>%
    filter(Race != "Unknown") %>%
    group_by(Race, Institution_Type) %>%
    summarise(n = n(), .groups = "drop")
  
  p <- ggplot(dist_data, aes(x = Race, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Race", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$demo_work_dist <- renderPlotly({
  dist_data <- demo_data() %>%
    filter(Work_Status != "Unknown") %>%
    group_by(Work_Status, Institution_Type) %>%
    summarise(n = n(), .groups = "drop")
  
  p <- ggplot(dist_data, aes(x = Work_Status, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Work Status", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$demo_education_dist <- renderPlotly({
  edu_order <- c("Some High School", "High School Graduate", "Tech School",
                 "Some College", "College Graduate", "Post-Graduate")
  
  dist_data <- demo_data() %>%
    filter(Education_Label != "Unknown") %>%
    group_by(Education_Label, Institution_Type) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Education_Label = factor(Education_Label, levels = edu_order))
  
  p <- ggplot(dist_data, aes(x = Education_Label, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Household Max Education", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

# =============================================================================
# TAB 5: PERFORMANCE ANALYSIS
# =============================================================================

output$perf_status_dist <- renderPlotly({
  dist_data <- perf_data() %>%
    filter(Performance != "Unknown") %>%
    group_by(Performance, Institution_Type) %>%
    summarise(n = n(), .groups = "drop")
  
  p <- ggplot(dist_data, aes(x = Performance, y = n, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = format(n, big.mark = ",")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Performance Status", y = "Count") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$perf_forbear_rate <- renderPlotly({
  forbear_data <- perf_data() %>%
    group_by(Institution_Type, Forbearance) %>%
    summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
    group_by(Institution_Type) %>%
    mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
    filter(Forbearance == "Yes")
  
  p <- ggplot(forbear_data, aes(x = Institution_Type, y = percentage, fill = Institution_Type)) +
    geom_col() +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.7,
              size = 4, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "Institution Type", y = "Forbearance Rate (%)") +
    theme(legend.position = "none") +
    ylim(0, max(forbear_data$percentage) * 1.2)
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

output$perf_by_fico <- renderPlotly({
  perf_data_filtered <- perf_data() %>%
    filter(Performance != "Unknown", FICO_category != "Unknown") %>%
    mutate(Delinquent = ifelse(Performance %in% c("60 Days Delinquent", "180+ Days Delinquent",
                                                   "Foreclosure/Bankruptcy"), "Yes", "No"))
  
  perf_summary <- perf_data_filtered %>%
    group_by(FICO_category, Institution_Type, Delinquent) %>%
    summarise(weighted_n = sum(analysis_weight, na.rm = TRUE), .groups = "drop") %>%
    group_by(FICO_category, Institution_Type) %>%
    mutate(percentage = weighted_n / sum(weighted_n) * 100) %>%
    filter(Delinquent == "Yes") %>%
    mutate(FICO_category = factor(FICO_category, levels = fico_order))
  
  p <- ggplot(perf_summary, aes(x = FICO_category, y = percentage, fill = Institution_Type)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_dodge(0.9),
              vjust = -0.7, size = 3, fontface = "bold") +
    theme_minimal() +
    scale_fill_manual(values = c("Credit Union" = colors_cu, "Non-Credit Union" = colors_noncu)) +
    labs(x = "FICO Category", y = "Delinquency Rate (%)") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p) %>% layout(showlegend = FALSE)
})

# =============================================================================
# TAB 6: DATA EXPLORER
# =============================================================================

output$exp_total_loans <- renderValueBox({
  valueBox(format(nrow(explorer_data()), big.mark = ","), "Filtered Loans",
           icon = icon("filter"), color = "blue")
})

output$exp_cu_pct <- renderValueBox({
  pct <- sum(explorer_data()$analysis_weight[explorer_data()$Institution_Type == "Credit Union"], na.rm = TRUE) /
         sum(explorer_data()$analysis_weight, na.rm = TRUE) * 100
  valueBox(paste0(round(pct, 1), "%"), "CU Share",
           icon = icon("building"), color = "green")
})

output$exp_avg_fico <- renderValueBox({
  avg <- weighted.mean(explorer_data()$Score_Origin, explorer_data()$analysis_weight, na.rm = TRUE)
  valueBox(round(avg, 0), "Avg FICO",
           icon = icon("chart-line"), color = "yellow")
})

output$exp_avg_amount <- renderValueBox({
  avg <- weighted.mean(explorer_data()$Amount_Borrowed, explorer_data()$analysis_weight, na.rm = TRUE)
  valueBox(paste0("$", format(round(avg/1000, 0), big.mark = ","), "K"), "Avg Amount",
           icon = icon("dollar-sign"), color = "purple")
})

output$exp_data_table <- renderDT({
  display_cols <- c("Institution_Type", "FICO_category", "COVID_period", "Age_Category",
                   "Sex_Label", "Race", "Work_Status", "Veteran",
                   "Amount_Borrowed", "Interest_Rate", "Monthly_Payment",
                   "LTV_Category", "DTI_Category", "Loan_Type", "Performance",
                   "Forbearance", "open_year")
  
  # Get available columns
  available_cols <- display_cols[display_cols %in% names(explorer_data())]
  
  datatable(
    explorer_data()[, available_cols, drop = FALSE],
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    ),
    extensions = 'Buttons',
    filter = 'top',
    rownames = FALSE
  ) %>%
    formatCurrency(c("Amount_Borrowed", "Monthly_Payment"), digits = 0) %>%
    formatRound("Interest_Rate", digits = 2)
})
