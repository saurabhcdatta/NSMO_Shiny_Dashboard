# ui.R - User Interface Definition

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "NSMO Survey Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Executive Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Survey Responses", tabName = "responses", icon = icon("comments")),
      menuItem("Model: CU vs Non-CU", tabName = "ml_cu", icon = icon("brain")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #3c8dbc; }
        .small-box { border-radius: 5px; }
        .info-box { min-height: 90px; margin-bottom: 15px; }
        .sig-note { font-size: 12px; font-style: italic; padding: 5px; margin-top: 5px;
          background-color: #f9f9f9; border-left: 3px solid #3c8dbc; }
        .sig-yes { border-left-color: #27ae60; }
        .sig-no { border-left-color: #95a5a6; }
      "))
    ),
    
    tabItems(
      # TAB 1: EXECUTIVE SUMMARY
      tabItem(tabName = "summary",
              h2("Executive Summary: CU vs Non-CU Comparison"),
              
              fluidRow(
                box(
                  title = "Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(2, selectizeInput("sum_fico", "FICO Category:",
                                         choices = fico_order, selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All FICO...'))),
                  column(2, selectizeInput("sum_period", "COVID Period:",
                                         choices = period_order, selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All periods...'))),
                  column(1, selectizeInput("sum_first_mort", "First Time:",
                                         choices = c("First Mortgage", "Not First"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("sum_loan_type", "Loan Type:",
                                         choices = c("Conventional", "FHA/VA/FSA"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All types...'))),
                  column(1, selectizeInput("sum_term", "Loan Term:",
                                         choices = c("30 Years", "Other"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All terms...'))),
                  column(1, selectizeInput("sum_cashout", "Purpose:",
                                         choices = c("Refinance", "Purchase"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All purposes...'))),
                  column(2, selectizeInput("sum_race", "Race:",
                                         choices = c("White", "Black", "Asian", "Other"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All races...'))),
                  column(1, selectizeInput("sum_gender", "Gender:",
                                         choices = c("Male", "Female"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("sum_jumbo", "Jumbo:",
                                         choices = c("Jumbo", "Non-Jumbo"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...')))
                )
              ),
              
              fluidRow(
                valueBoxOutput("sum_total_loans_cu", width = 2),
                valueBoxOutput("sum_total_loans_noncu", width = 2),
                valueBoxOutput("sum_rate_spread_cu", width = 2),
                valueBoxOutput("sum_rate_spread_noncu", width = 2),
                valueBoxOutput("sum_median_amt_cat_cu", width = 2),
                valueBoxOutput("sum_median_amt_cat_noncu", width = 2)
              ),
              
              fluidRow(
                infoBoxOutput("sum_avg_fico_cu", width = 3),
                infoBoxOutput("sum_avg_fico_noncu", width = 3),
                infoBoxOutput("sum_avg_ltv_cu", width = 3),
                infoBoxOutput("sum_avg_ltv_noncu", width = 3)
              ),
              
              fluidRow(
                box(title = "Key Metrics Comparison: CU vs Non-CU", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("sum_metrics_compare", height = 400))
              ),
              
              fluidRow(
                box(title = "CU: FICO Distribution (%)", width = 3, status = "info", solidHeader = TRUE,
                    plotlyOutput("sum_fico_heatmap_cu", height = 400)),
                box(title = "Non-CU: FICO Distribution (%)", width = 3, status = "info", solidHeader = TRUE,
                    plotlyOutput("sum_fico_heatmap_noncu", height = 400)),
                box(title = "Sex Distribution (%)", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("sum_sex_pie", height = 400))
              ),
              
              fluidRow(
                box(title = "Race Composition (%)", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("sum_race_donut", height = 400))
              ),
              
              fluidRow(
                box(title = "Performance Metrics", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("sum_performance_metrics", height = 350))
              )
      ),
      
      # TAB 2: SURVEY RESPONSES - TREND ANALYSIS
      tabItem(tabName = "responses",
              h2("Survey Response Trend Analysis"),
              
              fluidRow(
                box(
                  title = "Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(2, selectInput("trend_var", "Response Variable:",
                                      choices = response_choices, selected = response_vars[1])),
                  column(1, selectizeInput("trend_fico", "FICO Category:",
                                         choices = fico_order, selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All FICO...'))),
                  column(1, selectizeInput("trend_period", "COVID Period:",
                                         choices = period_order, selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All periods...'))),
                  column(1, selectizeInput("trend_first_mort", "First Time:",
                                         choices = c("First Mortgage", "Not First"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("trend_loan_type", "Loan Type:",
                                         choices = c("Conventional", "FHA/VA/FSA"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("trend_term", "Term:",
                                         choices = c("30 Years", "Other"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("trend_cashout", "Purpose:",
                                         choices = c("Refinance", "Purchase"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(2, selectizeInput("trend_race", "Race:",
                                         choices = c("White", "Black", "Asian", "Other"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("trend_gender", "Gender:",
                                         choices = c("Male", "Female"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...'))),
                  column(1, selectizeInput("trend_jumbo", "Jumbo:",
                                         choices = c("Jumbo", "Non-Jumbo"), selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All...')))
                )
              ),
              
              div(style = "padding: 10px; background-color: #ecf0f5; margin: 10px; border-radius: 3px;",
                  strong("Selected Question:"), br(), textOutput("trend_variable_label", inline = TRUE)),
              
              fluidRow(
                box(title = "Yearly Trend: Weighted % Positive Response", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("trend_yearly", height = 400))
              ),
              
              fluidRow(
                box(title = "Year-over-Year Change in % Positive", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("trend_yoy_change", height = 350)),
                box(title = "Net Difference: CU - Non-CU", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("trend_net_diff", height = 350))
              ),
              
              fluidRow(
                box(title = "Trend by COVID Period", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("trend_by_period", height = 350)),
                box(title = "Net Difference by COVID Period", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("trend_net_by_period", height = 350))
              )
      ),
      
      # TAB 3: ML ANALYSIS - CU vs Non-CU
      tabItem(tabName = "ml_cu",
              h2("Machine Learning: CU vs Non-CU Comparison"),
              
              fluidRow(
                box(
                  title = "Model Configuration", width = 12, status = "primary", solidHeader = TRUE,
                  column(3, selectInput("ml_cu_outcome", "Outcome Variable:",
                                      choices = c(
                                        "Customer Satisfaction" = "satisfaction",
                                        "Rate Spread (%)" = "rate_spread"
                                      ),
                                      selected = "satisfaction")),
                  column(5, 
                         tags$label("COVID Period Filter:", style = "font-weight: bold; margin-bottom: 10px;"),
                         br(),
                         checkboxGroupInput("ml_cu_period", NULL,
                                          choices = period_order,
                                          selected = period_order,
                                          inline = TRUE)),
                  column(2, br(), actionButton("run_ml_cu", "Run Model", 
                                             class = "btn-primary btn-lg", 
                                             style = "margin-top: 5px; width: 100%;")),
                  column(2, br(), actionButton("reset_ml_cu", "Reset", 
                                             class = "btn-warning btn-lg", 
                                             style = "margin-top: 5px; width: 100%;"))
                )
              ),
              
              fluidRow(
                box(title = "Model Performance & Statistical Test", width = 12, status = "success", solidHeader = TRUE,
                    htmlOutput("ml_cu_performance"))
              ),
              
              fluidRow(
                box(title = "Top 10 Features: Credit Union", width = 6, status = "primary", solidHeader = TRUE,
                    plotlyOutput("ml_cu_importance_cu", height = 450)),
                box(title = "Top 10 Features: Non-Credit Union", width = 6, status = "primary", solidHeader = TRUE,
                    plotlyOutput("ml_cu_importance_noncu", height = 450))
              ),
              
              fluidRow(
                box(title = "Key Findings & Statistical Inference", width = 12, status = "success", solidHeader = TRUE,
                    htmlOutput("ml_cu_interpretation"))
              )
      ),
      
      # TAB 3: LOAN CHARACTERISTICS
      tabItem(tabName = "loan_chars",
              h2("Loan Characteristics Analysis"),
              
              fluidRow(
                box(
                  title = "Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(2, selectInput("loan_fico", "FICO Category:",
                                      choices = c("All", fico_order), selected = "All")),
                  column(2, selectInput("loan_period", "COVID Period:",
                                      choices = c("All", period_order), selected = "All")),
                  column(2, selectInput("loan_type_filter", "Loan Type:",
                                      choices = c("All", "Conventional", "FHA/VA/FSA"), selected = "All")),
                  column(2, selectInput("loan_gse", "GSE:",
                                      choices = c("All", "GSE", "Other"), selected = "All")),
                  column(2, selectInput("loan_term", "Term:",
                                      choices = c("All", "30 Years", "Other"), selected = "All")),
                  column(2, selectInput("loan_cashout", "Purpose:",
                                      choices = c("All", "Refinance", "Purchase"), selected = "All"))
                )
              ),
              
              fluidRow(
                infoBoxOutput("loan_avg_amount_cu", width = 3),
                infoBoxOutput("loan_avg_amount_noncu", width = 3),
                infoBoxOutput("loan_avg_rate_cu", width = 3),
                infoBoxOutput("loan_avg_rate_noncu", width = 3)
              ),
              
              fluidRow(
                box(title = "Average Loan Metrics Comparison", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("loan_metrics_compare", height = 400))
              ),
              
              fluidRow(
                box(title = "LTV Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("loan_ltv_dist", height = 350)),
                box(title = "DTI Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("loan_dti_dist", height = 350))
              ),
              
              fluidRow(
                box(title = "Credit Score Comparison (Origin vs Last)", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("loan_score_compare", height = 350))
              )
      ),
      
      # TAB 4: DEMOGRAPHICS
      tabItem(tabName = "demographics",
              h2("Demographic Analysis"),
              
              fluidRow(
                box(
                  title = "Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(3, selectInput("demo_period", "COVID Period:",
                                      choices = c("All", period_order), selected = "All")),
                  column(3, selectInput("demo_race", "Race:",
                                      choices = c("All", "White", "Black", "Asian", "Other"), selected = "All")),
                  column(3, selectInput("demo_work", "Work Status:",
                                      choices = c("All", "Self-Employed", "Employed", "Retired", 
                                                "Unemployed", "Not Working"), selected = "All")),
                  column(3, selectInput("demo_metro", "Metro Status:",
                                      choices = c("All", "MSA", "Non-MSA"), selected = "All"))
                )
              ),
              
              fluidRow(
                box(title = "Distribution by Age", width = 6, status = "primary", solidHeader = TRUE,
                    plotlyOutput("demo_age_dist", height = 350)),
                box(title = "Distribution by Sex", width = 6, status = "primary", solidHeader = TRUE,
                    plotlyOutput("demo_sex_dist", height = 350))
              ),
              
              fluidRow(
                box(title = "Distribution by Race", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("demo_race_dist", height = 350)),
                box(title = "Distribution by Work Status", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("demo_work_dist", height = 350))
              ),
              
              fluidRow(
                box(title = "Distribution by Education", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("demo_education_dist", height = 350))
              )
      ),
      
      # TAB 5: PERFORMANCE ANALYSIS
      tabItem(tabName = "performance",
              h2("Performance Analysis"),
              
              fluidRow(
                box(
                  title = "Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(3, selectInput("perf_period", "COVID Period:",
                                      choices = c("All", period_order), selected = "All")),
                  column(3, selectInput("perf_fico", "FICO Category:",
                                      choices = c("All", fico_order), selected = "All")),
                  column(3, selectInput("perf_forbear", "Forbearance:",
                                      choices = c("All", "Yes", "No"), selected = "All")),
                  column(3, selectInput("perf_jumbo", "Jumbo:",
                                      choices = c("All", "Jumbo", "Non-Jumbo"), selected = "All"))
                )
              ),
              
              fluidRow(
                box(title = "Performance Status Distribution", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("perf_status_dist", height = 400))
              ),
              
              fluidRow(
                box(title = "Forbearance Rate", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("perf_forbear_rate", height = 350)),
                box(title = "Performance by FICO Category", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("perf_by_fico", height = 350))
              )
      ),
      
      # TAB 6: DATA EXPLORER
      tabItem(tabName = "explorer",
              h2("Data Explorer"),
              
              fluidRow(
                box(
                  title = "Advanced Filters", width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(2, selectizeInput("exp_years", "Origination Years:",
                                         choices = available_years, selected = NULL, multiple = TRUE,
                                         options = list(placeholder = 'All years...'))),
                  column(2, selectInput("exp_performance", "Performance:",
                                      choices = c("All", "Current/Other", "60 Days Delinquent", 
                                                "180+ Days Delinquent", "Foreclosure/Bankruptcy"), selected = "All")),
                  column(2, selectInput("exp_education", "Education:",
                                      choices = c("All", "Some High School", "High School Graduate",
                                                "Tech School", "Some College", "College Graduate", "Post-Graduate"), 
                                      selected = "All")),
                  column(2, selectInput("exp_age", "Age Category:",
                                      choices = c("All", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), 
                                      selected = "All")),
                  column(2, selectInput("exp_ltv", "LTV Category:",
                                      choices = c("All", "≤80%", "80-90%", "90-95%", ">95%"), selected = "All")),
                  column(2, selectInput("exp_dti", "DTI Category:",
                                      choices = c("All", "≤36%", "36-43%", "43-50%", ">50%"), selected = "All"))
                )
              ),
              
              fluidRow(
                valueBoxOutput("exp_total_loans", width = 3),
                valueBoxOutput("exp_cu_pct", width = 3),
                valueBoxOutput("exp_avg_fico", width = 3),
                valueBoxOutput("exp_avg_amount", width = 3)
              ),
              
              fluidRow(
                box(title = "Filtered Data Summary", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("exp_data_table"))
              )
      )
    )
  )
)
