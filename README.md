# NSMO Survey Analysis Dashboard - Complete Installation Guide

## 📦 Package Contents

You have received a modular Shiny dashboard with the following files:

1. **app.R** - Main application launcher
2. **global.R** - Data preprocessing and shared functions
3. **ui.R** - User interface definition
4. **server.R** - Main server logic (Tabs 1-2)
5. **server_tabs.R** - Additional server logic (Tabs 3-6)
6. **README.md** - This file

## 🚀 Quick Start

### Step 1: File Organization

Place ALL files in the same directory as your `nsmo.rds` data file:

```
your_project_folder/
├── app.R
├── global.R
├── ui.R
├── server.R
├── server_tabs.R
├── nsmo.rds          <- YOUR DATA FILE
└── README.md
```

### Step 2: Install Required Packages

Run this code in R to install all dependencies:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "tidyr",
  "plotly",
  "DT",
  "scales"
))
```

### Step 3: Launch the Dashboard

```r
# Navigate to your project folder
setwd("path/to/your_project_folder")

# Run the app
library(shiny)
runApp("app.R")
```

The dashboard will open in your default web browser!

## 📊 Dashboard Features

### 6 Interactive Tabs

1. **Executive Summary** - High-level CU vs Non-CU comparison
2. **Survey Responses** - Question-by-question satisfaction analysis
3. **Loan Characteristics** - Deep dive into loan metrics
4. **Demographics** - Population breakdown
5. **Performance Analysis** - Delinquency and forbearance tracking
6. **Data Explorer** - Advanced filtering and data export

### 40+ Variables Created

- **Demographics**: Age, Sex, Education, Race, Hispanic, Work Status, Veteran
- **Loan Metrics**: FICO scores, Amount, Rate, Payment, LTV/DTI/CLTV categories
- **Performance**: Forbearance status, Delinquency tracking
- **Time Periods**: Pre-COVID, COVID, Post-COVID

### Key Features

✅ Weighted statistics throughout
✅ Statistical significance testing (with * indicators)
✅ Unweighted counts, weighted percentages
✅ No legends (space-saving design)
✅ Consistent chart types across tabs
✅ Multiple filter combinations
✅ Year selection (multiple years)
✅ Color-coded visualizations
✅ Export-ready data tables

## 🎨 Color Scheme

- **Credit Union**: Blue (#3498db)
- **Non-Credit Union**: Red (#e74c3c)
- **Positive/CU Higher**: Green (#27ae60)
- **Negative/Non-CU Higher**: Dark Red (#c0392b)

## 📝 Customization

### Adding Variable Labels

Edit `global.R` around line 15-25:

```r
variable_labels <- list(
  "x05a" = "Satisfaction with loan application process",
  "x05b" = "Satisfaction with loan terms",
  # Add your labels from nsmo-appendix-c-v60.pdf
  "x06" = "Your label here",
  "x07" = "Your label here"
  # ... etc
)
```

### Modifying Filters

Edit `ui.R` to add/remove filter options in each tab's filter box.

### Adjusting Charts

Edit `server.R` or `server_tabs.R` to modify chart types, colors, or layouts.

## 🐛 Troubleshooting

### Problem: "Error: object 'df' not found"

**Solution**: Ensure `nsmo.rds` is in the same directory and named correctly.

### Problem: "Package X is not installed"

**Solution**: Run `install.packages("X")` replacing X with the package name.

### Problem: "Error in source('server_tabs.R')"

**Solution**: Ensure all 5 .R files are in the same directory.

### Problem: Charts not displaying

**Solution**: 
- Check your data has the required columns
- Verify filters aren't excluding all data
- Check browser console for JavaScript errors

### Problem: Slow performance

**Solution**:
- Filter data to smaller subsets
- Reduce number of simultaneous open tabs
- Use Chrome or Firefox for better performance

## 📊 Data Requirements

Your `nsmo.rds` file should contain these columns:

### Required:
- `cu` - Credit union indicator (1 = CU, 0 = Non-CU)
- `analysis_weight` - Survey weights
- `open_year`, `open_month` - Loan origination date
- Response variables: `x05a`, `x05b`, `x05c`, etc.

### Optional (for full functionality):
- Demographics: `age_o1`, `age_o2`, `sex_o1`, `sex_o2`, `x74r`, `x75r`, `x76r`, `x76s`, `x77r`, `x78r`, `x79ra`, `x79rb`, `x80r`
- Loan: `first_mort_r`, `first_mort_s`, `score_orig_r`, `score_orig_s`, `z41`, `z42`, `z43`, `ltv`, `cltv`, `dti`, `term`, `loan_type`, `gse`, `metro_lmi`, `jumbo`, `cashout`, `rate_spread`, `pmms`, `loan_amount_cat`
- Performance: `forb*` columns, `perf_status*` columns, `score_YYYY_r`, `score_YYYY_s`

Missing columns will be handled gracefully - those sections just won't display data.

## 📈 Usage Tips

### Executive Summary Tab
- Use filters to focus on specific segments
- Compare key metrics side-by-side
- Export summary table for reports

### Survey Responses Tab
- Select different questions from dropdown
- Watch for * indicators (statistical significance)
- Use FICO heatmap to spot patterns

### Loan Characteristics Tab
- Compare average metrics across institutions
- Analyze LTV and DTI distributions
- Track credit score changes (Origin vs Last)

### Demographics Tab
- Filter by COVID period to see trends
- Combine race/work/metro filters
- Export for further analysis

### Performance Analysis Tab
- Monitor forbearance rates
- Track delinquency by FICO category
- Compare CU vs Non-CU performance

### Data Explorer Tab
- Use advanced filters for deep dives
- Select multiple years
- Export filtered data to CSV/Excel

## 🔧 Advanced Configuration

### Changing Chart Heights

In `ui.R`, modify the `height` parameter:

```r
plotlyOutput("chart_name", height = 400)  # Change 400 to desired height
```

### Adding New Filters

1. Add filter input in `ui.R`:
```r
selectInput("new_filter", "Label:", choices = c("All", "Option1", "Option2"))
```

2. Apply filter in corresponding reactive in `server.R`:
```r
if (input$new_filter != "All") data_filtered <- data_filtered %>% filter(column == input$new_filter)
```

### Modifying Color Scheme

In `global.R`, change the color variables:

```r
colors_cu <- "#YOUR_COLOR"        # Credit Union color
colors_noncu <- "#YOUR_COLOR"     # Non-Credit Union color
colors_positive <- "#YOUR_COLOR"   # Positive/Higher color
colors_negative <- "#YOUR_COLOR"   # Negative/Lower color
```

## 📞 Support

### Common Issues

**Dashboard won't start:**
- Check all 5 .R files are present
- Verify `nsmo.rds` is in the same folder
- Check R console for specific error messages

**Missing data in charts:**
- Verify column names match expected format
- Check filters aren't too restrictive
- Review global.R preprocessing logic

**Performance issues:**
- Close unused browser tabs
- Clear browser cache
- Restart R session

## 🎯 Next Steps

1. ✅ **Test the dashboard** - Run with your data
2. ✅ **Add variable labels** - Complete the list in global.R
3. ✅ **Customize filters** - Add/remove based on your needs
4. ✅ **Share with team** - Get feedback on layout and features
5. ✅ **Deploy** - Consider shinyapps.io for sharing

## 📄 File Structure Details

### app.R (Main Launcher)
- Sources all components
- Launches the Shiny app
- **Do not modify** unless you know what you're doing

### global.R (Setup & Preprocessing)
- Loads packages and data
- Creates all 40+ derived variables
- Defines helper functions
- Sets up color scheme
- **Modify here** to add variable labels

### ui.R (User Interface)
- Defines all 6 tabs
- Creates filter boxes
- Sets up chart containers
- **Modify here** to change layout or add filters

### server.R (Main Logic)
- Reactive data filters
- Executive Summary outputs
- Survey Responses outputs
- **Modify here** to change Tab 1-2 logic

### server_tabs.R (Additional Logic)
- Loan Characteristics outputs
- Demographics outputs
- Performance outputs
- Data Explorer outputs
- **Modify here** to change Tab 3-6 logic

## 🌟 Best Practices

1. **Always test filters** - Apply different combinations to ensure they work
2. **Check significance stars** - Use * to identify meaningful differences
3. **Export data regularly** - Use Data Explorer tab to save filtered datasets
4. **Document customizations** - Keep notes on any changes you make
5. **Version control** - Save copies before making major changes

## ✨ Features Highlight

### Intelligent Data Processing
- Automatically handles missing values
- Combines borrower/co-borrower fields
- Creates industry-standard categories
- Tracks most recent credit scores

### Statistical Rigor
- Chi-square significance testing
- Weighted percentages (survey-adjusted)
- Unweighted counts (actual observations)
- Clear significance indicators (*)

### Professional Visualizations
- Consistent color scheme
- No cluttered legends
- Percentage labels on bars
- Ordered categories (Pre→During→Post)

### Flexible Filtering
- Multiple dimensions simultaneously
- Multi-year selection
- "All" option for each filter
- Instant reactivity

Enjoy your comprehensive NSMO dashboard! 🎉
