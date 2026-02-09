library(shiny)
library(shinyBS)
library(bslib)
library(shinyWidgets)
library(jsonlite)
library(scales)
library(plotly)
library(pivottabler)
library(tidyverse)

# pivot table data
verification_2023 <- readRDS("pivot_table_2023.RDS") %>% mutate(year = rep(2023, nrow(.)))
verification_2022 <- readRDS("pivot_table_2022.RDS") %>% mutate(year = rep(2022, nrow(.)))
verification_2021 <- readRDS("pivot_table_2021.RDS") %>% mutate(year = rep(2021, nrow(.)))
verification_2020 <- readRDS("pivot_table_2020.RDS") %>% mutate(year = rep(2020, nrow(.)))
verification_2019 <- readRDS("pivot_table_2019.RDS") %>% mutate(year = rep(2019, nrow(.)))
verification_2018 <- readRDS("pivot_table_2018.RDS") %>% mutate(year = rep(2018, nrow(.)))
verification_2017 <- readRDS("pivot_table_2017.RDS") %>% mutate(year = rep(2017, nrow(.)))

verification_all <- bind_rows(
  verification_2017,
  verification_2018,
  verification_2019,
  verification_2020,
  verification_2021,
  verification_2022,
  verification_2023
) %>%
  select(-agency) %>%
  rename(`Case ID` = case_id,
         `Dollar Amount in Error` = `Dollar Amount in Error`,
         `Error Discovery` = Verification,
         `Error Responsibility` = Responsibility,
         `Error Type` = Type,
         `Error Nature` = Nature,
         `Error Element` = Element,
         `Error Timing` = `Time Period`,
         `Case Type` = `Action Type`,
         `Year` = year) %>%
  mutate(`Error Discovery` = ifelse(is.na(`Error Discovery`), "Other", `Error Discovery`))

colnames(verification_all)

# base percentages
base_case_all <- readRDS("base_case_2017_2023.rds")
base_cat_all  <- readRDS("base_cat_2017_2023.rds")

# errors by categories
df_error_2023 <- readRDS("snap_error_2023.RDS") %>% mutate(year = rep(2023, nrow(.)))
df_error_2022 <- readRDS("snap_error_2022.RDS") %>% mutate(year = rep(2022, nrow(.)))
df_error_2021 <- readRDS("snap_error_2021.RDS")  %>% mutate(year = rep(2021, nrow(.)))
df_error_2020 <- readRDS("snap_error_2020.RDS")  %>% mutate(year = rep(2020, nrow(.)))
df_error_2019 <- readRDS("snap_error_2019.RDS")  %>% mutate(year = rep(2019, nrow(.)))
df_error_2018 <- readRDS("snap_error_2018.RDS")  %>% mutate(year = rep(2018, nrow(.)))
df_error_2017 <- readRDS("snap_error_2017.RDS") %>% mutate(year = rep(2017, nrow(.)))

df_error <- do.call("rbind", list(df_error_2017, 
                                  df_error_2018, 
                                  df_error_2019, 
                                  df_error_2020, 
                                  df_error_2021, 
                                  df_error_2022,
                                  df_error_2023)) %>%
  rename(`Dollar Amount in Error` = dollar_amount)

# errors by demographics
df_2023 <- readRDS("snap_demographics_2023.RDS") %>% mutate(year = rep(2023, nrow(.)))
df_2022 <- readRDS("snap_demographics_2022.RDS") %>% mutate(year = rep(2022, nrow(.)))
df_2021 <- readRDS("snap_demographics_2021.RDS")  %>% mutate(year = rep(2021, nrow(.)))
df_2020 <- readRDS("snap_demographics_2020.RDS")  %>% mutate(year = rep(2020, nrow(.)))
df_2019 <- readRDS("snap_demographics_2019.RDS")  %>% mutate(year = rep(2019, nrow(.)))
df_2018 <- readRDS("snap_demographics_2018.RDS")  %>% mutate(year = rep(2018, nrow(.)))
df_2017 <- readRDS("snap_demographics_2017.RDS") %>% mutate(year = rep(2017, nrow(.)))

df <- do.call("rbind", list(df_2017, 
                            df_2018, 
                            df_2019, 
                            df_2020, 
                            df_2021, 
                            df_2022,
                            df_2023))

df <- df %>%
  drop_na(Employment) %>%
  drop_na(Race) %>%
  drop_na(Gender) %>%
  drop_na(Age) %>%
  drop_na(`Disability (Household Member)`) %>%
  drop_na(`Disability (Household Head)`) %>%
  drop_na(`Unit Composition`) %>%
  drop_na(`Homelessness`) %>%
  drop_na(`Expedited Service`) %>%
  drop_na(`Application or Renewal`) %>%
  drop_na(`Status of Error Findings`)

type_to_responsibility <- list(
  "Information not reported by client" = "Client Errors",
  "Incomplete or incorrect information provided, but agency not required to verify" = "Client Errors",
  "Information withheld by client (case referred for Intentional Program Violation [IPV] investigation)" = "Client Errors",
  "Inaccurate information reported by collateral contact" = "Client Errors",
  
  "Policy incorrectly applied" = "Agency Errors",
  "Agency failed to verify required information" = "Agency Errors",
  "Agency failed to follow up on inconsistent or incomplete information" = "Agency Errors",
  "Reported information disregarded or not applied" = "Agency Errors",
  "Agency failed to follow up on impending changes" = "Agency Errors",
  "Acted on incorrect Federal computer match information not requiring verification" = "Agency Errors",
  
  "Computer-generated mass change" = "Technical Errors",
  "Data entry and/or coding error" = "Technical Errors",
  "Computer programming error" = "Technical Errors",
  "Arithmetic computation error" = "Technical Errors",
  "Computer user error" = "Technical Errors"
)

ui <- fluidPage(
  title = "SNAP Quality Control Viewer",
  
  tags$head(
    includeHTML("google-analytics.html"),
    tags$style(HTML("
  /* General Styling for the Page */
  h1 {
    color: navy !important; 
    font-weight: bold;
    font-family: 'Arial', sans-serif; 
    font-size: 24px; 
    margin-bottom: 10px;
  }
  p {
    font-size: 14px; 
    font-family: 'Arial', sans-serif;
    color: #666666;
    margin-bottom: 15px;
  }
  .section-description {
    font-size: 14px; 
    color: #666666;
    font-family: 'Arial', sans-serif;
    margin-bottom: 20px;
  }

  /* Navbar Styling */
  .navbar-nav {
    margin-left: 0 !important; 
    padding-left: 0 !important; 
  }
  .navbar-default .navbar-nav > li > a {
    font-size: 14px; 
    font-weight: bold;
    font-family: 'Arial', sans-serif; 
  }

  /* Sticky Footer Styling */
  html, body, #shiny-content {
    height: 100%; 
    margin: 0; 
    padding: 0; 
    display: flex; 
    flex-direction: column; 
  }
  #footer-container {
    flex: 1; 
    display: flex;
    flex-direction: column;
  }
  #footer {
    margin-top: 60px; 
    padding: 10px;
    background-color: #f9f9f9;
    border-top: 1px solid #ddd;
    font-size: 12px;
    color: gray;
  }
  #footer a {
    color: navy;
    text-decoration: none;
  }
  #footer a:hover {
    text-decoration: underline;
  }

  /* Styling for Sankey Links */
  .sankey-link {
    fill-opacity: 0.2; /* Default transparency for all links */
    transition: fill 0.2s, fill-opacity 0.2s; /* Smooth transition */
  }
  .sankey-link:hover {
    fill: gold !important; /* Highlighted color on hover */
    fill-opacity: 1 !important; /* Full opacity on hover */
  }
"))
    
  
  ),

  div(
    id = "footer-container",
    div(
      style = "flex: 1;",
      titlePanel(
        h1("SNAP Quality Control Error Viewer")
      ),
      div(
        class = "section-description",
        p(HTML(
          "This application visualizes Quality Control (QC) errors in Supplemental Nutrition Assistance Program (SNAP) cases, based on a nationally representative sample reviewed by state and federal auditors. 
    Use the <strong>Error Pathways</strong> tab to examine how errors occurred and what caused them, 
    <strong>Error Demographics</strong> to see which groups were most affected, 
    and <strong>Error Severity</strong> to assess the financial impact of different error categories.
          The <strong>Pivot Table</strong> tab provides a customizable table that summarizes SNAP QC errors by selected categories."
        ))
      ),
      
      navbarPage(
        title = NULL,
        id = "nav",

        # Sankey Diagram Tab
        tabPanel(
          "Error Pathways",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectizeInput(
                "year_sankey", "Select Year(s):",
                choices = sort(unique(df_error$year), decreasing = TRUE),
                multiple = TRUE, selected = c(2023, 2022, 2021, 2020, 2019, 2018, 2017)
              ),
              bsTooltip(
                "year_sankey",
                "If multiple years are selected, the displayed plot will be based on aggregated data.",
                placement = "top", trigger = "hover"
              ),
              
              selectizeInput(
                "combination", "Select Error Pathway to Visualize:",
                choices = c("Element → Nature → Type", "Element → Nature", "Element → Type", "Nature → Type"),
                selected = "Element → Nature → Type"
              ),
              bsTooltip(
                "combination",
                "Error element refers to what went wrong, error nature refers to how it happened, and error type refers to why it happened.",
                placement = "top", trigger = "hover"
              ),
              
              radioButtons(
                "flow_by", "Flow by:",
                choices = c("Occurrences" = "count", "Dollar amount" = "dollars"),
                selected = "count", inline = TRUE
              ),
              bsTooltip(
                "flow_by",
                "Choose what the link width represents: number of case-errors or total dollars in error.",
                placement = "top", trigger = "hover"
              ),
              
              uiOutput("dynamicFilters"),
              
              selectizeInput(
                "state_sankey", "Select State:",
                choices = c("All States", sort(unique(as.character(df_error$State))))
              ),
              
              uiOutput("threshold_ui"),

              bsTooltip(
                "threshold",
                "Hide small flows. This threshold applies to the selected metric (occurrences or dollars).",
                placement = "top", trigger = "hover"
              ),
              
              selectizeInput(
                "status_filter", "Filter by Error Status:",
                choices = c("All", "Underissuance", "Overissuance"),
                selected = "All"
              ),
              bsTooltip(
                "status_filter",
                "Error status refers to whether the error led to an under- or overissuance.",
                placement = "top", trigger = "hover"
              )
            ),
            mainPanel(
              width = 9,
              div(
                style = "position: relative; width: 100%;",
                conditionalPanel(
                  condition = "input.combination == 'Element → Type'",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px; height: auto;",
                    h5(HTML("<span style='color: black; font-weight: bold;'>What went wrong<br>(Error Element)</span>")),
                    h5(HTML("<span style='color: black; font-weight: bold;'>Why it happened<br>(Error Type)</span>"))
                  )
                ),
                conditionalPanel(
                  condition = "input.combination == 'Nature → Type'",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px; height: auto;",
                    h5(HTML("<span style='color: black; font-weight: bold;'>How it happened<br>(Error Nature)</span>")),
                    h5(HTML("<span style='color: black; font-weight: bold;'>Why it happened<br>(Error Type)</span>"))
                  )
                ),
                conditionalPanel(
                  condition = "input.combination == 'Element → Nature'",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px; height: auto;",
                    h5(HTML("<span style='color: black; font-weight: bold;'>What went wrong<br>(Error Element)</span>")),
                    h5(HTML("<span style='color: black; font-weight: bold;'>How it happened<br>(Error Nature)</span>"))
                  )
                ),
                conditionalPanel(
                  condition = "input.combination == 'Element → Nature → Type'",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 10px; height: auto;",
                    h5(HTML("<span style='color: black; font-weight: bold;'>What went wrong<br>(Error Element)</span>")),
                    h5(HTML("<span style='color: black; font-weight: bold; text-align: center;'>How it happened<br>(Error Nature)</span>")),
                    h5(HTML("<span style='color: black; font-weight: bold;'>Why it happened<br>(Error Type)</span>"))
                  )
                ),
                plotlyOutput("sankeyPlot", height = "700px")  
              )
            )
          )
        ),
        
        # Bar Chart Tab
        tabPanel(
          "Error Demographics",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              selectInput("year_bar", "Select Year(s):", 
                          choices = sort(unique(df$year), decreasing = TRUE), 
                          multiple = TRUE, selected = c(2023, 2022, 2021, 2020, 2019, 2018, 2017)),
              bsTooltip("year_bar", 
                        "If multiple years are selected, the displayed plot will be based on aggregated data.", 
                        placement = "top", trigger = "hover"),
              
              radioButtons("error_selection", "Select Error Category:", choices = c("Type", "Nature", "Element")),
              uiOutput("error_dropdown_bar"),
              
              selectizeInput("state_bar", "Select State(s):",
                             choices = c("All States", sort(unique(as.character(df_error$State)))),
                             multiple = TRUE,
                             selected = "All States"),
              
              selectInput("demographic", "Select Demographic:", 
                          choices = c("Employment", "Race", "Gender", "Age", "Disability (Household Head)", 
                                      "Disability (Household Member)", "Unit Composition", "Homelessness", 
                                      "Expedited Service", "Application or Renewal", "Status of Error Findings"), 
                          selected = "Employment"),
              checkboxInput(inputId = "include_other_errors", label = "Include Other Errors for comparison", value = TRUE),
              bsTooltip(id = "include_other_errors", 
                        title = "Check this box to compare the demographic distribution for your selected error against all other errors. This helps you see whether the pattern is error-specific or reflects a general trend.",
                        placement = "top", trigger = "hover")
              
            ),
            mainPanel(
              width = 9,
              conditionalPanel(
                condition = "(input.error_selection && input.demographic != '')",
                plotOutput("barPlot", height = "761px", width = "100%")
              )
            )
          )
        ),
        
        # Error Severity Tab
        tabPanel(
          "Error Severity",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              
              selectInput("year_amt", "Select Year(s):", 
                          choices = sort(unique(df_error$year), decreasing = TRUE),
                          multiple = TRUE, selected = max(df_error$year)),
              bsTooltip("year_amt", 
                        "Select one or more years to include in the analysis.",
                        placement = "top", trigger = "hover"),
              
              selectInput("state_amt", "Select State:", 
                          choices = c("All States", sort(unique(as.character(df_error$State)))),
                          selected = "All States"),
              bsTooltip("state_amt", 
                        "Choose a state to filter results, or select 'All States' to see national data.",
                        placement = "top", trigger = "hover"),
              
              selectInput("status_amt", "Select Error Status:",
                          choices = c("All", "Underissuance", "Overissuance"),
                          selected = "All"),
              bsTooltip("status_amt", 
                        "Error status refers to whether the error led to an under- or overissuance.",
                        placement = "top", trigger = "hover"),
              
              radioButtons("error_responsibility", "Error Responsibility:",
                           choices = c("All", "Client Errors", "Agency Errors", "Technical Errors"),
                           selected = "All", inline = FALSE),
              
              selectInput("view_amt_by", "Group By:",
                          choices = c("Error Type", "Error Nature", "Error Element"),
                          selected = "Error Type"),
              bsTooltip("view_amt_by", 
                        "Choose how to group errors for comparison.",
                        placement = "top", trigger = "hover"),
              selectInput("amt_sort_by", "Sort By:",
                          choices = c("Average Dollar Amount", "Number of Cases"),
                          selected = "Average Dollar Amount"),
              
              radioButtons("amt_top_n", "Display Top:",
                           choices = c(5, 10, 20), selected = 5, inline = TRUE)
              
            ),
            
            mainPanel(
              width = 9,
              
              tags$p(
                style = "font-size: 13px; color: gray;",
                "This plot shows the average dollar amount associated with each type of error. Dot size reflects how many unique cases involved that error. 
        Results are filtered by year, state, and error status."
              ),
              
              plotlyOutput("errorDollarPlot", height = "655px"),
              br(),
              downloadButton("download_dollar_data", "Download Data", class = "btn-primary"),
              br(), br(),
              textOutput("totalDollarSummary")
            )
          )
        ),
        
      # Pivot Table Tab
      tabPanel(
        "Pivot Table",
        sidebarLayout(
          sidebarPanel(
            width = 3,

            h4("Filter data"),
            pickerInput(
              inputId = "filter_vars",
              label   = "Choose filters:",
              choices = c("Error Responsibility", "Error Type", "Error Nature", "Error Element",
                          "Error Discovery", "Error Timing", "Status of Error Findings",
                          "Case Type", "Year", "State"),
              selected = c("State", "Year"),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                liveSearch = TRUE,
                `selected-text-format` = "count > 2",
                `none-selected-text` = "None"
              )
            ),
            bsTooltip(
              id = "filter_vars",
              title = "Choose which fields to filter. Leaving this empty includes ALL data.",
              placement = "top",
              trigger = "hover"
            ),

            uiOutput("dynamic_filters"),

            tags$hr(),

            h4("Configure pivot"),
            selectInput(
              "pivot_rows", "Select row:",
              choices = c("Error Responsibility", "Error Type", "Error Nature", "Error Element",
                          "Error Discovery", "Error Timing", "Status of Error Findings",
                          "Case Type", "Year", "State"),
              selected = "Error Type"
            ),
            selectInput(
              "pivot_cols", "Select column:",
              choices = c("Error Responsibility", "Error Type", "Error Nature", "Error Element",
                          "Error Discovery", "Error Timing", "Status of Error Findings",
                          "Case Type", "Year", "State"),
              selected = "Error Discovery"
            ),

            tags$hr()
          ),

          # Main panel
          mainPanel(
            width = 9,

            tags$style(HTML("
              #pvt_title .pvt-heading { font-size: 1.30rem; font-weight: 700; margin: 0 0 2px 0; }
              #pvt_title .pvt-sub     { color: #6c757d; font-size: 0.96rem;  margin: 0 0 8px 0; }
              #pvt_wrap {
                max-width: 1500px;
                overflow: auto;
                border: 1px solid #e6eef5; border-radius: 8px; padding: 6px; background: #ffffff;
              }
              .pvt_notes { color:#5f6b76; font-size: 0.96rem; margin-top: 8px; }
            ")),

            uiOutput("pvt_title"),

            div(
              id = "pvt_wrap",
              pivottablerOutput("pvt_table", width = "100%", height = "685px")
            ),

            div(
              class = "pvt_notes",
              "Notes: Each row in the dataset represents a unique case–error combination, ",
              "so a single case may appear more than once if it involves multiple types of errors."
            )
          )
        )
      ),


      # Base Rate Tab
      tabPanel(
        "Base Rates",
        sidebarLayout(
          sidebarPanel(
            width = 3,

            h4("Filter data"),
            pickerInput(
              inputId = "br_filter_vars",
              label   = "Choose filters:",
              choices = c("Error Responsibility", "Error Type", "Error Nature", "Error Element",
                          "Error Discovery", "Error Timing", "Status of Error Findings",
                          "Case Type", "Year", "State"),
              selected = c("State", "Year"),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                liveSearch = TRUE,
                `selected-text-format` = "count > 2",
                `none-selected-text` = "None"
              )
            ),
            bsTooltip(
              id = "br_filter_vars",
              title = "Choose which fields to filter. Leaving this empty includes ALL data.",
              placement = "top",
              trigger = "hover"
            ),

            uiOutput("br_dynamic_filters"),

            tags$hr(),

            h4("Base rates"),
            pickerInput(
              inputId  = "br_base_group_vars",
              label    = "Break down base rates by:",
              choices  = c(
                "State", "Year", "Case Type", "Status of Error Findings",
                "Error Type", "Error Nature", "Error Element"
              ),
              selected = c("State", "Year"),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                liveSearch = TRUE,
                `selected-text-format` = "count > 2",
                `none-selected-text` = "Overall (no breakdown)"
              )
            )
          ),

          mainPanel(
            width = 9,
            tags$h4("Base error rates"),
            DT::DTOutput("base_rates_table")
          )
        )
      )
    ),
    
    
    tags$div(
      id = "footer",
      style = "margin-top: 60px;",
      p(
        "Data Source: ",
        tags$a(href = "https://www.fns.usda.gov/snap/qc/database", target = "_blank", "SNAP Quality Control Data. "),
        "For the sankey diagram, nodes are ordered in descending frequency. ",
        "For the bar chart, percentages that round to 0% will not be displayed.",
        "For illustrative examples of how error element, nature, and type can come together to form a possible error scenario, refer to ",
        tags$a(href = "https://docs.google.com/spreadsheets/d/1zLF0h30Ic2Y9cv4sRDV3UTuInR-QgeN9RWrvRA0bT3k/edit?usp=sharing", 
               target = "_blank", "this reference sheet."),
        tags$br(),
        "Developed by: Zhaowen Guo, Better Government Lab, Georgetown University"
      )
    ) 
  )
)
)
  
  

server <- function(input, output, session) {
  
  element_choices <- list(
    "Select All" = "Select All",
    "Individual and Household Eligibility" = c(
      "Student status",
      "Citizenship and noncitizen status",
      "Residency",
      "Unit composition",
      "Recipient disqualification",
      "Social Security number"
    ),
    "Employment and Work Requirements" = c(
      "Employment and training programs",
      "Time-limited participation",
      "Work registration requirements",
      "Voluntary quit/reduced work effort",
      "Workfare and comparable workfare",
      "Employment status/job availability",
      "Acceptance of employment"
    ),
    "Resources and Assets" = c(
      "Bank accounts or cash on hand",
      "Nonrecurring lump-sum payment",
      "Other liquid assets",
      "Real property",
      "Vehicles",
      "Other nonliquid resources",
      "Combined resources"
    ),
    "Income Sources and Deductions" = c(
      "Wages and salaries",
      "Self-employment",
      "Other earned income",
      "Earned income deductions",
      "Dependent care deduction",
      "RSDI benefits",
      "Veterans' benefits",
      "SSI and/or State SSI supplement",
      "Unemployment compensation",
      "Workers' compensation",
      "Other government benefits",
      "Contributions",
      "Deemed income",
      "TANF, PA, or GA",
      "Educational grants/scholarships/loans",
      "Other unearned income",
      "Child support payments received from absent parent"
    ),
    "Deductions and Allowances" = c(
      "Standard deduction",
      "Shelter deduction",
      "Standard utility allowance",
      "Medical expense deductions",
      "Child support payment deduction"
    ),
    "Income and Benefit Calculation" = c(
      "Combined gross income",
      "Combined net income",
      "Arithmetic computation",
      "Transitional benefits"
    ),
    "Others (Element)" = c(
      "Reporting systems",
      "SNAP simplification project",
      "Demonstration projects"
    )
  )
  
  nature_choices <- list(
    "Select All" = "Select All",
    "Participation and Reporting Errors" = c(
      "Unit improperly participating under retrospective budgeting",
      "Unit improperly participating under prospective budgeting",
      "Unit improperly participating under monthly reporting",
      "Unit improperly participating under quarterly reporting",
      "Unit improperly participating under semiannual reporting",
      "Unit improperly participating under change reporting",
      "Unit improperly participating under status reporting",
      "Unit improperly participating under 5 hour reporting",
      "Unit improperly participating in transitional benefits"
    ),
    "Deductions and Benefits Calculation Errors" = c(
      "Deduction that should have been included was not",
      "Deduction included that should not have been",
      "Incorrect standard used",
      "Incorrect amount used resulting from change in residence",
      "Incorrect standard used resulting from change in unit size",
      "Benefit/allotment/eligibility incorrectly computed",
      "Incorrect use of allotment tables",
      "Improper prorating of initial month’s benefits"
    ),
    "Income Calculation and Reporting Errors" = c(
      "Unreported source of income",
      "Rounding used/not used or incorrectly applied",
      "All income from source known but not included",
      "More income received from this source than budgeted",
      "Employment status changed from unemployed to employed",
      "Employment status changed from employed to unemployed",
      "Change only in amount of earnings",
      "Conversion to monthly amount not used or incorrectly applied",
      "Averaging not used or incorrectly applied",
      "Less income received from this source than budgeted",
      "Cost of doing business not used or incorrectly applied",
      "Failed to consider/anticipate month with extra pay date"
    ),
    "Resource and Income Limit Errors" = c(
      "Incorrect resource limit applied",
      "Resource should have been excluded",
      "Incorrect income limit applied",
      "Exceeds prescribed limit",
      "Resource should have been included",
      "Failed to consider or incorrectly considered income of ineligible member"
    ),
    "Eligibility and Inclusion/Exclusion Errors" = c(
      "Eligible person(s) excluded",
      "Ineligible person(s) included",
      "Eligible person(s) with no income, resources, or deductible expenses excluded",
      "Eligible person(s) with income excluded",
      "Eligible person(s) with resources excluded",
      "Eligible person(s) with deductible expenses excluded",
      "Newborn improperly excluded",
      "Eligible noncitizen excluded",
      "Ineligible noncitizen included"
    ),
    "Others (Nature)" = c(
      "Transcription or computation errors",
      "Child support payment(s) not considered or incorrectly applied for initial month(s) of eligibility",
      "Retained child support payment(s) not considered or incorrectly applied",
      "Variance/errors resulting from noncompliance with this means-tested public assistance program",
      "Incorrectly prorated",
      "Variances resulting from use of automatic Federal information exchange system",
      "Pass-through not considered or incorrectly applied"
    )
  )
  
  type_choices <- list(
    "Select All" = "Select All",
    "Client Errors" = c("Information not reported by client", 
                        "Incomplete or incorrect information provided, but agency not required to verify",
                        "Information withheld by client (case referred for Intentional Program Violation [IPV] investigation)",
                        "Inaccurate information reported by collateral contact"),
    "Agency Errors" = c("Policy incorrectly applied", 
                        "Agency failed to verify required information", 
                        "Agency failed to follow up on inconsistent or incomplete information", 
                        "Reported information disregarded or not applied", 
                        "Agency failed to follow up on impending changes",
                        "Acted on incorrect Federal computer match information not requiring verification"),
    "Technical Errors" = c("Computer-generated mass change", 
                           "Data entry and/or coding error", 
                           "Computer programming error", 
                           "Arithmetic computation error", 
                           "Computer user error")
  )
  
  output$dynamicFilters <- renderUI({
    req(input$combination)
    
    if (input$combination == "Element → Type") {
      fluidRow(
        column(6, selectInput("elementFilter", "Select Element:", choices = element_choices, multiple = TRUE, selected = "Select All")),
        column(6, selectInput("typeFilter", "Select Type:", choices = type_choices, multiple = TRUE, selected = "Select All"))
      )
    } else if (input$combination == "Nature → Type") {
      fluidRow(
        column(6, selectInput("natureFilter", "Select Nature:", choices = nature_choices, multiple = TRUE, selected = "Select All")),
        column(6, selectInput("typeFilter", "Select Type:", choices = type_choices, multiple = TRUE, selected = "Select All"))
      )
    } else if (input$combination == "Element → Nature") {
      fluidRow(
        column(6, selectInput("elementFilter", "Select Element:", choices = element_choices, multiple = TRUE, selected = "Select All")),
        column(6, selectInput("natureFilter", "Select Nature:", choices = nature_choices, multiple = TRUE, selected = "Select All"))
      )
    } else if (input$combination == "Element → Nature → Type") {
      fluidRow(
        column(6, selectInput("elementFilter", "Select Element:", choices = element_choices, multiple = TRUE, selected = "Select All")),
        column(6, selectInput("natureFilter", "Select Nature:", choices = nature_choices, multiple = TRUE, selected = "Select All")),
        column(12, selectInput("typeFilter", "Select Type:", choices = type_choices, multiple = TRUE, selected = "Select All"))
      )
    }
  })
  
  AMOUNT_COL <- "Dollar Amount in Error" 
  
  output$threshold_ui <- renderUI({
    req(input$flow_by)
    if (input$flow_by == "count") {
      numericInput("threshold", "Filter flows ≥ (occurrences):", value = 1000, min = 0, step = 1)
    } else {
      numericInput("threshold", "Filter flows ≥ (dollars):", value = 10000, min = 0, step = 1)
    }
  })
  
  # Metric-aware aggregator (counts vs dollars)
  summarise_links <- function(df, ..., mode = c("count", "dollars"), use_abs = TRUE) {
    mode <- match.arg(mode)
    grps <- rlang::quos(...)
    if (mode == "count") {
      df %>% dplyr::group_by(!!!grps) %>%
        dplyr::summarise(value = dplyr::n(), .groups = "drop")
    } else {
      df %>% dplyr::group_by(!!!grps) %>%
        dplyr::summarise(
          value = {
            v <- .data[[AMOUNT_COL]]
            if (use_abs) sum(abs(v), na.rm = TRUE) else sum(v, na.rm = TRUE)
          },
          .groups = "drop"
        )
    }
  }
  
  # Sankey render
  output$sankeyPlot <- renderPlotly({
    req(input$combination, input$flow_by)
    
    thr <- if (!is.null(input$threshold) && !is.na(input$threshold)) {
      as.numeric(input$threshold)
    } else if (identical(input$flow_by, "dollars")) {
      10000   
    } else {
      1000
    }
    
    filtered_data <- df_error %>% dplyr::filter(year %in% as.numeric(input$year_sankey))
    if (input$state_sankey != "All States") {
      filtered_data <- filtered_data %>% dplyr::filter(State == input$state_sankey)
    }
    if (!is.null(input$typeFilter) && !("Select All" %in% input$typeFilter)) {
      filtered_data <- filtered_data %>% dplyr::filter(Type %in% input$typeFilter)
    }
    if (!is.null(input$elementFilter) && !("Select All" %in% input$elementFilter)) {
      filtered_data <- filtered_data %>% dplyr::filter(Element %in% input$elementFilter)
    }
    if (!is.null(input$natureFilter) && !("Select All" %in% input$natureFilter)) {
      filtered_data <- filtered_data %>% dplyr::filter(Nature %in% input$natureFilter)
    }
    if (!is.null(input$status_filter) && input$status_filter != "All") {
      filtered_data <- filtered_data %>% dplyr::filter(`Status of Error Findings` == input$status_filter)
    }
    shiny::validate(need(nrow(filtered_data) > 0, "No data for the current filters."))
    
    mode_now <- if (identical(input$flow_by, "dollars")) "dollars" else "count"
    use_abs  <- TRUE
    nodes <- links <- NULL
    
    if (input$combination == "Nature → Type") {
      links_nt <- summarise_links(filtered_data %>% dplyr::filter(!is.na(Nature), !is.na(Type)),
                                  Nature, Type, mode = mode_now, use_abs = use_abs) %>%
        dplyr::filter(value > thr)
      shiny::validate(need(nrow(links_nt) > 0, "No flows pass the threshold."))
      
      df_nat  <- links_nt %>% dplyr::group_by(Nature) %>% dplyr::summarise(flow = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(flow))
      df_type <- links_nt %>% dplyr::group_by(Type)   %>% dplyr::summarise(flow = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(flow))
      
      nodes <- data.frame(
        name = c(df_nat$Nature, df_type$Type),
        x = c(rep(0.01, nrow(df_nat)), rep(0.99, nrow(df_type))),
        y = c(seq(0.01, 0.99, length.out = nrow(df_nat)),
              seq(0.01, 0.99, length.out = nrow(df_type)))
      )
      links <- links_nt %>%
        dplyr::mutate(
          source = match(Nature, nodes$name) - 1,
          target = match(Type,   nodes$name) - 1
        ) %>%
        dplyr::select(source, target, value)
      
    } else if (input$combination == "Element → Nature") {
      links_en <- summarise_links(filtered_data %>% dplyr::filter(!is.na(Element), !is.na(Nature)),
                                  Element, Nature, mode = mode_now, use_abs = use_abs) %>%
        dplyr::filter(value > thr)
      shiny::validate(need(nrow(links_en) > 0, "No flows pass the threshold."))
      
      df_ele <- links_en %>% dplyr::group_by(Element) %>% dplyr::summarise(flow = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(flow))
      df_nat <- links_en %>% dplyr::group_by(Nature)  %>% dplyr::summarise(flow = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(flow))
      
      nodes <- data.frame(
        name = c(df_ele$Element, df_nat$Nature),
        x = c(rep(0.01, nrow(df_ele)), rep(0.99, nrow(df_nat))),
        y = c(seq(0.01, 0.99, length.out = nrow(df_ele)),
              seq(0.01, 0.99, length.out = nrow(df_nat)))
      )
      links <- links_en %>%
        dplyr::mutate(
          source = match(Element, nodes$name) - 1,
          target = match(Nature,  nodes$name) - 1
        ) %>%
        dplyr::select(source, target, value)
      
    } else if (input$combination == "Element → Type") {
      links_et <- summarise_links(filtered_data %>% dplyr::filter(!is.na(Element), !is.na(Type)),
                                  Element, Type, mode = mode_now, use_abs = use_abs) %>%
        dplyr::filter(value > thr)
      shiny::validate(need(nrow(links_et) > 0, "No flows pass the threshold."))
      
      df_ele <- links_et %>% dplyr::group_by(Element) %>% dplyr::summarise(flow = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(flow))
      df_typ <- links_et %>% dplyr::group_by(Type)    %>% dplyr::summarise(flow = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(flow))
      
      nodes <- data.frame(
        name = c(df_ele$Element, df_typ$Type),
        x = c(rep(0.01, nrow(df_ele)), rep(0.99, nrow(df_typ))),
        y = c(seq(0.01, 0.99, length.out = nrow(df_ele)),
              seq(0.01, 0.99, length.out = nrow(df_typ)))
      )
      links <- links_et %>%
        dplyr::mutate(
          source = match(Element, nodes$name) - 1,
          target = match(Type,    nodes$name) - 1
        ) %>%
        dplyr::select(source, target, value)
      
    } else if (input$combination == "Element → Nature → Type") {
      base <- filtered_data %>% dplyr::filter(!is.na(Element), !is.na(Nature), !is.na(Type))
      
      links_en <- summarise_links(base, Element, Nature, mode = mode_now, use_abs = use_abs) %>%
        dplyr::filter(value > thr)
      links_nt <- summarise_links(base, Nature,  Type,   mode = mode_now, use_abs = use_abs) %>%
        dplyr::filter(value > thr)
      shiny::validate(need(nrow(links_en) > 0 || nrow(links_nt) > 0, "No flows pass the threshold."))
      
      element_totals <- links_en %>% dplyr::group_by(Element) %>% dplyr::summarise(total = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(total))
      nature_totals  <- dplyr::bind_rows(
        links_en %>% dplyr::transmute(Nature, value),
        links_nt %>% dplyr::transmute(Nature, value)
      ) %>% dplyr::group_by(Nature) %>% dplyr::summarise(total = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(total))
      type_totals    <- links_nt %>% dplyr::group_by(Type) %>% dplyr::summarise(total = sum(value), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(total))
      
      nodes <- data.frame(
        name = c(element_totals$Element, nature_totals$Nature, type_totals$Type),
        x = c(rep(0.01, nrow(element_totals)),
              rep(0.50, nrow(nature_totals)),
              rep(0.99, nrow(type_totals))),
        y = c(seq(0.01, 0.99, length.out = nrow(element_totals)),
              seq(0.01, 0.99, length.out = nrow(nature_totals)),
              seq(0.01, 0.99, length.out = nrow(type_totals)))
      )
      
      links <- dplyr::bind_rows(
        links_en %>%
          dplyr::mutate(
            source = match(Element, nodes$name) - 1,
            target = match(Nature,  nodes$name) - 1
          ) %>%
          dplyr::select(source, target, value),
        links_nt %>%
          dplyr::mutate(
            source = match(Nature, nodes$name) - 1,
            target = match(Type,   nodes$name) - 1
          ) %>%
          dplyr::select(source, target, value)
      )
    }
    
    link_hover <- if (mode_now == "count")
      "%{source.label} → %{target.label}: %{value:,.0f} case-errors<extra></extra>"
    else
      "%{source.label} → %{target.label}: $%{value:,.0f}<extra></extra>"
    
    node_hover <- if (mode_now == "count")
      "%{label}<br>Total: %{value:,.0f} case-errors<extra></extra>"
    else
      "%{label}<br>Total: $%{value:,.0f}<extra></extra>"
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      domain = list(x = c(0, 1), y = c(0, 1)),
      link = list(
        source = links$source,
        target = links$target,
        value  = links$value,
        color  = "rgba(220, 220, 220, 0.8)",
        hovertemplate = link_hover
      ),
      node = list(
        label = nodes$name,
        x = nodes$x,
        y = nodes$y,
        pad = 15,
        thickness = 15,
        color = ifelse(
          nodes$x == 0.01, "#d48f90",
          ifelse(nodes$x == 0.50, "#ab84a5", "#d8b847")
        ),
        align = "center",
        hovertemplate = node_hover
      )
    ) %>%
      layout(
        autosize = TRUE,
        hovermode = "x unified",
        margin = list(t = 0, b = 50, l = 0, r = 0),
        font = list(size = 12, color = "black", family = "Arial"),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # Bar Chart
  output$error_dropdown_bar <- renderUI({
    req(input$error_selection)
    
    if (input$error_selection == "Type") {
      
      
      selectInput("error_choice", "Select Error Type:", 
                  choices = list(
                    "Client Errors" = c("Information not reported by client", 
                                        "Incomplete or incorrect information provided, but agency not required to verify",
                                        "Information withheld by client (case referred for Intentional Program Violation [IPV] investigation)",
                                        "Inaccurate information reported by collateral contact"),
                    "Agency Errors" = c("Policy incorrectly applied", 
                                        "Agency failed to verify required information", 
                                        "Agency failed to follow up on inconsistent or incomplete information", 
                                        "Reported information disregarded or not applied", 
                                        "Agency failed to follow up on impending changes",
                                        "Acted on incorrect Federal computer match information not requiring verification"),
                    "Technical Errors" = c("Computer-generated mass change", 
                                           "Data entry and/or coding error", 
                                           "Computer programming error", 
                                           "Arithmetic computation error", 
                                           "Computer user error")
                  ),
                  selected = "Policy incorrectly applied")
    } else if (input$error_selection == "Nature") {
      selectInput("error_choice", "Select Error Nature:", 
                  choices = list(
                    "Participation and Reporting Errors" = c(
                      "Unit improperly participating under retrospective budgeting",
                      "Unit improperly participating under prospective budgeting",
                      "Unit improperly participating under monthly reporting",
                      "Unit improperly participating under quarterly reporting",
                      "Unit improperly participating under semiannual reporting",
                      "Unit improperly participating under change reporting",
                      "Unit improperly participating under status reporting",
                      "Unit improperly participating under 5 hour reporting",
                      "Unit improperly participating in transitional benefits"
                    ),
                    "Deductions and Benefits Calculation Errors" = c(
                      "Deduction that should have been included was not",
                      "Deduction included that should not have been",
                      "Incorrect standard used",
                      "Incorrect amount used resulting from change in residence",
                      "Incorrect standard used resulting from change in unit size",
                      "Benefit/allotment/eligibility incorrectly computed",
                      "Incorrect use of allotment tables",
                      "Improper prorating of initial month’s benefits"
                    ),
                    "Income Calculation and Reporting Errors" = c(
                      "Unreported source of income",
                      "Rounding used/not used or incorrectly applied",
                      "All income from source known but not included",
                      "More income received from this source than budgeted",
                      "Employment status changed from unemployed to employed",
                      "Employment status changed from employed to unemployed",
                      "Change only in amount of earnings",
                      "Conversion to monthly amount not used or incorrectly applied",
                      "Averaging not used or incorrectly applied",
                      "Less income received from this source than budgeted",
                      "Cost of doing business not used or incorrectly applied",
                      "Failed to consider/anticipate month with extra pay date"
                    ),
                    "Resource and Income Limit Errors" = c(
                      "Incorrect resource limit applied",
                      "Resource should have been excluded",
                      "Incorrect income limit applied",
                      "Exceeds prescribed limit",
                      "Resource should have been included",
                      "Failed to consider or incorrectly considered income of ineligible member"
                    ),
                    "Eligibility and Inclusion/Exclusion Errors" = c(
                      "Eligible person(s) excluded",
                      "Ineligible person(s) included",
                      "Eligible person(s) with no income, resources, or deductible expenses excluded",
                      "Eligible person(s) with income excluded",
                      "Eligible person(s) with resources excluded",
                      "Eligible person(s) with deductible expenses excluded",
                      "Newborn improperly excluded",
                      "Eligible noncitizen excluded",
                      "Ineligible noncitizen included"
                    ),
                    "Others" = c(
                      "Transcription or computation errors",
                      "Child support payment(s) not considered or incorrectly applied for initial month(s) of eligibility",
                      "Retained child support payment(s) not considered or incorrectly applied",
                      "Variance/errors resulting from noncompliance with this means-tested public assistance program",
                      "Incorrectly prorated",
                      "Variances resulting from use of automatic Federal information exchange system",
                      "Pass-through not considered or incorrectly applied"
                    )
                  ),
                  selected = "Deduction that should have been included was not")
    } else {
      selectInput("error_choice", "Select Error Element:", 
                  choices = list(
                    "Individual and Household Eligibility" = c(
                      "Student status",
                      "Citizenship and noncitizen status",
                      "Residency",
                      "Unit composition",
                      "Recipient disqualification",
                      "Social Security number"
                    ),
                    "Employment and Work Requirements" = c(
                      "Employment and training programs",
                      "Time-limited participation",
                      "Work registration requirements",
                      "Voluntary quit/reduced work effort",
                      "Workfare and comparable workfare",
                      "Employment status/job availability",
                      "Acceptance of employment"
                    ),
                    "Resources and Assets" = c(
                      "Bank accounts or cash on hand",
                      "Nonrecurring lump-sum payment",
                      "Other liquid assets",
                      "Real property",
                      "Vehicles",
                      "Other nonliquid resources",
                      "Combined resources"
                    ),
                    "Income Sources and Deductions" = c(
                      "Wages and salaries",
                      "Self-employment",
                      "Other earned income",
                      "Earned income deductions",
                      "Dependent care deduction",
                      "RSDI benefits",
                      "Veterans' benefits",
                      "SSI and/or State SSI supplement",
                      "Unemployment compensation",
                      "Workers' compensation",
                      "Other government benefits",
                      "Contributions",
                      "Deemed income",
                      "TANF, PA, or GA",
                      "Educational grants/scholarships/loans",
                      "Other unearned income",
                      "Child support payments received from absent parent"
                    ),
                    "Deductions and Allowances" = c(
                      "Standard deduction",
                      "Shelter deduction",
                      "Standard utility allowance",
                      "Medical expense deductions",
                      "Child support payment deduction"
                    ),
                    "Income and Benefit Calculation" = c(
                      "Combined gross income",
                      "Combined net income",
                      "Arithmetic computation",
                      "Transitional benefits"
                    ),
                    "Others" = c(
                      "Reporting systems",
                      "SNAP simplification project",
                      "Demonstration projects"
                    )
                  ),
                  selected = "Wages and salaries")
    }
  })
  
  data <- reactive({
    req(input$error_choice, input$demographic)
    
    if (is.null(input$year_bar) || length(input$year_bar) == 0) return(NULL)
    
    selected_states <- input$state_bar
    real_states <- setdiff(selected_states, "All States")
    include_all_states <- "All States" %in% selected_states
    
    df_filtered_real <- df %>%
      filter(year %in% input$year_bar) %>%
      filter(State %in% real_states)
    
    error_var <- switch(input$error_selection,
                        "Type" = "error_type",
                        "Nature" = "error_nature",
                        "Element" = "error_element")
    
    selected_error_real <- df_filtered_real %>% filter(.data[[error_var]] == input$error_choice)
    other_errors_real   <- df_filtered_real %>% filter(.data[[error_var]] != input$error_choice)
    
    selected_group <- selected_error_real %>%
      group_by(State, !!sym(input$demographic)) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      group_by(State) %>%
      mutate(Proportion = Count / sum(Count) * 100,
             Group = paste("Selected Error:\n", input$error_choice))
    
    other_group <- NULL
    if (input$include_other_errors) {
      other_group <- other_errors_real %>%
        group_by(State, !!sym(input$demographic)) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        group_by(State) %>%
        mutate(Proportion = Count / sum(Count) * 100,
               Group = "Other Errors")
    }
    
    selected_group_all <- NULL
    other_group_all <- NULL
    if (include_all_states) {
      df_filtered_all <- df %>% filter(year %in% input$year_bar)
      
      selected_error_all <- df_filtered_all %>% filter(.data[[error_var]] == input$error_choice)
      other_errors_all   <- df_filtered_all %>% filter(.data[[error_var]] != input$error_choice)
      
      selected_group_all <- selected_error_all %>%
        group_by(!!sym(input$demographic)) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        mutate(Proportion = Count / sum(Count) * 100,
               Group = paste("Selected Error:\n", input$error_choice),
               State = "All States")
      
      if (input$include_other_errors) {
        other_group_all <- other_errors_all %>%
          group_by(!!sym(input$demographic)) %>%
          summarise(Count = n(), .groups = 'drop') %>%
          mutate(Proportion = Count / sum(Count) * 100,
                 Group = "Other Errors",
                 State = "All States")
      }
    }
    
    df_combined <- bind_rows(
      selected_group,
      other_group,
      selected_group_all,
      other_group_all
    ) %>%
      filter(round(Proportion, 0) > 0) %>%
      mutate(Group = factor(Group, levels = c(paste("Selected Error:\n", input$error_choice), "Other Errors")))
    
    return(df_combined)
  })
  
  
  output$barPlot <- renderPlot({
    req(data())
    
    df_plot <- data() %>%
      mutate(State = factor(State, levels = unique(State)))
    
    p <- ggplot(df_plot, aes(x = Group, y = Proportion, fill = !!sym(input$demographic))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(
        aes(label = ifelse(Proportion > 0, paste0(round(Proportion, 1), "%"), ""),
            group = !!sym(input$demographic)),
        position = position_dodge(width = 0.9),
        vjust = -0.5, color = "black", size = 4
      ) +
      scale_fill_manual(
        values = c(
          "#5b859e", "#1e395f", "#75884b", "#1e5a46",
          "#df8d71", "#af4f2f", "#b38711", "#732f30",
          rep("#59385c", 100)
        ),
        guide = guide_legend(override.aes = list(color = NA))
      ) +
      scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
      labs(x = NULL, y = "Proportion (%)", fill = input$demographic) +
      theme_minimal(base_size = 14)
    
    if (length(unique(df_plot$State)) > 1) {
      p <- p + facet_wrap(~State) +
        theme(strip.text = element_text(size = 16, face = "bold"))
    }
    
    return(p)
  })
  
  

  # Error severity
  output$errorDollarPlot <- renderPlotly({
    req(input$year_amt, input$view_amt_by, input$amt_sort_by, input$amt_top_n)
    
    df_filtered <- df_error %>%
      mutate(ResponsibilityGroup = type_to_responsibility[Type]) %>%
      filter(year %in% input$year_amt) %>%
      filter(if (input$state_amt != "All States") State == input$state_amt else TRUE) %>%
      filter(if (input$status_amt != "All") `Status of Error Findings` == input$status_amt else TRUE) %>%
      filter(if (input$error_responsibility != "All") ResponsibilityGroup == input$error_responsibility else TRUE)
      
    group_var <- switch(input$view_amt_by,
                        "Error Type" = "Type",
                        "Error Nature" = "Nature",
                        "Error Element" = "Element")
    
    if (is.null(group_var) || !(group_var %in% names(df_filtered))) {
      return(plotly_empty(type = "scatter") %>% layout(title = "Invalid grouping selected"))
    }
    
    df_summary <- df_filtered %>%
      filter(!is.na(`Dollar Amount in Error`)) %>%
      select(case_id, !!sym(group_var), `Dollar Amount in Error`) %>%
      distinct() %>%
      group_by(across(all_of(group_var))) %>%
      summarise(
        avg_dollar = mean(`Dollar Amount in Error`, na.rm = TRUE),
        n_cases = n_distinct(case_id),
        .groups = "drop"
      ) %>%
      rename(group = !!sym(group_var))
    
    sort_var <- if (input$amt_sort_by == "Average Dollar Amount") "avg_dollar" else "n_cases"
    top_n <- as.numeric(input$amt_top_n)
    
    df_summary <- df_summary %>%
      arrange(desc(.data[[sort_var]])) %>%
      slice_head(n = top_n) %>%
      mutate(
        label_wrapped = str_wrap(group, width = 25)
      )
    
    
    plot_ly(
      data = df_summary,
      x = ~label_wrapped,
      y = ~avg_dollar,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = ~sqrt(n_cases) * 3,
        color = 'rgba(93, 164, 214, 0.7)',
        line = list(width = 1, color = 'rgba(50,50,50,0.3)')
      ),
      text = ~paste0("<b>", group, "</b><br>",
                     "Avg Dollar Error: $", round(avg_dollar, 1), "<br>",
                     "Number of Cases: ", n_cases),
      hoverinfo = 'text'
    ) %>%
      layout(
        font = list(family = "Arial"),
        title = list(
          text = paste("Error Severity by", input$amt_sort_by),
          y = 0.95,
          font = list(family = "Arial", size = 16)
        ),
        xaxis = list(
          title = list(text = input$view_amt_by, font = list(family = "Arial"), standoff = 25),
          tickangle = 0,
          tickfont = list(family = "Arial", size = 11),
          tickvals = df_summary$label_wrapped,
          ticktext = df_summary$label_wrapped,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = "Average Dollar Amount in Error ($)", font = list(family = "Arial")),
          tickfont = list(family = "Arial")
        ),
        showlegend = FALSE,
        margin = list(b = 160, t = 60)
      ) %>%
      config(
        displayModeBar = FALSE
      )
  })
  
  
  output$download_dollar_data <- downloadHandler(
    filename = function() {
      paste0("error_severity_all_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$year_amt)
      
      df_filtered <- df_error %>%
        filter(year %in% input$year_amt) %>%
        filter(if (input$state_amt != "All States") State == input$state_amt else TRUE) %>%
        filter(if (input$status_amt != "All") `Status of Error Findings` == input$status_amt else TRUE) %>%
        mutate(ResponsibilityGroup = dplyr::recode(Type, !!!type_to_responsibility))
      
      group_var <- switch(input$view_amt_by,
                          "Error Type" = "Type",
                          "Error Nature" = "Nature",
                          "Error Element" = "Element",
                          NULL)
      
      if (is.null(group_var) || !(group_var %in% names(df_filtered))) {
        write.csv(data.frame(Message = "Invalid grouping selected."), file, row.names = FALSE)
        return()
      }
      
      df_summary <- df_filtered %>%
        filter(!is.na(`Dollar Amount in Error`)) %>%
        select(case_id, !!sym(group_var), ResponsibilityGroup, `Dollar Amount in Error`) %>%
        distinct() %>%
        group_by(!!sym(group_var), ResponsibilityGroup) %>%
        summarise(
          Average_Dollar_Error = mean(`Dollar Amount in Error`, na.rm = TRUE),
          Case_Count = n_distinct(case_id),
          .groups = "drop"
        ) %>%
        rename(`Error Category` = !!sym(group_var))
      
      sort_var <- if (input$amt_sort_by == "Average Dollar Amount") "Average_Dollar_Error" else "Case_Count"
      
      df_sorted <- df_summary %>%
        arrange(desc(.data[[sort_var]]))
      
      write.csv(df_sorted, file, row.names = FALSE)
    }
  )
  
  # Pivot Table
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  .sanitize_id <- function(x) gsub("[^A-Za-z0-9_]", "_", tolower(x))
  .pretty_list <- function(x, n = 4) {
    x <- sort(unique(as.character(x)))
    if (!length(x)) return("All")
    if (length(x) <= n) paste(x, collapse = ", ") else paste0(length(x), " selected")
  }
  
  .pretty_years <- function(sel, all_vals) {
    nums <- suppressWarnings(as.integer(sel))
    if (all(!is.na(nums))) {
      nums <- sort(nums)
      if (length(nums) >= 2 && diff(range(nums)) + 1 == length(nums)) {
        return(paste0(min(nums), "–", max(nums)))
      } else {
        return(paste(nums, collapse = ", "))
      }
    }
    .pretty_list(sel)
  }
  
  .error_dim_vars <- c("Error Type", "Error Nature", "Error Element")
  
  .clean_cat_or_na <- function(x) {
    if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
      x <- haven::as_factor(x, levels = "default")
    }
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  
  .get_vals <- function(df, var) {
    if (is.null(var) || !nzchar(var) || !var %in% names(df)) return(character(0))
    v <- df[[var]]
    
    if (var %in% .error_dim_vars) v <- .clean_cat_or_na(v)
    v <- as.character(v)
    v <- trimws(v)
    v[v == ""] <- NA_character_
    v <- v[!is.na(v)]
    sort(unique(v))
  }
  
  output$dynamic_filters <- renderUI({
    req(input$filter_vars)
    
    lapply(input$filter_vars, function(v) {
      df0 <- verification_all
      
      if (!v %in% names(df0)) {
        return(tags$div(
          style = "color:#888; font-size: 12px; margin-bottom: 6px;",
          paste0(v, " (not available in this mode)")
        ))
      }
      
      choices <- .get_vals(df0, v)
      
      pickerInput(
        inputId  = paste0("flt__", .sanitize_id(v)),
        label    = v,
        choices  = choices,
        selected = choices,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          liveSearch = TRUE,
          `selected-text-format` = "count > 2",
          `none-selected-text` = "All"
        )
      )
    })
  })

  output$br_dynamic_filters <- renderUI({
  req(input$br_filter_vars)

    lapply(input$br_filter_vars, function(v) {
      df0 <- if (v %in% .error_dim_vars) base_cat_all else base_case_all

      if (!v %in% names(df0)) {
        return(tags$div(
          style = "color:#888; font-size: 12px; margin-bottom: 6px;",
          paste0(v, " (not available)")
        ))
      }

      choices <- .get_vals(df0, v)

      pickerInput(
        inputId  = paste0("br_flt__", .sanitize_id(v)),
        label    = v,
        choices  = choices,
        selected = choices,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          liveSearch = TRUE,
          `selected-text-format` = "count > 2",
          `none-selected-text` = "All"
        )
      )
    })
  })

  
  filtered_df <- reactive({
    df <- verification_all
    vars <- input$filter_vars %||% character(0)
    
    for (v in vars) {
      if (!v %in% names(df)) next
      id  <- paste0("flt__", .sanitize_id(v))
      sel <- input[[id]]
      if (is.null(sel) || length(sel) == 0) next
      df <- df[as.character(df[[v]]) %in% as.character(sel), , drop = FALSE]
    }
    
    df
  })
  
  base_case_filtered <- reactive({
    df <- base_case_all
    vars <- input$filter_vars %||% character(0)
    
    for (v in vars) {
      if (v %in% .error_dim_vars) next
      if (!v %in% names(df)) next
      
      id  <- paste0("flt__", .sanitize_id(v))
      sel <- input[[id]]
      if (is.null(sel) || length(sel) == 0) next
      
      df <- df[as.character(df[[v]]) %in% as.character(sel), , drop = FALSE]
    }
    
    if (!("Case ID" %in% names(df))) stop("base_case_all must contain a 'Case ID' column (exact name).")
    if (!("has_error" %in% names(df))) stop("base_case_all must contain a 'has_error' TRUE/FALSE column.")
    df$`Case ID` <- as.character(df$`Case ID`)
    df
  })
  
  base_cat_filtered <- reactive({
    cat_df <- base_cat_all
    vars <- input$filter_vars %||% character(0)
    
    if (!("Case ID" %in% names(cat_df))) stop("base_cat_all must contain a 'Case ID' column (exact name).")
    cat_df$`Case ID` <- as.character(cat_df$`Case ID`)
    
    denom_ids <- base_case_filtered() %>% dplyr::select(`Case ID`)
    cat_df <- cat_df %>% dplyr::semi_join(denom_ids, by = "Case ID")
    
    present_dims <- intersect(.error_dim_vars, names(cat_df))
    if (length(present_dims)) {
      cat_df <- cat_df %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(present_dims), .clean_cat_or_na))
    }
    
    for (v in intersect(vars, .error_dim_vars)) {
      if (!v %in% names(cat_df)) next
      id  <- paste0("flt__", .sanitize_id(v))
      sel <- input[[id]]
      if (is.null(sel) || length(sel) == 0) next
      
      cat_df <- cat_df %>%
        dplyr::filter(!is.na(.data[[v]]), .data[[v]] %in% as.character(sel))
    }
    
    cat_df
  })
  
  base_rates_summary <- reactive({
    denom_df <- base_case_filtered()
    cat_df   <- base_cat_filtered()
    grp_raw <- input$base_group_vars %||% character(0)
    err_group <- intersect(grp_raw, .error_dim_vars)
    err_group <- err_group[err_group %in% names(cat_df)]
    
    # overall base error rate
    if (length(err_group) == 0) {
      denom_group <- grp_raw[grp_raw %in% names(denom_df)]
      
      if (length(denom_group) == 0) {
        return(
          denom_df %>%
            dplyr::summarise(
              reviewed_n = dplyr::n(),
              error_n    = sum(has_error, na.rm = TRUE),
              error_rate = dplyr::if_else(reviewed_n > 0, error_n / reviewed_n, NA_real_),
              .groups = "drop"
            )
        )
      } else {
        return(
          denom_df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(denom_group))) %>%
            dplyr::summarise(
              reviewed_n = dplyr::n(),
              error_n    = sum(has_error, na.rm = TRUE),
              error_rate = dplyr::if_else(reviewed_n > 0, error_n / reviewed_n, NA_real_),
              .groups = "drop"
            ) %>%
            dplyr::arrange(dplyr::across(dplyr::all_of(denom_group)))
        )
      }
    }
    
    # category base rates by Error Type/Nature/Element
    denom_group <- intersect(grp_raw, intersect(names(denom_df), names(cat_df)))
    denom_group <- setdiff(denom_group, .error_dim_vars)
    
    denom_s <- if (length(denom_group) == 0) {
      denom_df %>%
        dplyr::summarise(reviewed_n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(.join_key = 1L)
    } else {
      denom_df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(denom_group))) %>%
        dplyr::summarise(reviewed_n = dplyr::n(), .groups = "drop")
    }
    
    cat_df2 <- cat_df
    for (v in err_group) cat_df2 <- cat_df2 %>% dplyr::filter(!is.na(.data[[v]]))
    
    err_s <- {
      gvars <- c(denom_group, err_group)
      out <- cat_df2 %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(gvars))) %>%
        dplyr::summarise(
          error_n = dplyr::n_distinct(`Case ID`),
          .groups = "drop"
        )
      if (length(denom_group) == 0) out <- out %>% dplyr::mutate(.join_key = 1L)
      out
    }
    
    joined <- if (length(denom_group) == 0) {
      dplyr::left_join(err_s, denom_s, by = ".join_key") %>% dplyr::select(-.join_key)
    } else {
      dplyr::left_join(err_s, denom_s, by = denom_group)
    }
    
    joined %>%
      dplyr::mutate(
        error_rate = dplyr::if_else(!is.na(reviewed_n) & reviewed_n > 0, error_n / reviewed_n, NA_real_)
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(denom_group, err_group))))
  })
  
  br_base_case_filtered <- reactive({
    df <- base_case_all
    vars <- input$br_filter_vars %||% character(0)

    for (v in vars) {
      if (v %in% .error_dim_vars) next
      if (!v %in% names(df)) next

      id  <- paste0("br_flt__", .sanitize_id(v))
      sel <- input[[id]]
      if (is.null(sel) || length(sel) == 0) next

      df <- df[as.character(df[[v]]) %in% as.character(sel), , drop = FALSE]
    }

    if (!("Case ID" %in% names(df))) stop("base_case_all must contain a 'Case ID' column (exact name).")
    if (!("has_error" %in% names(df))) stop("base_case_all must contain a 'has_error' TRUE/FALSE column.")
    df$`Case ID` <- as.character(df$`Case ID`)
    df
  })

  br_base_cat_filtered <- reactive({
    cat_df <- base_cat_all
    vars <- input$br_filter_vars %||% character(0)

    if (!("Case ID" %in% names(cat_df))) stop("base_cat_all must contain a 'Case ID' column (exact name).")
    cat_df$`Case ID` <- as.character(cat_df$`Case ID`)

    denom_ids <- br_base_case_filtered() %>% dplyr::select(`Case ID`)
    cat_df <- cat_df %>% dplyr::semi_join(denom_ids, by = "Case ID")

    present_dims <- intersect(.error_dim_vars, names(cat_df))
    if (length(present_dims)) {
      cat_df <- cat_df %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(present_dims), .clean_cat_or_na))
    }

    for (v in intersect(vars, .error_dim_vars)) {
      if (!v %in% names(cat_df)) next
      id  <- paste0("br_flt__", .sanitize_id(v))
      sel <- input[[id]]
      if (is.null(sel) || length(sel) == 0) next

      cat_df <- cat_df %>%
        dplyr::filter(!is.na(.data[[v]]), .data[[v]] %in% as.character(sel))
    }

    cat_df
  })

  br_base_rates_summary <- reactive({
    denom_df <- br_base_case_filtered()
    cat_df   <- br_base_cat_filtered()

    grp_raw <- input$br_base_group_vars %||% character(0)
    err_group <- intersect(grp_raw, .error_dim_vars)
    err_group <- err_group[err_group %in% names(cat_df)]

    # overall base error rate
    if (length(err_group) == 0) {
      denom_group <- grp_raw[grp_raw %in% names(denom_df)]

      if (length(denom_group) == 0) {
        return(
          denom_df %>%
            dplyr::summarise(
              reviewed_n = dplyr::n(),
              error_n    = sum(has_error, na.rm = TRUE),
              error_rate = dplyr::if_else(reviewed_n > 0, error_n / reviewed_n, NA_real_),
              .groups = "drop"
            )
        )
      } else {
        return(
          denom_df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(denom_group))) %>%
            dplyr::summarise(
              reviewed_n = dplyr::n(),
              error_n    = sum(has_error, na.rm = TRUE),
              error_rate = dplyr::if_else(reviewed_n > 0, error_n / reviewed_n, NA_real_),
              .groups = "drop"
            ) %>%
            dplyr::arrange(dplyr::across(dplyr::all_of(denom_group)))
        )
      }
    }

    # category base rates by Error Type/Nature/Element
    denom_group <- intersect(grp_raw, intersect(names(denom_df), names(cat_df)))
    denom_group <- setdiff(denom_group, .error_dim_vars)

    denom_s <- if (length(denom_group) == 0) {
      denom_df %>%
        dplyr::summarise(reviewed_n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(.join_key = 1L)
    } else {
      denom_df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(denom_group))) %>%
        dplyr::summarise(reviewed_n = dplyr::n(), .groups = "drop")
    }

    cat_df2 <- cat_df
    for (v in err_group) cat_df2 <- cat_df2 %>% dplyr::filter(!is.na(.data[[v]]))

    err_s <- {
      gvars <- c(denom_group, err_group)
      out <- cat_df2 %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(gvars))) %>%
        dplyr::summarise(
          error_n = dplyr::n_distinct(`Case ID`),
          .groups = "drop"
        )
      if (length(denom_group) == 0) out <- out %>% dplyr::mutate(.join_key = 1L)
      out
    }

    joined <- if (length(denom_group) == 0) {
      dplyr::left_join(err_s, denom_s, by = ".join_key") %>% dplyr::select(-.join_key)
    } else {
      dplyr::left_join(err_s, denom_s, by = denom_group)
    }

    joined %>%
      dplyr::mutate(
        error_rate = dplyr::if_else(!is.na(reviewed_n) & reviewed_n > 0, error_n / reviewed_n, NA_real_)
      ) %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(denom_group, err_group))))
  })


  output$pvt_title <- renderUI({
    title_txt <- sprintf("%s by %s", input$pivot_rows, input$pivot_cols)
    
    pivot_agg_val <- input$pivot_agg %||% "n_distinct"
    metric_txt <- switch(pivot_agg_val,
                         sum = "Total dollar amount",
                         mean = "Average dollar amount",
                         n_distinct = "Unique cases",
                         "Unique cases")
    
    filter_bits <- character(0)
    vars <- input$filter_vars %||% character(0)
    
    for (v in vars) {
      df0 <- verification_all
      
      if (!v %in% names(df0)) next
      
      all_vals <- .get_vals(df0, v)
      sel_id   <- paste0("flt__", .sanitize_id(v))
      sel      <- input[[sel_id]]
      
      if (is.null(sel) || length(sel) == 0 || length(sel) == length(all_vals)) next
      
      val_txt <- if (tolower(v) == "year") .pretty_years(sel, all_vals) else .pretty_list(sel, n = 3)
      filter_bits <- c(filter_bits, sprintf("%s: %s", v, val_txt))
    }
    
    sub_txt <- if (length(filter_bits) == 0) {
      paste0("Showing all data. Metric: ", tools::toTitleCase(metric_txt), ".")
    } else {
      paste0("Showing ", paste(filter_bits, collapse = " • "), ". ",
             "Metric: ", tools::toTitleCase(metric_txt), ".")
    }
    
    htmltools::tagList(
      tags$div(class = "pvt-heading", title_txt),
      tags$div(class = "pvt-sub", sub_txt)
    )
  })
  
  output$base_rates_table <- DT::renderDT({
    out <- br_base_rates_summary() %>%
      dplyr::mutate(
        `Error rate (%)`      = round(100 * error_rate, 1),
        `Reviewed cases (N)`  = reviewed_n,
        `Error cases (N)`     = error_n
      ) %>%
      dplyr::select(-reviewed_n, -error_n, -error_rate)

    DT::datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  output$pvt_table <- renderPivottabler({
    # req(!isTRUE(input$show_base_rates))
    req(input$pivot_rows, input$pivot_cols)
    
    df <- filtered_df()
    pivot_agg_val <- input$pivot_agg %||% "n_distinct"
    
    pt <- PivotTable$new()
    pt$addData(df)
    pt$addRowDataGroups(input$pivot_rows)
    pt$addColumnDataGroups(input$pivot_cols)
    
    if (pivot_agg_val == "n_distinct") {
      pt$defineCalculation(
        calculationName     = "UniqueCases",
        summariseExpression = "n_distinct(`Case ID`)",
        caption             = "Unique Cases"
      )
    } else if (pivot_agg_val == "mean") {
      pt$defineCalculation(
        calculationName     = "ErrorAmount",
        summariseExpression = "round(mean(`Dollar Amount in Error`, na.rm = TRUE), 1)",
        caption             = "Average Error Amount ($)"
      )
    } else {
      pt$defineCalculation(
        calculationName     = "ErrorAmount",
        summariseExpression = "sum(`Dollar Amount in Error`, na.rm = TRUE)",
        caption             = "Total Error Amount ($)"
      )
    }
    
    pt$theme <- list(
      fontName = "Verdana, Arial",
      fontSize = "0.8em",
      headerBackgroundColor = "rgba(93, 164, 214, 0.2)",
      headerColor = "#000000",
      cellBackgroundColor = "#ffffff",
      cellColor = "#000000",
      outlineCellBackgroundColor = "rgba(0,0,0,0)",
      outlineCellColor = "#000000",
      totalBackgroundColor = "rgba(93, 164, 214, 0.3)",
      totalColor = "#000000",
      borderColor = "rgba(93, 164, 214, 0.5)"
    )
    
    pt$evaluatePivot()
    pivottabler(pt)
  })
}  
  
shinyApp(ui = ui, server = server)
