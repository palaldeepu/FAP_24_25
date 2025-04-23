# Written by Dr. Deepu Palal, free to use and distribute with citation

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# List of required packages
packages <- c("shiny", "RCurl", "jsonlite", "tidyr", "dplyr", "httr")

sapply(packages, install_and_load)
options(nwarnings = 10000)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Data",

    # Application title
    titlePanel("FAP Batch 2024-25 Summary of Data Entry"),
    tags$style(HTML("
        body {
            background-color: #E0FFFF;  /* Very light blue background for the entire page */
        }
         h2 {
            font-weight: bold;
            text-align: center;
            margin-bottom: 40px;
        }    
            
        h3 {
            font-weight: bold;
            text-align: center;
            color: blue;
            margin-bottom: 20px;
        }
        .well {
            background-color: #ADD8E6;  /* Light blue color for sidebar */
        }
        
    ")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput("input_rollno", "Roll No", value = 24001, min = 24001, max = 24250)
        ),

        mainPanel(
          DT::dataTableOutput("summary_student_table")
        )
    )
    
  ),
  tabPanel("Summary",
           titlePanel("FAP Batch 2024-25 Summary of Data Entry"),
           h3("Family Details"),
            DT::dataTableOutput("family_table"),
            h3("Member Details"),
            DT::dataTableOutput("individual_table")
           )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  log_file <- "shiny_log.txt"
  
  log_error <- function(e) {
    write(paste(Sys.time(), "ERROR:", conditionMessage(e)), file = log_file, append = TRUE)
  }
  
  log_warning <- function(w) {
    write(paste(Sys.time(), "WARNING:", conditionMessage(w)), file = log_file, append = TRUE)
    invokeRestart("muffleWarning")  # optional: suppress warning in UI
  }
  
  withCallingHandlers({
    tryCatch({
  

  api_token       <- '5533B9A560D94525EF771C7C26E74999'
  api_url         <- 'https://redcap.dpuerp.in/redcap/api/'
  
  proxy_settings <- use_proxy(
    url = "10.30.70.23",  
    port = 8080,                      
    username = "deepu_palal",  
    password = "abcd"         
  )
  
  response <- POST(
    url = api_url,
 # config = proxy_settings,
    body = list(
      token = api_token,
      content = "record",
      format = "json",
      type = "flat"
    ),
    encode = "form"
  )
  
  records_json <- content(response, "text", encoding = "UTF-8")
  records <- fromJSON(records_json)
  
  records <- records %>%
    separate(col = record_id, into = c("student_id", "family_no"), sep = "-", remove = FALSE)
  
  #### Separate Individual data ####
  
  individual_data <- records %>%
    filter(redcap_repeat_instrument == "individual_member_details") %>%
    select(1:5,28:342)
  
  individual_data <- individual_data %>%
    filter(!(coalesce(fname, "") == "" & coalesce(mname, "") == "" & coalesce(lname, "") == ""))
  
  individual_data <- individual_data %>%
    group_by(record_id) %>%
    mutate(member_number = row_number()) %>%
    ungroup() %>%
    relocate(member_number, .after = 3)
  
  individual_data <- individual_data %>%
    select(record_id, student_id, member_number, fname, mname, lname, age, sex, whether_pregnant, height, weight,
           waist_circum, hip_circum, sbp, dbp, diabetes_mellitus, diabetes_mellitus_rx, hypertension, hypertension_rx)
  
  
  #### Saperate Family data ####
  
  family_data <- records %>%
    filter(redcap_repeat_instrument == "") %>%
    select(1:3,26,6:25)
  
  
  family_data <- family_data %>%
    filter(!(coalesce(hh_fname, "") == "" & 
               coalesce(hh_mname, "") == "" & 
               coalesce(hh_lname, "") == "" & 
               coalesce(household_unique_id, "") == ""))
  
  family_data$family_income[family_data$family_income == 999] <- NA
  
  
  #### Prepare Summary ####
  
  # Household-level summary
  summary_family <- family_data %>%
    transmute(
      Roll_No = user_name,
      Record_ID = record_id,
      Family_Income_Missing = is.na(family_income) | family_income == "",
      Total_members = household_members_no
    )
  
  # Member-level field counts
  summary_individual <- individual_data %>%
    group_by(record_id) %>%
    summarise(
      No_of_members_entered = n(),
      Age = sum(!is.na(age) & age != ""),
      Sex = sum(!is.na(sex) & sex != ""),
      Height = sum(!is.na(height) & height != ""),
      Weight = sum(!is.na(weight) & weight != ""),
      WC = sum(!is.na(waist_circum) & waist_circum != ""),
      HC = sum(!is.na(hip_circum) & hip_circum != ""),
      SBP = sum(!is.na(sbp) & sbp != ""),
      DBP = sum(!is.na(dbp) & dbp != ""),
      DM = sum(!is.na(diabetes_mellitus) & diabetes_mellitus != ""),
      DMRx = sum(!is.na(diabetes_mellitus_rx) & diabetes_mellitus_rx != ""),
      HTN = sum(!is.na(hypertension) & hypertension != ""),
      HTNRx = sum(!is.na(hypertension_rx) & hypertension_rx != ""),
      .groups = "drop"
    )
  
  # Combine both using left_join
  summary <- summary_family %>%
    left_join(summary_individual, by = c("Record_ID" = "record_id"))
  
  
  summary <- summary %>%
    separate(col = Record_ID, into = c("student_id", "family_no"), sep = "-", remove = FALSE) %>%
    select(Roll_No, student_id, Record_ID, 4:19) %>%
    select(-family_no)
  
  #### Reactive Code ####
  
  student_idno <- reactive({
    req(input$input_rollno) 
    
    student_id_found <- family_data %>%
      filter(user_name == input$input_rollno) %>%
      pull(student_id) %>%
      unique()
    
    if (length(student_id_found) == 0) {
      return(1)
    } else {
      return(student_id_found)
    }
  })
  
  family_data_student <- reactive({
    family_data %>%
    filter(student_id == student_idno())
  })
  
  individual_data_student <- reactive({
    individual_data %>%
    filter(student_id == student_idno())
  })
  
  summary_student <- reactive({
    summary %>%
      filter(student_id == student_idno())
  })
  
    
  output$family_table <- renderDataTable(family_data_student (), options = list(pageLength = 10))
  output$individual_table <- renderDataTable(individual_data_student (), options = list(pageLength = 20))
  output$summary_student_table <- renderDataTable(summary_student (), options = list(pageLength = 10))
  
  
    }, error = log_error)
  }, warning = log_warning)
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
