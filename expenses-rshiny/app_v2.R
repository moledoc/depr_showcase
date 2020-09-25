# Check if necessary packages exists.
# If not then download them
check_packages <- function(pkg) {
 new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
 if (length(new.pkg))
  install.packages(new.pkg, dependencies  =  TRUE,repos = "https://ftp.acc.umu.se/mirror/CRAN/") 
 sapply(pkg, require, character.only  =  TRUE)
}

# The following packages are dependencies for this app.
packages <- c("shiny", "shinydashboard", "data.table", "DT", "dplyr", "stringr")
check_packages(packages)

# Set session time to UTC, so the date time is not affected by local time.
Sys.setenv(TZ = "UTC")

# Ensure UTF-8 encoding
options(encoding = "UTF-8")

# If data dir does not exist, create it and make the template files.
if (!dir.exists("data/")) {
 dir.create("data/")
 data <- data.table(
  Date = as.Date(NA_character_),
  Expense = NA_real_,
  Type = NA_character_,
  Description = NA_character_
  )
 data_types <- data.table(
  Type = "Unknown",
  Description = "Unknown"
  )
 data_scratchpad <- data.table("")
 fwrite(x = data, file = "data/data.csv", sep = ",")
 fwrite(x = data_types, file = "data/data_types.csv", sep = ",")
 fwrite(x = data_scratchpad, file = "data/data_scratchpad.csv", sep = ",")

}

# Read in data files.
data <- fread(file = "data/data.csv", sep = ",")
data[, Date :=  as.Date(Date, origin = "1970-01-01")]
data_types <- fread(file  =  "data/data_types.csv", sep = ",")
data_scratchpad <- fread(file  =  "data/data_scratchpad.csv", sep = "")

# UI header
header <- dashboardHeader(title  =  "Expenses")

# UI sidebar
sidebar <- dashboardSidebar (
 sidebarMenu(
  menuItem("Booking", tabName = "booking_tab", icon = icon("book")),
  menuItem("Graphics", tabName = "graphics_tab", icon = icon("bar-chart")
  )
 )
)

# Panels: expense
expense_panel <- tabPanel(
 "Add expense",
 fluidRow(
  box(
   title = strong("Add new expense"),
   dateInput(
    inputId = "date",
    label   = "Date of the expense:",
    width   = 90
   ),
   textInput(
    inputId = "expense",
    label   = "Expense amount:",
    width   = 385,
   ),
   selectInput(
    inputId = "type",
    label   = "Choose type:",
    choices = c(
     "Choose one" = "",
     data_types[, Type %>% unique() %>% sort()]
     ),
    width   = 385,
   ),
   selectInput(
    inputId = "desc",
    label   = "Choose description:",
    choices = "",
    width   = 385,
   ),
   actionButton("submit_new_exp","Submit"),
   width = 3
  ),
  box(
   title = strong("Expenses"),
   column(12, dataTableOutput("show_data"))
  )
 )
)

# Panels: type
type_panel <- tabPanel(
 "Add type/desc",
 fluidRow(
  box(
   textInput(
   inputId = "new_type",
   label = "Insert new type:",
   width = 385
   ),
   textInput(
   inputId = "new_desc",
   label = "Insert new description:",
   width = 385
   ),
   actionButton("add_new_type", "Add"), 
   actionButton("delete_type", "Delete"),
   width = 3
  ),
  box(
  title = strong("Already existing"),
  column(12, dataTableOutput("show_types")),
  width = 8
  )
 )
)

# Panels: scratchpad
scratchpad_panel <- tabPanel(
 "Scratchpad",
 fluidRow(
  box(
   textAreaInput(
    inputId = "desc",
    label   = "",
    width   = "200%",
    height  = "300px",
    value   = paste0(data_scratchpad, collapse = "\n"),
    resize  = "both"
   ),
   actionButton("save_scratchpad", "Save scratchpad"),
   width = 3
  )
 )
)

# dashboard tab: booking
booking_tab <- tabItem(
 tabName = "booking_tab",
 tabBox(
  id = "booking_tab_box",
  width = 12,
  expense_panel,
  scratchpad_panel,
  type_panel
 )
)

# dashboard tab: graphics
graphics_tab <- tabItem(
 tabName = "graphics_tab",
 # TODO
 h2("TODO")
)

# dashboard tabs
tabitems <- tabItems(booking_tab, graphics_tab)

# dashboard body
body <- dashboardBody(tabitems)

# UI
ui <- dashboardPage(header, sidebar, body)

# LOGIC FUNCTIONS
# update 'add expense'
update_exp_tab <- function(input, output, session){
  updateDateInput(
   session = session,
   inputId = "date",
   label   = "Date of the expense:"
  )
  updateTextInput(
   session = session,
   inputId = "expense",
   label   = "Expense amount",
   value   = ""
  )

  updateSelectizeInput(
   session = session,
   inputId = "type",
   choices = c("Choose one" = "", data_types[, Type %>% unique() %>% sort()])
  )

  updateSelectizeInput(
   session = session,
   inputId = "desc",
   choices = c("Choose one" = "", data_types[Type==input$type, Description %>% unique() %>% sort()])
  )

  output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})
}

# update 'type tab'

update_exp_tab <- function(input, output, session){

 updateSelectizeInput(
  session = session,
  inputId = "type",
  choices = c("Choose one" = "", data_types[, Type %>% unique() %>% sort()])
 )
 
 updateSelectizeInput(
  session = session,
  inputId = "desc",
  choices = c("Choose one" = "", data_types[Type==input$type, Description %>% unique() %>% sort()])
 )
 
 output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})
}


# Server
server <- function(input, output, session) {
 observe({
  update_exp_tab(input, output, session)
  update_exp_tab(input, output, session)
 })

 observeEvent(input$submit_new_exp, {
  tryCatch({
    if(!("" %in% c(input$expense, input$type, input$desc))){
     data <- rbindlist(
      list(
       data[!is.na(Date)],
       list(input$date, as.numeric(input$expense), input$type, input$desc)
      )
     )
     assign("data",data,envir = .GlobalEnv)
     output$show_data <- DT::renderDataTable({datatable(data[order(-Date)])})
  
     fwrite(
      x = list( input$date, as.numeric(input$expense), input$type, input$desc),
      file = "data/data.txt",
      append = TRUE,
      sep = ","
     )
     showNotification(
      paste0( "Expense added: (", input$date, ",", input$expense, ",", input$type, ",", input$desc, ")"),
      type = "message", duration = 10)
   } else{
     showNotification(
      "Invalid expense,type or description",
      type = "error",
      duration = 10)
   }
  },
  warning = function(cond){
   showNotification("Invalid expense", type = "error", duration = 10) 
  }
 )
})
 session$onSessionEnded(function() {
  stopApp()
 })
}

# Run app in given port.
shiny::shinyApp(ui, server, options = list(port = 4004))
