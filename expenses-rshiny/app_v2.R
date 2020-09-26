# Check if necessary packages exists.
# If not then download them
check_packages <- function(pkg) {
 new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
 if (length(new.pkg))
  install.packages(new.pkg, dependencies  =  TRUE,repos = "https://ftp.acc.umu.se/mirror/CRAN/") 
 sapply(pkg, require, character.only  =  TRUE)
}

# The following packages are dependencies for this app.
packages <- c("shiny", "shinydashboard", "data.table", "DT", "dplyr", "stringr","ggplot2","lubridate")
check_packages(packages)

# Set session time to UTC, so the date time is not affected by local time.
Sys.setenv(TZ = "UTC")

# Ensure UTF-8 encoding
options(encoding = "UTF-8")

# Do some preformat for dates (for graphics)
cur_date <- Sys.Date()
date_to_helper <- month(cur_date) + 1
date_from_helper <- month(cur_date) - 2
date_to_init <- ymd(
 paste0(
  year(cur_date),
  if_else(date_to_helper < 10, paste0("0", date_to_helper), as.character(date_to_helper)),
  "01"
 )
)
date_from_init <- ymd(
 paste0(
  year(cur_date),
  if_else(date_from_helper < 10, paste0("0", date_from_helper), as.character(date_from_helper)),
  "01"
 )
)

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
 fwrite(x = data_scratchpad, file = "data/data_scratchpad.csv", sep = "")

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
   fluidRow(
    actionButton("submit_new_exp", "Submit"),
    actionButton("submit_del_exp", "Delete"),
    width  = 3,
   ),
   p(),
   selectInput(
    inputId = "delete",
    labe    = "Delete expense:",
    choices = c(
     "Select expenses" = "",
     1:data[, .N]
    ),
    multiple = TRUE
   ),
   width  = 3,
  ),
  box(
   title = strong("Expenses"),
   column(12, dataTableOutput("show_data"))
  )
 )
)

# Panels: type
type_panel <- tabPanel(
 "Add type/description",
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
    inputId = "scratchpad",
    label   = "",
    width   = "200%",
    height  = "300px",
    value   = paste0(
     unlist(data_scratchpad) %>%
      gsub(pattern = "\"", replacement = ""),
     collapse = "\n"
     ),
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
  id     = "booking_tab_box",
  width  = 24,
  expense_panel,
  scratchpad_panel,
  type_panel
 )
)

# dashboard tab: graphics
graphics_tab <- tabItem(
 tabName = "graphics_tab",
 width  = 24,
 tabBox(
  id     = "graphics_tab_box",
  width  = 24,
  height = "100%",
  fluidRow(
   width = "200%",
   box(plotOutput("show_analytics"), width = 9, height = "100%"),
   box(
    width = 3,
    dateInput(
     inputId = "date_from",
     label   = "Date from",
     width   = 90,
     value   = date_from_init
    ),
    dateInput(
     inputId = "date_to",
     label   = "Date to",
     width   = 90,
     value   = date_to_init
    ),
    selectInput(
     inputId = "sel_type",
     label   = "Choose types:",
     choices = c("Choose types" = "", data_types[, Type %>% unique() %>% sort()]),
     multiple = TRUE
    ),
    selectInput(
     inputId = "sel_desc",
     label   = "Choose description:",
     choices = "",
     multiple = TRUE
     ),
    radioButtons(
     inputId  = "plots",
     label    = "Plot type:",
     choices  = c(
      "Scatterplot" = "scatterplot",
      "Boxplot"     = "boxplot"
      )
     )
    )
   )
  )
 )


# dashboard tabs
tabitems <- tabItems(booking_tab, graphics_tab)

# dashboard body
body <- dashboardBody(tabitems)

# UI
ui <- dashboardPage(header, sidebar, body)

# LOGIC FUNCTIONS

# Server
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
   inputId = "delete",
   choices = c("Delete expense:" = "", 1:data[, .N])
  )
  output$show_data <- DT::renderDataTable({datatable(data[order(-Date, Type, Description)])})
}

# update 'type tab'
update_type_tab <- function(input, output, session){

 updateTextInput(
  session = session,
  inputId = "new_type",
  label   = "Insert new type: ",
  value   = ""
 )
 updateTextInput(
  session = session,
  inputId = "new_desc",
  label   = "Insert new description: ",
  value   = ""
 )
 output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})
}

# Graphics
make_plot <- function(input, output, session){
 # TODO:
 analytics_plot <- ggplot(data,aes(x = Date, y = Expense, fill = Type))
 analytics_plot <- analytics_plot + geom_point()
 # return(analytics_plot)
 output$show_analytics <- renderPlot({analytics_plot})
}


# Server
server <- function(input, output, session) {
 observe({
  update_exp_tab(input, output, session)
  update_type_tab(input, output, session)
  make_plot(input, output, session)
 })
 observe({
  updateSelectizeInput(
   session = session,
   inputId = "desc",
   choices = c("Choose one" = "", data_types[Type==input$type,Description %>% unique() %>% sort()])
    )
 })

 observe({
  updateSelectizeInput(
   session = session,
   inputId = "sel_desc",
   choices = c("Choose one" = "", data_types[Type %in% input$sel_type,Description %>% unique() %>% sort()])
    )
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
      file = "data/data.csv",
      append = TRUE,
      sep = ","
     )

     showNotification(
      paste0( "Expense added: (", input$date, ",", input$expense, ",", input$type, ",", input$desc, ")"),
      type = "message", duration = 10
     )
   } else{
     showNotification(
      "Invalid expense,type or description",
      type = "error", duration = 10
     )
   }
  },
  warning = function(cond){
   showNotification("Invalid expense", type = "error", duration = 10)
  },
  finally = {
    update_exp_tab(input, output, session)
  })
 })

 observeEvent(input$submit_del_exp, {
  del_index <- as.numeric(input$delete)
  notification <- ""
  for (i in 1:length(del_index)){
   notification <- paste0(
    notification,
    "(", data[del_index[i], Date], ",",
         data[del_index[i], Expense], ",",
         data[del_index[i], Type], ",",
         data[del_index[i], Description], ")", "\n"
   )
  }
  showNotification(
   paste0("Deleted data:\n", notification),
   type = "message", duration = 10
  )
  data <- data[order(-Date, Type, Description)][-del_index]
  assign("data", data, envir = .GlobalEnv)
  fwrite(
   x      = data,
   file   = "data/data.csv",
   sep    = ",",
   append = FALSE
  )
  update_exp_tab(input ,output, session)
 })

 observeEvent(input$add_new_type,{
  new_type <- input$new_type %>% tolower()
  new_desc <- input$new_desc %>% tolower()
  if(
   length(grep(x = new_type, pattern = ",", fixed = T)) > 0 |
   length(grep(x = new_desc, pattern = ",", fixed = T)) > 0
  ){
   showNotification(
    "Commas are not allowed in type/description!",
    type = "warning", duration = 10
   )
  } else if(data_types[Type == new_type & Description == new_desc, .N] == 0 &
  input$new_type != "" & input$new_desc != ""){
   data_types <- rbindlist(
    list(data_types, list(new_type, new_desc))
   )
   assign("data_types", data_types, envir = .GlobalEnv)
   fwrite(
    x      = list(new_type, new_desc),
    file   = "data/data_types.csv",
    sep    = ",",
    append = TRUE
   )
   showNotification(
    paste0("Added: (", new_type, ",", new_desc, ")"),
    type = "message", duration = 10
    )
  } else {
   showNotification(
    "Type/Description already exists or one field is empty",
    type = "default", duration = 10
   )
  }
  update_exp_tab(input, output, session)
  update_type_tab(input, output, session)
 })

 observeEvent(input$delete_type,{
  del_type <- input$new_type %>% tolower()
  del_desc <- input$new_desc %>% tolower()
  if(
   length(grep(x = del_type, pattern = ",", fixed = T)) > 0 |
   length(grep(x = del_desc, pattern = ",", fixed = T)) > 0
  ){
   showNotification(
    "Commas are not allowed in type/description!",
      type = "warning", duration = 10
   )
  } else if(data_types[Type == del_type & Description == del_desc, .N] > 0 &
  input$new_type != "" & input$new_desc != ""){
   data_types <- data_types[Type != del_type | Description != del_desc]
   assign("data_types", data_types, envir = .GlobalEnv)
   fwrite(
    x      = data_types,
    file   = "data/data_types.csv",
    sep    = ",",
    append = FALSE
   )
   showNotification(
    paste0("Deleted: (", del_type, ",", del_desc, ")"),
    type = "message", duration = 10
    )
  } else {
   showNotification(
    "Type/Description doesn't exist or one field is empty",
    type = "default", duration = 10
   )
  }
  update_exp_tab(input, output, session)
  update_type_tab(input, output, session)
 })

 observeEvent(input$save_scratchpad, {
  new_scratch <- input$scratchpad %>% strsplit("\n")
  data_scratchpad <- data.table(new_scratch[[1]])
  assign("data_scratchpad", data_scratchpad, envir = .GlobalEnv)
  fwrite(x = data_scratchpad, file = "data/data_scratchpad.csv", append = F)
  showNotification("Scratchpad saved", type = "message",duration = 10)
  updateTextInput(
   session = session,
   inputId = "scratchpad",
   label = ""
  )
 })

 # output$show_analytics <- renderPlot({
 #  make_plot(input, output, session)
 # })


 session$onSessionEnded(function() {
  stopApp()
 })
}

# Run app in given port.
shiny::shinyApp(ui, server, options = list(port = 4004))
