
# Check if necessary packages exists.
# If not then download them
check_packages <- function(pkg){
	  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
	      install.packages(new.pkg, dependencies = TRUE,repos="https://ftp.acc.umu.se/mirror/CRAN/")
    sapply(pkg, require, character.only = TRUE)
}

# The following packages are dependencies for this app.
packages<-c("shiny","shinydashboard","data.table","DT","dplyr","stringr")
check_packages(packages)

# Load libraries
#library(shiny)
#library(shinydashboard)
#library(data.table)
##library(fasttime)
#library(DT)

# Set session time to UTC, so the date time is not affected by local time.
Sys.setenv(TZ='UTC')

# Ensure UTF-8 encoding
options(encoding="UTF-8",encoding="UTF-8")

# Check if data/ directory exists.
# If not, create the dir and all the necessary files.
if (!dir.exists("data/")){
	dir.create("data/")
	data <- data.table(
					   Date=as.Date(NA_character_),
					   Expense=NA_real_,
					   Type=NA_character_,
					   Description=NA_character_
					   )
	data_types <- data.table(
					   Type="Unknown",
					   Description="Unknown"
					   )
	# TODO: UPDATE WINDOW TEXT TO BE "INITIALIZING"
	data_scratchpad <- data.table("")
	# saveRDS(object = data, file = "data/data.csv")
	# saveRDS(object = data_types, file = "data/data_types.csv")
	# saveRDS(object = data_scratchpad, file = "data/data_scratchpad.csv")
	fwrite( x=data, file="data/data.csv", sep=",")
	fwrite( x=data_types, file="data/data_types.csv", sep=",")
	fwrite( x=data_scratchpad, file="data/data_scratchpad.csv", sep=",")
} else{
	data <- fread(file = "data/data.csv",sep=",")
	data[,Date := as.Date(Date,origin="1970-01-01")]
	data_types <- fread(file = "data/data_types.csv",sep=",")
	data_scratchpad <- fread(file = "data/data_scratchpad.csv",sep="")
}



## Read in the data and make 'Date' column a timestamp.
#data <- fread(file="data/data.txt",sep="|")
#data[,Date := as.Date(Date,origin="1970-01-01")]
##data[,Date := fasttime::fastPOSIXct(Date)]
#
## Read in the data types
#data_types <- fread(file="data/data_types.txt",sep="|")
#
## Read in scratchpad values
#data_scratchpad <- fread(file="data/data_scratchpad.txt",header=F,sep="")


# UI header
header <- dashboardHeader( title = "Expenses")
# UI sidebar
sidebar <- dashboardSidebar(
				sidebarMenu(
					menuItem("Account book",tabName="account_book",icon=icon("book")),
					menuItem("Analytics",tabName="analytics",icon=icon("bar-chart"))
				)
			)
# UI body
body <- dashboardBody(
		tabItems(
			tabItem(tabName="account_book",
				tabBox(
					id="account_book_tabs",
					width=12,
					tabPanel("Add expense",
						fluidRow(
							box(
								title=strong("Add new expense"),
								dateInput(inputId="date",
									label="Date of the expense:",
									width=90),
								textInput(inputId="expense",
								#numericInput(inputId="expense",
									label="Expense amount:",
									#value=0,
									#min=0,
									width=385),
								selectInput(inputId = "type",
									label=strong("Choose type:"),
									choices=c("Choose one" = "",data_types[,Type %>% unique() %>% sort()]),
									width=385),
								selectInput(inputId = "desc",
									label=strong("Choose description:"),
									choices="",
									width=385),
								actionButton("submit_new_exp","Submit"), 
								width=3
							),
							box(title=strong("Expenses"),
								column(12,dataTableOutput("show_data")),width=8
							)
						)
					),
					tabPanel("Scratchpad",
						fluidRow(
							box(
								textAreaInput(inputId="scratchpad",
									label="",
									#width='300px',
									#height='350px',
									width='200%',
									height='300px',
									value=paste0(data_scratchpad,collapse="\n"),
									resize='both'
									),
								actionButton("save_scratchpad","Save scratchpad"), 
								width=3
							)
						)
					),
					tabPanel("Add type/desc",
						fluidRow(
							box(
								textInput(inputId="new_type",
									label="Insert new type:",
									width=385),
								textInput(inputId="new_desc",
									label="Insert new description:",
									width=385),
								actionButton("add_new_type","Add"), 
								actionButton("delete_type","Delete"), 
								width=3
							),
							box(title=strong("Already existing"),
								column(12,dataTableOutput("show_types")),width=8
							)
						)
					),
					tabPanel("Import/export data",
						# TODO: Allow import from other directories.
						# TODO: improve import/export design
						# TODO: implement logic for import/export
						# TODO: allow other separators in import/export
						fluidRow(
							box(
								radioButtons(inputId="imp_exp_choice",
											 label="Which file to import/export",
											 choices = c("Data","Data types","Scratchpad")
											 ),
# 								textInput(inputId="imp_exp_path",
# 									label="Insert path of the imported/exported file",
# 									width=385),
								actionButton("import","Import"), 
								actionButton("export","Export"), 
								p(),
								tags$b("NB!"),
								p("Imported data must be located in the same directory as the app with corresponding file name (data.csv,data_types.csv or data_scratchpad.csv)."),
								p("(Currently,) Imported data must be comma separated and follow the data structure: Date,Expense,Type,Description."),
								p("Exported data is found in data/ directory with .csv extension."),
								width=5
							)
						)
					)#,
					#		 #TODO:
					#tabPanel("Delete expense",
					#	fluidRow(
					#		box(
					#			textInput(inputId="new_type",
					#				label="Insert new type:",
					#				width=385),
					#			textInput(inputId="new_desc",
					#				label="Insert new description:",
					#				width=385),
					#			actionButton("delete_exp","Delete"), 
					#			width=3
					#		),
					#		box(title=strong("Already existing"),
					#			column(12,dataTableOutput("show_types")),width=8
					#		)
					#	)
					#)
				)
			),
		tabItem(tabName="analytics",
				h2("TODO") #TODO: <++>
		)
	)
)

# UI
ui <- dashboardPage(header,sidebar,body)
	

# Server
##TODO: REFACTOR ALL THE update<>Input's (?)
## THERE ARE QUITE A FEW OF THEM.
server = function(input, output,session) {
	observe({
		updateSelectizeInput(session=session, inputId="desc",
			choices = c("Choose one" = "", data_types[Type==input$type,Description %>% unique() %>% sort()])
		)
	})

	observeEvent(input$submit_new_exp, {

		tryCatch(
			{
				if(!("" %in% c(input$expense,input$type,input$desc))){
					data <<- rbindlist(list(data[!is.na(Date)],list(input$date,as.numeric(input$expense),input$type,input$desc)))

					output$show_data <- DT::renderDataTable({datatable(data[order(-Date)])})

					# saveRDS(
					# 	object = data,
					# 	file = "data/data.Rds"
					# )

					fwrite(
						x=list(input$date,as.numeric(input$expense),input$type,input$desc),
					   	file="data/data.txt",
					   	append=TRUE,
					   	sep=","
					)

					showNotification(paste0("Expense added: (",
												input$date,",",
												input$expense,",",
												input$type,",",
												input$desc,")"
											),
						type="message",duration=10)	
				}
				else{
					showNotification("Invalid expense,type or description",
						type="error",duration=10)	
				}	
			
			},
			warning=function(cond){
				showNotification("Invalid expense",type="error",duration=10)	
			}
		)

		updateDateInput(session=session, inputId="date",
			label="Date of the expense:"
		)

		updateTextInput(session=session, inputId="expense",
			label="Expense amount", value=""
		)

		updateSelectizeInput(session=session, inputId="type",
			choices=c("Choose one" = "",data_types[,Type %>% unique() %>% sort()])
		)

		updateSelectizeInput(session=session, inputId="desc",
			choices = c("Choose one" = "", data_types[Type==input$type,Description %>% unique() %>% sort()])
		)

	})

	observeEvent(input$add_new_type, {
					 
		new_type_cap <- input$new_type %>% tolower()
		new_desc_cap <- input$new_desc%>% tolower()

		if(data_types[Type==new_type_cap & Description==new_desc_cap, .N]==0 & 
		   	input$new_type!="" & input$new_desc!=""){


			data_types <<- rbindlist(list(data_types,list(new_type_cap, new_desc_cap)))

			output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})

			# saveRDS(
			# 	object = data_types,
			# 	file = "data/data_types.Rds"
			# )

		    fwrite(
				x=list(new_type_cap, new_desc_cap),
		       	file="data/data_types.txt",
		       	append=TRUE,
				sep=","
		    )

			updateSelectizeInput(session=session, inputId="type",
				choices=c("Choose one" = "",data_types[,Type %>% unique() %>% sort()])
			)

			updateSelectizeInput(session=session, inputId="desc",
				choices = c("Choose one" = "", data_types[Type==input$type,Description %>% unique() %>% sort()])
			)

			showNotification(paste0("Added: (",new_type_cap,",",new_desc_cap,")"),
				type="message",duration=10
			)
			
		}
		else{
			showNotification("Type/Description already exists",
				type="default",duration=10
			)
		}

		updateTextInput(session=session, inputId="new_type",
			label="Insert new type:",
			value=""
		)
		updateTextInput(session=session, inputId="new_desc",
			label="Insert new description:",
			value=""
		)

		rm(new_type_cap,new_desc_cap)

	})

	observeEvent(input$delete_type, {

		new_type_cap <- input$new_type %>% tolower()
		new_desc_cap <- input$new_desc %>% tolower()

		if(data_types[Type==new_type_cap & Description==new_desc_cap, .N]>0 & 
		   	input$new_type!="" & input$new_desc!=""){

			data_types <<- data_types[Type!=new_type_cap & Description!=new_desc_cap]

			output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})
			# saveRDS(
			# 	object = data_types,
			# 	file = "data/data_types.Rds"
			# )

			fwrite(
				x=data_types,
				file="data/data_types.txt",
				append=F,
				sep=","
			)

			updateSelectizeInput(session=session, inputId="type",
				choices=c("Choose one" = "",data_types[,Type %>% unique() %>% sort()])
			)

			updateSelectizeInput(session=session, inputId="desc",
				choices = c("Choose one" = "", data_types[Type==input$type,Description %>% unique() %>% sort()])
			)

			showNotification(paste0("Deleted : (",new_type_cap,",",new_desc_cap,")"),
				type="message",duration=10
			)

		}
		else{
			showNotification("Type/Description doesn't exists",
				type="default",duration=10
			)
		}

		updateTextInput(session=session, inputId="new_type",
			label="Insert new type:",
			value=""
		)
		updateTextInput(session=session, inputId="new_desc",
			label="Insert new description:",
			value=""
		)

		rm(new_type_cap,new_desc_cap)
	})
	observeEvent(input$save_scratchpad, {
		data_scratchpad <<- input$scratchpad #%>% strsplit("\n")
		# saveRDS( object = data_scratchpad, file = "data/data_scratchpad.Rds")
		fwrite(x=data_scratchpad,file="data/data_scratchpad.txt",append=F)
		showNotification("Scratchpad saved", type="message",duration=10)
	})

	observeEvent(input$import,{
		infile <- dplyr::case_when(
			input$imp_exp_choice %in% 'Data' ~ 'data.csv',
			input$imp_exp_choice %in% 'Data Types' ~ 'data_types.csv',
			input$imp_exp_choice %in% 'Scratchpad' ~ 'data_scratchpad.csv',
		)
		sep=','
		if(infile %in% 'data_scratchpad.csv'){
			sep=''
		}
		dot_index <- stringr::str_locate(pattern=fixed('.'),infile)
		null <- tryCatch({
			obj <- fread(file=infile,sep=sep)
			obj_var <- str_sub(infile,1,dot_index[1]-1)
			assign(obj_var,obj,envir=.GlobalEnv)

			# saveRDS(
			# 	object = obj_var,
			# 	file = paste0("data/",obj_var,".Rds")
			# )
			fwrite(x=eval(parse(text=obj_var)),file=paste0("data/",obj_var,".csv"),append=T)

			output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})
			updateTextInput(session=session, inputId="new_type",
				label="Insert new type:",
				value=""
			)
			updateTextInput(session=session, inputId="new_desc",
				label="Insert new description:",
				value=""
			)
			showNotification(paste0("Imported: ",imp_exp_choice), type="message",duration=10)
		},
		waring=function(cond){
			showNotification(paste0("File not found: ",infile), type="warning",duration=10)
		},
		error = function(cond){
			showNotification(paste0("File not found: ",infile), type="error",duration=10)
		})

	})

	observeEvent(input$export,{
		infile <- dplyr::case_when(
			input$imp_exp_choice %in% 'Data' ~ 'data.csv',
			input$imp_exp_choice %in% 'Data Types' ~ 'data_types.csv',
			input$imp_exp_choice %in% 'Scratchpad' ~ 'data_scratchpad.csv',
		)
		sep=','
		if(infile %in% 'data_scratchpad.csv'){
			sep=''
		}
		dot_index <- stringr::str_locate(pattern=fixed('.'),infile)
		obj_var <- str_sub(infile,1,dot_index[1]-1)
		fwrite(x=eval(parse(text=obj_var)),file=paste0("data/",infile),sep=sep)

		showNotification(paste0("Exported: ",imp_exp_choice," -> data/",infile), type="message",duration=10)
	})

	output$show_data <- DT::renderDataTable({datatable(data[order(-Date)])})
	output$show_types <- DT::renderDataTable({datatable(data_types[order(Type,Description)])})

	session$onSessionEnded(function() {
			stopApp()
	})
}

# Run app in given port.
shiny::shinyApp(ui,server,options = list(port = 4004))

