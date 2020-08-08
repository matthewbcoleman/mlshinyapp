#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#source('myfunctions.R') to source functions into app

library(shiny)
library(readxl)
library(tools)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Read in Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #Input: select file
            fileInput('file1', 'Choose CSV, TSV, Text, Excel',
                      accept = c('text/plain', 'text.csv',
                                 'application/vnd.ms-excel',
                                 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
                    ) ),
            #Horizontal Line        
            tags$hr(),
            
            
            
            #Checking whether the file has a header
            checkboxInput('header', 'Header', TRUE),
            
            radioButtons('sep', 'Separator',
                         choices = c(Comma = ',',
                                     Semicolon = ';',
                                     Tab = '\t',
                                     `Excel File` = 'Excel'),
                         selected = ','),
            
            radioButtons('quote', "Quote",
                         choices = c(None = '',
                                     'Double Quote' = '"',
                                     'Single Quote' = "'"),
                         selected = '"'),
            
            # radioButtons('rownames', 'Row Names',
            #              choices = c('Yes' = TRUE,
            #                          'No' =  FALSE),
            #              selected = FALSE),
            
            #Horizontal Line
            tags$hr(),
            
            #Slider Bar to choose the number of rows to show. 
            sliderInput('disp', 'Number of Rows to Display (Max 10)',
                        min = 0,
                        max = 10,
                        value = 5,
                        step = 1),
            
            #Select predictors and response here
            fluidRow(
              checkboxGroupInput("inCheckboxGroup",
                               label = "Checkbox Group Input:",
                               c("label 1" = "option1",
                                 "label 2" = "option2"),
                               selected = "")
              ),
            
            #Choosing Columns 
            uiOutput("choose_columns")
                
            ) ,

        # Show (or not show) the outputted table
        mainPanel(
            tabsetPanel(
                tabPanel("Table", tableOutput('contents')),
                tabPanel("Summary", verbatimTextOutput("summary"))
                #tabPanel("Plot", plotOutput("plot")), 
            )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    dsnames <- c()
    infile <- reactive({input$file1})
  
    data_set <- reactive({ 
        req(input$file1)
        
      if((tolower(file_ext(infile()$datapath)) == 'xls') |
         (tolower(file_ext(infile()$datapath))== 'xlsx')){

        data_set <- read_excel(infile()$datapath)
        
        # observeEvent({updateRadioButtons(session, 'sep', 'Separator',
        #              choices = c(Comma = ',',
        #                          Semicolon = ';',
        #                          Tab = '\t',
        #                          `Excel File` = 'Excel'),
        #              selected = 'Excel')})

        }
      else{
            data_set <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
        }
        
    }) 


        observe({
            if((input$disp != 0) & (ncol(data_set()) <= 10)){
            output$contents <- renderTable({
                return(data_set())
            })
        }
        if((input$disp != 0) & (ncol(data_set()) > 10)){
            output$contents <- renderTable({
                return(head(data_set()[,1:10], input$disp))
            }, caption = paste0('Showing first 10 columns of ', ncol(data_set())))
        }
            }) 
    
    
    observe({
        req(input$file1)
        dsnames <- names(data_set())
        cb_options <- list()
        cb_options[ dsnames] <- dsnames
        updateCheckboxGroupInput(session, "inCheckboxGroup",
                                 label = "Check Box Group",
                                 choices = cb_options,
                                 selected = "")
    })
    
    #To choose columns
    # output$choose_columns <- renderUI({
    #     colnames <- names(data_set())
    #     
    #     checkboxGroupInput('columns', 'Choose Columns',
    #                        choices = colnames,
    #                        selected = '')
    # })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
