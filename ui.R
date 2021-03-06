library(shiny)
x = 3
shinyUI(fluidPage(theme = "bootstrap.css",
  #  includeCSS("font-awesome.css"),
  #   tags$head(
  #     tags$style(HTML(""))
  #   ),
  titlePanel("Works"),
  fluidRow(
    column(12,
           sidebarPanel(width = x,
                  fileInput('file',"Please upload the works file"),
                  tags$hr(),
                  conditionalPanel(condition = "input.cond == 'File Upload'",
                                   fileInput('file',"Choose the works Excel File")),
                  conditionalPanel(condition = "input.cond == 'Datewise'",
                                   uiOutput('daterangeui')),
                  conditionalPanel(condition = "input.cond == 'Group to view'",
                                   uiOutput('groupui')),
                  conditionalPanel(condition = "input.cond == 'People'",
                                   uiOutput('peopleui')),
                  conditionalPanel(condition = "output.fileUploaded",
                                   downloadButton('downloadData', 'Download')),
                  conditionalPanel(condition = "input.cond == 'Storyline'",
                                   uiOutput('stories'))
           ),
           mainPanel(width = 12-x,
                  uiOutput('maindisplay')
           )
    )
    
  )
))
