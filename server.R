#.libPaths("C:/Program Files/R/win-library/3.1")
library(shiny)
suppressPackageStartupMessages(library(gdata))
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
shinyServer(function(input, output) {
  dummytable <- function(){
    tb = tbl_df(read.xls("Works-template.xlsx"))
    tb %>%
      mutate(Date = as.Date(as.POSIXct(Date*60*60*24,origin = "1899-12-30"))) %>%
      arrange(Date)
  }
  
  wk <- reactive({
    #tb = tbl_df(read.xls("Works.xlsx"))
    if(is.null(input$file))
      return(NULL)
    file1 <- input$file
    tb = tbl_df(read.xls(file1$datapath))
    tb %>%
      mutate(Date = as.Date(as.POSIXct(Date*60*60*24,origin = "1899-12-30"))) %>%
      arrange(Date)
  })
  
  output$fileUploaded <- reactive({
    return(is.null(input$file))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$downloadData <- downloadHandler(
    filename = paste('Template-', Sys.Date(), '.xlsx', sep=''),
    content = function(file) {
      file.copy('Works-template.xlsx', file, overwrite = TRUE)
      #file.remove('BatchPL.pdf')
    }
  )
  
  
  output$maindisplay <- renderUI({
    if(is.null(input$file))
      return(h5("New to the application? Start by downloading the Excel file here"))
    tabsetPanel(id='cond',
                tabPanel("Datewise",
                         dataTableOutput('daterangetable')),
                tabPanel("Ideas",
                         dataTableOutput('ideas')),
                tabPanel("Group to view",
                         h5("Ideas"),
                         dataTableOutput('ideasbygroup')),
                #                          h5("Groupwise Table"),
                #                          dataTableOutput('groupwisetable')),
                tabPanel("People",
                         dataTableOutput('peoplewisetable'))
    )
  })
  output$daterangetable <- renderDataTable(expr = {
    cd <- input$daterange
    wk() %>%
      filter(Date>cd[1],
             Date<cd[2]) %>%
      as.data.frame},
    options = list(rowCallback = I(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
        //if (parseFloat(data[0]) >= 5.0)
          $("td:eq(0)", row).css("font-weight", "bold");
      }'))
  )
  
  output$daterangeui <- renderUI({
    dateRangeInput('daterange',"Date Range of works done",
                   start = min(wk()$Date),
                   end = max(wk()$Date))
  })
  
  output$groupui <- renderUI({
    checkboxGroupInput('group',"Group to view",choices = levels(wk()$Division),selected = "All")
  })
  
  output$peopleui <- renderUI({
    checkboxGroupInput('people',"Coordinating People",choices = levels(wk()$Coordinating.with),selected = wk()$Coordinating.with)
  })
  
  
  output$ideas <- renderDataTable({
    wk() %>%
      filter(Status.of.work=="Idea Conceived") %>%
      as.data.frame    
  })
  
  output$ideasbygroup <- renderDataTable({
    #dummytable = rbind(c("","","",))
    casted = wk() %>%
      rbind(dummytable()) %>%
      group_by(Initiative,Division,Status.of.work) %>%
      summarise(tDate = toString(max(Date))) %>%
      dcast(Initiative+Division~Status.of.work) %>%
      tbl_df()
    
    colnames(casted) <- make.names(colnames(casted))
    
    casted %>%
      filter(Division %in% input$group) %>%
      mutate(outstanding = ifelse(is.na(Implement) & nchar(Idea.Conceived)>2,
                                  as.numeric(Sys.Date()-as.Date(Idea.Conceived,"%Y-%m-%d")),
                                  0)) %>%
      inner_join(wk() %>% 
                   group_by(Initiative) %>% 
                   summarise(Coordinators = pasting(Coordinating.with)) %>% 
                   ungroup %>%
                   unique) %>%
      select(Initiative,Idea.Conceived,Development,Implement,outstanding,Coordinators) %>%
      as.data.frame
  },
  options = list(
    rowCallback = I(
      'function(row, data) {
        // Bold cells for those >= 5 in the first column
       //if (parseFloat(data[4]) >= 5.0)
          $("td:eq(0)", row).css("font-weight", "bold");
       if (parseFloat(data[4]) >= 5)
          $("td:eq(4)", row).css("background-color", "red");
      }')
  )
  )
  
  pasting <- function(x)(
    paste(unique(x), collapse = ",")
  )
  output$groupwisetable <- renderDataTable({
    wk() %>%
      filter(Division %in% input$group) %>%
      as.data.frame
  })
  
  output$peoplewisetable <- renderDataTable({
    wk() %>%
      filter(Coordinating.with %in% input$people) %>%
      as.data.frame
  })
  #   
  #   file
  #   daterange
  #   group
  #   people
  
})
