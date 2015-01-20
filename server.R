#.libPaths("C:/Program Files/R/win-library/3.1")
library(shiny)
suppressPackageStartupMessages(library(gdata))
suppressPackageStartupMessages(library(dplyr))
library(reshape2)
shinyServer(function(input, output) {
  dummytable <- function(){
    tb = tbl_df(read.xls("Works-template.xlsm"))
    tb %>%
      mutate(Date = ifelse(is.na(Date),as.Date("1970-01-01"),as.Date(as.POSIXct(Date*60*60*24,origin = "1899-12-30")))) %>%
      arrange(Date)
  }
  
  datefix <- function(x){
    as.Date.numeric(ifelse(is.na(x),as.Date("1970-01-01"),as.Date(as.POSIXct(x*60*60*24,origin = "1899-12-30"))),origin = "1970-01-01")
  }
  
  maxDate <- function(x,y){
    if(x == "NA" | x == "<NA>" | is.na(x))
      return(y)
    else if(y == "NA" | y == "<NA>" | is.na(y))
      return(x)
    else
      max(x,y)
  }
  wk <- reactive({
    #tb2 = tbl_df(read.xls("Online Advertisements followup (2).xlsm", sheet = "Discussions"))
    if(is.null(input$file))
      return(NULL)
    file1 <- input$file
    tb1 = tbl_df(read.xls(file1$datapath, sheet = "Leads"))
    tb2 = tbl_df(read.xls(file1$datapath, sheet = "Discussions"))
    tb1 = tb1 %>%
      mutate(Date.Contacted = datefix(Date.Contacted)) %>%
      arrange(Date.Contacted)
    tb2 = tb2 %>%
      mutate(Date = datefix(Date)) %>%
      arrange(Date)
    list(tb1, tb2, inner_join(tb2,tb1,by = c("Initiative"="Initiative",
                                           "Division" = "Division",
                                           "Site" = "Site",
                                           "Contact.Details" = "Contact.Details",
                                           "Email.ID"="Email.ID")))
  })
  
  output$fileUploaded <- reactive({
    return(is.null(input$file))
  })
  
  output$colwidth <- reactive({
    if(is.null(input$file))
      return(5)
    return(2)
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$downloadData <- downloadHandler(
    filename = paste('Template-', Sys.Date(), '.xlsm', sep=''),
    content = function(file) {
      file.copy('Works-template.xlsm', file, overwrite = TRUE)
      #file.remove('BatchPL.pdf')
    }
  )
  
  
  output$maindisplay <- renderUI({
    if(is.null(input$file))
      return(h5("New to the application? Start by downloading the Excel file here"))
    tabsetPanel(id='cond',
                tabPanel("Group to view",
                         h5("Ideas"),
                         dataTableOutput('ideasbygroup')),
                #                          h5("Groupwise Table"),
                #                          dataTableOutput('groupwisetable')),
                tabPanel("Datewise",h3("Range of activity between dates"),
                         dataTableOutput('daterangetable')),
                tabPanel("Ideas",
                         dataTableOutput('ideas')),
                tabPanel("People",
                         dataTableOutput('peoplewisetable'))
    )
  })
  output$daterangetable <- renderDataTable(expr = {
    cd <- input$daterange
    
    wk()[[3]] %>%
      rowwise() %>%
      #mutate(Date = as.numeric(as.Date(Date)),Date.Contacted = as.numeric(as.Date(Date.Contacted))) %>%
      mutate(Dday = maxDate(Date,Date.Contacted)) %>%
      filter(as.Date.numeric(Dday,origin = "1970-01-01")>cd[1],
             as.Date.numeric(Dday,origin = "1970-01-01")<cd[2]) %>%
      select(-Dday) %>%
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
                   start = min(wk()[[3]]$Date),
                   end = max(wk()[[3]]$Date))
  })
  
  output$groupui <- renderUI({
    checkboxGroupInput('group',"Group to view",choices = levels(as.factor(wk()[[3]]$Division)),selected = "All")
  })
  
  output$peopleui <- renderUI({
    checkboxGroupInput('people',"Coordinating People",choices = levels(as.factor(wk()[[3]]$Coordinating.with)),selected = wk()[[3]]$Coordinating.with)
  })
  
  
  output$ideas <- renderDataTable({
    wk()[1] %>%
      #filter(Status.of.work=="Idea Conceived") %>%
      as.data.frame    
  })
  
  output$ideasbygroup <- renderDataTable({
    #dummytable = rbind(c("","","",))
    casted = wk()[[3]] %>%
      #rbind(dummytable()) %>%
      group_by(Initiative,Division,Status.of.work,Date.Contacted) %>%
      summarise(tDate = toString(max(Date))) %>%
      dcast(Initiative+Division+Date.Contacted~Status.of.work) %>%
      tbl_df() %>%
      filter(Division %in% input$group)
    
    colnames(casted) <- make.names(colnames(casted))
    #casted[,4:dim(casted)[2]] = as.Date(casted[,4:dim(casted)[2]])
    #casted$outstanding = max(casted[,4:dim(casted)[2]])
    #       mutate(outstanding = ifelse(is.na(Implement) & nchar(Idea.Conceived)>2,
    #                                   as.numeric(Sys.Date()-as.Date(Idea.Conceived,"%Y-%m-%d")),
    #                                   0)) %>%
    inner_join(casted,wk()[[3]] %>% 
                 group_by(Initiative) %>% 
                 summarise(Coordinators = pasting(Coordinating.with)) %>% 
                 ungroup %>%
                 unique) %>%
      #select(Initiative,Idea.Conceived,Development,Implement,outstanding,Coordinators) %>%
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
    wk()[[3]] %>%
      filter(Division %in% input$group) %>%
      as.data.frame
  })
  
  output$peoplewisetable <- renderDataTable({
    wk()[[3]] %>%
      filter(Coordinating.with %in% input$people) %>%
      as.data.frame
  })
  #   
  #   file
  #   daterange
  #   group
  #   people
  
})
