library(shiny)
library(tidyverse)
library(minicexpack)





shinyServer(function(input, output, session){
  
  # ------------ Functions for the Server -------------------
  # This would be aesthetics like palettes
  
  
  
  #------------ Importing Data ----------------
  
  dat <- reactive ({
    
    req(input$minicex_file)
    
    infile <- input$minicex_file
    
    
    req(input$minicex_file,
        file.exists(input$minicex_file$datapath))
    
    withProgress({
      setProgress(message = "Processing data . . .")
      
    mcex_read(infile$datapath)
    
    })
    
  })
  
  
  
  ttable <- reactive ({
    
    req(input$timetable)
    
    ttablefile <- input$timetable
    
    
    req(input$timetable,
        file.exists(input$timetable$datapath))
    
    withProgress({
      setProgress(message = "Processing timetable data . . .")
      
      
      readxl::read_excel(ttablefile$datapath, sheet = "2022-2023", range = "A6:AP183") %>% 
        mutate(name = paste0(`...3`," ", `...4`)) %>% 
        select(c("...2", "1":"28", name)) %>% 
        rename(matric = "...2",
               "05-06-23" = "1",
               "12-06-23" = "2",
               "19-06-23" = "3",
               "26-06-23" = "4",
               "03-07-23" = "...11",
               "10-07-23" = "...12",
               "17-07-23" = "...13",
               "24-07-23" = "...14",
               "31-07-23" = "5",
               "07-08-23" = "6",
               "14-08-23" = "7",
               "21-08-23" = "8",
               "28-08-23" = "9",
               "04-09-23" = "10",
               "11-09-23" = "11",
               "18-09-23" = "12",
               "25-09-23" = "13",
               "02-10-23" = "14",
               "09-10-23" = "15",
               "16-10-23" = "16",
               "23-10-23" = "17",
               "30-10-23" = "18",
               "06-11-23" = "19",
               "13-11-23" = "20",
               "20-11-23" = "21",
               "27-11-23" = "22",
               "04-12-23" = "23",
               "11-12-23" = "24",
               "18-12-23" = "...35",
               "25-12-23" = "...36",
               "01-01-24" = "...37",	
               "08-01-24" = "**",
               "15-01-24" = "25",
               "22-01-24" = "26",
               "29-01-24" = "27",
               "05-02-24" = "28")  %>% 
        pivot_longer(cols = -c(matric, name), 
                     names_to = "Week",
                     values_to = "Rotation") %>% 
        mutate (WeekN = dmy(Week),
                WeekN = lubridate::week(WeekN)) %>% 
        mutate (Rotation =  zoo::na.locf(Rotation),
                matric = tolower(matric))
      
    })
    
  })
  
  
  yearlist <- reactive ({
    
    
    
    ttable() %>% 
      select(matric, name) %>% 
      unique()
      
  
    
  })
  
  
  
  # -------------- Create Reactive Objects from Data ----------
  
  
  date_of_data <- reactive({
    
    req(input$minicex_file)
    
    infile <- input$minicex_file
    
    req(input$minicex_file,
        file.exists(input$minicex_file$datapath))
    
    file.info(infile)$ctime
  })
    
  
  WeekNumberRequirement <- reactive ({
    mcex_weekn("20230605")
  })
  
  stuTasks <- reactive({
    mcex_tasks(dat())
  })
  
  stuTasksLong <- reactive({
    mcex_longtasks(dat())
  })
  
  notEnoughTasks <- reactive({
    mcex_enoughtasks(dat(), WeekNumberRequirement())
  })

  dateTasks <- reactive({
    mcex_datetasks(dat())
  })  
  
  dateTasksLong <- reactive({
    mcex_longdatetasks(dat())
  })
  

  



  
  
  #------------- Output Raw Data ----------------
  
  output$raw_data <- renderDT({
      dat()
    
  })
    
  
  output$raw_yl <- renderDT({
    yearlist()
  })  
    
  
  output$t_nocontrib <- renderTable({
    yearlist() %>% 
      filter(!yearlist()$matric %in% dat()$matric)
  })  


  output$t_net <- renderTable({
    notEnoughTasks()
  })
  
  
    #------------------ Output Plots --------------------    
    
    
    output$start_plot <- renderPlot({
      mcexplot_tasks(dat())
    })
    
  
  
  
  
  
  
  
  # ---------- Inglis Processing ----------------
  
  inglisthisweek <- reactive({
    
    ttable() %>% 
      filter(WeekN == lubridate::week(Sys.Date()),
             Rotation == "Inglis Veterinary Practice") %>% 
      select(matric, name)
    
    
  })
  
  output$inglisthisweek <- renderDT({
    
    inglisthisweek()
    
    
  })
  
  
  output$inglisthisweek_none <- renderDT({
    
    inglisthisweek() %>% 
      filter(!(matric %in% dat()$matric)) 
  })
  
    
  
  output$inglis_tasks <- renderDT({
    dat() %>% 
      filter(matric %in% inglisthisweek()$matric) %>% 
      select(matric, StudentName, taskCounter) %>% 
      group_by(matric) %>% 
      add_tally() %>% 
      select(-taskCounter) %>% 
      unique()
    
  })
  
  
  output$inglis_tasks_less2 <- renderDT({
    
    dat() %>% 
      filter(matric %in% inglisthisweek()$matric) %>% 
      select(matric, StudentName, taskCounter) %>% 
      group_by(matric) %>% 
      add_tally() %>% 
      select(-taskCounter) %>% 
      filter(n <2)
    
  })
  
  
    #-------------  Close Server App Brackets -----------------
 
})