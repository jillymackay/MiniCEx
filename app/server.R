library(shiny)
library(tidyverse)






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
    
    req(input$minicex_file)
    
    infile <- input$minicex_file
    
    
    req(input$minicex_file,
        file.exists(input$minicex_file$datapath))
    
    
    withProgress({
      setProgress(message = "Processing timetable data . . .")
      
      
      readxl::read_excel(infile$datapath, sheet = "Timetable") 

      
    })
    
  })
  
  
  yearlist <- reactive ({
    
    
    
    ttable() %>% 
      select(matric, name) %>% 
      unique()
      
  
    
  })
  
  
  
  # -------------- Create Reactive Objects from Data ----------
  
  
  date_of_data <- reactive({
    
  dat() %>% 
      filter(!is.na(DateEvent)) %>% 
      summarise(max = max(DateEvent))
    
    
  })
    
  
  WeekNumberRequirement <- reactive ({
    mcex_weekn()
  })
  
  stuTasks <- reactive({
    mcex_tasks(dat())
  })
  
  stuTasksLong <- reactive({
    mcex_longtasks(dat())
  })
  
  notEnoughTasks <- reactive({
    
    WN <- as.numeric(mcex_weekn())
    mcex_enoughtasks(dat(), WN) %>% 
      unnest_wider(data_StudentName, names_sep = "_")

  })

  dateTasks <- reactive({
    mcex_datetasks(dat())
  })  
  
  dateTasksLong <- reactive({
    mcex_longdatetasks(dat())
  })
  
  SurgList <- reactive({
    dat() %>% 
      select(matric, StudentName, SurgTx) %>% 
      filter(SurgTx == TRUE) %>%
      unique()
  })
  
  rotations <- reactive({
    dat() %>% 
      janitor::tabyl(Rotation) %>% 
      arrange(percent)
  })

  
  mats_not_match <- reactive({
    dat() %>% 
      mcex_matriccheck()
  })


  
  
  #------------- Output Tables ----------------
  
  output$raw_data <- renderDT({
      dat()
    
  })
    
  
  output$raw_yl <- renderDT({
    yearlist()
  })  
    
  
  output$t_nocontrib <- renderDT({

    yearlist() %>% 
      filter(!yearlist()$matric %in% dat()$matric)
    
  })  


  output$t_net <- renderTable({
    withProgress({
    notEnoughTasks()
    })
  })
  
  output$t_below <- renderTable({
     dat() %>% 
       filter(OverallAssessorFeedback == "Below expected level") %>% 
       group_by(matric) %>% 
       nest(StudentName) %>% 
       unnest_wider(col = data, names_sep =" ") %>% 
       select(rowID, matric, `data StudentName`, MainTask, Assessor, DateOfTask) %>%  
       arrange(DateOfTask, matric) 
  })
  
  
  
  output$t_nosurg <- renderTable({

    yearlist() %>%
      filter(!(yearlist()$matric %in% SurgList()$matric)) 
  })
  
  
  output$t_rotations <- renderTable({
    rotations()
  })
  
  
  output$t_matsmatch <- renderTable({
    mats_not_match()
  })
  
  output$s_dateofdata <- renderPrint({
    date_of_data()
  })
  
  
    #------------------ Output Plots --------------------    
    
    
    output$start_plot <- renderPlot({
      mcexplot_tasks(dat())+
        scale_fill_uoe()
    })
    
  
  output$p_datetasks <- renderPlot({
    mcexplot_datetasks(dat())+
      scale_fill_uoe()
    
  })
  
  
  output$p_sppxmat <- renderPlot({
    mcexplot_tasks(dat(), matric, Spp)+
      scale_fill_uoe()
  })
  
  output$p_sppxweek <- renderPlot({
    mcexplot_tasks(dat(), week, Spp)+
      scale_fill_uoe()
  })
  
  output$p_fbackxweek <- renderPlot({
     # mcexplot_tasks(dat(), week, OverallAssessorFeedback) +
     #  scale_fill_uoe()
    # # 
     dat() %>% 
       mutate(week = week(DateOfTask)) %>% 
       ggplot(aes(x=WeekN, y=taskCounter, fill = OverallAssessorFeedback)) +
       geom_bar(stat = "identity") +
       theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom') +
       scale_fill_uoe()
  })
  
  output$tt_p_fbackxweek <- renderUI({
    hover <- input$ph_fbackxweek
    y <- nearPoints(df = dat(), coordinfo = input$ph_fbackxweek)
    req(nrow(y) != 0)
    verbatimTextOutput("tt_vals")
  })
  

  
  output$tt_vals <- renderPrint({
    if(is.null(input$ph_fbackxweek)) return()
    
    hover <- input$ph_fbackxweek
    
    y <- nearPoints(df = dat(), coordinfo = input$ph_fbackxweek) %>% 
      select(matric, rowID, OverallAssessorFeedback)
    req(nrow(y) != 0)
    y
  })
  
  output$d_fbackxweek <- renderUI({
    req(input$ph_fbackxweek)
    verbatimTextOutput("v_fbackxweek")
  })
  
  output$v_fbackxweek <- renderPrint({
    hover <- input$ph_fbackxweek
    y <- nearPoints(dat(), input$ph_fbackxweek, xvar = "week", yvar="taskCounter")
    req(nrow(y)!= 0)
    x
  })
  
  output$p_fbackxweekfspp <- renderPlot({
    mcexplot_facettasks(dat(), week, OverallAssessorFeedback, Spp)+
      scale_fill_uoe()
  })
  
  output$p_fbackxspp <- renderPlot({
    mcexplot_tasks(dat(), Spp, OverallAssessorFeedback) +
      labs("Overall feedback by Species")+
      scale_fill_uoe()
  })
  
  
  output$p_entrustxmatric <- renderPlot({
    mcexplot_tasks(dat(), matric, OverallAssessorFeedback)+
      scale_fill_uoe()
  })
  
  output$p_entrustxtask <- renderPlot({
    dat() %>% 
      group_by(matric) %>% 
      nest(StudentName) %>% 
      unnest_wider(col = data, names_sep =" ") %>% 
      ungroup() %>% 
      select(rowID, matric, Spp, taskCounter,
             c(Organisation:OverallAssessorFeedback), 
             c(ClinicalExam:Other)) %>% 
      pivot_longer(cols = -c(rowID, matric, Spp, taskCounter, Organisation:OverallAssessorFeedback),
                   names_to = "Task",
                   values_to = "T/F") %>% 
      filter(`T/F` == TRUE) %>% 
      select(-`T/F`) %>% 
      pivot_longer(cols = -c(rowID, matric, Spp, taskCounter, Task),
                   names_to = "Feedback",
                   values_to = "Score") %>% 
      filter(!is.na(Score),
             Feedback != "OverallAssessorFeedback") %>% 
      ggplot(aes(x = Feedback, y = Score, fill = Score)) +
      geom_bar(stat = "identity") +
      facet_wrap(facets = ~ Task) +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = 'bottom',
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_fill_uoe()
    
  })
  
  
  output$p_fbackxtask <- renderPlot({
    dat() %>% 
      group_by(matric) %>% 
      nest(StudentName) %>% 
      unnest_wider(col = data, names_sep =" ") %>% 
      ungroup() %>% 
      select(rowID, matric, Spp, taskCounter, Organisation:OverallAssessorFeedback, ClinicalExam:Other) %>% 
      pivot_longer(cols = -c(rowID, matric, Spp, taskCounter,
                             c(Organisation:OverallAssessorFeedback)),
                   names_to = "Task",
                   values_to = "T/F") %>% 
      filter(`T/F` == TRUE) %>% 
      select(-`T/F`) %>% 
      pivot_longer(cols = -c(rowID, matric, Spp, taskCounter, Task),
                   names_to = "Feedback",
                   values_to = "Score") %>% 
      filter(!is.na(Score),
             Feedback == "OverallAssessorFeedback") %>% 
      ggplot(aes(x = Feedback, y = Score, fill = Score)) +
      geom_bar(stat = "identity") +
      facet_wrap(facets = ~ Task) +
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = 'bottom',
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      scale_fill_uoe()
  })
  
  # ---------- Inglis Processing ----------------
  
  inglisthisweek <- reactive({
    

    # what_week <- if(is.character(what_week)){as.character(what_week)}
    # else{Sys.Date()}
    # 
    
    ttable() %>%
      filter(WeekN == lubridate::week(Sys.Date()),
             Rotation == "Inglis Veterinary Practice") %>%
      select(c(matric,name))  
    
  })
  
  output$inglisthisweek <- renderDT({
    
    inglisthisweek()
    
    
  })
  
  
  output$inglisthisweek_none <- renderDT({
    
    mdat <- dat() %>% 
      filter(WeekN == lubridate::week(Sys.Date()))
    
    inglisthisweek() %>% 
      filter(!(matric %in% mdat$matric)) 
  })
  
    
  output$inglis_fulltasks <- renderDT({
    
    dat() %>% 
      filter(matric %in% inglisthisweek()$matric,
             WeekN == week(Sys.Date())) %>% 
      select(rowID, StudentName, Rotation, Assessor, Referral, MainTask, OverallAssessorFeedback) 
  })
  
  
  output$inglis_tasks <- renderDT({
    dat() %>% 
      filter(matric %in% inglisthisweek()$matric,
             WeekN == lubridate::week(Sys.Date())) %>% 
      select(matric, StudentName, taskCounter) %>% 
      group_by(matric) %>% 
      add_tally() %>% 
      select(-taskCounter) %>% 
      unique()
    
  })
  
  
  output$inglis_tasks_less2 <- renderDT({
    
    dat() %>% 
      filter(matric %in% inglisthisweek()$matric,
             WeekN == lubridate::week(Sys.Date())) %>% 
      select(matric, StudentName, taskCounter) %>% 
      group_by(matric) %>% 
      add_tally() %>% 
      select(-taskCounter) %>% 
      filter(n <2)
    
  })
  
  
  # ---------------------- Flat Images -------------------
  
  
  output$logo <- renderImage({
    # Return a list containing the filename
    list(src = file.path("media/RDVS_2col_cmyk.png"),
         contentType = 'image/png',
         width = 221,
         height = 52,
         alt = "R(D)SVS Logo")
  }, deleteFile = FALSE)

  
    #-------------  Close Server App Brackets -----------------
 
})