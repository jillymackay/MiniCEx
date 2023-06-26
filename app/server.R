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
      
      
      mcex_ttable(ttablefile$datapath, sheet = "2022-2023") 

      
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
      janitor::tabyl(Rotation)
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
    
  
  output$t_nocontrib <- renderTable({
    yearlist() %>% 
      filter(!yearlist()$matric %in% dat()$matric)
  })  


  output$t_net <- renderTable({
    notEnoughTasks()
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
  
  
    #------------------ Output Plots --------------------    
    
    
    output$start_plot <- renderPlot({
      mcexplot_tasks(dat())
    })
    
  
  output$p_datetasks <- renderPlot({
    mcexplot_datetasks(dat())
    
  })
  
  
  output$p_sppxmat <- renderPlot({
    mcexplot_tasks(dat(), matric, Spp)
  })
  
  output$p_sppxweek <- renderPlot({
    mcexplot_tasks(dat(), week, Spp)
  })
  
  output$p_fbackxweek <- renderPlot({
    mcexplot_tasks(dat(), week, OverallAssessorFeedback)
  })
  
  output$p_fbackxweekfspp <- renderPlot({
    mcexplot_facettasks(dat(), week, OverallAssessorFeedback, Spp)
  })
  
  output$p_fbackxspp <- renderPlot({
    mcexplot_tasks(dat(), Spp, OverallAssessorFeedback) +
      labs("Overall feedback by Species")
  })
  
  
  output$p_entrustxmatric <- renderPlot({
    mcexplot_tasks(dat(), matric, OverallAssessorFeedback)
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
            axis.ticks.y=element_blank())
    
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
            axis.ticks.y=element_blank())
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