list.of.packages <- c("shiny", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
mcex_install <- !("minicexpack") %in% installed.packages()[, "Package"]
if (mcex_install) devtools::install_github("jillymackay/minicexpack")



library(shiny)
library(tidyverse)
library(minicexpack)





shinyServer(function(input, output, session){
  
  # ------------ Functions for the Server -------------------
  # This would be aesthetics like palettes
  
  
  
  #------------ Importing Data ----------------
  
   ordat <- reactive ({
     
     mcex_read("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/MiniCEx Analyses/AY 2023-2024/App Data - Save Your MiniCEx Spreadsheet Here/FY MiniCE 2023_2024.xlsx")
   
       })
   
   dat <- reactive({
     
     req(ordat())
     mcex_edit("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/MiniCEx Analyses/AY 2023-2024/App Data - Save Your MiniCEx Spreadsheet Here/FY MiniCE 2023_2024_RowsToEdit.xlsx", ordat())
   })
  
  
  ttable <- reactive ({
    
    mcex_ttable("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/Timetables & Student Groupings/2023-24/USE THIS ONE WORKING COPY FY23-24 core rotation timetable.xlsx", sheet = "23-24")
    
  })
  
  
  yearlist <- reactive ({
    
    
    
    ttable() %>% 
      select(matric, name) %>% 
      unique()
      
  
    
  })
  
  
  
  # -------------- Create Reactive Objects from Data ----------
  
  
  date_of_data <- reactive({

    as.character(file.info("//cmvm.datastore.ed.ac.uk/cmvm/mvmsan/rdsvsshared/Fieldsec/BVMS Years/Final Year/MiniCEx Analyses/AY 2023-2024/App Data - Save Your MiniCEx Spreadsheet Here/FY MiniCE 2023_2024_RowsToEdit.xlsx")$ctime)
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
    
  
  output$t_dateofdata <- renderText({
    date_of_data()
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
    
    req(input$timetable)
    
    ttablefile <- input$timetable
    
    
    req(input$timetable,
        file.exists(input$timetable$datapath))
    
    mcex_inglis(ttablefile$datapath, sheet = "2022-2023")
    
    
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
  
  
    #-------------  Close Server App Brackets -----------------
 
})