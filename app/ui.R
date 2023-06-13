
library(shiny)
library(DT)




shinyUI(
  navbarPage(title = "MiniCEx Analyses",
             id = "navbar",
             
             tabPanel(title = "Upload your data",
                      fluidPage(fluidRow(tags$p("Upload your MiniCEx data here"),
                                         tags$em("Note: This should be an excel file exactly as downloaded from Sharepoint. If you make modifications to that file, particularly to column headings, you may get unexpected and unreliable results"),
                                         fileInput(inputId = "minicex_file",
                                                   label = "Upload your xlsx file here")),
                                fluidRow(tags$p("If you want, you can also upload a FY timetable"),
                                         tags$em("Please note this utility is still under development."),
                                         fileInput(inputId = "timetable",
                                                   label ="Upload your timetable xlsx file here")),
                                fluidRow(tags$h2("MiniCEx Snapshot"),
                                         tags$p("Once your MiniCEx data is uploaded it will appear here. It shoud look like your excel file, but with some extra columns at the end.
                                                If this doesn't look right at this stage, double check that your file hasn't been changed from the version downloaded
                                                from Sharepoint."),
                                         tags$p("You can search and browse through the MiniCEx data here, e.g. search for student matric `s1234567` or assessor `Alex Corbishley`."),
                                         DTOutput("raw_data")),
                                fluidRow(tags$h2("Yearlist Snapshot"),
                                         DTOutput("raw_yl")))
             ),
             
             # -------------- Output -----------------------         
             
             tabPanel(title = "Data Output",
                      fluidPage(
                        fluidRow(column(width = 6, 
                                        plotOutput(outputId = "start_plot"))
                                 ))),
             
             
             
             # -------------- Inglis Report ----------------
             
             tabPanel(title = "Inglis Check",
                      fluidPage(tags$h2("About this page"),
                                tags$p("This report aims to highlight students at Inglis", 
                                       tags$em(" this week "), 
                                       "who have not yet completed two MiniCExs. It is recommended you run this report mid-week."),
                                tags$em("Note, you will need a timetable file and MiniCEx file uploaded in the 'Upload' tab to run this report."),
                                tags$h3("These students are thought to be at Inglis this week"),
                                DTOutput("inglisthisweek"),
                                tags$h3("Students believed to be at Inglis this week with NO MiniCExs recorded"),
                                DTOutput("inglisthisweek_none"),
                                tags$h3("Students believed to be at Inglis this week with recorded tasks"),
                                DTOutput("inglis_tasks"),
                                tags$h3("Students believed to be at Inglis this week with fewer than 2 tasks recorded"),
                                DTOutput("inglis_tasks_less2")
                                ))
             
             
             
             
             
             
             # ------------- app close brackets---------------
             
))