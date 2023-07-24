
library(shiny)
library(DT)




shinyUI(
  navbarPage(title = "MiniCEx Analyses",
             id = "navbar",
             
             # tabPanel(title = "Upload your data",
             #          fluidPage(fluidRow(tags$p("Upload your MiniCEx data here"),
             #                             tags$em("Note: This should be an excel file exactly as downloaded from Sharepoint. If you make modifications to that file, particularly to column headings, you may get unexpected and unreliable results"),
             #                             fileInput(inputId = "minicex_file",
             #                                       label = "Upload your xlsx file here")),
             #                    fluidRow(tags$p("Upload your edit file data here"),
             #                             tags$em("Note: This should be an excel file exactly as downloaded from Sharepoint. If you make modifications to that file, particularly to column headings, you may get unexpected and unreliable results"),
             #                             fileInput(inputId = "minicex_edit",
             #                                       label = "Upload your xlsx file here")),
             #                    fluidRow(tags$p("If you want, you can also upload a FY timetable"),
             #                             tags$em("Please note this utility is still under development."),
             #                             fileInput(inputId = "timetable",
             #                                       label ="Upload your timetable xlsx file here")),
             #                    fluidRow(tags$h2("MiniCEx Snapshot"),
             #                             tags$p("Once your MiniCEx data is uploaded it will appear here. It shoud look like your excel file, but with some extra columns at the end.
             #                                    If this doesn't look right at this stage, double check that your file hasn't been changed from the version downloaded
             #                                    from Sharepoint."),
             #                             tags$p("You can search and browse through the MiniCEx data here, e.g. search for student matric `s1234567` or assessor `Alex Corbishley`."),
             #                             DTOutput("raw_data")),
             #                    fluidRow(tags$h2("Yearlist Snapshot"),
             #                             DTOutput("raw_yl")))
             # ),
             
             # -------------- Output -----------------------         
             
             tabPanel(title = "MiniCEx Report",
                      fluidPage(fluidRow(column(width = 12,
                                                tags$h1("About this report"),
                                                tags$p("This is an in-development version of an app to analyse MiniCEx data. The intention is that the FY team at R(D)SVS will be able to use this app to process and analyse MiniCEx data as needed."),
                                                tags$p("This page expects three files to exist:"),
                                                tags$li("In Fieldsec > BVMS Years > Final Year > Timetables & Student Groupings > 2023-23 > USE THIS ONE WORKING COPY FY23-24 core rotation timetable.xlsx"),
                                                tags$li("In Fieldsec > BVMS Years > Final Year > MiniCEx Analyses > AY 2023-2024 > App Data - Save Your MiniCEx Spreadsheet here > FY MiniCE 2023_2024.xlsx"),
                                                tags$li("In Fieldsec > BVMS Years > Final Year > MiniCEx Analyses > AY 2023-2024 > App Data - Save Your MiniCEx Spreadsheet here > FY MiniCE 2023_2024_RowsToEdit.xlsx"),
                                                tags$p(),
                                                tags$p ("If you are having issues with the app, please check that these files are present first. Note that 'FY MiniCE 2023_2024.xlsx' should be saved straight from Teams, and `FY MiniCE 2023_2024_RowsToEdit.xlsx` should only contain rows that need to be edited. If this does not fix the issue, email Jill."),
                                                tags$h4("Version 1"),
                                                tags$h2("Notes"),
                                                tags$h3("Date of file read:"),
                                                tags$p("This MiniCEx analysis was run on data from:", textOutput(outputId = "t_dateofdata")),
                                                tags$h2("MiniCEx Submissions to Check"),
                                                tags$p("These data have an issue where the matriculation number does not match with data collected by MS Forms. 
                                                       These are likely typos in the matriculation but should be reviewed by a human eye and if erroneous data is spotted,
                                                       copy and paste the row into the edit spreadsheet and (as good practice) highlight the cells you change. After you have
                                                       saved the data, reload the app. Note that any entry in the table below will be creating some errors in the data,
                                                       e.g. potentially showing a student has fewer than required number of MiniCExs until they are fixed."),
                                                tableOutput(outputId = "t_matsmatch"),
                                                tags$h2("Students who have not yet contributed a MiniCEx"),
                                                tableOutput(outputId = "t_nocontrib"),
                                                tags$h2("Students who have completed fewer than the required number of tasks"),
                                                tableOutput(outputId = "t_net")
                                                )),
                                fluidRow(column(width = 12,
                                                tags$h2("Distribution of task type by date"),
                                                plotOutput(outputId = "p_datetasks")
                                                )),
                                fluidRow(column(width = 12,
                                                tags$h2("Species used for MiniCEx per student"),
                                                plotOutput(outputId = "p_sppxmat"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Species logged per week"),
                                                plotOutput(outputId = "p_sppxweek"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Overall performance marks by date"),
                                                plotOutput(outputId = "p_fbackxweek"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Overall performance by species and date"),
                                                plotOutput(outputId = "p_fbackxweekfspp"),
                                                plotOutput(outputId = "p_fbackxspp"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Students performing below expected level"),
                                                tags$p("Students who performed below expected level, task and assessor shown. New students shown at bottom of table"),
                                                tableOutput(outputId = "t_below"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Entrustability scores by student"),
                                                plotOutput(outputId = "p_entrustxmatric"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Entrustability by Task"),
                                                plotOutput(outputId = "p_entrustxtask"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Overall feedback by Task"),
                                                plotOutput(outputId = "p_fbackxtask"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Students with no surgery task"),
                                                tableOutput(outputId = "t_nosurg"))),
                                fluidRow(column(width = 12,
                                                tags$h2("Rotations"),
                                                tableOutput(outputId = "t_rotations")))
                                )),
             
             
             
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