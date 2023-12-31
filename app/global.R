





mcex_read <- function(file_path, sheet = "Form1") {
  readxl::read_excel(file_path) |> 
  janitor::clean_names()  |> 
    rename("rowID" = "id",
           "Email" = "email",
           "AutoName" = "name",
           "StudentName" = "student_name",
           "GivenMatric" = "student_matriculation_number_eg_s190001",
           "Assessor" = "assessor_name",
           "SelfComplete" = "is_the_student_completing_this_form_on_behalf_of_the_assessor",
           "Rotation" = "rotation_week",
           "Species" = "what_species_did_you_work_with",
           "Referral" = "was_this_task_a_referral_level_procedure_see_guidance",
           "MainTask" = "please_indicate_the_nature_of_the_task_what_was_the_main_activity",
           "TaskSummary" = "brief_summary_of_the_task",
           "DateEvent" = "date_of_feedback",
           "Organisation" = "organisation_well_organised_approach_to_the_task_evidence_of_suitable_preparation_beforehand",
           "Communication" = "communication_effectively_communicates_with_client_colleagues_including_appropriate_use_of_language_non_verbal_skills_and_rapport" ,
           "History" = "history_taking_where_applicable_methodical_approach_relevant_information_gathered_appropriate_recording",
           "PhysicalExam" = "physical_examination_of_patient_where_applicable_methodical_approach_competent_examination_performed_relevant_data_gathered_and_recorded_considerate_patient_handling_throughout",
           "ClinicalSkills" = "clinical_skills_task_competently_performed_with_without_instruction_good_manual_dexterity_safe_and_appropriate_handling_of_equipment" ,
           "ClinicalReasoning" = "clinical_reasoning_appropriate_treatment_plan_formulated_based_on_information_available_rational_selection_of_further_diagnostic_tests_procedures_or_next_steps",
           "OverallAssessorFeedback" = "assessor_feedback_on_students_overall_professionalism_and_competence_on_this_occasion",
           "Feedback_forNextTime" = "please_provide_some_overall_feedback_for_the_student_on_what_they_did_well_and_what_they_could_improve_on_next_time",
           "StudentReflection" = "student_self_reflection_on_feedback_received_and_next_steps",
           "StudentDeclaration" = "student_declaration_i_confirm_that_i_have_checked_the_contents_of_this_form_with_my_assessor_prior_to_submission_and_they_have_approved_it") %>%
    mutate(GivenMatric = tolower(GivenMatric),
           matric = str_extract(GivenMatric, "\\D\\d\\d\\d\\d\\d\\d\\d"),
           matric = case_when (is.na(matric) ~ str_extract(Email, "\\D\\d\\d\\d\\d\\d\\d\\d"),
                               TRUE ~ as.character(matric)),
           DateOfTask = case_when(str_detect(DateEvent, "\\d\\d\\d\\d\\d") ~ as.Date(as.numeric(DateEvent), origin = "1899-12-30"),
                                  str_detect(DateEvent,"\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d") ~ dmy(DateEvent),
                                  TRUE ~ NA),
           taskCounter = 1,
           Spp = as.factor(Species),
           Rotation = as_factor(Rotation),
           MainTask = as_factor(MainTask),
           TaskSummary = tolower(TaskSummary),
           ClinicalExam = str_detect(MainTask, "Clinical Examination"),
           Diagnostics = str_detect(MainTask, "Diagnostic Procedures"),
           MedTx = str_detect(MainTask, "Medical Treatment"),
           SurgTx = str_detect(MainTask, "Surgical Treatment"),
           CommsSkills = str_detect(MainTask, "Communication Skills"),
           Dentistry = str_detect(MainTask, "Dentistry"),
           Other = str_detect(MainTask, "Other"),
           OverallAssessorFeedback = factor(OverallAssessorFeedback, levels = c("Above expected level",
                                                                                "At expected level",
                                                                                "Below expected level")),
           WeekN = ymd(DateOfTask),
           WeekN = lubridate::week(WeekN)) |> 
    mutate_at(.vars = vars(c("Organisation":"ClinicalReasoning")),
              .funs = function(x) case_when(x == "N/A" ~ NA,
                                            TRUE ~ as.character(x)))   |> 
    mutate_at(.vars = vars(c("Organisation":"ClinicalReasoning")),
              .funs = function(.) factor(., levels = c("The student didn't require any assistance from me",
                                                       "The student didn't require much assistance from me",
                                                       "I had to provide help at several points",
                                                       "I had to do most/all of the task")))
  
}



mcex_datetasks <- function(minicex_data) {
  
  minicex_data %>%
    group_by(DateOfTask) %>%
    summarise(ClinicalExam = sum(ClinicalExam),
              Diagnostics = sum(Diagnostics),
              MedTx = sum(MedTx),
              SurgTx = sum(SurgTx),
              CommsSkills = sum(CommsSkills),
              Dentistry = sum(Dentistry),
              totalTasks = sum(taskCounter))
  
}







mcex_enoughtasks <- function(minicex_data, week_requirement = 24){
  
  minicex_data <- minicex_data
  
  namesMatric <- minicex_data %>%
    select(matric, StudentName)
  
  
  notEnoughTasks <- mcex_tasks(minicex_data) %>%
    filter(totalTasks < week_requirement) %>%
    select(matric, totalTasks) %>%
    arrange("totalTasks") %>%
    left_join(namesMatric, by="matric") %>%
    mutate(totalTasks = as.numeric(totalTasks)) %>%
    unique() %>%
    group_by(matric, totalTasks) %>%
    nest(StudentName) %>%
    arrange(totalTasks) %>%
    unnest_wider(data, names_sep = "_")
  
  return(notEnoughTasks)
}




mcex_inglis <- function(file_path, what_week = 0){
  
 d <- readxl::read_excel(file_path, sheet = "Timetable")
  
  what_week <- if(is.character(what_week)){as.character(what_week)}
  else{Sys.Date()}
  
  
  d %>%
    filter(WeekN == lubridate::week(what_week),
           Rotation == "Inglis Veterinary Practice") %>%
    select(c(matric,name))
  
}





mcex_longdatetasks <- function(minicex_data){
  
  minicex_data %>%
    mcex_datetasks() %>%
    pivot_longer(cols = -c(DateOfTask, totalTasks), names_to = "Task", values_to = "count")
  
}


mcex_longtasks <- function (minicex_data) {
  
  
  minicex_data %>%
    mcex_tasks() %>%
    pivot_longer(cols = -c(matric, totalTasks), names_to = "Task", values_to = "count")
  
  
}


mcex_matriccheck <- function (minicex_data){
  
  minicex_data %>%
    mutate(automatric = str_extract(Email, "\\D\\d\\d\\d\\d\\d\\d\\d"),
           matmatch = str_equal(x = automatric, y= matric)) %>%
    filter(matmatch == FALSE) %>%
    select(rowID, StudentName, AutoName, matric, Rotation, Email, Assessor, SelfComplete) %>%
    rename("Row ID" = rowID,
           "Student Name" = StudentName,
           "Name of Submitting Account" = AutoName,
           "Matriculation Number" = matric,
           "Was it a self complete" = SelfComplete)
  
  
}




mcex_tasks <- function(minicex_data) {
  
  minicex_data %>%
    group_by(matric) %>%
    summarise(ClinicalExam = sum(ClinicalExam),
              Diagnostics = sum(Diagnostics),
              MedTx = sum(MedTx),
              SurgTx = sum(SurgTx),
              CommsSkills = sum(CommsSkills),
              Dentistry = sum(Dentistry),
              totalTasks = sum(taskCounter))
}







mcex_weekn <- function(CalculationDate = 0, FYStartDate = "20230605", SummerHolidayStartDate = "20230703", XmasHolidayStartDate = "20231218"){
  
  FYStartDate <- as.numeric(FYStartDate)
  
  SummerHolidayStartDate <- as.numeric(SummerHolidayStartDate)
  
  XmasHolidayStartDate <- as.numeric(XmasHolidayStartDate)
  
  CalculationDate <- if(is.character(CalculationDate)){as.numeric(CalculationDate)}
  else{Sys.Date()}
  
  
  
  
  if (ymd(CalculationDate) <= ymd(SummerHolidayStartDate)) {
    as.numeric((ymd(CalculationDate) - ymd(FYStartDate))) / 7
  } else {
    if(as.numeric(ymd(CalculationDate)) <= (as.numeric(ymd(SummerHolidayStartDate))+27)) {
      4
    } else {
      if (ymd(CalculationDate) < ymd(XmasHolidayStartDate)) {
        (as.numeric(ymd(CalculationDate) - ymd(FYStartDate)) - 28)/7
      } else {
        if (ymd(CalculationDate) >= ymd(XmasHolidayStartDate)) { 24
          
        }}}
  }
  
  
  
}



mcexplot_datetasks <- function(minicex_data, date_breaks = "1 week"){
  
  db <- date_breaks
  
  minicex_data %>%
    mcex_longdatetasks() %>%
    ggplot(aes(x=DateOfTask, y=count, fill = Task)) +
    geom_bar(stat = "identity") +
    scale_x_date(date_breaks = {{db}}) +
    theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom')
  
}






mcexplot_facettasks <- function(minicex_data, x_var = week, fill_var = OverallAssessorFeedback, facet_var = Spp){
  
  x_var = enquo(x_var)
  
  fill_var = enquo(fill_var)
  
  facet_var = enquo(facet_var)
  
  ddd <- minicex_data %>%
    mutate(week = week(DateOfTask))
  
  ddd %>%
    ggplot(aes(x={{x_var}}, y=taskCounter, fill = {{fill_var}})) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom') +
    facet_wrap(facets = vars({{facet_var}})) +
    labs(title = "Performance by week of task and species")
  
}



mcexplot_tasks <- function(minicex_data, x_var = matric, fill_var = OverallAssessorFeedback){
  
  x_var = enquo(x_var)
  
  fill_var = enquo(fill_var)
  
  ddd <- minicex_data %>%
    mutate(week = week(DateOfTask))
  
  ddd %>%
    ggplot(aes(x={{x_var}}, y=taskCounter, fill = {{fill_var}})) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom')
  
  
}








ed_palettes <- list(#Edinburgh University Brand Colour Palettes
  "core" = c("#D50032",
             "#041E42"),
  "bright" = c("#830065",
               "#d0006f",
               "#ad033b",
               "#c25e03",
               "#f9a800",
               "#61bf1a",
               "#29c2de",
               "#0099ab"),
  # A selection of the bright palette intended to work for dichotomous scales
  "bright-two-tone" = c("#830065",
                        "#d0006f",
                        "#f9a800",
                        "#29c2de",
                        "#0099ab"),
  # Note the muted palette is not great for colour blind folk
  "muted" = c("#154734",
              "#004f71",
              "#46877f",
              "#c6dbe9",
              "#949108",
              "#ba8285",
              "#704f45",
              "#692e1f"),
  # A selection of the muted palette intended to work for dichotomous scales
  "muted-two-tone"= c("#154734",
                      "#949108",
                      "#c6dbe9",
                      "#ba8285",
                      "#704f45"),
  # Colours optimised for online display per brand guidelines, some poor colourblind choices
  "digital" = c("#a50034",
                "#d0006f",
                "#830065",
                "#6d4f47",
                "#007288",
                "#154734",
                "#333f48",
                "#487a7b",
                "#004f71"),
  # Recruitment colours
  "recruit-ug" = c("#c25e03",
                   "#f9a800",
                   "#61bf1a"),
  "recruit-pg" = c("#830065",
                   "#4a7875",
                   "#c6dbe9"),
  # Recruitment branding with core colours, use with caution, core colours are valuable
  "recruit-ug-core" = c("#d50032",
                        "#c25e03",
                        "#61bf1a",
                        "#f9a800",
                        "#041e42"),
  "recruit-pg-core" = c("#d50032",
                        "#830065",
                        "#4a7875",
                        "#c6dbe9",
                        "#041e42")
  
  
  
  
  
)



ed_col <- function(palette = "bright", n, alpha = 1, reverse = FALSE) {
  
  pal <- ed_palettes[[palette]]
  
  if (is.null(pal))
    stop("Palette not found.")
  
  if(missing(n)) {
    n <- length(pal)
  }
  
  if (reverse) {
    pal <- rev(pal)
  }
  
  pal <- colorRampPalette(pal, alpha)(n)
  
  return(pal)
  
}

#' Creates a palette from the UoE Edinburgh branded colours
#'
#' @param palette Choose from 'ed_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @importFrom grDevices colorRampPalette
ed_pal <- function(palette = "bright", alpha = 1, reverse = FALSE) {
  
  function(n) {
    ed_col(palette, n, alpha, reverse)
  }
  
}


#' Creates a University of Edinburgh colour scale for ggplot2
#'
#'
#' @param palette Choose from 'ed_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' library(UoEColouR)
#'
#' ggplot(mpg) +
#'  geom_point(aes(x = hwy, y = cty, color = manufacturer)) +
#'  scale_color_uoe("bright")
#'
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#'
#'
scale_colour_uoe <- function(palette = "bright", discrete = TRUE, alpha = 1, reverse = FALSE, ...) {
  
  if (discrete) {
    discrete_scale("colour", "ed_col", ed_pal(palette, alpha = alpha, reverse = reverse), ...)
  }
  else {
    scale_color_gradientn(colours = ed_col(palette, 256, alpha = alpha, reverse = reverse), ...)
  }
}



#' Creates a University of Edinburgh fill scale for ggplot2
#'
#' @param palette Choose from 'ed_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams ed_pal
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @examples
#' library(ggplot2)
#' library(UoEColouR)
#'
#' ggplot(aes(x = manufacturer, fill = manufacturer), data = mpg) +
#'   geom_bar() +
#'   scale_fill_uoe()
#' @export
#'


scale_fill_uoe <- function(palette = "bright", discrete = TRUE, alpha = 1, reverse = FALSE, ...) {
  
  if (discrete) {
    discrete_scale("fill", "ed_col", ed_pal(palette, alpha = alpha, reverse = reverse), ...)
  }
  else {
    scale_fill_gradientn(colours = ed_col(palette, 256, alpha = alpha, reverse = reverse), ...)
  }
}