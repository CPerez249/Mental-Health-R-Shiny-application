library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(DT)




data <- read_excel("CUIDADO DE LA SALUD MENTAL EN EL ÁMBITO UNIVERSITARIO(1-403).xlsx", sheet = "Sheet1", .name_repair = "unique")

# Function to detect the columns with the following words
find_matching_column <- function(keyword, column_names) {
  matched_col <- column_names[grepl(keyword, column_names, ignore.case = TRUE)]
  if (length(matched_col) > 0) { 
    return(matched_col[1])
  } 
  else {
    stop(paste("Couldn't find a column with a name that has:", keyword))
  }
}

# Matches the excel columns with the ones we will use in the code

demographic_col <- find_matching_column("País de origen", colnames(data))
age_range_col <- find_matching_column("Rango de edad:", colnames(data))
degree_col <- find_matching_column("Ámbito de conocimiento de los estudios que estás cursando:", colnames(data)) 
years_at_uni_col<- find_matching_column("¿Cuántos años llevas en la universidad?", colnames(data))
pending_subjects_col <- find_matching_column("¿Tienes asignaturas pendientes de cursos anteriores?", colnames(data))
stress_ffc_col <- find_matching_column("¿Con qué frecuencia experimentas estrés por conflictos en tus relaciones familiares, de amistad, o de pareja?", colnames(data))
ffc_conflicts_col <- find_matching_column("¿Crees que los conflictos familiares, de amistad o de pareja impactan en otros planos de tu vida (académico, laboral, social, emocional...)?", colnames(data))
toxic_relationship_col <- find_matching_column("¿Crees que sabrías identificar si estás inmerso/a en una relación complicada/tóxica?", colnames(data))
electronic_device_procastination_col <- find_matching_column("¿En qué medida crees que el uso de dispositivos electrónicos", colnames(data))
guilty_procastination_col <- find_matching_column("¿En qué medida te sientes culpable después de procrastinar una tarea?", colnames(data))
postpone_tasks_col <- find_matching_column("¿Con qué frecuencia pospones tareas importantes debido a tu estado de ánimo?", colnames(data))
frustation_col <- find_matching_column("¿Con qué frecuencia experimentas frustración (debida a factores académicos, personales, familiares, laborales, deportivos...)?", colnames(data))
emotions_mistake_col <- find_matching_column("las emociones que se te generan después de haberte equivocado en una tarea?", colnames(data))
failed_frustation_col <- find_matching_column("Cuando no consigues alcanzar tus objetivos habiéndote esforzado al máximo, ¿durante cuánto tiempo experimentas una sensación de frustración/fracaso?", colnames(data))
insomnia_col <- find_matching_column("dificultades para conciliar el sueño", colnames(data))
anhedonia_col <- find_matching_column("¿Con qué frecuencia te encuentras sin ganas de hacer nada (anhedonia)?", colnames(data))
guilty_failed_objective_col <- find_matching_column("¿Con qué frecuencia tienes sentimientos de castigo o culpabilidad sobre ti mismo/a por no cumplir con las metas/objetivos que te has propuesto?", colnames(data))
social_networks_selfesteem_col <- find_matching_column("¿En qué medida la exposición constante a estilos de vida idealizados en las redes sociales influye en tu autopercepción/autoestima?", colnames(data))
erase_profile_col <- find_matching_column("¿Con qué frecuencia te planteas borrar tu perfil de alguna de tus redes sociales debido a los efectos que ejercen sobre ti?", colnames(data))
social_networks_mofify_lifestyle_col <- find_matching_column("¿En qué medida consideras que la influencia de las redes sociales puede llegar a modificar el estilo de vida de una persona?", colnames(data))
care_image_col <- find_matching_column("¿En qué medida te importa la percepción/imagen que los demás tienen sobre ti?", colnames(data))
grades_selfesteem_col <- find_matching_column("¿Crees que tus resultados académicos afectan a tu autoestima y a la percepción que tienes de ti mismo/a?", colnames(data))
satisfaction_col <- find_matching_column("En términos generales ¿te sientes a ", colnames(data))
stress_lifestyle_grades_col <- find_matching_column("¿Sientes estrés al intentar compaginar tu vida social con los estudios?", colnames(data))
stess_after_degree_col <- find_matching_column("¿Pensar en tu vida después de la carrera te genera estrés?", colnames(data))
stress_exercise_col <- find_matching_column("¿Crees que realizar actividad física te puede ayudar a la prevención o a la mejoría de niveles elevados de estrés?", colnames(data))
#print(data[[guilty_procastination_col]])
#print(data[[postpone_tasks_col]])
# Generate tables
head(data)

for (row in 1:nrow(data)) {
  if (data[row, demographic_col]=="España"){
    data[row, "Country"] <- "Spain"
  }
  else if (data[row, demographic_col]=="Paraguay") {
    data[row, "Country"] <- "Paraguay"
  }
  else{
    data[row, "Country"] <- "Other"
  }
} 

for (row in 1:nrow(data)) {
  if (data[row, age_range_col]=="18-24 años"){
    data[row, "Age"] <- "18-24 years old"
  }
  else if (data[row, age_range_col]=="25-30 años"){
    data[row, "Age"] <- "25-30 years old"
  }
  else{
    data[row, "Age"] <- "> 30 years old"
  }
  
}

for (row in 1:nrow(data)) {
  if (data[row, degree_col]=="Ingeniería o Arquitectura"){
    data[row, "Degree"] <- "Engineering or Architecture"
  }
  else if (data[row, degree_col]=="Ciencias Sociales o Jurídicas"){
    data[row, "Degree"] <- "Social or Legal Sciences"
  }
  else if (data[row, degree_col]=="Ciencias de la Salud"){
    data[row, "Degree"] <- "Health Sciences"
  }
  else if (data[row, degree_col]=="Ciencias (matemáticas, física, química...)"){
    data[row, "Degree"] <- "Sciences (mathematics, physics, chemistry...)"
  }
  else{
    data[row, "Degree"] <- "Arts and Humanities"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, pending_subjects_col]=="Sí"){
    data[row, "Subjects"] <- "Yes"
  }
  else{
    data[row, "Subjects"] <- "No"
  }
} 

for (row in 1:nrow(data)) {
  if (data[row, toxic_relationship_col]=="Sí"){
    data[row, "ToxicRelationship"] <- "Yes"
  }
  else{
    data[row, "ToxicRelationship"] <- "No"
  }
} 

for (row in 1:nrow(data)) {
  if (data[row, postpone_tasks_col]=="1"){
    data[row, "PostponesTasks"] <- "1"
  }
  else if (data[row, postpone_tasks_col]=="2"){
    data[row, "PostponesTasks"] <- "2"
  }
  else if (data[row, postpone_tasks_col]=="3"){
    data[row, "PostponesTasks"] <- "3"
  }
  else if (data[row, postpone_tasks_col]=="4"){
    data[row, "PostponesTasks"] <- "4"
  }
  else{
    data[row, "PostponesTasks"] <- "5"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, frustation_col]=="1"){
    data[row, "Frustration"] <- "1"
  }
  else if (data[row, frustation_col]=="2"){
    data[row, "Frustration"] <- "2"
  }
  else if (data[row, frustation_col]=="3"){
    data[row, "Frustration"] <- "3"
  }
  else if (data[row, frustation_col]=="4"){
    data[row, "Frustration"] <- "4"
  }
  else{
    data[row, "Frustration"] <- "5"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, emotions_mistake_col]=="1"){
    data[row, "Emotions"] <- "1"
  }
  else if (data[row, emotions_mistake_col]=="2"){
    data[row, "Emotions"] <- "2"
  }
  else if (data[row, emotions_mistake_col]=="3"){
    data[row, "Emotions"] <- "3"
  }
  else if (data[row, emotions_mistake_col]=="4"){
    data[row, "Emotions"] <- "4"
  }
  else{
    data[row, "Emotions"] <- "5"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, failed_frustation_col]=="1"){
    data[row, "Failed"] <- "1"
  }
  else if (data[row, failed_frustation_col]=="2"){
    data[row, "Failed"] <- "2"
  }
  else if (data[row, failed_frustation_col]=="3"){
    data[row, "Failed"] <- "3"
  }
  else if (data[row, failed_frustation_col]=="4"){
    data[row, "Failed"] <- "4"
  }
  else{
    data[row, "Failed"] <- "5"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, care_image_col]=="1"){
    data[row, "Perception"] <- "Not at all"
  }
  else if (data[row, care_image_col]=="2"){
    data[row, "Perception"] <- "Very little"
  }
  else if (data[row, care_image_col]=="3"){
    data[row, "Perception"] <- "Somewhat"
  }
  else if (data[row, care_image_col]=="4"){
    data[row, "Perception"] <- "Quite a bit"
  }
  else{
    data[row, "Perception"] <- "A lot"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, care_image_col]=="1"){
    data[row, "Stress"] <- "Never"
  }
  else if (data[row, care_image_col]=="2"){
    data[row, "Stress"] <- "Rarely"
  }
  else if (data[row, care_image_col]=="3"){
    data[row, "Stress"] <- "Sometimes"
  }
  else if (data[row, care_image_col]=="4"){
    data[row, "Stress"] <- "Quite often"
  }
  else{
    data[row, "Stress"] <- "Always"
  }
}

for (row in 1:nrow(data)) {
  if (data[row, care_image_col]=="1"){
    data[row, "Conflicts"] <- "Never"
  }
  else if (data[row, care_image_col]=="2"){
    data[row, "Conflicts"] <- "Rarely"
  }
  else if (data[row, care_image_col]=="3"){
    data[row, "Conflicts"] <- "Sometimes"
  }
  else if (data[row, care_image_col]=="4"){
    data[row, "Conflicts"] <- "Quite often"
  }
  else{
    data[row, "Conflicts"] <- "Always"
  }
}

head(data)
tables1 <- fluidRow(
  box(
    title="Age range vs Country of origin",
    plotOutput("Age", height = "300px")
    ,checkboxGroupInput("Age", "Age",
                        c("18-24 years old" = "18-24 años",
                          "25-30 years old" = "25-30 años",
                          ">30 years old" = "> 30 años")),
    style = "height: 500px;"
  ),
  box(
    title="Degree vs Pending subjects",
    plotOutput("DegreeSubjects", height = "300px")
    ,checkboxGroupInput("Degree", "Degree",
                        c("Engineering or Architecture" = "Ingeniería o Arquitectura",
                          "Social or Legal Sciences" = "Ciencias Sociales o Jurídicas",
                          "Health Sciences" = "Ciencias de la Salud",
                          "Sciences (mathematics, physics, chemistry...)" = "Ciencias (matemáticas, física, química...)",
                          "Arts and Humanities" = "Artes y Humanidades")),
    style = "height: 500px;"
  ),
  box(
    title = "Number of Participants by Degree and self-satisfaction",
    plotOutput("Degree", height = "300px", width = "1000px")
    ,sliderInput("satisfaction_slider", "Select level of satisfaction ", min=1,max=5, value= c(1,5), step=1),
    width = 12
  )
)

tables2 <- fluidRow(
  box(
    title="Frequency of stress due to conflicts vs Would you realize if you are in a toxic relationship",
    plotOutput("ToxicRelationship1", height = "300px")
    ,checkboxGroupInput("ToxicRelationship1", "Would you realize if you are in a toxic relationship",
                        c("Yes" = "Sí",
                          "No" = "No")),
    style = "height: 500px;"
  ),
  box(
    title="Do you believe conflicts affect on your daily life vs Would you realize if you are in a toxic relationship",
    plotOutput("ToxicRelationship2", height = "300px")
    ,checkboxGroupInput("ToxicRelationship2", "Would you realize if you are in a toxic relationship",
                        c("Yes" = "Sí",
                          "No" = "No")),
    style = "height: 500px;"
  )
)

tables3 <- fluidRow(
  box(
    title="How much do you believe electronic devices affect the procrastination of your tasks",
    plotOutput("Procrastination1", height = "300px"),
    style = "height: 500px;"
  ),
  box(
    title="To what point do you feel guilty about postponing your tasks vs How often do you postpone your tasks due to your feelings",
    plotOutput("Postpones_tasks1", height = "300px")
    ,checkboxGroupInput("Postpones_tasks1", "How often do you postpone your tasks due to your feelings? 1=never 5=always",
                        c("1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5")),
    style = "height: 500px;"
    
  )
)
tables4 <- fluidRow(
  box(
    title="How often do you experience frustration (due to academic, personal, family, work, sports factors...)?",
    plotOutput("Frustration1", height = "300px")
    ,checkboxGroupInput("Frustration1", "How often do you experience frustration (due to academic, personal, family, work, sports factors...)? 1=never 5=always",
                        c("1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5")),
    style = "height: 500px;",
    width = 12
  ),
  box(
    title="How do you manage the emotions that arise after making a mistake in a task?",
    plotOutput("Emotions1", height = "300px")
    ,checkboxGroupInput("Emotions1", "How do you manage the emotions that arise after making a mistake in a task? 1=badly 5=great",
                        c("1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5")),
    style = "height: 500px;",

  ),
  box(
    title="When you fail to achieve your goals despite your best efforts, how long do you experience a feeling of frustration/failure?",
    plotOutput("Failed1", height = "300px")
    ,checkboxGroupInput("Failed1", "How long do you experience a feeling of frustration/failure? 1=don't experience it 5=indefinetly",
                        c("1" = "1",
                          "2" = "2",
                          "3" = "3",
                          "4" = "4",
                          "5" = "5")),
    style = "height: 500px;",

  )
)

tables5 <- fluidRow(
  box(
    title = "Age of Participants by Degree and insomnia",
    plotOutput("DegreeInsomnia1", height = "300px", width = "1000px")
    ,sliderInput("insomnia_slider", "Select level of insomnia ", min=1,max=5, value= c(1,5), step=1),
    width = 12
  ),
  box(
    title = "How often do you find yourself not wanting to do anything (anhedonia)?",
    plotOutput("PostponedAnhedonia", height = "300px", width = "1000px")
    ,sliderInput("anhedonia_slider", "How often do you find yourself not wanting to do anything (anhedonia)? ", min=1,max=5, value= c(1,5), step=1),
    width = 12
  )
)
tables6 <- fluidRow(
  box(
    title = "To what extent do you care about the perception/image that others have about you?",
    plotOutput("Perception1", height = "300px", width = "1000px"),
    plotOutput("Perception2", height = "300px", width = "1000px")
    ,sliderInput("satisfaction_slider", "In general terms, do you feel happy/satisfied with yourself? 1: Not at all satisfied, 2: Slightly satisfied, 3: Somewhat satisfied, 4: Quite satisfied, 5: Very satisfied", min=1,max=5, value= c(1,5), step=1),
    width = 12
  )
)

tables7 <- fluidRow(
  box(
    title = "Do you feel stressed when trying to balance your social life with your studies? 1: Never, 2: Rarely, 3: Sometimes, 4: Quite often, 5: Always",
    plotOutput("StressedHistogram", height = "300px"),
    width = 12
  )
)

#################################### USER INTERFACE #####################################################
header <- dashboardHeader(title = "Mental Health in CEU San Pablo University", titleWidth = 450)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("Participants", tabName = "Participants", icon = icon("user")),
    menuItem("Self-esteem", tabName = "Self-esteem", icon = icon("chart-bar")),
    menuItem("Conflicts", tabName = "Conflicts", icon = icon("chart-bar")),
    menuItem("Procrastination", tabName = "Procrastination", icon = icon("chart-bar")),
    menuItem("Emotional State", tabName = "Emotional-state", icon = icon("chart-bar")),
    menuItem("Tolerance to Frustration", tabName = "Tolerance_to_frustration", icon = icon("chart-bar")),
    menuItem("Stress", tabName = "Stress", icon = icon("chart-bar")),
    menuItem("Dataset", tabName = "Dataset", icon = icon("database"))
  )
)

body <- dashboardBody(tabItems(tabItem(tabName = "Home",
                                       h2("Home"),
                                       p("The main objective of this web application is to demonstrate the feautures available in R-Shiny and how the dynamic interface allows for an easier analysis on Mental Health Data"),
                                       p("All data was obtained from a previous study conducted by the CEU San Pablo University."),
                                       p("The code for the shiny application can be found in the following GitHub repository: https://github.com/CPerez249/Mental_Health" ),
                                       img(src = "university.png", height = "200px", width = "200px")
                                       ),
                               tabItem(tabName = "Participants",tables1),
                               tabItem(tabName = "Self-esteem", tables6),
                               tabItem(tabName = "Conflicts",tables2),
                               tabItem(tabName = "Procrastination",tables3),
                               tabItem(tabName = "Emotional-state",tables5),
                               tabItem(tabName = "Tolerance_to_frustration",tables4),
                               tabItem(tabName = "Stress",tables7),
                               tabItem(tabName = "Dataset",
                                       h2("Dataset"),
                                       p("All data was obtained from a previous study conducted by the CEU San Pablo University."),
                                       p("Proyecto COIL (Collaborative Online International Learning) en colaboración con la Universidad Católica Nuestra Señora de la Asunción, Paraguay, y la CEU San Pablo University, España. CUIDADO DE LA SALUD MENTAL EN EL ENTORNO UNIVERSITARIO: Análisis de la situación actual e iniciativas de futuro."),
                                       downloadButton("DownloadDataset", "Download Dataset")
                               ))) 

ui <- dashboardPage(title = 'Mental Health in University CEU San Pablo ',header, sidebar, body, skin='green')

################################################### SERVER ##########################################################

server <- function(input, output) {
  
  data1 <- reactive({
    subset(data, data[[age_range_col]] %in% input$Age)})
  output$Age <- renderPlot({
    ggplot(data1(), aes(x = Country, fill = Age)) +
      geom_bar() +
      labs(x = "Country of Origin", y= "Number of participants") +
      theme_minimal()
  })
  
  data2 <- reactive({
    subset(data, 
           as.numeric(data[[satisfaction_col]]) >= input$satisfaction_slider[1] &
             as.numeric(data[[satisfaction_col]]) <= input$satisfaction_slider[2]
    )})
  output$Degree <- renderPlot({
    ggplot(data2(), aes(x = Degree, fill = Degree)) +
      geom_bar() +
      labs(x = "Degree", y= "Number of participants") +
      theme_minimal()
  })
  
  data3 <- reactive({
    subset(data, data[[degree_col]] %in% input$Degree)})
  output$DegreeSubjects <- renderPlot({
    ggplot(data3(), aes(x = Subjects, fill = Degree)) +
      geom_bar() +
      labs(x = "Pending subjects", y= "Number of participants") +
      theme_minimal()
  })
  
  data4 <- reactive({
    subset(data, data[[toxic_relationship_col]] %in% input$ToxicRelationship1)})
  output$ToxicRelationship1 <- renderPlot({
    ggplot(data4(), aes(x = Stress, fill = ToxicRelationship)) +
      geom_bar() +
      labs(x = "Frequency of stress", y= "Would you realize if you are in a toxic relationship") +
      theme_minimal()
  })
  
  data5 <- reactive({
    subset(data, data[[toxic_relationship_col]] %in% input$ToxicRelationship2)})
  output$ToxicRelationship2 <- renderPlot({
    ggplot(data5(), aes(x = Conflicts, fill = ToxicRelationship)) +
      geom_bar() +
      labs(x = "Do you believe conflicts affect on your daily life", y= "Would you realize if you are in a toxic relationship") +
      theme_minimal()
  })
  
  #data6 <- reactive({
  #  data})
  #output$Procrastination2 <- renderPlot({
  #  ggplot(data6(), aes(x = .data[[electronic_device_procastination_col]])) +
  #    geom_bar() +
  #    labs(x = "How much do you believe electronic devices affect the procrastination of your tasks", y= "Would you realize if you are in a toxic relationship") +
  #    theme_minimal()
  #})
  data6 <- reactive({
    data %>% group_by(.data[[electronic_device_procastination_col]]) %>% summarise(Count = n())})
  output$Procrastination1 <- renderPlot({
    ggplot(data6(), aes(x = "", y = Count, fill = .data[[electronic_device_procastination_col]])) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(x = NULL, y = NULL, fill = "Procrastination levels") +
      theme_void()
  })
  
  
  data7 <- reactive({
    subset(data, data[[postpone_tasks_col]] %in% input$Postpones_tasks1)})
  output$Postpones_tasks1 <- renderPlot({
    ggplot(data7(), aes(x = .data[[guilty_procastination_col]], fill = PostponesTasks)) +
      geom_bar() +
      labs(x = "Guilt about procrastitation", y= "How often you postpone tasks") +
      theme_minimal()
  })
  
  data8 <- reactive({
    subset(data, data[[frustation_col]] %in% input$Frustration1)})
  output$Frustration1 <- renderPlot({
    ggplot(data8(), aes(x = Degree, fill = Frustration)) +
      geom_bar() +
      labs(x = "Degree", y= "How often do you experience frustration") +
      theme_minimal()
  })
  
  data9 <- reactive({
    subset(data, data[[emotions_mistake_col]] %in% input$Emotions1)})
  output$Emotions1 <- renderPlot({
    ggplot(data9(), aes(x = Age, fill = Emotions)) +
      geom_bar() +
      labs(x = "Age", y= "Number of participants") +
      theme_minimal()
  })
  
  data10 <- reactive({
    subset(data, data[[failed_frustation_col]] %in% input$Failed1)})
  output$Failed1 <- renderPlot({
    ggplot(data10(), aes(x = Emotions, fill = Failed)) +
      geom_bar() +
      labs(x = "How do you manage the emotions that arise after making a mistake", y= "Number of participants") +
      theme_minimal()
  })
  
  data11 <- reactive({
    subset(data, 
           as.numeric(data[[insomnia_col]]) >= input$insomnia_slider[1] &
             as.numeric(data[[insomnia_col]]) <= input$insomnia_slider[2]
    )})
  output$DegreeInsomnia2 <- renderPlot({
    ggplot(data11(), aes(x = Degree, fill = Degree)) +
      geom_bar() +
      labs(x = "Degree", y= "Number of participants with insomnia") +
      theme_minimal()
  })
  
  output$DegreeInsomnia1 <- renderPlot({
    ggplot( data11(),aes(x = Degree, y = Age
                    )) +
      geom_point(alpha = 0.7, color = "blue") +
      geom_jitter(alpha = 0.7, color = "blue", width = 0.2, height = 0.2)+
      labs(x = "Degree", y = "Number of participants") +
      theme_minimal()
  })
  
  data12 <- reactive({
    subset(data, 
           as.numeric(data[[anhedonia_col]]) >= input$anhedonia_slider[1] &
             as.numeric(data[[anhedonia_col]]) <= input$anhedonia_slider[2]
    )})
  output$PostponedAnhedonia <- renderPlot({
    ggplot(data12(), aes(x = PostponesTasks, fill = PostponesTasks)) +
      geom_bar() +
      labs(x = "How often do you postpone tasks due to your emotional state", y= "Number of participants") +
      theme_minimal()
  })
  
  data13 <- reactive({
    subset(data, 
           as.numeric(data[[satisfaction_col]]) >= input$satisfaction_slider[1] &
             as.numeric(data[[satisfaction_col]]) <= input$satisfaction_slider[2]
    )})
  output$Perception1 <- renderPlot({
    ggplot(data13(), aes(x = Perception, y = Age)) +
      geom_point(alpha = 0.7, color = "blue") +
      geom_jitter(alpha = 0.7, color = "blue", width = 0.2, height = 0.2)+
      labs(x = "Perception", y= "Number of participants") +
      theme_minimal()
  })
  output$Perception2 <- renderPlot({
    ggplot(data13(), aes(x = Perception, y=Age)) +
      geom_tile(aes(fill = ..count..), stat = "bin2d") +
      scale_fill_gradient(low = "lightblue", high = "blue") +
      labs(x = "Perception", y= "Number of participants",fill = "Count") +
      theme_minimal()
  })
  
  
  
  output$StressedHistogram <- renderPlot({
    ggplot(data, aes(x = as.numeric(.data[[stress_lifestyle_grades_col]]))) +
      geom_histogram(binwidth = 1,fill = "purple") +
      labs(x = "Stress level", y = "Number of participants", tittle="Do you feel stressed when trying to balance your social life with your studies? 1: Never, 2: Rarely, 3: Sometimes, 4: Quite often, 5: Always") +
      theme_minimal()
  })
  
  output$DownloadDataset <- downloadHandler(
    filename = function(){
      "Mental_Health_Data.xlsx" 
    },
    content = function(file){
      file.copy("CUIDADO DE LA SALUD MENTAL EN EL ÁMBITO UNIVERSITARIO(1-403).xlsx", file)
    }
  )
  
}



shinyApp(ui, server)
