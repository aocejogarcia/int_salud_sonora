#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
## app.R ##
library(shiny)
library(shinydashboard)
library(shinylive)
library(dplyr)
library(tidyr)
library(DT)
library(googlesheets4)
library(shinyjs)
library(digest)
library(base)
library(dplyr)

#catalogo <- readRDS('../snsp_inteligencia/zoonosis/catalogo.rds') 
catalogo <- readRDS('catalogo.rds')
respuestas <- readRDS('opcion_multiple.rds')

#loadData <- function() {
#  files <- list.files(file.path(responsesDir), full.names = TRUE)
#  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#  data <- do.call(rbind, data)
#  data
#}


humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#saveData <- function(data) {
#  fileName <- sprintf("%s_%s.csv",
#                      humanTime(),
#                      digest::digest(data))
#  
#  write.csv(x = data, file = file.path(responsesDir, fileName),
#            row.names = FALSE, quote = TRUE)
#}
#table <- "responses"

#SHEET_ID <- 'https://docs.google.com/spreadsheets/d/1Bwz3qwR132x3nBmmAE_Yvi8x91aFtiMmoNj3Fl62kIk/edit?usp=sharing'

gs4_auth(path = "zippy-acronym-328605-b21a0b43cc6c.json", email = "service-account@zippy-acronym-328605.iam.gserviceaccount.com", cache = "secrets")

SHEET_ID <- '1Bwz3qwR132x3nBmmAE_Yvi8x91aFtiMmoNj3Fl62kIk'
#googlesheets4::gs4_auth_configure(api_key = "AIzaSyAOvrtxCMVI5ydvoiXevWYbsoTpgmbMleI")
#googlesheets4::gs4_deauth()

# Code to save new responses: ----
saveData <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append(SHEET_ID, data)
}

# Code to read all responses: ----
loadData <- function() {
  read_sheet(SHEET_ID)
}


labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

fieldsMandatory <- c("name", "favourite_pkg")

#appCSS <- ".mandatory_star { color: red; }"

appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"


fieldsAll <- c("name", "favourite_pkg", "used_shiny", "r_num_years", "os_type")

responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

## UI ##
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = 'ACCIONES EXTRAMUROS'),
                    dashboardSidebar(
                      sidebarMenu(
                      #  menuItem("Ectodesparasitación", tabName = "ecto", icon = icon("vial-virus", lib = "font-awesome")),
                      #  menuItem("Entomología", tabName = "ento", icon = icon("bug", lib = "font-awesome")),
                      #  menuItem("Rociado residual", tabName = "rociado", icon = icon("spray-can", lib = "font-awesome"))
                      )#,
                      #selectInput(inputId = 'AÑO',
                      #            label = 'Año de defunción',
                      #            choices = c(2020, 2021, 2022, 2023, 2024, 2025),
                      #            selected = 2024)#,
                      #                      selectInput(inputId = 'DSB',
                      #                                  label = 'Seleccione el Distrito de Salud para el Bienestar',
                      #                                  choices = c(sort(unique(defunciones$DSBRESIDENCIA)), 'Todos'),
                      #                                  selected = 'Todos'),
                      #                      selectInput(inputId = 'MUNICIPIO',
                      #                                  label = 'Seleccione el municipio',
                      #                                  choices = c(unique(opciones_DSB$MUN), 'Todos'),
                      #                                  selected = 'Todos'),
                      #                      selectInput(inputId = 'EDAD',
                      #                                  label = 'Seleccione el grupo de edad',
                      #                                  choices = c('Menores de 1 año', 'De 1 a 4 años', 'De 5 a 9 años', 'De 10 a 19 años', 'De 20 a 59 años', '60 años y más', 'General'),
                      #                                  selected = 'General'),
                      #                      selectInput(inputId = 'SEXO',
                      #                                  label = 'Seleccione el sexo',
                      #                                  choices = c('Mujeres', 'Hombres', 'Ambos'),
                      #                                  selected = 'Ambos')
                    ),
                    dashboardBody(shinyjs::useShinyjs(),
                                  shinyjs::inlineCSS(appCSS),
                                  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))), 
                                  #DT::dataTableOutput("responsesTable"),
                                  downloadButton("downloadBtn", "Download responses"),
                                  div(
                                    id = "form",
                                    selectInput(inputId = 'AREA', 
                                                   label = 'Selecciona el área de captura:',
                                                   choices = respuestas %>% select(AREA_CAPTURA) %>% filter(!is.na(AREA_CAPTURA)), 
                                                selected = ''),
                                    dateInput(inputId = 'FECHA',
                                              label = 'Selecciona la fecha en la que se realizaron las acciones:',
                                              format = 'dd-mm-yyyy',
                                              language = 'es'),
                                    selectInput(inputId = 'DISTRITO', 
                                                label = 'Distrito de Salud donde se realizaron las acciones:',
                                                choices = respuestas %>% select(DISTRITO) %>% filter(!is.na(DISTRITO)), 
                                                selected = ''),
                                    selectInput(inputId = 'MUNICIPIO', 
                                                label = 'Seleccione municipio', 
                                                choices = unique(catalogo$NOM_MUN), 
                                                selected = ''),
                                    selectInput(inputId = 'LOCALIDAD', 
                                                label = 'Seleccione localidad', 
                                                choices = NULL, 
                                                selected = ''),
                                    selectInput(inputId = 'MOTIVO', 
                                                label = 'Motivo de la intervención:', 
                                                choices = respuestas %>% select(MOTIVO_INT) %>% filter(!is.na(MOTIVO_INT)), 
                                                selected = ''),
                                    selectInput(inputId = 'TIPO', 
                                                label = 'Tipo de intervención:', 
                                                choices = respuestas %>% select(TIPO_INT) %>% filter(!is.na(TIPO_INT)), 
                                                selected = ''),
                                    sliderInput('num_consultas', 'Número de consultas médicas otorgadas', 0, 500, 0, ticks = FALSE),
                                    sliderInput('num_consul_nino', 'Consultas de niño sano (menores de 9 años)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_ansiedad', 'Detecciones de casos de ansiedad (mayores de 5 años)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_depresion', 'Detecciones de casos de depresión (mayores de 5 años)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_violencia', 'Detecciones de violencia en la mujer (12-55 años)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_adicciones', 'Detecciones de adicciones', 0, 500, 0, ticks = FALSE),
                                    sliderInput('exa_visual', 'Exámenes de agudeza visual', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_enf_cronica', 'Detecciones de enfermedades crónicas (número de cuestionarios de
                                                factores de riesgo aplicados)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_enveje', 'Detecciones asociadas al envejecimiento (alteraciones de memoria, incontinencia urinaria, 
                                                factores de riesgo de caídas y depresión)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('prueba_vih', 'Pruebas rápidas de VIH realizadas', 0, 500, 0, ticks = FALSE),
                                    sliderInput('prueba_sifilis', 'Pruebas rápidas de Sífilis realizadas', 0, 500, 0, ticks = FALSE),
                                    sliderInput('prueba_hepatitis', 'Pruebas rápidas de Hepatitis C realizadas', 0, 500, 0, ticks = FALSE),
                                    sliderInput('citologias', 'Citologías cervicales en mujeres de 25 a 34 años', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_vph', 'Detecciones de VPH de primera vez (35 a 64 años)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('ex_mama', 'Exploraciones clínicas de mama (25 a 39 años)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('prueba_glucemia', 'Pruebas de glucemia capilar realizadas', 0, 500, 0, ticks = FALSE),
                                    sliderInput('tamizajes', 'Tamizajes de presión arterial', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_vigilanciae', 'Detecciones de casos sujetos a vigilancia epidemiológica', 0, 500, 0, ticks = FALSE),
                                    sliderInput('det_colera', 'Detecciones de casos sospechosos a cólera y monitoreo de enteropatógenos', 0, 500, 0, ticks = FALSE),
                                    sliderInput('migrantes', 'Número de migrantes atendidos', 0, 500, 0, ticks = FALSE),
                                    sliderInput('indigenas', 'Número de personas atendidas que se identifican como indígenas', 0, 500, 0, ticks = FALSE),
                                    sliderInput('activos_visita', 'Total de activos visitados (negocios, escuelas, iglesias, líderes vecinales, entre otros)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('casas_visita', 'Total de casas promocionadas (excluir casas ausentes, renuentes o deshabitadas)', 0, 500, 0, ticks = FALSE),
                                    selectInput(inputId = 'ENTORNOS', 
                                                label = 'Entornos con brechas de salud colectiva:',
                                                choices = respuestas %>% select(ENTORNO) %>% filter(!is.na(ENTORNO)), 
                                                selected = ''),
                                    sliderInput('caf_visita', 'Total de Consultorios Anexos a Farmacia (CAF) visitados', 0, 500, 0, ticks = FALSE),
                                    sliderInput('material_entrega', 'Total de material entregado (dípticos, trípticos y carteles)', 0, 500, 0, ticks = FALSE),
                                    sliderInput('vso_entrega', 'Total de Vida Suero Oral (VSO) entregados', 0, 500, 0, ticks = FALSE),
                                    sliderInput('preserex_entrega', 'Total de preservativos externos/masculinos entregados', 0, 500, 0, ticks = FALSE),
                                    sliderInput('preserint_entrega', 'Total de preservativos internos/femeninos entregados', 0, 500, 0, ticks = FALSE),
                                    sliderInput('lubricantes', 'Gel lubricante a base de agua entregados', 0, 500, 0, ticks = FALSE),
                                    selectInput(inputId = 'MECANISMO', 
                                                label = 'Mecanismo de educación para la salud:',
                                                choices = respuestas %>% select(MECANISMO) %>% filter(!is.na(MECANISMO)), 
                                                selected = ''),
                                    selectizeInput(inputId = 'TEMATICA', 
                                                label = 'Temática abordada en el mecanismo de educación para la salud (pueden ser varios):',
                                                choices = respuestas$TEMATICA, 
                                                selected = '',
                                                multiple = T),
                  
                                    textInput("favourite_pkg", "Favourite R package"),
                                    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
                                    sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
                                    selectInput("os_type", "Operating system used most frequently",
                                                c("",  "Windows", "Mac", "Linux")),
                                    actionButton("submit", "Submit", class = "btn-primary")
                                  ),
                                  shinyjs::hidden(
                                    div(
                                      id = "thankyou_msg",
                                      h3("Thanks, your response was submitted successfully!"),
                                      actionLink("submit_another", "Submit another response")
                                    )
                                  ),
                                  shinyjs::hidden(
                                    span(id = "submit_msg", "Submitting..."),
                                    div(id = "error",
                                        div(br(), tags$b("Error: "), span(id = "error_msg"))
                                    )
                                  )
                                  
                    )
)


server <- function(input, output, session) {
  
  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  ) 
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  
  # action to take when submit button is pressed
  #  observeEvent(input$submit, {
  #    saveData(formData())
  #  })
  
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # action to take when submit button is pressed
  #observeEvent(input$submit, {
  #  saveData(formData())
  #  shinyjs::reset("form")
  #  shinyjs::hide("form")
  #  shinyjs::show("thankyou_msg")
  #})
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })    
  
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("mimic-google-form_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$MUNICIPIO, {
    municipios <- catalogo
    
    updateSelectInput(session = session,
                      inputId = 'LOCALIDAD',
                      label = 'Seleccione localidad',
                      choices = unique(municipios$NOM_LOC[municipios$NOM_MUN == input$MUNICIPIO]),
                      selected = '')
    
  })
  
}

shinyApp(ui, server)

#shinylive::export(appdir = '../snsp_inteligencia/defunciones', destdir = '../snsp_inteligencia/defunciones/docs')
