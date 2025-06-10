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

catalogo <- catalogo %>% mutate(DISTRITO = case_when(
  MUN %in% c('001', '005', '008', '009', '013', '014', '020', '021', '023', '024', '028', '030', '032', '034', '037',
                 '038', '040', '041', '044', '045', '050', '052', '053', '054', '056', '057', '061', '062', '063', '066', '067', '068') ~ 'Distrito de Salud para el Bienestar 1',
  MUN %in% c('004', '007', '017', '046', '047', '060', '065') ~ 'Distrito de Salud para el Bienestar 2',
  MUN %in% c('002', '006', '010', '011', '015', '016', '019', '022', '027', '031', '035', '036', '039', '043', '058', '059', '064') ~ 'Distrito de Salud para el Bienestar 3',
  MUN %in% c('012', '018', '025', '029', '049', '051', '069', '072') ~ 'Distrito de Salud para el Bienestar 4',
  MUN %in% c('003', '026', '033', '042', '071') ~ 'Distrito de Salud para el Bienestar 5',
  MUN %in% c('048', '055', '070') ~ 'Distrito de Salud para el Bienestar 6',
  T ~ MUN))

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

#gs4_auth(path = "zippy-acronym-328605-b21a0b43cc6c.json", email = "service-account@zippy-acronym-328605.iam.gserviceaccount.com", cache = "secrets")

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

fieldsMandatory <- c('AREA',	'FECHA',	'MUNICIPIO', 	'LOCALIDAD',	'MOTIVO',	'TIPO')

#appCSS <- ".mandatory_star { color: red; }"

appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"


fieldsAll <- c('AREA',	'FECHA',	'MUNICIPIO',	'LOCALIDAD',	'MOTIVO',	'TIPO',	'con_fem_m1',	'con_fem_1_4',	'con_fem_5_14',	'con_fem_15_24',	'con_fem_25_44',	'con_fem_45_64',	'con_fem_65mas',	'con_mas_m1',	'con_mas_1_4',	'con_mas_5_14',	'con_mas_15_24',	'con_mas_25_44',	'con_mas_45_64',	'con_mas_65mas',	'con_fem_nino_m1',	'con_fem_nino_1_4',	'con_fem_nino_5_14',	'con_mas_nino_m1',	'con_mas_nino_1_4',	'con_mas_nino_5_14',	'ansi_fem_5_14',	'ansi_fem_15_24',	'ansi_fem_25_44',	'ansi_fem_45_64',	'ansi_fem_65mas',	'ansi_mas_5_14',	'ansi_mas_15_24',	'ansi_mas_25_44',	'ansi_mas_45_64',	'ansi_mas_65mas',	'depre_fem_5_14',	'depre_fem_15_24',	'depre_fem_25_44',	'depre_fem_45_64',	'depre_fem_65mas',	'depre_mas_5_14',	'depre_mas_15_24',	'depre_mas_25_44',	'depre_mas_45_64',	'depre_mas_65mas',	'viol_4_14',	'viol_15_24',	'viol_25_44',	'viol_45_64',	'viol_65mas',	'adic_fem_m1',	'adic_fem_1_4',	'adic_fem_5_14',	'adic_fem_15_24',	'adic_fem_25_44',	'adic_fem_45_64',	'adic_fem_65mas',	'adic_mas_m1',	'adic_mas_1_4',	'adic_mas_5_14',	'adic_mas_15_24',	'adic_mas_25_44',	'adic_mas_45_64',	'adic_mas_65mas',	'exvis_fem_m1',	'exvis_fem_1_4',	'exvis_fem_5_14',	'exvis_fem_15_24',	'exvis_fem_25_44',	'exvis_fem_45_64',	'exvis_fem_65mas',	'exvis_mas_m1',	'exvis_mas_1_4',	'exvis_mas_5_14',	'exvis_mas_15_24',	'exvis_mas_25_44',	'exvis_mas_45_64',	'exvis_mas_65mas',	'det_enf_cronica',	'envej_fem_45_64',	'envej_fem_65mas',	'envej_mas_45_64',	'envej_mas_65mas',	'vih_fem_m1',	'vih_fem_1_4',	'vih_fem_5_14',	'vih_fem_15_24',	'vih_fem_25_44',	'vih_fem_45_64',	'vih_fem_65mas',	'vih_mas_m1',	'vih_mas_1_4',	'vih_mas_5_14',	'vih_mas_15_24',	'vih_mas_25_44',	'vih_mas_45_64',	'vih_mas_65mas',	'sifi_fem_m1',	'sifi_fem_1_4',
               'sifi_fem_5_14',	'sifi_fem_15_24',	'sifi_fem_25_44',	'sifi_fem_45_64',	'sifi_fem_65mas',	'sifi_mas_m1',	'sifi_mas_1_4',	'sifi_mas_5_14',	'sifi_mas_15_24',	'sifi_mas_25_44',	'sifi_mas_45_64',	'sifi_mas_65mas',	'hepc_fem_m1',	'hepc_fem_1_4',	'hepc_fem_5_14',	'hepc_fem_15_24',	'hepc_fem_25_44',	'hepc_fem_45_64',	'hepc_fem_65mas',	'hepc_mas_m1',	'hepc_mas_1_4',	'hepc_mas_5_14',	'hepc_mas_15_24',	'hepc_mas_25_44',	'hepc_mas_45_64',	'hepc_mas_65mas',	'citologias',	'vph_25_44',	'vph_45_64',	'exploraciones',	'gluc_fem_m1',	'gluc_fem_1_4',	'gluc_fem_5_14',	'gluc_fem_15_24',	'gluc_fem_25_44',	'gluc_fem_45_64',	'gluc_fem_65mas',	'gluc_mas_m1',	'gluc_mas_1_4',	'gluc_mas_5_14',	'gluc_mas_15_24',	'gluc_mas_25_44',	'gluc_mas_45_64',	'gluc_mas_65mas',	'tamiz_fem_m1',	'tamiz_fem_1_4',	'tamiz_fem_5_14',	'tamiz_fem_15_24',	'tamiz_fem_25_44',	'tamiz_fem_45_64',	'tamiz_fem_65mas',	'tamiz_mas_m1',	'tamiz_mas_1_4',	'tamiz_mas_5_14',	'tamiz_mas_15_24',	'tamiz_mas_25_44',	'tamiz_mas_45_64',	'tamiz_mas_65mas',	'epi_fem_m1',	'epi_fem_1_4',	'epi_fem_5_14',	'epi_fem_15_24',	'epi_fem_25_44',	'epi_fem_45_64',	'epi_fem_65mas',	'epi_mas_m1',	'epi_mas_1_4',	'epi_mas_5_14',	'epi_mas_15_24',	'epi_mas_25_44',	'epi_mas_45_64',	'epi_mas_65mas',	'colera_fem_m1',	'colera_fem_1_4',	'colera_fem_5_14',	'colera_fem_15_24',	'colera_fem_25_44',	'colera_fem_45_64',	'colera_fem_65mas',	'colera_mas_m1',	'colera_mas_1_4',	'colera_mas_5_14',	'colera_mas_15_24',	'colera_mas_25_44',	'colera_mas_45_64',	'colera_mas_65mas',	'migrantes',	'indigenas',	'negocios_visi',	'escuelas_visi',	'iglesias_visi',	'lid_vecinal_visi',	'cas_promo',	'cas_ausentes',	'cas_renuen',	'cas_deshab',	'c_rehab',	'ceresos',	'campo_agri',	'albergue_migra',
               'albergue_otro',	'caf_visita',	'mat_entrega',	'vso_entrega',	'p_ext_entrega',	'p_int_entrega',	'lubri_entrega',	'mecanismo',	'tematica',	'p_sensibilizadas',	'ester_cani_m',	'ester_cani_h',	'ester_feli_m',	'ester_feli_h',	'vac_cani_m',	'vac_cani_h',	'vac_feli_m',	'vac_feli_h',	'desp_cani_m',	'desp_cani_h',	'desp_feli_m',	'desp_feli_h',	'cont_larvario',	'recipientes_rev',	'cant_insect',	'p_ssani',	'p_ecobuq',	'casas_sanea',	'vac_vph_fem_m1',	'vac_vph_fem_1_4',	'vac_vph_fem_5_14',	'vac_vph_fem_15_24',	'vac_vph_fem_25_44',	'vac_vph_fem_45_64',	'vac_vph_fem_65mas',	'vac_vph_mas_m1',	'vac_vph_mas_1_4',	'vac_vph_mas_5_14',	'vac_vph_mas_15_24',	'vac_vph_mas_25_44',	'vac_vph_mas_45_64',	'vac_vph_mas_65mas',	'vactot_vph_mas_m1',	'vactot_vph_mas_1_4',	'vactot_vph_mas_5_14',	'vactot_vph_mas_15_24',	'vactot_vph_mas_25_44',	'vactot_vph_mas_45_64',	'vactot_vph_mas_65mas',	'vac_cov_fem_m1',	'vac_cov_fem_1_4',	'vac_cov_fem_5_14',	'vac_cov_fem_15_24',	'vac_cov_fem_25_44',	'vac_cov_fem_45_64',	'vac_cov_fem_65mas',	'vac_cov_mas_m1',	'vac_cov_mas_1_4',	'vac_cov_mas_5_14',	'vac_cov_mas_15_24',	'vac_cov_mas_25_44',	'vac_cov_mas_45_64',	'vac_cov_mas_65mas',	'vactot_cov_mas_m1',	'vactot_cov_mas_1_4',	'vactot_cov_mas_5_14',	'vactot_cov_mas_15_24',	'vactot_cov_mas_25_44',	'vactot_cov_mas_45_64',	'vactot_cov_mas_65mas',	'vac_hepab_fem_m1',	'vac_hepab_fem_1_4',	'vac_hepab_fem_5_14',	'vac_hepab_fem_15_24',	'vac_hepab_fem_25_44',	'vac_hepab_fem_45_64',	'vac_hepab_fem_65mas',	'vac_hepab_mas_m1',	'vac_hepab_mas_1_4',	'vac_hepab_mas_5_14',	'vac_hepab_mas_15_24',	'vac_hepab_mas_25_44',	'vac_hepab_mas_45_64',	'vac_hepab_mas_65mas',	'vactot_hepab_mas_m1',	'vactot_hepab_mas_1_4',	'vactot_hepab_mas_5_14',	'vactot_hepab_mas_15_24',	'vactot_hepab_mas_25_44',	'vactot_hepab_mas_45_64',	'vactot_hepab_mas_65mas',	'vac_flu_fem_m1',	'vac_flu_fem_1_4',	'vac_flu_fem_5_14',	'vac_flu_fem_15_24',	'vac_flu_fem_25_44',	'vac_flu_fem_45_64',	'vac_flu_fem_65mas',	'vac_flu_mas_m1',	'vac_flu_mas_1_4',
               'vac_flu_mas_5_14',	'vac_flu_mas_15_24',	'vac_flu_mas_25_44',	'vac_flu_mas_45_64',	'vac_flu_mas_65mas',	'vactot_flu_mas_m1',	'vactot_flu_mas_1_4',	'vactot_flu_mas_5_14',	'vactot_flu_mas_15_24',	'vactot_flu_mas_25_44',	'vactot_flu_mas_45_64',	'vactot_flu_mas_65mas',	'vac_hexa_fem_m1',	'vac_hexa_fem_1_4',	'vac_hexa_fem_5_14',	'vac_hexa_fem_15_24',	'vac_hexa_fem_25_44',	'vac_hexa_fem_45_64',	'vac_hexa_fem_65mas',	'vac_hexa_mas_m1',	'vac_hexa_mas_1_4',	'vac_hexa_mas_5_14',	'vac_hexa_mas_15_24',	'vac_hexa_mas_25_44',	'vac_hexa_mas_45_64',	'vac_hexa_mas_65mas',	'vactot_hexa_mas_m1',	'vactot_hexa_mas_1_4',	'vactot_hexa_mas_5_14',	'vactot_hexa_mas_15_24',	'vactot_hexa_mas_25_44',	'vactot_hexa_mas_45_64',	'vactot_hexa_mas_65mas',	'vac_rota_fem_m1',	'vac_rota_fem_1_4',	'vac_rota_fem_5_14',	'vac_rota_fem_15_24',	'vac_rota_fem_25_44',	'vac_rota_fem_45_64',	'vac_rota_fem_65mas',	'vac_rota_mas_m1',	'vac_rota_mas_1_4',	'vac_rota_mas_5_14',	'vac_rota_mas_15_24',	'vac_rota_mas_25_44',	'vac_rota_mas_45_64',	'vac_rota_mas_65mas',	'vactot_rota_mas_m1',	'vactot_rota_mas_1_4',	'vactot_rota_mas_5_14',	'vactot_rota_mas_15_24',	'vactot_rota_mas_25_44',	'vactot_rota_mas_45_64',	'vactot_rota_mas_65mas',	'vac_neu23_fem_m1',	'vac_neu23_fem_1_4',	'vac_neu23_fem_5_14',	'vac_neu23_fem_15_24',	'vac_neu23_fem_25_44',	'vac_neu23_fem_45_64',	'vac_neu23_fem_65mas',	'vac_neu23_mas_m1',	'vac_neu23_mas_1_4',	'vac_neu23_mas_5_14',	'vac_neu23_mas_15_24',	'vac_neu23_mas_25_44',	'vac_neu23_mas_45_64',	'vac_neu23_mas_65mas',	'vactot_neu23_mas_m1',	'vactot_neu23_mas_1_4',	'vactot_neu23_mas_5_14',	'vactot_neu23_mas_15_24',	'vactot_neu23_mas_25_44',	'vactot_neu23_mas_45_64',	'vactot_neu23_mas_65mas',	'vac_sr_fem_m1',	'vac_sr_fem_1_4',	'vac_sr_fem_5_14',	'vac_sr_fem_15_24',	'vac_sr_fem_25_44',	'vac_sr_fem_45_64',	'vac_sr_fem_65mas',	'vac_sr_mas_m1',	'vac_sr_mas_1_4',	'vac_sr_mas_5_14',	'vac_sr_mas_15_24',	'vac_sr_mas_25_44',	'vac_sr_mas_45_64',	'vac_sr_mas_65mas',	'vactot_sr_mas_m1',	'vactot_sr_mas_1_4',	'vactot_sr_mas_5_14',	'vactot_sr_mas_15_24',	'vactot_sr_mas_25_44',	'vactot_sr_mas_45_64',	'vactot_sr_mas_65mas',	'vac_srp_fem_m1',	'vac_srp_fem_1_4',	'vac_srp_fem_5_14',	'vac_srp_fem_15_24',
               'vac_srp_fem_25_44',	'vac_srp_fem_45_64',	'vac_srp_mas_m1',	'vac_srp_mas_1_4',	'vac_srp_mas_5_14',	'vac_srp_mas_15_24',	'vac_srp_mas_25_44',	'vac_srp_mas_45_64',	'vactot_srp_mas_m1',	'vactot_srp_mas_1_4',	'vactot_srp_mas_5_14',	'vactot_srp_mas_15_24',	'vactot_srp_mas_25_44',	'vactot_srp_mas_45_64',	'vac_dpt_fem_m1',	'vac_dpt_fem_1_4',	'vac_dpt_fem_5_14',	'vac_dpt_fem_15_24',	'vac_dpt_fem_25_44',	'vac_dpt_fem_45_64',	'vac_dpt_fem_65mas',	'vac_dpt_mas_m1',	'vac_dpt_mas_1_4',	'vac_dpt_mas_5_14',	'vac_dpt_mas_15_24',	'vac_dpt_mas_25_44',	'vac_dpt_mas_45_64',	'vac_dpt_mas_65mas',	'vactot_dpt_mas_m1',	'vactot_dpt_mas_1_4',	'vactot_dpt_mas_5_14',	'vactot_dpt_mas_15_24',	'vactot_dpt_mas_25_44',	'vactot_dpt_mas_45_64',	'vactot_dpt_mas_65mas',	'vac_bcg_fem_m1',	'vac_bcg_fem_1_4',	'vac_bcg_fem_5_14',	'vac_bcg_fem_15_24',	'vac_bcg_fem_25_44',	'vac_bcg_fem_45_64',	'vac_bcg_fem_65mas',	'vac_bcg_mas_m1',	'vac_bcg_mas_1_4',	'vac_bcg_mas_5_14',	'vac_bcg_mas_15_24',	'vac_bcg_mas_25_44',	'vac_bcg_mas_45_64',	'vac_bcg_mas_65mas',	'vactot_bcg_mas_m1',	'vactot_bcg_mas_1_4',	'vactot_bcg_mas_5_14',	'vactot_bcg_mas_15_24',	'vactot_bcg_mas_25_44',	'vactot_bcg_mas_45_64',	'vactot_bcg_mas_65mas',	'vac_tdpa_fem_m1',	'vac_tdpa_fem_1_4',	'vac_tdpa_fem_5_14',	'vac_tdpa_fem_15_24',	'vac_tdpa_fem_25_44',	'vac_tdpa_fem_45_64',	'vac_tdpa_fem_65mas',	'vac_tdpa_mas_m1',	'vac_tdpa_mas_1_4',	'vac_tdpa_mas_5_14',	'vac_tdpa_mas_15_24',	'vac_tdpa_mas_25_44',	'vac_tdpa_mas_45_64',	'vac_tdpa_mas_65mas',	'vactot_tdpa_mas_m1',	'vactot_tdpa_mas_1_4',	'vactot_tdpa_mas_5_14',	'vactot_tdpa_mas_15_24',	'vactot_tdpa_mas_25_44',	'vactot_tdpa_mas_45_64',	'vactot_tdpa_mas_65mas',	'vac_td_fem_m1',	'vac_td_fem_1_4',	'vac_td_fem_5_14',	'vac_td_fem_15_24',	'vac_td_fem_25_44',	'vac_td_fem_45_64',	'vac_td_fem_65mas',	'vac_td_mas_m1',	'vac_td_mas_1_4',	'vac_td_mas_5_14',	'vac_td_mas_15_24',	'vac_td_mas_25_44',	'vac_td_mas_45_64',	'vac_td_mas_65mas',	'vactot_td_mas_m1',	'vactot_td_mas_1_4',	'vactot_td_mas_5_14',	'vactot_td_mas_15_24',	'vactot_td_mas_25_44',	'vactot_td_mas_45_64',	'vactot_td_mas_65mas',	'vac_neu13_fem_m1',	'vac_neu13_fem_1_4',
               'vac_neu13_fem_5_14',	'vac_neu13_fem_15_24',	'vac_neu13_fem_25_44',	'vac_neu13_fem_45_64',	'vac_neu13_fem_65mas',	'vac_neu13_mas_m1',	'vac_neu13_mas_1_4',	'vac_neu13_mas_5_14',	'vac_neu13_mas_15_24',	'vac_neu13_mas_25_44',	'vac_neu13_mas_45_64',	'vac_neu13_mas_65mas',	'vactot_neu13_mas_m1',	'vactot_neu13_mas_1_4',	'vactot_neu13_mas_5_14',	'vactot_neu13_mas_15_24',	'vactot_neu13_mas_25_44',	'vactot_neu13_mas_45_64',	'vactot_neu13_mas_65mas'
               )



responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

## UI ##
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = 'ACCIONES EXTRAMUROS', titleWidth = '100%'),
                    dashboardSidebar(disable = T,
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
                                  #downloadButton("downloadBtn", "Download responses"),
                                   div(
                                    id = "form",
                                    h3(tags$b('Máscara de captura para monitoreo de acciones extramuros')),
                                    fluidRow(
                                      column(12, 
                                    selectInput(inputId = 'AREA', 
                                                   label = labelMandatory('Selecciona el área de captura:'),
                                                   choices = respuestas %>% select(AREA_CAPTURA) %>% filter(!is.na(AREA_CAPTURA)), 
                                                selected = '')
                                    )
                                    ),
                                    fluidRow(
                                    column(4, wellPanel(
                                    dateInput(inputId = 'FECHA',
                                              label = labelMandatory('Selecciona la fecha en la que se realizaron las acciones'),
                                              format = 'dd-mm-yyyy',
                                              language = 'es'),
                                    selectInput(inputId = 'DISTRITO', 
                                                label = labelMandatory('Distrito de Salud donde se realizaron las acciones'),
                                                choices = sort(unique(catalogo$DISTRITO)), 
                                                selected = '')
                                    )),
                                    column(4, wellPanel(
                                    selectInput(inputId = 'MUNICIPIO', 
                                                label = labelMandatory('Seleccione municipio'), 
                                                choices = unique(catalogo$NOM_MUN), 
                                                selected = ''),
                                    selectInput(inputId = 'LOCALIDAD', 
                                                label = labelMandatory('Seleccione localidad'), 
                                                choices = NULL, 
                                                selected = '')
                                    )),
                                    column(4, wellPanel(
                                    selectInput(inputId = 'MOTIVO', 
                                                label = labelMandatory('Motivo de la intervención'), 
                                                choices = respuestas %>% select(MOTIVO_INT) %>% filter(!is.na(MOTIVO_INT)), 
                                                selected = ''),
                                    selectInput(inputId = 'TIPO', 
                                                label = labelMandatory('Tipo de intervención'), 
                                                choices = respuestas %>% select(TIPO_INT) %>% filter(!is.na(TIPO_INT)), 
                                                selected = '')
                                    ))
                                    ),
                                    tags$hr(style="border-color: black;"),
                                    h3(tags$b('Acciones de atención a salud de la persona')),
                                    h4('Consultas médicas otorgadas'),
                                    fluidRow(
                                    column(6, 
                                           numericInput('con_fem_m1', 'Femenino < 1 año', 0),
                                           numericInput('con_fem_1_4', 'Femenino de 1-4 años', 0),
                                           numericInput('con_fem_5_14', 'Femenino de 5-14 años', 0),
                                           numericInput('con_fem_15_24', 'Femenino de 15-24 años', 0),
                                           numericInput('con_fem_25_44', 'Femenino de 25-44 años', 0),
                                           numericInput('con_fem_45_64', 'Femenino de 45-64 años', 0),
                                           numericInput('con_fem_65mas', 'Femenino de 65 y más', 0)
                                           ),
                                    column(6,
                                           numericInput('con_mas_m1', 'Masculino < 1 año', 0),
                                           numericInput('con_mas_1_4', 'Masculino de 1-4 años', 0),
                                           numericInput('con_mas_5_14', 'Masculino de 5-14 años', 0),
                                           numericInput('con_mas_15_24', 'Masculino de 15-24 años', 0),
                                           numericInput('con_mas_25_44', 'Masculino de 25-44 años', 0),
                                           numericInput('con_mas_45_64', 'Masculino de 45-64 años', 0),
                                           numericInput('con_mas_65mas', 'Masculino de 65 y más', 0)
                                           )
                                    ),
                                    hr(),
                                    h4('Consultas de niño sano'),
                                    fluidRow(
                                     column(6,
                                            numericInput('con_fem_nino_m1', 'Femenino < 1 año', 0),
                                            numericInput('con_fem_nino_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('con_fem_nino_5_14', 'Femenino de 5-14 años', 0)
                                     ),
                                     column(6,
                                            numericInput('con_mas_nino_m1', 'Masculino < 1 año', 0),
                                            numericInput('con_mas_nino_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('con_mas_nino_5_14', 'Masculino de 5-14 años', 0)
                                            )
                                    ),
                                    hr(),
                                    h4('Detecciones de casos de ansiedad (mayores de 5 años)'),
                                    fluidRow(
                                     column(6, 
                                            numericInput('ansi_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('ansi_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('ansi_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('ansi_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('ansi_fem_65mas', 'Femenino de 65 y más', 0)
                                            ),
                                     column(6,
                                            numericInput('ansi_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('ansi_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('ansi_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('ansi_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('ansi_mas_65mas', 'Masculino de 65 y más', 0)
                                            )
                                    ),
                                    hr(),
                                    h4('Detecciones de casos de depresión (mayores de 5 años'),
                                    fluidRow(
                                      column(6,
                                             numericInput('depre_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('depre_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('depre_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('depre_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('depre_fem_65mas', 'Femenino de 65 y más', 0)
                                             ),
                                      column(6,
                                             numericInput('depre_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('depre_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('depre_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('depre_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('depre_mas_65mas', 'Masculino de 65 y más', 0)
                                             ),
                                    ),
                                    hr(),
                                    h4('Detecciones de violencia en la mujer (12-55 años)'),
                                    fluidRow(
                                      column(2, numericInput('viol_4_14', '5-14 años', 0)),
                                      column(2, numericInput('viol_15_24', '15-24 años', 0)),
                                      column(2, numericInput('viol_25_44', '25-44 años', 0)),
                                      column(2, numericInput('viol_45_64', '45-64 años', 0)),
                                      column(2, numericInput('viol_65mas', '65 y más', 0) )
                                    ),
                                    hr(),
                                    h4('Detecciones de adicciones'),
                                    fluidRow(
                                      column(6,
                                             numericInput('adic_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('adic_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('adic_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('adic_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('adic_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('adic_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('adic_fem_65mas', 'Femenino de 65 y más', 0)
                                             ),
                                      column(6,
                                             numericInput('adic_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('adic_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('adic_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('adic_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('adic_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('adic_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('adic_mas_65mas', 'Masculino de 65 y más', 0)
                                             ),
                                    ),
                                    hr(),
                                    h4('Exámenes de agudeza visual'),
                                    fluidRow(
                                      column(6,
                                             numericInput('exvis_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('exvis_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('exvis_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('exvis_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('exvis_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('exvis_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('exvis_fem_65mas', 'Femenino de 65 y más', 0)
                                             ),
                                      column(6,
                                             numericInput('exvis_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('exvis_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('exvis_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('exvis_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('exvis_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('exvis_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('exvis_mas_65mas', 'Masculino de 65 y más', 0)
                                             )
                                    ),
                                    hr(),
                                    h4('Detecciones de enfermedades crónicas (número de cuestionarios de
                                                factores de riesgo aplicados)'),
                                    fluidRow(
                                      column(6, numericInput('det_enf_cronica', 'Cuestionarios', 0))
                                    ),
                                    hr(),
                                    h4('Detecciones asociadas al envejecimiento (alteraciones de memoria, incontinencia urinaria, 
                                                factores de riesgo de caídas y depresión)'),
                                    fluidRow(
                                     column(6, 
                                            numericInput('envej_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('envej_fem_65mas', 'Femenino de 65 y más', 0)),
                                     column(6,
                                            numericInput('envej_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('envej_mas_65mas', 'Masculino de 65 y más', 0))  
                                    ),
                                    hr(),
                                    h4('Pruebas rápidas de VIH realizadas'),
                                    fluidRow(
                                      column(6,
                                             numericInput('vih_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('vih_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('vih_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('vih_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('vih_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('vih_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('vih_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('vih_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('vih_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('vih_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('vih_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('vih_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('vih_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('vih_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Pruebas rápidas de Sífilis realizadas'),
                                    fluidRow(
                                      column(6,
                                             numericInput('sifi_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('sifi_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('sifi_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('sifi_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('sifi_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('sifi_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('sifi_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('sifi_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('sifi_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('sifi_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('sifi_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('sifi_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('sifi_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('sifi_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Pruebas rápidas de Hepatitis C realizadas'),
                                    fluidRow(
                                      column(6,
                                             numericInput('hepc_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('hepc_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('hepc_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('hepc_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('hepc_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('hepc_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('hepc_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('hepc_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('hepc_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('hepc_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('hepc_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('hepc_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('hepc_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('hepc_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Citologías cervicales a mujeres de 25 a 34 años'),
                                    fluidRow(
                                      column(6, numericInput('citologias', 'Realizadas', 0))
                                    ),
                                    hr(),
                                    h4('Detecciones de VPH de primera vez (35 a 64 años)'),
                                    fluidRow(
                                      column(6, numericInput('vph_25_44', 'Femenino de 25-44 años', 0)),
                                      column(6, numericInput('vph_45_64', 'Femenino de 45-64 años', 0))
                                    ),
                                    hr(),
                                    h4('Exploraciones clínicas de mama (25 a 39 años)'),
                                    fluidRow(
                                      column(6, numericInput('exploraciones', 'Realizadas', 0))
                                    ),
                                    hr(),
                                    h4('Pruebas de glucemia capilar realizadas'),
                                    fluidRow(
                                      column(6,
                                             numericInput('gluc_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('gluc_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('gluc_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('gluc_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('gluc_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('gluc_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('gluc_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('gluc_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('gluc_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('gluc_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('gluc_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('gluc_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('gluc_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('gluc_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Tamizajes de presión arterial'),
                                    fluidRow(
                                      column(6,
                                             numericInput('tamiz_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('tamiz_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('tamiz_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('tamiz_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('tamiz_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('tamiz_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('tamiz_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('tamiz_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('tamiz_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('tamiz_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('tamiz_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('tamiz_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('tamiz_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('tamiz_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Detecciones de casos sujetos a vigilancia epidemiológica'),
                                    fluidRow(
                                      column(6,
                                             numericInput('epi_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('epi_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('epi_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('epi_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('epi_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('epi_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('epi_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('epi_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('epi_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('epi_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('epi_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('epi_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('epi_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('epi_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Detecciones de casos sospechosos a cólera y monitoreo de enteropatógenos'),
                                    fluidRow(
                                      column(6,
                                             numericInput('colera_fem_m1', 'Femenino < 1 año', 0),
                                             numericInput('colera_fem_1_4', 'Femenino de 1-4 años', 0),
                                             numericInput('colera_fem_5_14', 'Femenino de 5-14 años', 0),
                                             numericInput('colera_fem_15_24', 'Femenino de 15-24 años', 0),
                                             numericInput('colera_fem_25_44', 'Femenino de 25-44 años', 0),
                                             numericInput('colera_fem_45_64', 'Femenino de 45-64 años', 0),
                                             numericInput('colera_fem_65mas', 'Femenino de 65 y más', 0)
                                      ),
                                      column(6,
                                             numericInput('colera_mas_m1', 'Masculino < 1 año', 0),
                                             numericInput('colera_mas_1_4', 'Masculino de 1-4 años', 0),
                                             numericInput('colera_mas_5_14', 'Masculino de 5-14 años', 0),
                                             numericInput('colera_mas_15_24', 'Masculino de 15-24 años', 0),
                                             numericInput('colera_mas_25_44', 'Masculino de 25-44 años', 0),
                                             numericInput('colera_mas_45_64', 'Masculino de 45-64 años', 0),
                                             numericInput('colera_mas_65mas', 'Masculino de 65 y más', 0)
                                      )
                                    ),
                                    hr(),
                                    h4('Número de migrantes atendidos'),
                                    fluidRow(
                                      column(6, numericInput('migrantes', '', 0))
                                    ),
                                    hr(),
                                    h4('Número de personas atendidas que se identifican como indígenas'),
                                    fluidRow(
                                      column(6, numericInput('indigenas', '', 0))
                                    ),
                                    tags$hr(style="border-color: black;"),
                                    h3(tags$b('Acciones en entornos')),
                                    h4('Activos visitados'),
                                    fluidRow(
                                      column(2, numericInput('negocios_visi', 'Negocios', 0)),
                                      column(2, numericInput('escuelas_visi', 'Escuelas', 0)),
                                      column(2, numericInput('iglesias_visi', 'Iglesias', 0)),
                                      column(2, numericInput('lid_vecinal_visi', 'Líderes Vecinales', 0)),
                                    ),
                                    hr(),
                                    h4('Casas visitadas'),
                                    fluidRow(
                                      column(2, numericInput('cas_promo', 'Promocionadas', 0)),
                                      column(2, numericInput('cas_ausentes', 'Ausentes', 0)),
                                      column(2, numericInput('cas_renuen', 'Renuentes', 0)),
                                      column(2, numericInput('cas_deshab', 'Deshabitadas', 0))
                                    ),
                                    hr(),
                                    h4('Entornos con brechas de salud colectiva visitados'),
                                    fluidRow(
                                      column(2, numericInput('c_rehab', 'Centros de rehabilitación', 0)),
                                      column(2, numericInput('ceresos', 'CERESOS', 0)),
                                      column(2, numericInput('campo_agri', 'Campo agrícolas', 0)),
                                      column(2, numericInput('albergue_migra', 'Albergues de migrantes', 0)),
                                      column(2, numericInput('albergue_otro', 'Otro tipo de albergues', 0))
                                    ),
                                    hr(),
                                    h4('Total de Consultorios Anexos a Farmacia (CAF) visitados'),
                                    fluidRow(
                                      column(6, numericInput('caf_visita', '', 0))
                                    ),
                                    hr(),
                                    h4('Material infográfico entregado (dípticos, trípticos y carteles)'),
                                    fluidRow(
                                      column(6, numericInput('mat_entrega', '', 0))
                                    ),
                                    hr(),
                                    h4('Vida Suero Oral (VSO) entregados'),
                                    fluidRow(
                                      column(6, numericInput('vso_entrega', '', 0))
                                    ),
                                    hr(),
                                    h4('Preservativos entregados'),
                                    fluidRow(
                                      column(6, numericInput('p_ext_entrega', 'Preservativos externos/masculinos entregados', 0)),
                                      column(6, numericInput('p_int_entrega', 'Preservativos internos/femeninos entregados', 0))
                                    ),
                                    hr(),
                                    h4('Gel lubricante a base de agua entregados'),
                                    fluidRow(
                                      column(6, numericInput('lubri_entrega', '', 0))
                                    ),
                                    hr(),
                                    h4('Mecanismo de educación para la salud:'),
                                   fluidRow(
                                     column(4,
                                            selectInput(inputId = 'mecanismo', 
                                                        label = 'Mecanismo de educación para la salud utilizado:',
                                                        choices = respuestas %>% select(MECANISMO) %>% filter(!is.na(MECANISMO)), 
                                                        selected = '')
                                            ),
                                     column(4,
                                            selectizeInput(inputId = 'tematica', 
                                                           label = 'Temática abordada (pueden ser varias):',
                                                           choices = respuestas$TEMATICA, 
                                                           selected = '',
                                                           multiple = T)
                                            ),
                                     column(4,
                                            numericInput('p_sensibilizadas', 'Número de personas sensibilizadas', 0))
                                   ),
                                   tags$hr(style="border-color: black;"),
                                   h3(tags$b('Acciones de una sola salud')),
                                   h4('Esterilizaciones en animales de compañía'),
                                   fluidRow(
                                     column(6,
                                            numericInput('ester_cani_m', 'Canino Macho', 0),
                                            numericInput('ester_cani_h', 'Canino Hembra', 0)
                                            ),
                                     column(6,
                                            numericInput('ester_feli_m', 'Felino Macho', 0),
                                            numericInput('ester_feli_h', 'Felino Hembra', 0)
                                            )
                                   ),
                                   hr(),
                                   h4('Vacunación antirrábica'),
                                   fluidRow(
                                     column(6,
                                            numericInput('vac_cani_m', 'Canino Macho', 0),
                                            numericInput('vac_cani_h', 'Canino Hembra', 0)
                                     ),
                                     column(6,
                                            numericInput('vac_feli_m', 'Felino Macho', 0),
                                            numericInput('vac_feli_h', 'Felino Hembra', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Desparasitaciones'),
                                   fluidRow(
                                     column(6,
                                            numericInput('desp_cani_m', 'Canino Macho', 0),
                                            numericInput('desp_cani_h', 'Canino Hembra', 0)
                                     ),
                                     column(6,
                                            numericInput('desp_feli_m', 'Felino Macho', 0),
                                            numericInput('desp_feli_h', 'Felino Hembra', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Casas intervenidas con control larvario'),
                                   fluidRow(
                                     column(6,
                                            numericInput('cont_larvario', 'Cantidad', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Recipientes revisados (eliminados, controlados y tratados)'),
                                   fluidRow(
                                     column(6,
                                            numericInput('recipientes_rev', 'Cantidad', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Cantidad de insecticida (temefos o abate) utilizado'),
                                   fluidRow(
                                     column(6,
                                            numericInput('cant_insect', 'Cantidad en gramos', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Cantidad de personas beneficiadas con Estrategia Super Sani'),
                                   fluidRow(
                                     column(6,
                                            numericInput('p_ssani', 'Cantidad', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Cantidad de personas beneficiadas con Estrategia Ecobuquis'),
                                   fluidRow(
                                     column(6,
                                            numericInput('p_ecobuq', 'Cantidad', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Casa limpia'),
                                   fluidRow(
                                     column(6,
                                            numericInput('casas_sanea', 'Casas comprometidas a saneamiento', 0)
                                     )
                                   ),
                                   tags$hr(style="border-color: black;"),
                                   h3(tags$b('Vacunación universal')),
                                   h4('VPH'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_vph_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_vph_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_vph_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_vph_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_vph_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_vph_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_vph_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_vph_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_vph_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_vph_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_vph_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_vph_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_vph_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_vph_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_vph_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_vph_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_vph_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_vph_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_vph_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_vph_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_vph_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('COVID-19'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_cov_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_cov_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_cov_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_cov_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_cov_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_cov_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_cov_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_cov_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_cov_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_cov_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_cov_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_cov_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_cov_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_cov_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_cov_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_cov_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_cov_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_cov_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_cov_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_cov_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_cov_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Hepatitis B'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_hepab_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_hepab_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_hepab_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_hepab_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_hepab_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_hepab_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_hepab_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_hepab_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_hepab_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_hepab_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_hepab_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_hepab_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_hepab_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_hepab_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_hepab_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_hepab_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_hepab_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_hepab_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_hepab_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_hepab_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_hepab_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Influenza'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_flu_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_flu_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_flu_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_flu_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_flu_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_flu_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_flu_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_flu_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_flu_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_flu_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_flu_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_flu_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_flu_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_flu_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_flu_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_flu_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_flu_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_flu_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_flu_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_flu_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_flu_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Hexavalente acelular'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_hexa_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_hexa_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_hexa_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_hexa_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_hexa_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_hexa_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_hexa_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_hexa_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_hexa_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_hexa_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_hexa_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_hexa_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_hexa_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_hexa_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_hexa_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_hexa_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_hexa_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_hexa_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_hexa_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_hexa_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_hexa_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Rotavirus'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_rota_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_rota_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_rota_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_rota_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_rota_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_rota_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_rota_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_rota_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_rota_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_rota_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_rota_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_rota_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_rota_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_rota_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_rota_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_rota_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_rota_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_rota_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_rota_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_rota_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_rota_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Neumocócica Conjugada 23'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_neu23_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_neu23_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_neu23_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_neu23_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_neu23_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_neu23_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_neu23_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_neu23_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_neu23_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_neu23_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_neu23_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_neu23_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_neu23_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_neu23_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_neu23_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_neu23_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_neu23_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_neu23_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_neu23_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_neu23_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_neu23_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('SR'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_sr_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_sr_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_sr_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_sr_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_sr_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_sr_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_sr_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_sr_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_sr_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_sr_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_sr_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_sr_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_sr_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_sr_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_sr_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_sr_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_sr_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_sr_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_sr_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_sr_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_sr_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('SRP'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_srp_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_srp_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_srp_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_srp_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_srp_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_srp_fem_45_64', 'Femenino de 45-64 años', 0)
                                            
                                     ),
                                     column(4,
                                            numericInput('vac_srp_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_srp_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_srp_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_srp_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_srp_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_srp_mas_45_64', 'Masculino de 45-64 años', 0)
                                            
                                     ),
                                     column(4,
                                            numericInput('vactot_srp_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_srp_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_srp_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_srp_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_srp_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_srp_mas_45_64', 'Total de 45-64 años', 0)
                                            
                                     )
                                   ),
                                   hr(),
                                   h4('DPT'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_dpt_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_dpt_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_dpt_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_dpt_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_dpt_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_dpt_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_dpt_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_dpt_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_dpt_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_dpt_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_dpt_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_dpt_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_dpt_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_dpt_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_dpt_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_dpt_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_dpt_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_dpt_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_dpt_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_dpt_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_dpt_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('BCG'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_bcg_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_bcg_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_bcg_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_bcg_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_bcg_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_bcg_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_bcg_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_bcg_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_bcg_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_bcg_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_bcg_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_bcg_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_bcg_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_bcg_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_bcg_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_bcg_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_bcg_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_bcg_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_bcg_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_bcg_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_bcg_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('TDPA'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_tdpa_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_tdpa_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_tdpa_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_tdpa_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_tdpa_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_tdpa_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_tdpa_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_tdpa_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_tdpa_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_tdpa_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_tdpa_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_tdpa_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_tdpa_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_tdpa_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_tdpa_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_tdpa_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_tdpa_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_tdpa_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_tdpa_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_tdpa_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_tdpa_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('TD'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_td_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_td_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_td_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_td_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_td_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_td_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_td_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_td_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_td_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_td_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_td_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_td_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_td_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_td_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_td_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_td_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_td_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_td_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_td_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_td_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_td_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                   hr(),
                                   h4('Neumocócica 13'),
                                   fluidRow(
                                     column(4,
                                            numericInput('vac_neu13_fem_m1', 'Femenino < 1 año', 0),
                                            numericInput('vac_neu13_fem_1_4', 'Femenino de 1-4 años', 0),
                                            numericInput('vac_neu13_fem_5_14', 'Femenino de 5-14 años', 0),
                                            numericInput('vac_neu13_fem_15_24', 'Femenino de 15-24 años', 0),
                                            numericInput('vac_neu13_fem_25_44', 'Femenino de 25-44 años', 0),
                                            numericInput('vac_neu13_fem_45_64', 'Femenino de 45-64 años', 0),
                                            numericInput('vac_neu13_fem_65mas', 'Femenino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vac_neu13_mas_m1', 'Masculino < 1 año', 0),
                                            numericInput('vac_neu13_mas_1_4', 'Masculino de 1-4 años', 0),
                                            numericInput('vac_neu13_mas_5_14', 'Masculino de 5-14 años', 0),
                                            numericInput('vac_neu13_mas_15_24', 'Masculino de 15-24 años', 0),
                                            numericInput('vac_neu13_mas_25_44', 'Masculino de 25-44 años', 0),
                                            numericInput('vac_neu13_mas_45_64', 'Masculino de 45-64 años', 0),
                                            numericInput('vac_neu13_mas_65mas', 'Masculino de 65 y más', 0)
                                     ),
                                     column(4,
                                            numericInput('vactot_neu13_mas_m1', 'Total < 1 año', 0),
                                            numericInput('vactot_neu13_mas_1_4', 'Total de 1-4 años', 0),
                                            numericInput('vactot_neu13_mas_5_14', 'Total de 5-14 años', 0),
                                            numericInput('vactot_neu13_mas_15_24', 'Total de 15-24 años', 0),
                                            numericInput('vactot_neu13_mas_25_44', 'Total de 25-44 años', 0),
                                            numericInput('vactot_neu13_mas_45_64', 'Total de 45-64 años', 0),
                                            numericInput('vactot_neu13_mas_65mas', 'Total de 65 y más', 0)
                                     )
                                   ),
                                    actionButton("submit", "Enviar", class = "btn-primary")
                                  ),
                                  shinyjs::hidden(
                                    div(
                                      id = "thankyou_msg",
                                      h3("¡Gracias, tu respuesta ha sido enviada satisfactoriamente!"),
                                      actionLink("submit_another", "Enviar otra respuesta")
                                    )
                                  ),
                                  shinyjs::hidden(
                                    span(id = "submit_msg", "Enviando..."),
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
  
  
   #action to take when submit button is pressed
   #observeEvent(input$submit, {
   #  saveData(formData())
   #})
  
  
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
  
  observeEvent(input$DISTRITO, {
    distritos <- catalogo
    
    updateSelectInput(session = session,
                      inputId = 'MUNICIPIO',
                      label = 'Seleccione municipio',
                      choices = unique(distritos$NOM_MUN[distritos$DISTRITO == input$DISTRITO]),
                      selected = '')
    
  })
  
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
