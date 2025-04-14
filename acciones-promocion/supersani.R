library(leaflet)
library(leaflet.extras)
library(lubridate)
library(sf)
library(plotly)
library(raster)
#library(rgeos)
library(stringi)
library(stringr)
library(ggmap)
library(tidyverse)
library(lubridate)
library(readxl)
library(htmlwidgets)

ssani1 <- readxl::read_xlsx('acciones-promocion/datos/Supersani/supersani24.xlsx',
                           sheet = 'DS01 Super Sani 1er y 2do tri', range = 'B7:AG71') %>%
                        filter(!is.na(Escuela)) 
ssani2 <- readxl::read_xlsx('acciones-promocion/datos/Supersani/supersani24.xlsx',
                            sheet = 'DS02 Super Sani 1er y 2do tri', range = 'B7:AG19') %>% filter(!is.na(Escuela)) 
ssani3 <- readxl::read_xlsx('acciones-promocion/datos/Supersani/supersani24.xlsx',
                            sheet = 'DS03 Super Sani 1er y 2do tri', range = 'B7:AG21') %>% filter(!is.na(Escuela)) 
ssani4 <- readxl::read_xlsx('acciones-promocion/datos/Supersani/supersani24.xlsx',
                            sheet = 'DS04 Super Sani 1er y 2do tri', range = 'B7:AG38') %>% filter(!is.na(Escuela))
ssani5 <- readxl::read_xlsx('acciones-promocion/datos/Supersani/supersani24.xlsx',
                            sheet = 'DS05 Super Sani 1er y 2do tri', range = 'B8:AG27') %>% filter(!is.na(Escuela))
ssani6 <- readxl::read_xlsx('acciones-promocion/datos/Supersani/supersani24.xlsx',
                            sheet = 'DS06 Super Sani 1er y 2do tri', range = 'B7:AG30') %>% filter(!is.na(Escuela))


####Base de FMRR con georeferencias---- 

fmrr_ss <- read_rds('epidemiologia/datos/FMRR/fmrr_071024.rds') %>% 
  mutate(lat = case_when(
    IDE_ID == '1531690' ~ 29.178431375403207,
    IDE_ID == '1430787' ~ 29.202548421140566,
    IDE_ID == '1516703' ~ 30.732108811465793,
    IDE_ID == '1305818' ~ 27.148860351653255,
    IDE_ID == '1521027' ~ 29.09065150044901,
    IDE_ID == '1459960' ~ 29.110434717764257,
    T ~ lat),
    long = case_when(
    IDE_ID == '1531690' ~ -111.01278117567244,
    IDE_ID == '1430787' ~ -110.77101507463306,
    IDE_ID == '1516703' ~ -112.39461448263778,
    IDE_ID == '1305818' ~ -109.42910675105355,
    IDE_ID == '1521027' ~ -110.9267648445041,
    IDE_ID == '1459960' ~ -111.01196566879983,
    T ~ long))




  
#Búsqueda y modificación de coordenadas de las escuelas visitadas durante 2024 ----
#Valdría la pena utilizar el archivo de escuelas que ya tiene latitud y longitud, a fin de ahorrar tiempo y pasos. Este código se hizo con base en la información que nos envió Promoción.
#                                DISTRITO DE SALUD 01 ----

#1er trimestre
ssani1_a <- ssani1 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 1:3) %>% 
  mutate(domicilio = paste("Escuela ", Escuela, ", ", Colonia, ", ", "Hermosillo, Sonora, México", sep = ""),
         Escuela = str_remove_all(Escuela, pattern= "#"))

ssani1_a <- ssani1_a %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')   

ssani1_a <- ssani1_a %>%
  mutate(lat = case_when(
    Escuela == 'Escuela Primaria Sor Juana Ines de la Cruz' ~ 29.059741283588426,
    T ~ lat),
    long = case_when(
      Escuela == 'Escuela Primaria Sor Juana Ines de la Cruz' ~ -110.96915900409566,
      T ~ long))

ssani1_a <- ssani1_a %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%  #se hace punto
#sf_use_s2(TRUE)
  st_buffer(ssani1_a, dist = 1000) #se hace polígono


#2do trimestre
ssani1_b <- ssani1 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 4:6) %>% 
  mutate(domicilio = paste("Escuela ", Escuela, ", ", Colonia, ", ", "Hermosillo, Sonora, México", sep = ""))

ssani1_b <- ssani1_b %>% distinct(Escuela, .keep_all = T) %>%  
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 

ssani1_b <- ssani1_b %>% 
  mutate(lat = case_when(
    Escuela == 'Primaria Enrique C Rebsamen' ~ 29.03860053803895,
    T ~ lat),
    long = case_when(
      Escuela == 'Primaria Enrique C Rebsamen' ~ -110.98098448875642,
      T ~ long))

ssani1_b <- ssani1_b %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%  #punto
  st_buffer(ssani1_b, dist = 1000) #polígono



#3er trimestre
ssani1_c <- ssani1 %>% filter(month(`Fecha De  Visita`) %in% 7:9) %>% 
  mutate(domicilio = paste("Escuela ", Escuela, ", ", Colonia, ", ", "Hermosillo, Sonora, México", sep = ""))

ssani1_c <- ssani1_c %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')


ssani1_c <- ssani1_c %>% 
  mutate(lat = case_when(
    Escuela == 'Primaria Benemerito de las Americas' ~ 29.38023667425357,
    Escuela == 'Escuela Secundaria General No.16' ~ 29.178086229078566, 
    Escuela == 'Escuela Secundaria Técnica No.57' ~ 29.11716471992934, 
    T ~ lat),
    long = case_when(
      Escuela == 'Primaria Benemerito de las Americas' ~ -110.89455412701433,
      Escuela == 'Escuela Secundaria General No.16' ~ -111.00337887030265,
      Escuela == 'Escuela Secundaria Técnica No.57' ~ -111.00825616446004,
      T ~ long))

ssani1_c <- ssani1_c %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%  #punto
  st_buffer(ssani1_c, dist = 1000) #polígono
  


###.      Casos confirmados durante 2024 (DS 01)

fmrr_ss1 <- fmrr_ss %>% filter(CVE_JUR_RES == 1, ESTATUS_CASO == 2) %>% 
           #mutate(domicilio = paste(IDE_CAL, " ", NUM_EXT, ", COL. ", IDE_COL, ", ", DES_MPO_RES, ", SONORA, MEXICO", sep = ""))
            st_as_sf(coords = c('long', 'lat'), crs = 4326)

#Coordenadas de casos confirmados convertidas en puntos
#fmrr_ss1 <- fmrr_ss1 %>%
#  mutate(geometry = str_c('POINT (', long, ' ', lat, ')', sep = '')) %>%
#  filter(!is.na(lat)) %>%
#  mutate(geo = NA)
#pts <- st_as_sfc(fmrr_ss1$geometry) %>%
#  st_sf(ID = fmrr_ss1$IDE_ID)
#pts <- pts %>% st_set_crs(4326)


###.       Casos probables DS01
fmrr_ss1b <- fmrr_ss %>% filter(CVE_JUR_RES == 1, ESTATUS_CASO == 1) %>% 
  mutate(lat = case_when(
    IDE_ID == 1673997 ~ 29.10660981645385,
    T ~ lat),
    long = case_when(
      IDE_ID == 1673997 ~ -111.04862254642418,
      T ~ long)) %>% 
              st_as_sf(coords = c("long", "lat"))
  


######################   DISTRITO DE SALUD 02 ----
#1er trimestre
ssani2_a <- ssani2 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 1:3) %>% 
  mutate(Escuela = str_remove_all(Escuela, pattern = "#")) %>% 
                                mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Caborca, Sonora, México", sep = ""))
                              
ssani2_a <- ssani2_a %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')  

ssani2_a <- ssani2_a %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani2_a, dist = 1000)

#2do trimestre
ssani2_b <- ssani2 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 4:6) %>% 
  mutate(Escuela = str_remove_all(Escuela, pattern = "#")) %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Caborca, Sonora, México", sep = ""))

ssani2_b <- ssani2_b %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')  


ssani2_b <- ssani2_b %>% 
  mutate(lat = case_when(
    Escuela == 'Escuela primaria Justo Sierra Mendez' ~ 30.720398072368734,
    T ~ lat),
    long = case_when(
      Escuela == 'Escuela primaria Justo Sierra Mendez' ~ -112.16782146286737,
      T ~ long))

ssani2_b <- ssani2_b %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani2_b, dist = 1000) 

#3er trimestre
ssani2_c <- ssani2 %>% filter(month(`Fecha De  Visita`) %in% 7:9) %>% 
  mutate(Escuela = str_remove_all(Escuela, pattern = "#")) %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Caborca, Sonora, México", sep = ""))

ssani2_c <- ssani2_c %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 


ssani2_c <- ssani2_c %>% 
  mutate(lat = case_when(
    Escuela == 'ESCUELA PRIMARIA ESCUADRON 201' ~ 30.72767089442805,
    T ~ lat),
    long = case_when(
      Escuela == 'ESCUELA PRIMARIA ESCUADRON 201' ~ -112.01660169755536,
      T ~ long))

ssani2_c <- ssani2_c %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani2_c, dist = 1000)


#Casos confirmados durante 2024 (DS 02)#NO HAY CASOS CONFIRMADOS AL 22 DE AGOSTO DE 24

fmrr_ss2 <- fmrr_ss %>% filter(CVE_JUR_RES == 2, ESTATUS_CASO == 2) %>% 
  #mutate(domicilio = paste(IDE_CAL, " ", NUM_EXT, ", COL. ", IDE_COL, ", ", DES_MPO_RES, ", SONORA, MEXICO", sep = ""))
            st_as_sf(coords = c('long', 'lat'), crs = 4326)


#casos probables DS02
fmrr_ss2b <- fmrr_ss %>% filter(CVE_JUR_RES == 2, ESTATUS_CASO == 1) %>% 
             st_as_sf(coords = c("long", "lat"))



################      DISTRITO DE SALUD 03 ----
#1er trimestre
ssani3_a <- ssani3 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 1:3)  %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) 
  
ssani3_a <- ssani3_a %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 

ssani3_a <- ssani3_a %>%  
  mutate(lat = case_when(
    Escuela == 'Primaria Lic. Horacio Sobarzo' ~ 30.624468282938317,
    Escuela == 'Primaria Margarita Arvayo' ~ 30.66197465174379,
    T ~ lat
  ),
  long = case_when(
    Escuela == 'Primaria Lic. Horacio Sobarzo' ~ -110.9712856138487,
    Escuela == 'Primaria Margarita Arvayo' ~ -110.93549970219736,
    T ~ long
  ))

ssani3_a <- ssani3_a %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani3_a, dist = 1000)


#2do trimestre
ssani3_b <- ssani3 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 4:6)  %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) 

ssani3_b <- ssani3_b %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')  

ssani3_b <- ssani3_b %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani3_b, dist = 1000) 


#3er trimestre
ssani3_c <- ssani3 %>%  filter(month(`Fecha De  Visita`) %in% 7:9)  %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = ""))
 

ssani3_c <- ssani3_c %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 

ssani3_c <- ssani3_c %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani3_c, dist = 1000)


#####.      Casos confirmados durante 2024 (DS 03)

fmrr_ss3 <- fmrr_ss %>% filter(CVE_JUR_RES == 3, ESTATUS_CASO == 2) %>% 
            st_as_sf(coords = c('long', 'lat'), crs = 4326)

#.          Casos probables DS03
fmrr_ss3b <- fmrr_ss %>% filter(CVE_JUR_RES == 3, ESTATUS_CASO == 1) %>%
             st_as_sf(coords = c("long", "lat"))


######################DISTRITO DE SALUD 04 ---- 
#1er trimestre
ssani4_a <- ssani4 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 1:3) %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) 

ssani4_a <- ssani4_a %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')

ssani4_a <- ssani4_a %>%
  mutate(lat = case_when(
    Escuela == 'ESCUELA PRIMARIA HEROES DE NACOZARI' ~ 27.496775631927406,
    Escuela == 'ESC. PRIMARIA JAIME TORRES BODET' ~ 27.50503699761668,
    Escuela == 'ESCUELA PRIM. JORGE HUMBERTO GUZMAN ROMAN' ~ 27.494273955118125,
    domicilio == 'ESCUELA PRIMARIA JESUS RABSAMEN, Col. COL BENITO JUAREZ, Sonora, México' ~ 27.499708319032568,
    Escuela == 'ESC. TELESECUNDARIA 3' ~ 27.521857914683668,
    Escuela == 'ESCUELA PRIMARIA GRAL. PLUTARCO ELIAS CALLES' ~ 27.484833106267548,
    Escuela == 'ESC. LAZARO CARDENAS' ~ 27.45633870098364,
    Escuela == 'ESC. HERMAN BRUSS' ~ 27.49268189371229,
    T ~ lat),
    long = case_when(
      Escuela == 'ESCUELA PRIMARIA HEROES DE NACOZARI' ~ -109.92495063007841,
      Escuela == 'ESC. PRIMARIA JAIME TORRES BODET' ~ -109.93480947286743,
      Escuela == 'ESCUELA PRIM. JORGE HUMBERTO GUZMAN ROMAN' ~ -109.90925767531016,
      domicilio == 'ESCUELA PRIMARIA JESUS RABSAMEN, Col. COL BENITO JUAREZ, Sonora, México' ~ -109.91152196939397,
      Escuela == 'ESC. TELESECUNDARIA 3' ~ -109.25413469307475,
      Escuela == 'ESCUELA PRIMARIA GRAL. PLUTARCO ELIAS CALLES' ~ -109.92278808569615,
      Escuela == 'ESC. LAZARO CARDENAS' ~ -109.9457414836651,
      Escuela == 'ESC. HERMAN BRUSS' ~ -110.0002240935118,
      T ~ long
    ))
ssani4_a <- ssani4_a %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani4_a, dist = 1000)



#2do trimestre
ssani4_b <- ssani4 %>% filter(!is.na(`Acción...5`), month(`Fecha De  Visita`) %in% 4:6) %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) 

ssani4_b <- ssani4_b %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 

ssani4_b <- ssani4_b %>%  
  mutate(lat = case_when(
    Escuela == "ESC. CLUB DE GOLF # 1" ~ 27.575963186808057,
    Escuela == "ESC. DISTRIBUIDORES NISSAN 54 - 1" ~ 27.459187823853952,
    Escuela == "ESC. MICAELA MUNGUIA RIVAS" ~ 27.494369869379067,
    Escuela == "ESC. JAIME TORRES BODET" ~ 27.4710590437333,
    domicilio == "ESC. NUEVA CREACIÓN, Col. VILLAS DEL REY, Sonora, México" ~ 27.491441760095082,
    domicilio == "ESC. NUEVA CREACIÓN, Col. CASA BLANCA, Sonora, México" ~ 27.48867205544988,
    T ~ lat),
  long = case_when(
    Escuela == "ESC. CLUB DE GOLF # 1" ~ -109.92878887715254,
    Escuela == "ESC. DISTRIBUIDORES NISSAN 54 - 1" ~ -109.89944181329678,
    Escuela == "ESC. MICAELA MUNGUIA RIVAS" ~ -109.90714514336571,
    Escuela == "ESC. JAIME TORRES BODET" ~ -109.9243898324002,
    domicilio == "ESC. NUEVA CREACIÓN, Col. VILLAS DEL REY, Sonora, México" ~ -109.97664005155461,
    domicilio == "ESC. NUEVA CREACIÓN, Col. CASA BLANCA, Sonora, México" ~ -109.98826394552007,
    T ~ long
  ))

ssani4_b <- ssani4_b %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani4_b, dist = 1000) 


#3er trimestre
ssani4_c <- ssani4 %>% filter(month(`Fecha De  Visita`) %in% 7:9) %>% 
  mutate(domicilio = paste(Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) 

ssani4_c <- ssani4_c %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')

ssani4_c <- ssani4_c %>%
  mutate(lat = case_when(
    Escuela == 'ESC. ALVARO OBREGÓN' ~ 27.372996361411815,
    Escuela == 'ESC. VICENTE SUAREZ' ~ 27.476266075559106,
    T ~ lat),
    long = case_when(
      Escuela == 'ESC. ALVARO OBREGÓN' ~ -109.89263538163699,
      Escuela == 'ESC. VICENTE SUAREZ' ~ -109.9178127907065,
      T ~ long
    ))


ssani4_c <- ssani4_c %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani4_c, dist = 1000) 


#Casos confirmados durante 2024 (DS 04)

fmrr_ss4 <- fmrr_ss %>% filter(CVE_JUR_RES == 4, ESTATUS_CASO == 2) %>% 
st_as_sf(coords = c("long", "lat"))
    

#casos probables DS04
fmrr_ss4b <- fmrr_ss %>% filter(CVE_JUR_RES == 4, ESTATUS_CASO == 1) %>%
  st_as_sf(coords = c("long", "lat"))


######################DISTRITO DE SALUD 05 ----
#1er trimestre
ssani5_a <- ssani5 %>% filter(!is.na(`Acción...5`)) %>%
  mutate(Escuela = str_remove_all(Escuela, pattern= '"')) %>%  
  mutate(`Fecha De  Visita` = ymd(`Fecha De  Visita`)) %>% 
  filter(month(`Fecha De  Visita`) %in% 1:3) %>% 
  mutate(domicilio = paste("Escuela ", Escuela, ", ", Colonia, ", ", "Navojoa, Sonora, México", sep = "")) 

ssani5_a <- ssani5_a %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')

ssani5_a <- ssani5_a %>%
  mutate(lat = case_when(
    Escuela == 'HEROES DE NACOZARIT' ~ 27.08519093030574,
    Escuela == 'Escuela  Primaria Juventino R Solano' ~ 27.101874574308304,
    T ~ lat),
    long = case_when(
      Escuela == 'HEROES DE NACOZARIT' ~ -109.42698536120851,
      Escuela == 'Escuela  Primaria Juventino R Solano' ~ -109.44028420231851,
      T ~ long))

ssani5_a <- ssani5_a %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani5_a, dist = 1000) 

#2do trimestre
ssani5_b <- ssani5 %>% filter(!is.na(`Acción...5`)) %>%
  mutate(Escuela = str_remove_all(Escuela, pattern= '"')) %>%  
  mutate(`Fecha De  Visita` = ymd(`Fecha De  Visita`)) %>% 
  filter(month(`Fecha De  Visita`) %in% 4:6) %>% 
  mutate(domicilio = paste("Escuela ", Escuela, ", ", Colonia, ", ", "Navojoa, Sonora, México", sep = "")) 

ssani5_b <- ssani5_b %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 

ssani5_b <- ssani5_b %>%  
  mutate(lat = case_when(
    Escuela == 'Primaria Vicente Guerrero' ~ 27.057898874572086,
    Escuela == 'Primaria Emiliano Zapata' ~ 27.13178101770649,
    T ~ lat),
    long = case_when(
      Escuela == 'Primaria Vicente Guerrero' ~ -109.46082734181614,
      Escuela == 'Primaria Emiliano Zapata' ~ -109.44819972243135,
      T ~ long
    ))


ssani5_b <- ssani5_b %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani5_b, dist = 1000) 



#Casos confirmados durante 2024 (DS 05)

fmrr_ss5 <- fmrr_ss %>% filter(CVE_JUR_RES == 5, ESTATUS_CASO == 2) %>% 
            st_as_sf(coords = c("long", "lat"))


#casos probables DS05
fmrr_ss5b <- fmrr_ss %>% filter(CVE_JUR_RES == 5, ESTATUS_CASO == 1) %>%
             st_as_sf(coords = c("long", "lat"))




######################DISTRITO DE SALUD 06 ----
#1er trimestre
ssani6_a <- ssani6 %>% filter(!is.na(`Acción...5`)) %>%
  mutate(`Fecha De  Visita` = ymd(`Fecha De  Visita`)) %>% 
  mutate(Escuela = str_remove_all(Escuela, pattern= "#")) %>% 
  filter(month(`Fecha De  Visita`) %in% 1:3)


ssani6_a <- ssani6_a %>% distinct(Escuela, .keep_all = T) %>% 
  left_join(
  aggregate(`Acción...5` ~ Escuela, data = ssani6_a, paste, collapse = ',') %>% 
  left_join(
    aggregate(`Fecha De  Visita` ~ Escuela, data = ssani6_a, paste, collapse = ',')
  ) %>% 
  mutate(visita = str_split(string = as.character(`Fecha De  Visita`), pattern = ',', simplify = T),
         accion = str_split(string = as.character(`Acción...5`), pattern = ',', simplify = T)) %>% 
    dplyr::select(-`Acción...5`, -`Fecha De  Visita`))

  
ssani6_a <- ssani6_a %>% dplyr::select(-c(`Fecha /Reunión Comité De Padres`:`Fecha De Entrega De Resultados...32`)) %>%
  mutate(domicilio = paste("Escuela ", Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis')

ssani6_a <- ssani6_a %>%
  mutate(lat = case_when(
    Escuela == 'Primaria Eva Samano' ~ 32.46617345893653,
    Escuela == 'Preparatoria CETMAR 14 Extensión San Luis' ~ 32.4583424148028,
    Escuela == 'Jardín de niños Rafaela Sotelo' ~ 31.325400128470225,
    Escuela == 'Primaria María Mercedes Andrade' ~ 32.46371563128446,
    T ~ lat
  ), 
  long = case_when(
    Escuela == 'Primaria Eva Samano' ~ -114.78787464623154,
    Escuela == 'Preparatoria CETMAR 14 Extensión San Luis' ~ -114.8134781217442,
    Escuela == 'Jardín de niños Rafaela Sotelo' ~ -113.49831882602402, 
    Escuela == 'Primaria María Mercedes Andrade' ~ -114.77382683351608,
    T ~ long))

ssani6_a <- ssani6_a %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani6_a, dist = 1000) 



#2do trimestre

ssani6_b <- ssani6 %>% filter(!is.na(`Acción...5`)) %>%
  mutate(`Fecha De  Visita` = ymd(`Fecha De  Visita`)) %>% 
  mutate(Escuela = str_remove_all(Escuela, pattern= "#")) %>% 
  filter(month(`Fecha De  Visita`) %in% 4:6) %>% 
  mutate(Escuela = case_when(
    Escuela == 'Escuela Secundaria 75' ~ 'Escuela Secundaria Técnica 75',
    T ~ Escuela))
  
  ssani6_b <- ssani6_b %>% distinct(Escuela, .keep_all = T) %>% 
  left_join(
    aggregate(`Acción...5` ~ Escuela, data = ssani6_b, paste, collapse = ',') %>% 
      left_join(
        aggregate(`Fecha De  Visita` ~ Escuela, data = ssani6_b, paste, collapse = ',')
      ) %>% 
      mutate(visita = str_split(string = as.character(`Fecha De  Visita`), pattern = ',', simplify = T),
             accion = str_split(string = as.character(`Acción...5`), pattern = ',', simplify = T)) %>% 
      dplyr::select(-`Acción...5`, -`Fecha De  Visita`))
  
  
ssani6_b <- ssani6_b %>%
  dplyr::select(-c(`Fecha /Reunión Comité De Padres`:`Fecha De Entrega De Resultados...32`)) %>%
  mutate(domicilio = paste("Escuela ", Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = "")) %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 

ssani6_b <- ssani6_b %>%  
  mutate(lat = case_when(
    Escuela == 'Escuela Secundaria 22 Miguel Hidalgo y Costilla' ~ 32.4769776595646,
    Escuela == 'Escuela Secundaria 30' ~ 32.47269893603108,
    Escuela == 'Secundaria Luis B Sánchez 14' ~ 32.20290359945266,
    T ~ lat),
    long = case_when(
      Escuela == 'Escuela Secundaria 22 Miguel Hidalgo y Costilla' ~ -114.77417237514311,
      Escuela == 'Escuela Secundaria 30' ~ -114.79244603950397,
      Escuela == 'Secundaria Luis B Sánchez 14' ~ -114.99731060285109,
      T ~ long
    ))

ssani6_b <- ssani6_b %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani6_b, dist = 1000) 


#3er trimestre
ssani6_c <- ssani6 %>% filter(month(`Fecha De  Visita`) %in% 7:9) %>%
  mutate(domicilio = paste("Escuela ", Escuela, ", Col. ", Colonia, ", ", "Sonora, México", sep = ""))

ssani6_c <- ssani6_c %>% 
  tidygeocoder::geocode(address = domicilio, method = 'arcgis') 


ssani6_c <- ssani6_c %>%  
  mutate(lat = case_when(
    Escuela == 'Primaria Lazaro Cardenas' ~ 32.380477333728045,
    Escuela == 'Escuela Secundaria Tecnica 4' ~ 32.46787759276056,
    T ~ lat),
    long = case_when(
      Escuela == 'Primaria Lazaro Cardenas' ~ -114.87114318650497,
      Escuela ==  'Escuela Secundaria Tecnica 4' ~ -114.78638413389493,
      T ~ long
    ))

ssani6_c <- ssani6_c %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>% 
  st_buffer(ssani6_c, dist = 1000)

 
#Casos confirmados FMRR durante 2024 (DS 06)

fmrr_ss6 <- fmrr_ss %>% filter(CVE_JUR_RES == 6, ESTATUS_CASO == 2) %>%
  st_as_sf(coords = c("long", "lat"))


#casos probables FMRR DS06
fmrr_ss6b <- fmrr_ss %>% filter(CVE_JUR_RES == 6, ESTATUS_CASO == 1) %>%
  st_as_sf(coords = c("long", "lat"))



#Icono para marcador
icon.ion <- makeAwesomeIcon(icon = 'marker', markerColor = 'red', library='ion')
tick <- makeIcon(
    "https://cdn-icons-png.flaticon.com/512/8005/8005043.png",
    "https://cdn-icons-png.flaticon.com/512/8005/8005043.png",
    24,
    24
  )
tick2 <- makeIcon(
  "data/FMRR/garrapata_negra.png",
  "data/FMRR/garrapata_negra.png",
  24,
  24)

#Mapa sobre escuelas visitadas por Súper Sani vs casos de FMRR durante el 2024

#mapass <- 
leaflet() %>%
  addTiles(group = 'OSM') %>%  
  addPolygons(data = ssani1_a,
              group = 'Súper Sani: 1T',
              fill = T, 
              stroke = F, 
              fillColor = "orange", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = ~paste(ssani1_a$Escuela, "Fecha de visita: ", ssani1_a$`Fecha De  Visita`,
                            " Acciones realizadas: ", ssani1_a$Acción...5, sep = '<br>')) %>% 
  addPolygons(data = ssani1_b,
              group = 'Súper Sani: 2T',
              fill = T, 
              stroke = F, 
              fillColor = "green", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani1_b$Escuela, "Fecha de visita: ", ssani1_b$`Fecha De  Visita`,
                            " Acciones realizadas: ", ssani1_b$Acción...5, sep = '<br>')) %>% 
  addPolygons(data = ssani1_c,
              group = 'Súper Sani: 3T',
              fill = T, 
              stroke = F, 
              fillColor = "purple", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani1_c$Escuela, "Fecha de visita: ", ssani1_c$`Fecha De  Visita`, sep = '<br>')) %>%
  addMarkers(data = fmrr_ss1,
                    group = 'Casos confirmados de FMRR durante el 2024',
                    icon = tick,
                    label = paste("Col. ", fmrr_ss1$IDE_COL),
                    popup = paste("Caso confirmado", " ", "Localidad: ", fmrr_ss1$DES_LOC_RES, "Colonia: ", fmrr_ss1$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss1$FEC_INI_SIGNOS_SINT, sep = '<br>'),
                    ) %>% 
  addMarkers(data = fmrr_ss1b,
             group = 'Casos probables de FMRR durante el 2024',
             icon = tick2,
             label = paste("Col. ", fmrr_ss1b$IDE_COL),
             popup = paste("Caso probable", " ", "Localidad: ", fmrr_ss1b$DES_LOC_RES, "Colonia: ", fmrr_ss1b$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss1b$FEC_INI_SIGNOS_SINT, sep = '<br>'),
  ) %>% 
addPolygons(data = ssani2_a,
            group = 'Súper Sani: 1T',
            fill = T, 
            stroke = F, 
            fillColor = "orange", 
            fillOpacity = 0.5,
            popup = paste(ssani2_a$Escuela, 'Fecha de visita: ', ssani2_a$`Fecha De  Visita`,
                          ' Acciones realizadas: ', ssani2_a$Acción...5, sep = '<br>')) %>%
  addPolygons(data = ssani2_b,
              group = 'Súper Sani: 2T',
              fill = T, 
              stroke = F, 
              fillColor = "green", 
              fillOpacity = 0.5,
              popup = paste(ssani2_b$Escuela, 'Fecha de visita: ', ssani2_b$`Fecha De  Visita`,
                            ' Acciones realizadas: ', ssani2_b$Acción...5, sep = '<br>')) %>%
  addPolygons(data = ssani2_c,
              group = 'Súper Sani: 3T',
              fill = T, 
              stroke = F, 
              fillColor = "purple", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani2_c$Escuela, "Fecha de visita: ", ssani2_c$`Fecha De  Visita`, sep = '<br>')) %>%
  addMarkers(data = fmrr_ss2,
             group = 'Casos confirmados de FMRR durante el 2024',
             icon = tick,
             label = paste("Col. ", fmrr_ss2$IDE_COL),
             #clusterOptions = markerClusterOptions(),
             popup = paste("Caso confirmado", " ", "Localidad: ", fmrr_ss2$DES_LOC_RES, "Colonia: ", fmrr_ss2$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss2$FEC_INI_SIGNOS_SINT,
                           sep = '<br>')) %>%
  addMarkers(data = fmrr_ss2b,
             group = 'Casos probables de FMRR durante el 2024',
             icon = tick2,
             label = paste("Col. ", fmrr_ss2b$IDE_COL),
             popup = paste("Caso probable", " ", "Localidad: ", fmrr_ss2b$DES_LOC_RES, "Colonia: ", fmrr_ss2b$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss2b$FEC_INI_SIGNOS_SINT, sep = '<br>'),
  ) %>%
  addPolygons(data = ssani3_a,
              group = 'Súper Sani: 1T',
              fill = T, 
              stroke = F, 
              fillColor = "orange", 
              fillOpacity = 0.5,
              popup = paste(ssani3_a$Escuela, 'Fecha de visita: ', ssani3_a$`Fecha De  Visita`,
                            ' Acciones realizadas: ', ssani3_a$Acción...5, sep = '<br>')) %>%
  addPolygons(data = ssani3_b,
              group = 'Súper Sani: 2T',
              fill = T, 
              stroke = F, 
              fillColor = "green", 
              fillOpacity = 0.5,
              popup = paste(ssani3_b$Escuela, 'Fecha de visita: ', ssani3_b$`Fecha De  Visita`,
                            ' Acciones realizadas: ', ssani3_b$Acción...5, sep = '<br>')) %>% 
  addPolygons(data = ssani3_c,
              group = 'Súper Sani: 3T',
              fill = T, 
              stroke = F, 
              fillColor = "purple", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani3_c$Escuela, "Fecha de visita: ", ssani3_c$`Fecha De  Visita`, sep = '<br>')) %>%
  addMarkers(data = fmrr_ss3,
                    group = 'Casos confirmados de FMRR durante el 2024',
                    label = paste("Col. ", fmrr_ss3$IDE_COL),
                    popup = paste("Caso confirmado", " ", "Localidad: ", fmrr_ss3$DES_LOC_RES, "Colonia: ", fmrr_ss3$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss3$FEC_INI_SIGNOS_SINT, sep = '<br>'),
                    icon = tick) %>%
  addMarkers(data = fmrr_ss3b,
             group = 'Casos probables de FMRR durante el 2024',
             icon = tick2,
             label = paste("Col. ", fmrr_ss3b$IDE_COL),
             popup = paste("Caso probable", " ", "Localidad: ", fmrr_ss3b$DES_LOC_RES, "Colonia: ", fmrr_ss3b$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss3b$FEC_INI_SIGNOS_SINT, sep = '<br>'),
  ) %>%
  addPolygons(data = ssani4_a,
              group = 'Súper Sani: 1T',
              fill = T, 
              stroke = F, 
              fillColor = "orange", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani4_a$Escuela, "Fecha de visita: ", ssani4_a$`Fecha De  Visita`,
                            " Acciones realizadas: ", ssani4_a$Acción...5, sep = '<br>')) %>% 
  addPolygons(data = ssani4_b,
              group = 'Súper Sani: 2T',
              fill = T, 
              stroke = F, 
              fillColor = "green", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani4_b$Escuela, "Fecha de visita: ", ssani4_b$`Fecha De  Visita`,
                            " Acciones realizadas: ", ssani4_b$Acción...5, sep = '<br>')) %>% 
  addPolygons(data = ssani4_c,
              group = 'Súper Sani: 3T',
              fill = T, 
              stroke = F, 
              fillColor = "purple", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani4_c$Escuela, "Fecha de visita: ", ssani4_c$`Fecha De  Visita`, sep = '<br>')) %>%
  addMarkers(data = fmrr_ss4,
                    group = 'Casos confirmados de FMRR durante el 2024',
                    label = paste("Col. ", fmrr_ss4$IDE_COL),
                    popup = paste("Caso confirmado", " ", "Localidad: ", fmrr_ss4$DES_LOC_RES, "Colonia: ", fmrr_ss4$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss4$FEC_INI_SIGNOS_SINT, sep = '<br>'),
                    icon = tick)  %>%
  addMarkers(data = fmrr_ss4b,
             group = 'Casos probables de FMRR durante el 2024',
             icon = tick2,
             label = paste("Col. ", fmrr_ss4b$IDE_COL),
             popup = paste("Caso probable", " ", "Localidad: ", fmrr_ss4b$DES_LOC_RES, "Colonia: ", fmrr_ss4b$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss4b$FEC_INI_SIGNOS_SINT, sep = '<br>'),
  ) %>%
  addPolygons(data = ssani5_a,
              group = 'Súper Sani: 1T',
              fill = T, 
              stroke = F, 
              fillColor = "orange", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani5_a$Escuela, "Fecha de visita: ", ssani5_a$`Fecha De  Visita`,
                            " Acciones realizadas: ", ssani5_a$Acción...5, sep = '<br>')) %>%
  addPolygons(data = ssani5_b,
              group = 'Súper Sani: 2T',
              fill = T, 
              stroke = F, 
              fillColor = "green", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani5_b$Escuela, "Fecha de visita: ", ssani5_b$`Fecha De  Visita`,
                            " Acciones realizadas: ", ssani5_b$Acción...5, sep = '<br>')) %>% 
  addMarkers(data = fmrr_ss5,
                    group = 'Casos confirmados de FMRR durante el 2024',
                    label = paste("Col. ", fmrr_ss5$IDE_COL),
                    popup = paste("Caso confirmado", " ", "Localidad: ", fmrr_ss5$DES_LOC_RES, "Colonia: ", fmrr_ss5$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss5$FEC_INI_SIGNOS_SINT, sep = '<br>'),
                    icon = tick) %>%
  addMarkers(data = fmrr_ss5b,
             group = 'Casos probables de FMRR durante el 2024',
             icon = tick2,
             label = paste("Col. ", fmrr_ss5b$IDE_COL),
             popup = paste("Caso probable", " ", "Localidad: ", fmrr_ss5b$DES_LOC_RES, "Colonia: ", fmrr_ss5b$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss5b$FEC_INI_SIGNOS_SINT, sep = '<br>'),
  ) %>%
  addPolygons(data = ssani6_a,
              group = 'Súper Sani: 1T',
              fill = T, 
              stroke = F, 
              fillColor = "orange", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = ~paste(Escuela, "Fecha de visita: ", visita[,1],
                            " Acciones realizadas: ", accion[,1],"Fecha de visita: ", visita[,2],
                            " Acciones realizadas: ", accion[,2],"Fecha de visita: ", visita[,3],
                            " Acciones realizadas: ", accion[,3], sep = '<br>')) %>%
   
  addPolygons(data = ssani6_b,
              group = 'Súper Sani: 2T',
              fill = T, 
              stroke = F, 
              fillColor = "green", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = ~paste(Escuela, "Fecha de visita: ", visita[,1],
                             " Acciones realizadas: ", accion[,1],"Fecha de visita: ", visita[,2],
                             " Acciones realizadas: ", accion[,2], sep = '<br>'))  %>% 
  addPolygons(data = ssani6_c,
              group = 'Súper Sani: 3T',
              fill = T, 
              stroke = F, 
              fillColor = "purple", 
              fillOpacity = 0.5,
              label = ~Escuela,
              popup = paste(ssani6_c$Escuela, "Fecha de visita: ", ssani6_c$`Fecha De  Visita`, sep = '<br>')) %>%
  addMarkers(data = fmrr_ss6,
                    group = 'Casos confirmados de FMRR durante el 2024',
                    label = paste("Col. ", fmrr_ss6$IDE_COL),
                    popup = paste("Caso confirmado", " ", "Localidad: ", fmrr_ss6$DES_LOC_RES, "Colonia: ", fmrr_ss6$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss6$FEC_INI_SIGNOS_SINT, sep = '<br>'),
                    icon = tick) %>% 
  addMarkers(data = fmrr_ss6b,
             group = 'Casos probables de FMRR durante el 2024',
             icon = tick2,
             label = paste("Col. ", fmrr_ss6b$IDE_COL),
             popup = paste("Caso probable", " ", "Localidad: ", fmrr_ss6b$DES_LOC_RES, "Colonia: ", fmrr_ss6b$IDE_COL, "Fecha de inicio de síntomas: ", fmrr_ss6b$FEC_INI_SIGNOS_SINT, sep = '<br>'),
  ) %>%
addLayersControl(baseGroups = c('OMS'),
                 overlayGroups = c('Súper Sani: 1T',
                                   'Súper Sani: 2T',
                                   'Súper Sani: 3T',
                                   'Casos confirmados de FMRR durante el 2024',
                                   'Casos probables de FMRR durante el 2024'),
                 options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c('Casos confirmados de FMRR durante el 2024', 'Casos probables de FMRR durante el 2024')) %>% 
  addLegend("bottomleft", 
            group = 'Escuelas visitadas durante 2024',
            colors = c("orange", "green", "purple"),
            labels = c('Escuelas visitadas durante el 1er trimestre',
                       'Escuelas visitadas durante el 2do trimestre',
                       'Escuelas visitadas durante el 3er trimestre'),
            title = "Escuelas visitadas por el programa Súper Sani, durante el 2024") 
 

  
mapass %>% saveWidget('datos/Supersani/SSANI.html')







