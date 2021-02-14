library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
library(lubridate)
library(formattable)
library(DT)
library(gt)
library(plotly)
library(data.table)
library(reshape2)
library(scales)


log <- data.frame("Inicio...",Sys.time())
names(log) <- c("Step", "Time")
list_files <- list.files(paste(getwd(),"/datos_abiertos", sep = ""))

log_time <- function(step_name){
  r <- data.frame(step_name, Sys.time())
  names(r) <- c("Step", "Time")
  t <- log
  log <<- rbind(t, r)
  print(paste(Sys.time(), " ", step_name))
}

'%!in%' <- function(x,y)!('%in%'(x,y))


log_time("Cargando librerias...")

## Baja archivo
log_time("Descargando archivo...")
load("./data/datos_v2.Rdata")
load("./data/catalogo.Rdata")
dark_red <- "#981A25"
light_red <- "#C53743"
dark_green <- "#005900"
light_green <- "#4CA64C"
dark_blue <- "#1A3469"
light_blue <- "#3B5DA0"
dark_yellow <- "#C78C40"
light_yellow <- "#F9AF51"
light_ocean <- "#DFE9EB50"


#i <- 190
#while( file_list[i,]$file_name != "datos_abiertos_covid19_07.10.2020.zip"){
#while( i < 295){
  

  #ayer_file <- file_list[i-1,]$file_name
  #hoy_file <- file_list[i,]$file_name
    
  #temp2 <- paste(getwd(),"/datos_abiertos/",ayer_file, sep = "")
  
  #file_name2 <- unzip(temp2, list = TRUE)$Name[1]
  
  
  #temp <- paste(getwd(),"/datos_abiertos/", hoy_file, sep = "")
  
  #file_name <- unzip(temp, list = TRUE)$Name[1]
  
  #print(paste("El nombre del archivo de datos es", file_name))
  
  create_data_frame <- function(){
    datos <- por_sintomas
    
    colnames(datos) <- append("FECHA",colnames(datos)[-1])
    #datos <- datos %>% filter(FECHA >= today() - days(28))
    
    historico <- datos
    
    historico <- historico %>% group_by(ENTIDAD_RES) %>% summarise(positivos = sum(hombres[RESULTADO %in% c(1,2,3)] + mujeres[RESULTADO %in% c(1,2,3)] + sexo_no_especificado[RESULTADO %in% c(1,2,3)]),
                                                                   negativos = sum(hombres[RESULTADO %in% c(7)] + mujeres[RESULTADO %in% c(7)] + sexo_no_especificado[RESULTADO %in% c(7)]),
                                                                   pendientes = sum(hombres[RESULTADO %in% c(6)] + mujeres[RESULTADO %in% c(6)] + sexo_no_especificado[RESULTADO %in% c(6)]),
                                                                   pct_positividad = positivos / (positivos + negativos + pendientes))
    
    index_entidades <- catalogo[["Catálogo de ENTIDADES"]]
    
    temp_index_entidades <- index_entidades
    temp_index_entidades$ENTIDAD_RES <- as.integer(temp_index_entidades$CLAVE_ENTIDAD)
    historico <- left_join(historico, temp_index_entidades, by = "ENTIDAD_RES")
    
    
    
    datos <- por_sintomas
    colnames(datos) <- append("FECHA",colnames(datos)[-1])
    #datos <- datos %>% filter(FECHA >= today() - days(28))
    data <- datos %>% filter(RESULTADO %in% c(1,2,3))
    resultado <- NULL
    for (entidad in unique(data$ENTIDAD_RES)) {
      z <- NULL
      ent_data <- subset(data, ENTIDAD_RES == entidad)
      ent_data <- ent_data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
      ent_data <- mutate(ent_data, total = cumsum(casos))
      ent_data <- mutate(ent_data, moving_avg = frollmean(x = casos, 7))
      ent_data <- mutate(ent_data, ratio =moving_avg/lag(moving_avg, n = 1))
      ent_data <- mutate(ent_data, ratio = frollmean(x = ratio, 7))
      
      comp_days <- tibble()
      i <- 14
      while (nrow(comp_days) == 0) {
        comp_days <- ent_data %>% filter(FECHA == today() - days(i))
        i <- i - 1
      }
      
      #comp_days$FECHA <- paste("Tasa al",format(comp_days$FECHA, "%d %b"))
      comp_days$ENTIDAD_RES <- entidad
      
      z <- dcast(comp_days, ENTIDAD_RES ~ FECHA, value.var = "ratio")  
      
      ratio_days <- ent_data %>% filter(FECHA <= today() - days(14))
      ratio_days$ENTIDAD_RES <- entidad
      ratio_days$lower <- ifelse(ratio_days$ratio < 1,1,0)
      ratio_days <- ratio_days %>%
        group_by(grp = rleid(lower)) %>%
        mutate(lower_count = cumsum(lower))
      
      z$lower_count <- tail(ratio_days,1)$lower_count
      
      colnames(z) <- c("ENTIDAD_RES", "Tasa", "Tasa Menor 1.00")
      
      #colnames(z) <- c("ENTIDAD_RES", "Tasa")
      if(is.null(resultado)){
        resultado <- z
      } else {
        resultado <- rbind(resultado, z)
      }
    }
    
    final_df <- left_join(resultado, historico, by = "ENTIDAD_RES")
    
    datos <- por_sintomas
    colnames(datos) <- append("FECHA",colnames(datos)[-1])
    #datos <- datos %>% filter(FECHA >= today() - days(28))
    data <- datos %>% filter(RESULTADO %in% c(1,2,3))
    resultado <- NULL
    for (entidad in unique(data$ENTIDAD_RES)) {
      z <- NULL
      ent_data <- subset(data, ENTIDAD_RES == entidad)
      ent_data <- ent_data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
      ent_data <- mutate(ent_data, total = cumsum(casos))
      ent_data <- mutate(ent_data, growth =(total-lag(total, n=1))/lag(total, n = 1))
      ent_data <- mutate(ent_data, avg_growth = frollmean(x = growth, 7))
      ent_data <- mutate(ent_data, doubling =log(2)/avg_growth)
      
      comp_days <- tibble()
      i <- 14
      while (nrow(comp_days) == 0) {
        comp_days <- ent_data %>% filter(FECHA == today() - days(i))
        i <- i - 1
      }
      #comp_days$FECHA <- "Crecimiento Porcentual"
      comp_days$ENTIDAD_RES <- entidad
      
      z <- dcast(comp_days, ENTIDAD_RES ~ FECHA, value.var = "avg_growth")
      
      z2 <- dcast(comp_days, ENTIDAD_RES ~ FECHA, value.var = "doubling")
      
      z3 <- cbind(z, z2)
      z3 <- z3[,c(1,2,4)]
      colnames(z3) <- c("ENTIDAD_RES", "Crecimiento Porcentual", "Duplicacion")
      if(is.null(resultado)){
        resultado <- z3
      } else {
        resultado <- rbind(resultado, z3)
      }
    }
    
    final_df <- left_join(resultado, final_df, by = "ENTIDAD_RES")
    datos <- por_sintomas
    colnames(datos) <- append("FECHA",colnames(datos)[-1])
    
    data <- datos %>% filter(RESULTADO %in% c(1,2,3))
    data <- data %>% filter(FECHA <= today() - days(14))
    resultado <- NULL
    for (entidad in unique(data$ENTIDAD_RES)) {
      z <- NULL
      
      ent_data <- subset(data, ENTIDAD_RES == entidad)
      #ent_data <- ent_data %>% filter(FECHA <= today() - days(13))
      ent_data <- ent_data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
      #ent_data <- mutate(ent_data, casos = total - lag(total, n =1))
      ent_data <- mutate(ent_data, lower = ifelse(casos < lag(casos, n =1),1,0))
      max_casos <- max(ent_data$casos, na.rm = T)
      ent_data <- mutate(ent_data, lower_max = ifelse(casos < max_casos,1,0))
      
      ent_data <- ent_data %>%
        group_by(grp = rleid(lower)) %>%
        mutate(lower_count = cumsum(lower))
      
      ent_data$lower_max_grp <- rleid(ent_data$lower_max)
      ent_data <- ent_data %>%
        group_by(lower_max_grp) %>%
        mutate(lower_count_max = cumsum(lower_max))
      
      
      ent_data$ENTIDAD_RES <- entidad
      
      #z <- tail(ent_data,1)[,c(10,7,9)]
      z <- tail(ent_data,1)[,c(9,6,8)]
      #z$ENTIDAD_RES <- entidad
      
      colnames(z) <- c("ENTIDAD_RES", "dias_baja", "dias_desde_pico")
      z$pico_casos <- max_casos
      z$ultimo_casos <- tail(ent_data,1)$casos
      if(is.null(resultado)){
        resultado <- z
      } else {
        resultado <- rbind(resultado, z)
      }
    }
    
    final_df <- left_join(resultado, final_df, by = "ENTIDAD_RES")
    
    final_df
  }
  
  temp2 <- paste(getwd(),"/datos_abiertos/datos_abiertos_covid19_",format(today()-days(1),"%d.%m.%Y"),".zip", sep = "")
  
  file_name2 <- unzip(temp2, list = TRUE)$Name[1]
  
  
  temp <- paste(getwd(),"/datos_abiertos/datos_abiertos_covid19_",format(today(),"%d.%m.%Y"),".zip", sep = "")
  #download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
  download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
  
  
  file_name <- unzip(temp, list = TRUE)$Name[1]
  
  print(paste("El nombre del archivo de datos es", file_name))
  
  if (file_name != file_name2) {
    
    log_time("Leyendo datos...")
    ## Leyendo datos
    date_file <- as.Date(substr(file_name, 1,6),"%y%m%d")
    hoy <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
    hoy$FECHA_ACTUALIZACION <- format(date_file,"%Y-%m-%d")
    if("CLASIFICACION_FINAL" %in% colnames(hoy)){
      index <- grep("CLASIFICACION_FINAL", colnames(hoy))
      colnames(hoy)[index]<- "RESULTADO"  
    }
    temp_hoy <- hoy
  
    log_time("Agrupando por fecha de inicio de sintomas...")
    
    ## Actualiza por sintomas
    por_sintomas <- hoy %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(FECHA_ACTUALIZACION) - days(13) ))
    por_sintomas <- por_sintomas %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)
    
    log_time("Agrupando por fecha de captura...")
  
    colnames(hoy)[1]<-"FECHA_CAPTURA"
    hoy <- hoy %>% group_by(FECHA_CAPTURA, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(FECHA_CAPTURA) - days(13) ))
    hoy <- hoy %>% mutate(FECHA_CAPTURA = as.Date(FECHA_CAPTURA, "%Y-%m-%d")) %>% arrange(FECHA_CAPTURA)
    if(exists("por_captura")){
      por_captura <- rbind(por_captura, hoy)  
    } else {
      por_captura <- hoy
    }
    
    
    log_time("Leyendo datos del dia de ayer...")
    ayer <- read.table(unz(temp2, file_name2), sep = ",", header = TRUE)
    if("CLASIFICACION_FINAL" %in% colnames(ayer)){
      index <- grep("CLASIFICACION_FINAL", colnames(ayer))
      colnames(ayer)[index]<- "RESULTADO"  
    }
  
    hoy <- temp_hoy
    
    log_time("Encontrando cambios de resultado y casos nuevos...")
    diff <- anti_join(hoy, ayer, by = "ID_REGISTRO")
    adds <- subset(diff, RESULTADO %in% c(1,2,3))
    
    same <- inner_join(hoy, ayer, by = "ID_REGISTRO")
    changes <- subset(same, RESULTADO.y %!in% c(1,2,3) & RESULTADO.x %in% c(1,2,3))
    
    log_time("Agrupando informacion para cambios y nuevos casos...")
    adds <- adds %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES) %>% summarise(casos = sum(RESULTADO %in% c(1,2,3)))
    changes <- changes %>% group_by(FECHA_SINTOMAS.x, ENTIDAD_RES.x, MUNICIPIO_RES.x) %>% summarise(casos = sum (RESULTADO.x %in% c(1,2,3)))
    colnames(changes) <- c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "casos")
    colnames(adds) <- c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "casos")
    
    log_time("Combinando cambios y casos nuevos...")
    both <- rbind(changes, adds)
    sum(both$casos)
    
    log_time("Agrupando casos previos...")
    ayer <- ayer %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(date_file) - days(13) ))
    ayer <- ayer %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)
    
    log_time("Creando data frame para reporte general...")
    bar <- create_data_frame()
    
    log_time("Creando reporte general...")
    
    #final_df <- bar[,c(14,9:12,6:8,2:5)]
    final_df <- bar[,c(15,10:13,6:9,2:5)]
    colnames(final_df) <- c("Estado", "Positivos", "Negativos", "Pendientes", "Positividad", "Crecimiento Promedio", "Tiempo de Duplicacion", "Tasa Casos Nuevos", "Tasa Menor a 1.00","En Descenso", "Ultimo Pico", "Pico de Casos",paste("Casos el dia",format(today() - days(13),"%d %b")) )
    
    #colnames(final_df) <- c("Estado", "Casos Positivos", "Casos Negativos", "Casos Pendientes", "% Positividad", "% Crecimiento Promedio", "Tiempo de Duplicacion", "Tasa Casos Nuevos", "Dias a la Baja", "Dias desde Ultimo Pico", "Pico de Casos", "Ultimo Reporte Casos")
    final_df <- final_df[order(final_df$Estado),]
    
    #datatable(final_df) %>% formatPercentage(c(5:6), 2) %>% formatRound(c(7), 2)
    gtt <- gt(final_df) %>% 
      data_color(columns = c(5), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`Positividad`,na.rm = T),max(final_df$`Positividad`,na.rm = T)), reverse = T)) %>%
      data_color(columns = c(6), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`Crecimiento Promedio`,na.rm = T),max(final_df$`Crecimiento Promedio`,na.rm = T)), reverse = T)) %>%
      data_color(columns = c(7), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`Tiempo de Duplicacion`, na.rm = T),max(final_df$`Tiempo de Duplicacion`, na.rm = T)))) %>%
      data_color(columns = c(8), colors = scales::col_bin(bins = 2,
                                                          palette = c(
                                                            light_red, light_green), 
                                                          domain = c(min(final_df$`Tasa Casos Nuevos`, na.rm = T),max(final_df$`Tasa Casos Nuevos`, na.rm = T)), reverse = T)) %>%
      data_color(columns = c(9), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`Tasa Menor a 1.00`, na.rm = T),max(final_df$`Tasa Menor a 1.00`, na.rm = T)))) %>%
      data_color(columns = c(10), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`En Descenso`, na.rm = T),max(final_df$`En Descenso`, na.rm = T)))) %>%
      data_color(columns = c(11), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`Ultimo Pico`, na.rm = T),max(final_df$`Ultimo Pico`, na.rm = T)))) %>%
      data_color(columns = c(12), colors = scales::col_numeric(
        palette = c(
          light_red, light_yellow, light_green),
        domain = c(min(final_df$`Pico de Casos`, na.rm = T),max(final_df$`Pico de Casos`, na.rm = T)), reverse = T)) %>%
      fmt_percent(columns = c(5:6)) %>% 
      fmt_number(columns = c(7,8), decimals = 2) %>%
      fmt_number(columns = c(2:4,11,12), sep_mark = ",", decimals = 0) %>%
      tab_spanner("Casos", columns = c(2:4)) %>%
      tab_spanner("Porcentaje de", columns = c(5:6)) %>%
      tab_spanner("Dias", columns = c(9:11))
    
    log_time("Guardando reporte general...")
    gtsave(gtt, filename = paste("./images/tabla_", today(),".png", sep = ""))
    gtt
  
    log_time("Guardando datos...")
    save(ayer, both, por_captura, por_sintomas, gtt, file = "./data/datos_v2.Rdata")
#    rm(ayer,both,por_captura, por_sintomas, temp, temp2, file_name, file_name2, temp_hoy, same, changes, adds, diff)
    
#    i <- i + 1
    log_time("Arrancando app...")
    shiny::runApp()
  }
#}
