library(dplyr)
library(lubridate)
library(formattable)
library(DT)
library(gt)
library(plotly)
library(data.table)
library(reshape2)
library(scales)

## Baja archivo
print("Descargando archivo...")
load("./data/datos.Rdata")

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

create_data_frame <- function(){
  datos <- por_sintomas
  colnames(datos) <- append("FECHA",colnames(datos)[-1])
  
  historico <- datos
  
  historico <- historico %>% group_by(ENTIDAD_RES) %>% summarise(positivos = sum(hombres[RESULTADO == 1] + mujeres[RESULTADO == 1] + sexo_no_especificado[RESULTADO == 1]),
                                                                 negativos = sum(hombres[RESULTADO == 2] + mujeres[RESULTADO == 2] + sexo_no_especificado[RESULTADO == 2]),
                                                                 pendientes = sum(hombres[RESULTADO == 3] + mujeres[RESULTADO == 3] + sexo_no_especificado[RESULTADO == 3]),
                                                                 pct_positividad = positivos / (positivos + negativos + pendientes))
  
  index_entidades <- catalogo[["Catálogo de ENTIDADES"]]
  
  temp_index_entidades <- index_entidades
  temp_index_entidades$ENTIDAD_RES <- as.integer(temp_index_entidades$CLAVE_ENTIDAD)
  historico <- left_join(historico, temp_index_entidades, by = "ENTIDAD_RES")
  
  
  
  datos <- por_sintomas
  colnames(datos) <- append("FECHA",colnames(datos)[-1])
  data <- datos %>% filter(RESULTADO == 1)
  resultado <- NULL
  for (entidad in unique(data$ENTIDAD_RES)) {
    z <- NULL
    ent_data <- subset(data, ENTIDAD_RES == entidad)
    ent_data <- ent_data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
    ent_data <- mutate(ent_data, total = cumsum(casos))
    ent_data <- mutate(ent_data, moving_avg = frollmean(x = casos, 7))
    ent_data <- mutate(ent_data, ratio =moving_avg/lag(moving_avg, n = 1))
    ent_data <- mutate(ent_data, ratio = frollmean(x = ratio, 7))
    
    comp_days <- ent_data %>% filter(FECHA == today() - days(14))
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
  data <- datos %>% filter(RESULTADO == 1)
  resultado <- NULL
  for (entidad in unique(data$ENTIDAD_RES)) {
    z <- NULL
    ent_data <- subset(data, ENTIDAD_RES == entidad)
    ent_data <- ent_data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
    ent_data <- mutate(ent_data, total = cumsum(casos))
    ent_data <- mutate(ent_data, growth =(total-lag(total, n=1))/lag(total, n = 1))
    ent_data <- mutate(ent_data, avg_growth = frollmean(x = growth, 7))
    ent_data <- mutate(ent_data, doubling =log(2)/avg_growth)
    
    comp_days <- ent_data %>% filter(FECHA == today() - days(14))
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
  data <- datos %>% filter(RESULTADO == 1)
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

#cont <- readline(prompt = paste("El nombre del archivo de datos es", file_name, ", continuar? [s/N]:"))

print(paste("El nombre del archivo de datos es", file_name))

if (file_name != file_name2) {
  
  print("Leyendo datos...")
  ## Leyendo datos
  hoy <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
  colnames(hoy)[33]<- "RESULTADO"
  temp_hoy <- hoy
  #unlink(temp)
  
  print("Agrupando por fecha de inicio de sintomas...")
  
  ## Actualiza por sintomas
  por_sintomas <- hoy %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= today() - days(13) ))
  por_sintomas <- por_sintomas %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)
  
  print("Agrupando por fecha de captura...")
  ## Actualiza por captura
  #rm(por_captura)

  colnames(hoy)[1]<-"FECHA_CAPTURA"
  hoy <- hoy %>% group_by(FECHA_CAPTURA, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(FECHA_CAPTURA) - days(13) ))
  hoy <- hoy %>% mutate(FECHA_CAPTURA = as.Date(FECHA_CAPTURA, "%Y-%m-%d")) %>% arrange(FECHA_CAPTURA)
  por_captura <- rbind(por_captura, hoy)
  
  ## Guarda la informacion
  print("Guardando nuevos archivos...")
  #save(por_captura, file="./data/por_captura.Rdata")
  #save(por_sintomas, file = "./data/por_sintomas.Rdata")
  
  ###########################
  require(dplyr)
  
  print("Abriendo archivo del dia de ayer...")
  temp <- paste(getwd(),"/datos_abiertos/datos_abiertos_covid19_",format(today()-days(1),"%d.%m.%Y"),".zip", sep = "")
  
  file_name <- unzip(temp, list = TRUE)$Name[1]
  print("Leyendo datos del dia de ayer...")
  ayer <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
  colnames(ayer)[33] <- "RESULTADO"
  #unlink(temp)
  
  hoy <- temp_hoy
  
  print("Encontrando cambios de resultado y casos nuevos...")
  diff <- anti_join(hoy, ayer, by = "ID_REGISTRO")
  adds <- subset(diff, RESULTADO == 1)
  
  same <- inner_join(hoy, ayer, by = "ID_REGISTRO")
  changes <- subset(same, RESULTADO.y != 1 & RESULTADO.x == 1)
  
  print("Agrupando informacion para cambios y nuevos casos...")
  adds <- adds %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES) %>% summarise(casos = sum(RESULTADO == 1))
  changes <- changes %>% group_by(FECHA_SINTOMAS.x, ENTIDAD_RES.x, MUNICIPIO_RES.x) %>% summarise(casos = sum (RESULTADO.x == 1))
  colnames(changes) <- c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "casos")
  colnames(adds) <- c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "casos")
  
  print("Combinando cambios y casos nuevos...")
  both <- rbind(changes, adds)
  sum(both$casos)
  
  print("Guardando casos nuevos...")
  #save(both, file = "./data/casos_nuevos.Rdata")
  
  print("Agrupando casos previos...")
  ayer <- ayer %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= today() - days(13) ))
  ayer <- ayer %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)
  print("Guardando datos casos previos...")
  #save(ayer, file = "./data/dia_previo.Rdata")
  

  
  bar <- create_data_frame()
  
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
  
  gtsave(gtt, filename = paste("./images/tabla_", today(),".png", sep = ""))
  
  save(ayer, both, por_captura, por_sintomas, gtt, file = "./data/datos.Rdata")
  save.image()
  
  shiny::runApp()
}
