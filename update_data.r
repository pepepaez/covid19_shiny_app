library(dplyr)
library(lubridate)

## Baja archivo
print("Descargando archivo...")
load("./data/datos.Rdata")

temp <- paste(getwd(),"/datos_abiertos/datos_abiertos_covid19_",format(today(),"%d.%m.%Y"),".zip", sep = "")
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
file_name <- unzip(temp, list = TRUE)$Name[1]

cont <- readline(prompt = paste("El nombre del archivo de datos es", file_name, ", continuar? [S/n]:"))

if (cont == "S" | cont == "") {
  
  print("Leyendo datos...")
  ## Leyendo datos
  hoy <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
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
  
  source("gt_process.r")
  
  bar <- create_data_frame()
  
  final_df <- bar[,c(14,9:12,6:8,2:5)]
  colnames(final_df) <- c("Estado", "Positivos", "Negativos", "Pendientes", "Positividad", "Crecimiento Promedio", "Tiempo de Duplicacion", "Tasa Casos Nuevos", "En Descenso", "Ultimo Pico", "Pico de Casos",paste("Casos el dia",format(today() - days(13),"%d %b")) )
  
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
      domain = c(min(final_df$`En Descenso`, na.rm = T),max(final_df$`En Descenso`, na.rm = T)))) %>%
    data_color(columns = c(10), colors = scales::col_numeric(
      palette = c(
        light_red, light_yellow, light_green),
      domain = c(min(final_df$`Ultimo Pico`, na.rm = T),max(final_df$`Ultimo Pico`, na.rm = T)))) %>%
    data_color(columns = c(11), colors = scales::col_numeric(
      palette = c(
        light_red, light_yellow, light_green),
      domain = c(min(final_df$`Pico de Casos`, na.rm = T),max(final_df$`Pico de Casos`, na.rm = T)), reverse = T)) %>%
    fmt_percent(columns = c(5:6)) %>% 
    fmt_number(columns = c(7,8), decimals = 2) %>%
    fmt_number(columns = c(2:4,11,12), sep_mark = ",", decimals = 0) %>%
    tab_spanner("Casos", columns = c(2:4)) %>%
    tab_spanner("Porcentaje de", columns = c(5:6)) %>%
    tab_spanner("Dias", columns = c(9:10))
  
  gtsave(gtt, filename = paste("./images/tabla_", today(),".png", sep = ""))
  
  save(ayer, both, por_captura, por_sintomas, gtt, file = "./data/datos.Rdata")
  save.image()
}
