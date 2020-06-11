  files <- list.files("../../historicos")

todos_datos = NULL

for (f in files) {
  file_name <- paste("../../historicos/", f, sep = "")
  datos <- read.table(file_name, sep = ",", header = TRUE)
  fecha <- substr(f, 3, 6)  
  captura <-   paste("2020",substr(fecha,1,2),substr(fecha,3,4), sep = "-")
  datos$FECHA_CAPTURA <- captura
  if(is.null(todos_datos)){
    todos_datos <- datos
  } else {
    todos_datos <- rbind(todos_datos, datos)  
    
  }
  
}

por_captura <- todos_datos %>% group_by(FECHA_CAPTURA, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(FECHA_CAPTURA) - days(13) ))
subset(por_captura, FECHA_CAPTURA == "2020-06-09") %>% group_by(FECHA_CAPTURA, RESULTADO) %>% summarise(act = sum(activos))

por_captura <- por_captura %>% mutate(FECHA_CAPTURA = as.Date(FECHA_CAPTURA, "%Y-%m-%d")) %>% arrange(FECHA_CAPTURA)
 

por_sintomas <- hoy %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= today() - days(13) ))
por_sintomas <- por_sintomas %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)

save(por_sintomas, file = "./data/por_sintomas.Rdata")
save.image()
save(por_captura, file = "./data/por_captura.Rdata")
save.image()

save(todos_datos, file = "./data/todos_datos.Rdata")
save.image()

## Baja archivo
temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
file_name <- unzip(temp, list = TRUE)$Name[1]
hoy <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
unlink(temp)

## Actualiza por sintomas
por_sintomas <- hoy %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= today() - days(13) ))
por_sintomas <- por_sintomas %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)

## Actualiza por captura
colnames(hoy)[1]<-"FECHA_CAPTURA"
hoy_temp <- hoy %>% group_by(FECHA_CAPTURA, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(FECHA_CAPTURA) - days(13) ))
hoy_temp <- hoy_temp %>% mutate(FECHA_CAPTURA = as.Date(FECHA_CAPTURA, "%Y-%m-%d")) %>% arrange(FECHA_CAPTURA)
save_por_captura <- por_captura
por_captura <- rbind(por_captura, hoy_temp)






