# temp <- tempfile()
# download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
# file_name <- unzip(temp, list = TRUE)$Name[1]
# datos <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
# unlink(temp)
# save(datos, file = "./data/datos.Rdata")
# save.image()

library(dplyr)
library(lubridate)

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
rm(por_captura)
load("./data/por_captura.Rdata")
colnames(hoy)[1]<-"FECHA_CAPTURA"
hoy <- hoy %>% group_by(FECHA_CAPTURA, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= as.Date(FECHA_CAPTURA) - days(13) ))
hoy <- hoy %>% mutate(FECHA_CAPTURA = as.Date(FECHA_CAPTURA, "%Y-%m-%d")) %>% arrange(FECHA_CAPTURA)
por_captura <- rbind(por_captura, hoy)

## Guarda la informacion
save(por_captura, file="./data/por_captura.Rdata")
save.image()
save(por_sintomas, file = "./data/por_sintomas.Rdata")
save.image()


###########################
require(dplyr)
temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_13.06.2020.zip",temp)
file_name <- unzip(temp, list = TRUE)$Name[1]
ayer <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
unlink(temp)

temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
file_name <- unzip(temp, list = TRUE)$Name[1]
hoy <- read.table(unz(temp, file_name), sep = ",", header = TRUE)
unlink(temp)

diff <- anti_join(hoy, ayer, by = "ID_REGISTRO")
adds <- subset(diff, RESULTADO == 1)

same <- inner_join(hoy, ayer, by = "ID_REGISTRO")
changes <- subset(same, RESULTADO.y != 1 & RESULTADO.x == 1)

adds <- adds %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES) %>% summarise(casos = sum(RESULTADO == 1))
changes <- changes %>% group_by(FECHA_SINTOMAS.x, ENTIDAD_RES.x, MUNICIPIO_RES.x) %>% summarise(casos = sum (RESULTADO.x == 1))
colnames(changes) <- c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "casos")
colnames(adds) <- c("FECHA", "ENTIDAD_RES", "MUNICIPIO_RES", "casos")

both <- rbind(changes, adds)
sum(both$casos)
save(both, file = "./data/casos_nuevos.Rdata")
save.image()

ayer <- ayer %>% group_by(FECHA_SINTOMAS, ENTIDAD_RES, MUNICIPIO_RES, RESULTADO) %>% summarise(casos = sum(T), mujeres = sum(SEXO == 1), hombres = sum(SEXO == 2), sexo_no_especificado = sum(SEXO == 99), ambulatorios = sum(TIPO_PACIENTE == 1), hospitalizado = sum(TIPO_PACIENTE == 2), otro = sum(TIPO_PACIENTE == 99), intensivo = sum(UCI == 1), activos = sum(as.Date(FECHA_SINTOMAS) >= today() - days(13) ))
ayer <- ayer %>% mutate(FECHA_SINTOMAS = as.Date(FECHA_SINTOMAS, "%Y-%m-%d")) %>% arrange(FECHA_SINTOMAS)
save(ayer, file = "./data/dia_previo.Rdata")
save.image()
