library(dplyr)
library(plotly)
library(lubridate)
library(data.table)

load("./data/datos.Rdata")
load("./data/catalogo.Rdata")

## Todos los casos
data <- datos
data <- subset(datos, ENTIDAD_UM == 2)

data <- data %>% group_by(FECHA_SINTOMAS) %>% arrange(FECHA_SINTOMAS) %>% summarise(casos = sum(RESULTADO == 1))
data <- mutate(data, total = cumsum(casos), moving_avg = frollmean(x = casos, 7))
data <- mutate(data, ratio =moving_avg/lag(moving_avg, n = 1))

fig <- plot_ly(data, x = ~FECHA_SINTOMAS, y = ~moving_avg, type = 'scatter', mode = 'lines', name = "Casos Acumulados")
fig <- fig %>% add_trace(y = ~casos, name = "Casos Diarios")

fig

## Casos Activos
data <- datos %>% filter(as.Date(FECHA_SINTOMAS) >= today() - days(14))
data <- data %>% group_by(FECHA_SINTOMAS) %>% arrange(FECHA_SINTOMAS) %>% summarise(casos = sum(RESULTADO == 1))
data <- mutate(data, total = cumsum(casos), moving_avg = frollmean(x = casos, 7))
data <- mutate(data, ratio =moving_avg/lag(moving_avg, n = 1))

fig <- plot_ly(data, x = ~FECHA_SINTOMAS, y = ~moving_avg, type = 'scatter', mode = 'lines', name = "Casos Acumulados")
fig <- fig %>% add_trace(y = ~casos, name = "Casos Diarios")

fig


###########################
require(dplyr)
temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/historicos/datos_abiertos_covid19_11.06.2020.zip",temp)
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
