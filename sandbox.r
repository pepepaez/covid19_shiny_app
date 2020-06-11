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
