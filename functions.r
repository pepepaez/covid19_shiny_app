library(dplyr)
library(plotly)
library(lubridate)
library(data.table)
library(reshape2)

load("./data/datos.Rdata")
load("./data/catalogo.Rdata")

datos <- datos %>% filter(as.Date(FECHA_SINTOMAS) >= dmy("18/03/2020"))

entidades <- catalogo[["Catálogo de ENTIDADES"]]$ENTIDAD_FEDERATIVA[1:32]
entidades <- append("TODOS LOS ESTADOS",entidades)

municipios <- c("TODOS")

index_entidades <- catalogo[["Catálogo de ENTIDADES"]]
index_municipios <- catalogo[["Catálogo MUNICIPIOS"]]

l <- list(
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2,
  x = 0.1,
  y = 0.9)

get_cities <- function(entidad){
  clave_entidad <- ""
  municipios <- c("TODOS")
  if(entidad != "TODOS LOS ESTADOS"){
    clave_entidad <- index_entidades[index_entidades["ENTIDAD_FEDERATIVA"]==entidad,]$CLAVE_ENTIDAD
    
    municipios <- append(municipios, subset(index_municipios, CLAVE_ENTIDAD == clave_entidad)$MUNICIPIO)
  }
}

topchart <- function(data){

  data <- data %>% filter(RESULTADO == 1)
  data <- data %>% group_by(FECHA_SINTOMAS) %>% summarise(casos = sum(RESULTADO))
  data <- mutate(data, total = cumsum(casos))
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~total, type = "scatter", mode = "lines", name = "Casos Acumulados", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~casos, name = "Casos Diarios", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(yaxis = list(type="log", title = "Escala Logaritmica", fixedrange = TRUE), legend = l, hovermode = "compare", xaxis = list(fixedrange = TRUE))
  fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = "purple", line=list(color = "#EECCFF"), opacity = 0.3, x0=today() - days(12), x1 = today(), xref = "x", y0=min(data$casos), y1=max(data$total), yref = "y")))
  fig
}

mvg_avg_ratio <- function(data){
  
  data <- data %>% filter(RESULTADO == 1)
  data <- data %>% group_by(FECHA_SINTOMAS) %>% summarise(casos = sum(RESULTADO))
  data <- mutate(data, total = cumsum(casos))
  data <- mutate(data, moving_avg = frollmean(x = casos, 7))
  data <- mutate(data, ratio =moving_avg/lag(moving_avg, n = 1))
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~moving_avg, type = 'scatter', mode = 'lines', name = "Promedio Semanal", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~ratio, name = "Proporcion de Promedio", yaxis = "y2", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(hovermode = "compare", yaxis2 = list(side = "right", overlaying = "y", title = "Proporcion de Promedio Semanal", fixedrange = TRUE, automargin = T), yaxis = list(title = "Promedio Semanal", fixedrange = TRUE),xaxis = list(fixedrange = TRUE), legend = l)
  fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = "purple", line=list(color = "#EECCFF"), opacity = 0.3, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(data$ratio), yref = "y2")))
  
  fig
}

growth_doubling_time <- function(data){

  #data <- data %>% filter(as.Date(FECHA_SINTOMAS) <= today() - days(10))

  data <- data %>% filter(RESULTADO == 1)
  data <- data %>% group_by(FECHA_SINTOMAS) %>% summarise(casos = sum(RESULTADO))
  data <- mutate(data, total = cumsum(casos))

  data <- mutate(data, growth =(total-lag(total, n=1))/lag(total, n = 1))
  data <- mutate(data, avg_growth = frollmean(x = growth, 7))
  data <- mutate(data, doubling =log(2)/avg_growth)
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~avg_growth, type = 'scatter', mode = 'lines', name = "Crecimiento Porcentual Promedio", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~doubling, name = "Tiempo de Duplicacion", yaxis = "y2", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(hovermode = "x", yaxis2 = list(side = "right", overlaying = "y", title = "Tiempo de Duplicacion",fixedrange = TRUE, automargin = T), yaxis = list(title = "Crecimiento Porcentual", fixedrange = TRUE, range = c(0, 0.15), tickformat = "%"), xaxis = list(fixedrange = TRUE), legend = l)
  fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = "purple", line=list(color = "#EECCFF"), opacity = 0.3, x0=today() - days(12), x1 = today() - days(0), xref = "x", y0=min(0), y1=max(0.15), yref = "y")))
  
  fig
}

cases_by_gender <- function(data){
  
  data <- data %>% group_by(FECHA_SINTOMAS, SEXO)  %>% summarise(casos = sum(RESULTADO == 1))
  data <- data %>% group_by(SEXO) %>% mutate(total = cumsum(casos))
  
  data <- dcast(data, FECHA_SINTOMAS ~ SEXO, sum)
  colnames(data) <- c("FECHA_SINTOMAS","Mujer","Hombre")
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~Mujer, type = 'scatter', mode = 'lines', name = "Mujeres", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~Hombre, name = "Hombres", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(hovermode = "x", legend = l, yaxis = list(title="",fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = "purple", line=list(color = "#EECCFF"), opacity = 0.3, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(c(max(data$Hombre),max(data$Mujer))), yref = "y")))
  fig
  
}

cases_by_age <- function(data){
  
  data <- data %>% group_by(EDAD, RESULTADO)  %>% summarise(casos = sum(RESULTADO))
  data <- data %>% group_by(RESULTADO) %>% mutate(total = cumsum(casos))
  
  data <- dcast(data, EDAD ~ RESULTADO, sum)
  #colnames(data) <- c("EDAD","Mujer","Hombre")
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~Mujer, type = 'scatter', mode = 'lines', name = "Mujeres", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~Hombre, name = "Hombres", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(hovermode = "x", legend = l, yaxis = list(title="",fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  
  fig
  
}

cases_by_condition <- function(data){
  
  data <- data %>% group_by(FECHA_SINTOMAS, TIPO_PACIENTE) %>% summarise(casos = sum(RESULTADO))
  data <- data %>% group_by(TIPO_PACIENTE) %>% mutate(total = cumsum(casos))
  
  data <- dcast(data, FECHA_SINTOMAS ~ TIPO_PACIENTE, sum)
  colnames(data) <- c("FECHA_SINTOMAS","Ambulatorio","Hospitalizado")
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~Ambulatorio, type = 'scatter', mode = 'lines', name = "Ambulatorio", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~Hospitalizado, name = "Hospitalizado", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(hovermode = "x", legend = l, yaxis = list(title="",fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = "purple", line=list(color = "#EECCFF"), opacity = 0.3, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(c(max(data$Ambulatorio),max(data$Hospitalizado))), yref = "y")))
  fig
  
}

test_result <- function(data){
  
  data <- data %>% group_by(FECHA_SINTOMAS, RESULTADO)  %>% summarise(casos = sum(RESULTADO))
  data <- data %>% group_by(RESULTADO) %>% mutate(total = cumsum(casos))
  
  data <- dcast(data, FECHA_SINTOMAS ~ RESULTADO, sum)
  colnames(data) <- c("FECHA_SINTOMAS","Positivo","Negativo", "Pendiente")
  data <- data %>% mutate(Positivo = ifelse(Positivo == 0, NA, Positivo)) %>% tidyr::fill(Positivo, .direction = c("down"))
  data <- data %>% mutate(Negativo = ifelse(Negativo == 0, NA, Negativo)) %>% tidyr::fill(Negativo, .direction = c("down"))
  
  
  fig <- plot_ly(data, x = as.Date(data$FECHA_SINTOMAS, format = "%Y-%m-%d"), y = ~Positivo, type = 'scatter', mode = 'lines', name = "Positivo", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~Negativo, name = "Negativo", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% add_trace(y = ~Pendiente, name = "Pendiente", line = list(shape= "spline", smoothing = 3))
  fig <- fig %>% layout(hovermode = "x", legend = l, yaxis = list(title="",fixedrange = TRUE), xaxis = list(fixedrange = TRUE))
  fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = "purple", line=list(color = "#EECCFF"), opacity = 0.3, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(c(max(data$Positivo),max(data$Negativo),max(data$Pendiente))), yref = "y")))
  fig
  
}

filter_data <- function(){

  is.na(data)<-sapply(data, is.infinite)
  data[is.na(data)]<-0
  
  data
}