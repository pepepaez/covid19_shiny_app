library(dplyr)
library(plotly)
library(lubridate)
library(data.table)
library(reshape2)
library(scales)

#load("./data/datos.Rdata")
load("./data/catalogo.Rdata")

load("./data/por_captura.Rdata")
load("./data/por_sintomas.Rdata")

#datos <- datos %>% filter(as.Date(FECHA_SINTOMAS) >= dmy("18/03/2020"))

dark_red <- "#981A25"
light_red <- "#C53743"
dark_green <- "#005900"
light_green <- "#4CA64C"
dark_blue <- "#1A3469"
light_blue <- "#3B5DA0"
dark_yellow <- "#C78C40"
light_yellow <- "#F9AF51"
light_ocean <- "#DFE9EB50"




entidades <- catalogo[["Catálogo de ENTIDADES"]]$ENTIDAD_FEDERATIVA[1:32]
entidades <- append("TODOS LOS ESTADOS",entidades)

municipios <- c("TODOS")

index_entidades <- catalogo[["Catálogo de ENTIDADES"]]
index_municipios <- catalogo[["Catálogo MUNICIPIOS"]]

l <- list(
  bgcolor = "#DFE9EB80",
  bordercolor = "#000000",
  borderwidth = 1,
  x = 0.01,
  y = 0.8)

get_cities <- function(entidad){
  clave_entidad <- ""
  municipios <- c("TODOS")
  if(entidad != "TODOS LOS ESTADOS"){
    clave_entidad <- index_entidades[index_entidades["ENTIDAD_FEDERATIVA"]==entidad,]$CLAVE_ENTIDAD
    
    municipios <- append(municipios, subset(index_municipios, CLAVE_ENTIDAD == clave_entidad)$MUNICIPIO)
  }
}

## for testing purposes only
# data <- por_sintomas
# data_source <- 1
# data <- por_captura
# data_source <- 2
# colnames(data) <- append("FECHA",colnames(data)[-1])
##

topchart <- function(data, data_source = 1){

  if(data_source == 1){
    data <- data %>% filter(RESULTADO == 1) 
    data <- data %>% group_by(FECHA) %>%summarise(casos = sum(hombres + mujeres + sexo_no_especificado), activos = sum(activos))
    data <- mutate(data, total= cumsum(casos))
  } else {
    data <- data %>% filter(RESULTADO == 1) 
    data <- data %>% group_by(FECHA) %>% summarise(total = sum(hombres + mujeres + sexo_no_especificado), activos = sum(activos))
    data <- mutate(data, casos= total - lag(total,1))
  }

  
  fig <- plot_ly(data, x = ~FECHA, y = ~total, type = "scatter", mode = "lines", name = "Casos Acumulados", line = list(shape= "spline", smoothing = 3, color = light_red, width = 1))
  fig <- fig %>% add_trace(y = ~casos, name = "Casos Diarios", line = list(shape= "spline", smoothing = 3, color = light_yellow))
  if(data_source == 1){
    fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = light_ocean, line=list(color = light_ocean), opacity = 1, x0=today() - days(12), x1 = today(), xref = "x", y0=min(data$casos), y1=max(data$total), yref = "y")))
    note_a <- format(subset(data, FECHA == today() - days(12))$total, big.mark = ",", scientific = F)
    ypos_a <- log10(subset(data, FECHA == today() - days(12))$total)
    
    note_b <- format(subset(data, FECHA == today() - days(12))$casos, big.mark = ",", scientific = F) 
    ypos_b <- log10(subset(data, FECHA == today() - days(12))$casos)
    
    note_date <- format(subset(data, FECHA == today() - days(12))$FECHA, "%b %d")
    xpos <- subset(data, FECHA == today() - days(12))$FECHA
    
  } else {
    note_a <- format(tail(data,1)$total, big.mark = ",", scientific = F)
    ypos_a <- log10(tail(data,1)$total)
    
    note_b <- format(tail(data,1)$casos, big.mark = ",", scientific = F) 
    ypos_b <- log10(tail(data,1)$casos)
    
    note_c <- format(tail(data,1)$activos, big.mark = ",", scientific = F) 
    ypos_c <- log10(tail(data,1)$activos)
    
    note_date <- format(tail(data,1)$FECHA, "%b %d")
    xpos <- tail(data,1)$FECHA
    
    fig <- fig %>% add_trace( y = ~activos, name = "Casos Activos", line = list(shape = "spline", smoothing = 3, color =light_blue))
    fig <- fig %>% add_annotations(text=paste('Activos:', note_c),
                                   xref = "x", yref= "y", x = xpos, y = ypos_c, 
                                   xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_blue),
                                   arrowcolor = dark_blue, arrowsize = 2, arrowwidth = 1, arrowhead = 6)
    }
  fig <- fig %>% layout(yaxis = list( type = "log", title = "Escala Logaritmica", fixedrange = TRUE, showline = T), legend = l, hovermode = "compare", xaxis = list(fixedrange = TRUE, title = "", showline = T), title = list(text = "F1 - Comportamiento Actual", anchor = "left", xref = "paper", x=0))
  
  fig <- fig %>% add_annotations(text=paste('Acumulados:', note_a,"<br>",note_date),
                  xref = "x", yref= "y", x = xpos, y = ypos_a, 
                  xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_red),
                  arrowcolor = dark_red, arrowsize = 2, arrowwidth = 1, arrowhead = 6)
  
  fig <- fig %>% add_annotations(text=paste('Diarios:',note_b),
                                 xref = "x", yref= "y", x = xpos, y = ypos_b, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_yellow),
                                 arrowcolor = dark_yellow, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text =  paste("Grafica logaritmica del progreso de la contingencia en la entidad seleccionada.<br><b>Entre mas plana se vea esta grafica se toma como indicador de que la velocidad de transmision y la tasa de contagio esta disminuyendo o ha llegado a su limite</b>.<br>",ifelse(data_source==1,"El area sombreada en cada grafica representa los ultimos 14 dias en los cuales es alta la probabilidad de que se documenten mas casos positivos.","")),
                                 xref = "paper", yref = "paper", x = 0.01, y = 0.9,
                                 xanchor = "left", yanchor = "middle", showarrow = F, align = "left")
  
  fig
}

mvg_avg_ratio <- function(data, data_source = 1){
  
  data <- data %>% filter(RESULTADO == 1)
  
  if(data_source == 1){
    data <- data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
    data <- mutate(data, total = cumsum(casos))
    data <- mutate(data, moving_avg = frollmean(x = casos, 7))
    data <- mutate(data, ratio =moving_avg/lag(moving_avg, n = 1))
    data <- mutate(data, ratio = frollmean(x = ratio, 7))
  } else {
    data <- data %>% group_by(FECHA) %>% summarise(total = sum(hombres + mujeres + sexo_no_especificado))
    data <- mutate(data, casos = total - lag(total,1))
    data <- mutate(data, moving_avg = frollmean(x = casos, 7))
    data <- mutate(data, ratio =moving_avg/lag(moving_avg, n = 1))
    data <- mutate(data, ratio = frollmean(x = ratio, 7))
  }
  
  
  fig <- plot_ly(data, x = ~FECHA, y = ~moving_avg, type = 'scatter', mode = 'lines', name = "Promedio Semanal", line = list(shape= "spline", smoothing = 3, color = light_yellow, width = 1, showline = T))
  fig <- fig %>% add_trace(y = ~ratio, name = "Proporcion de Promedio", yaxis = "y2", line = list(shape= "spline", smoothing = 3, color = light_green))
  fig <- fig %>% layout(hovermode = "compare", yaxis2 = list( showline = T, side = "right", overlaying = "y", title = "Proporcion de Promedio Semanal", fixedrange = TRUE, automargin = T), yaxis = list(showline = T, title = "Promedio Semanal", fixedrange = TRUE),xaxis = list(showline = T, fixedrange = TRUE, title = ""), legend = l, title = list(text = "F2 - Promedio Semanal v Tasa de Crecimiento", anchor = "left", xref = "paper", x=0))
  if(data_source == 1){
    fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = light_ocean, line=list(color = light_ocean), opacity = 1, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(data$ratio), yref = "y2")))
    note_a <- format(subset(data, FECHA == today() - days(12))$moving_avg, big.mark = ",", scientific = F, digits = 0, nsmall = 0)
    ypos_a <- subset(data, FECHA == today() - days(12))$moving_avg
    
    note_b <- format(subset(data, FECHA == today() - days(12))$ratio, big.mark = ",", scientific = F, digits = 2, nsmall = 2) 
    ypos_b <- subset(data, FECHA == today() - days(12))$ratio
    
    note_date <- format(subset(data, FECHA == today() - days(12))$FECHA, "%b %d")
    xpos <- subset(data, FECHA == today() - days(12))$FECHA
  } else {
    note_a <- format(tail(data,1)$moving_avg, big.mark = ",", scientific = F, digits = 0, nsmall = 0)
    ypos_a <- tail(data,1)$moving_avg
    
    note_b <- format(tail(data,1)$ratio, big.mark = ",", scientific = F, digits = 2, nsmall = 2) 
    ypos_b <- tail(data,1)$ratio
    
    xpos <- tail(data,1)$FECHA
    note_date <- format(tail(data,1)$FECHA, "%b %d")
  }
  fig <- fig %>% add_annotations(text=paste('Promedio:', note_a,"<br>",note_date),
                                 xref = "x", yref= "y", x = xpos, y = ypos_a, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_yellow),
                                 arrowcolor = dark_yellow, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text=paste('Tasa:', note_b,"<br>",note_date),
                                 xref = "x", yref= "y2", x = xpos, y = ypos_b, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = ifelse(as.numeric(note_b)<1,dark_green,ifelse(as.numeric(note_b)==1,dark_blue,dark_red))),
                                 arrowcolor = ifelse(as.numeric(note_b)<1,dark_green,ifelse(as.numeric(note_b)==1,dark_blue,dark_red)), arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text =  paste("En esta grafica buscamos que la <b>proporcion del promedio semanal</b> sea<br>menor a 1 para indicar una disminucion paulatina de la tasa de propagacion,<br>si se mantiene arriba de 1 entonces el crecimiento de contagios continuara"),
                                 xref = "paper", yref = "paper", x = 0.01, y = 0.9,
                                 xanchor = "left", yanchor = "middle", showarrow = F, align = "left")
  
  
  fig
}

growth_doubling_time <- function(data, data_source = 1){

  #data <- data %>% filter(as.Date(FECHA_SINTOMAS) <= today() - days(10))

  data <- data %>% filter(RESULTADO == 1)

  if(data_source == 1){
    data <- data %>% group_by(FECHA) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
    data <- mutate(data, total = cumsum(casos))
    data <- mutate(data, growth =(total-lag(total, n=1))/lag(total, n = 1))
    data <- mutate(data, avg_growth = frollmean(x = growth, 7))
    data <- mutate(data, doubling =log(2)/avg_growth)
  } else {
    data <- data %>% group_by(FECHA) %>% summarise(total = sum(hombres + mujeres + sexo_no_especificado))
    data <- mutate(data, casos = total - lag(total, 1))
    data <- mutate(data, growth =(total-lag(total, n=1))/lag(total, n = 1))
    data <- mutate(data, avg_growth = frollmean(x = growth, 7))
    data <- mutate(data, doubling =log(2)/avg_growth)
  }
    
  
  fig <- plot_ly(data, x = ~FECHA, y = ~avg_growth, type = 'scatter', mode = 'lines', name = "Crecimiento Porcentual Promedio", line = list(shape= "spline", smoothing = 3, showline = T, color = light_green, width = 1))
  fig <- fig %>% add_trace(y = ~doubling, name = "Tiempo de Duplicacion", yaxis = "y2", line = list(shape= "spline", smoothing = 3, showline = T, color = light_yellow))
  fig <- fig %>% layout(hovermode = "x", yaxis2 = list(showline = T, side = "right", overlaying = "y", title = "Tiempo de Duplicacion",fixedrange = TRUE, automargin = T), yaxis = list(showline = T, title = "Crecimiento Porcentual", fixedrange = TRUE, range = c(0, 0.15), tickformat = ".2%"), xaxis = list(fixedrange = TRUE, title = ""), legend = l, title = list(text = "F3 - Crecimiento Porcentual v Duplicacion de Tiempo", anchor = "left", xref = "paper", x=0))
  if(data_source == 1){
    fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = light_ocean, line=list(color = light_ocean), opacity = 1, x0=today() - days(12), x1 = today() - days(0), xref = "x", y0=min(0), y1=max(0.15), yref = "y")))
    note_a <- percent(subset(data, FECHA == today() - days(12))$avg_growth, accuracy = 0.01)
    ypos_a <- subset(data, FECHA == today() - days(12))$avg_growth
    
    note_b <- format(subset(data, FECHA == today() - days(12))$doubling, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_b <- subset(data, FECHA == today() - days(12))$doubling
    
    note_date <- format(subset(data, FECHA == today() - days(12))$FECHA, "%b %d")
    xpos <- subset(data, FECHA == today() - days(12))$FECHA
  } else {
      note_a <- percent(tail(data,1)$avg_growth, accuracy = 0.01)
      ypos_a <- tail(data,1)$avg_growth

      note_b <- format(tail(data,1)$doubling, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
      ypos_b <- tail(data,1)$doubling

      xpos <- tail(data,1)$FECHA
      note_date <- format(tail(data,1)$FECHA, "%b %d")
    }
  
  fig <- fig %>% add_annotations(text=paste('Crecimiento Porcentual:', note_a,"<br>",note_date),
                                 xref = "x", yref= "y", x = xpos, y = ypos_a, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_green),
                                 arrowcolor = dark_green, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text=paste('Tiempo Duplicacion:', note_b,"dias"),
                                 xref = "x", yref= "y2", x = xpos, y = ypos_b, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_yellow),
                                 arrowcolor = dark_yellow, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  
  fig <- fig %>% add_annotations(text =  paste("Aqui podemos apreciar la tasa de crecimiento porcentual.<br><b>El tiempo de duplicacion necesita ser y mantenerse lo mas alto posible</b><br><b>y la tasa de crecimiento necesita mantener su tendencia a la baja</b>"),
                                 xref = "paper", yref = "paper", x = 0.01, y = 0.9,
                                 xanchor = "left", yanchor = "middle", showarrow = F, align = "left")
  
  fig
}

cases_by_condition <- function(data, data_source = 1){
  
  data <- data %>% filter(RESULTADO == 1)
  
  if(data_source == 1){
    data <- data %>% group_by(FECHA) %>% summarise(amb = sum(ambulatorios), hosp = sum(hospitalizado), otr = sum(otro))
    data <- data %>% mutate(Ambulatorio = cumsum(amb), Hospitalizado = cumsum(hosp))
  } else {
    data <- data %>% group_by(FECHA) %>% summarise(Ambulatorio = sum(ambulatorios), Hospitalizado = sum(hospitalizado), Otro = sum(otro))
  }
    
  
  fig <- plot_ly(data, x = ~FECHA, y = ~Ambulatorio, type = 'scatter', mode = 'lines', name = "Ambulatorio", line = list(shape= "spline", smoothing = 3, showline = T, color = light_green, width = 1))
  fig <- fig %>% add_trace(y = ~Hospitalizado, name = "Hospitalizado", line = list(shape= "spline", smoothing = 3, color = light_yellow))
  fig <- fig %>% layout(hovermode = "x", legend = l, yaxis = list(title="",fixedrange = TRUE, showline = T), xaxis = list(fixedrange = TRUE, title = "", showline = T), title = list(text = "F4 - Acumulado por Condicion de Paciente", anchor = "left", xref = "paper", x=0))
  if(data_source == 1){
    fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = light_ocean, line=list(color = light_ocean), opacity = 1, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(c(max(data$Ambulatorio),max(data$Hospitalizado))), yref = "y")))
    
    note_a <- format(subset(data, FECHA == today() - days(12))$Ambulatorio, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_a <- subset(data, FECHA == today() - days(12))$Ambulatorio
    
    note_b <- format(subset(data, FECHA == today() - days(12))$Hospitalizado, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_b <- subset(data, FECHA == today() - days(12))$Hospitalizado
    
    note_date <- format(subset(data, FECHA == today() - days(12))$FECHA, "%b %d")
    xpos <- subset(data, FECHA == today() - days(12))$FECHA
  } else {
    note_a <- format(tail(data,1)$Ambulatorio, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_a <- tail(data,1)$Ambulatorio
    
    note_b <- format(tail(data,1)$Hospitalizado, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_b <- tail(data,1)$Hospitalizado
    
    xpos <- tail(data,1)$FECHA
    note_date <- format(tail(data,1)$FECHA, "%b %d")
  }
  
  fig <- fig %>% add_annotations(text=paste('Ambulatorio:', note_a,"<br>",note_date),
                                 xref = "x", yref= "y", x = xpos, y = ypos_a, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_green),
                                 arrowcolor = dark_green, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text=paste('Hospitalizado:', note_b),
                                 xref = "x", yref= "y", x = xpos, y = ypos_b, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_yellow),
                                 arrowcolor = dark_yellow, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text =  paste("Grafica de distribucion por tipo de paciente a lo largo de la contingencia<br><b>No se incluyen pacientes en Cuidados Intensivos</b>"),
                                 xref = "paper", yref = "paper", x = 0.01, y = 0.9,
                                 xanchor = "left", yanchor = "middle", showarrow = F, align = "left")
  
  fig
  
  
}


test_result <- function(data, data_source = 1){
  
  #  data <- data %>% filter(RESULTADO == 1)
  
  if(data_source == 1){
    data <- data %>% group_by(FECHA, RESULTADO)  %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
    data <- data %>% group_by(RESULTADO) %>% mutate(total = cumsum(casos))
  } else {
    data <- data %>% group_by(FECHA, RESULTADO) %>% summarise(casos = sum(hombres + mujeres + sexo_no_especificado))
  }
  
  data <- dcast(data, FECHA ~ RESULTADO, sum)
  colnames(data) <- c("FECHA", "Positivo", "Negativo", "Pendiente")
  data <- data %>% mutate(Positivo = ifelse(Positivo == 0, NA, Positivo)) %>% tidyr::fill(Positivo, .direction = c("down"))
  data <- data %>% mutate(Negativo = ifelse(Negativo == 0, NA, Negativo)) %>% tidyr::fill(Negativo, .direction = c("down"))
  
  
  fig <- plot_ly(data, x = ~FECHA, y = ~Positivo, type = 'scatter', mode = 'lines', name = "Positivo", line = list(shape= "spline", smoothing = 3, color = light_red, width = 1))
  fig <- fig %>% add_trace(y = ~Negativo, name = "Negativo", line = list(shape= "spline", smoothing = 3, color = light_green))
  fig <- fig %>% add_trace(y = ~Pendiente, name = "Pendiente", line = list(shape= "spline", smoothing = 3, color = light_yellow))
  fig <- fig %>% layout(hovermode = "x", legend = l, yaxis = list(showline = T, title="",fixedrange = TRUE), xaxis = list(showline = T, fixedrange = TRUE, title = ""), title = list(text = "F5 - Acumulado por Resultado de Prueba", anchor = "left", xref = "paper", x=0))
  if(data_source == 1){
    fig <- fig %>% layout(shapes = list(list(type="rect", fillcolor = light_ocean, line=list(color = light_ocean), opacity = 1, x0=today() - days(12), x1 = today(), xref = "x", y0=min(0), y1=max(c(max(data$Positivo, na.rm = T),max(data$Negativo, na.rm = T),max(data$Pendiente, na.rm = T))), yref = "y")))
    
    note_a <- format(subset(data, FECHA == today() - days(12))$Positivo, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_a <- subset(data, FECHA == today() - days(12))$Positivo
    
    note_b <- format(subset(data, FECHA == today() - days(12))$Negativo, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_b <- subset(data, FECHA == today() - days(12))$Negativo
    
    note_c <- format(subset(data, FECHA == today() - days(12))$Pendiente, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_c <- subset(data, FECHA == today() - days(12))$Pendiente 
    
    note_date <- format(subset(data, FECHA == today() - days(12))$FECHA, "%b %d")
    xpos <- subset(data, FECHA == today() - days(12))$FECHA
  } else {
    note_a <- format(tail(data,1)$Positivo, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_a <- tail(data,1)$Positivo
    
    note_b <- format(tail(data,1)$Negativo, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_b <- tail(data,1)$Negativo
    
    note_c <- format(tail(data,1)$Pendiente, big.mark = ",", scientific = F, digits = 0, nsmall = 0) 
    ypos_c <- tail(data,1)$Pendiente
    
    xpos <- tail(data,1)$FECHA
    note_date <- format(tail(data,1)$FECHA, "%b %d")
  }
  
  fig <- fig %>% add_annotations(text=paste('Positivo:', note_a),
                                 xref = "x", yref= "y", x = xpos, y = ypos_a, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_red),
                                 arrowcolor = dark_red, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text=paste('Negativo:', note_b,"<br>",note_date),
                                 xref = "x", yref= "y", x = xpos, y = ypos_b, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_green),
                                 arrowcolor = dark_green, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text=paste('Pendiente:', note_c),
                                 xref = "x", yref= "y", x = xpos, y = ypos_c, 
                                 xanchor = "right", yanchor = "middle", showarrow = T, font = list(color = dark_yellow),
                                 arrowcolor = dark_yellow, arrowsize = 2, arrowwidth = 1, arrowlength = 3, arrowhead = 6)
  fig <- fig %>% add_annotations(text =  paste("Grafica de distribucion por resultado de prueba a COVID19.<br><b>Numeros reportados por Secretaria de Salud, puede no incluir pruebas de laboratorios privados.</b>"),
                                 xref = "paper", yref = "paper", x = 0.01, y = 0.9,
                                 xanchor = "left", yanchor = "middle", showarrow = F, align = "left")
  
   fig
  
}

filter_data <- function(){

  is.na(data)<-sapply(data, is.infinite)
  data[is.na(data)]<-0
  
  data
}