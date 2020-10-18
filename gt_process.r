library(formattable)
library(DT)
library(gt)
library(dplyr)
library(plotly)
library(lubridate)
library(data.table)
library(reshape2)
library(scales)

load("./data/catalogo.Rdata")
load("./data/datos.Rdata")


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


zz <- create_data_frame()