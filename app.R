#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source("functions.r")

entidades <- catalogo[["Catálogo de ENTIDADES"]]$ENTIDAD_FEDERATIVA[1:32]
entidades <- append("TODOS LOS ESTADOS",entidades)

municipios <- c("TODOS")

index_entidades <- catalogo[["Catálogo de ENTIDADES"]]
index_municipios <- catalogo[["Catálogo MUNICIPIOS"]]

get_cities <- function(entidad){
  clave_entidad <- ""
  municipios <- c("TODOS")
  if(entidad != "TODOS LOS ESTADOS"){
    clave_entidad <- index_entidades[index_entidades["ENTIDAD_FEDERATIVA"]==entidad,]$CLAVE_ENTIDAD
    
    municipios <- append(municipios, subset(index_municipios, CLAVE_ENTIDAD == clave_entidad)$MUNICIPIO)
  }
}


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("COVID19 - Mexico - Reporte Diario"),
    tabsetPanel(type = "tabs",
                tabPanel("Graficas Principales",
                  fluidRow(
                      column(3,
                             selectInput("entidad", "Estado:", entidades)),
                      column(3,
                             selectInput("municipio", "Municipio:", municipios)),
                      column(3,
                             radioButtons("fuente", "Fuente:", c("Por Fecha de Captura", "Por Fecha de Sintomas")))),
                  column(6, plotlyOutput("topChart")),
                  column(6, plotlyOutput("distCases")),
              
                  fluidRow(
                      column(6,
                             plotlyOutput("mvg_avg_ratio")),
                      column(6,
                             plotlyOutput("growth_doubling_time"))
                  ),
                  fluidRow(
                      column(6,
                             plotlyOutput("cases_by_condition")),
                      column(6,
                             plotlyOutput("test_result"))
                  ),

                ),
                tabPanel("Reporte por Estado",
                         fluidRow(
                           column(12,
                                  h3("Este reporte toma los casos en base a la fecha en que se iniciaron sintomas, por esto los datos que se representan tienen un desfase con la informacion que se presenta en los reportes de la Secretaria de Salud."))
                         ),
                         fluidRow(
                           column(12,
                                  gt_output("data_table_all"))
                         ))
    )
    # fluidRow(
    #   column(12,
    #          plotlyOutput("all_charts"))
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
      
      if(input$entidad == "TODOS LOS ESTADOS"){
        updateSelectInput(session, "municipio", choices = c("TODOS"))
      } else {
        m <- get_cities(input$entidad)
        updateSelectInput(session, "municipio", choices = m)
      }
        
    })
  
  data_set = reactive({
    if(input$fuente == "Por Fecha de Captura"){
      datos <- por_captura
    } else {
      datos <- por_sintomas
    }
    colnames(datos) <- append("FECHA",colnames(datos)[-1])
    if(input$entidad == "TODOS LOS ESTADOS"){
      datos
    } else {
      clave_entidad <- index_entidades[index_entidades["ENTIDAD_FEDERATIVA"]==input$entidad,]$CLAVE_ENTIDAD
      if(input$municipio == "TODOS"){
        subset(datos, ENTIDAD_RES == as.integer(clave_entidad))
      } else {
        clave_municipio <- index_municipios[index_municipios["MUNICIPIO"]==input$municipio,]$CLAVE_MUNICIPIO
        subset(datos, ENTIDAD_RES == as.integer(clave_entidad) & MUNICIPIO_RES == as.integer(clave_municipio) )
      }
    }
  })
  
  data_set_dist = reactive({
    datos <- ayer
    colnames(datos) <- append("FECHA",colnames(datos)[-1])
    if(input$entidad == "TODOS LOS ESTADOS"){
      datos
    } else {
      clave_entidad <- index_entidades[index_entidades["ENTIDAD_FEDERATIVA"]==input$entidad,]$CLAVE_ENTIDAD
      if(input$municipio == "TODOS"){
        subset(datos, ENTIDAD_RES == as.integer(clave_entidad))
      } else {
        clave_municipio <- index_municipios[index_municipios["MUNICIPIO"]==input$municipio,]$CLAVE_MUNICIPIO
        subset(datos, ENTIDAD_RES == as.integer(clave_entidad) & MUNICIPIO_RES == as.integer(clave_municipio) )
      }
    }
  })
  
  data_set_dist_cases = reactive({
    if(input$entidad == "TODOS LOS ESTADOS"){
      both
    } else {
      clave_entidad <- index_entidades[index_entidades["ENTIDAD_FEDERATIVA"]==input$entidad,]$CLAVE_ENTIDAD
      if(input$municipio == "TODOS"){
        subset(both, ENTIDAD_RES == as.integer(clave_entidad))
      } else {
        clave_municipio <- index_municipios[index_municipios["MUNICIPIO"]==input$municipio,]$CLAVE_MUNICIPIO
        subset(both, ENTIDAD_RES == as.integer(clave_entidad) & MUNICIPIO_RES == as.integer(clave_municipio) )
      }
    }
  })
  
  data_source = reactive({
    if(input$fuente == "Por Fecha de Captura"){
      2
    } else {
      1
    }
  })
    
    output$log_chart_text <- renderText({
        paste("<p>Grafica logaritmica del progreso de la contingencia en la entidad seleccionada, <b>entre mas plana se vea esta grafica se toma como indicador de que la velocidad de transmision y la tasa de contagio esta disminuyendo o ha llegado a su limite</b></p>.",ifelse(data_source()==1,"<p style=\"background-color:#DFE9EB50\">El area sombreada en cada grafica representa los ultimos 14 dias en los cuales es alta la probabilidad de que se documenten casos con inicio de sintomas en esas fechas.</p><br>","<br>"))
    })
    
    output$mvg_avg_ratio_text <- renderText({
        paste("<p>En esta grafica buscamos que la <font color=\"#4CA64C\">proporcion del promedio semanal sea menor a 1</font>, esto indicaria un eventual fin en los contagios, si se mantiene por <font color=\"#C53743\">encima de 1 entonces el crecimiento de contagios continuara</font></p><br>")
    })
    output$growth_doubling_time_text <- renderText({
        paste("<p>Aqui podemos apreciar la tasa de crecimiento porcentual, <b>el tiempo de duplicacion necesita ser y mantenerse lo mas alto posible</b></p><br>")
    })

    output$topChart <- renderPlotly({
       topchart(data_set(), data_source())
    })
    
    output$distCases <- renderPlotly({
      distCases(data_set_dist_cases(), data_set_dist())
    })
    
    output$mvg_avg_ratio <- renderPlotly({
      mvg_avg_ratio(data_set(), data_source())  
    })
    
    output$growth_doubling_time <- renderPlotly({
        growth_doubling_time(data_set(), data_source())
    })
    
    output$cases_by_condition <- renderPlotly({
      cases_by_condition(data_set(), data_source())
    })
    
    output$test_result <- renderPlotly({
      test_result(data_set(), data_source())
    })
    
    output$data_table_all <- render_gt({
      gtt %>% cols_align(align = "center")
    })
    
    # output$all_charts <- renderPlotly({
    #   fig1 <- topchart(data_set(), data_source())
    #   fig2 <- mvg_avg_ratio(data_set(), data_source())
    #   fig3 <- growth_doubling_time(data_set(), data_source())
    #   fig4 <- cases_by_condition(data_set(), data_source())
    #   fig5 <- test_result(data_set(), data_source())
    #   
    #   fig6 <- subplot(fig2, fig3, shareX = F, shareY = F)
    #   fig7 <- subplot(fig4, fig5, shareX = F, shareY = F)
    #   fig <- subplot(fig1, fig6, fig7, nrows = 3, shareX = F, shareY = F)
    #   fig
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
