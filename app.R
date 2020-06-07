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

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("COVID19 - Mexico - Reporte Diario"),
    fluidRow(
        column(3,
               selectInput("entidad", "Estado:", entidades)),
        column(3,
               selectInput("municipio", "Municipio:", municipios))),
    htmlOutput("log_chart_text"),
    plotlyOutput("topChart"),
    
    fluidRow(
        column(6,
               htmlOutput("mvg_avg_ratio_text"),
               plotlyOutput("mvg_avg_ratio")),
        column(6,
               htmlOutput("growth_doubling_time_text"),
               plotlyOutput("growth_doubling_time"))
    ),
    fluidRow(
        column(4,
               h5("Distribucion de casos por Sexo"),
               plotlyOutput("cases_by_gender")),
        column(4,
               h5("Distribucion de casos por condicion"),
               plotlyOutput("cases_by_condition")),
        column(4,
               h5("Distribucion por resultado a prueba COVID19"),
               plotlyOutput("test_result"))
    )
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
    
    output$log_chart_text <- renderText({
        paste("Grafica logaritmica del progreso de la contingencia en la entidad seleccionada, <b>esta informacion esta ajustada al dia en que se registraron sintomas</b>, esto provoca que durante los ultimos 10 dias de la grafica se aprecie una disminucion drastica que no es real, esto es porque con el paso de los dias esta grafica se actualiza para asignar los nuevos casos a la fecha de inicio de sintomas. <p style=\"background-color:#EECCFF\">El area sombreada en cada grafica representa los ultimos 14 dias en los cuales es alta la probabilidad de que se documenten casos con inicio de sintomas en esas fechas.</p>")
    })
    
    output$mvg_avg_ratio_text <- renderText({
        paste("En esta grafica buscamos que la <font color=\"#04BF17\">proporcion del promedio semanal sea menor a 1</font>, esto indicaria un eventual fin en los contagios, si se mantiene por <font color=\"DE1802\">encima de 1 entonces el crecimiento de contagios continuara</font>")
    })
    output$growth_doubling_time_text <- renderText({
        paste("Aqui podemos apreciar la tasa de crecimiento porcentual, esta grafica esta limitada 2 semanas atras para no considerar casos que aun no se han detectado por estar dentro del periodo de incubacion, <b>el tiempo de duplicacion necesita ser y mantenerse lo mas alto posible</b>")
    })

    output$topChart <- renderPlotly({
       topchart(data_set())
    })
    
    output$mvg_avg_ratio <- renderPlotly({
      mvg_avg_ratio(data_set())  
    })
    
    output$growth_doubling_time <- renderPlotly({
        growth_doubling_time(data_set())
    })
    
    output$cases_by_gender <- renderPlotly({
      cases_by_gender(data_set())
    })
    
    output$cases_by_condition <- renderPlotly({
      cases_by_condition(data_set())
    })
    
    output$test_result <- renderPlotly({
      test_result(data_set())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
