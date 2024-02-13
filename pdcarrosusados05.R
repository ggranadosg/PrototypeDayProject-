#Este archivo contiene solamente el código del shiny precisa del archivo del análisis
#del set de datos carrosusados.csv
# Cargar paquetes necesarios
library(tidyverse)
library(shiny)
library(ggplot2)

# Leer el archivo CSV
#carros_usados <- read.csv("D:/BajadoEnD/20231218Módulo05Estadística&ProgramaciónConR/PrototypeDay/carrosusados.csv")


poly_model <- lm(price ~ poly(year, 2) + poly(mileage, 2), data = carros_usados_limpio)

predict_car <- function(year, mileage) {
  prediction <- predict(poly_model, newdata = data.frame(year = year, mileage = mileage))
  return(prediction)
}
predict_car

# UI
ui <- fluidPage(
  titlePanel("Predicción de Ventas y Precio de Automóviles"),
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Año del Modelo", value = 2020),
      numericInput("mileage", "Kilometraje", value = 50000),
      actionButton("predict", "Predecir")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predicción", textOutput("prediction_text"),textOutput("prediction_text2")),
        tabPanel("Datos", tableOutput("dataOutput")),
        tabPanel("Estadísticas", verbatimTextOutput("statsOutput")),
        tabPanel("Regresión", plotOutput("regressionPlot")),
        tabPanel("Correlación", plotOutput("correlationPlot")),
        tabPanel("Equipo 21",
                 h4("Febrero 2023"),
                 h4("Prototype Day"),
                 h5("Christian Arturo Meza Álvarez kosa_inc@hotmail.com"),
                 h5("Enrique Narváez Rodriguez nihaonarvi@gmail.com "),
                 h5("Juan Gerardo Granados Gálvez ggranadosg@gmail.com"),
                 h5("Jorge Enrique Madariaga Puertas madpue6@gmail.com")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    prediction <- predict_car(input$year, input$mileage)
    output$predictionOutput <- renderText({
      paste("Precio estimado de venta: $", round(prediction, 2))
    })
  })
  
  output$dataOutput <- renderTable({
    head(carros_usados_limpio)
  })
  
  output$statsOutput <- renderPrint({
    summary(carros_usados_limpio)
  })
  
  output$regressionPlot <- renderPlot({
    ggplot(data = carros_usados_limpio, aes(x = mileage, y = price)) +
      geom_point(aes(color = factor(accidents_or_damage))) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relación entre Precio y Kilometraje",
           x = "Kilometraje",
           y = "Precio") +
      theme_minimal() +
      coord_cartesian(xlim = c(0, 100000),ylim = c(500, 100000))
  })
  
  # Implementación de un gráfico de correlación simplificado
  output$correlationPlot <- renderPlot({
    # Este código es un marcador de posición. Deberías implementar tu propia lógica para el gráfico de correlación.
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

