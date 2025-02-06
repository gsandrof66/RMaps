library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput("plot")
)

server <- function(input, output) {
  output$plot <- renderPlotly({
    p <- plot_ly(
      data = iris, 
      x = ~Sepal.Length, 
      y = ~Sepal.Width, 
      type = 'scatter', 
      mode = 'markers'
    )
    
    p <- htmlwidgets::onRender(p, "
      function(el, x) {
        el.setAttribute('aria-label', 'Scatter plot of Sepal Length vs Sepal Width');
      }
    ")
    p
  })
}
shinyApp(ui, server)
