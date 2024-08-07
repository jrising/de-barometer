library(shiny)
library(leaflet)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"),
    tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css")
  ),
  leafletOutput("slosh")
)

server <- function(input, output, session) {
  output$slosh <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -75.4110209, lat = 38.6882882, zoom = 10) %>%
      addTiles(urlTemplate = "slosh-cat1/{z}/{x}/{y}.png", attribution = "Flooding inundation")
  })
}

shinyApp(ui, server)
