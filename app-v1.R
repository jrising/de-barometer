## setwd("~/research/coastal/barometer/rshiny")

## Load required packages
library(bslib)
library(bsicons)
library(htmlwidgets)
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(tibble)
library(glue)
library(plotly)
library(leaflet)

source("businesses.R")
source("updatetables.R")
source("timeseries.R")
source("gauge.R")

prep.htf <- function(df2, df2.error) {
    ggplot(df2, aes(year.x)) +
        geom_bar(stat="identity", aes(y=`High Tide Flood Days`, fill=group)) + geom_errorbar(data=df2.error, aes(ymin=lb, ymax=ub), width=.5) +
        theme_bw() +
        labs(title="High Tide Flooding Outlook",
             x=NULL,
             y="High Tide Flooding Days", fill=NULL)

}

## Define UI for the application
html_template <- readLines("businesses-head.html", warn=FALSE)
html_content <- glue_collapse(glue(paste(html_template, collapse="\n"), boxnames=paste(boxnames, collapse=""), boxdata=paste(boxdata, collapse="")), sep="\n")

addResourcePath("static", "static")

custom_theme <- bs_theme(
    preset = "cerulean",
    version = 5,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#0199F8",
    secondary = "#FF374B",
    base_font = "Maven Pro"
)

ui <- fluidPage(
    theme = custom_theme,

    tags$head(tags$script(src="https://www.google.com/jsapi"),
              tags$script(HTML(html_content)),
              tags$script(src="static/businesses.js")),

    includeCSS("static/app.css"),

    ## Application title
    titlePanel("Delaware Coastal Barometer"),

    ## Main panel to display the plot
    mainPanel(
        card(card_header("Top-line Barometer"),
             plotlyOutput("speedometer")),
        card(card_header("High Tide Flood Days"),
             card_body(
                 plotOutput("barPlot"),
                 markdown("From [https://tidesandcurrents.noaa.gov/high-tide-flooding/annual-outlook.html]."))),
        card(card_header("Storm Surge Inundation"),
             card_body(
                 leafletOutput("slosh"),
                 markdown("Hover over the map to see the inundation levels in feet. From [https://www.nhc.noaa.gov/nationalsurge/?text]."))),
        card(card_header("Recent changes"),
             card_body(
                 fluidRow(
                     column(4, plotlyOutput("tsplot.emp")),
                     column(4, plotlyOutput("tsplot.inc")),
                     column(4, plotlyOutput("tsplot.out"))
                 )
             )),
        card(card_header("Coastal Businesses"),
             card_body(
                 HTML('<div id="chart_div" style="width: 100%; height: 400px;"></div>'))),
        card(card_header("Economic Contributions"),
             card_body(
                 selectInput("sector_select",
                             "Economic Sector:",
                             choices = newres2$Sector,
                             selected = "Full Economy"),
                 tableOutput("table")))
    )
)

## Define server logic required to draw the plot
server <- function(input, output) {
    get.htf <- reactive({
        ## Read the data from CSV file
        df <- read_csv("data/htf.csv")

        df$year.x <- as.numeric(substring(df$Year, 1, 4))
        df$group <- trimws(substring(df$Year, 5))
        lb <- df$`High Tide Flood Days`[!is.na(df$group) & df$group == '(projected lower bound)']
        ub <- df$`High Tide Flood Days`[!is.na(df$group) & df$group == '(projected upper bound)']

        df2 <- rbind(subset(df, group == '(observed)')[, c('High Tide Flood Days', 'year.x', 'group')],
                     tibble(`High Tide Flood Days`=(lb + ub)/2,
                            year.x=df$year.x[!is.na(df$group) & df$group == '(projected lower bound)'],
                            group='(projected)'))
        df2.error <- tibble(lb, ub, year.x=df$year.x[!is.na(df$group) & df$group == '(projected lower bound)'])

        list('df2'=df2, 'df2.error'=df2.error)
    })

  ## Generate bar plot
    output$barPlot <- renderPlot({
        df2s <- get.htf()
        prep.htf(df2s$df2, df2s$df2.error)
    })

    output$slosh <- renderLeaflet({
        factored <- factor(c("0 - 1 ft", "1 ft", "3 ft", "7 ft", "10 ft"), levels=c("0 - 1 ft", "1 ft", "3 ft", "7 ft", "10 ft"))
        pal <- colorFactor(
            palette = c('#000000', '#0000FF88', '#00FF00FF', '#FFFF00FF', '#FF0000FF'),
            domain = factored
        )

        leaflet() %>%
            addTiles() %>%
            addTiles(urlTemplate="static/slosh-cat1/{z}/{x}/{y}.png",
                     attribution="Flooding inunation",
                     options=tileOptions(tms=T, tileSize=256, minZoom=4, maxZoom=18)) %>%
            setView(-75.4110209, 38.6882882, zoom=10) %>%
            addLegend(pal=pal, values=factored,
                  title="Inundation (feet)", position="bottomright")
    })

    ## Reactive expression to subset dataframe based on selection
    dataset_to_display <- reactive({
        if (input$sector_select %in% newres2$Sector) {
            row <- newres2[newres2$Sector == input$sector_select,]
        } else {
            row <- df.out2[df.out2$Sector == input$sector_select,]
            Direct.Jobs <- pmax(0, predict(mod.jobs, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Direct.Income <- pmax(0, predict(mod.income, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Direct.Added <- pmax(0, predict(mod.added, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Direct.Output <- pmax(0, predict(mod.output, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Total.Jobs <- pmax(Direct.Jobs, predict(mod.jobstot, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Total.Income <- pmax(Total.Income, predict(mod.incometot, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Total.Added <- pmax(Total.Added, predict(mod.addedtot, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            Total.Output <- pmax(Total.Output, predict(mod.outputtot, data.frame(est.total=row$Establishments, emp.total=row$Employment)))
            ratio <- row$Employment / Direct.Jobs
            row <- data.frame(ratio * Direct.Jobs, ratio * Direct.Income, ratio * Direct.Added, ratio * Direct.Output,
                              ratio * Total.Jobs, ratio * Total.Income, ratio * Total.Added, ratio * Total.Output)
        }
        data.frame(Row=c('Direct', 'Indirect/Induced', 'Total'), Employment=round(c(row$Direct.Jobs, row$Total.Jobs - row$Direct.Jobs, row$Total.Jobs)),
                   `Total Income`=round(c(row$Direct.Income, row$Total.Income - row$Direct.Income, row$Total.Income)),
                   `Added Value`=round(c(row$Direct.Added, row$Total.Added - row$Direct.Added, row$Total.Added)),
                   `Total Output`=round(c(row$Direct.Output, row$Total.Output - row$Direct.Output, row$Total.Output)))
    })

    ## Output the data table
    output$table <- renderTable({
        dataset_to_display()
    })

    tssector <- reactive({
        if (input$sector_select == 'Full Economy' || input$sector_select %in% newres2$Sector) {
            input$sector_select
        } else {
            NA
        }
    })

    for (plotname in names(plot2label)) {
        local({
            myplotname <- plotname
            output[[paste0("tsplot.", myplotname)]] <- renderPlotly({
                print(tssector())
                plot_ly(
                    get.ts(tssector(), plot2label[[myplotname]]),
                    x=~year,
                    y=~scaled,
                    type='bar',
                    text=~paste(round(scaled, 1), units),
                    hoverinfo="text",
                    textposition='none') %>% layout(title=plot2label[[myplotname]])
            })
        })
    }

    output$speedometer <- renderPlotly({
        df2s <- get.htf()
        df.emp <- get.ts(NA, plot2label[['emp']])
        df.inc <- get.ts(NA, plot2label[['inc']])
        df.out <- get.ts(NA, plot2label[['out']])

        get.gauge(list('risk'=list(-df2s$df2$`High Tide Flood Days`),
                       'econ'=list(df.emp$scaled, df.inc$scaled, df.out$scaled)))
    })
}

## Run the application
run_with_themer(shinyApp(ui=ui, server=server))

