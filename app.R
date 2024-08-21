## setwd("~/research/coastal/barometer/rshiny")

## Load required packages
library(shiny)
library(bs4Dash)
library(readr)
library(ggplot2)
library(tibble)
library(glue)
library(plotly)
library(leaflet)
library(DT)

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

addResourcePath("static", "static")

ui <- dashboardPage(

    dashboardHeader(title="Delaware Coastal Barometer"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Barometer", tabName="barometer", icon=icon("rainbow")),
            menuItem("Economic Contribution", tabName="contribute", icon=icon("industry")),
            menuItem("Coastal Risks", tabName="risks", icon=icon("umbrella")),
            menuItem("Economic Trends", tabName="trends", icon=icon("chart-line")),
            menuItem("About the Barometer", tabName="about", icon=icon("question"))
        )
    ),

    dashboardBody(
        includeCSS("static/app.css"),

        tabItems(
            tabItem(tabName="barometer",
                    box(title="Bottom-Line Coastal Barometer", width=12,
                        markdown("The Bottom-Line Coastal Barometer combines risks and recent trends to estimate the health of the coastal economy of Delaware."),
                        plotOutput("speedometer", height='80%')
                        ),
                    fluidRow(
                        box(title="Coastal Risks", width=6, height=340,
                            markdown("See Coastal Risks tab for more on the environmental risks Delaware's coast faces."),
                            plotOutput("speedometer_risks", height='80%')
                            ),
                        box(title="Economic Trends", width=6, height=340,
                            markdown("See Economic Trends tab for more on the recent performance of Delaware's coast."),
                            plotOutput("speedometer_trends", height='80%')
                            )),
                    ),
            tabItem(tabName="contribute",
                    box(title="Economic Contributions", width=12,
                        markdown("The economic contribution table estimates the direct contribtution of the coastal economy (that is, the employment and economic output of its industries), as well as the indirect and induced contribution throughout the state of Delaware (that is, additional jobs and economic activity that the coast supports."),
                        selectInput("sector_select",
                                    "Economic Sector:",
                                    choices=newres2.contrib$Sector,
                                    selected="Full Economy"),
                        DTOutput("table"))),
            tabItem(tabName="risks",
                    box(title="High Tide Flood Days", width=12,
                        plotOutput("barPlot"),
                        markdown("From the [Annual High Tide Flooding Outlook](https://tidesandcurrents.noaa.gov/high-tide-flooding/annual-outlook.html).")),
                    box(title="Storm Surge Inundation", width=12,
                        leafletOutput("slosh"),
                        markdown("Hover over the map to see the inundation levels in feet. From the [National Storm Surge Risk Maps - Version 3](https://www.nhc.noaa.gov/nationalsurge/?text)."))
                    ),
            tabItem(tabName="trends",
                    selectInput("sector_select2",
                                "Economic Sector:",
                                choices=newres2.trends$Sector,
                                selected="Full Economy"),
                    box(title="Coastal Employment", width=6, plotlyOutput("tsplot.emp")),
                    box(title="Personal Income", width=6, plotlyOutput("tsplot.inc")),
                    box(title="Industry Output", width=6, plotlyOutput("tsplot.out"))
                    ),
            tabItem(tabName="about",
                    box(title="About the Barometer", width=12,
                        markdown("The Coastal Barometer provides a bottom-line estimate of the risks and recent economic trends for Delaware's coastal economy. We use the Coastal Infrastructure Zone definition shown below, drawing upon zip-code level for Sussex County."),
                        img(src='static/Coastal Economies Project Map JR 2024.png', width="70%"))))),

    footer=dashboardFooter(
        left = a(
            href = "https://www.deseagrant.org/",
            target = "_blank", "Delaware Sea Grant"
        ),
        right = "2024"
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
            palette=c('#000000', '#0000FF88', '#00FF00FF', '#FFFF00FF', '#FF0000FF'),
            domain=factored
        )

        leaflet() %>%
            addTiles() %>%
            addTiles(#urlTemplate="static/slosh-cat1/{z}/{x}/{y}.png",
                urlTemplate="http://existencia.org/barometer/slosh-cat1/{z}/{x}/{y}.png",
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
        tibble(Contribution=c('Direct', 'Indirect/Induced', 'Total'), Employment=round(c(row$Direct.Jobs, row$Total.Jobs - row$Direct.Jobs, row$Total.Jobs)),
                   `Total Income`=round(c(row$Direct.Income, row$Total.Income - row$Direct.Income, row$Total.Income) / 1000),
                   `Added Value`=round(c(row$Direct.Added, row$Total.Added - row$Direct.Added, row$Total.Added) / 1000),
                   `Total Output`=round(c(row$Direct.Output, row$Total.Output - row$Direct.Output, row$Total.Output) / 1000))
    })

    ## Output the data table
    output$table <- renderDT({
        datatable(dataset_to_display(),
                  extensions = 'FixedHeader',
                  options = list(
                      paging = FALSE,
                      searching = FALSE,
                      info = FALSE,
                      lengthChange = FALSE,
                      order = list(),  # Disable sorting entirely
                      columnDefs = list(
                          list(targets = "_all", orderable = FALSE),  # Turn off sorting for all columns
                          list(targets = "_all", className = 'dt-center')  # Center-align columns
                      )
                  ),
                  container = htmltools::withTags(table(
                                             class = 'display',
                                             thead(
                                                 tr(
                                                     th(rowspan = 2, colspan=2, ""),
                                                     th(colspan = 1, "Employment"),
                                                     th(colspan = 1, "Total Income"),
                                                     th(colspan = 1, "Added Value"),
                                                     th(colspan = 1, "Total Output")
                                                 ),
                                                 tr(
                                                     th("(people)"),
                                                     th("(1000s USD)"),
                                                     th("(1000s USD)"),
                                                     th("(1000s USD)")
                                                 )
                                             )))
                  ) %>% formatRound(columns = c("Employment", "Total Income", "Added Value", "Total Output"), digits = 0, mark = ",")
    })

    tssector <- reactive({
        if (input$sector_select2 == 'Full Economy' || input$sector_select2 %in% newres2$Sector) {
            input$sector_select2
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
                    textposition='none') %>% layout(yaxis = list(title=plot2label[[myplotname]]),
                                                    xaxis = list(title="Year"))
            })
        })
    }

    output$speedometer <- renderPlot({
        df2s <- get.htf()
        df.emp <- get.ts(NA, plot2label[['emp']])
        df.inc <- get.ts(NA, plot2label[['inc']])
        df.out <- get.ts(NA, plot2label[['out']])

        get.gauge(list('risk'=list(-df2s$df2$`High Tide Flood Days`),
                       'econ'=list(df.emp$scaled, df.inc$scaled, df.out$scaled)))
    })

    output$speedometer_risks <- renderPlot({
        df2s <- get.htf()
        get.gauge(list('risk'=list(-df2s$df2$`High Tide Flood Days`)))
    })

    output$speedometer_trends <- renderPlot({
        df.emp <- get.ts(NA, plot2label[['emp']])
        df.inc <- get.ts(NA, plot2label[['inc']])
        df.out <- get.ts(NA, plot2label[['out']])
        get.gauge(list('econ'=list(df.emp$scaled, df.inc$scaled, df.out$scaled)))
    })


}

## Run the application
shinyApp(ui=ui, server=server)

