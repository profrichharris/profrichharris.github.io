# This checks that the required packages are installed and then loaded
installed <- installed.packages()[,1]
pkgs <- c("shiny", "sf", "dplyr", "ggplot2", "sf", "classInt", "RColorBrewer",
          "ggspatial")
install <- pkgs[!(pkgs %in% installed)]
if(length(install)) install.packages(install, dependencies = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# This downloads the map and the data and merges them
map <- read_sf("https://github.com/profrichharris/profrichharris.github.io/raw/main/MandM/boundary%20files/MDB_Local_Municipal_Boundary_2011.geojson")
mapping_data <- read_csv("https://github.com/profrichharris/profrichharris.github.io/raw/main/MandM/data/education.csv")
map <- left_join(map, mapping_data, by = "LocalMunicipalityCode")

# This selects out from the data the variables of interest
df <- map |>
  st_drop_geometry() |>
  select(where(is.double), -starts_with("Shape"))

# This defines the user interface
ui <- fluidPage(

    # Application title
    titlePanel("Educational geographies for South African municipalities"),

    # Sidebar with various types of input
    sidebarLayout(
        sidebarPanel(
            varSelectInput("var", "Mapping variable", df),
            sliderInput("brks", "Classes", min = 3, max = 8, value = 5, step = 1),
            selectInput("type", "Classification", c("equal", "quantile", "jenks")),
            selectInput("pal", "Palette", rownames(brewer.pal.info)),
            checkboxInput("rev", "Reverse palette"),
            checkboxInput("north", "North arrow")
          ),

        # The main panel with contain the map plot
        mainPanel(
          plotOutput("map")
        )
    )
)

# This defines the server side of the app, taking various inputs
# from the user interface
server <- function(input, output, session) {
  
  output$map <- renderPlot({
    
    vals <- map |>
      pull(!!input$var)
    
    brks <- classIntervals(vals, n = input$brks, style = input$type)$brks
    map$gp <- cut(vals, brks, include.lowest = TRUE)
    
    
    p <- ggplot(map, aes(fill = gp)) +
      geom_sf() +
      scale_fill_brewer("%", palette = input$pal, direction = ifelse(input$rev, 1, -1)) +
      theme_minimal() +
      guides(fill = guide_legend(reverse = TRUE)) +
      labs(caption = "Source: 2011 Census, Statistics South Africa")
    
    if(input$north) p <- p + annotation_north_arrow(location = "tl",
                                              style = north_arrow_minimal(text_size = 14))
  
    p
  
  }, res = 100)}

# This loads and runs the app
shinyApp(ui, server)
