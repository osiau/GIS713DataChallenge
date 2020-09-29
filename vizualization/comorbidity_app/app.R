library(sp)
library(rgdal)
library(viridis)
library(shiny)
source("comorbidity_clean.R")

states_loc <- "cb_2018_us_state_20m/cb_2018_us_state_20m.shp"
states <- readOGR(states_loc)

comorbidity_names <- unique(comorbidity_clean$condition)

statedata <- merge(x=states,y=comorbidity_clean[age_group == "All ages"],by.x='STATEFP',by.y='stateFIPS', duplicateGeoms = T)

breaks <- seq(0, 1, by=0.1)
statedata$color_index <- findInterval(statedata$pct_with_comorbidity, breaks, all.inside=T)

pal <- colorRampPalette(rev(magma(10)))
cols <- pal(length(breaks - 1))


# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Pct. of COVID Deaths with Comorbidity"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("comorbidity_name", "Condition:", 
                  choices=comorbidity_names),
      hr(),
      helpText("Data from CDC.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("comorbidityMap")  
    )
    
  )
)

# Define a server for the Shiny app
server <- function(input, output) {
  
  
  # Fill in the spot we created for a plot
  output$comorbidityMap <- renderPlot({
    comorbidity_selected <- spTransform(statedata[statedata$condition == input$comorbidity_name
                                                  & is.na(statedata$pct_with_comorbidity)==0,],CRS("+init=epsg:2163"))
    # Render a map
    plot(comorbidity_selected,col=cols[comorbidity_selected$color_index],xlim=c(-2.45e6,2.15e6),ylim=c(-2.1e6,6.5e5),
         main=paste("% of COVID Deaths with",input$comorbidity_name))
    labels <- paste(round(breaks * 100)[1:(length(breaks) - 1)], round(breaks * 100)[2:length(breaks)], sep="-")
    legend("bottomleft", legend=labels, fill=cols)
  })
}

shinyApp(ui=ui,server=server)