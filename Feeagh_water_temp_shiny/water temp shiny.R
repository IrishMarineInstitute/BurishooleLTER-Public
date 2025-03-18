install.packages('rsconnect')
library(rsconnect)


library(shiny)
library(ggplot2)

# GitHub raw CSV links
github_url1 <- "https://raw.githubusercontent.com/RicardoDkIT/Cumm_SWT_forecasts_Lough_Feeagh/refs/heads/main/Last_forecast_generated_Feeagh_Cummulative_SWT.csv"
github_url2 <- "https://raw.githubusercontent.com/RicardoDkIT/Cumm_SWT_forecasts_Lough_Feeagh/refs/heads/main/Past_sonde_observations_since_2023.csv"  # Replace with actual second file URL

# Load data
df1 <- read.csv(url(github_url1))
df2 <- read.csv(url(github_url2))

# Convert date columns
df1$date <- as.Date(df1$Forecast_date, format = "%Y-%m-%d")
df2$date <- as.Date(df2$datetime, format = "%m/%d/%Y")
df1$temperature<-df1$Cummulative_SWT_in_degree_C
df2$temperature<-df2$observation


# UI
ui <- fluidPage(
  titlePanel("Water temperature environment for glochidial drop off"),
  
  # No sidebar, just the main panel for the plots
  mainPanel(
    fluidRow(
      column(6, plotOutput("tempPlot1", height = "400px")),  # First graph
      column(6, plotOutput("tempPlot2", height = "400px"))   # Second graph (last 2 weeks)
    ),
    # Add static text box below the plots
    textOutput("staticText")  # Static text message
  )
)

# Server
server <- function(input, output) {
  
  # Filtered data for full dataset
  filtered_data1 <- reactive({
    df1[df1$date >= min(df1$date) & df1$date <= max(df1$date), ]
  })
  
  # Filter data for the last 2 weeks
  filtered_data2 <- reactive({
    last_date <- max(df2$date)
    df2[df2$date >= (last_date - 14) & df2$date <= last_date, ]
  })
  
  # First plot (full dataset)
  output$tempPlot1 <- renderPlot({
    ggplot(filtered_data1(), aes(x = date, y = temperature)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = 2800, color = "red", linetype = "dashed") +  # Horizontal line
      labs(title = "Forecasted growing degree days", x = "Date", y = "Temperature (°C)") +
      theme_bw()
  })
  
  # Second plot (last 2 weeks)
  output$tempPlot2 <- renderPlot({
    ggplot(filtered_data2(), aes(x = date, y = temperature)) +
      geom_line(color = "red") +
      geom_hline(yintercept = 15, color = "red", linetype = "dashed") +  # Horizontal line
      labs(title = "Temperature (Last 2 Weeks)", x = "Date", y = "Temperature (°C)") +
      theme_bw()
  })
  
  # Output static text box
  output$staticText <- renderText({
    "These two plots indicate the right conditions for glochidial drop-off from fish in the Furance hatchery.\n\n
    The graph on the left shows the forecasted surface water temperature on Lough Feeagh for the next 21 days, 
    converted into growing degree days (GDD) since the 1st September.\n
    The graph on the right shows the surface water temperature over the last 2 weeks.\n
    We think optimal conditions are a GDD > 2800, and about 2 weeks of water temperature > 15°C.\n\n
    These forecasts are produced automatically by Ricardo Marroquín Paíz, using data extracted from our ERDDAP server,
    which allows access to the Feeagh AWQMS data in real time: \n
    https://erddap.marine.ie/erddap/tabledap/IMINewportBuoys.graph"
  })
}

# Run the App
shinyApp(ui = ui, server = server)

