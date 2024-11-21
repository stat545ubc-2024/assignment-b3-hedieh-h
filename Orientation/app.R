library(shiny)
library(dplyr)
library(DT)
library(shinyjs)  # Load shinyjs package

# Load your dataset
orientation_schedule <- read.csv("data/orientation_schedule.csv")

# UI for the Shiny app
ui <- fluidPage(
  useShinyjs(),  # Enable JavaScript interactions
  titlePanel("Orientation Schedule"),
  tags$style(HTML("
    #map-image {
      width: 100%; /* Adjust to the width of the container */
      height: auto; /* Maintain aspect ratio */
      max-width: 1000px; /* Optional: Set maximum width */
      display: block; /* Center the image */
      margin-left: auto; /* Center the image */
      margin-right: auto; /* Center the image */
    }
    #map-container {
      position: relative; /* Parent container for positioning markers */
      width: 100%;
      max-width: 1000px; /* Match the map-image max-width */
      margin: auto; /* Center the container */
    }
    .event-flag {
  position: absolute; /* Absolute positioning for precise placement */
  width: auto; /* Let the size of the content determine the width */
  transform: translate(-50%, -50%); /* Center the flag relative to its coordinates */
  z-index: 1; /* Ensure it stays above the map */
    }
    .off-campus {
      position: absolute;
      font-size: 14px;
      background-color: rgba(0, 0, 0, 0.5);
      color: white;
      padding: 5px;
      border-radius: 5px;
      z-index: 9999; /* Ensure off-campus message is on top */
      transform: translate(-50%, -50%); /* Center it */
    }
    .description-box {
      background-color: #f9f9f9;
      padding: 15px;
      border: 1px solid #ccc;
      border-radius: 5px;
      height: 200px;
      overflow-y: auto;
      margin-top: 20px;
    }
    .description-box h4 {
      margin-top: 0;
    }
    .description-box p {
      font-size: 14px;
      color: #555;
    }
    .placeholder-text {
      color: #888;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      selectInput("day", "Select Day", choices = c("All", unique(orientation_schedule$Weekday))),
      selectInput("category", "Select Category", choices = c("All", unique(orientation_schedule$Category))),
      checkboxInput("registration", "Show events that require registration", value = FALSE),
      tags$div(
        id = "map-container",
        img(src = "campus_map.png", id = "map-image"),
        uiOutput("dynamic_markers") # Placeholder for event markers
      ),
      # Add the description text box here
      div(
        class = "description-box",
        h4("Event Description"),
        textOutput("description_text")  # This is where the description will dynamically appear
      )
      ),
    mainPanel(
      DTOutput("schedule_table"),
      h3("Your Selected Events"),
      DTOutput("selected_events_table"),
      downloadButton("download_schedule", "Download My Schedule")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive expression to filter the schedule based on user input
  filtered_schedule <- reactive({
    schedule <- orientation_schedule
    if (input$registration) {
      schedule <- schedule %>% filter(`Registration.needed` == "Yes")
    }
    if (input$day != "All") {
      schedule <- schedule %>% filter(Weekday == input$day)
    }
    if (input$category != "All") {
      schedule <- schedule %>% filter(Category == input$category)
    }
    return(schedule)
  })

  # Render the filtered schedule table
  output$schedule_table <- renderDT({
    filtered_data <- filtered_schedule() %>%
      select(Day, Weekday, Event.Name, Category, Time, Duration, Event.Location) # Adjust columns as needed

    filtered_data$Add <- sapply(1:nrow(filtered_data), function(i) {
      paste('<button class="add-btn" onclick="Shiny.setInputValue(\'add_event\',', i, ')">Add to Schedule</button>')
    })

    datatable(
      filtered_data,
      options = list(pageLength = 5, autoWidth = TRUE),
      selection = 'single',
      escape = FALSE # Allow HTML for the button
    )
  })

  # Create a reactive value to store selected events
  selected_event <- reactive({
    req(input$schedule_table_rows_selected)
    filtered_schedule()[input$schedule_table_rows_selected, ]
  })

  # Track the map container size dynamically
  observe({
    runjs('
      var mapContainer = document.getElementById("map-container");
      var mapContainerWidth = mapContainer.offsetWidth;
      var mapContainerHeight = mapContainer.offsetHeight;
      Shiny.setInputValue("map_container_size", {width: mapContainerWidth, height: mapContainerHeight});
    ')
  })

  # Generate markers dynamically for the selected event
  output$dynamic_markers <- renderUI({
    event <- selected_event()
    req(nrow(event) > 0)

    if (is.na(event$X) || is.na(event$Y)) {
      # Display "off-campus" message if coordinates are NA
      return(
        tags$div(
          class = "off-campus",
          style = "left: 50%; top: 50%;",
          "Event Location is Off Campus"
        )
      )
    }

    # If coordinates are valid, show the flag marker
    x <- event$X / 1052 * 100
    y <- event$Y / 1133 * 100
    event_name <- event$Event.Name

    req(input$map_container_size)
    map_width <- input$map_container_size$width
    map_height <- input$map_container_size$height

    # Increase scaling factor for the flag size
    flag_width <- min(map_width * 0.05, 100)  # Max flag width
    flag_height <- min(map_height * 0.06, 100)  # Max flag height

    # Render flag marker
    tags$div(
      class = "event-flag",
      style = paste0(
        "left: ", x , "%; ",
        "top: ", y , "%; ",
        "position: absolute; z-index: 1;"
      ),
      tags$img(src = "flag.png", height = flag_height, width = flag_width, title = event_name)
    )
  })
  # Render event description in a text box when an event is clicked
  output$description_text <- renderText({
    event <- selected_event()  # Get the selected event
    req(nrow(event) > 0)  # Ensure there is a selected event

    # Extract the event description from the dataset
    event_description <- event$Event.Description  # Replace with your actual column name
    event_description  # Return the description to be rendered
  })

  # Dynamically update the event description or show a placeholder
  observe({
    event <- selected_event()  # Get the selected event

    # Check if an event is selected
    if (nrow(event) > 0) {
      # If an event is selected, use the event's description
      description_text <- event$Event.Description[1]  # Adjust based on your data
    } else {
      # If no event is selected, show the placeholder text
      description_text <- "Select an event to view its description"
    }

    # Update the description text output in the UI
    output$description_text <- renderText({
      description_text  # This will dynamically update the displayed description
    })
  })

  # Create a reactive value to store all selected events
  selected_events <- reactiveVal(data.frame())

  observeEvent(input$add_event, {
    selected_row <- input$add_event
    req(selected_row) # Ensure valid input


    # Get the event data for the selected row
    selected_data <- filtered_schedule()[selected_row, ]
    # Check the event description for the selected row
    print(selected_data$Event.Description)  # Debugging line

    # Add to the selected events if it's not already in the schedule
    current_selected <- selected_events()
    if (!any(duplicated(rbind(current_selected, selected_data)))) {
      selected_events(rbind(current_selected, selected_data))
    }
  })

  # Render the table of selected events
  output$selected_events_table <- renderDT({
    selected_data <- selected_events()
    req(nrow(selected_data) > 0)
    # Sort the selected data by Day (or any other column like 'Weekday', 'Time', etc.)
    selected_data <- selected_data %>%
      arrange(Day)  # Adjust this if you want to sort by a different column
    selected_data <- selected_data %>%
      select(Day, Weekday, Event.Name,Category, Time, Duration, Event.Location) # Adjust columns as needed
    selected_data$Delete <- sapply(1:nrow(selected_data), function(i) {
      paste('<button class="delete-btn" onclick="Shiny.setInputValue(\'delete_event\',', i, ')">Delete</button>')
    })
    datatable(selected_data, options = list(pageLength = 5, autoWidth = TRUE), escape = FALSE)
  })

  # Remove selected event when "Delete" is clicked
  observeEvent(input$delete_event, {
    current_selected <- selected_events()
    req(nrow(current_selected) > 0)
    row_to_remove <- input$delete_event
    updated_selected <- current_selected[-row_to_remove, ]
    selected_events(updated_selected)
  })

  # Download handler for the selected events
  output$download_schedule <- downloadHandler(
    filename = function() {
      paste("my_schedule_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_events(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
