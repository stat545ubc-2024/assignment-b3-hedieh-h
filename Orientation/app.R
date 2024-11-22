# Back in 2023 I planned an orientation week for my college at UofT. Back then, we payed 5k to create an event app so students can access event information easily. I wanted to challenge myself to make this app using shiny!
# Load the neccessary packages
library(shiny)
library(dplyr)
library(DT)
library(shinyjs)

# Load the orientation schedule dataset
orientation_schedule <- read.csv("data/orientation_schedule.csv")

# UI for the Shiny app
ui <- fluidPage(
  useShinyjs(),  # Enable JavaScript interactions
  titlePanel("Orientation Schedule"), # add title for the page

  # add raw HTML to use CSS style to control the appearance of the app
  tags$style(HTML("
    body {
    background-color: #E3F3FD;
    font-family: Arial, sans-serif;
    }
    h1 {
    color: #4CAF50;
    }
    .event-schedule-box {
    background-color: #f5f5f5; /* Light gray background */
    border: 1px solid #d3d3d3; /* Slightly darker gray border */
    padding: 15px;
    margin-bottom: 20px;
    border-radius: 10px; /* Rounded corners */
  }
  .your-schedule-box {
    background-color: #f5f5f5; /* Light gray background */
    border: 1px solid #d3d3d3; /* Slightly darker gray border */
    padding: 15px;
    margin-bottom: 20px;
    border-radius: 10px; /* Rounded corners */
  }
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

  # create the side bar, including the filtering options and the event map and the event description box
  sidebarLayout(
    sidebarPanel(
      selectInput("day", "Select Day", choices = c("All", unique(orientation_schedule$Weekday))),
      selectInput("category", "Select Category", choices = c("All", unique(orientation_schedule$Category))),
      checkboxInput("registration", "Show events that require registration", value = FALSE), # the checkbox is un-checked by default

      # Add the map image to the side bar
      tags$div(
        id = "map-container",
        img(src = "campus_map.png", id = "map-image"),
        uiOutput("dynamic_markers") # Placeholder for event markers (the event marker location is determined in the server section)
      ),

      # Add the description text box to the side bar
      div(
        class = "description-box",
        h4("Event Description"),
        textOutput("description_text")  # Placeholder for event description (This is replaced with appropriate text when user selects an event)
      )
      ),

    # Create the main panel, containing the event schedule and the user's created schedule
    mainPanel(

      # Make a box for the event schedule
      div(
        class = "event-schedule-box",
        DTOutput("schedule_table")
      ),

      # Make a box for the user's schedule, including a button for downloading the schedule
      div(
        class = "your-schedule-box",  # Add a box around your schedule
        h3("Your Selected Events"),
        DTOutput("selected_events_table"),
        downloadButton("download_schedule", "Download My Schedule")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Feature 1: a table of events that can be filtered based on event category (high energy, low energy, educational, meals), day of the week, and whether the event requires registration or not.
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
      select(Day, Weekday, Event.Name, Category, Time, Duration, Event.Location) # only show the columns relevant to the user
    filtered_data$Add <- sapply(1:nrow(filtered_data), function(i) {
      paste('<button class="add-btn" onclick="Shiny.setInputValue(\'add_event\',', i, ')">Add to Schedule</button>') # make a button to allow users to add the selected event to their schedule
    })
    datatable(
      filtered_data,
      options = list(pageLength = 5, autoWidth = TRUE, # users can see 5 events at a time and can flip to next page is there is more events
      columnDefs = list(list(orderable = FALSE, targets = "_all")) # Disable sorting for all columns
      ),
      selection = 'single', # allow users to only select one row at a time
      escape = FALSE
    )
  })

  # Create a reactive value to store user's selected events
  selected_event <- reactive({
    req(input$schedule_table_rows_selected)
    filtered_schedule()[input$schedule_table_rows_selected, ]
  })

  # Feature 2: an image of the campus map. When the user selects an event in the event table, the location of the event will be indicated on the map using a flag. If the event is happening on campus, no flag will appear and a text box will appear to say the event is off campus.
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

  # Feature 3: place a box in the side bar where the event description would be shown. If no event is selected, a place-holder text will appear to say this is where the event description would show up once an event is selected.
  # Render event description in a text box when an event is clicked
  output$description_text <- renderText({
    event <- selected_event()
    req(nrow(event) > 0)  # Ensure there is a selected event

    # Extract the event description from the dataset
    event_description <- event$Event.Description
    event_description
  })

  # Dynamically update the event description or show a placeholder
  observe({
    event <- selected_event()  # Get the selected event

    # Check if an event is selected
    if (nrow(event) > 0) {

      # If an event is selected, use the event's description
      description_text <- event$Event.Description[1]
    } else {

      # If no event is selected, show the placeholder text
      description_text <- "Select an event to view its description"
    }

    # Update the description text output in the UI
    output$description_text <- renderText({
      description_text
    })
  })

  # Feature 4: "your schedule" table where users can store the events they are interested in. Users can add events to their schedule by clicking a button, and if they change their mind later, they can remove the event from their schedule by clickin the "delete" button. Once they make their schedule, they can download the schedule to their device
  selected_events <- reactiveVal(data.frame())
  observeEvent(input$add_event, {
    selected_row <- input$add_event
    req(selected_row) # Ensure valid input

    # Get the event data for the selected row
    selected_data <- filtered_schedule()[selected_row, ]

    # Check the event description for the selected row
    print(selected_data$Event.Description)

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

     # Only display the relevant columns
    selected_data <- selected_data %>%
      select(Day, Weekday, Event.Name,Category, Time, Duration, Event.Location)

    # Add delete button
     selected_data$Delete <- sapply(1:nrow(selected_data), function(i) {
      paste('<button class="delete-btn" onclick="Shiny.setInputValue(\'delete_event\',', i, ')">Delete</button>')
    })
    # Display the table
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
