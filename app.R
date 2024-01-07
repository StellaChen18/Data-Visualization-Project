library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(maps)
library(geosphere)
library(RColorBrewer)

# Data preparing
## Raw data
restaurant <- read.csv(here::here("data/restaurant/restaurants.csv"))
uber <- read.csv(here::here("data/uber/uberdrive-1.csv"))
uscity <- read.csv(here::here("data/uscities.csv"))

## Data wrangling and cleaning
### restaurant data
dt1 <- restaurant %>%
  select(-position) # Remove position column which is useless
# %>% mutate(type = case_when(price_range = "$" ~ "Inexpensive",price_range = `$$` ~ "Moderately Expensive"))

dt2 <- dt1 %>%
  filter(price_range == "$") %>%
  mutate(type = "Inexpensive")
dt3 <- dt1 %>%
  filter(price_range == "$$") %>%
  mutate(type = "Moderately Expensive")
dt4 <- dt1 %>%
  filter(price_range == "$$$") %>%
  mutate(type = "Expensive")
dt5 <- dt1 %>%
  filter(price_range == "$$$$") %>%
  mutate(type = "Very Expensive")
dt6 <- dt1 %>%
  filter(price_range == "") %>%
  mutate(type = "null")
price_type <- bind_rows(dt2, dt3, dt4, dt5, dt6) # Group different price range and add price type

restaurant_clean <- price_type %>%
  na_if("") %>% # Transform null to NA
  filter(!is.na(score) &
    !is.na(price_range)) %>% # Remove NAs and nulls in score and price_range column
  filter(!score < 2) # Remove outlier

highscore <- restaurant_clean %>%
  filter(score >= 4.5) %>%
  mutate(ranking = "High Score") %>%
  group_by(type, ranking) %>%
  summarise(count = n())

highrating <- restaurant_clean %>%
  filter(ratings >= 150) %>%
  mutate(ranking = "High Rating") %>%
  group_by(type, ranking) %>%
  summarise(count = n())

highscore_rating <- restaurant_clean %>%
  filter(score >= 4.5 & ratings >= 150) %>%
  mutate(ranking = "High Score & Rating") %>%
  group_by(type, ranking) %>%
  summarise(count = n())
popular <- bind_rows(highscore, highrating, highscore_rating) # Group different rating level and add rating

hot <- restaurant_clean %>%
  filter(score >= 4.5 & ratings >= 150) %>%
  arrange(desc(score))

### uber data
eat <- uber %>%
  rename(
    "start" = START_DATE.,
    "end" = END_DATE.,
    "category" = CATEGORY.,
    "departure" = START.,
    "arrive" = STOP.,
    "distance" = MILES.,
    "propose" = PURPOSE.
  ) %>%
  filter(propose == "Meal/Entertain") # Rename and filter the uber eats data

eat1 <- eat %>% summarise(
  starttime =
    mdy_hm(eat$start, quiet = FALSE)
)
eat2 <- eat %>% summarise(
  endtime =
    mdy_hm(eat$end, quiet = FALSE)
) # Transfer start and end time from character to datetime form
eat <- eat %>%
  select(
    -start,
    -end,
    -category
  ) %>%
  bind_cols(eat1, eat2) %>%
  mutate(id = order(eat$distance)) %>%
  arrange(id) %>% # Add order and arrange order from 1 to 157
  select(id, starttime, endtime, departure, arrive, distance, propose)

eat_clean <- eat %>%
  filter(
    !distance == 0,
    !departure == "Unknown Location",
    !arrive == "Unknown Location" # Remove NAs
  )

eatcity <- uscity %>%
  filter(city %in% c(
    "Morrisville", "Cary", "Apex", "Northwoods",
    "Capitol One", "Lahore", "Whitebridge", "Midtown",
    "Midtown East", "Farmington Woods", "Orlando", "Waverly Place",
    "Eastgate", "Storyville", "Westpark Place", "Emeryville",
    "Lexington Park at Amberly", "The Drag", "Edgehill Farms",
    "Hudson Square", "West Palm Beach", "Houston", "North Austin",
    "Tanglewood", "Arabi", "Berkeley", "El Cerrito", "Raleigh",
    "Durham", "Karachi", "Wayne Ridge", "Colombo", "Galveston",
    "Seattle", "Downtown", "Sharpstown", "Seaport", "South Congress",
    "Georgian Acres", "Old City", "West University", "University District",
    "Rose Hill", "Weston", "Fort Pierce", "Savon Height", "Kissimmee",
    "Walnut Terrace", "Faubourg Marigny", "Congress Ave District", "Hazelwood",
    "Lower Manhattan", "Macgregor Downs", "Galveston", "Convention Center District",
    "Coxville", "Oakland", "Nugegoda", "Port Bolivar", "Redmond", "Greater Greenspoint",
    "Gramercy-Flatiron", "Parkway Museums", "Alief", "Soho", "Colombo"
  )) %>%
  select(city, state_id, lat, lng) # Filter eat data with geographic data

eatlocation <- eat_clean %>%
  left_join(eatcity,
    by = c("departure" = "city")
  ) %>%
  left_join(eatcity,
    by = c("arrive" = "city")
  ) %>%
  rename(
    "lat.start" = "lat.x",
    "lng.start" = "lng.x",
    "lat.end" = "lat.y",
    "lng.end" = "lng.y"
  ) %>%
  select(
    starttime, endtime,
    departure, lng.start, lat.start,
    arrive, lng.end, lat.end,
    distance
  ) %>%
  na.omit() # Add latitude and longitude for departure and arrive places

# Define UI for application
ui <- navbarPage("Restaurants near me - Uber Eats",
  theme = shinytheme("united"),
  # Price situation of those popular restaurants
  tabPanel("Price Type Situation",
    icon = icon("dollar-sign"),
    h3("Price situation of popular restaurants"),
    column(
      4,
      offset = 1,
    ),
    radioButtons("rankingtype",
      "Ranking Type:",
      choices = c(
        "High Score", "High Rating",
        "High Score & Rating"
      ),
      selected = NULL
    ),
    plotlyOutput("pricechart", width = "100%", height = "800px"),
    textOutput("txt")
  ),

  # Distribution of popular restaurants
  tabPanel("Restaurants Distribution Map",
    icon = icon("map-location-dot"),
    h3("Distribution of popular restaurants"),
    column(
      6,
      offset = 1,
    ),
    sidebarLayout(
      mainPanel(leafletOutput("leafletmap", height = "1000px")),
      sidebarPanel(selectInput("pricetype",
        "Price Type:",
        choices = c(
          "Inexpensive", "Moderately Expensive",
          "Expensive"
        ),
        multiple = TRUE,
        selected = c("Inexpensive", "Expensive")
      )),
    )
  ),

  # Traveling flow for delicious foods
  tabPanel("Traveling Flow Map",
    icon = icon("route"),
    h3("Traveling flow for delicious foods"),
    column(
      8,
      offset = 1,
    ),
    leafletOutput("flowmap", width = "100%", height = "900px")
  ),
  # About
  tabPanel("About",
    icon = icon("comment"),
    includeMarkdown("README.md")
  ),
  # CSS
  includeCSS("style.css"),
)


# Define server logic
server <- function(input, output) {

  # Part1
  ## Reactive part
  ratp <- reactive({
    popular %>% filter(ranking == input$rankingtype)
  })


  ## Output plotly chart
  output$pricechart <- renderPlotly({
    situation <- ratp() %>%
      ggplot(popular,
        mapping = aes(
          x = type,
          y = count,
          fill = type
        )
      ) +
      geom_col() +
      geom_text(aes(label = count)) +
      theme_bw(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) +
      labs(x = "Type", y = "Number") +
      facet_wrap(~type, scales = "free_y") +
      scale_fill_brewer(palette = "Pastel2")
  })


  ## Output text
  output$txt <- renderText({
    paste("Above figure reveals that whether it's a highly scored or highly rated or both, `Inexpensive` type of restaurants makes up a large portion of the popular restaurants, which is obvious. After all, who doesn't love a good restaurant with a good price?")
  })

  # Part2
  ## Reactive part
  distritype <- reactive({
    hot %>% filter(type %in% input$pricetype)
  })

  ## Output leaflet map
  output$leafletmap <- renderLeaflet({
    distribution <- leaflet() %>%
      addTiles() %>%
      setView(lat = 37.09024, lng = -95.712891, zoom = 5) %>%
      addMarkers(
        data = distritype(),
        lng = ~lng,
        lat = ~lat,
        popup = paste(
          "Restaurant Name: ", hot$name, "<br>",
          "Full Address: ", hot$full_address, "<br>",
          "Price Type: ", hot$type, "<br>",
          "Score: ", hot$score, "<br>",
          "Rating: ", hot$ratings
        ),
        clusterOptions = markerClusterOptions()
      )
  })

  # Part3
  ## Output leaflet flow map
  output$flowmap <- renderLeaflet({
    ## Flow part
    flows <- gcIntermediate(eatlocation[, 4:5], eatlocation[, 7:8], sp = TRUE, addStartEnd = TRUE)
    flows$origins <- eatlocation$departure
    flows$destinations <- eatlocation$arrive
    flows$distance <- eatlocation$distance

    hover <- paste0(
      flows$origins, " to ",
      flows$destinations, ":",
      flows$distance, "miles"
    )

    pal <- colorFactor(brewer.pal(12, "Paired"), flows$origins)

    map <- leaflet() %>%
      addTiles() %>%
      addPolylines(
        data = flows, label = hover,
        group = ~origins,
        weight = 2,
        color = ~ pal(origins)
      ) %>%
      addLayersControl(
        overlayGroups = unique(flows$origins),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
