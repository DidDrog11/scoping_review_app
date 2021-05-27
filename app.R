#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(snakecase)
library(scales)
library(plotly)
library(tmap)
library(knitr)

# Data stored in www required.
studies <- read_rds("www/studies_app.rds")
studies_all <- read_rds("www/studies_all_app.rds")
spatial <- read_rds("www/spatial_trap_app.rds") %>%
    mutate(lat = sf::st_coordinates(.)[,2], lon = sf::st_coordinates(.)[,1])
species <- read_rds("www/rodent_spatial_app.rds") %>%
    mutate(trap_nights = case_when(trap_nights == 0 ~ "Not reported",
                                   TRUE ~ as.character(trap_nights)))
rodent_summary <- read_rds("www/rodent_species_app.rds")
pathogen <- read_rds("www/pathogen_app.rds") %>%
    mutate(classification = to_sentence_case(classification))
level_0 <- read_rds("www/level_0.rds")
level_2 <- read_rds("www/level_2.rds")
level_2_sites <- read_rds("www/level_2_sites.rds")

# Colour palettes
fact_aim <- colorFactor(palette = c("orange", "purple"), spatial$Aim, na.color = NA, alpha = F)
fact_ap <- colorFactor(palette = c("green", "orange"), species$`Presence/Absence`, na.color = NA, alpha = F)



shinyApp(
    # Define UI for application that draws a histogram
    ui = fluidPage(lang = "en",
                   # # remove shiny "red" warning messages on GUI
                   # tags$style(type="text/css",
                   #            ".shiny-output-error { visibility: hidden; }",
                   #            ".shiny-output-error:before { visibility: hidden; }"
                   # ),

                   # load page layout
                   dashboardPage(

                       skin = "green",

                       dashboardHeader(title = "Rodents and potential zoonotic diseases in West Africa", titleWidth = 600),

                       dashboardSidebar(width = 300,
                                        sidebarMenu(
                                            menuItem("Home", tabName = "home", icon = icon("home")),
                                            menuItem("Included studies", tabName = "studies", icon = icon("table")),
                                            menuItem("General study descriptives", tabName = "study-plots", icon = icon("stats", lib = "glyphicon")),
                                            menuItem("Study trapping locations", tabName = "studytrapmap", icon = icon("map marked alt")),
                                            menuItem("Trap site density", tabName = "densitytrap", icon = icon("globe-africa", lib = "font-awesome")),
                                            menuItem("Species presence and absence", tabName = "speciespresence", icon = icon("map marked alt")),
                                            menuItem("Habitats of trapped species", tabName = "species-plots", icon = icon("stats", lib = "glyphicon")),
                                            menuItem("Microorganisms assayed", tabName = "pathogen", icon = icon("virus", lib = "font-awesome")),
                                            menuItem("Microorganisms detected", tabName = "pathogenpresence", icon = icon("map marked alt")),
                                            menuItem("Rodent species and their microorganisms", tabName = "rodentinfect", icon = icon("paw", lib = "font-awesome"))
                                        ) #end sidebarMenu
                       ), #end dashboardSidebar

                       dashboardBody(
                           tabItems(
                               tabItem(tabName = "home", #home section
                                       includeMarkdown("www/home.Rmd")),
                               tabItem(tabName = "studies", #included studies table
                                       fluidRow(
                                           p("")
                                       ),
                                       fluidRow(
                                           DTOutput("studies") %>%
                                               withSpinner(color = "green"))),
                               tabItem(tabName = "study-plots", #included studies table
                                       fluidRow(
                                           p("There has been an increase in rodent trapping studies over the last 5 decades with trapping occuring in countries such as Senegal and Guinea more frequently than others.")
                                       ),
                                       fluidRow(
                                           splitLayout(
                                               cellWidths = c("50%", "50%"),
                                               plotOutput("publicationyear"),
                                               plotOutput("publicationcountry"))),
                                       fluidRow(
                                           p("Speciation has typically been through morphological measurements, however, more recently molecular tools are increasingly being used to identify trapped rodents to species level. The reporting of trapping effort is particularly helpful when comparing studies to give a measure of likely comprehensiveness of trapping activity at site, here we have categorised incomplete recording as those studies which have not presented trapping effort at the same level at which captures were reported")
                                       ),
                                       fluidRow(
                                           splitLayout(
                                               cellWidths = c("50%", "50%"),
                                               plotOutput("speciation"),
                                               plotOutput("trappingeffort")))
                               ),
                               tabItem(tabName = "studytrapmap", #location of trapping activities
                                       p("This table on the left contains all 124 included studies. The black points on the map are the trapping locations aross all the studies. Selecting a row will plot the trap locations from the selected studies. Selecting a trapping site will produce some further information about the site. Some studies did not report information either coordinates or village names to allow plotting on this map. The points with a darker colour indicate multiple trapping events that are overlayed at a single site."),
                                       fluidRow(
                                           column(12, verbatimTextOutput("selectedstudies"))),
                                       fluidRow(
                                           column(6, DT::dataTableOutput("studytraps")),
                                           column(6, leafletOutput("trapmap", height= 800))
                                       ) %>%
                                           withSpinner(color = "green")), #add comma when uncommenting below
                               tabItem(tabName = "densitytrap", #location of trapping activities
                                       p("This map shows the density of trapping by administrative level 2 in West African countries"),
                                       fluidRow(
                                           tmapOutput("trapdensity", height = 850) %>%
                                               withSpinner(color = "green"))),
                               tabItem(tabName = "speciespresence",
                                       p("This map can be used to visualise the presence and absence of small mammal species derived from all of the included studies. Where absence hasn't be explicitly recorded this is more appropriately descibed as pseudo-absence, this is because absence of a species is imputed from a study where it has been reported elsewhere within the studied area. The appropriateness of this designation is closely linked to the trapping effort made at a site."),
                                       p("All species that have been reported in the included studies are available for visualisation. To focus on a specific or multiple Genus' you can type the name into the box below which will filter the radial options."),
                                       fluidRow(
                                           column(2, selectizeInput(
                                               "genus", "Mammal genus: ",
                                               choices = c(
                                                   "All" = "All",
                                                   species %>%
                                                       tibble() %>%
                                                       mutate(genus = to_sentence_case(genus)) %>%
                                                       arrange(genus) %>%
                                                       distinct(genus) %>%
                                                       pull(genus)),
                                               multiple = T
                                           ))
                                       ),
                                       fluidRow(
                                           column(2,
                                                  radioButtons(
                                                      "selected", "Select species",
                                                      choices = c("None selected" = "",
                                                                  species %>%
                                                                      tibble() %>%
                                                                      group_by(Classification) %>%
                                                                      summarise(number = n()) %>%
                                                                      arrange(-number) %>%
                                                                      pull(Classification)),
                                                      width = "100%"),
                                                  style = "overflow-y:scroll; max-height: 700px; position:relative;"),
                                           column(10,
                                                  leafletOutput("speciesmap", height= 700)))),
                               tabItem(tabName = "species-plots",
                                       p("Different species of rodents are more commonly found in human modified landscapes than others. This plot shows the relative number of individuals trapped in these different categories. Due to how the studies have reported the trapping locations there remains significant uncertainty around the coding of habitats. I discuss this in further detail in the linked manuscript."),
                                       fluidRow(
                                           column(2, selectizeInput(
                                               "genusplot", "Mammal genus: ",
                                               choices = c(
                                                   "All" = "All",
                                                   species %>%
                                                       tibble() %>%
                                                       mutate(genus = to_sentence_case(genus)) %>%
                                                       arrange(genus) %>%
                                                       distinct(genus) %>%
                                                       pull(genus)),
                                               multiple = T,
                                               selected = "All"
                                           ))),
                                       plotOutput("species-plot", height = 700) %>%
                                           withSpinner(color = "green")),
                               tabItem(tabName = "pathogen", #showing plots of trapped individuals
                                       fluidRow(
                                           p("The plot below shows the microorganisms tested for in the included studies. Some microorganisms were assayed at much greater frequency than others. The below plot is displayed on a log scale because of this."),
                                           p("The number of individual rodents tested for each potential pathogen are shown on the orange bar with the number positive shown with the purple bar.")),
                                       fluidRow(
                                           column(2, checkboxGroupInput("pathogenselected", "Select the microorganisms to plot: ",
                                                                        choices = c("Bacteria" = "Bacteria",
                                                                                    "Parasites" = "Parasite",
                                                                                    "Viruses" = "Virus",
                                                                                    "All" = "All"),
                                                                        selected = "All"))
                                       ),
                                       fluidRow(
                                           column(12, plotOutput("pathogenstested", height = 700))
                                       )
                               ),
                               tabItem(tabName = "pathogenpresence",
                                       p("This map can be used to visualise the locations of microrganisms detected in the included studies."),
                                       p("All microorganisms that have been reported in the included studies are available for visualisation. To focus on a specific group (i.e. Viruses, Bacteria) this can be selected to filter the radial options. The points on this map are clustered as multiple species were tested at each geographic point. The colours initially displayed relate to the number of records at a geographic point rather than absence or presence of a microorganism. At the most detailed level the points radiate from the point coordinate with colour relating to thr Absence or Presence of the microorganism."),
                                       fluidRow(
                                           column(2, selectizeInput(
                                               "microorganism", "Microorganism domain: ",
                                               choices = c(
                                                   "All" = "All",
                                                   pathogen %>%
                                                       ungroup() %>%
                                                       distinct(pathogen_group) %>%
                                                       pull(pathogen_group))))),
                                       fluidRow(
                                           column(2,
                                                  radioButtons(
                                                      "selectedmicroorganism", "Select microorganism",
                                                      choices = c("All" = "All",
                                                                  pathogen %>%
                                                                      group_by(pathogen_clean) %>%
                                                                      summarise(number = n()) %>%
                                                                      arrange(-number) %>%
                                                                      pull(pathogen_clean)),
                                                      width = "100%"),
                                                  style = "overflow-y:scroll; max-height: 700px; position:relative;"),
                                           column(10,
                                                  leafletOutput("microorganismmap", height= 700)))),
                               tabItem(tabName = "rodentinfect",
                                       p("This page shows the rodents that have been tested for an individual microorganism. This can be shown either based on the rodent species or microorganism"),
                                       fluidRow(
                                           column(6,
                                                  selectizeInput(
                                                      "mammal_genus", "Mammal genus: ",
                                                      choices = c(
                                                          "All" = "All",
                                                          species %>%
                                                              tibble() %>%
                                                              mutate(genus = to_sentence_case(genus)) %>%
                                                              arrange(genus) %>%
                                                              distinct(genus) %>%
                                                              pull(genus)),
                                                      multiple = T,
                                                      selected = "All")),
                                           column(6,
                                                  selectizeInput(
                                                      "microorganism_domain", "Microorganism domain: ",
                                                      choices = c(
                                                          "All" = "All",
                                                          pathogen %>%
                                                              ungroup() %>%
                                                              distinct(pathogen_group) %>%
                                                              pull(pathogen_group))))),
                                       fluidRow(
                                           column(6,
                                                  checkboxGroupInput(
                                                      "selectedspecies", "Select species of interest",
                                                      choices = c("All" = "All",
                                                                  species %>%
                                                                      tibble() %>%
                                                                      group_by(Classification) %>%
                                                                      summarise(number = n()) %>%
                                                                      arrange(-number) %>%
                                                                      pull(Classification))),
                                                  style = "overflow-y:scroll; max-height: 200px; position:relative;"),
                                           column(6,
                                                  radioButtons("selectedpathogen", "Select microorganism of interest",
                                                      choices = c("All" = "All",
                                                                  pathogen %>%
                                                                      group_by(pathogen_clean) %>%
                                                                      summarise(number = n()) %>%
                                                                      arrange(-number) %>%
                                                                      pull(pathogen_clean))),
                                                  style = "overflow-y:scroll; max-height: 200px; position:relative;")),
                                       fluidRow(
                                           column(8,
                                                  DT::dataTableOutput("species_pathogens")),
                                           column(4,
                                                  leafletOutput("organismmap"))))
                               # tabItem(tabName = "other", #placeholder
                               #         includeMarkdown("www/other.md")
                           ) #end dashboardBody
                       )
                   )
    ), #end dashboardPage
    # end fluidPage

    # Define server logic
    server = function(input, output) {
        # Included studies page
        studytraps <- reactive({ studies })

        output$studies <- renderDT(
            studies %>%
                select(-Country),
            escape = FALSE,
            options = list(pageLength = 10,
                           autoWidth = TRUE,
                           columnDefs = list(list(width = '5px', targets = c(0,1,3)))),
            filter = "top"
        )
        # Study descriptives plots
        output$publicationyear <- renderPlot({
            studies %>%
                ggplot(aes(x = `Year published`, fill = Aim)) +
                geom_bar() +
                scale_fill_manual(values = c("orange", "purple")) +
                theme_minimal() +
                labs(y = "Number of studies published")
        })

        output$publicationcountry <- renderPlot({
            spatial %>%
                tibble() %>%
                distinct(unique_id, country) %>%
                left_join(., studies %>%
                              rename("unique_id" = `Unique ID`), by = "unique_id") %>%
                ggplot(aes(x = fct_rev(fct_infreq(country)), y = `Year published`)) +
                geom_count() +
                coord_flip() +
                theme_minimal() +
                labs(x = "Country")
        })

        output$speciation <- renderPlot({

            studies_all %>%
                mutate(Speciation = factor(case_when(speciation == "morphological, molecular" ~ "Morphological and molecular",
                                                     TRUE ~ to_sentence_case(speciation))),
                       Aim = factor(aim)) %>%
                ggplot(aes(x = Speciation, fill = Aim)) +
                geom_bar() +
                scale_fill_manual(values = c("orange", "purple")) +
                theme_minimal() +
                coord_flip() +
                labs(x = element_blank(),
                     y = "Number of studies",
                     title = "Method of speciation")
        })

        output$trappingeffort <- renderPlot({

            studies_all %>%
                mutate(`Trapping effort` = factor(trapping_effort),
                       Aim = factor(aim)) %>%
                ggplot(aes(x = `Trapping effort`, fill = Aim)) +
                geom_bar() +
                scale_fill_manual(values = c("orange", "purple")) +
                theme_minimal() +
                labs(x = "Reporting of trapping effort",
                     y = "Number of studies")
        })
        # Study trapping locations page
        output$studytraps <- renderDT({
            datatable(studies %>%
                          select(-c(Title, Country, `Journal name`, `Detailed aim 1`, `Detailed aim 2`)),
                      selection = list(target = 'row'),
                      escape = FALSE,
                      options = list(pageLength = 15,
                                     autoWidth = TRUE),
                      filter = "top")
        })

        output$selectedstudies <- renderPrint({
            cat("Selected studies:")
            cat(paste0(studytraps()$`Unique ID`[input$studytraps_rows_selected]), sep = ", ")
        })

        output$trapmap <- renderLeaflet(
            leaflet() %>%
                addTiles() %>%
                setView(lng = 0, lat = 10, zoom = 5) %>%
                addCircleMarkers(data = spatial %>%
                                     distinct(geometry, .keep_all = T),
                                 fillColor = "black",
                                 fillOpacity = 0.3,
                                 stroke = F,
                                 radius = 1,
                                 group = "All studies")

        )

        observe({
            selStudy <- studytraps()$`Unique ID`[input$studytraps_rows_selected]

            mapStudy <- spatial %>%
                filter(unique_id %in% selStudy)

            leafletProxy("trapmap", data = mapStudy) %>%
                clearGroup("Selected studies") %>%
                clearControls() %>%
                addTiles() %>%
                addCircleMarkers(fillColor = ~fact_aim(Aim),
                                 fillOpacity = 0.6,
                                 stroke = F,
                                 radius = 6,
                                 popup = paste0("Study ID: ", mapStudy$unique_id,
                                                "<br>",
                                                mapStudy$`Trap period`,
                                                "<br>",
                                                "Village/Region: ",mapStudy$town_village,
                                                "<br>",
                                                "Number of trap nights: ", mapStudy$trap_nights),
                                 group = "Selected studies") %>%
                addLegend("topright",
                          title = "Aim of the study",
                          pal = fact_aim,
                          opacity = 0.6,
                          values = ~Aim)
        })
        # Trap site density page
        output$trapdensity <- renderTmap({

            tmap_mode("view")
            tmap_options(basemaps = c("OpenStreetMap.Mapnik", "Esri.WorldImagery", "Esri.WorldGrayCanvas"))

            tm_shape(level_0 %>%
                         filter(GID_0 %in% wa_countries)) +
                tm_polygons(alpha = 0, lwd = 1,
                            group = "Country borders") +
                tm_shape(level_2_sites) +
                tm_polygons(col = "site_density", style = "fixed", breaks = c(0, 0.001, 0.005, 0.01, 0.05, 1, 6),
                            palette = "-viridis", colorNA = NULL, border.alpha = 1, border.col = "grey", lwd = 0.1,
                            title = paste("Density of trap sites per 1000 km2"),
                            id = "NAME_2",
                            popup.vars = c("Site density:" = "site_density",
                                           "Studies trapping here:" = "Studies"),
                            group = "Trap density map")

        })
        # Species presence and absence page
        species_choices <- reactive({
            if(input$genus == "All"){
                species %>%
                    tibble() %>%
                    mutate(genus = to_sentence_case(genus)) %>%
                    group_by(Classification) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(Classification)
            } else {
                species %>%
                    tibble() %>%
                    mutate(genus = to_sentence_case(genus)) %>%
                    filter(genus %in% input$genus) %>%
                    group_by(Classification) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(Classification)
            }
        })

        observeEvent(
            input$genus, {
                x <- species_choices()

                updateRadioButtons(inputId = "selected",
                                   choices = x)
            }
        )

        selected_species <- reactive({
            species %>%
                filter(Classification %in% input$selected)
        })

        output$selected <- renderPrint({
            cat("Selected species: ")
            cat(paste0(input$selected))
        })

        output$speciesmap <- renderLeaflet({
            leaflet() %>% addTiles() %>%
                setView(lng = 0, lat = 10, zoom = 5) %>%
                addCircleMarkers(data = spatial %>%
                                     distinct(geometry, .keep_all = T),
                                 fillColor = "black",
                                 fillOpacity = 0.3,
                                 stroke = F,
                                 radius = 1)
        })

        observe({
            selSpecies <- selected_species()

            leafletProxy("speciesmap", data = selSpecies) %>%
                clearGroup("Selected species") %>%
                addCircleMarkers(lng = selSpecies$lon,
                                 lat = selSpecies$lat,
                                 fillColor = selSpecies$pa_colour,
                                 fillOpacity = 0.8,
                                 stroke = F,
                                 radius = 3,
                                 popup = paste0("Study ID: ", selSpecies$unique_id,
                                                "<br>",
                                                "Number of trapped individuals: ", selSpecies$number,
                                                "<br>",
                                                "Village/Region: ",selSpecies$town_village,
                                                "<br>",
                                                "Number of trap nights: ", selSpecies$trap_nights),
                                 group = "Selected species") %>%
                clearControls() %>%
                addLegend("topright",
                          title = "Presence or Absence",
                          colors = c("green", "orange"),
                          labels = c("Absence", "Presence"),
                          opacity = 0.8)
        })

        observe({
            proxy <- leafletProxy("speciesmap", data = species)

            proxy %>%
                clearControls() %>%
                addLegend("topright",
                          title = "Presence or Absence",
                          colors = c("green", "orange"),
                          labels = c("Absence", "Presence"),
                          opacity = 0.8
                )
        })
        # Habitats of trapped species plots
        speciesPlot <- reactive({

            if(length(input$genusplot) == 0) {
                rodent_summary %>%
                    mutate(genus = to_sentence_case(genus)) %>%
                    pull(classification) %>%
                    unique()
            } else if(input$genusplot == "All") {
                rodent_summary %>%
                    mutate(genus = to_sentence_case(genus)) %>%
                    pull(classification) %>%
                    unique()} else {
                        rodent_summary %>%
                            mutate(genus = to_sentence_case(genus)) %>%
                            filter(genus %in% input$genusplot) %>%
                            pull(classification) %>%
                            unique()
                    }

        })

        observe({

            speciesPlot <- rodent_summary %>%
                filter(classification %in% speciesPlot())

            output$`species-plot` <- renderPlot({

                speciesPlot %>%
                    filter(trap_habitat != "NA") %>%
                    mutate(classification = factor(to_sentence_case(classification)),
                           trap_habitat = factor(to_sentence_case(trap_habitat)),
                           genus = factor(genus)) %>%
                    ggplot() +
                    geom_bar(aes(x = fct_rev(fct_infreq(classification)), fill = trap_habitat), position = "dodge") +
                    coord_flip() +
                    theme_minimal() +
                    labs(x = "Genus/Species",
                         y = "Number of individuals trapped",
                         fill = "Habitat type trapped in")
            })
        })
        # Microorganisms assayed page
        pathogensSelected <- reactive({

            if(length(input$pathogenselected) < 1) {
                pathogensSelected <- c("Bacteria", "Virus", "Parasite")
            } else if(input$pathogenselected == "All"){
                pathogensSelected <- c("Bacteria", "Virus", "Parasite")
            } else {
                pathogensSelected <- (input$pathogenselected)
            }

        })

        observe({

            pathogens <- pathogensSelected()

            output$pathogenstested <- renderPlot({

                plot_pathogen <- pathogen %>%
                    filter(pathogen_group %in% pathogens) %>%
                    mutate(pathogen_tested = factor(to_sentence_case(pathogen_tested)),
                           test_positive = factor(case_when(assay = grepl("tested", assay) ~ "Assayed",
                                                            assay = grepl("positive", assay) ~ "Number positive",
                                                            TRUE ~ "Other"))) %>%
                    group_by(pathogen_tested, test_positive) %>%
                    summarise(number = sum(number))

                ggplot(plot_pathogen) +
                    geom_col(aes(x = fct_reorder(pathogen_tested, number), y = number, fill = test_positive),
                             position = "dodge") +
                    scale_y_continuous(trans = "log1p", name = "Number of rodents assayed/number positive on log scale") +
                    scale_fill_manual(values = c("orange", "purple")) +
                    coord_flip() +
                    labs(fill = element_blank(),
                         x = element_blank()) +
                    theme_minimal()

            })
        })
        # Microorganisms detected page
        pathogen_choices <- reactive({
            if(input$microorganism == "All"){
                pathogen %>%
                    group_by(pathogen_clean) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(pathogen_clean)
            } else {
                pathogen %>%
                    filter(pathogen_group %in% input$microorganism) %>%
                    group_by(pathogen_clean) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(pathogen_clean)
            }
        })

        observeEvent(
            input$microorganism, {
                x <- pathogen_choices()

                updateRadioButtons(inputId = "selectedmicroorganism",
                                   choices = c("All" = "All", x))
            }
        )

        selected_microorganisms <- reactive({

            if(input$selectedmicroorganism == "All") {
                pathogen %>%
                    filter(!grepl("tested", assay))
            } else {
                pathogen %>%
                    filter(pathogen_clean %in% input$selectedmicroorganism & !grepl("tested", assay))
            }
        })

        output$selectedmicroorganism <- renderPrint({
            cat("Selected microorganism: ")
            cat(paste0(input$selectedmicroorganism))
        })

        output$microorganismmap <- renderLeaflet({
            leaflet() %>% addTiles() %>%
                setView(lng = 0, lat = 10, zoom = 5) %>%
                addCircleMarkers(data = pathogen %>%
                                     distinct(geometry, .keep_all = T) %>%
                                     st_as_sf(),
                                 fillColor = "black",
                                 fillOpacity = 0.3,
                                 stroke = F,
                                 radius = 1)
        })

        observe({
            selPathogen <- selected_microorganisms()

            leafletProxy("microorganismmap", data = selPathogen %>%
                             filter(!grepl("tested", assay))) %>%
                clearGroup("Selected species") %>%
                addCircleMarkers(lng = selPathogen$lon,
                                 lat = selPathogen$lat,
                                 fillColor = selPathogen$pa_colour,
                                 fillOpacity = 0.8,
                                 stroke = F,
                                 radius = 6,
                                 popup = paste0("Study ID: ", selPathogen$unique_id,
                                                "<br>",
                                                "Rodent species: ", selPathogen$classification,
                                                "<br>",
                                                "Microorganism tested: ", selPathogen$pathogen_clean,
                                                "<br>",
                                                "Number of samples tested: ", selPathogen$samples_tested,
                                                "<br>",
                                                "Number of positive samples: ", selPathogen$number,
                                                "<br>",
                                                "Village/Region: ", selPathogen$town_village,
                                                "<br>",
                                                "Year/month of rodent sampling: ", paste(selPathogen$year_trapping, " - ", selPathogen$month)),
                                 clusterOptions = markerClusterOptions(),
                                 group = "Selected species") %>%
                clearControls() %>%
                addLegend("topright",
                          title = "Presence or Absence",
                          colors = c("#f1a340", "#998ec3"),
                          labels = c("Absence", "Presence"),
                          opacity = 0.8)
        })

        observe({
            proxy <- leafletProxy("microorganismmap", data = pathogen)

            proxy %>%
                clearControls() %>%
                addLegend("topright",
                          title = "Presence or Absence",
                          colors = c("#f1a340", "#998ec3"),
                          labels = c("Absence", "Presence"),
                          opacity = 0.8
                )
        })
        # Rodent species and their microorganisms page
        mammal_pathogen <- reactive({
            if(input$mammal_genus == "All"){
                species %>%
                    tibble() %>%
                    mutate(genus = to_sentence_case(genus)) %>%
                    group_by(Classification) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(Classification)
            } else {
                species %>%
                    tibble() %>%
                    mutate(genus = to_sentence_case(genus)) %>%
                    filter(genus %in% input$mammal_genus) %>%
                    group_by(Classification) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(Classification)
            }
        })

        observeEvent(
            input$mammal_genus, {
                x <- mammal_pathogen()

                updateCheckboxGroupInput(inputId = "selectedspecies",
                                   choices = c("All" = "All", x))
            }
        )

        pathogen_mammal <- reactive({
            if(input$microorganism_domain == "All"){
                pathogen %>%
                    group_by(pathogen_clean) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(pathogen_clean)
            } else {
                pathogen %>%
                    filter(pathogen_group %in% input$microorganism_domain) %>%
                    group_by(pathogen_clean) %>%
                    summarise(number = n()) %>%
                    arrange(-number) %>%
                    pull(pathogen_clean)
            }
        })

        observeEvent(
            input$microorganism_domain, {
                x <- pathogen_mammal()

                updateRadioButtons(inputId = "selectedpathogen",
                                   choices = c("All" = "All", x))
            }
        )

        selected_micro_species <- reactive({

            if (is.null(input$selectedspecies)){
                pathogen
            }else if(input$selectedspecies == "All" & input$selectedpathogen == "All") {
                pathogen
            } else if(input$selectedspecies == "All") {
                pathogen %>%
                    filter(pathogen_clean %in% input$selectedpathogen)
            } else if(input$selectedpathogen == "All") {
                pathogen %>%
                    filter(classification %in% input$selectedspecies)
            } else {
                pathogen %>%
                    filter(pathogen_clean %in% input$selectedpathogen & classification %in% input$selectedspecies)
            }
        })

        output$species_pathogens <- renderDT({
            selected_micro_species() %>%
                    select(unique_id, country, iso3c, classification, pathogen_clean, assay, number, samples_tested) %>%
                    filter(!grepl("tested", assay)) %>%
                    select(-assay) %>%
                    group_by(country, iso3c, classification, pathogen_clean) %>%
                    summarise(`Number of studies` =  n_distinct(unique_id),
                              `Number positive` = sum(number),
                              `Number tested` = sum(samples_tested)) %>%
                    rename("Country" = "country",
                           "ISO3" = "iso3c",
                           "Classification" = "classification",
                           "Microorganism name" = "pathogen_clean") %>%
                ungroup() %>%
                mutate(ID = row_number()) %>%
                select(ID, ISO3, Country, Classification, `Microorganism name`, `Number of studies`, `Number positive`, `Number tested`)

            })

        output$organismmap <- renderLeaflet(
            leaflet() %>%
                addTiles() %>%
                setView(lng = 0, lat = 10, zoom = 4) %>%
                addCircleMarkers(data = spatial %>%
                                     distinct(geometry, .keep_all = T),
                                 fillColor = "black",
                                 fillOpacity = 0.3,
                                 stroke = F,
                                 radius = 1,
                                 group = "All studies")

        )

        observe({

            organismmap_data <- selected_micro_species() %>%
                select(unique_id, country, iso3c, classification, pathogen_clean, assay, number, samples_tested) %>%
                filter(!grepl("tested", assay)) %>%
                select(-assay) %>%
                group_by(country, iso3c, classification, pathogen_clean) %>%
                summarise(`Number of studies` =  n_distinct(unique_id),
                          `Number positive` = sum(number),
                          `Number tested` = sum(samples_tested)) %>%
                rename("Country" = "country",
                       "ISO3" = "iso3c",
                       "Classification" = "classification",
                       "Microorganism name" = "pathogen_clean") %>%
                ungroup() %>%
                mutate(ID = row_number()) %>%
                select(ID, ISO3, Country, Classification, `Microorganism name`, `Number of studies`, `Number positive`, `Number tested`) %>%
                filter(ID %in% input$species_pathogens_rows_selected) %>%
                group_by(Country, ISO3) %>%
                summarise(`Number tested` = sum(`Number tested`)) %>%
                left_join(., level_0 %>%
                              rename("ISO3" = "GID_0"),
                          by = "ISO3") %>%
                st_as_sf()

            leafletProxy("organismmap", data = organismmap_data) %>%
                clearGroup("Selected studies") %>%
                addPolygons(fillColor = organismmap_data$`Number tested`,
                                 group = "Selected studies")
        })
    }
)
