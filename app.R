#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

renv::install(packages = c("shiny",
                           "tidyverse",
                           "here",
                           "DT",
                           "countrycode",
                           "leaflet",
                           "leaflet.extras",
                           "shinydashboard",
                           "shinycssloaders",
                           "sf",
                           "snakecase",
                           "scales"))


library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(here)
library(leaflet)
library(snakecase)
library(scales)

studies <- read_rds(here("www", "studies_app.rds"))
studies_all <- read_rds(here("www", "studies_all_app.rds"))
spatial <- read_rds(here("www", "spatial_trap_app.rds")) %>%
    mutate(lat = sf::st_coordinates(.)[,2], lon = sf::st_coordinates(.)[,1])
species <- read_rds(here("www", "rodent_spatial_app.rds"))
rodent_summary <- read_rds(here("www", "rodent_species_app.rds"))
pathogen <- read_rds(here("www", "pathogen_app.rds"))

fact_aim <- colorFactor(palette = c("orange", "purple"), spatial$Aim, na.color = NA, alpha = F)
fact_ap <- colorFactor(palette = c("green", "orange"), species$`Presence/Absence`, na.color = NA, alpha = F)

test <- head(species$Classification, 5)

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
                                            menuItem("Species presence and absence", tabName = "speciespresence", icon = icon("map marked alt")),
                                            menuItem("Habitats of trapped species", tabName = "species-plots", icon = icon("stats", lib = "glyphicon")),
                                            menuItem("Microorganisms assayed", tabName = "pathogen", icon = icon("virus", lib = "font-awesome"))
                                        ) #end sidebarMenu
                       ), #end dashboardSidebar

                       dashboardBody(
                           tabItems(
                               tabItem(tabName = "home", #home section
                                       includeMarkdown("www/home.Rmd")),
                               tabItem(tabName = "studies", #included studies table
                                       DTOutput("studies") %>%
                                           withSpinner(color = "green")),
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
                                           p("The plot on the left shows all the microorganisms tested for in the included studies.")),
                                       fluidRow(
                                           column(2, checkboxGroupInput("pathogenselected", "Select the microorganisms to plot: ",
                                                                        choices = c("Bacteria" = "Bacteria",
                                                                                    "Parasites" = "Parasite",
                                                                                    "Viruses" = "Virus",
                                                                                    "All" = "All"),
                                                                        selected = "All"))
                                       ),
                                       fluidRow(
                                           column(8, plotOutput("pathogenstested", height = 700)),
                                           column(4, DT::dataTableOutput("pathogenstestedtable"))
                                       )
                                       ))
                               # tabItem(tabName = "other", #placeholder
                               #         includeMarkdown("www/other.md")
                       ) #end dashboardBody
                   ) #end dashboardPage
    ), # end fluidPage

    # Define server logic
    server = function(input, output) {

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



    }
)
