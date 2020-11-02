# ----------
# Log-Layout
# ----------


scattr.breaks <- c(0, 2, 4, 6, 8, 10, 12)
scattr.labels <- c("1", "100", "10k", "1M", "100M", "10B", "1T")


# -------------
# Define App UI
# -------------


ui <- fluidPage(

  # Allow for resetting fileInput -> does not reset file1 to NULL!
  shinyjs::useShinyjs(),

  titlePanel(# Header Panel
    title = div(
      img(src = "shinyhex.jpg"),
      HTML("<b> ScattR </b>"),
      style = "font-family: 'times', cursive; font-size:30px; font-weight: 500; line-height: 1.1; color: #2F0B3A;"),
    windowTitle = "Shiny ScattR"
    ),

  sidebarLayout(# Sidebar
    sidebarPanel(
      width = 3,
      tags$head(
        tags$script(# Window resize
          '
          var dimension = [0, 0];
          $(document).on("shiny:connected", function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
          });
          $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
          });
          '
        ),

        tags$style( # Load message
          type="text/css", '
          #loadmessage {
          position: fixed;
          top: 0px;
          left: 0px;
          width: 100%;
          padding: 5px 0px 5px 0px;
          text-align: center;
          font-weight: bold;
          font-size: 100%;
          color: #000000;
          background-color: #FACC2E;
          z-index: 105;
          }
          '
          ,
          ".well {background-color:#FFFFFF; border-color:#FACC2E;}",
          ".shiny-output-error { visibility: hidden; }"
          )
        ),

      # ---
      tags$h6(HTML("<b> <u> Data Selection </b> </u>")),

      fluidRow(# Data selection

        column(# External data set
          width = 7,
          fileInput(
            inputId = "file1",
            label = "Open File (SAS, Stata, CSV)",
            multiple = F,
            accept = c(".sas7bdat", ".csv", ".dta"),
            placeholder = "No file selected"
            )
          ),

        column(# Reset fileInput
          width = 2,
          actionButton(
            inputId = "resetName",
            label = "",
            icon = icon("trash-alt")
            )
          ),

        column(# R data set
          width = 3,
          selectInput(
            inputId = "internal",
            label = "R Data",
            choices = c(... = ".",
                        names(
                          which(sapply(mget(objects(envir = .GlobalEnv), .GlobalEnv), is.data.frame) == T)
                          )
                        )
            )
          )
        ),

      # Align Button with selectInput
      tags$style(type='text/css', "#file1 { width:100%; margin-top: 25px;}"),
      tags$style(type='text/css', "#internal { width:100%; margin-top: 25px;}"),
      tags$style(type='text/css', "#resetName { width:100%; margin-top: 25px;}"),


      # ---
      tags$h6(HTML("<b> <u> Plot Creation </b> </u>")),

      fluidRow(# Plot creation

        column(# Radio buttons
          width = 12,
          radioButtons(
            inputId = "plotchoice",
            label = "",
            choices = c("Off" = "","Standard", "Hierarchy"),
            inline = T
            )
          ),

        column(# Layer 1
          width = 4,
          conditionalPanel( # Conditions for Hierarchy
            condition = "input.plotchoice == 'Hierarchy'",
            selectInput(
              inputId = "layer1",
              label = paste0("Layer 1: ",layer1),
              choices = c("..." = ".")
              )
            )
          ),

        column(# Layer 2
          width = 4,
          conditionalPanel(
              condition = "input.plotchoice == 'Hierarchy' & input.layer1 != '.'",
              selectInput(
                inputId = "layer2",
                label = paste0("Layer 2: ",layer2),
                choices = c("..." = ".")
                )
              )
          ),

        column(# Layer 3
          width = 4,
          conditionalPanel(
            condition = "input.plotchoice == 'Hierarchy' & input.layer1 != '.' & input.layer2 != '.'",
            selectInput(
              inputId = "layer3",
              label = paste0("Layer 3: ",layer3),
              choices = c("..." = ".")
              )
            )
          )

        ),

      tags$hr(),

      # - Header Standard Plot Options
      tags$h6(HTML("<b> <u> Basic Plot Options </b> </u>")),

      fluidRow(# Dynamically update X and Y variable conten after loading data
        column( # X Variable
          width = 6,
          uiOutput("choose_x")
          ),

        column( # Y Variable
          width = 6,
          uiOutput("choose_y")
          )
        ),

      fluidRow(# Standard Options

        column(# Axes scaling
          width = 6,
          radioButtons(
            inputId = "axis",
            label = "Axes Scaling",
            choices = c("Standard", "Logarithmic"),
            selected = "Standard", inline = T
            )
          ),

        column(# Interactive element
          width = 6,
          selectInput(
            inputId = "ia_element",
            label = "> Mouse click",
            choices = c(... = ".")
            )
          )

        ),

      tags$hr(),

      # ---
      tags$h6(HTML("<b> <u> Advanced Plot Options </b> </u>")),

      fluidRow(# Advanced Plot Options

        column(# Color
          width = 6,
          selectInput(
            inputId = "col",
            label = "> Color by",
            choices = c(... = ".")
            )
          ),

        column(# Transparency
          width = 6,
          sliderInput(
            inputId = "alpha",
            min = 0.1, max = 1.0, value = 0.3, step = 0.05,
            label="Color Transparency", ticks = F)
          )

        ),
      fluidRow(

        column( # Group vertical
          width = 6,
          selectInput(
            inputId = "facet_col",
            label = "> Group vertical by",
            choices = c(... = ".")
            )
          ),

        column(# Group horizontal
          width = 6,
          selectInput(
            inputId = "facet_row",
            label = "> Group horizontal by",
            choices = c(... = ".")
            )
          )

        ),

      fluidRow(

        column(# Linear regression
          width = 6,
          radioButtons(
            inputId = "smooth",
            label = "Linear Regression",
            choices = c("Off","On"), selected = "Off",
            inline = T
            )
          ),

        column(# Heatmap
          width = 6,
          radioButtons(
            inputId = "heat",
            label = "Heatmap",
            choices = c("Off","On"), selected = "Off",
            inline = T
            )
          )

        ),

      fluidRow(# Conditional options
        column(# Heatmap: bins
          width = 6,
          conditionalPanel(
            condition = "input.heat == 'On'",
            sliderInput(
              inputId = "bins",
              min = 20, max = 60, value = 30, step = 10,
              label = "Bins", ticks = F
              )
            )
          )

        ),

      tags$hr(),

      # ---
      tags$h6(HTML("<b> <u> Export Option </b> </u>")),

      downloadButton(# Download plot
        outputId = "downloadPlot",
        label = "Download Plot"
        ),

      conditionalPanel(# Loading Screen
        condition="$('html').hasClass('shiny-busy')",
        tags$div("Loading Plot ...", id="loadmessage")
        )

      ),

  # ---

  mainPanel(# Output (Right Window)
    width = 9,

    tabsetPanel(

      type = "tabs",

      tabPanel(# Plot Window

        title = "Plot",

        conditionalPanel(# Standard axes
          condition = "input.axis == 'Standard'",
          plotOutput(
            outputId = "standard.plot",
            width="100%", height = "800",
            click = "plot_click"),
          verbatimTextOutput("standard.info")
          ),

        conditionalPanel(# Log axes
          condition = "input.axis == 'Logarithmic'",
          plotOutput(
            outputId = "log.plot",
            width="100%", height = "800",
            click = "plot_click"),
          verbatimTextOutput("log.info")
          )

        ),

      tabPanel(# Lookup Window
        title = "Lookup",
        DT::dataTableOutput("frame"),
        downloadButton(
          outputId = "download_filtered",
          label = "Download Data"
          )
        )

      )

    )

  )

)



# =============
# Define Server
# =============


server <- function(input, output, session) {

  # Maximum size of dataset
  options(shiny.maxRequestSize=1000*1024^2)

  # fileInput resetter
  observeEvent(input$resetName, {
    shinyjs::reset("file1")
  })

  # --------------------------
  # 1. Generate input data set
  # --------------------------

  scattr.data <- reactive({

    # 1.1 Validation that a dataset must exist
    validate(
      need(!is.null(input$file1) | input$internal != "." , "Please choose the data set first.")
      )


    # 1.2 Upload file via browse button
    if (!is.null(input$file1) & input$internal == ".") {

      # 1.2.1 SAS
      if (str_detect(input$file1$datapath, ".sas7bdat")){
        scattr.data <- haven::read_sas(input$file1$datapath)
      }

      # 1.2.1 Stata
      if (str_detect(input$file1$datapath, ".dta")){
        scattr.data <- haven::read_dta(input$file1$datapath)
      }

      # 1.2.1 CSV-Euro
      if (str_detect(input$file1$datapath, ".csv")){
        scattr.data <- read_delim(input$file1$datapath,
                                  delim =";", locale=locale("de", encoding = "latin1", decimal_mark = ","))
      }

      # 1.2.2 Column guessing
      scattr.data <- scattr:::guess_column(scattr.data)

      # Hierarchies as factors (if defined)
      if (!is.null(layer1)){
        scattr.data <-
          scattr.data %>%
          mutate_at(c(layer1, layer2, layer3), factor)
      }

      # 1.2.3 Force Conversion
      if (!is.null(forced_factor)){
        scattr.data <-
          scattr.data %>%
          mutate_at(forced_factor, as.factor)
      }
      if (!is.null(forced_character)){
        scattr.data <-
          scattr.data %>%
          mutate_at(forced_character, as.character)
      }
      if (!is.null(forced_numeric)){
        scattr.data <-
          scattr.data %>%
          mutate_at(forced_numeric, as.numeric)
      }

    }

    # 1.3 Internal R Object (Tibble/Data Frame)
    # The OR condition is because input$file1 does not reset to NULL after Reset
    if ((is.null(input$file1) | !is.null(input$file1)) & input$internal != "."){

      scattr.data <- as_tibble(get(input$internal))

      # 1.3.1 Guess ID Columns
      scattr.data <- scattr:::guess_id(scattr.data)

      # 1.3.2 Hierarchies as factors (if defined)
      if (!is.null(layer1)){
        scattr.data <-
          scattr.data %>%
          mutate_at(c(layer1, layer2, layer3), factor)
      }

      # 1.3.3 Force Conversion
      if (!is.null(forced_factor)){
        scattr.data <-
          scattr.data %>%
          mutate_at(forced_factor, as.factor)
      }
      if (!is.null(forced_character)){
        scattr.data <-
          scattr.data %>%
          mutate_at(forced_character, as.character)
      }
      if (!is.null(forced_numeric)){
        scattr.data <-
          scattr.data %>%
          mutate_at(forced_numeric, as.numeric)
      }

    }

    # 1.4 Update input fields depending on loaded data set
    updateSelectInput(session,
                      inputId = "x",
                      choices = names(keep(scattr.data,is.numeric)),
                      selected = names(keep(scattr.data,is.numeric))[1])

    updateSelectInput(session,
                      inputId = "y",
                      choices = names(keep(scattr.data,is.numeric)),
                      selected = names(keep(scattr.data,is.numeric))[2])

    updateSelectInput(session,
                      inputId = "col",
                      choices = c(... = ".", names(keep(scattr.data,is.factor))))

    updateSelectInput(session,
                      inputId = "facet_col",
                      choices = c(... = ".", names(keep(scattr.data,is.factor))))

    updateSelectInput(session,
                      inputId = "facet_row",
                      choices = c(... = ".", names(keep(scattr.data,is.factor))))

    updateSelectInput(session,
                      inputId = "ia_element",
                      choices = c(... = ".", names(keep(scattr.data,is.character))))

    updateSelectInput(session,
                      inputId = "layer1",
                      choices = c("..." = ".",levels(scattr:::pull_safely(scattr.data,layer1)$result)))

    updateSelectInput(session,
                      inputId = "layer2",
                      choices = c("..." = ".",levels(scattr:::pull_safely(scattr.data,layer2)$result)))

    updateSelectInput(session,
                      inputId = "layer3",
                      choices = c("..." = ".",levels(scattr:::pull_safely(scattr.data,layer3)$result)))

    # 1.5 Return dataset
    scattr.data

    })


  # --------------------------------------------------
  # 2. Dynamically get content for select input fields
  # --------------------------------------------------

  # 2.1 X
  output$choose_x <- renderUI({


    # get colnames from data
    dat <-  scattr.data()
    colnames <- names(dat)

    # Create the checkboxes and select them all by default
    selectInput("x", "> X Variable", as.list(colnames))
  })

  # 2.2 Y
  output$choose_y <- renderUI({


    # Get the data set with the appropriate name
    dat <-  scattr.data()
    colnames <- names(dat)

    # Create the checkboxes and select them all by default
    selectInput("y", "> Y Variable", as.list(colnames))
  })


  # -------------------------
  # 3. Subset data for ggplot
  # -------------------------


  sub.set <- reactive({

    # 3.1 Standard axis
    if (input$axis == "Standard"){

      # 3.1.1 Load Complete Data
      if (input$layer1 == "." & input$layer2 == "." & input$layer3 == "." &
          input$plotchoice == "Standard") sub.set <- scattr.data()

      # 3.1.2 If value for layer1 is selected
      if (input$layer1 != "." & input$layer2 == "." & input$layer3 == ".") {

        sub.set <- scattr.data()[pull(scattr.data(),layer1) == input$layer1,]

        # Update Layer 2 based on selection of Layer 1
        if (!is.null(layer2)) {
          updateSelectInput(
            session,
            inputId = "layer2",
            choices = c("..." = "." ,
                        levels(
                          droplevels(
                            pull(scattr.data(),layer2)[pull(scattr.data(),layer1) == input$layer1]
                            )
                          )
                        )
          )
        }
      }

      # 3.1.3 If value for layer1 and layer2 is selected
      if (input$layer1 != "." & input$layer2 != "." & input$layer3 == ".")  {

        sub.set <- scattr.data()[pull(scattr.data(),layer1) == input$layer1 &
                                 pull(scattr.data(),layer2) == input$layer2, ]

        # Update Layer 3 Based on selection of Layer 2
        if (!is.null(layer3)) {
          updateSelectInput(
            session,
            inputId = "layer3",
            choices = c("..." = "." ,
                        levels(
                          droplevels(
                            pull(scattr.data(),layer3)[pull(scattr.data(),layer2) == input$layer2])
                          )
                        )
            )
        }

      }

      # 3.1.4 If value for layer1 and layer2 and layer3 is selected
      if (input$layer1 != "." & input$layer2 != "." & input$layer3 != ".")  {

        sub.set <- scattr.data()[pull(scattr.data(),layer1) == input$layer1 &
                                 pull(scattr.data(),layer2) == input$layer2 &
                                 pull(scattr.data(),layer3) == input$layer3,]

        # No updating neccessary

      }


      # 3.1.5 Plot information
      sub.set$Total_Observations <- dim(sub.set)[1]
      sub.set$Plotted_Observations <- dim(sub.set[!is.na(pull(sub.set,input$x)) &
                                                  !is.na(pull(sub.set,input$y)),])[1]
      sub.set$NAs <- dim(sub.set[is.na(pull(sub.set,input$x)) |
                                 is.na(pull(sub.set,input$y)), ])[1]
      sub.set$Zeros <- sum(sub.set[,input$x] == 0, na.rm = T) + sum(sub.set[,input$y] == 0, na.rm=T)
      sub.set$Negatives <- sum(sub.set[,input$x] < 0, na.rm = T) + sum(sub.set[,input$y] < 0, na.rm=T)

    }

    # 3.2 Log axis
    if (input$axis == "Logarithmic"){

      # 3.2.1 Load Complete Data
      if (input$layer1 == "." & input$layer2 == "." & input$layer3 == "." &
          input$plotchoice == "Standard") {

        sub.set <- scattr.data()
        sub.set <- mutate(sub.set, !!input$x := log10(!!as.name(input$x)),
                          !!input$y := log10(!!as.name(input$y)))
      }

      # 3.2.2 If value for layer1 is selected
      if (input$layer1 != "." & input$layer2 == "." & input$layer3 == ".") {

        sub.set <- scattr.data()[pull(scattr.data(),layer1) == input$layer1,]
        sub.set <- mutate(sub.set, !!input$x := log10(!!as.name(input$x)),
                          !!input$y := log10(!!as.name(input$y)))

        # Update Layer 2 based on selection of Layer 1
        if (!is.null(layer2)) {
          updateSelectInput(
            session,
            inputId = "layer2",
            choices = c("..." = "." ,
                        levels(
                          droplevels(
                            pull(scattr.data(),layer2)[pull(scattr.data(),layer1) == input$layer1])
                          )
                        )
          )
        }

      }

      # 3.2.3 If value for layer1 and layer2 is selected
      if (input$layer1 != "." & input$layer2 != "." & input$layer3 == ".")  {

        sub.set <- scattr.data()[pull(scattr.data(),layer1) == input$layer1 &
                                 pull(scattr.data(),layer2) == input$layer2,]
        sub.set <- mutate(sub.set, !!input$x := log10(!!as.name(input$x)),
                          !!input$y := log10(!!as.name(input$y)))

        # Update Layer 3 Based on selection of Layer 2
        if (!is.null(layer3)) {
          updateSelectInput(
            session,
            inputId = "layer3",
            choices = c("..." = "." ,
                        levels(
                          droplevels(
                            pull(scattr.data(),layer3)[pull(scattr.data(),layer2) == input$layer2])
                          )
                        )
            )
        }

      }

      # 3.2.4 If value for layer1 and layer2 and layer3 is selected
      if (input$layer1 != "." & input$layer2 != "." & input$layer3 != ".")  {

        sub.set <- scattr.data()[pull(scattr.data(),layer1) == input$layer1 &
                                 pull(scattr.data(),layer2) == input$layer2 &
                                 pull(scattr.data(),layer3) == input$layer3,]
        sub.set <- mutate(sub.set, !!input$x := log10(!!as.name(input$x)),
                          !!input$y := log10(!!as.name(input$y)))

      }

      # 3.2.5 Plot Information
      sub.set$Total_Observations <- dim(sub.set)[1]
      sub.set$Plotted_Observations <- dim(sub.set[!is.na(pull(sub.set,input$x)) &
                                                    !is.na(pull(sub.set,input$y)) &
                                                    pull(sub.set,input$x) != -Inf &
                                                    pull(sub.set,input$y) != -Inf,])[1]
      sub.set$NAs <- dim(sub.set[is.na(pull(sub.set,input$x)) & !is.nan(pull(sub.set,input$x)) |
                                   is.na(exp(pull(sub.set,input$y))) & !is.nan(pull(sub.set,input$y)),])[1]
      sub.set$Zeros <- sum(exp(sub.set[,input$x]) == 0, na.rm=T) + sum(exp(sub.set[,input$y]) == 0, na.rm=T)
      sub.set$Negatives <- sum(is.nan(pull(sub.set,input$x))) + sum(is.nan(pull(sub.set,input$y)))

    }

    # 3.3 Return
    sub.set

    })


  # --------------------------------------------------
  # 4. Reactive Number of observations in grouped plot
  # --------------------------------------------------


  # 4.1 Standard axes
  nobs <- reactive({

    # 4.1.1 Add facet dimensions
    facets <- paste(input$facet_row, "~", input$facet_col)

    # 4.1.2 Count number of obs. per column dimension
    if (facets != ". ~ ." & input$facet_row == ".") {
      nobs <- sub.set() %>%
        filter(!is.na(!!as.name(input$x)) & !is.na(!!as.name(input$y))) %>%
        group_by(!!as.name(input$facet_col)) %>%
        summarize(n=n())
    }

    # 4.1.3 Count number of obs. per row dimension
    if (facets != ". ~ ." & input$facet_col == ".") {
      nobs <- sub.set() %>%
        filter(!is.na(!!as.name(input$x)) & !is.na(!!as.name(input$y))) %>%
        group_by(!!as.name(input$facet_row)) %>%
        summarize(n=n())
    }

    # 4.1.4 Count number of obs. per column  and row dimension
    if (facets != ". ~ ." & input$facet_col != "." & input$facet_row != ".") {
      nobs <- sub.set() %>%
        filter(!is.na(!!as.name(input$x)) & !is.na(!!as.name(input$y))) %>%
        group_by(!!!list(as.name(input$facet_col), as.name(input$facet_row))) %>%
        summarize(n=n())
    }

    # 4.1.5 Return
    nobs
  })

  # 4.2 Log axes
  nobs_log <- reactive({

    # 4.2.1 Add facet dimensions
    facets <- paste(input$facet_row, "~", input$facet_col)

    # 4.2.2 Count number of obs. per column dimension
    if (facets != ". ~ ." & input$facet_row == ".") {
      nobs <- sub.set() %>%
        filter(!is.na(!!as.name(input$x)) & !is.na(!!as.name(input$y)) &
               !!as.name(input$x) != -Inf & !!as.name(input$y) != -Inf ) %>%
        group_by(!!as.name(input$facet_col)) %>%
        summarize(n=n())
      }

    # 4.2.3 Count number of obs. per row dimension
    if (facets != ". ~ ." & input$facet_col == ".") {
      nobs <- sub.set() %>%
        filter(!is.na(!!as.name(input$x)) & !is.na(!!as.name(input$y)) &
               !!as.name(input$x) != -Inf & !!as.name(input$y) != -Inf ) %>%
        group_by(!!as.name(input$facet_row)) %>%
        summarize(n=n())
      }

    # 4.2.4 Count number of obs. per column  and row dimension
    if (facets != ". ~ ." & input$facet_col != "." & input$facet_row != ".") {
      nobs <- sub.set() %>%
        filter(!is.na(!!as.name(input$x)) & !is.na(!!as.name(input$y)) &
               !!as.name(input$x) != -Inf & !!as.name(input$y) != -Inf ) %>%
        group_by(!!!list(as.name(input$facet_col), as.name(input$facet_row))) %>%
        summarize(n=n())
      }

    # 4.2.5 Return
    nobs
  })


  # -----------------------
  # 5. ggplot standard axes
  # -----------------------


  output$standard.plot <- renderPlot({

    # Standard axes scaling
    if (input$axis == "Standard"){

      # 5.1 Message when no data was loaded
      if (is.null(input$file1) & input$internal == "." ) {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Click browse to open a file or select a dataset from the current R session."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 5.2 Plot choice = "Off"
      if ((!is.null(input$file1) | input$internal != ".") &
          input$plotchoice != "Standard" & input$plotchoice != "Hierarchy") {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Choose between creating a Standard plot or based on a predefined hierarchy.
The option Hierarchy needs at least one layer defined via define_hierarchy()."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 5.3 Radio Button Hierarchy without layer1 selected
      if (input$plotchoice == "Hierarchy" & input$layer1 == ".") {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Select a value for layer 1."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 5.4 Prompt to define_hierarchy
      if (input$plotchoice == "Hierarchy" & input$layer1 == "." & is.null(layer1)) {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="A hierarchical plot requires you to define at least one layer via define_hierarchy()."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 5.5 Hierarchy already selected and switch to "Standard
      if (input$plotchoice == "Standard" & input$layer1 != ".") {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Deselect values from hierarchical plot before creating new standard plot"),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 5.6 Plotten
      else if ( input$plotchoice == "Standard" |
               (input$plotchoice != "Standard" & input$plotchoice == "Hierarchy" & input$layer1 != ".")) {


        # Define facet dimensions
        facets <- paste(input$facet_row, "~", input$facet_col)

        # 5.6.1 Initial Plot
        plot.object <- ggplot(sub.set(), aes_string(x = input$x, y = input$y)) +
          geom_point(alpha=input$alpha, na.rm = T) +
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::comma) +
          labs(title = paste0("Total Number of Observations in Subset: ",
                              format(sub.set()$Total_Observations,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " | Observations Plotted: ",
                              format(sub.set()$Plotted_Observations,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " || NA-Values: ",
                              format(sub.set()$NAs,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " | Zero-Values: ",
                              format(sub.set()$Zeros,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " | Negative-Values: ",
                              format(sub.set()$Negatives,
                                     nsmall=0, big.mark=".", decimal.mark = ","))) +
          theme_bw() +
          theme(panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size = 11),
                legend.title = element_text(size = 11),
                strip.text.x = element_text(size = 9), # Facet Col
                strip.text.y= element_text(size = 9)) # Facet Row

        # 5.6.2 Add color dimension
        if(input$col != ".") {
          plot.object <- plot.object +
            geom_point(alpha = input$alpha, aes_string(col = input$col), na.rm = T) +
            labs(color ="Color Key:")
        }

        # 5.6.3 Add heatmap
        if(input$heat == "On"){
          plot.object <- ggplot(sub.set(), aes_string(x = input$x, y = input$y)) +
            geom_bin2d(bins = input$bins, na.rm = T) +
            scale_fill_continuous(name = "Number of Observations in Rectangle: ") +
            scale_x_continuous(labels = scales::comma) +
            scale_y_continuous(labels = scales::comma) +
            labs(title = paste0("Total Number of Observations in Subset: ",
                                format(sub.set()$Total_Observations,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " | Observations Plotted: ",
                                format(sub.set()$Plotted_Observations,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " || NA-Values: ",
                                format(sub.set()$NAs,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " | Zero-Values: ",
                                format(sub.set()$Zeros,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " | Negative-Values: ",
                                format(sub.set()$Negatives,
                                       nsmall=0, big.mark=".", decimal.mark = ","))) +
            theme_bw() +
            theme(panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = "right",
                  legend.text = element_text(size = 11),
                  legend.title = element_text(size = 11),
                  strip.text.x = element_text(size = 9), # Facet Col
                  strip.text.y= element_text(size = 9)) # Facet Row
        }

        # 5.6.4 Add Smoother
        if(input$smooth == "On"){
          plot.object <- plot.object +
            geom_smooth(
              method = "lm",
              colour = "black",
              na.rm = T)
        }


        # 5.6.5.1 Generate number of obs. per column dimension
        if (facets != ". ~ ." & input$facet_row == ".") {

          plot.object <- plot.object +
            facet_grid(facets, drop = T) +
            geom_text(data = nobs(),
                      aes(x = quantile(pull(sub.set(),input$x), na.rm = T, probs = 0.999),
                          y = scattr:::roundUp(max(sub.set()[, input$y],na.rm = T)),
                          label = format(n, nsmall = 0, big.mark = ".", decimal.mark = ",")),
                      colour = "black", size = 4)
        }

        # 5.6.5.2 Generate number of obs. per row dimension
        if (facets != '. ~ .' & input$facet_col == ".") {

          plot.object <- plot.object +
            facet_grid(facets, drop = T) +
            geom_text(data = nobs(),
                      aes(x = quantile(pull(sub.set(),input$x), na.rm = T, probs = 0.999),
                          y = scattr:::roundUp(max(sub.set()[, input$y],na.rm = T)),
                          label = format(n, nsmall = 0, big.mark = ".", decimal.mark = ",")),
                      colour = "black", size = 4)
        }

        # 5.6.5.3 Generate number of obs. per column and row dimension
        if (facets != '. ~ .' & input$facet_col != "." & input$facet_row != ".") {

          plot.object <- plot.object + facet_grid(facets, drop = T) +
            geom_text(data=nobs(),
                      aes(x = quantile(pull(sub.set(),input$x), na.rm = T, probs = 0.999),
                          y = scattr:::roundUp(max(sub.set()[, input$y],na.rm = T)),
                          label=format(n, nsmall = 0, big.mark = ".", decimal.mark = ",")),
                      colour = "black", size = 4)
        }

      }

      # 5.6.6 Ausgabe
      plot.object

    }
  })

  # ------------------
  # 6. ggplot log axes
  # ------------------


  output$log.plot <- renderPlot({

    if (input$axis == "Logarithmic") {

      # 6.1 Message when no data was loaded
      if (is.null(input$file1) & input$internal == "." ) {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Click browse to open a file or select a dataset from the current R session."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 6.2 Plot choice = "Off"
      if ((!is.null(input$file1) | input$internal != ".") &
          input$plotchoice != "Standard" & input$plotchoice != "Hierarchy") {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Choose between creating a Standard plot or based on a predefined hierarchy.
              The option Hierarchy needs at least one layer defined via define_hierarchy()."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 6.3 Radio Button Hierarchy without layer1 selected
      if (input$plotchoice == "Hierarchy" & input$layer1 == ".") {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Select a value for layer 1."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 6.4 Prompt to define_hierarchy
      if (input$plotchoice == "Hierarchy" & input$layer1 == "." & is.null(layer1)) {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="A hierarchical plot requires you to define at least one layer via define_hierarchy()."),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 6.5 Hierarchy already selected and switch to "Standard
      if (input$plotchoice == "Standard" & input$layer1 != ".") {

        plot.object <- ggplot(tibble(a=c(1), b=c(1)), aes(x = a, y = b)) +
          geom_text(
            data=tibble(
              text="Deselect values from hierarchical plot before creating new standard plot"),
            aes(x=1, y=1, label=text), colour="black", size=10) +
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.background = element_rect(fill = "white", colour = "grey50"))

      }

      # 6.6 Plotten
      else if ( input$plotchoice == "Standard" |
                (input$plotchoice != "Standard" & input$plotchoice == "Hierarchy" & input$layer1 != ".")) {

        # Define facet dimensions
        facets <- paste(input$facet_row, "~", input$facet_col)

        # Standard (Frame) Plot
        plot.object <- ggplot(sub.set(), aes_string(x = input$x, y = input$y)) +
          geom_point(alpha=input$alpha, na.rm = T) +
          scale_x_continuous(breaks = scattr.breaks,
                             labels = scattr.labels,
                             limits = c(scattr:::roundDown(min(sub.set()[, input$x],na.rm=T)),
                                        scattr:::roundUp(max(sub.set()[, input$x], na.rm=T)))) +
          scale_y_continuous(breaks = scattr.breaks,
                             labels = scattr.labels,
                             limits = c(scattr:::roundDown(min(sub.set()[, input$y],na.rm=T)),
                                        scattr:::roundUp(max(sub.set()[, input$y], na.rm=T)))) +
          labs(title = paste0("Total Number of Observations in Subset: ",
                              format(sub.set()$Total_Observations,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " | Observations Plotted: ",
                              format(sub.set()$Plotted_Observations,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " || NA-Values: ",
                              format(sub.set()$NAs,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " | Zero-Values: ",
                              format(sub.set()$Zeros,
                                     nsmall=0, big.mark=".", decimal.mark = ","),
                              " | Negative-Values: ",
                              format(sub.set()$Negatives,
                                     nsmall=0, big.mark=".", decimal.mark = ","))) +
          theme_bw() +
          theme(panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size = 11),
                legend.title = element_text(size = 11),
                strip.text.x = element_text(size = 9),
                strip.text.y= element_text(size = 9))

        # Add color dimension

        if(input$col != ".") {
          plot.object <- plot.object +
            geom_point(alpha=input$alpha, aes_string(col = input$col), na.rm = T) +
            labs(color ="Key:")
        }

        # Add heatmap
        if(input$heat == "On"){

          plot.object <- ggplot(sub.set(), aes_string(x = input$x, y = input$y)) +
            geom_bin2d(bins=input$bins, na.rm = T) +
            scale_fill_continuous(name="Number of Observations in Rectangle: ") +
            scale_x_continuous(breaks = scattr.breaks,
                               labels = scattr.labels,
                               limits = c(scattr:::roundDown(min(sub.set()[, input$x],na.rm=T)),
                                          scattr:::roundUp(max(sub.set()[, input$x], na.rm=T)))) +
            scale_y_continuous(breaks = scattr.breaks,
                               labels = scattr.labels,
                               limits = c(scattr:::roundDown(min(sub.set()[, input$y],na.rm=T)),
                                          scattr:::roundUp(max(sub.set()[, input$y], na.rm=T)))) +
            labs(title = paste0("Total Number of Observations in Subset: ",
                                format(sub.set()$Total_Observations,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " | Observations Plotted: ",
                                format(sub.set()$Plotted_Observations,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " || NA-Values: ",
                                format(sub.set()$NAs,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " | Zero-Values: ",
                                format(sub.set()$Zeros,
                                       nsmall=0, big.mark=".", decimal.mark = ","),
                                " | Negative-Values: ",
                                format(sub.set()$Negatives,
                                       nsmall=0, big.mark=".", decimal.mark = ","))) +
            theme_bw() +
            theme(panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = "right",
                  legend.text = element_text(size = 11),
                  legend.title = element_text(size = 11),
                  strip.text.x = element_text(size = 9),
                  strip.text.y= element_text(size = 9))

        }

        # Add Smoother
        if(input$smooth == "On"){
          plot.object <- plot.object +
            geom_smooth(
              method = "lm",
              colour = "black",
              na.rm = T)
        }


        # Count number of obs. per column dimension
        if (facets != ". ~ ." & input$facet_row == ".") {

          plot.object <- plot.object +
            facet_grid(facets, drop = T) +
            geom_text(data=nobs_log(),
                      aes(x=mean(pull(sub.set(),input$x)[is.finite(pull(sub.set(),input$x))]),
                          y=scattr:::roundUp(max(sub.set()[, input$y],na.rm=T)),
                          label=format(n, nsmall=0, big.mark=".", decimal.mark = ",")),
                      colour="black", size=4)
        }

        # Count number of obs. per row dimension
        if (facets != ". ~ ." & input$facet_col == ".") {

          plot.object <- plot.object + facet_grid(facets, drop = T) +
            geom_text(data=nobs_log(),
                      aes(x=mean(pull(sub.set(),input$x)[is.finite(pull(sub.set(),input$x))]),
                          y=scattr:::roundUp(max(sub.set()[, input$y],na.rm=T)),
                          label=format(n, nsmall=0, big.mark=".", decimal.mark = ",")),
                      colour="black", size=4)
        }

        # Count number of obs. per column and row dimension
        if (facets != ". ~ ." & input$facet_col != "." & input$facet_row != ".") {

          plot.object <- plot.object + facet_grid(facets, drop = T) +
            geom_text(data=nobs_log(),
                      aes(x=mean(pull(sub.set(),input$x)[is.finite(pull(sub.set(),input$x))]),
                          y=scattr:::roundUp(max(sub.set()[, input$y],na.rm=T)),
                          label=format(n, nsmall=0, big.mark=".", decimal.mark = ",")),
                      colour="black", size=4)
        }

      }

      # Ausgabe
      plot.object
    }

  })



  # ------------
  # 7. Info Text
  # ------------

  # 7.1 Standard
  output$standard.info <- renderPrint({

    if ( (input$plotchoice == "Standard" | (input$plotchoice == "Hierarchy" & input$layer1 != ".") ) &
         input$ia_element != "."){

      click_location <- select(sub.set(), input$ia_element, input$x, input$y)

      if (input$facet_row != ".") click_location <- select(sub.set(), input$ia_element,
                                                           input$x, input$y, input$facet_row)
      if (input$facet_col != ".") click_location <- select(sub.set(), input$ia_element,
                                                           input$x, input$y, input$facet_col)
      if (input$facet_row != "." & input$facet_col != ".") {
        click_location <- select(sub.set(), input$ia_element,
                                 input$x, input$y, input$facet_row, input$facet_col)
      }

      nearPoints(click_location, input$plot_click, addDist = T, threshold = 2)

    }

  })

  # 7.2 Log
  output$log.info <- renderPrint({

    if ( (input$plotchoice == "Standard" | (input$plotchoice == "Hierarchy" & input$layer1 != ".") ) &
         input$ia_element != "."){

      click_location <- select(sub.set(), input$ia_element, input$x, input$y)

      if (input$facet_row != ".") click_location <- select(sub.set(), input$ia_element, input$x, input$y, input$facet_row)
      if (input$facet_col != ".") click_location <- select(sub.set(), input$ia_element, input$x, input$y, input$facet_col)
      if (input$facet_row != "." & input$facet_col != ".") {
        click_location <- select(sub.set(), input$ia_element, input$x, input$y, input$facet_row, input$facet_col)
      }

      #  click_location <- mutate(click_location, !!input$x := log10(!!as.name(input$x)),
      #                  !!input$y := log10(!!as.name(input$y)))

      nearPoints(click_location, input$plot_click, addDist = T, threshold = 2)

    }

  })


  # ----------------
  # 8. Look-Up Table
  # ----------------

  output$frame <- DT::renderDataTable({

    if (!is.null(input$file1) | input$internal != "."){

      if (input$ia_element != ".") {
        sub.set() %>%
          select(input$ia_element, input$x, input$y, everything())
      }

      else sub.set() %>%
        select(input$x, input$y, everything())
      }

    })



  # -----------
  # 9. Download
  # -----------

  # 9.1 Plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(Sys.time(),".png", sep="") },
    content = function(file) {
      ggsave(file, device = "png", dpi = 300, units = "cm", width = 30, height = 15, scale = 1.5)
    }
  )

  # 9.2 Data from Look-up
  output$download_filtered <-
    downloadHandler(
      filename = "Filtered Data.csv",
      content = function(file){
        write.table(sub.set()[input[["frame_rows_all"]], ],
                    file, sep = ";", dec=",", row.names = F)
      }
    )

  # Terminate
  session$onSessionEnded(stopApp)
}


# -------
# Run App
# -------


shinyApp(ui = ui, server = server)


