#
# The user-interface definition of a shiny app object.
#

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

css <- "
.irs-from, .irs-to { color: black; background: transparent }
"

# define several aesthetics
format.sidebar.box.params.main <- list(width = "100%", collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE)
format.sidebar.box.params.sub <- c()

header = dashboardHeader(title = "CoMo Models")

# Interactive params (transmission params)
sidebar = dashboardSidebar(
  # SEIRD
  conditionalPanel(condition="input.simulation == 'SEIRD_params'", 
                   box(title = "Transmission parameters",
                       width= "100%", background = "black",
                       # beta
                       box(title = withMathJax("$$\\beta$$"),
                           id = "seird.params.beta",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "beta", label = withMathJax("$$\\beta$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.800)
                          ),
                       # kappa
                       box(title = withMathJax("$$\\kappa$$"),
                           id = "seird.params.kappa",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "kappa", label = withMathJax("$$\\kappa$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.500)
                          ),
                       # gamma
                       box(title = withMathJax("$$\\gamma$$"),
                           id = "seird.params.gamma",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "gamma", label = withMathJax("$$\\gamma$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.9000)
                          ),
                       # mu
                       box(title = withMathJax("$$\\mu$$"),
                           id = "seird.params.mu",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "mu", label = withMathJax("$$\\mu$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.0050)
                          )
                       )
                   ),
  
  # SEIaImIsRD
  conditionalPanel(condition="input.simulation == 'SEIaImIsRD_params'",
                   box(title = "Transmission parameters",
                       width= "100%", background = "black",
                       # beta
                       box(title = withMathJax("$$\\beta$$"),
                           id = "sc.params.beta",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "sc.beta.ia", label = withMathJax("$$\\beta_{I_a}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.800),
                           sliderInput(inputId = "sc.beta.im", label = withMathJax("$$\\beta_{I_m}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.900),
                           sliderInput(inputId = "sc.beta.is", label = withMathJax("$$\\beta_{I_s}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 1.000)
                       ),
                       # kappa
                       box(title = withMathJax("$$\\kappa$$"),
                           id = "sc.params.kappa",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "sc.kappa", label = withMathJax("$$\\kappa$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.500)
                       ),
                       # omega
                       box(title = withMathJax("$$\\omega$$"),
                           id = "sc.params.omega",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "sc.omega", label = withMathJax("$$\\omega$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.500)
                       ),
                       # p_symptom
                       box(title = withMathJax("$$\\eta$$"),
                           id = "sc.params.p_symptom",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "sc.p_symptom.im", label = withMathJax("$$\\eta_{E->I_m}$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.1400),
                           sliderInput(inputId = "sc.p_symptom.is", label = withMathJax("$$\\eta_{E->I_s}$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.0500)
                       ),
                       # gamma
                       box(title = withMathJax("$$\\gamma$$"),
                           id = "sc.params.gamma",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "sc.gamma.ia", label = withMathJax("$$\\gamma_{I_a}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.9000),
                           sliderInput(inputId = "sc.gamma.im", label = withMathJax("$$\\gamma_{I_m}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.5000),
                           sliderInput(inputId = "sc.gamma.is", label = withMathJax("$$\\gamma_{I_s}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.0500)
                       ),
  
                       # mu
                       box(title = withMathJax("$$\\mu$$"),
                           id = "sc.params.mu",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "sc.mu.ia", label = withMathJax("$$\\mu_{I_a}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.0050),
                           sliderInput(inputId = "sc.mu.im", label = withMathJax("$$\\mu_{I_m}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.0500),
                           sliderInput(inputId = "sc.mu.is", label = withMathJax("$$\\mu_{I_s}$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.3000)
                           )
                       )
                   ),
  # SEIRDAge
  conditionalPanel(condition="input.simulation == 'SEIRDAge_params'",
                   # input contact matrices
                   fileInput(inputId = "age.contact.files", label = "Load contact matrices and population (.rda format)", 
                             accept = ".rda", multiple = TRUE, placeholder = "Please select files all at once"), 
                   # select country from the contact matrix info (allow one only)
                   selectInput(inputId = "age.country", label = "Select country",
                               choices = NULL, multiple = FALSE),
                   # transmission params
                   box(title = "Transmission parameters",
                       width= "100%", background = "black",
                       # beta
                       box(title = withMathJax("$$\\beta$$"),
                           id = "age.params.beta",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "age.beta", label = withMathJax("$$\\beta$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.800)
                       ),
                       # kappa
                       box(title = withMathJax("$$\\kappa$$"),
                           id = "age.params.kappa",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "age.kappa", label = withMathJax("$$\\kappa$$"),
                                       min = 0.000, max = 1.000, step = 0.0001, value = 0.500)
                       ),
                       # gamma
                       box(title = withMathJax("$$\\gamma$$"),
                           id = "age.params.gamma",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "age.gamma", label = withMathJax("$$\\gamma$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.9000)
                       ),
                       # mu
                       box(title = withMathJax("$$\\mu$$"),
                           id = "age.params.mu",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "age.mu", label = withMathJax("$$\\mu$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.0050)
                       )
                   )
                  )
)


body = dashboardBody(
  shinyjs::useShinyjs(),
  # extendShinyjs(text = jscode, functions = c("winprint")),
  
  tabsetPanel(
    id = "simulation",
    # SEIRD
    tabPanel(title="SEIRD", value='SEIRD_params',
             div(class = "col-sm-12 col-md-6 col-lg-auto",
                 box(width = "100%", 
                     title="Model",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                     # actionButton(inputId = "control.params.beta", label = withMathJax("$\\beta$")),
                     tabBox(width = "100%",
                            title = "", 
                            tabPanel(title = "SEIRD Diagram",
                                     h5("Please click on the transmission parameters you want to modify on the diagram below:"),
                                     grVizOutput(outputId = "model_flowchart"),
                                     ),
                            tabPanel(title = "SEIRD ODE system",
                                     fluidPage(uiOutput(outputId = 'dS'),
                                               uiOutput(outputId = 'dE'),
                                               uiOutput(outputId = 'dI'),
                                               uiOutput(outputId = 'dR'),
                                               uiOutput(outputId = 'dD')
                                     )
                            )
                     )
                 ),
                 
                 
                 #actionButton("slider.hide.beta", "hide beta"),
                 box(width = "100%", 
                     title="Simulation result",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
                     plotlyOutput(outputId = "SEIRD")
                 ),
                 box(width = "100%", 
                     title="R0",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                     verbatimTextOutput(outputId = "R0")
                 )
             )
    ),
    
    # SEIaImIsRD
    tabPanel(title="SEIaImIsRD", value='SEIaImIsRD_params',
             div(class = "col-sm-12 col-md-6 col-lg-auto",
                 box(width = "100%", 
                     title="Model",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                     # actionButton(inputId = "control.params.sc.beta", label = withMathJax("$$\\beta$$")),
                     tabBox(width = "100%",
                            title = "", 
                            tabPanel(title = "SEIaImIsRD Diagram",
                                     h5("Please click on the transmission parameters you want to modify on the diagram below:"),
                                     grVizOutput(outputId = "sc_model_flowchart")),
                            tabPanel(title = "SEIaImIsRD ODE system",
                                     fluidPage(uiOutput(outputId = 'sc.dS'),
                                               uiOutput(outputId = 'sc.dE'),
                                               uiOutput(outputId = 'sc.dI'),
                                               uiOutput(outputId = 'sc.dR'),
                                               uiOutput(outputId = 'sc.dD')
                                     )
                            )
                     )
                 ),
                 box(width = "100%", 
                     title="Simulation result",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
                     plotlyOutput(outputId = "SEIaImIsRD")
                 ),
                 box(width = "100%", 
                     title="R0",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                     verbatimTextOutput(outputId = "sc.R0")
                 )
             )
    ),
    
    # SEIRDAge
    tabPanel(title="SEIRDAge", value='SEIRDAge_params',
             div(class = "col-sm-12 col-md-6 col-lg-auto", # row-sm-12 row-md-6 row-lg-auo",
                 box(width = "100%", 
                     title="Files loaded ",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                     verbatimTextOutput(outputId = "age.contact.names")),
                 box(width = "100%", 
                     title="Model",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = TRUE,
                     tabBox(width = "100%",
                            title = "", 
                            tabPanel(title = "SEIRDAge Diagram",
                                     h5("Please click on the transmission parameters you want to modify on the diagram below:"),
                                     grVizOutput(outputId = "age_model_flowchart")),
                            tabPanel(title = "SEIRDAge ODE system",
                                     fluidPage(uiOutput(outputId = 'age.dS'),
                                               uiOutput(outputId = 'age.dE'),
                                               uiOutput(outputId = 'age.dI'),
                                               uiOutput(outputId = 'age.dR'),
                                               uiOutput(outputId = 'age.dD')
                                     )
                            )
                     )
                 ),
                 box(width = "100%", 
                     title="Simulation result by compartment",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
                     plotlyOutput(outputId = "SEIRDAge.by.compartment")
                 ),
                 box(width="100%",
                     title="Simulation result by age",
                     collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
                     selectInput(inputId = "age_range.select", label = "Selected age range",
                                 choices = NULL),
                     plotlyOutput(outputId = "SEIRDAge.by.age")
                 )
             )
    )
  )
)


ui <- dashboardPage(
  title = "CoMo Models",
  header = dashboardHeader(title = "CoMo Models"),
  sidebar = sidebar,
  body = body
)
