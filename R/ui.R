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
  ## ====================== SEIRD ======================= ##
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
  
  ## =================== SEIaImIsRD ===================== ##
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

  ## =================== SEIRD_Age ====================== ##
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
  ),

  ## ===================== SEIRDV ======================= ##
  conditionalPanel(condition="input.simulation == 'SEIRDV_params'",
                   # transmission params
                   box(title = "Transmission parameters",
                       width= "100%", background = "black",
                       # beta
                       box(title = withMathJax("$$\\beta$$"),
                           id = "vac.params.beta",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "vac.beta", label = withMathJax("$$\\beta$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 1.0000)
                       ),
                       # kappa
                       box(title = withMathJax("$$\\kappa$$"),
                           id = "vac.params.kappa",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "vac.kappa", label = withMathJax("$$\\kappa$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.9000)
                       ),
                       # gamma
                       box(title = withMathJax("$$\\gamma$$"),
                           id = "vac.params.gamma",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "vac.gamma", label = withMathJax("$$\\gamma$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.5000)
                       ),
                       # mu
                       box(title = withMathJax("$$\\mu$$"),
                           id = "vac.params.mu",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "vac.mu", label = withMathJax("$$\\mu$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.0100)
                       ),
                       # nu (vac rate)
                       box(title = withMathJax("$$\\nu$$"),
                           id = "vac.params.nu",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "vac.nu", label = withMathJax("$$\\nu$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.4000)
                       ),
                       # delta (rate of immunity loss)
                       box(title = withMathJax("$$\\delta_V$$"),
                           id = "vac.params.delta.v",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "vac.delta.v", label = withMathJax("$$\\delta_V$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.1000)
                       ),
                       box(title = withMathJax("$$\\delta_R$$"),
                           id = "vac.params.delta.r",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "vac.delta.r", label = withMathJax("$$\\delta_R$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.0500)
					   )
                   )
  ),

  ## =================== SEIRD_RU ===================== ##
  conditionalPanel(condition="input.simulation == 'SEIRD_RU_params'",
                   # input contact matrices
                   fileInput(inputId = "ru.contact.files", label = "Load contact matrices and demographic data (.rda format)", 
                             accept = ".rda", multiple = TRUE, placeholder = "Please select files all at once"), 
                   # select country from the contact matrix info (allow one only)
                   selectInput(inputId = "ru.country", label = "Select country",
                               choices = NULL, multiple = FALSE),
                   # transmission params
                   box(title = "Transmission parameters",
                       width= "100%", background = "black",
                       # beta
                       box(title = withMathJax("$$\\beta$$"),
                           id = "ru.params.beta",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "ru.beta", label = withMathJax("$$\\beta$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.3000)
                       ),
                       # kappa
                       box(title = withMathJax("$$\\kappa$$"),
                           id = "ru.params.kappa",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "ru.kappa", label = withMathJax("$$\\kappa$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.2000)
                       ),
                       # gamma
                       box(title = withMathJax("$$\\gamma$$"),
                           id = "ru.params.gamma",
                           width = "100%", background = "black", 
                           collapsible = TRUE, status = "primary", solidHeader = FALSE,  collapsed = TRUE,
                           sliderInput(inputId = "ru.gamma", label = withMathJax("$$\\gamma$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.1000)
                       ),
                       # mu
                       box(title = withMathJax("$$\\mu$$"),
                           id = "ru.params.mu",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "ru.mu", label = withMathJax("$$\\mu$$"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.0300)
                       ),
					   # C (connectedness)
                       box(title = withMathJax("Connectedness"),
                           id = "ru.params.c",
                           width = "100%", background = "black",
                           collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = TRUE,
                           sliderInput(inputId = "ru.c", label = withMathJax("Connectedness"),
                                       min = 0.0000, max = 1.0000, step = 0.0001, value = 0.1000)
                       )
                   )
  )
)


body = dashboardBody(
  shinyjs::useShinyjs(),
  # extendShinyjs(text = jscode, functions = c("winprint")),
  
  tabsetPanel(
    id = "simulation",
    ## ==================== SEIRD ======================== ##
    tabPanel(title="SEIRD", value='SEIRD_params',
             div(class = "col-sm-12 col-md-12 col-lg-10",
				 fluidRow(
					 box(width = 6, 
						 title="Model",
						 collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
						 # actionButton(inputId = "control.params.beta", label = withMathJax("$\\beta$")),
						 box(width = "100%",
							 title = "Diagram",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 h5("Please click on the transmission parameters you want to modify on the diagram below:"),
							 grVizOutput(outputId = "model_flowchart"),
							 verbatimTextOutput(outputId = "SEIRD.param.desc")
						 ),
						 box(width = "100%",
							 title = "SEIRD ODE system",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 fluidPage(uiOutput(outputId = 'dS'),
									   uiOutput(outputId = 'dE'),
									   uiOutput(outputId = 'dI'),
									   uiOutput(outputId = 'dR'),
									   uiOutput(outputId = 'dD')
							 )
						 )
					 ),
					 # actionButton("slider.hide.beta", "hide beta"),
					 box(width = 6, 
						 title="Simulation",
						 collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
						 box(width = "100%",
						     title = "States",
						     collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                             plotlyOutput(outputId = "SEIRD.states")
						 ),
						 box(width = "100%",
							 title = "Daily Incidence and Deaths",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 plotlyOutput(outputId = "SEIRD.changes")
						 ),
						 box(width = "100%",
							 title = "Basic Reproduction Number",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 verbatimTextOutput(outputId = "R0")
						 )
					 )
				 )
			 )
    ),
    
    ## ====================== SEIaImIsRD ============================== ##
    tabPanel(title="SEIaImIsRD", value='SEIaImIsRD_params',
             div(class = "col-sm-12 col-md-12 col-lg-10",
				 fluidRow(
					 box(width = 6, 
						 title="Model",
						 collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
						 # actionButton(inputId = "control.params.sc.beta", label = withMathJax("$$\\beta$$")),
						 box(width = "100%",
							 title = "Diagram",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 h5("Please click on the transmission parameters you want to modify on the diagram below:"),
							 grVizOutput(outputId = "sc_model_flowchart"),
							 verbatimTextOutput(outputId = "SEIaImIsRD.param.desc")
						 ),
						 box(width = "100%",
							 title = "SEIaImIsRD ODE system",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 fluidPage(uiOutput(outputId = 'sc.dS'),
									   uiOutput(outputId = 'sc.dE'),
									   uiOutput(outputId = 'sc.dI'),
									   uiOutput(outputId = 'sc.dR'),
									   uiOutput(outputId = 'sc.dD')
							 )
						 )
					 ),
					 box(width = 6, 
						 title="Simulation",
						 collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
						 box(width = "100%",
						     title = "States",
						     collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                             plotlyOutput(outputId = "SEIaImIsRD.states")
						 ),
						 box(width = "100%",
							 title = "Daily Incidence and Deaths",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 plotlyOutput(outputId = "SEIaImIsRD.changes")
						 ),
						 box(width = "100%",
							 title = "Basic Reproduction Number",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 verbatimTextOutput(outputId = "sc.R0")
						 )
					 )
			     )
             )
    ),
    
    ## ====================== SEIRDAge ============================ ##
    tabPanel(title="SEIRDAge", value='SEIRDAge_params',
             div(class = "col-sm-12 col-md-12 col-lg-10", # row-sm-12 row-md-6 row-lg-auo",
				fluidRow(
					 box(width = 6, 
						 title="Model",
						 collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
						 # actionButton(inputId = "control.params.sc.beta", label = withMathJax("$$\\beta$$")),
						 box(width = "100%",
							 title = "Diagram",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 h5("Please click on the transmission parameters you want to modify on the diagram below:"),
							 grVizOutput(outputId = "age_model_flowchart"),
							 verbatimTextOutput(outputId = "SEIRDAge.param.desc")
						 ),
						 box(width = "100%",
							 title = "SEIRDAge ODE system",
							 collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
							 fluidPage(uiOutput(outputId = 'age.dS'),
                                       uiOutput(outputId = 'age.dE'),
                                       uiOutput(outputId = 'age.dI'),
                                       uiOutput(outputId = 'age.dR'),
                                       uiOutput(outputId = 'age.dD')
							 )
						 ),
						 box(width = "100%", 
							 title="Files loaded ",
                             collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                             verbatimTextOutput(outputId = "age.contact.names")
						 )
					 ),
					 box(width = 6, 
						 title="Simulation",
						 collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
						 box(width = "100%", 
							 title="Result by compartment groups",
                             collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
					         box(width = "100%",
						         title = "States",
						         collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                 plotlyOutput(outputId = "SEIRDAge.states.by.compartment")
							 ),
					         box(width = "100%",
						         title = "Daily Incidence and Deaths",
						         collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                 plotlyOutput(outputId = "SEIRDAge.changes.by.compartment")
					         )
                         ),
						 box(width = "100%", 
							 title="Result by age groups",
                             collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
							 selectInput(inputId = "age_range.select", label = "Selected age range",
                                 choices = NULL),
					         box(width = "100%",
						         title = "States",
						         collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                 plotlyOutput(outputId = "SEIRDAge.states.by.age")
							 ),
					         box(width = "100%",
						         title = "Daily Incidence and Deaths",
						         collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                 plotlyOutput(outputId = "SEIRDAge.changes.by.age")
					         )
                         )
					 )
			    )
             )
    ),


    ## ====================== SEIRDV ============================ ##
    tabPanel(title="SEIRDV", value='SEIRDV_params',
             div(class = "col-sm-12 col-md-12 col-lg-10",
                fluidRow(
                        box(width = 6,
                                title="Model",
                                collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                                box(width = "100%",
                                        title = "Diagram",
                                        collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                        h5("Please click on the transmission parameters you want to modify on the diagram below:"),
                                        grVizOutput(outputId = "vac_model_flowchart"),
                                        verbatimTextOutput(outputId = "SEIRDV.param.desc")
                                ),
                                box(width = "100%",
                                        title = "SEIRDV ODE system",
                                        collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                        fluidPage(uiOutput(outputId = 'vac.dS'),
                                                uiOutput(outputId = 'vac.dE'),
                                                uiOutput(outputId = 'vac.dI'),
                                                uiOutput(outputId = 'vac.dR'),
                                                uiOutput(outputId = 'vac.dV'),
                                                uiOutput(outputId = 'vac.dD')
                                        )
                                )
                        ),
                        box(width = 6,
                                title="Simulation",
                                collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
                                box(width = "100%",
                                    title = "States",
                                    collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
									plotlyOutput(outputId = "SEIRDV.states")
                                ),
                                box(width = "100%",
                                    title = "Daily Incidence and Deaths",
                                    collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                    plotlyOutput(outputId = "SEIRDV.changes")
								)
						)
				)
			 )
    ),

  ## ====================== SEIRD_RU =========================== ##
    tabPanel(title="SEIRD_RU", value='SEIRD_RU_params',
             div(class = "col-sm-12 col-md-12 col-lg-10",
                fluidRow(
                        box(width = 6,
                                title="Model",
                                collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
                                box(width = "100%",
                                    title = "Diagram",
                                    collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                    h5("Please click on the transmission parameters you want to modify on the diagram below:"),
                                    grVizOutput(outputId = "ru_model_flowchart"),
                                    verbatimTextOutput(outputId = "SEIRD_RU.param.desc")
                                ),
                                box(width = "100%",
                                    title = "SEIRD_RU ODE system",
                                    collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                    fluidPage(uiOutput(outputId = 'ru.dS'),
											#uiOutput(outputId = 'ru.dS_Y'),
                                            uiOutput(outputId = 'ru.dE'),
											#uiOutput(outputId = 'ru.dE_Y'),
                                            uiOutput(outputId = 'ru.dI'),
											#uiOutput(outputId = 'ru.dI_Y'),
                                            uiOutput(outputId = 'ru.dR'),
											#uiOutput(outputId = 'ru.dR_Y'),
											#uiOutput(outputId = 'ru.dC_U'),
											#uiOutput(outputId = 'ru.dC_Y'),
                                            uiOutput(outputId = 'ru.dD'),
											#uiOutput(outputId = 'ru.dD_Y')
                                    )
                                ),
								box(width = "100%", 
									title="Files loaded ",
									collapsible = TRUE, status = "primary", solidHeader = TRUE, collapsed = FALSE,
									verbatimTextOutput(outputId = "ru.contact.names")
								)
                        ),
                        box(width = 6,
                                title="Simulation",
                                collapsible = TRUE, status = "primary", solidHeader = TRUE,  collapsed = FALSE,
                                box(width = "100%",
                                    title = "States",
                                    collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
									plotlyOutput(outputId = "SEIRD_RU.states")
                                ),
                                box(width = "100%",
                                    title = "Daily Incidence and Deaths",
                                    collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
                                    plotlyOutput(outputId = "SEIRD_RU.changes")
								),
								box(width = "100%",
									title = "Basic Reproduction Number",
									collapsible = TRUE, status = "primary", solidHeader = FALSE, collapsed = FALSE,
									verbatimTextOutput(outputId = "ru.R0")
								)
						)
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
