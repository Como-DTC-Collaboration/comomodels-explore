#
# The server definition of a shiny app object.
#
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  #%%%%%%%%%
  # SEIRD
  #%%%%%%%%%
  # reactive buttons for modifying transmission params
  # observeEvent(input$control.params.beta, {
  #   shinyjs::toggle(id="seird.params.beta")
  # })

  # model diagram
  output$model_flowchart <- renderGrViz(
    
    DiagrammeR::grViz("
    digraph seird {
    
      # initiate graph
      graph [layout = dot, rankdir = LR]
      
      # global node settings
      node [shape = box, fontname = Helvetica, fixedsize = true, 
            style = filled, fillcolor = lightgray, width = 0.6]
      S;E;I;R;D

      node [shape = circle, fillcolor = lightblue, width = 0.3]
      beta[label = <&#946;>];
      kappa[label = <&#954;>];
      mu[label = <&#956;>]; 
      gamma[label =  <&#947;>];
      
      # edge definitions with the node IDs
      edge [arrowhead=none]
      S -> beta
      E -> kappa
      I -> {mu gamma}
      edge [arrowhead=vee]
      beta -> E
      kappa -> I
      mu -> D
      gamma -> R 
      }") 
  )

  observe({
    req(input$model_flowchart_click)
    nodeid <- input$model_flowchart_click$id[[1]]
	nodeid.int <- strsplit(nodeid, "node")[[1]][2] %>% as.integer()
	nodeval <- input$model_flowchart_click$nodeValues
	desc <- c("Susceptible", "Exposed", "Infected", "Recovered (Removed)", "Mortality",
	          "the rate at which an infected individual exposes susceptible", 
			  "the rate of progression from exposed to infectious (reciprocal of the incubation period)",
			  "the rate of disease-caused mortality of the infected",
			  "the rate of removal (i.e. the recovery rate of the infected)")
	trans.params <- c("beta", "kappa", "mu", "gamma")
	
	output$SEIRD.param.desc <- renderText({paste0(nodeval[[length(nodeval)]], ": ", desc[nodeid.int])})
	if(nodeid.int %in% seq(6, 9)){
	  shinyjs::toggle(id=paste0("seird.params.", trans.params[nodeid.int - 5]))
	}
  })
 
  # ODE system
  output$dS <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}S}{\\text{d}t} = -S \\beta I$$"))
  })
  
  output$dE <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}E}{\\text{d}t} = S \\beta I - \\kappa E$$"))
  })
  
  output$dI <- renderUI({
    withMathJax(
      helpText("$$\\frac{dI}{dt} = \\kappa E - (\\gamma + \\mu)I$$"))
  })
  
  output$dR <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}R}{\\text{d}t} = \\gamma I$$"))
  })
  
  output$dD <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}D}{\\text{d}t} = \\mu I$$"))
  })
  
  # model initialization
  model0 <- reactive({
    beta <- input$beta
    kappa <- input$kappa
    gamma <- input$gamma
    mu <- input$mu
    
    # Test model
    
    # set up test data:
    # initial population (in fraction)
    S <- 0.99
    E <- 0.00
    I <- 0.01
    R <- 0.00
    
    # 1. create the instance in class SEIRD
    model <- SEIRD()
    # 2. set up parameters and initial population
    transmission_parameters(model) <- list(beta = beta, kappa = kappa, gamma = gamma, mu = mu)
    initial_conditions(model) <- list(S0=S, E0=E, I0=I, R0=R)
    # 3. run simulation
    return(model)
  })
  
  # ODE simulation and plot
  model0.output <- reactive({
    input$goButton
    model <- model0()
    # 4. ode simulation (period: 2000 time points, most of the time will relax)
    t <- seq(0, 2000, by = 1)
    output <- run(model, t)
	return(output)
	})

  # 5.1 plot states
  output$SEIRD.states <- renderPlotly({
    output <- model0.output()
    model.plot <- ggplot(output$states, aes(x = time, y = value)) +
                   geom_line(aes(color = compartment)) +
                   scale_color_brewer(palette = "Dark2") +
                   labs(x = "time (days)", y = "fraction of the population") +
                   theme(legend.position = "bottom", legend.title = element_blank())
                   # theme(text = element_text(size = 20)) 
    #  ggtitle(paste0("model: R0=", model@R0)) # Show R0 on the title
    model.plot <- ggplotly(model.plot)#, dynamicTicks = TRUE, originalData = FALSE)
    return(model.plot)
  })
  outputOptions(output, "SEIRD.states", suspendWhenHidden = FALSE)

  # 5.2 plot changes
  output$SEIRD.changes <- renderPlotly({
    output <- model0.output()
    model.plot <- ggplot(output$changes, aes(x = time, y = value, fill = compartment)) +
                    geom_bar(stat="identity", position = position_dodge()) +
                    labs(x = "time (days)", y = "fraction of the population per day") +
                    theme(legend.position = "bottom", legend.title = element_blank()) #, text = element_text(size = 20)) +
                    scale_fill_brewer(palette = "Dark2")
    model.plot <- ggplotly(model.plot)
    return(model.plot)
  })
  outputOptions(output, "SEIRD.changes", suspendWhenHidden = FALSE)
  
  # R0 calculation
  output$R0 <- renderText({
    # input$goButton
    model <- model0()
    # calculate R0
    paste0("R0 = ", R0(model))
  })
  
  
  #%%%%%%%%%%%
  # SEIaImIsRD
  #%%%%%%%%%%%
  # observeEvent(input$control.params.sc.beta, {
  #   shinyjs::toggle(id="sc.params.beta")
  # })

  # model diagram
  output$sc_model_flowchart <- renderGrViz(
    DiagrammeR::grViz("
    digraph seiaimisrd {

      # initiate graph
      graph [layout = neato, rankdir = LR]
      
      # global node settings
      node [shape = box, fontname = Helvetica, fixedsize = true, 
            style = filled, fillcolor = lightgray, width = 0.6]
      S;E;Ia;Im;Is;R;D

      node [shape = circle, fillcolor = lightblue, width = 0.3]
      beta[label = <&#946;>];
      kappa[label = <&#954;>];
      eta[label = <&#951;>];
      mu[label = <&#956;>]; 
      gamma[label =  <&#947;>];
      omega[label = <&#969;>]

      
      # edge definitions with the node IDs
      # edge definitions with the node IDs
      edge [arrowhead=none]
      S -> beta
      E -> kappa
      kappa -> eta
      {Ia Im Is} -> mu 
      {Ia Im Is} -> gamma
      R -> omega

      edge [arrowhead=vee]
      beta -> E
      eta -> {Ia Im Is}
      mu -> D
      gamma -> R
      omega -> S

      }")
  )

  observe({
    req(input$sc_model_flowchart_click)
    nodeid <- input$sc_model_flowchart_click$id[1]
	nodeid.int <- strsplit(nodeid, "node")[[1]][2] %>% as.integer()
	nodeval <- input$sc_model_flowchart_click$nodeValues
	desc <- c("Susceptible", "Exposed", "Infected (asymptomatic)", "Infected (mild)", "Infected (severe)", 
	          "Recovered (Removed)", "Mortality",
	          "the rate at which an infected individual exposes susceptible", # beta
			  "the rate of progression from exposed to infectious (reciprocal of the incubation period)", # kappa
			  "the probability of exposed individuals moving into each of the infected groups", # eta
			  "the rate of disease-caused mortality of the infected", # mu
			  "the rate of removal (i.e. the recovery rate of the infected)", # gamma
			  "the rate at which recovered individuals become susceptible (i.e. the loss of immunity)") # omega
	trans.params <- c("beta", "kappa", "p_symptom", "mu", "gamma", "omega")
	# for Ia, Im, Is, the correct (entire) nodeval is the second item of the list
	output$SEIaImIsRD.param.desc <- renderText({paste0(nodeval[[length(nodeval)]], ": ", desc[nodeid.int])})
	if(nodeid.int %in% seq(8, 13)){
	  shinyjs::toggle(id=paste0("sc.params.", trans.params[nodeid.int - 7]))
	}
  })

  
  # ODE system
  output$sc.dS <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}S}{\\text{d}t} = -S \\sum_i{\\beta_iI_i} + \\omega R$$"))
  })
  
  output$sc.dE <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}E}{\\text{d}t} = S \\sum_i{\\beta_iI_i} - \\kappa E$$"))
  })
  
  output$sc.dI <- renderUI({
    withMathJax(
      helpText("$$\\frac{dI_i}{dt} = \\eta_i \\kappa E - (\\gamma_i + \\mu_i)I_i$$"))
  })
  
  output$sc.dR <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}R}{\\text{d}t} = - \\omega R + \\sum_i{\\gamma_iI_i}$$"))
  })
  
  output$sc.dD <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}D}{\\text{d}t} = \\sum_i{\\mu_i I_i}$$"))
  })
  
  # model initialization
  model0.sc <- reactive({
    # params
    beta <- list(asymptomatic = input$sc.beta.ia, mild = input$sc.beta.im, severe = input$sc.beta.is)
    kappa <- input$sc.kappa
    omega <- input$sc.omega
    p_symptom <- list(mild = input$sc.p_symptom.im, severe = input$sc.p_symptom.is)
    gamma <- list(asymptomatic = input$sc.gamma.ia, mild = input$sc.gamma.im, severe = input$sc.gamma.is)
    mu <- list(asymptomatic = input$sc.mu.ia, mild = input$sc.mu.im, severe = input$sc.mu.is)
    
    # Test model
    
    # set up test data:
    # initial population (in fraction)
    S <- 0.99
    E <- 0.01
    I_asymptomatic <- 0.00
    I_mild <- 0.00
    I_severe <- 0.00
    R <- 0.00
    D <- 0.00
    
    
    # 1. create the instance in class SEIaImIsRD
    model <- SEIaImIsRD()
    # 2. set up parameters and initial population
    transmission_parameters(model) <- list(beta = beta, kappa = kappa, omega = omega, p_symptom = p_symptom, gamma = gamma, mu = mu)
    initial_conditions(model) <- list(S = S, E = E, I_asymptomatic = I_asymptomatic, I_mild = I_mild, I_severe = I_severe, R = R, D = D)
    
    return(model)
  })


  # ODE simulation and plot
  model0.sc.output <- reactive({
    model <- model0.sc()
    # 4. ode simulation (period: 2000 time points, most of the time will relax)
    t <- seq(0, 2000, by = 1)
    output <- run(model,t)
    return(output)
  })

  # 5.1 plot states
  output$SEIaImIsRD.states <- renderPlotly({
    output <- model0.sc.output()
    model.plot <- ggplot(output$states, aes_string(x = "time", y = "value")) +
                    geom_line(aes_string(colour = "compartment")) +
                    scale_color_brewer(palette = "Dark2") +
                    theme(legend.position = "bottom", legend.title = element_blank()) +
                    labs(x = "time (days)", y = "fraction of the population")
    #  ggtitle(paste0("model: R0=", model@R0)) # Show R0 on the title
    ggplotly(p=model.plot)#, dynamicTicks = TRUE, originalData = FALSE)
    model.plot
  })
  outputOptions(output, "SEIaImIsRD.states", suspendWhenHidden = FALSE)

  # 5.2 plot changes
  output$SEIaImIsRD.changes <- renderPlotly({
    output <- model0.sc.output()
    model.plot <- ggplot(output$changes, aes_string(x = "time", y = "value", fill = "compartment")) +
                    geom_bar(stat="identity", position = position_dodge()) +
                    scale_fill_brewer(palette = "Dark2") +
                    theme(legend.position = "bottom", legend.title = element_blank()) +
                    labs(x = "time (days)", y = "fraction of the population per day")
    ggplotly(p=model.plot)
    model.plot
  })
  outputOptions(output, "SEIaImIsRD.changes", suspendWhenHidden = FALSE)
  
  # R0 calculation
  output$sc.R0 <- renderText({
    model <- model0.sc()
    # calculate R0
    R0 <- R0(model)
    paste0("R0 = ", R0)
  })
  
  #%%%%%%%%%
  # SEIRDAge
  #%%%%%%%%%
  # reactive buttons for modifying transmission params
  # observeEvent(input$control.params.age.beta, {
  #   shinyjs::toggle(id="age.params.beta")
  # })

  # list the file names of uploaded .rda files
  output$age.contact.names <- renderText({
    # names(input)[grepl("age.contact.file", names(input))]
    input$age.contact.files[, 1]
  })
  
  # load .rda files (contact matrices population)
  load.age.contact_matrices <- reactive({
    if(is.null(input$age.contact.files))
      return ()
    else{
      # load age-structured contact matrices
      nfiles = nrow(input$age.contact.files)
      rda = list() #  store all rda file contents into a list
      for (f in 1 : nfiles)
      {
        file <- input$age.contact.files$datapath[f]
        ext <- tools::file_ext(file)
        validate(need(ext == "rda", "Please upload an rda file"))
        e <- new.env(parent = parent.frame())
        load(file, e)
        rda[[ls(e)[1]]] = get(ls(e)[1], e) 
        # load(file = paste0(getwd(), "/data/contact_work.rda"))
        # is /data/population.rda consistent with the contact matrices??
      }
      return(rda)
    }
  })
  
  # update input$age.country choices
  observeEvent(load.age.contact_matrices(),{
    if(is.null(input$age.contact.files))
      updateSelectInput(session,'age.country',choices = NULL)
    else{
      rda <- load.age.contact_matrices()
      country.list <- names(rda$contact_home)
      names(country.list) <- country.list
      updateSelectInput(session,'age.country',choices = country.list)
    }
  })
  
  # model diagram
  output$age_model_flowchart <- renderGrViz(
    
    DiagrammeR::grViz("
    digraph seirdage {
    
      # initiate graph
      graph [layout = dot, rankdir = LR]
      
      # global node settings
      node [shape = box, fontname = Helvetica, fixedsize = true, 
            style = filled, fillcolor = lightgray, width = 0.6]
      S;E;I;R;D

      node [shape = circle, fillcolor = lightblue, width = 0.3]
      beta[label = <&#946;>];
      kappa[label = <&#954;>];
      mu[label = <&#956;>]; 
      gamma[label =  <&#947;>];
      
      # edge definitions with the node IDs
      edge [arrowhead=none]
      S -> beta
      E -> kappa
      I -> {mu gamma}

      edge [arrowhead=vee]
      beta -> E
      kappa -> I
      mu -> D
      gamma -> R 
      }") 
  )
  
  observe({
    req(input$age_model_flowchart_click)
    nodeid <- input$age_model_flowchart_click$id[[1]]
	nodeid.int <- strsplit(nodeid, "node")[[1]][2] %>% as.integer()
	nodeval <- input$age_model_flowchart_click$nodeValues
	desc <- c("Susceptible", "Exposed", "Infected", "Recovered (Removed)", "Mortality",
	          "the rate at which an infected individual exposes susceptible", 
			  "the rate of progression from exposed to infectious (reciprocal of the incubation period)",
			  "the rate of disease-caused mortality of the infected",
			  "the rate of removal (i.e. the recovery rate of the infected)")
	trans.params <- c("beta", "kappa", "mu", "gamma")
	
	output$SEIRDAge.param.desc <- renderText({paste0(nodeval[[length(nodeval)]], ": ", desc[nodeid.int])})
	if(nodeid.int %in% seq(6, 9)){
	  shinyjs::toggle(id=paste0("age.params.", trans.params[nodeid.int - 5]))
	}
  })

  
  # ODE system
  output$age.dS <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}S_i}{\\text{d}t} = - \\beta S_i \\sum_j{C_{ij} I_j} $$"))
  })
  
  output$age.dE <- renderUI({
    withMathJax(
      helpText("$$\\frac{dE_i}{dt} = \\beta S_i \\sum_j{C_{i,j} I_j} - \\kappa E_i $$"))
  })
  
  output$age.dI <- renderUI({
    withMathJax(
      helpText("$$\\frac{dI_i}{dt} = \\kappa E_i - (\\gamma + \\mu_i) I_i $$"))
  })
  
  output$age.dR <- renderUI({
    withMathJax(
      helpText("$$\\frac{dR_i}{dt} = \\gamma I_i $$"))
  })
  
  output$age.dD <- renderUI({
    withMathJax(
      helpText("$$\\frac{dD_i}{dt} = \\mu_i I_i $$"))
  })
  
  # model initialization
  model0.age <- reactive({
    if(is.null(input$age.contact.files))
      return()
    else {
      # access to contact matrices & country data
      rda = load.age.contact_matrices()
      contact_home <- rda$contact_home
      contact_work <- rda$contact_work
      contact_school <- rda$contact_school
      contact_other <- rda$contact_other
      population <- rda$population
      
      country = input$age.country
      
      # construct age groups - number equal to the nrow (ncol) of contact matrices
      n_ages = length(contact_home[[country]])
      ages <- c(0, seq_len(n_ages) * 80/n_ages)
      age_names <- vector(length = n_ages)
      for(i in seq_along(age_names)) {
        age_names[i] <- paste0(ages[i], "-", ages[i + 1])
      }
      format_matrix <- function(contact_matrix, age_names) {
        colnames(contact_matrix) <- age_names
        contact_matrix$age_infectee <- age_names
        contact_matrix %>%
          pivot_longer(all_of(age_names)) %>% 
          rename(age_infector=name) %>% 
          mutate(age_infector=fct_relevel(age_infector, age_names)) %>% 
          mutate(age_infectee=fct_relevel(age_infectee, age_names))
      }
      c_home <- format_matrix(contact_home[[country]], age_names) %>% mutate(type="home")
      c_work <- format_matrix(contact_work[[country]], age_names) %>% mutate(type="work")
      c_school <- format_matrix(contact_school[[country]], age_names) %>% mutate(type="school")
      c_other <- format_matrix(contact_other[[country]], age_names) %>% mutate(type="other")
      
      c_all <- c_home %>%
        bind_rows(c_work) %>% 
        bind_rows(c_school) %>% 
        bind_rows(c_other)
      c_combined <- c_all %>% 
        group_by(age_infectee, age_infector) %>% 
        summarise(value=sum(value))
      
      # !!! some countries population data are missing !!!
      pops = population[population$country == country, ]$pop
      pop_fraction = pops/sum(pops)
      pop_fraction[n_ages] = sum(pop_fraction[n_ages:length(pop_fraction)])
      pop_fraction = pop_fraction[1:n_ages] # suppose our contact matrices are subpop of 1:n_age in population
      
      c_combined_wider <- c_combined %>% 
        pivot_wider(id_cols = age_infectee, names_from = age_infector,
                    values_from=value) %>% 
        ungroup() %>% 
        select(-age_infectee) %>% 
        as.matrix()
      
      # update input$age_range.select choices
      age.list <- age_names
      names(age.list) <- age_names
      observeEvent(input$age.contact.files,{
        updateSelectInput(session,'age_range.select',choices = age.list)
      })
      
      beta <- input$age.beta
      kappa <- input$age.kappa
      gamma <- input$age.gamma
      mu <- input$age.mu
      
      S <- 0.99
      E <- 0.00
      I <- 0.01
      R <- 0.00
      D <- 0.00
      
      model <- SEIRDAge(n_age_categories = n_ages, 
                        contact_matrix = c_combined_wider, 
                        age_ranges = as.list(age_names))
      transmission_parameters(model) <- list(b=beta, k=kappa, g=gamma, mu=mu)
      initial_conditions(model) <- list(S0=pop_fraction*S,
                                        E0=rep(0, n_ages),
                                        I0=pop_fraction*I,
                                        R0=rep(0, n_ages),
                                        D0=rep(0, n_ages))
      return(model)
    }
  })
  
  # ODE simulation and plot
  ## simulation result
  age.model.simulation <- reactive({
    if(is.null(input$age.contact.files))
      return ()
    else{
      model <- model0.age()
      t <- seq(0, 200, by = 1)
      output <- run(model, times=t)
      return (output)
    }
  })

  ## plot by compartment (states)
  output$SEIRDAge.states.by.compartment <- renderPlotly({
    if(is.null(input$age.contact.files))
      return ()
    else{
      output <- age.model.simulation()
	  output <- output$states
      # set up color palette - more than 8 groups, need to be set manually
      n.cols <- length(unique(output$age_range))
      colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n.cols)
      model.plot <- ggplot(output, aes(x=time, y=value, colour=age_range)) +
        geom_line() +
        facet_grid(vars(compartment), space = "free", scales = "free") +
        scale_color_manual(values = colors) +
        theme_classic()
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return (model.plot)
    }
  })
  ## plot by compartment (changes)
  output$SEIRDAge.changes.by.compartment <- renderPlotly({
    if(is.null(input$age.contact.files))
      return ()
    else{
      output <- age.model.simulation()
	  output <- output$changes
      # set up color palette - more than 8 groups, need to be set manually
      n.cols <- length(unique(output$age_range))
      colors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n.cols)
      model.plot <- ggplot(output, aes(x=time, y=value, fill=age_range)) +
        geom_bar(stat="identity", position=position_dodge()) +
        facet_grid(vars(compartment), space = "free", scales = "free") +
        scale_color_manual(values = colors) +
        theme_classic()
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return (model.plot)
    }
  })
  
  ## plot by age groups (states)
  output$SEIRDAge.states.by.age <- renderPlotly({
    if(is.null(input$age.contact.files))
      return ()
    else{
      output <- age.model.simulation() 
	  output <- output$states
      output <- subset(output, age_range == input$age_range.select)
      model.plot <- ggplot(output, aes(x=time, y=value)) +
        geom_line(aes(colour=compartment)) +
        scale_color_brewer(palette = "Set2") +
        theme_classic()
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return(model.plot)
    }
  })

  ## plot by age groups (states)
  output$SEIRDAge.changes.by.age <- renderPlotly({
    if(is.null(input$age.contact.files))
      return ()
    else{
      output <- age.model.simulation() 
	  output <- output$changes
	  # output <- output %>% filter(compartment=="Incidence")
      output <- subset(output, age_range == input$age_range.select)
      model.plot <- ggplot(output, aes(x=time, y=value, fill=compartment)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_color_brewer(palette = "Set2") +
        theme_classic()
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return(model.plot)
    }
  })



  #%%%%%%%%%%%
  # SEIRDV
  #%%%%%%%%%%%

  # model diagram
  output$vac_model_flowchart <- renderGrViz(
    DiagrammeR::grViz("
    digraph seirdv {
      # initiate graph
      graph [layout = neato, rankdir = LR]
      
      # global node settings
      node [shape = box, fontname = Helvetica, fixedsize = true, 
            style = filled, fillcolor = lightgray, width = 0.6]
      S;E;I;R;V;D
      node [shape = circle, fillcolor = lightblue, width = 0.3]
      beta[label = <&#946;>];
      kappa[label = <&#954;>];
      mu[label = <&#956;>]; 
      gamma[label =  <&#947;>];
      deltar[label = <&#948;>];
      deltav[label = <&#948;>];
      nu[label = <&#957;>];
      
      # edge definitions with the node IDs
      # edge definitions with the node IDs
      edge [arrowhead=none]
      S -> beta
      S -> nu
      E -> kappa
      I -> gamma
      I -> mu
      R -> deltar
      V -> deltav
      edge [arrowhead=vee]
      beta -> E
      nu -> V
      kappa -> I
      gamma -> R
      mu -> D
      deltar -> S
      deltav -> S
      }")
  )

  observe({
    req(input$vac_model_flowchart_click)
    nodeid <- input$vac_model_flowchart_click$id[1]
	nodeid.int <- strsplit(nodeid, "node")[[1]][2] %>% as.integer()
	nodeval <- input$vac_model_flowchart_click$nodeValues
	desc <- c("Susceptible", "Exposed", "Infected", 
	          "Recovered (Removed)", "Vaccinated", "Mortality",
	          "the rate at which an infected individual exposes susceptible", # beta
			  "the rate of progression from exposed to infectious (reciprocal of the incubation period)", # kappa
			  "the rate of disease-caused mortality of the infected", # mu
			  "the rate of removal (i.e. the recovery rate of the infected)", # gamma
			  "the rate at which recovered individuals become susceptible (i.e. the loss of immunity)", # deltar
                          "the rate at which vaccinated individuals become susceptible (i.e. the loss of immunity)", # deltav
                          "the rate of vaccination in susceptible population") # nu
	trans.params <- c("beta", "kappa", "mu", "gamma", "deltar", "deltav", "nu")
	output$SEIRDV.param.desc <- renderText({paste0(nodeval[[length(nodeval)]], ": ", desc[nodeid.int])})
	if(nodeid.int %in% seq(7, 13)){
	  shinyjs::toggle(id=paste0("vac.params.", trans.params[nodeid.int - 6]))
	}
  })

  
  # ODE system
  output$vac.dS <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}S}{\\text{d}t} = -S \\beta I - \nu \\text{Inter}(t) S + \\delta_V V + \\delta_R R$$"))
  })
  
  output$vac.dE <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}E}{\\text{d}t} = S \\beta I - \\kappa E$$"))
  })
  
  output$vac.dI <- renderUI({
    withMathJax(
      helpText("$$\\frac{dI}{dt} = \\kappa E - (\\gamma + \\mu)I$$"))
  })
  
  output$vac.dR <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}R}{\\text{d}t} = \\gamma I - \\delta_R R$$"))
  })

  output$vac.dV <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}V}{\\text{d}t} = \\nu Inter(t) S - \\delta_V V$$"))
  })
  
  output$vac.dD <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}D}{\\text{d}t} = \\mu I$$"))
  })



 
  
  # model initialization
  model0.vac <- reactive({
    # params
    beta <- input$vac.beta
    kappa <- input$vac.kappa
    gamma <- input$vac.gamma
    mu <- input$vac.mu
    nu <- input$vac.nu
    delta_R <- input$vac.delta.r
    delta_V <- input$vac.delta.v
    
    # Test model
    
    # set up test data:
    # initial population (in fraction)
    S <- 0.99
    E <- 0.00
    I <- 0.01
    R <- 0.00
    V <- 0.00
    
    
    # 1. create the instance in class SEIRDV
    model <- SEIRDV()
    # 2. set up parameters and initial population
    transmission_parameters(model) <- list(beta = beta, kappa = kappa, gamma = gamma, mu = mu, nu = nu, delta_V = delta_V, delta_R = delta_R)
    initial_conditions(model) <- list(S0 = S, E0 = E, I0 = I, R0 = R, V0 = V)
    intervention_parameters(model) <- list(starts=c(17, 35, 42),
                                          stops=c(25, 39, 49),
                                          coverages=c(1, 1, 1))
    return(model)
  })


  # ODE simulation and plot
  model0.vac.output <- reactive({
    model <- model0.vac()
    # 4. ode simulation (period: 2000 time points, most of the time will relax)
    t <- seq(0, 200, by = 1)
    output <- run(model, t)
    return(output)
  })

  # 5.1 plot states
  output$SEIRDV.states <- renderPlotly({
    output <- model0.vac.output()
    model.plot <- ggplot(output$states, aes(x = time, y = value)) +
          geom_line(aes(color = compartment)) +
          scale_color_brewer(palette = "Dark2") +
          labs(x = "time (days)", y = "fraction of the population") +
          theme(legend.position = "bottom", legend.title = element_blank())
          # theme(text = element_text(size = 20))

    model.plot <- ggplotly(model.plot)#, dynamicTicks = TRUE, originalData = FALSE)
    model.plot
  })
  outputOptions(output, "SEIRDV.states", suspendWhenHidden = FALSE)

  # 5.2 plot changes
  output$SEIRDV.changes <- renderPlotly({
    output <- model0.vac.output()
    model.plot <- ggplot(output$changes, aes(x = time, y = value, fill = compartment)) +
                    geom_bar(stat="identity", position = position_dodge()) +
                    scale_color_brewer(palette = "Dark2") +
                    labs(x = "time (days)", y = "fraction of the population \n per day") +
                    theme(legend.position = "bottom", legend.title = element_blank())#, text = element_text(size = 20))
    model.plot <- ggplotly(model.plot)
    model.plot
  })
  outputOptions(output, "SEIRDV.changes", suspendWhenHidden = FALSE)


  #%%%%%%%%%
  # SEIRD_RU
  #%%%%%%%%%

 # list the file names of uploaded .rda files
  output$ru.contact.names <- renderText({
    input$ru.contact.files[, 1]
  })
  
  # load .rda files (contact matrices population)
  load.ru.contact_matrices <- reactive({
    if(is.null(input$ru.contact.files))
      return ()
    else{
      # load contact matrices
      nfiles = nrow(input$ru.contact.files)
      rda = list() #  store all rda file contents into a list
      for (f in 1 : nfiles)
      {
        file <- input$ru.contact.files$datapath[f]
        ext <- tools::file_ext(file)
        validate(need(ext == "rda", "Please upload an rda file"))
        e <- new.env(parent = parent.frame())
        load(file, e)
        rda[[ls(e)[1]]] = get(ls(e)[1], e) 
        ## ???? is /data/{percentrural_by_country,agedemographics_by_country,country.codetoname}.rda; consistent with the contact matrices ????
      }
      return(rda)
    }
  })
  
  # update input$ru.country choices
  observeEvent(load.ru.contact_matrices(),{
    if(is.null(input$ru.contact.files))
      updateSelectInput(session,'ru.country',choices = NULL)
    else{
      rda <- load.ru.contact_matrices()
      country.list <- rda$country_codetoname$CountryName
      names(country.list) <- country.list
      updateSelectInput(session,'ru.country',choices = country.list)
    }
  })
  
  # model diagram
  output$ru_model_flowchart <- renderGrViz(
    
    DiagrammeR::grViz("
    digraph seird_ru {
    
      # initiate graph
      graph [layout = dot, rankdir = LR]
      
      # global node settings
      node [shape = box, fontname = Helvetica, fixedsize = true, 
            style = filled, fillcolor = lightgray, width = 0.6]
      S;E;I;R;D

      node [shape = circle, fillcolor = lightblue, width = 0.3]
      beta[label = <&#946;>];
      kappa[label = <&#954;>];
      mu[label = <&#956;>]; 
      gamma[label =  <&#947;>];
      
      # edge definitions with the node IDs
      edge [arrowhead=none]
      S -> beta
      E -> kappa
      I -> {mu gamma}

      edge [arrowhead=vee]
      beta -> E
      kappa -> I
      mu -> D
      gamma -> R 
      }") 
  )
  
  observe({
    req(input$ru_model_flowchart_click)
    nodeid <- input$ru_model_flowchart_click$id[[1]]
	nodeid.int <- strsplit(nodeid, "node")[[1]][2] %>% as.integer()
	nodeval <- input$ru_model_flowchart_click$nodeValues
	desc <- c("Susceptible", "Exposed", "Infected", "Recovered (Removed)", "Mortality",
	          "the rate at which an infected individual exposes susceptible", 
			  "the rate of progression from exposed to infectious (reciprocal of the incubation period)",
			  "the rate of disease-caused mortality of the infected",
			  "the rate of removal (i.e. the recovery rate of the infected)")
	trans.params <- c("beta", "kappa", "mu", "gamma")
	
	output$SEIRD_RU.param.desc <- renderText({paste0(nodeval[[length(nodeval)]], ": ", desc[nodeid.int])})
	if(nodeid.int %in% seq(6, 9)){
	  shinyjs::toggle(id=paste0("ru.params.", trans.params[nodeid.int - 5]))
	}
  })


  # ODE system
  output$ru.dS <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}S_i}{\\text{d}t} = - \\beta S_i \\left((\\sum_j{I_j})(\\sum_j{\\phi_j N_j}C + \\frac{I_i}{\\phi_i}N_i(1-C)\\right) $$"))
  })
  
  output$ru.dE <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}E_i}{\\text{d}t} = \\beta S_i \\left((\\sum_j{I_j})(\\sum_j{\\phi_j N_j}C + \\frac{I_i}{\\phi_i}N_i(1-C)\\right) - \\kappa E_i$$"))
  })
  
  output$ru.dI <- renderUI({
    withMathJax(
      helpText("$$\\frac{dI_i}{dt} = \\kappa E_i - (\\gamma + \\mu_i) I_i $$"))
  })
  
  output$ru.dR <- renderUI({
    withMathJax(
      helpText("$$\\frac{dR_i}{dt} = \\gamma I_i $$"))
  })
  
  output$ru.dD <- renderUI({
    withMathJax(
      helpText("$$\\frac{dD_i}{dt} = \\mu_i I_i $$"))
  })
  
  # model initialization
  model0.ru <- reactive({
    if(is.null(input$ru.contact.files))
      return()
    else {
      # access to contact matrices & country data
      rda = load.ru.contact_matrices()
      contact_all_urban <- rda$contact_all_urban
      contact_all_rural <- rda$contact_all_rural
      names_urban <- names(contact_all_urban)
      names_rural <- names(contact_all_rural)
      names_common <- intersect(names_urban,names_rural)
	  # demographic data
	  # data on percentage of the population that is rural
      percentrural_by_country <- rda$percentrural_by_country
	  # age demographic breakdown into 5-year age categories
	  agedemographics_by_country <- rda$agedemographics_by_country
	  # conversion table from 3 letter country codes to full names
	  country_codetoname <- rda$country_codetoname
	  
	  # get full name of country
      # country_fullname <- country_codetoname$CountryName[country_codetoname$CountryCode == country]
	  country_fullname <- input$ru.country
	  # get country code (equals to the original vignette country code)
	  country <- country_codetoname$CountryCode[country_codetoname$CountryName == country_fullname]
      country_rural <- percentrural_by_country$`2019`[percentrural_by_country$`Country Code` == country]  ## ??? can we select other years???
	  frac_rural <- country_rural/100 # turn percentage into fraction

	  # get fraction of population in each 5-year age range
	  # extract age demographic vector
      pop_byage_2019 <- agedemographics_by_country[(agedemographics_by_country$`Region, subregion, country or area *` == country_fullname
                                     & agedemographics_by_country$`Reference date (as of 1 July)` == 2019),5:25]
	  pop_byage_2019 <- as.double(pop_byage_2019)
      # normalize to fractions of the total population
      pop_byage_2019 <- pop_byage_2019/sum(pop_byage_2019) 
      # must sum last five entries because contact matrices have an 80+ category instead of 100+
      pop_byage_2019 <- c(pop_byage_2019[1:15], sum(pop_byage_2019[16:20]))

      
      beta <- input$ru.beta
      kappa <- input$ru.kappa
      gamma <- input$ru.gamma
      mu <- input$ru.mu
	  Connectedness <- input$ru.c
      
      start_infected_urban = 0.0001
	  start_infected_rural = 0
	  S0U = 1-frac_rural - start_infected_urban
	  E0U = 0
	  I0U = start_infected_urban
	  R0U = 0
	  S0Y = frac_rural - start_infected_rural
	  E0Y = 0
	  I0Y = start_infected_rural
	  R0Y = 0
      
	  # initialize model
      model <- SEIRD_RU()
	  # set transmission params
      transmission_parameters(model) <- list(b=beta, k=kappa, g=gamma, mu=mu, C=Connectedness)
	  # set initial conditions
	  initial_conditions(model) <- list(S0U, E0U, I0U, R0U, S0Y, E0Y, I0Y, R0Y)
	  #set demographic data
	  country_demog(model) <- list(pop_byage_2019*(1-frac_rural),pop_byage_2019*frac_rural)
      # set contact matrices
      contact_rates(model) <- list(contact_all_urban[[country]],contact_all_rural[[country]])
	  ## ??? For some countries, the contact_all_urban[[country]] contains NAs!!!! cause error

      return(model)
    }
  })
  
  # ODE simulation and plot
  ## simulation result
  ru.model.simulation <- reactive({
    if(is.null(input$ru.contact.files))
      return ()
    else{
      model <- model0.ru()
	  tend = 80
      t <- seq(0, tend, by = 1)
      output <- run(model, t)
	  # dim(output)
      return (output)
    }
  })

  ## plot by rual/urban separated compartment (states)
  output$SEIRD_RU.states <- renderPlotly({
    if(is.null(input$ru.contact.files))
      return ()
    else{
	  output <- ru.model.simulation()
	  i <- sapply(output, is.factor)
	  output[i] <- lapply(output[i], as.character)
      SEIR_df <- subset(output, compartment != "Incidences_U" & compartment != "Deaths_U" & compartment != "Incidences_Y" & compartment != "Deaths_Y")
      SEIR_df$compartment <- factor(SEIR_df$compartment, levels = c("S_U", "E_U", "I_U", "R_U", "S_Y", "E_Y", "I_Y", "R_Y"))
      col <- c("S_U" = "blue", "E_U" = "green", "I_U" = "yellow", "R_U" = "red", "S_Y" = "lightskyblue1", "E_Y" = "lightgreen", "I_Y" = "lightyellow", "R_Y" = "indianred")
      model.plot <- ggplot(SEIR_df, aes(x = time, y = value)) +
          geom_line(aes(color = compartment), size = 1.5) +
          labs(x = "time (days)", y = "fraction of the population") +
          theme(legend.position = "bottom", legend.title = element_blank()) +#, text = element_text(size = 20)) +
          scale_color_manual(values = col)
	  model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return (model.plot)
	}
  })
  ## plot by rual/urban separated compartment (changes)
  output$SEIRD_RU.changes <- renderPlotly({
    if(is.null(input$ru.contact.files))
      return ()
    else{
      output <- ru.model.simulation()
	  case_df <- subset(output, compartment == "Incidences_U" | compartment == "Deaths_U" | compartment == "Incidences_Y" | compartment == "Deaths_Y")
      case_df$compartment <- factor(case_df$compartment, levels = c("Incidences_U", "Deaths_U", "Incidences_Y", "Deaths_Y"))
      col <- c("Cases_U" = "grey28", "Deaths_U" = "black", "Cases_Y" = "lightgray", "Deaths_Y" = "darkgray")
      model.plot <- ggplot(case_df, aes(x = time, y = value, fill = compartment)) +
          geom_bar(stat="identity", position = position_dodge()) +
          labs(x = "time (days)", y = "fraction of the population") +
          theme(legend.position = "bottom", legend.title = element_blank()) + #, text = element_text(size = 20)) +
          scale_fill_discrete(labels = c("Cases_U","Deaths_U","Cases_Y","Deaths_Y"))
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return (model.plot)
    }
  })

  # R0 calculation
  output$ru.R0 <- renderText({
    model <- model0.ru()
    # calculate R0
    R0 <- R0(model)
    paste0("R0 = ", R0)
  })
  


}

# beta [label='\u1d66'] 
# gamma [label='\u1d67']
