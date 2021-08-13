#
# The server definition of a shiny app object.
#

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  #%%%%%%%%%
  # SEIRD
  #%%%%%%%%%
  # reactive buttons for modifying transmission params
  observeEvent(input$control.params.beta, {
    shinyjs::toggle(id="seird.params.beta")
  })
  
  observeEvent(input$control.params.kappa, {
    shinyjs::toggle(id="seird.params.kappa")
  })
  
  observeEvent(input$control.params.gamma, {
    shinyjs::toggle(id="seird.params.gamma")
  })
  
  observeEvent(input$control.params.mu, {
    #  cat("\nSidebar param config")
    shinyjs::toggle(id="seird.params.mu")
  })
  # model diagram
  output$model.flowchart <- renderGrViz(
    
    DiagrammeR::grViz("digraph {

      # initiate graph
      graph [layout = dot, rankdir = LR]
      
      # global node settings
      node [shape = rectangle, fontname = Helvetica, style = filled, fillcolor = lightblue]#, fontcolor = gray35]
      
      # label nodes
      S [label = 'S']
      E [label =  'E']
      I [label = 'I']
      R [label =  'R']
      D [label =  'D']
      
      # edge definitions with the node IDs
      S -> E [label='\u1d66'] 
      E -> I -> D
      I -> R [label='\u1d67']
      }") 
  )
  output$testplot <- renderPlot({
    par(mar = c(1, 1, 1, 1))
    openplotmat()
    elpos <- coordinates (c(1, 1, 2, 4))
    fromto <- matrix(ncol = 2, byrow = TRUE, data = c(1, 2, 2, 3, 2, 4, 4, 7, 4, 8,3,5,3,6))
    nr <- nrow(fromto)
    arrpos <- matrix(ncol = 2, nrow = nr)
    for (i in 1:nr)
      arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                                    from = elpos[fromto[i, 1], ], lwd = 2, arr.pos = 0.6, arr.length = 0.5)
    textellipse(elpos[1,], 0.1, lab = "start", box.col = "green",
                shadow.col = "darkgreen", shadow.size = 0.005, cex = 1.5)
    textrect (elpos[2,], 0.15, 0.05,lab = "found term?", box.col = "grey",
              shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textrect (elpos[4,], 0.15, 0.05,lab = "related?", box.col = "grey",
              shadow.col = "darkblue", shadow.size = 0.005, cex = 1.5)
    textellipse(elpos[3,], 0.1, 0.1, lab = c("other","term"), box.col = "orange",
                shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textellipse(elpos[7,], 0.1, 0.1, lab = c("make","a link"),box.col = "orange",
                shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textellipse(elpos[8,], 0.1, 0.1, lab = c("new","article"),box.col = "orange",
                shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textellipse(elpos[5,], 0.1, 0.1, lab = c("make","a link"),box.col = "orange",
                shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    textellipse(elpos[6,], 0.1, 0.1, lab = c("new","article"),box.col = "orange",
                shadow.col = "red", shadow.size = 0.005, cex = 1.5)
    #
    dd <- c(0.0, 0.025)
    # actionButton(arrpos[2, 1] + 0.05, arrpos[2, 2], inputId = "control.params.beta", label = withMathJax("$\\beta$"))
    text(arrpos[3, 1] - 0.05, arrpos[3, 2], "no")
    text(arrpos[4, 1] + 0.05, arrpos[4, 2] + 0.05, "yes")
    text(arrpos[5, 1] - 0.05, arrpos[5, 2] + 0.05, "no")
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
    E <- 0.01
    I <- 0.00
    R <- 0.00
    
    # 1. create the instance in class SEIaImIsRD
    model <- SEIRD()
    # 2. set up parameters and initial population
    transmission_parameters(model) <- list(beta = beta, kappa = kappa, gamma = gamma, mu = mu)
    initial_conditions(model) <- list(S0=S, E0=E, I0=I, R0=R)
    
    return(model)
  })
  
  # ODE simulation and plot
  output$SEIRD <- renderPlotly({
    input$goButton
    model <- model0()
    # 4. ode simulation (period: 2000 time points, most of the time will relax)
    t <- seq(0, 2000, by = 1)
    output <- run(model, t)
    # 5. plot
    # plot only output$states, not output$changes
    model.plot <- plot_dataframe(output$states, x = "time", y = "value", c = "compartment") +
                  scale_color_brewer(palette = "Set2")
    #  ggtitle(paste0("model: R0=", model@R0)) # Show R0 on the title
    ggplotly(p=model.plot)#, dynamicTicks = TRUE, originalData = FALSE)
    model.plot
  })
  outputOptions(output, "SEIRD", suspendWhenHidden = FALSE)
  
  # R0 calculation
  output$R0 <- renderText({
    # input$goButton
    model <- model0()
    # 3. calculate R0
    paste0("R0 = ", R0(model))
  })
  
  
  #%%%%%%%%%%%
  # SEIaImIsRD
  #%%%%%%%%%%%
  observeEvent(input$control.params.sc.beta, {
    shinyjs::toggle(id="sc.params.beta")
  })
  
  observeEvent(input$control.params.sc.kappa, {
    shinyjs::toggle(id="sc.params.kappa")
  })
  
  observeEvent(input$control.params.sc.gamma, {
    shinyjs::toggle(id="sc.params.gamma")
  })
  
  observeEvent(input$control.params.mu, {
    shinyjs::toggle(id="sc.params.mu")
  })
  
  observeEvent(input$control.params.sc.p_symptom, {
    shinyjs::toggle(id="sc.params.p_symptom")
  })
  
  observeEvent(input$control.params.sc.omega, {
    #  cat("\nSidebar param config")
    shinyjs::toggle(id="sc.params.omega")
  })
  
  # model diagram
  output$sc.model.flowchart <- renderGrViz(
    DiagrammeR::grViz("digraph {

      # initiate graph
      graph [layout = dot, rankdir = LR]
      
      # global node settings
      node [shape = rectangle, fontname = Helvetica, style = filled, fillcolor = lightblue]#, fontcolor = gray35]
      
      # label nodes
      S [label = 'S']
      E [label =  'E']
      Ia [label = 'Ia']
      Im [label = 'Im']
      Is [label = 'Is']
      R [label =  'R']
      D [label =  'D']
      
      # edge definitions with the node IDs
      S -> E [label='\u1d66'] 
      E -> {Ia Im Is}
      {Ia Im Is} -> R 
      {Ia Im Is} -> D
      }")
  )
  
  # ODE system
  output$sc.dS <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}S}{\\text{d}t} = -S \\sum_i{\\beta_{I_i}I_i} + \\omega R$$"))
  })
  
  output$sc.dE <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}E}{\\text{d}t} = S \\sum_i{\\beta_{I_i}I_i} - \\kappa E$$"))
  })
  
  output$sc.dI <- renderUI({
    withMathJax(
      helpText("$$\\frac{dI_i}{dt} = \\eta_{E->I_i} \\kappa E - (\\gamma_{I_i} + \\mu_{I_i})I_{i}$$"))
  })
  
  output$sc.dR <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}R}{\\text{d}t} = - \\omega R + \\sum_i{\\gamma_{I_i}I_i}$$"))
  })
  
  output$sc.dD <- renderUI({
    withMathJax(
      helpText("$$\\frac{\\text{d}D}{\\text{d}t} = - \\sum_i{\\mu_i I_i}$$"))
  })
  
  # model initialization
  model0.sc <- reactive({
    # params
    beta <- list(i_asymptomatic = input$sc.beta.ia, i_mild = input$sc.beta.im, i_severe = input$sc.beta.is)
    kappa <- input$sc.kappa
    omega <- input$sc.omega
    p_symptom <- list(i_mild = input$sc.p_symptom.im, i_severe = input$sc.p_symptom.is)
    gamma <- list(i_asymptomatic = input$sc.gamma.ia, i_mild = input$sc.gamma.im, i_severe = input$sc.gamma.is)
    mu <- list(i_asymptomatic = input$sc.mu.ia, i_mild = input$sc.mu.im, i_severe = input$sc.mu.is)
    
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
  output$SEIaImIsRD <- renderPlotly({
    model <- model0.sc()
    # 4. ode simulation (period: 2000 time points, most of the time will relax)
    t <- seq(0, 2000, by = 1)
    model <- run(model, t)
    # 5. plot
    model.plot <- plot_dataframe(model@output, x = "time", y = "value", c = "compartment") +
                  scale_color_brewer(palette = "Set2")
    #  ggtitle(paste0("model: R0=", model@R0)) # Show R0 on the title
    model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
    model.plot
  })
  outputOptions(output, "SEIaImIsRD", suspendWhenHidden = FALSE)
  
  # R0 calculation
  output$sc.R0 <- renderText({
    model <- model0.sc()
    # 3. calculate R0
    model <- R0(model)
    paste0("R0 = ", model@R0)
  })
  
  #%%%%%%%%%
  # SEIRDAge
  #%%%%%%%%%
  # reactive buttons for modifying transmission params
  observeEvent(input$control.params.age.beta, {
    shinyjs::toggle(id="age.params.beta")
  })
  
  observeEvent(input$control.params.age.kappa, {
    shinyjs::toggle(id="age.params.kappa")
  })
  
  observeEvent(input$control.params.age.gamma, {
    shinyjs::toggle(id="age.params.gamma")
  })
  
  observeEvent(input$control.params.age.mu, {
    #  cat("\nSidebar param config")
    shinyjs::toggle(id="age.params.mu")
  })

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
  output$age.model.flowchart <- renderGrViz(
    DiagrammeR::grViz("digraph {

      # initiate graph
      graph [layout = dot, rankdir = LR]

      # global node settings
      node [shape = rectangle, fontname = Helvetica, style = filled, fillcolor = lightblue]#, fontcolor = gray35]

      # label nodes
      S [label = 'S']
      E [label =  'E']
      I [label = 'I']
      R [label =  'R']
      D [label =  'D']

      # edge definitions with the node IDs
      S -> E [label='\u1d66']
      E -> I -> D
      I -> R [label='\u1d67']
      }")
  )
  
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
      res <- run(model, time=t) %>%
        filter(compartment!="Incidence")
      return (res)
    }
  })
  
  ## plot by compartment
  output$SEIRDAge.by.compartment <- renderPlotly({
    if(is.null(input$age.contact.files))
      return ()
    else{
      res <- age.model.simulation()
      model.plot <- ggplot(res, aes(x=time, y=value, colour=age_range)) +
        geom_line() +
        facet_grid(vars(compartment), space = "free", scales = "free") +
        theme_classic()
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return (model.plot)
    }
  })
  
  ## plot by age groups
  output$SEIRDAge.by.age <- renderPlotly({
    if(is.null(input$age.contact.files))
      return ()
    else{
      res <- age.model.simulation()
      res <- subset(res, age_range == input$age_range.select)
      model.plot <- ggplot(res, aes(x=time, y=value)) +
        geom_line(aes(colour=compartment)) +
        scale_color_brewer(palette = "Set2") +
        theme_classic() # +
        # ggtitle(paste0("age range: ", age_range.select))
      model.plot <- ggplotly(p=model.plot, dynamicTicks = TRUE, originalData = FALSE)
      return(model.plot)
    }
  })
  
  # # R0 calculation
  # output$R0 <- renderText({
  #   model <- model0.age()
  #   # 3. calculate R0
  #   paste0("R0 = ", R0(model))
  # }),

}




