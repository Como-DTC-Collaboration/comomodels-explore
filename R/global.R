# Class definitions
#%%%%%%%%
# SEIRD %
#%%%%%%%%
#' An S4 object representing the SEIRD.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", R0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "gamma", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#'
#' @export SEIRD
SEIRD <- setClass("SEIRD",
                  # slots
                  slots = c(
                      output_names = "list",
                      initial_condition_names = "list",
                      transmission_parameter_names = "list",
                      initial_conditions = "list",
                      transmission_parameters = "list"
                  ),
                  # prototypes for the slots, automatically set parameter names and
                  # its data type
                  prototype = list(
                      output_names = list("S", "E", "I", "R", "D", "Incidence", "Deaths"),
                      initial_condition_names = list("S0", "E0", "I0", "R0"),
                      transmission_parameter_names = list("beta", "kappa", "gamma", "mu"),
                      initial_conditions = vector(mode = "list", length = 4),
                      transmission_parameters = vector(mode = "list", length = 4)
                  )
)
#' Retrieves initial conditions of SEIRD model.
#'
#' @param object An object of the class SEIRD.
#' @export
setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))


#' @describeIn SEIRD Retrieves initial conditions of SEIRD model.
#'
#' @param object An object of the class SEIRD.
#' @aliases initial_conditions,ANY,ANY-method
#' @export
setMethod("initial_conditions", "SEIRD",
          function(object) object@initial_conditions)

#' Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD.
#' @export
setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

#' @describeIn SEIRD Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD.
#' @aliases transmission_parameters,ANY,ANY-method
#' @export
setMethod("transmission_parameters", "SEIRD",
          function(object) object@transmission_parameters)

#' Set initial conditions (S0, E0, I0 and R0) of the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRD with initial conditions assigned.
#'
#' @export
setGeneric(
    "initial_conditions<-",
    function(object, value) {
        standardGeneric("initial_conditions<-")
    })

#' @describeIn SEIRD Setter method for initial conditions (S0, E0, I0 and R0)
#' of the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRD with initial conditions assigned.
#'
#' @aliases initial_conditions<-,ANY,ANY-method
#' @export
setMethod(
    "initial_conditions<-", "SEIRD",
    function(object, value) {
        
        if (mean(names(value) %in% object@initial_condition_names) != 1)
            stop(paste0("Initial conditions must contain: ",
                        object@initial_condition_names))
        init_cond <- value
        
        # raise errors if age category dimensions do not match initial state vectors
        # also raise errors if initial state and parameter values are not doubles
        for (p in list("S0", "E0", "I0", "R0")) {
            if (!is.numeric(init_cond[[p]])) {
                stop(glue("{p} format must be numeric"))
            }
        }
        
        # check that the initial conditions are properly normalized
        if (init_cond$S0 + init_cond$E0 + init_cond$I0 + init_cond$R0 != 1) {
            stop("Invalid initial conditions. Must add up to 1.")
        }
        
        object@initial_conditions <- init_cond
        
        object
    })

#' Set transmission parameters for SEIRD model
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD model)
#' @param value (list) list of values for beta, kappa, gamma, mu, respectively.
#'
#' @return object of class SEIRD with transmission parameter values
#' assigned.
#' @export
setGeneric(
    "transmission_parameters<-",
    function(object, value) {
        standardGeneric("transmission_parameters<-")
    })


#' @describeIn SEIRD Set transmission parameters (beta, kappa, gamma and mu)
#' of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD model)
#' @param value (list) list of values for beta, kappa, gamma, mu, respectively.
#'
#' @return object of class SEIRD with transmission parameter values
#' assigned.
#' @aliases transmission_parameters<-,ANY,ANY-method
#' @export
setMethod(
    "transmission_parameters<-", "SEIRD",
    function(object, value) {
        
        # create list of parameter values
        if (mean(names(value) %in% object@transmission_parameter_names) != 1)
            stop(paste0("Transmission parameters must contain: ",
                        object@transmission_parameter_names))
        trans_params <- value
        
        # check format of parameters
        if (length(trans_params$b) != 1
            | length(trans_params$k) != 1
            | length(trans_params$g) != 1
            | length(trans_params$m) != 1) {
            stop("The parameter values should be 1-dimensional.")
        }
        
        # if all above tests are passed, assign the trans_params namelist to the
        # object
        object@transmission_parameters <- trans_params
        
        object
    })

# SEIRD class specific functions

#' Solves ODEs of the SEIRD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' @export
setGeneric(name = "run",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda", ...) {
               standardGeneric("run")})

#' @describeIn SEIRD Solves ODEs of the SEIRD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' @aliases run,ANY,ANY-method
#' @export
setMethod(
    "run", "SEIRD",
    function(object, times, solve_method = "lsoda") {
        if (!is.double(times)) {
            stop("Evaluation times of the model storage format must be a vector.")
        }
        
        if (is.null(unlist(object@transmission_parameters)))
            stop("Transmission parameters must be set before running.")
        if (is.null(unlist(object@initial_conditions)))
            stop("Initial conditions must be set before running.")
        
        # set initial state vector
        state <- c(S = initial_conditions(object)$S0,
                   E = initial_conditions(object)$E0,
                   I = initial_conditions(object)$I0,
                   R = initial_conditions(object)$R0,
                   C = 0,
                   D = 0)
        # set transmission parameters vector
        parameters <- c(b = transmission_parameters(object)$beta,
                        k = transmission_parameters(object)$kappa,
                        g = transmission_parameters(object)$gamma,
                        m = transmission_parameters(object)$mu)
        # function for RHS of ode system
        right_hand_side <- function(t, state, parameters) {
            with(
                as.list(c(state, parameters)), {
                    s <- state[1]
                    e <- state[2]
                    i <- state[3]
                    r <- state[4]
                    c <- state[5]
                    d <- state[6]
                    # rate of change
                    ds <- -b * s * i
                    de <- b * s * i - k * e
                    di <- k * e - (g + m) * i
                    dr <- g * i
                    dc <- b * s * i
                    d_death <- m * i
                    # return the rate of change
                    list(c(ds, de, di, dr, dc, d_death))
                })
        }
        
        # call ode solver
        out <- ode(
            y = state, times = times, func = right_hand_side,
            parms = parameters, method = solve_method)
        
        output <- as.data.frame.array(out)
        
        # Compute incidences and deaths
        cases <- c(0, diff(output$C))
        deaths <- c(0, diff(output$D))
        output$Incidence <- cases
        output$Deaths <- deaths
        output <- output[, c("time", unlist(object@output_names))]
        
        # Create long format of output
        output <- melt(output, id.vars = "time")
        output <- output[, c("time", "value", "variable")]
        names(output) <- c("time", "value", "compartment")
        
        # Added for consistency of output format across models
        output$age_range <- rep("0-150", length(output$time))
        
        # Split output into 2 dataframes: one with S,E,I, and R and one with C and D
        states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
        states <- droplevels(states)
        changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
        changes <- droplevels(changes)
        
        list("states" = states, "changes" = changes)
    })

#' Calculates basic reproduction number
#'
#' @param model a model object from comomodels package
#'
#' @return an R0 value
#' @export
setGeneric("R0", def = function(model) {
    standardGeneric("R0")
})

#' @describeIn SEIRD Calculates basic reproduction number for SEIRD model
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \beta/(\gamma + \mu)}
#'
#' @param model an SEIRD model
#'
#' @return an R0 value
#' @export
#' @aliases R0,ANY,ANY-method
setMethod("R0", "SEIRD", function(model) {
    beta <- model@transmission_parameters$beta
    gamma <- model@transmission_parameters$gamma
    mu <- model@transmission_parameters$mu
    beta / (gamma + mu)
})



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SEIaImIsRD
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' SEIaImIsRD: an SEIRD model for compartments of different symptoms (asymptomatic, mild and severe) with different transmission rates (beta).
#'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' An S4 object representing the SEIaImIsRD model: SEIRD with infected population 
#' that are composed of different symptome compartments: asymptomatic, mild and severe (current settings).
#'
#' This class represents an extension of the SEIRD model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time, in which the infectious individuals
#' are subgrouped into compartments according to different severity of symptoms, i.e. asymptomatic, mild and severe.
#'
#' 1. Total initial population size is normalised to 1.
#' 2. The current model does not include natural death or birth.
#' 3. The current model defines different infection.
#'
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I_asymptomatic0", "I_mild0", "I_severe0", "R0", "D0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "p_symptom", "gamma", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters (double).
#' @slot R0 basic reproduction number (double).
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot output a list of two dataframes of the model simulation result (1. Cumulative population sizes of 
#'       compartments S, E, I.., R, D; 2. Daily Incidence and deaths).
#'
#' @import deSolve
#' @import ggplot2
#' @import reshape2
#'
#' @concept objects
#' @export SEIaImIsRD
SEIaImIsRD <- setClass(Class = "SEIaImIsRD",
         slots = c(
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           R0 = "numeric",
           output_names = "list",
           output = "list"
         ),
         prototype = list(
           initial_condition_names = list("S0", "E0", "I_asymptomatic0",
                                          "I_mild0", "I_severe0", "R0", "D0"),
           transmission_parameter_names = list("beta", "kappa", "omega",
                                              "p_symptom", "gamma", "mu"),
           initial_conditions = vector(mode = "list", length = 7),
           transmission_parameters = vector(mode = "list", length = 6),
           R0 = NA_real_,
           output_names = list("S", "E", "I_asymptomatic",
                               "I_mild", "I_severe", "R", "D",
                               "Incidence", "Deaths"),
           output = list("states" = data.frame(), "changes" = data.frame())
         ))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions
# Generics defined in SEIRD will only be reset methods here to avoid ambiguity.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' @describeIn SEIaImIsRD Retrieves initial conditions of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
#' @aliases initial_conditions,SEIaImIsRD-method
#' @export
setMethod("initial_conditions", "SEIaImIsRD",
          function(object) object@initial_conditions)


#' @describeIn SEIaImIsRD Retrieves transmission parameters of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
#' @aliases initial_conditions,SEIaImIsRD-method
#' @export
setMethod("transmission_parameters", "SEIaImIsRD",
          function(object) object@transmission_parameters)



#' @describeIn SEIaImIsRD Setter method for initial population sizes (in fraction)
#' of the SEIaImIsRD model.
#'
#' @param object An object of class SEIaImIsRD
#' @param value A numeric named list containing the initial fraction of population groups:
#' S - susceptible,
#' E - exposed,
#' I_asymptomatic - infected with no symptom,
#' I_mild - infected with mild symptoms,
#' I_severe - infected severe symptoms that need further hospitalization,
#' D - dead due to the infection,
#' R - recovered.
#'
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @aliases initial_conditions<-,SEIaImIsRD
#' @export
#'
setMethod(
  "initial_conditions<-",
  "SEIaImIsRD",
  function(object,
           value = list(S = NA_real_, E = NA_real_,
           I_asymptomatic = NA_real_, I_mild = NA_real_, I_severe = NA_real_,
           R = NA_real_,
           D = NA_real_)) {
    init_pop_list <- value
    names(init_pop_list) <- object@initial_condition_names
    object@initial_conditions <- init_pop_list
    # check if values are valid
    errors <- character()
    # check whether all required initial population groups are set
    is_na_pop <- is.na(init_pop_list)
    if (sum(is_na_pop) != 0) {
      msg <- paste("Missing initial setting for population group:",
                   paste(names(init_pop_list)[is_na_pop], collapse = ", ")
      )
      errors <- c(errors, msg)
    }else{
      # check whether the sum of initial population is normalized to 1
      sum_init_pop <- sum(unlist(init_pop_list))
      if (sum_init_pop != 1) {
        msg <- "Sum of initial population is not 1, please normalize"
        errors <- c(errors, msg)
      }
    }
    if (length(errors) == 0) {
      object@initial_conditions <- init_pop_list
      object
      }
    else stop(paste(errors, ", please check and rerun transmission_parameters<-.\n"))
  })


#' @describeIn SEIaImIsRD Setter method for transmission parameters
#' of the SEIaImIsRD model.
#'
#' @param object An object of class SEIaImIsRD
#' @param value A numeric named list of values for transmission parameters:
#' beta - a list of the effective contact rate from each infected group (i.e. rate at which an infected individual exposes susceptible),
#' kappa - rate of progression from exposed to infectious (the reciprocal is the incubation period),
#' omega - rate at which recovered individuals become susceptible,
#' p_symptom - a list of fraction of different infected groups,
#' gamma - a list of the rate of removal of each infected group (i.e. recovery rate of an infected individual),
#' mu - a list of the rate of disease-caused mortality of each infected group
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @aliases transmission_parameters<-,SEIaImIsRD-method
#' @export
#'
setMethod(
  "transmission_parameters<-",
  "SEIaImIsRD",
  function(
    object,
    value = list(beta = list(), kappa = NA_real_, omega = NA_real_,
    p_symptom = list(), gamma = list(), mu = list())) {
    param_list <- value
    names(param_list) <- object@transmission_parameter_names
    object@transmission_parameters <- param_list
    # check if values are valid
    errors <- character()
    # check whether all required parameters are set
    is_na_params <- is.na(param_list)
    if (sum(is_na_params) != 0) {
      msg <- paste("Missing parameters:",
                   paste(names(param_list)[is_na_params], collapse = ", ")
      )
      errors <- c(errors, msg)
    }
    # check whether the lengths of beta, gamma and mu correspond to the number of infected groups
    n_beta <- length(param_list$beta)
    if (n_beta != 3) {
      msg <- paste0(
        "Length of parameter beta,", n_beta, ", is not equal to the setting ", 3)
      errors <- c(errors, msg)
    }
    n_gamma <- length(param_list$gamma)
    if (n_gamma != 3) {
      msg <- paste0(
        "Length of parameter gamma,", n_gamma, ", is not equal to the setting ", 3)
      errors <- c(errors, msg)
    }
    n_mu <- length(param_list$mu)
    if (n_mu != 3) {
      msg <- paste0(
        "Length of parameter mu,", n_mu, ", is not equal to the setting ", 3)
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) {
      object@transmission_parameters <- param_list
      object
      }
    else stop(paste(errors, ", please check and rerun transmission_parameters<-.\n"))
    })

#' Solves the ode system.
#'
#' \deqn{\frac{dS(t)}{dt} = -S(t)(\beta.i_{asymptomatic} I_{asymptomatic}(t) + \beta.i_{mild} I_{mild}(t) + \beta.i_{severe} I_{severe}(t)) + \omega R(t)}
#' \deqn{\frac{dE(t)}{dt} = S(t)(\beta.i_{asymptomatic} I_{asymptomatic}(t) + \beta.i_{mild} I_{mild}(t) + \beta.i_{severe} I_{severe}(t)) - \kappa E(t)}
#' \deqn{\frac{dI_{asymptomatic}(t)}{dt} = p_symptom.i_{asymptomatic} \kappa E(t) - (\gamma.i_{asymptomatic} + \mu.i_{asymptomatic}) I_{asymptomatic}(t)}
#' \deqn{\frac{dI_{mild}(t)}{dt} = p_symptom.i_{mild}\kappa E(t) - (\gamma.i_{mild} + \mu.i_{mild}) I_{mild}(t)}
#' \deqn{\frac{dI_{severe}(t)}{dt} = p_symptom.i_{severe}\kappa E(t) - (\gamma.i_{severe} + \mu.i_{severe}) I_{severe}(t)}
#' \deqn{\frac{dR(t)}{dt} = -\omega R(t) + \gamma.i_{mild} I_{mild}(t) + \gamma.i_{severe} I_{severe}(t)}
#' \deqn{\frac{dD(t)}{dt} =  \mu.i_{asymptomatic} I_{asymptomatic}(t) + \mu.i_{mild} I_{mild}(t) + \mu.i_{severe} I_{severe}(t)}
#'
#' @param object An object of class SEIaImIsRD
#' @param times A list of time points of the simulation period
#' @param solve_method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return An object of class SEIaImIsRD with a dataframe of simulation output
#' @rdname SEIaImIsRD-class
#' @aliases run,SEIaImIsRD-method
#' @export
#'
setMethod("run",
          "SEIaImIsRD",
          def = function(
            object,
            times,
            solve_method = "lsoda") {
            # initial population groups
            pop_groups <- c(S = object@initial_conditions$S,
                            E = object@initial_conditions$E,
                            I_asymptomatic = object@initial_conditions$I_asymptomatic,
                            I_mild = object@initial_conditions$I_mild,
                            I_severe = object@initial_conditions$I_severe,
                            R = object@initial_conditions$R,
                            D = object@initial_conditions$D,
                            C = 0)
            # parameters
            params <- c(beta = object@transmission_parameters$beta,
                        kappa = object@transmission_parameters$kappa,
                        omega = object@transmission_parameters$omega,
                        p_symptom = object@transmission_parameters$p_symptom,
                        gamma = object@transmission_parameters$gamma,
                        mu = object@transmission_parameters$mu)

            # ODE system RHS
            ode_symptome_rhs <- function(t, pop_groups, parameters) {
              with(
                as.list(c(pop_groups, params)), {
                  dS <- -S * (beta.i_asymptomatic * I_asymptomatic + beta.i_mild * I_mild + beta.i_severe * I_severe) + omega * R
                  dE <- S * (beta.i_asymptomatic * I_asymptomatic + beta.i_mild * I_mild + beta.i_severe * I_severe) - kappa * E
                  dI_a <- (1.0 - p_symptom.i_mild - p_symptom.i_severe) * kappa * E - (gamma.i_asymptomatic + mu.i_asymptomatic) * I_asymptomatic
                  dI_m <- p_symptom.i_mild * kappa * E - (gamma.i_mild + mu.i_mild) * I_mild
                  dI_s <- p_symptom.i_severe * kappa * E - (gamma.i_severe + mu.i_severe) * I_severe
                  dR <- -omega * R + gamma.i_asymptomatic * I_asymptomatic + gamma.i_mild * I_mild + gamma.i_severe * I_severe
                  dD <-  mu.i_asymptomatic * I_asymptomatic + mu.i_mild * I_mild + mu.i_severe * I_severe
                  dC <- S * (beta.i_asymptomatic * I_asymptomatic + beta.i_mild * I_mild + beta.i_severe * I_severe)
                  # return the rate of cI_severenge
                  list(c(dS, dE, dI_a, dI_m, dI_s, dR, dD, dC))
                })
            }
            # solving ode
            output <- ode(y = pop_groups,
                          times = times,
                          func = ode_symptome_rhs,
                          parms = params,
                          method = solve_method)
            output <- as.data.frame(output)
            
            
            # Compute incidences and deaths
            cases <- c(0, diff(output$C))
            deaths <- c(0, diff(output$D))
            output$Incidence <- cases
            output$Deaths <- deaths
            output <- output[, c("time", unlist(object@output_names))]
            
            # Create long format of output
            output <- melt(output, id.vars = "time")
            output <- output[, c("time", "value", "variable")]
            names(output) <- c("time", "value", "compartment")

            # Added for consistency of output format across models
            output$age_range <- rep("0-150", length(output$time))

            # Split output into 2 dataframes: one with S,E,I, and R and one with C and D
            states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
            states <- droplevels(states)
            changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
            changes <- droplevels(changes)
            # reshape data frame into long format
            # output <- melt(output, id.vars = "time")
            # names(output) <- c("time", "population_group", "fraction")
            # names(output) <- c("time", "compartment", "value")
            object@output <- list("states" = states, "changes" = changes)
            return(object)
          })
 

#' Calculate the basic reproduction number (\code{R_0}) of the system using the next generation matrix approach.
#' @seealso \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6002118/pdf/main.pdf} mathematical
#' details of the next generation matrix approach.
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \rho(FV^{-1})},
#' where \deqn{F=\frac{\partial F_i{(x_0)}}{\partial x_j}} and \deqn{V=\frac{\partial V_i{(x_0)}}{\partial x_j}}
#' and \code{rho} represents the spectral radius of a matrix (i.e. the largest absolute of eigenvalue).
#' The \code{F_i} are the new infections, while the \code{V_i} transfers of infections from one compartment to
#' another. \code{x_0} is the disease-free equilibrium state.
#'
#' @param model A model of an SEIaImIsRD class object with initial_conditions and transmission_parameters set.
#' @return A SEIaImIsRD class object with R0 value stored
#' @rdname SEIaImIsRD-class
#' @aliases R0,SEIaImIsRD-method
#' @export
#'
setMethod("R0", "SEIaImIsRD", function(model) {
  # get required parameters:
  S <- model@initial_conditions$S
  beta <- model@transmission_parameters$beta
  kappa <- model@transmission_parameters$kappa
  p_symptom <- model@transmission_parameters$p_symptom
  gamma <- model@transmission_parameters$gamma
  mu <- model@transmission_parameters$mu
  # define matrices F and V:
  F <- matrix(0, 4, 4)
  V <- matrix(0, 4, 4)
  F[1, 2:4] <-  c(S * beta$i_asymptomatic, S * beta$i_mild, S * beta$i_severe)
  V[1, 1] <- kappa
  V[2, 1] <- -kappa * (1 - p_symptom$i_mild - p_symptom$i_severe)
  V[2, 2] <- gamma$i_asymptomatic + mu$i_asymptomatic
  V[3, 1] <- -kappa * p_symptom$i_mild
  V[3, 3] <- gamma$i_mild + mu$i_mild
  V[4, 1] <- -kappa * p_symptom$i_severe
  V[4, 4] <- gamma$i_severe + mu$i_severe
  # calculate R0 as the spectral radius for the matrix F x V^(-1):
  eigVals <- eigen(F %*% (solve(V)))$values
  model@R0 <- max(abs(eigVals))
  return(model)
  })


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# SEIRDAges
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' An S4 object representing the SEIRDAge.
#' 
#' Defines an age-structured SEIRD model and solves the set of
#' ordinary differential equations of the model with a chosen method of
#' numerical integration.
#'
#' @slot output_names names of the compartments which are used by the
#'     model.
#' @slot initial_condition_names names of the initial conditions used by the
#'     model.
#' @slot transmission_parameter_names names of the transmission parameters used
#'     by the model.
#' @slot initial_conditions named list containing the initial conditions of the
#'     model. Initial values for each compartment, S0, E0, I0, R0, D0.
#' @slot transmission_parameters named list containing the transmission
#'     parameters of the model. Transmission parameters b, k, g represent the
#'     rates of changes between the compartments.
#' @slot contact_matrix A square matrix with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns)
#' @slot n_age_categories number of age categories.
#' @slot age_ranges list of string characters representing the range of ages of
#'     people in each age category. This object must have length
#'     \code{n_age_categories} (otherwise an error is returned) and each element
#'     must be formatted as 'age1-age2'.
#'
#' @import deSolve
#' @import glue
#' @import tidyverse
#' @import reshape2
#' @importFrom methods new
#' @export SEIRDAge
#' 
SEIRDAge <- setClass('SEIRDAge',
         # slots
         slots = c(
           output_names = 'list',
           initial_condition_names = 'list',
           transmission_parameter_names = 'list',
           initial_conditions = 'list',
           transmission_parameters = 'list',
           age_ranges = 'list',
           n_age_categories = 'numeric',
           contact_matrix = 'matrix'
         ),

         # prototypes for the slots, automatically set output and param
         # names
         prototype = list(
           output_names = list('S', 'E', 'I', 'R', 'D' ,'Incidence'),
           initial_condition_names = list('S0', 'E0', 'I0', 'R0', 'D0'),
           transmission_parameter_names = list('b', 'k', 'g', 'mu'),
           initial_conditions = vector(mode = "list", length = 5),
           transmission_parameters = vector(mode = "list", length = 4),
           age_ranges = vector(mode = 'list'),
           n_age_categories = NA_real_,
           contact_matrix = matrix(NA)

         )
)

# Setter and getter methods for initial_conditions of an age-structured
# SEIRD model.

#' @describeIn SEIRDAge Retrieves initial_conditions for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDAge.
#'
#' @return Initial conditions of SEIRDAge model.
#' @export
#' @aliases initial_conditions,ANY,ANY-method
#' 
setMethod('initial_conditions', 'SEIRDAge',
          function(object) object@initial_conditions)

#' @describeIn SEIRDAge Sets initial_conditions of an age-structured
#' SEIRD model.
#'
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class SEIRDAge.
#' @param value a named list of (S0, E0, I0, R0) where each element can be a list
#' of vector of doubles, with each element corresponding to the fraction for a
#' single age group.
#'
#' @return Updated version of the age-structured SEIRD model.
#' @export
#' @aliases initial_conditions<-,ANY,ANY-method
#' 
setMethod(
  'initial_conditions<-', 'SEIRDAge',
  function(object, value) {
    S0 = value$S0
    E0 = value$E0
    I0 = value$I0
    R0 = value$R0
    D0 = value$D0
    # check that ICs are valid
    if (abs(sum(S0, E0, I0, R0, D0)-1)>=10^(-3)) {
      stop('Invalid initial conditions. Must sum to 1.')
    }

    # create list of parameter values
    ic <- list(S0, E0, I0, R0, D0)

    # add names to each value
    names(ic) = object@initial_condition_names

    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list('S0', 'E0', 'I0', 'R0', 'D0')){
      if(length(ic[[p]]) != object@n_age_categories){
        stop(glue('Wrong number of age groups for {p}
              compartments.'))}
      if(!is.numeric(ic[[p]])){
        stop(glue('{p} format must be numeric'))}
    }
    if(abs(sum(S0, E0, I0, R0, D0)-1)>=10^(-3)){
      stop('All compartments need to sum up to 1.')
    }

    # if all above tests are passed, assign the ic namelist to the object
    object@initial_conditions <- ic

    return(object)
  })

# Setter and getter methods for transmission_parameters of an age-structured
# SEIRD model.

#' @describeIn SEIRDAge Retrieves transmission_parameters for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDAge.
#'
#' @return Transmission parameters of SEIRDAge model.
#' @export
#' @aliases transmission_parameters,ANY,ANY-method
#' 
setMethod('transmission_parameters', 'SEIRDAge',
          function(object) object@transmission_parameters)

#' @describeIn SEIRDAge Sets transmission_parameters of an
#' age-structured SEIRD model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param value a named list of form list(b=, k=, g=, mu=)
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return Updated version of the age-structured SEIRD model.
#' @export
#' @aliases transmission_parameters<-,ANY,ANY-method
#' 
setMethod(
  'transmission_parameters<-', 'SEIRDAge',
  function(object, value) {

    # create list of parameter values
    b <- value$b
    k <- value$k
    g <- value$g
    mu <- value$mu
    
    trans_params <- list(b, k, g, mu)

    # add names to each value
    names(trans_params) = object@transmission_parameter_names

    # check format of parameters b, k and g
    if(length(b) != 1 | length(k) != 1 | length(g) != 1){
      stop('The parameter values should be 1-dimensional.')
    }
    
    # Set the row and column names of the instance's contact matrix
    rownames(object@contact_matrix) <- object@age_ranges
    colnames(object@contact_matrix) <- object@age_ranges

    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params

    return(object)
  })

#' @describeIn SEIRDAge Method to simulate output using from model.
#' 
#' Solves a system of ODEs which form an
#' age-structured SEIRD model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R) and Dead (D) groups in a given age group indexed by i is 
#' given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)}
#' \deqn{\frac{dE_i(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)} - \kappa E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t)} - \mu I_i(t)}
#' \deqn{\frac{dR_i(t)}{dt} = \gamma I_i(t)}
#' \deqn{\frac{dD_i(t)}{dt} = \mu I_i(t)}

#' where C is a contact matrix whose elements represents the
#' contact between different age groups (rows) with age groups of
#' people they come in contact with (columns). This function relies on the 
#' package deSolve to numerically integrate the set of equations above.
#' 
#'
#' @param object An object of the class SEIRDAge.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step). Default time series
#'        is seq(0, 100, by = 1).
#' @param solve_method A string indicating the chosen numerical integration
#' method for solving the ode system. Default is `lsoda` which is also the
#' default for the ode function in the deSolve package used in this function.
#'
#' @return data frame containing the time vector and time series of S, R, I and
#' D population fractions for each age group outputs with incidence numbers
#' for each age group.
#' 
#' 
#' @aliases run,ANY,ANY-method
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
setMethod(
  "run", 'SEIRDAge',
  function(object, times, solve_method = 'lsoda') {

    # error if times is not a vector or list of doubles
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    if(!is.numeric(times)){
      stop('Evaluation times of the model storage format must numeric.')
    }
    if(!(all(diff(times) > 0))){
      stop('Evaluation times of the model storage format must be increasing')
    }
    
    #fetch number of age catagories
    n_age <- object@n_age_categories

    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               D = initial_conditions(object)$D0,
               cc = rep(0, n_age))

    # set parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    mu = transmission_parameters(object)$mu)
    
    # fetch contact matrix of the instance
    C = object@contact_matrix

    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)),
        {
          S <- state[1:n_age]
          E <- state[(n_age + 1):(2 * n_age)]
          I <- state[(2 * n_age + 1):(3 * n_age)]
          R <- state[(3 * n_age + 1):(4 * n_age)]
          D <- state[(4 * n_age + 1):(5 * n_age)]
          cc <- state[(5 * n_age + 1):(6 * n_age)]
          
          
          # rate of change
          dS <- -b * S * C %*% I
          dE <- b * S * C %*% I - k * E
          dI <- k * E - g * I  - mu * I
          dR <- g * I
          dD <- mu * I
          dcc <- b * S * C %*% I
          # return the rate of change
          list(c(dS, dE, dI, dR, dD, dcc))
        })
    }

    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)

    #output as a dataframe
    output <- as.data.frame.array(out)

    # melt dataframe to wide format
    out_temp = melt(output, 'time')

    # add compartment and age range columns
    n_compartment_measurements <- length(times) * n_age
    out_temp$compartment = c(replicate(n_compartment_measurements, "S"),
                             replicate(n_compartment_measurements, "E"),
                             replicate(n_compartment_measurements, "I"),
                             replicate(n_compartment_measurements, "R"),
                             replicate(n_compartment_measurements, "D"),
                             replicate(n_compartment_measurements, "cc"))

    out_temp$age_range = unlist(rep(object@age_ranges, each=length(times)))

    # drop the old variable column
    out_temp = out_temp %>% 
      dplyr::select(-.data$variable) %>% 
      dplyr::mutate(compartment=as.factor(compartment)) %>% 
      dplyr::mutate(compartment=forcats::fct_relevel(compartment, "S", "E", "I", "R", "D", "cc")) %>% 
      dplyr::mutate(age_range=as.factor(age_range)) %>% 
      dplyr::mutate(age_range=forcats::fct_relevel(age_range, object@age_ranges))

    # compute incidence and deaths
    changes <- out_temp %>% 
      dplyr::filter(compartment %in% c("cc", "D")) %>% 
      dplyr::group_by(compartment, age_range) %>% 
      dplyr::mutate(value = c(0, diff(value))) %>% 
      dplyr::mutate(compartment = dplyr::if_else(compartment == "cc", "Incidence",
                                                 "Deaths")) %>% 
      dplyr::ungroup() %>% 
      as.data.frame()
    
    # remove cumulative cases column from state vector
    states = out_temp %>% 
      dplyr::filter(compartment != "cc") %>% 
      droplevels() %>% 
      dplyr::ungroup()

    return(list("states" = states, "changes" = changes))
  })


#' # describeIn SEIaImIsRD Plot the outcome of the ode similuation (for any dataframe)
#'
#' @param dataframe A dataframe in long format
#' @param x The variable in the dataframe to be mapped to the x axis in ggploty aesthetics. Default is set to "time".
#' @param y The variable in the dataframe to be mapped to the y axis in ggploty aesthetics. Default is set to "value".
#' @param c The variable in the dataframe to be plotted in different lines with different colours. Default is set to "compartment".
#'
#' @return A ggplot
#' @rdname plot_dataframe
#' @export
#' @aliases plot_dataframe,ANY,ANY-method
#'
plot_dataframe <- function(dataframe, x = "time", y = "value", c = "compartment") {
    p <- ggplot(dataframe, aes_string(x = x, y = y)) +
        geom_line(aes_string(colour = c)) +
        theme_classic()
    return(p)
}

