#' @include generics.R
#'
NULL


##=====================================================##
##===================== SEIRD =========================##
##=====================================================##

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


#' @describeIn SEIRD Retrieves initial conditions of SEIRD model.
#'
#' @param object An object of the class SEIRD.
#' 
#' @export
setMethod("initial_conditions", "SEIRD",
          function(object) object@initial_conditions)


#' @describeIn SEIRD Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD.
#' 
#' @export
setMethod("transmission_parameters", "SEIRD",
          function(object) object@transmission_parameters)

# SEIRD class specific functions

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
#' 
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
#' for solving the ode system. Default is lsoda which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
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

#' @describeIn SEIRD Calculates basic reproduction number for SEIRD model
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \beta/(\gamma + \mu)}
#'
#' @param model an SEIRD model
#'
#' @return an R0 value
#' 
#' @export
setMethod("R0", "SEIRD", function(model) {
  beta <- model@transmission_parameters$beta
  gamma <- model@transmission_parameters$gamma
  mu <- model@transmission_parameters$mu
  beta / (gamma + mu)
})

##=====================================================##
##=================== SEIRDAge ========================##
##=====================================================##

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
#' 
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
    
    if(length(mu) != 1 & length(mu) != object@n_age_categories){
      stop('The mortality parameter values should be of length 1 or
            number of age classes.')
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
#' \deqn{\frac{dE_i(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t) - \kappa E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t) - \mu I_i(t)}
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
#' @export
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
      dplyr::mutate(compartment=as.factor(.data$compartment)) %>% 
      dplyr::mutate(compartment=forcats::fct_relevel(.data$compartment, "S", "E", "I", "R", "D", "cc")) %>% 
      dplyr::mutate(age_range=as.factor(.data$age_range)) %>% 
      dplyr::mutate(age_range=forcats::fct_relevel(.data$age_range, object@age_ranges))

    # compute incidence and deaths
    changes <- out_temp %>% 
      dplyr::filter(.data$compartment %in% c("cc", "D")) %>% 
      dplyr::group_by(.data$compartment, .data$age_range) %>% 
      dplyr::mutate(value = c(0, diff(.data$value))) %>% 
      dplyr::mutate(compartment = dplyr::if_else(.data$compartment == "cc", "Incidence",
                                                 "Deaths")) %>% 
      dplyr::ungroup() %>% 
      as.data.frame()
    
    # remove cumulative cases column from state vector
    states = out_temp %>% 
      dplyr::filter(.data$compartment != "cc") %>% 
      droplevels() %>% 
      dplyr::ungroup()
    
    return(list("states" = states, "changes" = changes))
  })



##=====================================================##
##===================== SEIRDV ========================##
##=====================================================##

#' An S4 object representing the SEIRDV.
#'
#' This class represents the SEIRV model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time. Vaccinated
#' individuals are considered in their own compartment
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", R0", "V0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "gamma", "mu",  "nu",
#'       "delta_V", "delta_R").
#' @slot intervention_parameter_names list of names of intervention parameters
#'       (characters). Default is list ("starts", "stops", "coverages")
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#' @slot intervention_parameters list of values for intervention parameters
#'       (double).
#'
#' @import deSolve
#' @import tidyverse
#' @import glue
#' @import reshape2
#'
#' @export SEIRDV
SEIRDV <- setClass("SEIRDV",
                  # slots
                  slots = c(
                    output_names = "list",
                    initial_condition_names = "list",
                    transmission_parameter_names = "list",
                    intervention_parameter_names = "list",
                    initial_conditions = "list",
                    transmission_parameters = "list",
                    intervention_parameters = "list"
                  ),
                  # prototypes for the slots, automatically set parameter names and
                  # its data type
                  prototype = list(
                    output_names = list("S", "E", "I", "R", "V", "D", "Incidence", "Deaths"),
                    initial_condition_names = list("S0", "E0", "I0", "R0", "V0"),
                    transmission_parameter_names = list("beta", "kappa", "gamma",
                                                        "mu", "nu", "delta_V",
                                                        "delta_R"),
                    intervention_parameter_names = list("starts", "stops", "coverages"),
                    initial_conditions = vector(mode = "list", length = 5),
                    transmission_parameters = vector(mode = "list", length = 7),
                    intervention_parameters = vector(mode = "list", length = 3)
                  )
)

#' @describeIn SEIRDV Retrieves initial conditions of SEIRV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setMethod("initial_conditions", "SEIRDV",
          function(object) object@initial_conditions)

#' @describeIn SEIRDV Retrieves transmission parameters of SEIRV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setMethod("transmission_parameters", "SEIRDV",
          function(object) object@transmission_parameters)

#' @describeIn SEIRDV Setter method for initial conditions (S0, E0, I0, R0 and V0)
#' of the SEIRV model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRDV
#' @param value (list) list of initial conditions S0, E0, I0, R0, V0.
#'
#' @return object of class SEIRDV with initial conditions assigned.
#' 
#' @export
setMethod(
  "initial_conditions<-", "SEIRDV",
  function(object, value) {
    
    if (mean(names(value) %in% object@initial_condition_names) != 1)
      stop(paste0("Initial conditions must contain: ",
                  object@initial_condition_names))
    init_cond <- value
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S0", "E0", "I0", "R0", "V0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    
    # check that the initial conditions are properly normalized
    if (init_cond$S0 + init_cond$E0 + init_cond$I0 +
        init_cond$R0 + init_cond$V0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    object@initial_conditions <- init_cond
    
    object
  })

#' @describeIn SEIRDV Set transmission parameters (beta, kappa, gamma, mu, nu,
#' delta_V, delta_R) of the SEIRV model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRDV model)
#' @param value (list) list of values for beta, kappa, gamma, mu, nu, delta_V and
#' delta_R respectively.
#'
#' @return object of class SEIRDV with transmission parameter values
#' assigned.
#' 
#' @export
setMethod(
  "transmission_parameters<-", "SEIRDV",
  function(object, value) {
    
    # create list of parameter values
    if (mean(names(value) %in% object@transmission_parameter_names) != 1)
      stop(paste0("Transmission parameters must contain: ",
                  object@transmission_parameter_names))
    trans_params <- value
    
    # check format of parameters
    if (length(trans_params$beta) != 1
        | length(trans_params$kappa) != 1
        | length(trans_params$gamma) != 1
        | length(trans_params$mu) != 1
        | length(trans_params$nu) != 1
        | length(trans_params$delta_V) != 1
        | length(trans_params$delta_R) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    object
  })

# SEIRDV class specific functions

#' Retrieves intervention parameters of SEIRDV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setGeneric("intervention_parameters",
           function(object) standardGeneric("intervention_parameters"))


#' @describeIn SEIRDV Retrieves intervention parameters of SEIRDV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setMethod("intervention_parameters", "SEIRDV",
          function(object) object@intervention_parameters)

#' Set intervention parameters of the SEIRV model.
#'
#' Intervention parameters have same size. This class is designed for interventions
#' which last several days at least and have several days between them.
#'
#' @param object an object of the class SEIRDV
#' @param value (list) list of intervention parameters: starts, stops and
#'              coverages.
#'
#' @return object of class SEIRDV with intervention parameters assigned.
#' 
#' @export
setGeneric(
  "intervention_parameters<-",
  function(object, value) {
    standardGeneric("intervention_parameters<-")
  })

#' @describeIn SEIRDV Setter method for intervention parameters of the SEIRV model.
#'
#' Intervention parameters have same size. A tanh function is used to smooth interventions during simulation. This class is designed for interventions
#' which last several days at least and have several days between them; interventions involving rapid fluctuations may be distorted.
#'
#' @param object an object of the class SEIRDV
#' @param value (list) list of intervention parameters: starts, stops and
#'              coverages.
#'
#' @return object of class SEIRDV with intervention parameters assigned.
#' 
#' @export
setMethod(
  "intervention_parameters<-", "SEIRDV",
  function(object, value) {
    
    if (mean(names(value) %in% object@intervention_parameter_names) != 1)
      stop(paste0("Intervention parameters must contain: ",
                  object@intervention_parameter_names))
    interv_par <- value
    
    # raise errors if intervention parameters are not doubles
    for (p in list("starts", "stops", "coverages")) {
      if (!is.numeric(interv_par[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    
    # check that the intervention parameters are all of the same size
    if (length(interv_par$starts) != length(interv_par$stops)|
        length(interv_par$starts) != length(interv_par$coverages)|
        length(interv_par$coverages) != length(interv_par$stops)) {
      stop("Invalid intervention parameters. Must have same size.")
    }
    
    object@intervention_parameters <- interv_par
    
    object
  })

#' @describeIn SEIRDV Solves ODEs of the SEIRDV specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t) - nu Inter(t) S(t) + delta_V V(t) + delta_R R(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t) - delta_R R(t)}
#' \deqn{\frac{dV(t)}{dt} = nu Inter(t) S(t) - delta_V V(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' where Inter(t) is the value at time t of the intervention protocol defined by
#' the intervention parameters. This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRDV
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is lsoda which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I, R and V population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
#' @export
#' 
setMethod(
  "run", "SEIRDV",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    if (is.null(unlist(object@transmission_parameters)))
      stop("Transmission parameters must be set before running.")
    if (is.null(unlist(object@initial_conditions)))
      stop("Initial conditions must be set before running.")
    if (is.null(unlist(object@intervention_parameters)))
      stop("Intervention parameters must be set before running.")
    
    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               V = initial_conditions(object)$V0,
               C = 0,
               D = 0)
    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$beta,
                    k = transmission_parameters(object)$kappa,
                    g = transmission_parameters(object)$gamma,
                    m = transmission_parameters(object)$mu,
                    n = transmission_parameters(object)$nu,
                    d_v = transmission_parameters(object)$delta_V,
                    d_r = transmission_parameters(object)$delta_R)
    
    # set intervention parameters vector
    int_parms <- 
      InterventionParameters(
        start=intervention_parameters(object)$starts,
        stop=intervention_parameters(object)$stops,
        coverage= intervention_parameters(object)$coverages)
    
    # use tstep=0.1 and tanh_slope=1 for good nice step-function-like shape of
    # the intervention wave
    sim_parms <- SimulationParameters(start =0, stop = tail(times, n=1),
                                      tstep = 0.1)
    
    inter_prot <- intervention_protocol(int_parms, sim_parms, 1) %>%
      filter(time %in% times) %>%
      .$coverage
    
    intervention <- approxfun(times, inter_prot, rule=2)
    
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters, input) {
      with(
        as.list(c(state, parameters)), {
          inter <- input(t)
          s <- state[1]
          e <- state[2]
          i <- state[3]
          r <- state[4]
          v <- state[5]
          c <- state[6]
          d <- state[7]
          # rate of change
          ds <- -b * s * i - n * inter * s + d_v * v + d_r * r
          de <- b * s * i - k * e
          di <- k * e - (g + m ) * i
          dr <- g * i - d_r * r
          dv <- n * inter * s - d_v * v
          dc <- b * s * i
          d_death <- m * i
          # return the rate of change
          list(c(ds, de, di, dr, dv, dc, d_death))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method, input = intervention)
    
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
    
    # Split output into 2 dataframes: one with S,E,I,R and V and one with C and D
    states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
    states <- droplevels(states)
    changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
    changes <- droplevels(changes)
    
    list("states" = states, "changes" = changes)
  })

##=====================================================##
##=================== SEIRD_RU ========================##
##=====================================================##

#' An S4 object representing the SEIRD_RU.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time. The model
#' considers these populations in two types of communities: rural and urban.
#' This class considers the model where people DO NOT move between communities
#' but can infect people from the other community.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S_U0", "E_U0", "I_U0", "R_U0", "S_Y0",
#'       "E_Y0", "I_Y0", "R_Y0").
#' @slot initial_cases_deaths_names name for initial cases and deaths
#'       (characters). Default is list("C_U0", "D_U0", "C_Y0", "D_Y0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("b", "k", "g", "m", "C").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot initial_cases_deaths list of values for initial cases and deaths.
#'       Both set to 0, not to be changed by user (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#' @slot contact_rates list of two contact rates, one for urban communities
#'       and one for rural communities, with the number of contacts as a
#'       fraction of the total population size (double).
#' @slot contact_rates_names list of names for two contact rates,
#'       one for urban communities and one for rural communities
#' @slot country_demog list of two vectors, each describing the age breakdown
#'       of the population in the urban and rural communities.
#' @slot country_demog_names list of names for two demographic vectors,
#'       one for urban communities and one for rural communities
#' @slot fraction_rural fraction of the population that lives in a rural
#'      environment.
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#' @importFrom methods new
#' @export SEIRD_RU

SEIRD_RU <- setClass("SEIRD_RU",
         # slots
         slots = c(
           output_names = "list",
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           initial_cases_deaths_names = "list",
           initial_cases_deaths = "list",
           contact_rates = "list",
           contact_rates_names = "list",
           country_demog = "list",
           country_demog_names = "list",
           fraction_rural = "numeric"
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names = list("S_U", "E_U", "I_U", "R_U",
                               "Incidences_U", "Deaths_U",
                               "S_Y", "E_Y", "I_Y", "R_Y",
                               "Incidences_Y", "Deaths_Y"),
           initial_condition_names = list("S_U0", "E_U0", "I_U0", "R_U0",
                                          "S_Y0", "E_Y0", "I_Y0", "R_Y0"),
           initial_cases_deaths_names = list("C_U0", "D_U0", "C_Y0", "D_Y0"),
           transmission_parameter_names = list("b", "k", "g", "m", "C"),
           initial_conditions = vector(mode = "list", length = 8),
           initial_cases_deaths = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 5),
           contact_rates = vector(mode = "list", length = 2),
           contact_rates_names = list("urban", "rural"),
           country_demog = vector(mode = "list", length = 2),
           country_demog_names = list("urban", "rural"),
           fraction_rural = 0.0
         )
)

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Retrieves initial conditions of
#'             SEIRD_RU model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("initial_conditions", "SEIRD_RU",
          function(object) object@initial_conditions)

#-----------------------------------------------------------------------------
#' Retrieves initial cases and deaths of SEIRD_RU model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("initial_cases_deaths",
           function(object) standardGeneric("initial_cases_deaths"))

#' @describeIn SEIRD_RU Retrieves initial cases and deaths of
#'             SEIRD_RU model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("initial_cases_deaths", "SEIRD_RU",
          function(object) object@initial_cases_deaths)

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("transmission_parameters", "SEIRD_RU",
          function(object) object@transmission_parameters)

#-----------------------------------------------------------------------------
#' Retrieves contact rates of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("contact_rates",
           function(object) standardGeneric("contact_rates"))

#' @describeIn SEIRD_RU Retrieves contact rates of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("contact_rates", "SEIRD_RU",
          function(object) object@contact_rates)
#-----------------------------------------------------------------------------
#' Retrieves age demographics, if provided, of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("country_demog",
           function(object) standardGeneric("country_demog"))

#' @describeIn SEIRD_RU Retrieves age demographics, if provided, of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("country_demog", "SEIRD_RU",
          function(object) object@country_demog)

#-----------------------------------------------------------------------------
#' Retrieves fraction of the population that lives in a rural environment
#' of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("fraction_rural",
           function(object) standardGeneric("fraction_rural"))

#' @describeIn SEIRD_RU Retrieves fraction of the population that lives in a
#' rural environment of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("fraction_rural", "SEIRD_RU",
          function(object) object@fraction_rural)

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Setter method for initial conditions
#' (S0, E0, I0 and R0) for both communities in the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD_RU
#' @param value (list) list of initial conditions S_U0, E_U0, I_U0, R_U0, S_Y0,
#' E_Y0, I_Y0, R_Y0.
#'
#' @return object of class SEIRD_RU with initial conditions assigned.
#'
#' @export

setMethod(
  "initial_conditions<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    init_cond <- value

    # add names to each value
    names(init_cond) <- object@initial_condition_names

    # raise errors if initial state and parameter values are not doubles
    for (p in list("S_U0", "E_U0", "I_U0", "R_U0",
                   "S_Y0", "E_Y0", "I_Y0", "R_Y0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }

    # raise errors if initial states are not one value each
    for (p in list("S_U0", "E_U0", "I_U0", "R_U0",
                   "S_Y0", "E_Y0", "I_Y0", "R_Y0")) {
      if (length(init_cond[[p]]) != 1) {
        stop(glue("{p} must be one value"))
      }
    }

    # check that the initial conditions are properly normalized
    if (init_cond$S_U0 + init_cond$E_U0 + init_cond$I_U0 + init_cond$R_U0 +
        init_cond$S_Y0 + init_cond$E_Y0 + init_cond$I_Y0 + init_cond$R_Y0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }

    # if all above tests are passed, assign the init_cond namelist to the object
    # and assign initial cases and deaths
    object@initial_conditions <- init_cond
    init_c0_d0 <- list(init_cond$E_U0 + init_cond$I_U0 + init_cond$R_U0, 0,
                       init_cond$E_Y0 + init_cond$I_Y0 + init_cond$R_Y0, 0)
    names(init_c0_d0) <- object@initial_cases_deaths_names

    object@initial_cases_deaths <- init_c0_d0

    return(object)
  })

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Setter method for transmission parameters
#' (b, k, g, m and C) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of values for b, k, g, m, C,respectively.
#'
#' @return object of class SEIRD_RU with transmission parameter values
#' assigned.
#' @export

setMethod(
  "transmission_parameters<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    trans_params <- value

    # add names to each value
    names(trans_params) <- object@transmission_parameter_names

    # check format of parameters b, k, g, m, C
    if (length(trans_params$b) != 1
        | length(trans_params$k) != 1
        | length(trans_params$g) != 1
        | length(trans_params$m) != 1
        | length(trans_params$C) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }

    # check that the connectedness parameter C is between 0 and 1
    if (trans_params$C < 0 | trans_params$C > 1) {
      stop("Connectedness parameter C must be in the range 0-1.")
    }
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params

    return(object)
  })

#-----------------------------------------------------------------------------
#' Setter method for contact matrices, or contact rates directly, for urban and
#' rural communities in the SEIRD_RU model. Matrices must be of type double.
#' NOTE: if inputting matrices, you MUST assign fraction_rural first, and you 
#' must input full age demographics, i.e. you cannot only provide the fraction
#' of the population that is rural!
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of contact matrices or contact rates.
#'
#' @return object of class SEIRD_RU with contact rates assigned.
#' @export

setGeneric(
  "contact_rates<-",
  function(object, value) {
    standardGeneric("contact_rates<-")
  })

#' @describeIn SEIRD_RU Setter method for contact matrices for
#' urban and rural communities in the SEIR model.
#' Matrices must be of type double.
#' NOTE: if inputting matrices, you MUST assign fraction_rural first, and you 
#' must input full age demographics, i.e. you cannot only provide the fraction
#' of the population that is rural!
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of contact matrices, with urban first, then rural.
#'
#' @return object of class SEIRD_RU with contact matrices
#' assigned.
#' @export

setMethod(
  "contact_rates<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    contact_mat <- value

    # add names to each value
    names(contact_mat) <- object@contact_rates_names

    # check format of contact matrices
    if (typeof(contact_mat$urban) != "double"
        | typeof(contact_mat$rural) != "double") {
      stop("Contact matrices must be of type double.")
    }
    
    #check if input is matrices or numbers, process accordingly
    if (length(contact_mat$urban) > 1) {
      # check if age demographics have been provided
      if(length(country_demog(object)$urban) == 0 || length(country_demog(object)$rural) == 0) {
        stop("If you wish to use contact matrices for the model, you must provide
             vectors of the age demographics of the rural and urban communities
             BEFORE assigning the contact matrices.")
      }
    # compute contacts N_U and N_Y from contact matrices
    # number of urban contacts for an urban individual. The number of contacts
    # has been divided through by the size of the population.
    N_U <- sum(rowSums(contact_mat$urban) *
                 (country_demog(object)$urban / sum(country_demog(object)$urban))) / 2
    # number of rural contacts for a rural individual, where the number of
    # contacts has been divided by the size of the population.
    N_Y <- sum(rowSums(contact_mat$rural) *
                 (country_demog(object)$rural / sum(country_demog(object)$rural))) / 2
    contact_mat <- list(N_U, N_Y)
    names(contact_mat) <- object@contact_rates_names}
    # if all above tests are passed, assign the contact_mat namelist to the
    # object
    object@contact_rates <- contact_mat

    return(object)
  })

#-----------------------------------------------------------------------------
#' Setter method for demographic data for urban and rural communities in the
#' SEIR model. If inputting demographic vectors, both vectors together must sum
#' to 1.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of 2 sets of population fractions by age group, or
#' a single value noting the fraction of the population that is rural.
#'
#' @return object of class SEIRD_RU with  contact matrices
#' assigned.
#' @export

setGeneric(
  "country_demog<-",
  function(object, value) {
    standardGeneric("country_demog<-")
  })

#' @describeIn SEIRD_RU Setter method for demographic data for urban
#' and rural communities in the SEIR model. both vectors together must sum to 1.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of 2 sets of population fractions by age group, or
#' a single value noting the fraction of the population that is rural.
#'
#' @return object of class SEIRD_RU with demographic data
#' assigned.
#' @export

setMethod(
  "country_demog<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    demo_data <- value
    
    if (length(demo_data) == 2 && length(demo_data[[1]]) == 16 && length(demo_data[[2]]) == 16){
      # add names to each value
      names(demo_data) <- object@country_demog_names
  
      # check format of contact matrices
      if (typeof(demo_data$urban) != "double"
          | typeof(demo_data$rural) != "double") {
        stop("Contact matrices must be of type double.")
      }

      # check that vectors sum to 1: losing some precision in division
      # so want to make sure it is accurate to 0.1% of the population
      if (1 - sum(demo_data$urban) - sum(demo_data$rural) > 0.001) {
        stop("Sum over all age groups and both communities must be 1. The
             difference between 1 and the sum over age groups and over both
             communities is more than 0.001.")
      }
      
      #fraction of the population that is rural
      f_rural <- sum(demo_data$rural)
      # if all above tests are passed, assign the contact_mat namelist to the
      # object
      object@country_demog <- demo_data
      object@fraction_rural <- f_rural
    }
    else {
      if (length(demo_data) != 1) {
        stop("The entered demographic data must be either two vectors, or one 
             value.")
      }
      if (typeof(demo_data) != "double") {
        stop("Contact matrices or fraction must be of type double.")
      }
      if (demo_data >= 1 || demo_data <= 0) {
        stop("Frastion of the population that is rural must be more than 0 and
             less than 1.")
      }
      object@fraction_rural <- demo_data
    }
    
    return(object)
  })

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Solves ODEs of the SEIRD_RU specified
#' in object for the time points specified in times and integration method
#' specified in solve_method.
#'
#' For the urban community:
#' \deqn{\frac{dS_U(t)}{dt} = - b S_U (I_U + I_Y) N_U C
#'                            - b S_U/f_urban I_U/f_urban N_U (1-C)}
#' \deqn{\frac{dE_U(t)}{dt} =  b S_U (I_U + I_Y) N_U C
#'                            + b S_U/f_urban I_U/f_urban N_U (1-C) -k E_U}
#' \deqn{\frac{dI_U(t)}{dt} = k E_U - (g + m) I_U}
#' \deqn{\frac{dR_U(t)}{dt} = g I_U}
#' \deqn{\frac{dC_U(t)}{dt} = b S_U (I_U + I_Y) N_U C
#'                            + b S_U/f_urban I_U/f_urban N_U (1-C)}
#' \deqn{\frac{dD_U(t)}{dt} = m I_U}
#'
#' For the rural community:
#' \deqn{\frac{dS_Y(t)}{dt} = - b S_Y (I_U + I_Y) N_Y C
#'                            - b S_Y/f_rural I_Y/f_rural N_Y (1-C)}
#' \deqn{\frac{dE_Y(t)}{dt} =   b S_Y (I_U + I_Y) N_Y C
#'                            + b S_Y/f_rural I_Y/f_rural N_Y (1-C) - k E_Y}
#' \deqn{\frac{dI_Y(t)}{dt} = k E_Y - (g + m) I_Y}
#' \deqn{\frac{dR_Y(t)}{dt} = g I_Y}
#' \deqn{\frac{dC_Y(t)}{dt} =  b S_Y (I_U + I_Y) N_Y C
#'                            + b S_Y/f_rural I_Y/f_rural N_Y (1-C)}
#' \deqn{\frac{dD_Y(t)}{dt} = m I_Y}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD_RU
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths for both communities
#' in the SEIRD_RU model.
#' @export

setMethod(
  "run", "SEIRD_RU",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }

    # set initial state vector
    state <- c(SU = initial_conditions(object)$S_U0,
               EU = initial_conditions(object)$E_U0,
               IU = initial_conditions(object)$I_U0,
               RU = initial_conditions(object)$R_U0,
               CU = initial_cases_deaths(object)$C_U0,
               DU = initial_cases_deaths(object)$D_U0,
               SY = initial_conditions(object)$S_Y0,
               EY = initial_conditions(object)$E_Y0,
               IY = initial_conditions(object)$I_Y0,
               RY = initial_conditions(object)$R_Y0,
               CY = initial_cases_deaths(object)$C_Y0,
               DY = initial_cases_deaths(object)$D_Y0)

    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    m = transmission_parameters(object)$m,
                    C = transmission_parameters(object)$C,
                    N_U = contact_rates(object)$urban,
                    N_Y = contact_rates(object)$rural,
                    f_urban = (1-fraction_rural(object)),
                    f_rural = fraction_rural(object)
                    )
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
          su <- state[1]
          eu <- state[2]
          iu <- state[3]
          ru <- state[4]
          cu <- state[5]
          du <- state[6]
          sy <- state[7]
          ey <- state[8]
          iy <- state[9]
          ry <- state[10]
          cy <- state[11]
          dy <- state[12]
          # rate of change: urban
          dsu <- - (b * su * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                    b * su * (iu / f_urban) * N_U * (1 - C))
          deu <- b * su * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * su * (iu / f_urban) * N_U * (1 - C) - k * eu
          diu <- k * eu - (g + m) * iu
          dru <- g * iu
          dcu <- b * su * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * su * (iu / f_urban) * N_U * (1 - C)
          d_deathu <- m * iu
          # rate of change: rural
          dsy <- - (b * sy * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                    b * sy * (iy / f_rural) * N_Y * (1 - C))
          dey <- b * sy * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * sy * (iy / f_rural) * N_Y * (1 - C) - k * ey
          diy <- k * ey - (g + m) * iy
          dry <- g * iy
          dcy <- b * sy * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * sy * (iy / f_rural) * N_Y * (1 - C)
          d_deathy <- m * iy
          # return the rate of change
          list(c(dsu, deu, diu, dru, dcu, d_deathu,
                 dsy, dey, diy, dry, dcy, d_deathy))
        })
    }

    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)

    output <- as.data.frame.array(out)

    # Compute incidences and deaths: urban
    output$CU[2:length(output$CU)] <- output$CU[
      2:length(output$CU)] - output$CU[1:(length(output$CU) - 1)]
    output$CU[1] <- 0
    output$DU[2:length(output$DU)] <- output$DU[
      2:length(output$DU)] - output$DU[1:(length(output$DU) - 1)]

    # Compute incidences and deaths: rural
    output$CY[2:length(output$CY)] <- output$CY[
      2:length(output$CY)] - output$CY[1:(length(output$CY) - 1)]
    output$CY[1] <- 0
    output$DY[2:length(output$DY)] <- output$DY[
      2:length(output$DY)] - output$DY[1:(length(output$DY) - 1)]

    colnames(output) <- c("time", object@output_names)

    # Create long format of output
    output <- melt(output, id.vars = "time")
    names(output) <- c("time", "compartment", "value")

    # Added for consistency of output format across models
    output$age_group <- rep("0-150", length(output$time))

    return(output)
  })

#----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Returns the value of R0, the basic reproduction number,
#' for the model with the specified parameter values.
#' R0 is computed using the next generation matrix method as set out in 
#' van den Driessche (2017):
#' van  den  Driessche, P. Reproduction numbers of infectious disease models.
#' Infectious Disease Modelling, 2:288–303, 8 2017.
#' 
#' In this method, the variables are split into two groups: those that represent
#' infected individuals (in our case the exposed and infectious compartments in
#' both communities), and those that do not (the susceptible, recovered and dead).
#' We only further consider the system of equations consisting of the ODEs 
#' describing the variables in the first group,
#' in this case (E_U, E_Y, I_U and I_Y).
#' The differential equations for the variables in the first group are written
#' in the form
#' \deqn{\frac{dx_i}{dt} = F_i - V_i}
#' where F_i are the terms describing new cases of infection and V_i the terms
#' describing how individuals move between infected and other compartments.
#' We compute the matrices matF and matV as
#' \deqn{matF = \partial F_i(x_0)/\partial x_j}
#' \deqn{matV = \partial V_i(x_0)/\partial x_j}
#' where x_j are all the variables representing infected individuals.
#' Finally, R0 is the spectral radius of the matrix FV^(-1). For a more
#' detailed description of this method, see van den Driessche (2017).
#' 
#' @param model a model of the class SEIRD_RU
#'
#' @return the value of R0
#' @export
setMethod(
    "R0", "SEIRD_RU", function(model) {
      # initial susceptible populations: This method applies to the disease-free 
      # equilibrium. At the disease-free equilibrium that we are concerned about
      # everyone is susceptible, but it still matters which fraction of the
      # population lives in which community
      S0U <- 1 - fraction_rural(model)
      S0Y <- fraction_rural(model)
      # required parameter values
      b <- transmission_parameters(model)$b
      k <- transmission_parameters(model)$k
      g <- transmission_parameters(model)$g
      m <- transmission_parameters(model)$m
      C <- transmission_parameters(model)$C
      # compute contacts N_U and N_Y from contact matrices
      # number of urban contacts for an urban individual, as a fraction of the
      # total population
      NU <- contact_rates(model)$urban
      # number of rural contacts for a rural individual,  as a fraction of the
      # total population
      NY <- contact_rates(model)$rural
      # fraction of the population that is urban
      f_urban <- 1 - fraction_rural(model)
      #fraction of the population that is rural
      f_rural <- fraction_rural(model)
      # convenience parameter
      K <- ((1 - f_rural) * NU + f_rural * NY)

      matF <- matrix(data = c(0, 0, b * S0U * (K * C + NU / (1 - f_rural) * (1 - C)), b * S0U * K * C,
                              0, 0,         b * S0Y * K * C,       b * S0Y * (K * C + NY / f_rural * (1 - C)),   
                              0, 0,                 0,                                   0,
                              0, 0,                 0,                                   0),
                     nrow = 4, ncol = 4, byrow = TRUE)
      matV <- matrix(data = c(k, 0, 0, 0,
                              0, k, 0, 0,
                              -k, 0, (g + m), 0,
                              0, -k, 0, (g + m)),
                     nrow = 4, ncol = 4, byrow = TRUE)

      R0 <- max(abs(eigen(matF %*% solve(matV))$values))
      return(R0)
    })

##=====================================================##
##================== SEIaImIsRD =======================##
##=====================================================##

#' An S4 object representing the SEIaImIsRD.
#' 
#' This class defines the SEIaImIsRD model, an extension of the SEIRD model.
#' The model shows how populations of susceptible, exposed, infectious and 
#' recovered individuals evolve over time, in which the infectious individuals
#' are subgrouped into compartments according to different severity of symptoms, i.e. asymptomatic, mild and severe.
#'
#' Notes:
#' 1. Total initial population size is normalised to 1.
#' 2. The current model does not include natural death or birth.
#'
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I_asymptomatic0", "I_mild0", "I_severe0", "R0", "D0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "p_symptom", "gamma", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters (double).
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#'
#' @import deSolve
#' @import ggplot2
#' @import reshape2
#' @importFrom methods new
#' @export SEIaImIsRD
SEIaImIsRD <- setClass(Class = "SEIaImIsRD",
         slots = c(
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           output_names = "list"
         ),
         prototype = list(
           initial_condition_names = list("S0", "E0", "I_asymptomatic0",
                                          "I_mild0", "I_severe0", "R0", "D0"),
           transmission_parameter_names = list("beta", "kappa", "omega",
                                              "p_symptom", "gamma", "mu"),
           initial_conditions = vector(mode = "list", length = 7),
           transmission_parameters = vector(mode = "list", length = 6),
           output_names = list("S", "E", "I_asymptomatic",
                               "I_mild", "I_severe", "R", "D",
                               "Incidence", "Deaths")
         ))


#' @describeIn SEIaImIsRD Retrieves initial conditions of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
#' @export
setMethod("initial_conditions", "SEIaImIsRD",
          function(object) object@initial_conditions)


#' @describeIn SEIaImIsRD Retrieves transmission parameters of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
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
#' beta - a named list of the effective contact rate from each infected group (i.e. rate at which an infected individual exposes susceptible), each element with value in \code{[0,1]}
#' kappa - rate of progression from exposed to infectious (the reciprocal is the incubation period),
#' omega - rate at which recovered individuals become susceptible,
#' p_symptom - a named list of the probability of exposed individuals moving into each of the different infected groups, 
#' Here, only probabilities for the mild (p_symptom.mild) and severe (p_symptom.severe) groups need be specified and 
#' the asymptomatic probability is the remainder (1 - p_symptom.mild - p_symptom.severe). Thus we require 
#' p_symptom.mild + p_symptom.severe <= 1.
#' gamma - a list of the rate of removal of each infected group (i.e. recovery rate of an infected individual), each element with value in \code{[0,1]}
#' mu - a list of the rate of disease-caused mortality of each infected group, each element with value in \code{[0,1]}
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @export
#'
setMethod(
  "transmission_parameters<-",
  "SEIaImIsRD",
  function(
    object,
    value = list(beta = list(asymptomatic = NA_real_, mild = NA_real_, severe = NA_real_), 
    kappa = NA_real_, 
    omega = NA_real_,
    p_symptom = list(mild = NA_real_, severe = NA_real_), 
    gamma = list(asymptomatic = NA_real_, mild = NA_real_, severe = NA_real_), 
    mu = list(asymptomatic = NA_real_, mild = NA_real_, severe = NA_real_))) {
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
    n_p_symptom <- length(param_list$p_symptom)
    if (n_p_symptom != 2) {
      msg <- paste0(
        "Length of parameter p_symptom,", n_mu, ", is not equal to the setting ", 2)
      errors <- c(errors, msg)
    }
    else if (param_list$p_symptom %>% unlist %>% sum > 1){
      msg <- paste0("Sum of p_symptom, ", param_list$p_symptom %>% unlist %>% sum, ", is greater than ", 1)
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) {
      object@transmission_parameters <- param_list
      object
      }
    else stop(paste(errors, ", please check and rerun transmission_parameters<-.\n"))
    })

#' @describeIn SEIaImIsRD Solves the ode system.
#'
#' \deqn{\frac{dS(t)}{dt} = -S(t)(\beta.asymptomatic I_{asymptomatic}(t) + \beta.mild I_{mild}(t) + \beta.severe I_{severe}(t)) + \omega R(t)}
#' \deqn{\frac{dE(t)}{dt} = S(t)(\beta.asymptomatic I_{asymptomatic}(t) + \beta.mild I_{mild}(t) + \beta.severe I_{severe}(t)) - \kappa E(t)}
#' \deqn{\frac{dI_{asymptomatic}(t)}{dt} = p_symptom.asymptomatic \kappa E(t) - (\gamma.asymptomatic + \mu.asymptomatic) I_{asymptomatic}(t)}
#' \deqn{\frac{dI_{mild}(t)}{dt} = p_symptom.mild\kappa E(t) - (\gamma.mild + \mu.mild) I_{mild}(t)}
#' \deqn{\frac{dI_{severe}(t)}{dt} = p_symptom.severe\kappa E(t) - (\gamma.severe + \mu.severe) I_{severe}(t)}
#' \deqn{\frac{dR(t)}{dt} = -\omega R(t) + \gamma.asymptomatic I_{asymptomatic}(t) + \gamma.mild I_{mild}(t) + \gamma.severe I_{severe}(t)}
#' \deqn{\frac{dD(t)}{dt} =  \mu.asymptomatic I_{asymptomatic}(t) + \mu.mild I_{mild}(t) + \mu.severe I_{severe}(t)}
#'
#' @param object An object of class SEIaImIsRD
#' @param times A list of time points of the simulation period
#' @param solve_method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return A list of two dataframes: one with the time steps, time series of S,
#' E, I_asymptomatic, I_mild, I_severe and R population fractions, and one with the time steps,
#' time series of incidences and deaths population fraction
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
                  dS <- -S * (beta.asymptomatic * I_asymptomatic + beta.mild * I_mild + beta.severe * I_severe) + omega * R
                  dE <- S * (beta.asymptomatic * I_asymptomatic + beta.mild * I_mild + beta.severe * I_severe) - kappa * E
                  dI_a <- (1.0 - p_symptom.mild - p_symptom.severe) * kappa * E - (gamma.asymptomatic + mu.asymptomatic) * I_asymptomatic
                  dI_m <- p_symptom.mild * kappa * E - (gamma.mild + mu.mild) * I_mild
                  dI_s <- p_symptom.severe * kappa * E - (gamma.severe + mu.severe) * I_severe
                  dR <- -omega * R + gamma.asymptomatic * I_asymptomatic + gamma.mild * I_mild + gamma.severe * I_severe
                  dD <-  mu.asymptomatic * I_asymptomatic + mu.mild * I_mild + mu.severe * I_severe
                  dC <- S * (beta.asymptomatic * I_asymptomatic + beta.mild * I_mild + beta.severe * I_severe)
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
            list("states" = states, "changes" = changes)
          })
 

#' @describeIn SEIaImIsRD Calculate the basic reproduction number (\code{R_0}) of the system using the next generation matrix approach.
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
#' @return An R0 value
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
  F[1, 2:4] <-  c(S * beta$asymptomatic, S * beta$mild, S * beta$severe)
  V[1, 1] <- kappa
  V[2, 1] <- -kappa * (1 - p_symptom$mild - p_symptom$severe)
  V[2, 2] <- gamma$asymptomatic + mu$asymptomatic
  V[3, 1] <- -kappa * p_symptom$mild
  V[3, 3] <- gamma$mild + mu$mild
  V[4, 1] <- -kappa * p_symptom$severe
  V[4, 4] <- gamma$severe + mu$severe
  # calculate R0 as the spectral radius for the matrix F x V^(-1):
  eigVals <- eigen(F %*% (solve(V)))$values
  R0 = max(abs(eigVals))
  return(R0)
  })