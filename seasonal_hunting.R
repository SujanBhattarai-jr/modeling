#'  Lotka-Volterra model with hunting
#'  @param time Time
#'  @param state A vector with the current state of the system
#'  @param parameters A list with the parameters of the model
#'  @return A list with the rate of change of the state variables
#'  @export
#'  

# Lotka-Volterra model with hunting
lv_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Logistic growth of prey
    dPrey <- rprey * Prey * (1 - Prey / K) - alpha * Prey * Predator
    
    # Functional response and predator mortality
    dPredator <- eff * alpha * Prey * Predator - pmort * Predator
    
    # Density-dependent hunting rate
    density_dependent_hunting <- hunting_max / (1 + exp(-(Prey - hunting_inflection) / 100))
    
    # Seasonal variation in hunting rate
    seasonal_hunting <- 1 + seasonality_amplitude * sin(seasonality_frequency * time)
    
    # Total hunting rate
    hunting_rate <- density_dependent_hunting * seasonal_hunting
    
    # Adjust hunting rate based on minimum prey population
    if (Prey < min_prey_for_hunting) {
      hunting_rate <- 0
    }
    
    # Remove hunted prey
    dPrey <- dPrey - hunting_rate
    
    return(list(c(dPrey, dPredator)))
  })
}