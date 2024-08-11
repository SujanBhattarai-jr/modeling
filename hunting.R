#'  Predator-prey model with hunting
#'  @param t Time 
#'  @param state A vector with the current state of the system
#'  @param parameters A list with the parameters of the model
#'  @return A list with the rate of change of the state variables
#'  @export

pred_prey_hunting <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Ensure we don't hunt more prey than exist and respect the minimum prey population
    if (N > min_prey) {
      H <- H
    } else {
      H <- 0
    }
    # Rate of change
    dN <- rprey * N * (1 - N / K) - alpha * N * P - H
    dP <- eff * alpha * N * P - pmort * P
    
    # Return the rate of change
    list(c(dN, dP))
  })
}