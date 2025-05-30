complex_pred_prey <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    
    # Equation (1): Rate of change of prey X
    dX <- rX * X * (1 - X / KX) - aX * P * X - hX * X
    
    # Equation (2): Rate of change of predator P
    dP <- P * (c * (aX * X + aY * Y) - dP) * (1 - P / KP) - hP * P
    
    return(list(c(dX, dP)))
  })
}
