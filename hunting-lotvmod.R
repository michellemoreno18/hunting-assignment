
#' Complex Predator-Prey Model with Alternative Prey and Harvesting + Harvest Threshold
#' 
#' Adapted from "Rapid and direct recoveries of predators and prey through 
#' synchronized ecosystem management" by Jameal F. Samhouri, Adrian C. Stier, 
#' Shannon M. Hennessey, Mark Novak, Benjamin S. Halpern, and Phillip S. Levin (2017)
#'
#' Computes the rate of change of prey (X) and predator (P) populations,
#' incorporating logistic growth, predation, alternative prey (Y), and conditional harvesting.
#'
#' @param t Time
#' @param state Named vector of state variables: X (prey), P (predator), 
#' Y (alternative prey)
#' @param parms List of parameters:
#'  - rX: intrinsic growth rate of prey X
#'  - KX: carrying capacity of prey X
#'  - aX: attack rate on prey X
#'  - aY: attack rate on alternative prey Y
#'  - hX: harvesting rate of prey X
#'  - c: conversion efficiency of consumed prey into predator reproduction
#'  - dP: predator natural death rate
#'  - KP: carrying capacity of predator
#'  - hP: harvesting rate of predator
#'  - min_harvest_X: minimum prey population required before harvesting occurs
#'
#' @return A list with the rate of change of X and P

complex_pred_prey <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    
    # Use threshold to conditionally apply harvesting on prey
    effective_harvest_X <- if (X >= min_harvest_X) hX * X else 0
    
    # Rate of change of prey X
    dX <- rX * X * (1 - X / KX) - aX * P * X - effective_harvest_X
    
    # Rate of change of predator P
    dP <- P * (c * (aX * X + aY * Y) - dP) * (1 - P / KP) - hP * P
    
    return(list(c(dX, dP)))
  })
}
