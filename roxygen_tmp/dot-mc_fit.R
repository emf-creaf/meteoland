#' Fits Markov Chain Transition Matrices to State Sequence Modified from
#' weathergen
#' 
#' Fits Markov Chain Transition Matrices to State Sequence Modified from
#' weathergen
#' 
#' 
#' @param states character array of states (from 'd','w','e', for dry, wet and
#' extremely wet)
#' @param months numeric array of months for each daily time step
#' @return monthly list of transition matrices
#' @examples
#' 
#' transitions <- mc_fit(x=sample(c('d', 'w', 'e'), size=720, replace=TRUE, prob=c(0.5, 0.3, 0.2)), months=rep(rep(seq(1, 12), each=30), times=2))
#' 
#' 