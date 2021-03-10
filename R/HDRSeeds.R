#' HDR Seed Generator
#'
#' Helper function to create the seeds data.frame to be used by the SIPlibrary3
#' function.
#'
#' @param n An inetger equal to the number of SIPs to generate seeds for.
#' @param entity An integer between 1 and 100 million relating to the entity
#' making the simulation.
#' @param var_id An integer between 1 and 100 million to be the starting value
#' for each independent variable seed.
#' @param option1 An optional integer between 1 and 100 million to serve as a
#' secondary ID.
#' @param option2 An optional integer between 1 and 10 million to serve as a
#' tertiary ID.
#'
#' @return Data.frame containing integer values for simulation seeds.
#'
#' @examples seeds <- HDRSeeds(5)
#' seedsspecific <- HDRSeeds(5, 500, 1, 600, 700)
#'
#' @export
HDRSeeds <- function(n, entity = 1, var_id = "generate", option1 = 0, option2 = 0){
  varid <- ifelse(var_id == "generate", round(stats::runif(1,1,100000000),0), var_id)
  df <- as.data.frame(matrix(nrow = 4, ncol = n))
    for (i in 1:n){
      df[i] <- c((entity),varid-1+i,option1,option2)
    }
  return(df)
}
