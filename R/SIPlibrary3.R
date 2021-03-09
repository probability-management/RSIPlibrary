#' SIPLibrary 3.0 Generator
#'
#' Takes a data.frame and an optional data.frame containing metadata and coverts
#' it to a .xlsx format PM.org 3.0 Standard Library readable by ChanceCalc.
#' Allows a user to easily output R SIPs to Excel users.
#'
#' @param DATAFRAME Data.frame containing data or SIPs.
#' @param filename Character, ending in ".xlsx"
#' @param author Character.
#' @param METADF Data.frame with named rows of same length as DATAFRAME.
#' @param seeds Optional data.frame input for the HDR seeds. Default is "generate",
#' which calls the HDRSeeds function to generate statistically independent seeds.
#' @param bounds Passed to metalog function, numeric vector specifying lower or
#' upper bounds, none required if the distribution is unbounded.
#' @param boundedness Passed to metalog function, character string specifying
#' unbounded, semi-bounded upper, semi-bounded lower or bounded; accepts values
#' u, su, sl and b (default: 'u').
#' @param term_saved Passed to metalog function, a value between 3 and 16 for the
#' number of terms desired for all SIPs. Default is 5.
#'
#' @return Completion notification, file output to WD
#'
#' @examples testdf <- data.frame(SIP_1 = rnorm(5000),
#' SIP_2 = rnorm(5000),
#' SIP_3 = rnorm(5000),
#' SIP_4 = rnorm(5000),
#' SIP_5 = rnorm(5000))
#'
#' testmeta <- sapply(testdf, FUN = summary)
#'
#' SIPLibrary(testdf, "finaltesting.xlsx", "Aaron Brown", testmeta)
#'
#' @export
SIPlibrary3 <- function(DATAFRAME, filename, author, METADF = NULL, seeds = "generate", bounds = c(0,1), boundedness = "u", term_saved = 5){
  Seeds <- if(is.data.frame(seeds)){
    seeds
  } else {
    HDRSeeds(ncol(DATAFRAME))
  }

  wb <- openxlsx::createWorkbook()
  wks <- "Library"
  openxlsx::addWorksheet(wb, wks, gridLines = FALSE, tabColour = "#F2F2F2")

  countsips <- length(DATAFRAME)
  dflen <- length(DATAFRAME[[1]])
  if (is.null(METADF)) {
    metalen <- 0
  } else {
    metalen <- length(as.data.frame(METADF)[[1]])
  }

  openxlsx::writeData(wb, wks, c("PM_Trials", "PM_Lib_Provenance", "PM_SIP_Names", "PM_Meta", "PM_Meta_Index"), startRow = 2, startCol = 1)
  openxlsx::writeData(wb, wks, c("F Inverse", author, paste0(colnames(DATAFRAME), collapse = ","), paste("D",(16+countsips),":D",(15+countsips+metalen), sep = ""), c(1:(0+metalen))), startRow = 2, startCol = 2)
  openxlsx::writeData(wb, wks, c(1:(0+metalen)), startRow = 6, startCol = 2)
  openxlsx::writeData(wb, wks, c("Formula", "Base Formula", "Name", "Type (SIP or F Inverse)", "Terms", "Bound"), startRow = 2, startCol = 3)
  openxlsx::writeData(wb, wks, c("Lower", "Upper"), startRow = 8, startCol = 4)

  openxlsx::writeData(wb, wks, "Cholesky", startRow = 10, startCol = 3)
  openxlsx::writeData(wb, wks, "Copula", startRow = (10+countsips), startCol = 3)
  openxlsx::writeData(wb, wks, "HDR", startRow = (11+countsips), startCol = 3)
  openxlsx::writeData(wb, wks, c("Entity", "Var ID","Option1", "Option2"), startRow = (11+countsips), startCol = 4)

  openxlsx::writeData(wb, wks, "HDR_Formula", startRow = (15+countsips), startCol = 3)
  openxlsx::writeData(wb, wks, "MetaData", startRow = (16+countsips), startCol = 3)
  openxlsx::writeData(wb, wks, chol(stats::cor(DATAFRAME)), startRow = 10, startCol = 4, rowNames = TRUE, colNames = FALSE)
  if (!is.null(METADF)) {
    openxlsx::writeData(wb, wks, METADF, startRow = (16+countsips), startCol = 4, rowNames = TRUE, colNames = FALSE)
  }

  openxlsx::writeData(wb, wks, "Density", startRow = (16+countsips+metalen), startCol = 3)
  openxlsx::writeData(wb, wks, paste("Density", c(1:25), sep = ""), startRow = (16+countsips+metalen), startCol = 4)

  openxlsx::writeData(wb, wks, "A_Coef", startRow = (41+countsips+metalen), startCol = 3)
  openxlsx::writeData(wb, wks, paste("A_", c(1:term_saved), sep = ""), startRow = (41+countsips+metalen), startCol = 4)

  densityindexes <- c(1,10:108,117)

  openxlsx::writeData(wb, wks, Seeds, startRow = (11+countsips), startCol = 5, colNames = FALSE)

  yellow <- openxlsx::createStyle(fgFill = "#FFFF00")

  start_time <- Sys.time()
  print(paste("Metalog operation began at",Sys.time()))
  for (i in 1:countsips){
    name <- colnames(DATAFRAME)[i]
    metafit <- rmetalog::metalog(DATAFRAME[,i], bounds = bounds, boundedness = boundedness, term_lower_bound = term_saved, term_limit = term_saved)
    openxlsx::writeData(wb, wks, c(name, "", "", name, "F Inverse"), startRow = 1, startCol = 4+i)
    openxlsx::writeData(wb, wks, term_saved, startRow = 6, startCol = 4+i)
    openxlsx::writeData(wb, wks, boundedness, startRow = 7, startCol = 4+i)
    openxlsx::writeData(wb, wks, c(bounds[1], bounds[2]), startRow = 8, startCol = 4+i)
    openxlsx::writeData(wb, wks, stats::approx(metafit[["M"]][densityindexes,2],metafit[["M"]][densityindexes,1], n = 25)[2], startRow = (16+countsips+metalen), startCol = 4+i)
    openxlsx::writeData(wb, wks, metafit[[4]][,2], startRow = (41+countsips+metalen), startCol = 4+i)
    openxlsx::createNamedRegion(wb, wks, name, rows = 2:(40+countsips+metalen), cols = 4+i)
    openxlsx::createNamedRegion(wb, wks, paste(name,".Header", sep = ""), rows = 4:9, cols = 4+i)
    openxlsx::createNamedRegion(wb, wks, paste(name,".Cholesky", sep = ""), rows = 10:(9+countsips), cols = 4+i)
    openxlsx::createNamedRegion(wb, wks, paste(name,".HDR", sep = ""), rows = (11+countsips):(14+countsips), cols = 4+i)
    openxlsx::createNamedRegion(wb, wks, paste(name,".MetaData", sep = ""), rows = (16+countsips):(15+countsips+metalen), cols = 4+i)
    openxlsx::createNamedRegion(wb, wks, paste(name,".Density", sep = ""), rows = (16+countsips+metalen):(40+countsips+metalen), cols = 4+i)
    openxlsx::createNamedRegion(wb, wks, paste(name,".A_Coef", sep = ""), rows = (41+countsips+metalen):(40+countsips+metalen+term_saved), cols = 4+i)
    openxlsx::addStyle(wb, wks, yellow, rows = c(2,3,(10+countsips),(15+countsips)), cols = 4+i, stack = TRUE)
  }

  end_time <- Sys.time()
  print(paste("Metalog operation ended at",Sys.time()))
  print(end_time - start_time)

  openxlsx::createNamedRegion(wb, wks, "PM_Trials", rows = 2, cols = 2)
  openxlsx::createNamedRegion(wb, wks, "PM_Lib_Provenance", rows = 3, cols = 2)
  openxlsx::createNamedRegion(wb, wks, "PM_SIP_Names", rows = 4, cols = 2)
  openxlsx::createNamedRegion(wb, wks, "PM_Meta_Index", rows = 6:(5+metalen), cols = 2)
  openxlsx::createNamedRegion(wb, wks, "PM_Meta", rows = (16+countsips):(15+countsips+metalen), cols = 4)
  openxlsx::createNamedRegion(wb, wks, "PM_Row_Headers_1", rows = 1:(40+countsips+metalen+term_saved), cols = 3)
  openxlsx::createNamedRegion(wb, wks, "PM_Row_Headers_2", rows = 1:(40+countsips+metalen+term_saved), cols = 4)

  openxlsx::setColWidths(wb, wks, c(1,2), 17.5)
  openxlsx::insertImage(wb, wks, "Logos.png", 2.5, 2.1, startRow = 11, startCol = 1)
  openxlsx::saveWorkbook(wb,system.file("extdata", "Logos.png", package = "RSIPlibrary"),overwrite = TRUE)
  print("Done! SIP Library saved successfully to your current working directory.")
}
