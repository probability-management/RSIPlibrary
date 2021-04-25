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
#' SIPlibrary3(testdf, "finaltesting.xlsx", "Aaron Brown", testmeta)
#'
#' @export
SIPlibrary3 <- function(DATAFRAME, filename, author, METADF = NULL, seeds = "generate", bounds = c(0,1), boundedness = "u", term_saved = 5){
  if (any(grepl("^([a-zA-Z]{1,3})[0-9]", colnames(DATAFRAME)) == TRUE)) {
    print("Column names match possible cell coordinates in Excel. Please use different column names.")
  } else {
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

    openxlsx::writeData(wb, wks, c("PM_Property_Names", "PM_Trials", "PM_Lib_Provenance", "PM_SIP_Names", "PM_Meta", "PM_Meta_Index", "PM_NumCholesky", "PM_NumMeta", "PM_NumDensity", "PM_NumCoeffs", "PM_SIPmath"), startRow = 1, startCol = 1)
    openxlsx::writeData(wb, wks, c("PM_Property_Values", "F Inverse", author, paste0(colnames(DATAFRAME), collapse = ","), "Unused in 3.0", "Unused in 3.0"), startRow = 1, startCol = 2)
    openxlsx::writeData(wb, wks, c(countsips, metalen, 25, 16), startRow = 7, startCol = 2)
    openxlsx::writeData(wb, wks, c("3.2b"), startRow = 11, startCol = 2)
    openxlsx::writeData(wb, wks, c("PM_Row_Header_1", "Name", "Type (SIP or F Inverse)", "Terms", "Bound"), startRow = 1, startCol = 3)

    openxlsx::writeData(wb, wks, "Cholesky", startRow = 8, startCol = 3)
    openxlsx::writeData(wb, wks, "HDR", startRow = (8+countsips), startCol = 3)
    openxlsx::writeData(wb, wks, "MetaData", startRow = (12+countsips), startCol = 3)

    openxlsx::writeData(wb, wks, "Density", startRow = (12+countsips+metalen), startCol = 3)
    openxlsx::writeData(wb, wks, paste("Density", c(1:25), sep = ""), startRow = (12+countsips+metalen), startCol = 4)

    openxlsx::writeData(wb, wks, "A_Coef", startRow = (37+countsips+metalen), startCol = 3)
    openxlsx::writeData(wb, wks, paste("A_", c(1:term_saved), sep = ""), startRow = (37+countsips+metalen), startCol = 4)

    openxlsx::writeData(wb, wks, "PM_Row_Header_2", startRow = 1, startCol = 4)
    openxlsx::writeData(wb, wks, c("Lower", "Upper"), startRow = 6, startCol = 4)
    openxlsx::writeData(wb, wks, c("Entity", "Var ID","Option1", "Option2"), startRow = (8+countsips), startCol = 4)

    openxlsx::writeData(wb, wks, chol(stats::cor(DATAFRAME)), startRow = 8, startCol = 4, rowNames = TRUE, colNames = FALSE)
    if (!is.null(METADF)) {
      openxlsx::writeData(wb, wks, METADF, startRow = (12+countsips), startCol = 4, rowNames = TRUE, colNames = FALSE)
    }

    densityindexes <- c(1,10:108,117)

    openxlsx::writeData(wb, wks, Seeds, startRow = (8+countsips), startCol = 5, colNames = FALSE)

    start_time <- Sys.time()
    print(paste("Metalog operation began at",Sys.time()))
    for (i in 1:countsips){
      name <- colnames(DATAFRAME)[i]
      metafit <- rmetalog::metalog(DATAFRAME[,i], bounds = bounds, boundedness = boundedness, term_lower_bound = term_saved, term_limit = term_saved)
      openxlsx::writeData(wb, wks, c(paste("Var", i, sep = "_"), name, "F Inverse"), startRow = 1, startCol = 4+i)
      openxlsx::writeData(wb, wks, term_saved, startRow = 4, startCol = 4+i)
      openxlsx::writeData(wb, wks, c(boundedness), startRow = 5, startCol = 4+i)
      openxlsx::writeData(wb, wks, c(bounds[1], bounds[2]), startRow = 6, startCol = 4+i)
      openxlsx::writeData(wb, wks, stats::approx(metafit[["M"]][densityindexes,2],metafit[["M"]][densityindexes,1], n = 25)[2], startRow = (12+countsips+metalen), startCol = 4+i)
      openxlsx::writeData(wb, wks, metafit[[4]][,2], startRow = (37+countsips+metalen), startCol = 4+i)
    }

    end_time <- Sys.time()
    print(paste("Metalog operation ended at",Sys.time()))
    print(end_time - start_time)

    openxlsx::setColWidths(wb, wks, c(1:4), 17.5)
    openxlsx::insertImage(wb, wks, system.file("extdata", "Logos.png", package = "RSIPlibrary"), 2.5, 2.1, startRow = 12, startCol = 1)
    openxlsx::saveWorkbook(wb,file = filename, overwrite = TRUE)
    print("Done! SIP Library saved successfully to your current working directory.")
  }
}
