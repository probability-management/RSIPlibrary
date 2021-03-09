#' SIPLibrary 2.0 Generator
#'
#' Outputs a data.frame and an optional data.frame containing metadata to a .xlsx
#' format SIPmath Tools readable library file. Allows a user to easily output R
#' SIPs to Excel users.
#'
#' @param DATAFRAME Data.frame containing SIPs.
#' @param filename Character, ending in ".xlsx"
#' @param author Character.
#' @param METADF Data.frame with named rows of same length as DATAFRAME.
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
SIPLibrary <- function(DATAFRAME, filename, author, METADF = NULL) {
  if (any(grepl("^([a-zA-Z]{1,3})[0-9]", colnames(DATAFRAME)) == TRUE)) {
    print("Column names match possible cell coordinates in Excel. Please use different column names.")
  } else {

    wb <- openxlsx::createWorkbook()
    wks <- "Library"
    openxlsx::addWorksheet(wb, wks)

    dflen <- length(DATAFRAME[,1])
    if (is.null(METADF)) {
      metalen <- 0
    } else {
      metalen <- length(METADF[,1])
    }

    openxlsx::writeData(wb, wks, "PM_Trials", startRow = 1, startCol = 1)
    openxlsx::writeData(wb, wks, dflen, startRow = 1, startCol = 2)
    openxlsx::writeData(wb, wks, "PM_Lib_Provenance", startRow = 2, startCol = 1)
    openxlsx::writeData(wb, wks, author, startRow = 2, startCol = 2)
    openxlsx::writeData(wb, wks,"Meta Data", startRow = 4, startCol = 4)
    openxlsx::writeData(wb, wks,"Index", startRow = 4, startCol = 5)
    if (!is.null(METADF)) {
      for(i in 1:metalen) {
        openxlsx::writeData(wb, wks, rownames(METADF)[i],startRow = 4+i, startCol = 4)
        openxlsx::writeData(wb, wks, 6+metalen+dflen+i, startRow = 4+i, startCol = 5)
      }
    }

    openxlsx::writeData(wb, wks, DATAFRAME, startRow = (6+metalen), startCol = 2, rowNames = TRUE)
    openxlsx::writeData(wb, wks, METADF, startRow = (7+metalen+dflen), startCol = 2, rowNames = TRUE, colNames = FALSE)

    openxlsx::createNamedRegion(wb, wks, "PM_Trials", rows = 1, cols = 2)
    openxlsx::createNamedRegion(wb, wks, "PM_Lib_Provenance", rows = 2, cols = 2)

    if (!is.null(METADF)) {
      openxlsx::createNamedRegion(wb, wks, "PM_Meta", rows = 5:(4+metalen), cols = 4)
      openxlsx::createNamedRegion(wb, wks, "PM_Meta_Index", rows = 5:(4+metalen), cols = 5)
    }

    sipnames <- colnames(DATAFRAME)

    for (i in 1:length(DATAFRAME)) {
      openxlsx::createNamedRegion(wb, wks, sipnames[i], rows = (7+metalen):(6+metalen+dflen), cols = 2+i) #cell[[7+metalen,2+i]], cell[[6+metalen+dflen,2+i]], TRUE, TRUE)
      openxlsx::createNamedRegion(wb, wks, paste(sipnames[i],".MD", sep = ""), rows = (7+metalen):(6+(2*metalen)+dflen), cols = 2+i) #cell[[7+metalen,2+i]], cell[[6+(2*metalen)+dflen,2+i]], TRUE, TRUE)
    }

    openxlsx::groupRows(wb, wks, (7+metalen):(6+metalen+dflen), TRUE)

    openxlsx::saveWorkbook(wb,file=filename,overwrite = TRUE)
    print("Done! SIP Library saved successfully to your current working directory.")
  }
}
