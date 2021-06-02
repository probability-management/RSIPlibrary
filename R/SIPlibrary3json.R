#' JSON SIPLibrary 3.0 Generator
#'
#' Takes a data.frame and an optional data.frame containing metadata and coverts
#' it to a .json format PM.org 3.0 Standard Library readable by ChanceCalc and any
#' .json library reader.
#'
#' @param DATAFRAME Data.frame containing data or SIPs.
#' @param filename Character, ending in ".json"
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
#' SIPlibrary3json(testdf, "finaltesting.xlsx", "Aaron Brown", testmeta)
#'
#' @export
SIPlibrary3json <- function(DATAFRAME, filename, author, METADF = NULL, seeds = "generate", bounds = c(0,1), boundedness = "u", term_saved = 5){
  if (any(grepl("^([a-zA-Z]{1,3})[0-9]", colnames(DATAFRAME)) == TRUE)) {
    print("Column names match possible cell coordinates in Excel. Please use different column names.")
  } else {
    countsips <- length(DATAFRAME)
    densityindexes <- c(1,10:108,117)

    if (is.null(METADF)) {
      metadf <- sapply(DATAFRAME, FUN = summary)
      metalen <- length(as.data.frame(metadf)[[1]])
    } else {
      metadf <- METADF
      metalen <- length(as.data.frame(metadf)[[1]])
    }

    cholmatrix <- t(chol(cor(DATAFRAME)))
    cholvect <- rep(NA, countsips)
    SIPsvect <- rep(NA, countsips)

    Seeds <- if(is.data.frame(seeds)){
      seeds
    } else {
      RSIPlibrary::HDRSeeds(ncol(DATAFRAME))
    }

    for(i in 1:countsips) {
      cholvect[i] <- paste('\n\t\t\t[', paste(cholmatrix[,i], collapse = ','), ']', sep = '')
      }

    cholstring <- paste(cholvect, collapse = ",")

    for(i in 1:countsips) {
        metavect <- rep(NA, metalen)
        for (j in 1:metalen){
          metavect[j] <- paste('"', rownames(metadf)[j], '"',': ', metadf[j,i], sep = '')
        }

      metadata <- paste('\n\t\t\t"MetaData": {\n\t\t\t\t',paste(metavect, collapse = ',\n\t\t\t\t'), '\n\t\t\t},', sep = '')

      metafit <- rmetalog::metalog(DATAFRAME[,i], bounds = bounds, boundedness = boundedness, term_lower_bound = term_saved, term_limit = term_saved)
      density <- paste('\n\t\t\t"Density": [\n\t\t\t',paste(stats::approx(metafit[["M"]][densityindexes,2],metafit[["M"]][densityindexes,1], n = 25)[[2]], collapse = ',\n\t\t\t'), '\n\t\t\t],', sep = '')
      a_coef <- paste('\n\t\t\t"A_Coef": [\n\t\t\t',paste(metafit[[4]][,2], collapse = ',\n\t\t\t'), '\n\t\t\t]', sep = '')

      SIPsvect[i] <- paste('\n\t\t{\n\t\t"Header": {\n\t\t\t"Name": "',
                           colnames(DATAFRAME)[i],
                           '",\n\t\t\t"Type": "F Inverse",\n\t\t\t"Terms": ',
                           term_saved,
                           ',\n\t\t\t"Bound": "',
                           boundedness,
                           '",\n\t\t\t"Lower": ',
                           bounds[1],
                           ',\n\t\t\t"Upper": ',
                           bounds[2],
                           '\n\t\t\t},\n\t\t\t"HDR": {',
                           '\n\t\t\t\t"Entity": ',
                           Seeds[1,i],
                           ',\n\t\t\t\t"Var ID": ',
                           Seeds[2,i],
                           ',\n\t\t\t\t"Option1": ',
                           Seeds[3,i],
                           ',\n\t\t\t\t"Option2": ',
                           Seeds[4,i],
                           '\n\t\t\t},',
                           metadata,
                           density,
                           a_coef,
                           '\n\t\t',
                           sep = '')

      SIPs <- paste(SIPsvect, collapse = '},')
    }

    fullstring <- paste('{\n\t"Library_info": {\n\t\t"PM_Trials": "F Inverse",',
                        '\n\t\t"PM_Lib_Provenance": "',
                        paste(author, '",', sep = ""),
                        '\n\t\t"PM_SIPmath": "3.2b"\n\t},\n\t"Copula": {\n\t\t"type": "Cholesky",\n\t\t"matrix": [',
                        cholstring,
                        '\n\t\t]\n\t},\n\t"SIPs": [',
                        SIPs,
                        '}\n\t]\n}',
                        sep = '')

    write(fullstring, filename)
    print("Done! SIP Library saved successfully to your current working directory.")
}
}
