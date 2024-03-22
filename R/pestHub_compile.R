#' Compile data from different files exported by Web Scraping
#'
#'@description
#'This function is used to compile data exported from Web Scraping as CSV files into a single data frame that holds
#'locations for pest dispersion hubs
#'
#'The function requires the package 'readr'
#'
#'@param filepath The file path to the folder that holds the exported CSV files from "scrape_location.ipynb".
#'Should be in the format produced by the file.path() function (i.e. with '/' instead of '\\')
#'
#'@param columns A character or vector of the column names that have the data of interest
#'
#'@param types A vector of the location type names (spaces should be used between words)
#'
#'@param counties A vector of the county names (spaces should be used between words)
#'
#'@param error.return Logical. If \code{error.return = TRUE}, then creates a list of files containing no data as an output
#'
#'@return A data frame of the selected data from the files exported from scrape_location.ipynb.
#'If \code{error.return = TRUE}, a list is returned with data in the first element and errors in the second element.
#'
#'@examples
#'\dontrun{
#'
#'library(readr)
#'
#'GIVE EXAMPLES
#'
#'}
#'@export
pestHub_compile <- function(filepath, columns = "all", types, counties, error.return = FALSE) {

  files <- list.files(filepath, all.files = FALSE)
  files <- files[grepl('.csv', files, fixed = TRUE)]

  # files <- gsub("_", "+", files, fixed = TRUE)

  err.file <- NULL
  for(i in 1:length(files)){
    rmv.tab <- suppressWarnings(suppressMessages(as.data.frame(read.csv(paste(filepath, files[i], sep = "/")))))
    if(nrow(rmv.tab) == 0) {
      err.file <- rbind(err.file, data.frame(file = files[i]))
    }
  }

  files <- files[!is.element(files, err.file$file)]

  out <- suppressMessages(as.data.frame(read.csv(paste(filepath, files[1], sep = "/"))))
  names(out) <- tolower(names(out))

  if(length(columns) == 1 & columns[1] == "all") {
    columns <- names(out)
  }

  columns <- tolower(columns)

  columns <- gsub(" ", ".", columns)
  names(out) <- gsub(" ", ".", names(out))
  out <- out[, columns]

  names(out) <- gsub(' ', '_', names(out)) ## repeat for all weird characters

  type.tmp <- NA

  for (i in 1:length(types)) {
    tmp.tp <- gsub(' ', '+', types[i], fixed = TRUE)
    tmp.file <- gsub("_", "+", files[1], fixed = TRUE)
    tmp.file <- tolower(gsub(" ", "+", tmp.file, fixed = TRUE))
    if(grepl(tmp.tp, tmp.file, fixed = TRUE)) {
      type.tmp <- types[i]
    }
    rm(tmp.tp)
  }

  if(is.na(type.tmp)) {
    stop(paste0(' provided types not in ', files[1]))
  }

  out$location_type <- type.tmp
  rm(type.tmp)

  county.tmp <- NA

  for (i in 1:length(counties)) {
    tmp.cnt <- gsub(' ', '+', counties[i], fixed = TRUE)
    tmp.file <- gsub("_", "+", files[1], fixed = TRUE)
    tmp.file <- tolower(gsub(" ", "+", tmp.file, fixed = TRUE))
    if(grepl(tmp.cnt, tmp.file, fixed = TRUE)) {
      county.tmp <- counties[i]
    }
    rm(tmp.cnt)
  }

  if(is.na(county.tmp)) {
    stop(paste0(' provided counties not in ', files[1]))
  }

  out$county <- county.tmp
  rm(county.tmp)

  for (i in 2:length(files)) {
    tmp <- suppressMessages(as.data.frame(read.csv(paste(filepath, files[i], sep = "/"))))
    names(tmp) <- tolower(names(tmp))

    names(tmp) <- gsub(" ", ".", names(tmp))

    if (!all(is.element(columns, names(tmp)))) {
      stop(paste0('columns names of ', files[i], ' do not match columns provided '))
    }

    tmp <- tmp[, columns]

    names(tmp) <- gsub(' ', '_', names(tmp)) ## repeat for all weird characters

    type.tmp <- NA

    for (j in 1:length(types)) {
      tmp.tp <- gsub(' ', '+', types[j], fixed = TRUE)
      tmp.file <- gsub("_", "+", files[i], fixed = TRUE)
      tmp.file <- tolower(gsub(" ", "+", tmp.file, fixed = TRUE))
      if(grepl(tmp.tp, tmp.file, fixed = TRUE)) {
        type.tmp <- types[j]
      }
      rm(tmp.tp)
    }

    if(is.na(type.tmp)) {
      stop(paste0(' provided types not in ', files[i]))
    }

    tmp$location_type <- type.tmp
    rm(type.tmp)

    county.tmp <- NA

    for (j in 1:length(counties)) {
      tmp.cnt <- gsub(' ', '+', counties[j], fixed = TRUE)
      tmp.file <- gsub("_", "+", files[i], fixed = TRUE)
      tmp.file <- tolower(gsub(" ", "+", tmp.file, fixed = TRUE))
      if(grepl(tmp.cnt, tmp.file, fixed = TRUE)) {
        county.tmp <- counties[j]
      }
      rm(tmp.cnt)
    }

    if(is.na(county.tmp)) {
      stop(paste0(' provided counties not in ', files[i]))
    }

    tmp$county <- county.tmp
    rm(county.tmp)

    if (!all(is.element(names(out), names(tmp))) | !all(is.element(names(tmp), names(out)))) {
      stop(paste0('columns names of ', files[i], ' do not match names of ', files[1]))
    }

    out <- rbind(out, tmp)

    rm(tmp)
  }

  if(error.return) {
    return(list(data = out, error = err.file))
  } else {
    return(out)
  }

}
