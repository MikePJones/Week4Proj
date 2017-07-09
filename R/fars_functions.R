#' Import FARS data as data frame
#'
#' This function uses the \code{readr} package's \code{read_csv) function to search the Working Directory
#' for a file containing the FARS data to be read. If the file does not exist, the function alerts the user.
#' If the file does exist, the data from the file is imported as a data frame using the \code{dplyr} package's
#' \code{tbl_df} function.
#'
#' @param filename A bz2 file or a string pointing to such a file
#'
#' @return This function returns a data frame of all of the FARS data from the bz2 file
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read("notfound")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generate filename based on specified year
#'
#' This function generates a filename that can be used in the fars_read function. It takes as an input
#' a specific year, and provides as output the full filename as a character string.
#'
#' @param year An integer representing a year that is 2013, 2014, or 2015
#'
#' @return This function returns a character string of the full bz2 filename
#'
#' @examples
#' make_filename(2013)
#' make_filename(2014)
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Generate a table identifying the month of each datapoint
#'
#' This function uses the \code{mutate} and \code{select} functions from the \code{dplyr} package to strip
#' the full data frame associated with a given year, leaving only the rows associated with months and the year.
#' This provides a matrix of the number of rows of data from a given year, and columns of months, expressed
#' as an integer from 1 to 12, and the year.  If a year for which data is not available is input, function
#' alerts user that it is an invalid year.
#'
#' @param years An integer representing a year that is 2013, 2014, or 2015
#'
#' @return A table with the same number of rows as the data file associated with the year being evaluated,
#'    identifying the month associated with each datapoint
#'
#' @details makes use of the \code{\link{make_filename}} and \code{\link{fars_read}} functions by generating a
#'    filename based on the year provided, and then generating a data frame from the data associated with that
#'    year's datafile
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(2014)
#' fars_read_years(2015)
#'
#' @importFrom dplyr mutate select %>%
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Group the number of incidents per month in a given year
#'
#' This function returns the total number of incidents per month in a given year. The only input is the
#' year the user wishes to evaluate, and the \code{dplyr} package's \code{bind_rows}, \code{group_by}, and
#' \code{summarize} functions and the \code{tidyr} package's \code{spread} function are all used to aggregate
#' the individual datapoints into a table of months and incidents per month
#'
#' @inheritParams fars_read_years
#'
#' @return A table with the total number of incidents in each individual month of the given year
#'
#' @details makes use of the \code{\link{fars_read_years}} function, which uses the \code{\link{make_filename}}
#'     and \code{\link{fars_read}} functions, to first make a matrix of the same number of rows as the primary
#'     datafile, then to add the relevant incident data to each row, and then finally to summarize each month
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(2014)
#' fars_summarize_years(2015)
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Generate a map of all incidents in a given year
#'
#' This function uses the \code{maps} package to display dots for each incident on a map of a given state over
#' a given year.  It takes as inputs the state, represented by its number with all states ordered alphabetically,
#' and year.  The \code{filter} function from the \code{dplyr} package is used to consolidate the data
#'
#' @param state.num an integer from 0 through 51, representing each state and the District of Columbia
#' @param year An integer representing a year that is 2013, 2014, or 2015
#'
#' @return A map of the state with each dot representing an incident.  If there are no accidents in that state
#' in the given year, the function returns a message as such.  If a number is input that does not correspond
#' to a state, the function returns a message that the provided state number is invalid
#'
#' @examples
#' fars_map_state(5, 2013)
#' fars_map_state(51, 2014)
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
