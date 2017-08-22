# Copyright (C) 2017 Jasper Ginn & David Stolin
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Retrieve all submitted 10-K filings for a particular year
#'
#' \code{filingsByYear} retrieves all the urls of 10-K filings for a particular year. (e.g. \url{https://www.sec.gov/Archives/edgar/full-index/2004/}). It downloads the 'master.idx' file for each quarter and filters out the 10-K filings. Note that a company may submit multiple 10-K reports. These will differ by their ARC number. Also note that, prior to 2004, the SEC does not provide HTML versions of 10-K reports. Consequently, the TenK package will not be able to retrieve content prior to 2004.
#'
#' @param year Integer. Input cannot be less than 1993 or higher than the previous year.
#' @return Returns a data frame of dimensions n x k containing all 10-K filings for that year.
#'
#' @seealso See \url{https://goo.gl/pNMMcK} for documentation on this package.
#'   See also \code{\link{TenK_process}}.
#' @author Jasper Ginn
#' @importFrom utils txtProgressBar setTxtProgressBar download.file
#' @import rvest
#' @export

filingsByYear <- function(year) {
  # Helper function to read IDX files
  read.idx <- function(destfile) {
    io <- readLines(destfile)[-1:-12]
    lines <- lapply(io, function(x) {
      m <- tryCatch(
        strsplit(x, "\\|")[[1]]
      , error = function(e) {
        list(
          "ERROR",
          "ERROR",
          "ERROR"
        )
      })
      if(length(m) < 3) {
        return(NULL)
      }
      if(m[3] != "10-K") {
        return(NULL)
      } else {
        return(m)
      }
    })
    # Remove NULL
    lines <- lines[!sapply(lines, is.null)]
    # To df
    df <- do.call(rbind.data.frame, lines)
    # Names
    header <- c("CIK", "company_name", "form_type", "date_filed", "filename")
    names(df) <- header
    return(df)
  }
  current.year <- as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1])
  # Check if year in range
  if(year < 1993 | year > current.year) {
    stop(paste0("'year' parameter can't be less than 1993 or higher than ", as.character(current.year), "."))
  }
  # If year is current year, check how many quarters are available
  if(year == current.year) {
    t <- read_html(paste0("https://www.sec.gov/Archives/edgar/full-index/", year, "/")) %>%
      html_node("div#main-content") %>%
      html_node("table") %>%
      html_table()
    quarters <- paste0("QTR", 1:nrow(t))
  } else {
    quarters <- paste0("QTR", 1:4)
  }
  # This is the url with past data
  bUrl <- sapply(quarters, function(x) {
    paste0("https://www.sec.gov/Archives/edgar/full-index/", year, "/", x, "/master.idx")
  })
  # Download each of the files
  cat(paste0("Downloading data for year ", year, ". This could take a while ..."))
  # Progress bar
  pb <- utils::txtProgressBar(min=0, max=100, initial=0, style=3)
  progress <- 0 # Where we are now
  chunk <- 100 / (length(bUrl) * 2) # Increase by this number
  # Download data for each quarter
  for(url in bUrl) {
    q <- names(bUrl[url == bUrl])
    download.file(url, paste0(tempdir(), "/", q, ".idx"))
    # Update porgress bar
    progress <- progress + chunk
    utils::setTxtProgressBar(pb, progress)
  }
  # Read data
  n <- paste0(tempdir(), "/", names(bUrl), ".idx")
  d <- list()
  for(f in n) {
    ind <- which(f==n)
    d[[ind]] <- read.idx(f)
    # Delete file
    file.remove(f)
    # Update porgress bar
    progress <- progress + chunk
    utils::setTxtProgressBar(pb, progress)
  }
  # Bind
  df.bind <- do.call(rbind.data.frame, d)
  return(df.bind)
}
