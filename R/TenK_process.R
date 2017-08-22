# Copyright (C) 2016 Jasper Ginn & David Stolin
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

#' Scrape an entire 10-K report or the business description section/
#'
#' \code{TenK_process} retrieves the entire 10-K annual report or the business
#'   description from an url belonging to a SEC 10-K report. If an FTP url is
#'   passed, the function will determine the link to the HTML version. See
#'   documentation (execute 'vignette("TenK")' or visit \url{https://goo.gl/pNMMcK})
#'
#' @param URL url belonging to the 10-k annual report
#' @param metadata If FALSE, the function does not return any metadata. In this
#'   case, it will only return the url to the HTML 10-K report and the 10-K
#'   document or business description.
#' @param meta_list Choose which fields you want to return. Defaults to "ALL".
#'   For an overview of the variables that you can select, please see
#'   \url{https://goo.gl/AJFUul}.
#' @param return Return full report ("ALL"), business description ("BD") or only metadata ("NONE").
#'   Defaults to "ALL"
#' @return Depending on the 'return_meta' parameter and 'meta_list' parameter,
#'   the function returns a list with useful information. Please refer to the
#'   documentation for additional information.
#' @examples
#' res <- TenK_process("ftp://ftp.sec.gov/edgar/data/34782/0000034782-16-000102.txt")
#'
#' @seealso See \url{https://goo.gl/pNMMcK} for documentation on this package. You can
#'   also execute 'vignette("TenK")'.
#' @author Jasper Ginn
#' @importFrom xml2 read_html xml_find_first xml_length xml_text xml_children xml_name
#' @importFrom rvest html_node html_table html_text
#' @importFrom stringr str_extract_all str_locate_all str_sub str_count str_trim
#' @import dplyr
#' @export

TenK_process <- function( URL,
                          metadata = TRUE,
                          meta_list = list(),
                          retrieve = c("ALL", "BD", "NONE")) {

  # Match
  retrieve <- match.arg(retrieve)

  ######################################
  # Check if parameters are valid ------
  ######################################

  # If not a sec.gov url, stop.
  if(! grepl("sec.gov", URL)) stop("Not a sec.gov url")

  # Check URL (FTP / HTML)
  if(! grepl("ftp.sec.gov", URL) ) {
    # Marker
    FTP <- FALSE
    type <- "html"
  } else {
    FTP <- TRUE
    type <- "ftp"
  }

  # Check if any field names given in meta_list are invalid
  if( length(meta_list) > 0 ) {
    if(!all(names(meta_list) %in% c("CIK", "ARC", "index.url", "filing_date",
                                "company_name", "period_report",
                                "date_accepted", "htm10kurl", "htm10kinfo", "filer_info"))) {
      stop("Invalid fields passed to 'meta_list' argument. Please check your syntax.")
    }
  }

  # Test for logical
  if( mode(metadata) != "logical" ) {
    stop("'metadata' argument must either be TRUE or FALSE.")
  }

  ########################
  # Helper functions -----
  ########################

  # Function 'get'. Retrieves page from SEC.
    # param: URL - URL for 10-K SEC report
    # Returns: HTML

  get <- function(URL) {
    # Read URL with tryCatch
    tryCatch({
      t <- read_html(URL, encoding="UTF-8")
    }, error = function(e) {
      stop(paste0("Error parsing URL ", URL)) # Raise error
    })
  } #'End of function get'

  # Function 'get.text'. Takes html as input and recursively extracts text from elements. Essentially does the same thing
  # As rvest's 'read_html'. However, I've had some issues with the rvest function in that it squeezes sentences together
  # (see documentation). This is the first version of the 'get.text' function. It is slower than rvest's 'read_html' but
  # does a better job at extracting text.
    # Param: raw html extracted by get function (see line 94)
    # Returns: returns concatenated text of report.

  get.text <- function(html) {

    #########################################
    # Recursively extract text from reports #
    #########################################

    text.recursion <- function(node) {
      if( xml_length(node) <= 1 | xml_name(node) == "p" | xml_name(node) == "font" ) {
        # Extract text
        node.string <- xml_text(node)
        # Check for characters where number of characters <= 1
        if( nchar(node.string) <= 1 ) {
          return(NULL)
        }
        # Trim whitespace
        node.string <- str_trim(node.string)
        # Return
        return(node.string)
      } else {
        res <- unlist(sapply(xml_children(node), text.recursion))
        # Collapse
        res <- paste0(res, collapse = " ")
        return(res)
      }
    }
    # Navigation
    p <- html %>% xml_find_first("/html/body/document/type/sequence/filename/description/text")
    # If 0
    if(length(p) == 0) {
      p <- html %>% xml_find_first("/html/body/document/type/sequence/filename/text")
    }
    # Get children
    p <- p %>% xml_children()
    # For each, extract text
    res <- sapply(p, text.recursion)
    # Filter NULLS
    res <- res[sapply(res, is.null) == FALSE]
    # Collapse
    res <- paste0(res, collapse = " ")
    # Return
    return(res)
  }

  # Function 'preprocess'. Preprocesses a piece of text by stripping out additional spaces etc..
    # param: text - raw text extracted from 10-K report
    # Returns: pre-processed text.
        # * Lower case letters
        # * All \n \t removed
        # * Excess whitespace removed

  preProcess <- function(text) {

    # PREPROCESS FUNCTION: strips all whitespace in between words
      # Input: text
      # Output: text without superfluous spacing
    stripspace <- function(text) {
      # SPlit
      text_split <- str_split(text, "\\s")
      # Remove whitespaces
      WS <- sapply(text_split, function(x) ifelse(x == " " | x == "", FALSE, TRUE))
      # Subset
      text_split <- text_split[[1]][WS]
      # Rejoin and return
      return(paste(text_split, collapse = " "))
    }

    # Pre-process text - Replace all tabs & whitespaces
    text <- gsub("[\r\t\n]", " ", text)

    # Strip whitespace
    text <- stripspace(text)

    # Return
    return(text)

  } #'End of function preProcess'

  # Function 'extractBD'. Extracts business description from entire 10-K report..
    # param: text - processed text (processed by 'preProcess', see lines 74-103)
    # Returns: Business description section

  extractBD <- function(text) {

    # Pre-process data
    text <- preProcess(text)

    # To lowercase letters
    h <- tolower(text)
    #h <- stringi::stri_trans_general(h, "latin-ascii")

    # Number of characters in report
    char <- str_count(h)

    # Find mentions of "not applicable"
    NA_found <- ifelse(length(str_extract_all(h, "not applicable")[[1]]) >= 1, T, F)

    # Locate "part i item 1"
    item1 <- tbl_df(as.data.frame(str_locate_all(h, "part i item 1(.?)")[[1]]))  # IF length rows > 1, then a TOC is present in this document OR cautionary statement prevents finding of this combination
    if( nrow(item1) == 1) {
      # Take substring to check if in toc
      subs <- str_sub(h, item1$start, (item1$end + 400))
      # Number of items
      nitems <- length(str_extract_all(subs, "item")[[1]])
      # Check for longdots
      ndots <- length(str_extract_all(subs, "\\.")[[1]])
      dotdensity <- ndots / nchar(subs)
      # Item density (how many times 'item' is mentioned relative to length of the substring 'subs')
      itemdensity <- (nitems * 4) / nchar(subs)
      # If item density > 5%, look for business
      if( itemdensity >= 0.05 | dotdensity >= 0.2 ) {
        item1 <- tbl_df(as.data.frame(str_locate_all(h, "item 1.\\s?business")[[1]]))
      }
    }
    # If len == 0, then look for item 1 business
    if( nrow(item1) == 0 ) {
      # Look for business
      item1 <- tbl_df(as.data.frame(str_locate_all(h, "item [1|i].\\s?business")[[1]]))
    }
    # If still 0, then try the following
    if( nrow(item1) == 0 ) {
      item1 <- tbl_df(as.data.frame(str_locate_all(h, "item[s]? 1\\.? and 2\\.?")))
    }
    # If still 0, try the following
    if( nrow(item1) == 0 ) {
      item1 <- tbl_df(as.data.frame(str_locate_all(h, "item 1\\s?[\\.|-]?\\s?description")))
    }
    # If still 0, try
    if( nrow(item1) == 0 ) {
      item1 <- tbl_df(as.data.frame(str_locate_all(h, "item 1\\s?-\\s?business")))
    }
    # For each, check where mention in document is ( needs to be early on in the document )
    ch <- item1$start / char
    # Subset. Item cannot be beyond 30% of entire document.
    item1 <- item1[ch <= 0.2, ]
    # Subset
    if( nrow(item1) > 1 ) {
      # Subset
      h <- str_sub(h, (item1$end[2] + 1), char)
      text <- str_sub(text, (item1$end[2] + 1), char)
    } else {
      h <- str_sub(h, (item1$end[1] + 1), char)
      text <- str_sub(text, (item1$end[1] + 1), char)
    }

    # Locate "item 1a."
    stop_condition <- tbl_df(as.data.frame(str_locate_all(h, "item 1a(.?)")[[1]]))
    # For each, check before and after to check if "see" or "refer to" occurs
    if ( nrow(stop_condition) > 0 ) {
      takeout <- c()
      for( i in 1:nrow(stop_condition) ) {
        sub <- stop_condition[i, ]
        # String
        subst <- str_sub(h, (sub$start - 100), sub$end)
        # Check for "see" and 'refer to'
        check <- ifelse(grepl("see", subst) |
                          grepl("refer to", subst) |
                          grepl("described(\\s)?(below)? (in|under)", subst) |
                          grepl("contained(\\s)?(below)? (in|under)", subst) |
                          grepl("discussed(\\s)?(below)? (in|under)", subst) |
                          grepl("included(\\s)?(below)? (in|under|as)", subst) |
                          grepl("set forth (in|under)", subst) |
                          grepl("outlined(\\s)?(below)? (in|under)", subst) |
                          grepl("provided(\\s)?(below)? (in|under)", subst) |
                          grepl("set forth (in|under)", subst) |
                          grepl("detailed(\\s)?(below)? (in|under)", subst) |
                          grepl("can be found (in|under)", subst),
                        T, F)
        # Second check. See if directly preceded by "in" and "under"
        subst <- str_sub(h, (sub$start - 4), sub$end)
        check2 <- ifelse(grepl("in ", subst), T, F)
        subst <- str_sub(h, (sub$start - 7), sub$end)
        check3 <- ifelse(grepl("under ", subst), T, F)
        if( any(check,check2,check3) == T | is.na(subst) ) {
          takeout <- c(takeout, i)
        }
      }
      # Remove
      if(!is.null(takeout)) {
        stop_condition <- stop_condition[-takeout,]
      }
    }

    # If empty
    if( nrow(stop_condition) == 0 ) {
      # Else, locate next item (only if part i item 1 is found)
      stop_condition <- tbl_df(as.data.frame(str_locate_all(h, "item 2.\\s?(description of\\s)?propert")[[1]]))
    }
    # Extract
    BD <- str_sub(text, 1, (stop_condition$start[1] - 1))

    # Return
    return(BD)

  } #'End of function extractBD'

  ############
  # Call -----
  ############

  # If URL is FTP, resolve
  if( FTP ) {
    # Resolve
    res <- TenK_get( URL, meta_list, type = "ftp" )
    # If not metadata, delete
    if( metadata == FALSE ) {
      res <- list("htm10kurl" = res$htm10kurl)
    }
    # Set URL to HTML
    URL <- res$htm10kurl
  } else {
    # Resolve if metadata is not turned off
    if ( metadata ) {
      # Resolve
      res <- TenK_get( URL, meta_list, type = "html" )
    } else {
      res <- list("htm10kurl" = URL)
    }
  }

  # Get HTML
  rep <- get(URL)

  # If entire report
  if( retrieve == "ALL" ) {
    # Retrieve text
    rep <- rep %>% get.text %>%
      preProcess()
    # Add to list and return
    res$report <- rep
    # Return
    return(res)
  } else { # Else business description
    BD <- extractBD(rep %>% get.text())
    # If FTP, then res exists --> append
    res$busdescription <- BD
    # Return
    return(res)
  }
}
