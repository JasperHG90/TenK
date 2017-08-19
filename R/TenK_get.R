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

#' Converts an FTP url from an SEC 10-K report to a URL pointing towards the
#' HTML version of that report.
#'
#' \code{TenK_get} converts an FTP url from an SEC 10-K report to a URL pointing
#' towards the HTML version of that report. The function will also collect and
#' return metadata if desired by the user. This function should not be called
#' directly; it is called from \code{TenK_process}.
#'
#' @param URL url belonging to the 10-k annual report
#' @param meta_list Function returns a list with additional meta information
#'   (See overview of variables here: \url{https://goo.gl/AJFUul}). Choose which
#'   fields you want to return. Defaults to "ALL".
#' @param type Is the url an FTP or an HTML url?
#' @return Depending on the 'meta_list' parameter, the function returns a list
#'   with useful information. Please refer to the documentation for additional
#'   information. To view the documentation, execute 'vignette("TenK")'
#'
#' @seealso See \url{https://goo.gl/pNMMcK} for documentation on this package.
#'   See also \code{\link{TenK_process}}.
#' @author Jasper Ginn
#' @importFrom xml2 read_html
#' @importFrom rvest html_node html_table html_text
#' @importFrom stringr str_replace str_split str_replace_all str_sub str_detect
#' @importFrom purrr map
#' @import dplyr

TenK_get <- function( URL, type = c("ftp", "html"), meta_list ) {

  # Match arg
  type <- match.arg(type)

  # Check meta_list. If empty, then all arguments.
  if ( length(meta_list) > 0 ) {
    meta_fields <- names(meta_list)
  } else {
    meta_fields <- c("company_name",
                     "htm10kinfo",
                     "filing_date",
                     "date_accepted",
                     "period_report",
                     "filer_info")
  }

	###############################
	# Define helper functions -----
	###############################

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

	# Function 'resolve'. If URL passed to TenK_get is an FTP url, then reconstruct such
	# that it points to the HTML version. See documentation for more information
	  # param: ftp.url - url pointing to a 10-K document residing on SEC FTP servers.
	  # Returns: List with information about FTP & HTML reports

	resolve <- function(ftp.url, meta_fields, type) {

    # Function 'constructURL'. Takes the ftp url and uses it to construct a url to the html
	  # version of the report.
	    # param: ftp.url - url pointing to a 10-K document residing on SEC FTP servers.
	    # returns: list with company information (ARC, CIK, INDEX URL)

	  constructURL <- function(ftp.url, meta_fields, type) {

	    # Depending on type
	    if( type == "ftp" ) {
	      # 1. Remove FTP url
	      ftp.url <- str_replace(ftp.url,
	                             "ftp://ftp.sec.gov/edgar/data/",
	                             "")
	      # 2. Remove .txt at the end of the URL
	      ftp.url <- str_replace(ftp.url,
	                             ".txt",
	                             "")
	      # 3. Split CIK from file archive number
	      ftp.url.split <- unlist(str_split(ftp.url, "\\/"))
	      # If length != 2, stop
	      if(length(ftp.url.split) != 2) {
	        stop("Cannot resolve FTP url.")
	      }
	    } else {
	      # This is a html url
        ftp.url <- str_replace(ftp.url,
                               "https://www.sec.gov/Archives/edgar/data/",
                               "")
        # Split
        ftp.url.split <- unlist(str_split(ftp.url,
                               "\\/"))
        # Replace all dashes
        ftp.url.split[2] <- str_replace_all(ftp.url.split[2], "\\-", "")
        # Reform
        ftp.url.split[2] <- paste0(str_sub(ftp.url.split[2], 1, 10),
                                   "-",
                                   str_sub(ftp.url.split[2], 11, 12),
                                   "-",
                                   str_sub(ftp.url.split[2], 13, 19))


	    }

	    # Elements for new url
	    EL <- list(base = "https://www.sec.gov/Archives/edgar/data/")
	    EL$CIK = ftp.url.split[1]
	    EL$ARC = str_replace_all(ftp.url.split[2], "\\-", "")
	    EL$IND = paste0(ftp.url.split[2], "-index.htm")

	    # Add HTM to index
	    EL$index.url <- paste0(EL$base,
	                           ftp.url.split[1],
	                           "/", str_replace_all(ftp.url.split[2], "-", ""),
	                           "/", paste0(ftp.url.split[2], "-index.htm"))

	    # Construct & Return
	    return(EL)

	  } #'End of function constructURL'

	  # Function 'get10khtm'. Takes output from 'constructURL' (see lines 62-101) and scrapes
	  # Information from the index html document.
	    # param: info.list - output of the 'constructURL' function
	    # returns: info.list with added fields (COMPANY NAME, HTML URL, INFO HTML DOCUMENT,
	    #          FILING DATE, FISCAL YEAR)

	  get10khtm <- function(info.list, meta_fields) {
	    # Get index page
	    index <- get(info.list$index.url)
	    # If NULL, return NULL
	    if(is.null(index)) {
	      stop("Could not load report.")
	    }
	    # Grab the index table
	    t <- tryCatch({
	      index %>%
	        html_node(xpath = "//*[@id='formDiv']/div[4]/table") %>%
	        html_table() %>%
	        filter(., Type == "10-K")
	    }, error = function(e) {
	      tryCatch({
	        index %>%
	          html_node(xpath = "//*[@id='formDiv']/div[3]/table") %>%
	          html_table() %>%
	          filter(., Type == "10-K")
	      }, error = function(e) {
	        index %>%
	          html_node(xpath = "//*[@id='formDiv']/div/table") %>%
	          html_table() %>%
	          filter(., Type == "10-K")
	      })
	    })
	    # If more than 1 observation, then filter for .htm extension
	    if( nrow(t) > 1 ) {
	      t <- t[endsWith(t$Document, ".htm"),]
	    }
	    # If nrow == 0, return NULL
	    if(nrow(t) == 0) {
	      stop(paste0("Cannot extract 10-K information from index page ", index))
	    }
	    # Grab company name
	    if ("company_name" %in% meta_fields) {
	      CN <- tryCatch({
	        index %>%
	          html_node(xpath="//*[@id='filerDiv']/div[contains(@class, 'companyInfo')]/span") %>%
	          html_text() %>%
	          str_split(., "\n") %>%
	          unlist()
	      }, error = function(e) {
	        NULL
	      })
	      # Add
	      info.list$company_name <- ifelse(!is.null(CN),
	                                       CN[1],
	                                       "NA")
	    }
	    # Grab Filing date, accepted, period of report
	    if ("filing_date" %in% meta_fields) {
	      FD <- tryCatch({
	        index %>%
	          html_node(xpath='//*[@id="formDiv"]/div[2]/div[1]/div[2]') %>%
	          html_text()
	      }, error = function(e) {
	        "NA"
	      })
	      # Add
	      info.list$filing_date <- FD
	    }
	    # Date accepted
	    if ("date_accepted" %in% meta_fields) {
	      AC <- tryCatch({
	        index %>%
	          html_node(xpath='//*[@id="formDiv"]/div[2]/div[1]/div[4]') %>%
	          html_text()
	      }, error = function(e) {
	        "NA"
	      })
	      # Add
	      info.list$date_accepted <- AC
	    }
	    # Period of report
	    if ("period_report" %in% meta_fields) {
	      PR <- tryCatch({
	        index %>%
	          html_node(xpath='//*[@id="formDiv"]/div[2]/div[2]/div[2]') %>%
	          html_text()
	      }, error = function(e) {
	        "NA"
	      })
	      # Add
	      info.list$period_report <- PR
	    }
	    # Grab filer information
	    if ("filer_info" %in% meta_fields) {
	      # Filer data
	      filer <- index %>% html_node("div#filerDiv")
	      # Mailing addresses (business and other)
	      mail <- filer %>% html_nodes("div.mailer") %>% html_text()
	      # Split mailing addresses
	      mail_address <- map(mail, function(x) {
	        one <- lapply(str_split(x, "\n")[[1]], function(x) {
	          sub_one <- trimws(x)
	          if(sub_one == "") {
	            return(NULL)
	          } else {
	            return(sub_one)
	          }
	        })
	        # Remove NULL
	        one <- one[!sapply(one, function(x) is.null(x))]
	        # Return
	        return(one)
	      })
	      # Add to data
	      info.list$filer_info$mailing_address <- mail_address
	      # Get identity info
	      identity <- filer %>% html_nodes("p.identInfo") %>% html_text()
	      # Split at |
	      identity_split <- str_split(identity, "\\|")[[1]]
	      # Clean
	      identity_clean <- map(identity_split, function(x) {
	        sub_x <- trimws(x)
	        # Clean specific
	        if(str_detect(tolower(sub_x), "state of incorp") | str_detect(tolower(sub_x), "fiscal year end")) {
	          sub_x <- str_replace(sub_x, "Type: 10-K", "")
	        }
          if(str_detect(tolower(sub_x), "film no")) {
            sub_x <- str_replace(sub_x, "Assistant Director [0-9]*", "")
            sub_x <- str_split(sub_x, "SIC: ")[[1]]
            if(length(sub_x) > 1) {
              sub_x[2] <- paste0("SIC: ", sub_x[2])
            }
          }
	        sub_x
	      })
	      # Unlist
	      identity_clean <- unlist(identity_clean, recursive = FALSE)
	      # Append to info
	      info.list$filer_info$filer <- identity_clean
	    }

	    # Append info & Return list
	    info.list$htm10kurl <- paste0(info.list$base,
	                                  info.list$CIK,
	                                  "/",
	                                  info.list$ARC,
	                                  "/",
	                                  t$Document)
	    # Add htm10kinfo
	    if ("htm10kinfo" %in% meta_fields) {
	      # Add
	      info.list$htm10Kinfo <- list("10K_URL" = t$Document,
	                                   "Type" = t$Type,
	                                   "Size" = t$Size,
	                                   "Extension" = unlist(str_split(t$Document,
	                                                                  "\\."))[2])
	    }
	    # Remove base
	    info.list$base <- NULL
	    info.list$IND <- NULL
	    # Return
	    return(info.list)
	  } #'End of function get10khtm'

	  # Get URL
	  ROL <- constructURL(ftp.url, meta_fields, type)
	  # Grab K10 info
	  ROL <- get10khtm(ROL, meta_fields)
	  # Add ftp url if type is ftp
	  if (type == "ftp") ROL$FTPurl <- ftp.url
	  # Return
	  return(ROL)

	} #'End of function Resolve'

	#######################
	# Call functions -----
  #######################

	tmp <- resolve(URL, meta_fields, type)

	###############
	# Return -----
	###############

	return(tmp)

} #'End of function TenK_get'
