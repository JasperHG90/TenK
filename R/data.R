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

#' All FTP links to filed 10-K reports in 2013
#'
#' A dataset containing FTP links to all filed 10-K reports in 2013.
#'
#' @format data frame, 4926 rows, 5 variables:
#' \describe{
#'   \item{CIK}{Central Index Key (CIK) code for each company. See \url{https://www.sec.gov/edgar/searchedgar/cik.htm}.}
#'   \item{company_name}{name of the company.}
#'   \item{report_type}{Type of filing. Should all be 10-K files.}
#'   \item{filing_date}{Date the 10-K report was filed to the SEC.}
#'   \item{ftp_url}{URL pointing to the report hosted on the SEC FTP server.}
#' }
#' @source \url{ftp://ftp.sec.gov/edgar/full-index/2013/QTR1/master.idx}
"filings10K2013"

#' All FTP links to filed 10-K reports in 2014
#'
#' A dataset containing FTP links to all filed 10-K reports in 2014.
#'
#' @format data frame, 5553 rows, 5 variables:
#' \describe{
#'   \item{CIK}{Central Index Key (CIK) code for each company. See \url{https://www.sec.gov/edgar/searchedgar/cik.htm}.}
#'   \item{company_name}{name of the company.}
#'   \item{report_type}{Type of filing. Should all be 10-K files.}
#'   \item{filing_date}{Date the 10-K report was filed to the SEC.}
#'   \item{ftp_url}{URL pointing to the report hosted on the SEC FTP server.}
#' }
#' @source \url{ftp://ftp.sec.gov/edgar/full-index/2014/QTR1/master.idx}
"filings10K2014"

#' All FTP links to filed 10-K reports in 2015
#'
#' A dataset containing FTP links to all filed 10-K reports in 2015.
#'
#' @format data frame, 5671 rows, 5 variables:
#' \describe{
#'   \item{CIK}{Central Index Key (CIK) code for each company. See \url{https://www.sec.gov/edgar/searchedgar/cik.htm}.}
#'   \item{company_name}{name of the company.}
#'   \item{report_type}{Type of filing. Should all be 10-K files.}
#'   \item{filing_date}{Date the 10-K report was filed to the SEC.}
#'   \item{ftp_url}{URL pointing to the report hosted on the SEC FTP server.}
#' }
#' @source \url{ftp://ftp.sec.gov/edgar/full-index/2014/QTR1/master.idx}
"filings10K2015"

#' All FTP links to filed 10-K reports in 2016
#'
#' A dataset containing FTP links to all filed 10-K reports in 2016.
#'
#' @format data frame, 5462 rows, 5 variables:
#' \describe{
#'   \item{CIK}{Central Index Key (CIK) code for each company. See \url{https://www.sec.gov/edgar/searchedgar/cik.htm}.}
#'   \item{company_name}{name of the company.}
#'   \item{report_type}{Type of filing. Should all be 10-K files.}
#'   \item{filing_date}{Date the 10-K report was filed to the SEC.}
#'   \item{ftp_url}{URL pointing to the report hosted on the SEC FTP server.}
#' }
#' @source \url{ftp://ftp.sec.gov/edgar/full-index/2016/QTR1/master.idx}
"filings10K2016"
