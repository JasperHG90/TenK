## ---- eval=FALSE---------------------------------------------------------
#  # Install if not present
#  if(!require(devtools)) install.packages("devtools")
#  # Load
#  library(devtools)
#  # Install TenK from github
#  install_github("JasperHG90/TenK")

## ------------------------------------------------------------------------
library(TenK)

## ------------------------------------------------------------------------
data("filings10K2013") # Exchange 2013 for 2014, 2015 or 2016 for later years.
dim(filings10K2013)

## ---- eval=T-------------------------------------------------------------
# Retrieve business description
res <- TenK_process(filings10K2013$ftp_url[1], retrieve = "BD")
# Print
print(names(res))

## ------------------------------------------------------------------------
# Retrieve business description
res2 <- TenK_process(res$htm10kurl, retrieve = "BD")
# Print
print(names(res2))

## ------------------------------------------------------------------------
# Retrieve business description - select metadata fields
res_select <- TenK_process(filings10K2013$ftp_url[1], 
                           meta_list = list("filing_date" = 1,
                                            "date_accepted" = 1,
                                            "htm10kinfo" = 1), 
                           retrieve = "BD")
print(names(res_select))

## ------------------------------------------------------------------------
# Retrieve business description without metadata
res_no_metadata <- TenK_process(filings10K2013$ftp_url[1], metadata = FALSE)
# Print
print(names(res_no_metadata))

## ---- eval=F-------------------------------------------------------------
#  library(rjson) # Install if you don't have it
#  # Convenience function
#  savetojson <- function(data, path) {
#    # Write
#    g <- toJSON(data)
#    write(g, paste0(path, "/data.json"))
#  }
#  # Run
#  savetojson(res, "/users/jasper/desktop")

