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

## ------------------------------------------------------------------------
library(rjson)
res <- fromJSON(file = "/users/jasper/desktop/data.json")

## ---- include=FALSE, results="hide"--------------------------------------
library(RPostgreSQL)
# Open connection
db <- dbConnect(PostgreSQL(), user = "Jasper", host = "127.0.0.1")
# Drop db
dbSendQuery(db, "DROP TABLE IF EXISTS tenk_reports ;")
# Disconnect
dbDisconnect(db)

## ------------------------------------------------------------------------
# Install if you don't have this package
library(RPostgreSQL)
# Open connection
db <- dbConnect(PostgreSQL(), user = "Jasper", host = "127.0.0.1")
# Create table
q <- dbSendQuery(db, "CREATE TABLE tenk_reports (
                        cik integer,
                        arc bigint,
                        index_url VARCHAR(150),
                        company_name VARCHAR(150),
                        filing_date date,
                        date_accepted TIMESTAMP,
                        period_report date,
                        htm10kurl VARCHAR(150),
                        htm10kinfo_10K_url VARCHAR(100),
                        htm10kinfo_type CHAR(4),
                        htm10kinfo_size integer,
                        htm10kinfo_extension CHAR(3),
                        ftpurl VARCHAR(150),
                        report TEXT
                      );")

## ------------------------------------------------------------------------
# Add data - easiest way is to convert list to df
res_df <- as.data.frame(res, stringsAsFactors = F)
# This gives a df with 14 columns and 1 row
dim(res_df)
# Replace '.' with '_' and lowercase names
names(res_df) <- gsub("\\.", "_", names(res_df)) 
names(res_df) <- tolower(names(res_df))
# Write
dbWriteTable(db, "tenk_reports", res_df, append=T, 
             row.names = F)

## ------------------------------------------------------------------------
dbCheck <- function(conn, URL) {
  # Query
  q <- dbSendQuery(db, paste0("SELECT exists (SELECT 1 FROM tenk_reports WHERE htm10kurl = ",
                              "'",
                              URL, 
                              "'",
                              " LIMIT 1);"))
  # Fetch
  f <- fetch(q)
  # Clear result
  dbClearResult(q)
  # Return exists
  return( f$exists )
}

## ------------------------------------------------------------------------
# Run check
check <- dbCheck(db, res$htm10kurl)
# Print
print(check)

## ------------------------------------------------------------------------
res_query <- dbReadTable(db, "tenk_reports")
# Disconnect
dbDisconnect(db)

## ------------------------------------------------------------------------
str(res_query, nchar.max = 10)

## ------------------------------------------------------------------------
library(rmongodb) # install if you don't have it
# Details
database <- "tenk_reports"
col <- "records"
ns <- paste0(database,".",col)
print(ns)
# Create mongo connection
m <- mongo.create()

## ------------------------------------------------------------------------
# Insert data
mongo.insert(m, ns, res)

## ------------------------------------------------------------------------
recExists <- function(mongo_connection, ns, URL) {
  # Find
  q <- mongo.find.all(mongo_connection, ns, query = list("htm10kurl" = URL))
  # If len >0 , return TRUE, else return FALSE
  ifelse( length(q) > 0, return(TRUE), return(FALSE))
}

## ------------------------------------------------------------------------
ex <- recExists(m, ns, res$htm10kurl)
print(ex)

## ------------------------------------------------------------------------
# Find one record
res_mongo <- mongo.find.one(m, ns, query = list("htm10kurl" = res$htm10kurl))
# Note that the result is a mongo BSON 
class(res_mongo)
# We can turn it into an R list like this
res_mongo_list <- mongo.bson.to.list(res_mongo)
# We can also query all records at once - note that these get converted to a list immediately
res_mongo <- mongo.find.all(m, ns)
# Disconnect
mongo.destroy(m)

## ---- include=FALSE, results="hide"--------------------------------------
# Drop mongo collection
mongo <- mongo.create()
mongo.drop.database(mongo, "tenk_reports")
mongo.destroy(mongo)

## ------------------------------------------------------------------------
str(res_mongo, nchar.max = 10)

## ---- eval=FALSE---------------------------------------------------------
#  # Save to Rdata
#  save(results, file="/users/jasper/desktop/results.Rdata")

## ---- eval=FALSE---------------------------------------------------------
#  # Load
#  load(file="/users/jasper/desktop/results.Rdata")

