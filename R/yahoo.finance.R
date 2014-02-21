#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>
# Yahoo Finance interface.

library(RCurl)
library(XML)

# URLs for Yahoo Finance CSV API
url.yahoo.finance.base <- 'http://biz.yahoo.com/p/csv'
url.yahoo.finance.sector <- paste(url.yahoo.finance.base, 's_conameu.csv', 
                                  sep='/')
url.yahoo.industry.list <- 'http://biz.yahoo.com/ic/ind_index_alpha.htm'

# Helper function to safely download data using Curl.
yahoo.download.csv.as.df <- function(url, set.id=TRUE) {
  # NOTE: CSV has a NUL at the end
  csv <- rawToChar(getURLContent(url, binary=TRUE))
  read.csv(textConnection(csv))
}

# Return a dataframe of all sectors in Yahoo Finance.
yahoo.sectors <- function() {
  df <- yahoo.download.csv.as.df(url.yahoo.finance.sector)
  # sector ID is "1-indexed rank by name in the sector list"
  df$ID <- 1:nrow(df)
  return(df)
}

# Parse the list of industry IDs from the URL
#   http://biz.yahoo.com/ic/ind_index_alpha.html
# It is a mystery why Yahoo does not provide an API for this.
yahoo.industry.ids <- function() {
  html <- htmlParse(url.yahoo.industry.list)
  html.names <- as.vector(xpathSApply(html, "//td/a/font", xmlValue))
  html.urls <- as.vector(xpathSApply(html, "//td/a/font/../@href"))
  
  if (length(html.names) != length(html.urls)) {
    warning(paste("Got", length(html.names), "names but", 
                  length(html.urls), "URLs"))
  }

  html.names <- gsub("\n", " ", html.names)
  html.urls <- gsub("http://biz.yahoo.com/ic/([0-9]+).html", "\\1", html.urls)
  
  df <- data.frame(Name=character(length(html.urls)), 
                   ID=numeric(length(html.urls)), stringsAsFactors=FALSE)
  for (i in 1:length(html.urls)) {
    url = html.urls[i]
    val = suppressWarnings(as.numeric(url))
    if (! is.na(val) ) {
      df[i,'Name'] = html.names[i]
      df[i,'ID'] = val
    }
  }
  return(df)
}

# Return a dataframe of industries in a sector. If sector is NULL,
# this invokes yahoo.sector.industries.all(). Note that sector is 
# an integer ID, as provided in the dataframe returned by yahoo.sectors().
# The id.df parameter is a dataframe as returned by yahoo.industry.ids();
# this allows the user to avoid calling yahoo.industry.ids() repeatedly.
yahoo.sector.industries <- function( sector=NULL, id.df=NULL ) {
  if (is.null(id.df)) {
    id.df <- yahoo.industry.ids()
  }
  
  if (is.null(sector)) {
    return(yahoo.sector.industries.all(id.df))
  }
    
  url <- paste(url.yahoo.finance.base, 
               paste(as.integer(sector), 'conameu.csv', sep=''), 
               sep='/')
  df <- yahoo.download.csv.as.df(url)
  
  # fix broken Industry names
  df[,'Industry'] <- gsub(' +', ' ', df[,'Industry'])
  
  # default ID column
  df$ID <- (sector * 100) + 1:nrow(df)
  
  # set IDs based on http://biz.yahoo.com/ic/ind_index_alpha.html
  for (i in 1:nrow(id.df)) {
    name <- id.df[i, 'Name']
    if (nrow(df[df$Industry == name,]) > 0) {
      df[df$Industry == name, 'ID'] <- id.df[i, 'ID']
    }
  }
  
  df$Sector <- sector
  return(df)
}

# Return a dataframe of all industries in all sectors.
# See yahoo.sector.industres() for more detail.
yahoo.sector.industries.all <- function(id.df=NULL) {
  sec.df <- yahoo.sectors()
  ind.df <- NULL
  for (id in sec.df$ID) {
    df <- yahoo.sector.industries(id, id.df)
    if (is.null(ind.df)) {
      ind.df <- df
    } else {
      ind.df <- rbind(ind.df, df)
    }
  }
  return(ind.df)
}

# Return a dataframe of symbols in the specified industry.
# Note that industry is a numeric ID as provided by the 
# dataframe returned by yahoo.sector.industries().
yahoo.industry.symbols <- function( industry ) {
  url <- paste(url.yahoo.finance.base, 
               paste(as.integer(industry), 'conameu.csv', sep=''), 
               sep='/')
  df <- yahoo.download.csv.as.df(url)
  return(df)
}

# Return a character vector of details for the specified symbol.
# This vector consists of the following (named) elements, in order:
# Index Membership, Sector, Industry, Full Time Employees
yahoo.symbol.details <- function( sym ) {
  url <- paste('http://finance.yahoo.com/q/pr?s=', sym, sep='')
  html <- htmlParse(url)
  keys <- xpathSApply(html, 
              "//table[@class='yfnc_datamodoutline1']//td[@class='yfnc_tablehead1']", xmlValue)
  vals <- xpathSApply(html, 
              "//table[@class='yfnc_datamodoutline1']//td[@class='yfnc_tabledata1']", xmlValue)
  
  keys <- gsub(':', '', keys)
  vals <- gsub('([[:lower:]])([[:upper:]])', '\\1; \\2', vals)
  
  vec <- vals[1:4]
  names(vec) <- keys[1:4]
  return(vec)
}