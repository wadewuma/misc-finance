#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>
# Yahoo Finance interface.

library(RCurl)

url.yahoo.finance.base <- 'http://biz.yahoo.com/p/csv'
url.yahoo.finance.sector <- paste(url.yahoo.finance.base, 's_conameu.csv', 
                                  sep='/')

yahoo.download.csv.as.df <- function(url, set.id=TRUE) {
  # NOTE: CSV has a NUL at the end
  csv <- rawToChar(getURLContent(url, binary=TRUE))
  read.csv(textConnection(csv))
}

# See: https://code.google.com/p/yahoo-finance-managed/wiki/csvSectorDownload
yahoo.sectors <- function() {
  df <- yahoo.download.csv.as.df(url.yahoo.finance.sector)
  # sector ID is "1-indexed rank by name in the sector list"
  df$ID <- 1:nrow(df)
  return(df)
}

yahoo.industry.ids <- function() {
  html <- htmlParse('http://biz.yahoo.com/ic/ind_index_alpha.html')
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


# See: https://code.google.com/p/yahoo-finance-managed/wiki/csvIndustryDownload
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

yahoo.industry.symbols <- function( industry ) {
  url <- paste(url.yahoo.finance.base, 
               paste(as.integer(industry), 'conameu.csv', sep=''), 
               sep='/')
  df <- yahoo.download.csv.as.df(url)
}