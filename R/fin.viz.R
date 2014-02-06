#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>

library(RCurl)
library(XML)

url.finviz.base <- 'http://finviz.com/export.ashx?o=ticker'
#filter options:= &f=idx_sp500,sec_basicmaterials
url.finviz.screener <- 'http://finviz.com/screener.ashx?ft=4'

# Naughty FinViz, checking UserAgent like that!
finviz.user.agent <- 'Mozilla/5.0'



finviz.screener.options <- function() {
  html.raw <- getURLContent(url.finviz.screener, followlocation = TRUE, 
                            timeout = 100, useragent = finviz.user.agent)
  
  if (is.na(html.raw) || length(html.raw) == 0) {
    warning(paste("Read zero-length page from", url.finviz.screener))
    return(NULL)
  }
  
  return( htmlParse(html.raw) )
}

build.name.value.dataframe <- function(data.names, data.values, prefix) {
  if (length(data.names) != length(data.values)) {
    warning(paste('Names has length', length(data.names),
                  'but Values has length', length(data.values)))
    return(NULL)
  }
  
  df <- data.frame(Name=character(length(data.names)), 
                   Symbol=character(length(data.values)), 
                   stringsAsFactors=FALSE)
  
  for (i in 1:length(data.names)) {
    name = data.names[i]
    val = data.values[i]
    if (nchar(val) == 0) next
    
    df[i,'Name'] = name
    df[i,'Symbol'] = paste(prefix, val, sep='_')
  }
  
  # strip empty rows (i.e. "Any" option)
  return( df[nchar(df$Name) > 0,] )
}

finviz.extract.select.options <- function(id, prefix, html=NULL) {
  
  if (is.null(html)) {
    html <- finviz.screener.options()
    if (is.null(html)) {
      return(c())
    }
  }
  
  path <- paste("//select[@id='", id, "']/option", sep="")
  option.names <- xpathSApply(html, path, xmlValue)
  path <- paste("//select[@id='", id, "']/option/@value", sep="")
  option.values <- xpathSApply(html, path)
  
  return(build.name.value.dataframe(option.names, option.values, prefix))
}

finviz.sectors <- function(html=NULL) {
  finviz.extract.select.options('fs_sec', 'sec', html)
}

finviz.industries <- function(html=NULL) {
  finviz.extract.select.options('fs_ind', 'ind', html)
}

finviz.indexes <- function(html=NULL) {
  finviz.extract.select.options('fs_idx', 'idx', html)
}

finviz.exchanges <- function(html=NULL) {
  finviz.extract.select.options('fs_exch', 'exch', html)
}

finviz.locations <- function(html=NULL) {
  finviz.extract.select.options('fs_geo', 'geo', html)
}

finviz.market.caps <- function(html=NULL) {
  finviz.extract.select.options('fs_cap', 'cap', html)
}

finviz.query <- function(filters) {
  url <- paste(url.finviz.base, '&f=', paste(filters, collapse=','), sep='')
  csv <- getURLContent(url, followlocation = TRUE, timeout = 100, 
                       useragent = finviz.user.agent)
  if ((! is.na(csv)) && length(csv) > 0 ) {
    read.csv(textConnection(csv), header=TRUE)
  } else {
    c()
  }
}

finviz.sector.symbols <- function(industry.sym) {
  finviz.query(sector.sym)
}

finviz.industry.symbols <- function(industry.sym) {
  finviz.query(industry.sym)
}

finviz.get.symbol <- function(sym) {
  url <- paste(url.finviz.base, '&t=', paste(sym, collapse=','), sep='')
  csv <- getURLContent(url)
  if ((! is.na(csv)) && length(csv) > 0 ) {
    read.csv(textConnection(csv), header=TRUE)
  } else {
    c()
  }
}