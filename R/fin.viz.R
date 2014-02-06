#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>
# FinViz interface

library(RCurl)
library(XML)
library(png)

# FinViz URLs
url.finviz.base <- 'http://finviz.com/export.ashx?o=ticker'
url.finviz.chart <- 'http://finviz.com/publish.ashx?'
url.finviz.search <- 'http://finviz.com/search.ashx?'
url.finviz.screener <- 'http://finviz.com/screener.ashx?ft=4'
url.finviz.symbol <- 'http://finviz.com/quote.ashx?'

# Naughty FinViz, checking UserAgent like that!
finviz.user.agent <- 'Mozilla/5.0'


# Helper function to download a page from FinViz with a spoofed UserAgent
finviz.htmlParse <- function(url) {
 html.raw <- getURLContent(url, followlocation = TRUE, timeout = 100, 
                           useragent = finviz.user.agent)
 if (is.na(html.raw) || length(html.raw) == 0) {
   warning(paste("Read zero-length page from", url))
   return(NULL)
 }

 htmlParse(html.raw)
}

# ------------------------------------------------------------------------
# FILTERED QUERIES

# Helper function to return the parsed HTML of the FinViz Screener
# "All Filters" page.
finviz.screener.filters <- function() {
  return( finviz.htmlParse(url.finviz.screener) )
}

# Helper function to build a Name|Symbol dataframe from two character vectors
# and a filter symbol prefix (e.g. ind, sec, idx, etc).
build.name.symbol.dataframe <- function(data.names, data.values, prefix) {
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

# Helper function to return a dataframe with the Name & Symbol of 
# the specified filter options.
finviz.extract.filter.options <- function(id, prefix, html=NULL) {
  
  if (is.null(html)) {
    html <- finviz.screener.filters()
    if (is.null(html)) {
      return(c())
    }
  }
  
  path <- paste("//select[@id='", id, "']/option", sep="")
  option.names <- xpathSApply(html, path, xmlValue)
  path <- paste("//select[@id='", id, "']/option/@value", sep="")
  option.values <- xpathSApply(html, path)
  
  return(build.name.symbol.dataframe(option.names, option.values, prefix))
}

# Return a dataframe with the Name & Symbol of all Sector filter options.
finviz.sectors <- function(html=NULL) {
  finviz.extract.filter.options('fs_sec', 'sec', html)
}

# Return a dataframe with the Name & Symbol of all Industry filter options.
finviz.industries <- function(html=NULL) {
  finviz.extract.filter.options('fs_ind', 'ind', html)
}

# Return a dataframe with the Name & Symbol of all Index filter options.
finviz.indexes <- function(html=NULL) {
  finviz.extract.filter.options('fs_idx', 'idx', html)
}

# Return a dataframe with the Name & Symbol of all Exchange filter options.
finviz.exchanges <- function(html=NULL) {
  finviz.extract.filter.options('fs_exch', 'exch', html)
}

# Return a dataframe with the Name & Symbol of all Location filter options.
finviz.locations <- function(html=NULL) {
  finviz.extract.filter.options('fs_geo', 'geo', html)
}

# Return a dataframe with the Name & Symbol of all Market Cap filter options.
finviz.market.caps <- function(html=NULL) {
  finviz.extract.filter.options('fs_cap', 'cap', html)
}

# Helper function to perform a query using the FinViz export URL.
# The filters argument is a character vector containing one filter
# symbol (e.g. "sec_financial") per element.
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

# Return a dataframe of all symbols in the specified sector.
finviz.sector.symbols <- function(sector.sym) {
  finviz.query(sector.sym)
}

# Return a dataframe of all symbols in the specified industry.
finviz.industry.symbols <- function(industry.sym) {
  finviz.query(industry.sym)
}

# ------------------------------------------------------------------------
# SYMBOL INFO

# Helper function to return a specific page/view (112, 122, ...) from
# the FinVis quote page for a symbol.
finviz.get.symbol.page <- function(sym, page) {
  url <- paste(url.finviz.base, '&v=', page, '&t=', 
               paste(sym, collapse=','), sep='')
  csv <- getURLContent(url)
  if ((! is.na(csv)) && length(csv) > 0 ) {
    read.csv(textConnection(csv), header=TRUE)
  } else {
    c()
  }
}

# Return a dataframe containing the current quote for the specified symbol.
finviz.get.symbol <- function(sym) {
  sym.df <- NULL
  for (i in c(112, 122, 132, 142, 162, 172)) {
    df <- finviz.get.symbol.page(sym, i)
    if (is.null(sym.df)) {
      sym.df <- df
    } else {
      sym.df <- merge(sym.df, df)
    }
  }
  return(sym.df)
}

# Returns a dataframe giving the latest news for the specified symbol.
# Columns include Timestamp, URL, and Headline.
finviz.symbol.news <- function(sym) {
  url <- paste(url.finviz.symbol, "t=", sym, sep='')
  html <- finviz.htmlParse(url) 
  
  news.urls <- xpathSApply(html, 
                           "//table[@class='fullview-news-outer']/tr/td/a/@href")
  news.text <- xpathSApply(html, 
                           "//table[@class='fullview-news-outer']/tr/td/a", 
                           xmlValue)
  news.ts <- xpathSApply(html, 
                         "//table[@class='fullview-news-outer']/tr/td[@align='right']", 
                         xmlValue)
  # FIXME: Timestamp should be made a real datetime object, but the date 
  #        component is not always present.
  data.frame(Timestamp=news.ts, URL=news.urls, Headline=news.text)
}


# Display the FinViz chart for a symbol using the PNG package
# The 'type' parameter can be 'l' (line), 'c' (candle)
# The 'period' parameter can be 'd' (daily), 'w' (weekly), 'm' (monthly)
# 'advanced' can be zero or one: it only is valid for daily charts.
finviz.symbol.chart <- function(sym, period='d', type='c', advanced=TRUE) {
  adv <- 0
  if (advanced && period == 'd') adv <- 1
  
  opts <- paste('ty=', type, '&ta=', adv, '&p=', period, '&s=l', sep='')
  url <- paste( url.finviz.chart, opts, "&t=", sym, sep='' )
  html <- finviz.htmlParse(url) 
  
  img.url <- xpathSApply(html, 
          "//table[@class='body-text']//input[@id='dynamic']/@value")
  png <- getURLContent(img.url, binary=TRUE)
  img <- readPNG(png)
  
  grid::grid.raster(img)
}

# ------------------------------------------------------------------------
# COMPANY INFO

# Returns a dataframe containing company profile information from the
# FinViz "company profile" search page:
#   http://finviz.com/search.ashx?t=p&p=$QUERY
# Note that this will search for a company name NOT ticker symbol.
# Multiple rows will be returned if more than one company matches.
finviz.company.profile <- function(query) {
  url <- paste(url.finviz.search, "t=p&p=", query, sep='')
  html <- finviz.htmlParse(url) 
  
  rows <- xpathSApply(html, "//table/tr/td/table/tr/td", xmlValue)
  num.results = length(rows) / 10
  if (num.results < 1) {
    warning(paste("No results found for '", query, "'", sep=''))
    return(NULL)
  }
  
  df <- data.frame( Ticker=character(num.results),
                    Name=character(num.results),
                    Sector=character(num.results),
                    Industry=character(num.results),
                    Location=character(num.results),
                    MarketCap=character(num.results),
                    Profile=character(num.results),
                    stringsAsFactors=FALSE)
  
  for (i in 1:num.results) {
    idx = (i - 1) * 10
    df[i, 'Ticker'] = rows[idx + 2]
    df[i, 'Name'] = rows[idx + 3]
    df[i, 'Sector'] = rows[idx + 4]
    df[i, 'Industry'] = rows[idx + 5]
    df[i, 'Location'] = rows[idx + 6]
    df[i, 'MarketCap'] = rows[idx + 7]
    df[i, 'Profile'] = rows[idx + 9]
  }

  return(df)
}