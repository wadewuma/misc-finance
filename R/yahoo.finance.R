#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>
# Yahoo Finance interface.

library(RCurl)
library(XML)
library(rjson)

# URLs for Yahoo Finance CSV API
url.yahoo.finance.base <- 'http://biz.yahoo.com/p/csv'
url.yahoo.finance.sector <- paste(url.yahoo.finance.base, 's_conameu.csv', 
                                  sep='/')
url.yahoo.industry.list <- 'http://biz.yahoo.com/ic/ind_index_alpha.html'
url.yahoo.quote.base <- 'http://download.finance.yahoo.com/d/quotes.csv?s='
url.yahoo.symbol.search <- 'http://d.yimg.com/autoc.finance.yahoo.com/autoc?callback=YAHOO.Finance.SymbolSuggest.ssCallback&query='

#http://download.finance.yahoo.com/d/quotes.csv?s=%40%5EDJI,GOOG&f=nsl1op

#yahoo.user.agent <- 'Mozilla/5.0'
yahoo.user.agent <-  'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0'

yahoo.htmlParse <- function(url, binary=TRUE) {
  html.raw <- getURLContent(url, followlocation = TRUE, timeout = 100, 
                            useragent = yahoo.user.agent, binary=binary)
  if (is.na(html.raw) || length(html.raw) == 0) {
    warning(paste("Read zero-length page from", url))
    return(NULL)
  }
  htmlParse(html.raw)
}
  
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
yahoo.industries <- function() {
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
# The id.df parameter is a dataframe as returned by yahoo.industries();
# this allows the user to avoid calling yahoo.industries() repeatedly.
yahoo.sector.industries <- function( sector=NULL, id.df=NULL ) {
  if (is.null(id.df)) {
    id.df <- yahoo.industries()
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

# Return a dataframe of companies in the specified industry.
# Note that industry is a numeric ID as provided by the 
# dataframe returned by yahoo.sector.industries().
# WARNING: This returns company name, not symbol
yahoo.industry.companies <- function( industry ) {
  url <- paste(url.yahoo.finance.base, 
               paste(as.integer(industry), 'conameu.csv', sep=''), 
               sep='/')
  df <- yahoo.download.csv.as.df(url)
  return(df)
}

# Return a character vector of details for the specified symbol.
# This vector consists of the following (named) elements, in order:
# Sector, Industry, Full Time Employees
yahoo.symbol.details <- function( sym ) {
  url <- paste('http://finance.yahoo.com/quote/', sym, '/profile?p=', sym,
	       sep='')
  html <- yahoo.htmlParse(url, FALSE)
  vec <- as.vector(xpathSApply(html, "//div[@class='asset-profile-container']//*[self::span or self::strong]", xmlValue))
  keys <- character()
  vals <- character()
  for (i in seq(from=1, to=(length(vec)-1), by=2)) {
    keys <- c(keys, vec[i])
    vals <- c(vals, vec[i+1])
  }
  
  names(vals) <- keys
  return(vals)
}

# Return a character vector of stats for the specified symbol.
yahoo.symbol.stats <- function( sym ) {
  url <- paste('http://finance.yahoo.com/quote/', sym, '/key-statistics?p=', 
	       sym, sep='')
  html <- yahoo.htmlParse(url, FALSE)
  vec <- as.vector(xpathSApply(html, "//div[@id='Main']//td", xmlValue))
  keys <- character()
  vals <- character()
  for (i in seq(from=1, to=(length(vec)-1), by=2)) {
    keys <- c(keys, vec[i])
    vals <- c(vals, vec[i+1])
  }
  
  names(vals) <- keys
  return(vals)
}

# Fields that can be supplied to Yahoo Quotes
# FIXME: some of these screw up CSV parsing
yahoo.symbol.quote.fields <- c( AfterHoursChangeRealtime='c8',
      ##AnnualizedGain='g3', Ask='a', AskRealtime='b2', AskSize='a5',
      ##AverageDailyVolume='a2', Bid='b', BidRealtime='b3', BidSize='b6',
      #BookValuePerShare='b4', Change='c1', Change_ChangeInPercent='c',
      ##ChangeFromFiftydayMovingAverage='m7',
      #ChangeFromTwoHundreddayMovingAverage='m5', ChangeFromYearHigh='k4',
      #ChangeFromYearLow='j5', ChangeInPercent='p2', 
      #ChangeInPercentRealtime='k2', ChangeRealtime='c6', Commission='c3',
      Currency='c4', DaysHigh='h', DaysLow='g', DaysRange='m',
      #DaysRangeRealtime='m2', DaysValueChange='w1',
      #DaysValueChangeRealtime='w4', DividendPayDate='r1',
      ##TrailingAnnualDividendYield='d', 
      ##TrailingAnnualDividendYieldInPercent='y', DilutedEPS='e',
      ##EBITDA='j4', EPSEstimateCurrentYear='e7', EPSEstimateNextQuarter='e9',
      ##EPSEstimateNextYear='e8', ExDividendDate='q', 
      ##FiftydayMovingAverage='m3', SharesFloat='f6', HighLimit='l2',
      HoldingsGain='g4', HoldingsGainPercent='g1',
      HoldingsGainPercentRealtime='g5', HoldingsGainRealtime='g6',
      HoldingsValue='v1', HoldingsValueRealtime='v7', LastTradeDate='d1',
      LastTradePriceOnly='l1', LastTradeRealtimeWithTime='k1',
      LastTradeSize='k3', LastTradeTime='t1', LastTradeWithTime='l',
      LowLimit='l3', MarketCapitalization='j1', MarketCapRealtime='j3',
      MoreInfo='i', Name='n', Notes='n4', OneyrTargetPrice='t8',
      Open='o', OrderBookRealtime='i5', PEGRatio='r5', PERatio='r',
      PERatioRealtime='r2', PercentChangeFromFiftydayMovingAverage='m8',
      PercentChangeFromTwoHundreddayMovingAverage='m6',
      ChangeInPercentFromYearHigh='k5', PercentChangeFromYearLow='j6',
      PreviousClose='p', PriceBook='p6', PriceEPSEstimateCurrentYear='r6',
      PriceEPSEstimateNextYear='r7', PricePaid='p1', PriceSales='p5',
      ##Revenue='s6', SharesOwned='s1', SharesOutstanding='j2',
      ShortRatio='s7', StockExchange='x', Symbol='s', TradeDate='d2',
      # These are ASCII art and HTML <A> tags:
      #TickerTrend='t7', TradeLinks='t6', TradeLinksAdditional='f',
      TwoHundreddayMovingAverage='m4', Volume='v', YearHigh='k',
      YearLow='j', YearRange='w' )

yahoo.symbol.quote <- function(sym) {
  fields <- paste(yahoo.symbol.quote.fields, collapse='')
  syms <- paste(sym, collapse=',')
  url <- paste(url.yahoo.quote.base, syms, '&f=', fields, '&e=.csv', sep='')
  df.quote <- read.csv(url, header=FALSE, stringsAsFactors=FALSE,
                       col.names=names(yahoo.symbol.quote.fields))
  rownames(df.quote) <- df.quote$Symbol
  return(df.quote)
}

yahoo.symbol.for.company <- function(name) {
  url <- paste(url.yahoo.symbol.search, curlEscape(name), sep='')
  str <- rawToChar(getURLContent(url, binary=TRUE))
  str <- gsub('YAHOO.Finance.SymbolSuggest.ssCallback\\((.*)\\)$', '\\1', 
              str)
  lst <- fromJSON(str)
  rs <- lst$ResultSet$Result
  if (length(rs) < 1) {
    return("")
  }
  return(rs[[1]]$symbol)
}

yahoo.symbol.industry.syms <- function(sym) {
  sym.details <- yahoo.symbol.details(sym)
  ind <- yahoo.industries()
  ind.id <- ind[ind$Name == sym.details['Industry'],]$ID
  ind.comps <- yahoo.industry.companies(ind.id)
  syms <- sapply(ind.comps$Description[2:nrow(ind.comps)], 
                 yahoo.symbol.for.company)
  return(syms[syms != "" & syms != sym])
}
