#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>
# NYSE interface.

nyse.nya.url <- 'http://www.nyse.com/indexes/nyaindex.csv'

# Returns a data frame with the symbols in the NYA index.
# The data frame has the following columns:
# NAME TICKER COUNTRY ICB INDUS SUP.SEC SEC SUB.SEC
nyse.nya <- function() {
  read.csv( nyse.nya.url, header=TRUE, skip=1, stringsAsFactors=FALSE )
}