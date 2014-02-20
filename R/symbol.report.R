#!/usr/bin/env Rscript
# (c) Copyright 2014 mkfs <https://github.com/mkfs>
# R functions to generate a report for a symbol

library(knitr)
library(markdown)

# Generate path for report template, relative to this file
symbol.report.template.dir <- paste(dirname(dirname(parent.frame(2)$ofile)),
                                    'report_templates', sep='/')
symbol.report.src.dir <- dirname(parent.frame(2)$ofile)
symbol.report.template <- paste(symbol.report.template.dir,
                                'Ticker-Report.Rmd', sep='/')


# Use KnitR to generate a Markdown file from the template.
# Convert to HTML or PDF as directed. Returns path to generated file.
# Note that output.basename is the entire output path except extension.
symbol.report <- function(sym, output.basename=NULL, format='html') {
  # prepare output filenames
  if (is.null(output.basename)) {
    output.basename <- paste('SymbolReport', sym, sep='-')
  }
  output.md <- paste(output.basename, 'md', sep='.')
  output.html <- paste(output.basename, 'html', sep='.')
  output.pdf <- paste(output.basename, 'pdf', sep='.')
  
  # prepare environment for KnitR with 'ticker.symbol' defined
  rpt.env <- new.env() 
  assign('ticker.symbol', sym, envir=rpt.env)
  # source additional files
  sys.source( paste(symbol.report.src.dir,
                    'yahoo.finance.R', sep='/'), envir=rpt.env)
  
  # Generate the Markdown
  knit( symbol.report.template, output.md, envir=rpt.env)
  rv = output.md
  
  # HTML output
  if (format == "html") {
    markdownToHTML(output.md, output.html)
    rv = output.html
  } else if (format == 'pdf') {
    # TODO: html-to-pdf
    rv = output.pdf
  }
  return(rv)
}

# View a report in the RStudio viewer pane or a local browser
symbol.report.view <- function(sym) {
  # HTML file must be in R session tempdir for Viewer to display
  base.path <- paste(tempdir(), paste(sym, 'report', sep='-'), sep='/')
  
  # Generate report
  html.path <- symbol.report(sym, output=base.path)
  
  # Run viewer
  viewer <- getOption("viewer")
  if (exists('viewer', asNamespace('rstudio'))) {
    viewer <- rstudio::viewer
  } 
  if (is.null(viewer)) {
    viewer <- utils::browseURL
  }
  viewer(html.path)
}