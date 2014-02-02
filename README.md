misc-finance
============

Various scripts in R for financial analysis.

Note that these scripts are primarily used for basic tasks such as 
daily monitoring of symbols, charting trends, and research reports.

These scripts are not intended for day trading or any sort of realtime market
analysis.

	* symbol.alert.R : An R script for daily monitoring of a list of stock symbols with an email alert if they exceed specified buy/sell thresholds.
	
Examples:
	# symbol.alert.R cron job command line
	R --vanilla -e "source('/home/repo/misc-finance/R/symbol.alert.R'); symbol.alert('/home/me/symbol.alert.dat', 'me@gmail.com')" >/dev/null

