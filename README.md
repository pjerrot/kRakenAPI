# **R wrapper for the crypto trading api on the kraken.com exchange**

Please note that most functions currently only work from Linux. Kraken.com apparently made some changes on the website causing functions to return errors when shooting API queries from Windows machines. I still haven't figured out why yet - or found a fix.

Wrapping around (most of) these api calls: https://www.kraken.com/help/api

Functions to query and handle market data and personal kraken account.

Designed for myself - and others who know R and want to become crypto rich ;)

Use of functions is entirely at own risk!

Functions were written from looking at RBitcoin package - so thank you very much to Jan Gorecki for his work. 
I only wrote these api functions to enable trading etc. with coins not currently supported in Rbitcoin package

Load functions:
**source("https://raw.githubusercontent.com/pjerrot/kRakenAPI/master/kraken_api.r")**

Require following packages: "digest", "RCurl", "caTools", "jsonlite".

Feel free to work on functions. Please do this in new branch and send me pull request when ready.

**Enjoy!**
