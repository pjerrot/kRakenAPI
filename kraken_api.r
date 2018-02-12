####### KRAKEN.COM ###############

#####
# Functions to query and handle market data and personal kraken account
# Designed for myself - and others who know R and want to become crypto rich ;)
# Use of functions are entirely at own risk
# Functions were written from looking at RBitcoin package - so thank you very much to Jan Gorecki for his work. 
# I only wrote these api functions to enable trading etc. with coins not currently supported in Rbitcoin package
# Feel free to work on functions. Please do this in new branch and send me pull request when ready.
# Enjoy!
#####

library(digest)
library(RCurl)
library(caTools)
library(jsonlite)

# !! key <- api key from kraken
# !! secret <- private key from kraken

########
#ACCOUNT INFO
########


# TRADEBALANCE (VALUE OF ACCOUNT)
kraken_account_balance <- function(key, secret) {
  
  # BALANCE (ANTAL AF DE FORSKELLIGE COINS I PORTEFØLJEN)
  url <- as.character("https://api.kraken.com/0/private/TradeBalance")
  
  nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
  post_data <- paste0("nonce=", nonce)
  
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret, mode = "raw"), object = c(charToRaw(method_path), 
                              digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
  #curl <- getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE, postfields = post_data, httpheader = httpheader))
  query_result <- fromJSON(query_result_json)
  
  return(query_result)
}

# example: acc_balance <- kraken_account_balance(key, secret)$result$eb


# BALANCE (COINS BALANCE IN ACCOUNT)
kraken_coins_balance <- function(key, secret) {

  # BALANCE (ANTAL AF DE FORSKELLIGE COINS I PORTEFØLJEN)
  url <- as.character("https://api.kraken.com/0/private/Balance")
  
  nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
  post_data <- paste0("nonce=", nonce)
  
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret, mode = "raw"), object = c(charToRaw(method_path), 
                      digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
  #curl <- getCurlHandle(useragent = paste("Rbitcoin", packageVersion("Rbitcoin")))
  
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE, postfields = post_data, httpheader = httpheader))
  query_result <- fromJSON(query_result_json)
  
  return(query_result)
}

# OPEN ORDERS

kraken_get_open_orders <- function(key,secret) {
  url <- "https://api.kraken.com/0/private/OpenOrders"
  
  nonce <- as.character(as.numeric(Sys.time()) * 1e+06)
  post_data <- paste0("nonce=", nonce)
  
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret, mode = "raw"), object = c(charToRaw(method_path), 
                                                                    digest(object = paste0(nonce, post_data), algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
  curl <- getCurlHandle(useragent = "krakenuser")
  query_result_json <- rawToChar(getURLContent(curl = curl, 
                                               url = url, binary = TRUE, postfields = post_data, 
                                               httpheader = httpheader))
  query_result <- fromJSON(query_result_json)
  
  if (length(query_result$result$open)>0) {
    for (i in 1:length(query_result$result$open)){
      print(paste("Open order #",i,":",paste(names(query_result$result$open)[i],":",query_result$result$open[[i]]$descr$order)))
    }
  } else {
    print("Currently no open orders!")
  }  
  
  return(query_result)
}

##################	
# TRADING
##################

# PLACE ORDER  

#req <- list(pair="XXBTZEUR", type=c("buy","sell"),ordertype=c("market","limit"),price=9000,volume=0.006)

kraken_place_order <- function(pair, type, ordertype, price=NULL, volume, key, secret) {

	  req <- list(pair=pair, type=type,ordertype=ordertype,price=price,volume=volume)
	  url <- "https://api.kraken.com/0/private/AddOrder" 
      nonce <- as.character(as.numeric(Sys.time()) * 1000000)
      post_data <- paste0("nonce=", nonce)
      post_data <- paste(post_data, paste(paste(names(req), req, sep = "="), collapse = "&"), sep = "&")
      method_path <- gsub("^.*?kraken.com", "", url)
      sign <- hmac(key = base64Decode(secret, mode = "raw"), 
                     object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data), 
                     algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
      httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
      curl <- getCurlHandle(useragent = "krakenuser")
      query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, 
                                                   postfields = post_data, httpheader = httpheader))
      query_result <- fromJSON(query_result_json)
	  return(query_result)
}
 
# CANCEL ORDER
      
kraken_cancel_order <- function(txid, key, secret) {  #txid can be retreived from kraken_get_open_orders function
  url <- "https://api.kraken.com/0/private/CancelOrder" 
  req <- list(txid=txid)
  nonce <- as.character(as.numeric(Sys.time()) * 1000000)
  post_data <- paste0("nonce=", nonce)
  post_data <- paste(post_data, paste(paste(names(req), req, sep = "="), collapse = "&"), sep = "&")
  method_path <- gsub("^.*?kraken.com", "", url)
  sign <- hmac(key = base64Decode(secret, mode = "raw"), 
               object = c(charToRaw(method_path), digest(object = paste0(nonce, post_data), 
                                                         algo = "sha256", serialize = FALSE, raw = TRUE)), algo = "sha512", raw = TRUE)
  httpheader <- c('API-Key' = key, 'API-Sign' = base64Encode(sign))
  curl <- getCurlHandle(useragent = "krakenuser")
  query_result_json <- rawToChar(getURLContent(curl = curl, url = url, binary = TRUE, 
                                               postfields = post_data, httpheader = httpheader))
  query_result <- fromJSON(query_result_json)
  
  return(query_result)
}

#################
# PUBLIC INFO
#################

# TICKER INFO (PUBLIC)

kraken_get_ticker <- function(pair) {
  url <- "https://api.kraken.com/0/public/Ticker"
  pair <- paste(pair,collapse=",")
  req <- list(pair=pair)
  post_data <- paste(paste(paste(names(req), req, sep = "="), collapse = "&"), sep = "&")  
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE, postfields = post_data))
  query_result <- fromJSON(query_result_json)
  return(query_result)
}

#Output
#    a = ask array(<price>, <whole lot volume>, <lot volume>),
#    b = bid array(<price>, <whole lot volume>, <lot volume>),
#    c = last trade closed array(<price>, <lot volume>),
#    v = volume array(<today>, <last 24 hours>),
#    p = volume weighted average price array(<today>, <last 24 hours>),
#    t = number of trades array(<today>, <last 24 hours>),
#    l = low array(<today>, <last 24 hours>),
#    h = high array(<today>, <last 24 hours>),
#    o = today's opening price

# ex:  ticks <- kraken_get_ticker(c("XETHZEUR,XETCZEUR"))


# OHLC INFO (PUBLIC)

kraken_get_ohlc <- function(pair, interval=NULL) {
  # interval in minutes (1 (default), 5, 15, 30, 60, 240, 1440, 10080, 21600)
  url <- "https://api.kraken.com/0/public/OHLC"
  interval <- ifelse(is.null(interval),1,interval)
  req <- list(pair=pair, interval=interval)
  post_data <- paste(paste(paste(names(req), req, sep = "="), collapse = "&"), sep = "&")  
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE, postfields = post_data))
  query_result <- fromJSON(query_result_json)
  
  #last <- list(last=query_result$result$last, last_true=as.POSIXct(query_result$result$last,origin = "1970-01-01")  )
  
  ohlc_df <- data.frame(query_result$result[[1]])
  colnames(ohlc_df)[1:8] <- c("time_unix","open","high","low","close","vwap", "volume","count")
  ohlc_df$time_human <- as.POSIXct(as.numeric(as.character(ohlc_df[,1])),origin = "1970-01-01")  
  
  for (i in colnames(ohlc_df)){
    if (is.factor(ohlc_df[,i])) ohlc_df[,i] <- as.numeric(as.character(ohlc_df[,i] ))
  }
  
  outs <-list(query_result, ohlc_df)
  names(outs) <- c("orig_result", "ohlc_df") # returns raw result, ohlc dataframe and time for last trade (unix/true)
  return(outs)
}

# ORDERBOOK (PUBLIC)

kraken_get_coins <- function() {
  url <- "https://api.kraken.com/0/public/AssetPairs"
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE))
  query_result <- fromJSON(query_result_json)
  coinpairs <- names(query_result$result)
  out <- list(query_result,coinpairs)
  names(out) <- c("raw_results","coinpairs") #returns raw result and formatted orderbook dataframe
  return(out)
}

kraken_get_orderbook <- function(pair) {
  url <- "https://api.kraken.com/0/public/Depth"
  pair <- paste(pair,collapse=",")
  req <- list(pair=pair)
  post_data <- paste(paste(paste(names(req), req, sep = "="), collapse = "&"), sep = "&")  
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE, postfields = post_data))
  query_result <- fromJSON(query_result_json)
  
  orderbook_df <- rbind(data.frame(type="ask",query_result$result[[1]]$asks),
                        data.frame(type="bid",query_result$result[[1]]$bids))
  colnames(orderbook_df)[2:4] <- c("price","amount","time_unix")     
  orderbook_df$time_human <- as.POSIXct(as.numeric(as.character(orderbook_df$time_unix)),origin = "1970-01-01") 
  orderbook_df <- orderbook_df[order(orderbook_df$time_unix),]
  orderbook_df$price <- as.numeric(as.character(orderbook_df$price))
  orderbook_df$amount <- as.numeric(as.character(orderbook_df$amount))
  
  out <- list(query_result,orderbook_df)
  names(out) <- c("raw_results","orderbook_df") #returns raw result and formatted orderbook dataframe
  return(out)
}

# TRADES (PUBLIC)

kraken_get_trades <- function(pair) {
  url <- "https://api.kraken.com/0/public/Trades"
  pair <- paste(pair,collapse=",")
  req <- list(pair=pair)
  post_data <- paste(paste(paste(names(req), req, sep = "="), collapse = "&"), sep = "&")  
  query_result_json <- rawToChar(getURLContent(url = url, binary = TRUE, postfields = post_data))
  query_result <- fromJSON(query_result_json)
  
  tradedata_df <- data.frame(query_result$result[1])
  colnames(tradedata_df) <- c("price", "volume", "time_unix", "buy_sell", "market_limit", "miscellaneous")
  tradedata_df$time_human <- as.POSIXct(as.numeric(as.character(tradedata_df$time_unix)),origin = "1970-01-01") 
  tradedata_df$price <- as.numeric(as.character(tradedata_df$price))
  tradedata_df$volume <- as.numeric(as.character(tradedata_df$volume))
  
  last <- query_result$result[2]
  
  out <- list(query_result,tradedata_df,last)
  names(out) <- c("raw_results","tradedata_df","last_trade_id") #returns raw result and formatted orderbook dataframe
  return(out)
}


