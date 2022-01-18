# Load the secret (auth) file
# 3 vars are in auth file: BASE_URL, USER, PASS
source("secret/auth.R")

# Function to connect and download data
fromEtools <- function(apiUrl, dataKey = NULL) 
{
  
  # Make the url
  url <- paste(BASE_URL,apiUrl, sep="")
  # make the http request
  response <- http_request(url)
  
  total_pages <- as.numeric(unlist(response["total_pages"]))
  # extract and change data to dataframe
  df <- extract_data(response, dataKey)
  if(total_pages > 1) {
    nextPageUrl <- as.character(unlist(response['next']))
    for (nextPage in 2:total_pages) {
      response <- http_request(nextPageUrl)
      df <- extract_data(response, data_key = dataKey) %>%
        combine_datasets(., df)
      nextPageUrl <- as.character(unlist(response['next']))
    }
  }
  
  # Returns dataframe
  return(df)
  #return(params)
  
}

# make http request with httr
http_request <- function(url) 
{

  response <-  httr::GET(
    url, 
    authenticate(USER, PASS)
  )
  
  # Reading data content as text 
  response <- httr::content(response, "text") %>% 
    ## Transforming into JSON (Java Script Oriented Notation)
    jsonlite::fromJSON(., flatten = TRUE)
  return(response)
}

extract_data <- function(data, data_key)
{
  # Saving data as a R dataframe
  if(!is.null(data_key)) {
    return(as.data.frame(data[data_key]))
  } else {
    return(as.data.frame(data))
  }
}

# merge function
combine_datasets <- function(d1, d2) 
{
  d1.names <- names(d1)
  d2.names <- names(d2)
  
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  
  return(rbind(d1, d2))
}

# cleaning column names 
clean_colnames <- function(df, pattern="results.", replacement = "") 
{
  columns <- colnames(df)
  columns <- str_replace_all(columns, pattern = pattern, replacement) %>%
    trimws() %>%
    str_to_lower()
  
  colnames(df) = columns
  return(df)
}


