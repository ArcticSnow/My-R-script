getNews2 <- function(symbol, number){
  
  # load libraries
  require(XML); require(plyr); require(stringr); require(lubridate);  
  
  # construct url to news feed rss and encode it correctly
  url.b1 = 'http://www.google.com/finance/company_news?q='
  url    = paste(url.b1, symbol, '&output=rss', "&start=", 1,
                 "&num=", number, sep = '')
  url    = URLencode(url)
  
  # parse xml tree, get item nodes, extract data and return data frame
  doc   = xmlTreeParse(url, useInternalNodes = T);
  nodes = getNodeSet(doc, "//item");
  mydf  = ldply(nodes, as.data.frame(xmlToList))
  
  # clean up names of data frame
  names(mydf) = str_replace_all(names(mydf), "value\\.", "")
  
  # convert pubDate to date-time object and convert time zone
  mydf$pubDate = strptime(mydf$pubDate, 
                          format = '%a, %d %b %Y %H:%M:%S', tz = 'GMT')
  mydf$pubDate = with_tz(mydf$pubDate, tz = 'America/New_york')
  
  # drop guid.text and guid..attrs
  mydf$guid.text = mydf$guid..attrs = NULL
  
  return(mydf)    
}