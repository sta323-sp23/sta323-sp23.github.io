##########
# Demo 1 #
##########

library(tidyverse)

houses = jsonlite::read_json(
"https://www.anapioficeandfire.com/api/houses?pageSize=50&page=2"
) %>%
  tibble(house = .) %>%
  unnest_wider(house)

houses %>%
  view()

##
# pagination
##

res = list()
page = 1
pageSize = 50

repeat {
  
  cat("Reading page ", page, "\n")
  
  d = jsonlite::read_json(glue::glue(
    "https://www.anapioficeandfire.com/api/houses?pageSize=50&page={page}"
  ))
  
  res = c(res, d)
  
  if(length(d) != pageSize) {
    break
  }

  
  page = page + 1
}

################
#### Demo 2 ####
################

library(httr2)
# more advanced package to interact with http addresses 
# to get more information
# ex:
# better query interface
# get status codes

resp = request("https://www.anapioficeandfire.com/api/houses") %>%
  req_url_query(
    pageSize = 50,
    page = 5
  ) %>%
  # req_dry_run()
  req_perform()

resp %>%
  resp_status()

resp %>%
  resp_body_json()

resp %>%
  resp_headers("link")


# write a helper function to do the processing of figuring out
# next pg URL
get_links = function(resp) {
resp %>%
  resp_headers("link") %>%
  str_match_all("<(.*?)>; rel=\"(.*?)\"") %>%
  .[[1]] %>%
    {setNames(as.list(.[,2]), .[,3])}
}

# now I will automate this using the links machinery we just setup

# get first page

resp = request("https://www.anapioficeandfire.com/api/houses") %>%
  req_url_query(
    pageSize = 50,
    page = 1
  ) %>%
  req_perform()

# get links from first page
links = get_links(resp)

# save the result

result = resp %>%
  resp_body_json()

# loop until no more "next" text

repeat {
  if(is.null(links[["next"]])) {
    break
  }
  
  resp = request(links[["next"]]) %>%
    req_perform()
  
  links = get_links(resp)
  
  result = c(result, resp %>% resp_body_json())
  
}

# This is way more convoluted.
# Punchline: the information about how many pages to request is often 
# *somewhere* 
# often useful to glean this information for pagination purposes 
# because there's a rate limit


####resp = request("https://www.anapioficeandfire.com/api/houses") %>%
req_url_query(
  pageSize = 50,
  page = 5
) %>%
  # req_dry_run()
  req_perform()

##################
### EXERCISE 1 ###
##################


res = list()
page = 1
pageSize = 50

repeat {
  
  cat("Reading page ", page, "\n")
  
  d = jsonlite::read_json(glue::glue(
    "https://www.anapioficeandfire.com/api/characters?pageSize=50&page={page}"
  ))
  
  res = c(res, d)
  
  if(length(d) != pageSize) {
    break
  }
  
  
  page = page + 1
}


# there are 2134 characters

## EXERCISE 2 ## 

char = res %>%
  tibble(char = .) %>%
  unnest_wider(char)

char %>%
  nrow()

char %>%
  summarize(prop_alive = sum(died == "") / nrow(char))
