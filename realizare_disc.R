realizare_disc <- function(specChar) {
  
  # https://en.wikipedia.org/wiki/Iron_Maiden_(album)
  
  wikipedia_url <- "https://en.wikipedia.org/wiki/Iron_Maiden_(album)"
  content <- xml2::read_html(wikipedia_url) 
  
  ####################
  # ID_DISC AND ROLURI
  
  # we will take data from "content"
  # 5 musicians 
  roluri <- content %>%
    rvest::html_nodes("ul>li") %>%
    rvest::html_text() %>%
    grep(specChar, ., value = TRUE) %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    filter(row_number >= 1 & row_number <=5) %>%
    select(- row_number)
  
  # change the name of the column (from . to "roluri")
  names(roluri) <- "roluri"
  
  # split the columns into musician names and roles
  realizare_disc <- roluri %>% 
    separate(col = roluri , into = c("nume_muzician","roluri"), sep = specChar) %>%
    # add id_disc column (1 because these are the musicians for Iron Maiden album)
    mutate(id_disc = rep(1, 5)) %>%
    mutate(id_muzician = c(1: 5)) %>%
    select(id_disc, id_muzician, nume_muzician, roluri)
  
  return(realizare_disc)
}