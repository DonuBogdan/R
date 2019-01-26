formatii <- function(url) {

  # https://en.wikipedia.org/wiki/Iron_Maiden_discography
  
  wikipedia_url <- url 
  wikipedia_raw_data <- xml2::read_html(wikipedia_url)   
  
  # Take the name of the formation (html -> body -> div (al 3 -lea)-> h1)
  id_formatie <- 1

  den_formatie <- wikipedia_raw_data %>%
    rvest::html_node("h1") %>%
    rvest::html_text() %>%
    str_replace(" discography", "")  

  an_infiintare_lista <- wikipedia_raw_data %>%
    rvest::html_nodes("p") %>%
    html_text() %>%
    # If we don t transform it into a tibble, we will get NA's when we will apply the filter
    tibble() %>%
    # Take only needed row
    filter(str_detect(., "The discography")) %>%
    # Extract all numbers and special characters from our string
    str_extract_all("\\(?[0-9,.]+\\)?")

  # Take the first element of the list(transform it into character)
  #, take the year and also transform it into numeric
  an_infiintare <- as.numeric(an_infiintare_lista[[1]][2])

  cod_tara <- "UK"
  
  formatii <- data.frame(id_formatie = id_formatie, den_formatie = den_formatie, an_infiintare = 
                           an_infiintare, cod_tara = cod_tara)
  return(formatii)
}