discuri_clasamente <- function(url) {
  
  # https://en.wikipedia.org/wiki/Iron_Maiden_discography
  
  wikipedia_url <- url
  wikipedia_raw_data <- xml2::read_html(wikipedia_url) 
  
  discuri_clasamente <- wikipedia_raw_data %>%
    rvest::html_nodes("table") %>%
    .[[2]] %>%
    rvest::html_table(., fill = TRUE, header = TRUE)
  
  # Change the name of the columns (some arguments have the same name and this creates errors)
  names(discuri_clasamente) <- c("titlu_disc", "detalii_album", "UK", "AUT", "CAN", "FIN", "GER",
                                 "ITA", "JPN", "NZ", "NOR", "SWE", "SWI", "US", "GR", "certificari")
  
  discuri_clasamente <- discuri_clasamente %>%
    mutate(row_number = row_number()) %>%
    filter(row_number >= 2 & row_number <= 17) %>%
    select(-detalii_album, - certificari, - row_number)
  
  # change the type of some columns into numeric
  discuri_clasamente[2:14] <- sapply(discuri_clasamente[2:14], as.numeric)
  
  # add a column which contains the min. of each line
  discuri_clasamente$cea_mai_buna_clasare <- NA
  
  for(i in 1:nrow(discuri_clasamente)) {
    min = min(discuri_clasamente[i, 2:14], na.rm = T)
    discuri_clasamente$cea_mai_buna_clasare[i] <- min
  }
  
  # add cod_tara column (cod_tara column will contain the name of the countries with best positioning
  # for each line)
  discuri_clasamente$cod_tara <- NA
  
  for(i in 1:nrow(discuri_clasamente)) {
    for(j in 2:ncol(discuri_clasamente[2:14])) {
      if(!is.na(discuri_clasamente[i,j])) {
        if(discuri_clasamente[i,j] == discuri_clasamente$cea_mai_buna_clasare[i]) {
          discuri_clasamente$cod_tara[i] <- names(discuri_clasamente)[j]
        }
      }
    }
  }
  
  # select necessary columns
  discuri_clasamente <- discuri_clasamente %>%
    select(titlu_disc, cod_tara, cea_mai_buna_clasare) %>%
    mutate(comentarii = "-")
  
  return(discuri_clasamente)
}