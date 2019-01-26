discografie <- function() {
  
  # https://en.wikipedia.org/wiki/Iron_Maiden_discography
  
  wikipedia_url <- "https://en.wikipedia.org/wiki/Iron_Maiden_discography"
  wikipedia_raw_data <- xml2::read_html(wikipedia_url)  
  
  # titlu_disc
  
  # Studio Albums (first 16 are studio albums)
  
  titles_studio_albums <- wikipedia_raw_data %>%
    rvest::html_nodes("th>i>a") %>%
    rvest::html_text() %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    filter(row_number <= 16) %>%
    select(-row_number)
  
  # Live Albums (from 17 to 28 are live albums)
  
  titles_live_albums <- wikipedia_raw_data %>%
    rvest::html_nodes("th>i>a") %>%
    rvest::html_text() %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    filter(row_number >= 17 & row_number <= 28) %>%
    select(-row_number)
  
  # from 28 to 35 are compilation albums
  
  titlu_disc <- wikipedia_raw_data %>%
    rvest::html_nodes("th>i>a") %>%
    rvest::html_text() %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    filter(row_number >= 1 & row_number <= 35) %>%
    select(-row_number)
  
  names(titlu_disc) = "titlu_disc"
  
  #########################
  # id disc and id formatie
  
  id_disc <- c(1:nrow(titlu_disc))
  
  id_formatie <- c(rep(1,nrow(titlu_disc)))
  
  #
  # studio_sau_live column
  
  # first 16 are studio albums , and others are live albums,
  # for compilation albums we will put NA
  
  studio_sau_live <- c(rep("S", 16), rep("L", 12), rep(NA, 7))
  
  #
  # an_lansare (numeric:4), data_lansare (date) columns
  
  # studio + live + compilation albums release dates (keep only first 35 rows)
  data_lansare_albume <- wikipedia_raw_data %>%
    rvest::html_nodes("tr>td>ul>li") %>%
    rvest::html_text() %>%
    grep("Released", ., value = TRUE, fixed = TRUE) %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    filter(row_number >=1 & row_number <= 35) %>%
    select(-row_number)
  
  names(data_lansare_albume) <- "data_lansare"
  
  data_lansare_albume <- data_lansare_albume %>%
    # This regular expression matches the beginning of the string (^), 
    # any character (.) repeated zero or more times (*), and underscore :. The ? makes the match 
    # "lazy" so that it only matches are far as the first :.
    mutate(data_lansare = str_remove_all(data_lansare, "^.*?:"))
  
  # View(data_lansare_albume)
  
  # remove [] from the end of some dates
  data_lansare_albume$data_lansare[3] <- substr(data_lansare_albume$data_lansare[3],1,nchar(data_lansare_albume$data_lansare[3])-7)
  data_lansare_albume$data_lansare[15] <- substr(data_lansare_albume$data_lansare[15],1,nchar(data_lansare_albume$data_lansare[15])-4)
  
  data_lansare_albume <- dmy(data_lansare_albume$data_lansare)
  
  ##############
  ### an_lansare
  ##############
  
  an_lansare <- as.numeric(substring(data_lansare_albume,1,4))
  
  ################
  ### casa_discuri
  ################
  
  casa_discuri <- wikipedia_raw_data %>%
    rvest::html_nodes("td>ul>li") %>%
    rvest::html_text() %>%
    grep("Label", ., value = TRUE, fixed = TRUE) %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    filter(row_number >=1 & row_number <= 35) %>%
    select(-row_number)
  
  names(casa_discuri) <- "casa_discuri"
  # remove spaces
  casa_discuri$casa_discuri <- str_trim(casa_discuri$casa_discuri)
  
  # take only casa_discuri (delete label and other junks)
  casa_discuri <- casa_discuri %>%
    separate(col = casa_discuri, into = c("label" ,"casa_discuri", "junks"), sep = " ") %>%
    select(-label, -junks)
  
  ###########################
  ### album_single_compilatie
  ###########################
  
  # first 28 are simple albums (studio and live), and other 7 are compilation albums
  album_single_compilatie <- c(rep("A", 28), rep("C", 7))
  
  ##################################################
  ## build the final tibble of the discography table
  
  # our columns:
  
  # titlu_disc
  # id_formatie
  # id_disc
  # studio_sau_live
  # data_lansare_albume
  # an_lansare
  # casa_discuri
  # album_single_compilatie
  
  discografie <- data.frame(id_disc = id_disc, titlu_disc = titlu_disc, id_formatie = id_formatie
                            , album_single_compilatie = album_single_compilatie, studio_sau_live =
                              studio_sau_live, an_lansare = an_lansare, data_lansare = data_lansare_albume
                            , casa_discuri = casa_discuri)
  
  # View(discografie)
  
  ###############
  ###############
  ###############
  ##  SINGLES  ##
  
  singles <- wikipedia_raw_data %>%
    rvest::html_nodes("table") %>%
    .[[5]] %>%
    rvest::html_table(., fill=TRUE ,header=TRUE)
  
  # changing the names of the arguments (some arguments have the same name)
  names(singles) <- c("titlu_disc", "an_lansare", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"
                      , "12", "13", "14", "15", "16")
  
  singles <- singles %>%
    select(titlu_disc, an_lansare) %>%
    # delete first and last line
    mutate(row_number = row_number()) %>%
    filter(row_number >= 2 & row_number <=44) %>%
    select(titlu_disc, an_lansare)
  
  ## Complete data set with missing columns (fill only the ones that contain known values)
  
  singles$id_disc <- c(36:(nrow(singles)+35))
  singles$id_formatie <- c(rep(1, nrow(singles)))
  singles$album_single_compilatie <- "S"
  singles$studio_sau_live <- NA
  singles$data_lansare <- NA
  singles$casa_discuri <- NA
  
  # Respect the order of the previous df (of the columns)
  singles <- singles %>%
    select(id_disc, titlu_disc, id_formatie, album_single_compilatie, 
           studio_sau_live, an_lansare, data_lansare, casa_discuri)
  
  # View(singles)
  
  #######################################################################################
  ##### MERGE discografie dataframe with singles dataframe and create the final dataframe
  #######################################################################################
  
  discografie <- rbind(discografie, singles)
  
  # Change the data types where needed
  # glimpse(discografie)
  discografie$an_lansare <- as.numeric(discografie$an_lansare)
  
  discografie <- discografie %>%
    arrange(an_lansare) %>%
    mutate(ordine_an = NA)
  
  #### adding ordine_an column
  
  j <- 1
  discografie$ordine_an[1] <- 1
  for(i in 2:nrow(discografie)) {
    if(discografie$an_lansare[i] != discografie$an_lansare[i-1]) {
      j = j + 1 
      discografie$ordine_an[i] = j
    } else {
      discografie$ordine_an[i] = j
    }
  }
  
  # swap casa_discuri with ordine_an
  discografie <- discografie[, c(1,2,3,4,5,6,7,9,8)]
  
  return(discografie)
  
}