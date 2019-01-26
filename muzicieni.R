muzicieni <- function() {
  
  wikipedia_url <- "https://en.wikipedia.org/wiki/Iron_Maiden_discography"
  wikipedia_raw_data <- xml2::read_html(wikipedia_url) 
  
  id_persoana <- c(1,2,3,4,5)
  
  nume_prenume_scena_lista <- wikipedia_raw_data %>%
    # Take all paragraphs
    rvest::html_nodes('p') %>%
    # Take all a tags
    rvest::html_nodes('a') %>%
    rvest::html_text() %>%
    str_extract_all("\\b[A-Z]\\w+")
  
  # Remove empty elements from list
  nume_prenume_scena_lista <- nume_prenume_scena_lista[sapply(nume_prenume_scena_lista,length)>0]
  
  # Taking our musicians
  nume_prenume_scena_lista <- nume_prenume_scena_lista[c(2,4,5,6,7)]
  length(nume_prenume_scena_lista) # 5
  # Take each musician first name and last name and eliminate quotes
  prenume_1 <- noquote(nume_prenume_scena_lista[[1]][1])
  nume_2 <- noquote(nume_prenume_scena_lista[[1]][2])
  
  prenume_3 <- noquote(nume_prenume_scena_lista[[2]][1])
  nume_4.0 <- noquote(nume_prenume_scena_lista[[2]][2])
  # We have a musician that has 2 last names
  nume_4.1 <- noquote(nume_prenume_scena_lista[[2]][3])
  # Concatanate it
  nume_4 <- paste(nume_4.0, nume_4.1)
  
  prenume_5 <- noquote(nume_prenume_scena_lista[[3]][1]) 
  nume_6 <- noquote(nume_prenume_scena_lista[[3]][2])
  
  prenume_7 <- noquote(nume_prenume_scena_lista[[4]][1])
  nume_8 <- noquote(nume_prenume_scena_lista[[4]][2])
  
  prenume_9 <- noquote(nume_prenume_scena_lista[[5]][1])
  nume_10 <- noquote(nume_prenume_scena_lista[[5]][2]) 
  
  # Create vectors with nume_scena and prenume_scena
  nume_scena <- c(nume_2,nume_4, nume_6, nume_8, nume_10)
  
  prenume_scena <- c(prenume_1, prenume_3, prenume_5, prenume_7, prenume_9)
  
  #####################################
  # Take the real name of each musician
  
  # Steve Harris (https://en.wikipedia.org/wiki/Steve_Harris_(musician))
  
  wikipedia_url1 <- 'https://en.wikipedia.org/wiki/Steve_Harris_(musician)'
  wikipedia_raw_data1 <- xml2::read_html(wikipedia_url1) 
  
  nume_prenume_real1 <- wikipedia_raw_data1 %>%
    # Take the words between b tags
    rvest::html_nodes('b') %>%
    html_text()
  
  nume_prenume_real1 <- nume_prenume_real1[1]
  split_nume_prenume_1 <- str_split(nume_prenume_real1, " ")
  nume_real1 <- split_nume_prenume_1[[1]][3]
  prenume_real1.0 <- split_nume_prenume_1[[1]][1]
  prenume_real1.1 <- split_nume_prenume_1[[1]][2]
  prenume_real1 <- paste(prenume_real1.0, prenume_real1.1)
  
  
  # Di Anno Paul (https://en.wikipedia.org/wiki/Paul_Di%27Anno)
  
  wikipedia_url2 <- 'https://en.wikipedia.org/wiki/Paul_Di%27Anno'
  wikipedia_raw_data2 <- xml2::read_html(wikipedia_url2) 
  
  nume_prenume_real2 <- wikipedia_raw_data2 %>%
    # Take the words between b tags
    rvest::html_nodes('b') %>%
    html_text()
  
  nume_prenume_real2 <- nume_prenume_real2[3]
  split_nume_prenume_2 <- str_split(nume_prenume_real2, " ")
  nume_real2 <- split_nume_prenume_2[[1]][2]
  prenume_real2 <- split_nume_prenume_2[[1]][1]
  
  # Murray Dave (https://en.wikipedia.org/wiki/Dave_Murray_(musician))
  
  wikipedia_url3 <- 'https://en.wikipedia.org/wiki/Dave_Murray_(musician)'
  wikipedia_raw_data3 <- xml2::read_html(wikipedia_url3) 
  
  nume_prenume_real3 <- wikipedia_raw_data3 %>%
    # Take the words between b tags
    rvest::html_nodes('b') %>%
    html_text()
  
  nume_prenume_real3 <- nume_prenume_real3[1]
  split_nume_prenume_3 <- str_split(nume_prenume_real3, " ")
  nume_real3 <- split_nume_prenume_3[[1]][2]
  prenume_real3 <- split_nume_prenume_3[[1]][1]
  
  # Stratton Dennis (https://en.wikipedia.org/wiki/Dennis_Stratton)
  
  wikipedia_url4 <- 'https://en.wikipedia.org/wiki/Dennis_Stratton'
  wikipedia_raw_data4 <- xml2::read_html(wikipedia_url4) 
  
  nume_prenume_real4 <- wikipedia_raw_data4 %>%
    # Take the words between b tags
    rvest::html_nodes('b') %>%
    html_text()
  
  nume_prenume_real4 <- nume_prenume_real4[3]
  split_nume_prenume_4 <- str_split(nume_prenume_real4, " ")
  nume_real4 <- split_nume_prenume_4[[1]][3]
  prenume_real4.0 <- split_nume_prenume_4[[1]][1]
  prenume_real4.1 <- split_nume_prenume_4[[1]][2]
  prenume_real4 <- paste(prenume_real4.0, prenume_real4.1)
  
  # Burr Clive (https://en.wikipedia.org/wiki/Clive_Burr)
  
  wikipedia_url5 <- 'https://en.wikipedia.org/wiki/Clive_Burr'
  wikipedia_raw_data5 <- xml2::read_html(wikipedia_url5) 
  
  nume_prenume_real5 <- wikipedia_raw_data5 %>%
    # Take the words between b tags
    rvest::html_nodes('b') %>%
    html_text()
  
  nume_prenume_real5 <- nume_prenume_real5[1]
  split_nume_prenume_5 <- str_split(nume_prenume_real5, " ")
  nume_real5 <- split_nume_prenume_5[[1]][2]
  prenume_real5 <- split_nume_prenume_5[[1]][1]
  
  
  # Create vectors with nume_real and prenume_real
  nume_real <- c(nume_real1, nume_real2, nume_real3, nume_real4, nume_real5)
  
  prenume_real <- c(prenume_real1, prenume_real2, prenume_real3, prenume_real4, prenume_real5)
  
  # data_nastere
  
  # 1.
  
  data_nastere_lista_1 <- wikipedia_raw_data1 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td>span') %>%
    html_text()
  
  # Some small modifications
  data_nastere_1 <- str_trim(data_nastere_lista_1[[1]])
  data_nastere_1 <- str_replace(data_nastere_1, "\\(", "")
  data_nastere_1 <- ymd(str_replace(data_nastere_1, "\\)", ""))
  
  # 2.
  
  data_nastere_lista_2 <- wikipedia_raw_data2 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td>span') %>%
    html_text()
  
  # Some small modifications
  data_nastere_2 <- str_trim(data_nastere_lista_2[[1]])
  data_nastere_2 <- str_replace(data_nastere_2, "\\(", "")
  data_nastere_2 <- ymd(str_replace(data_nastere_2, "\\)", ""))
  
  # 3.
  
  data_nastere_lista_3 <- wikipedia_raw_data3 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td>span') %>%
    html_text()
  
  # Some small modifications
  data_nastere_3 <- str_trim(data_nastere_lista_3[[1]])
  data_nastere_3 <- str_replace(data_nastere_3, "\\(", "")
  data_nastere_3 <- ymd(str_replace(data_nastere_3, "\\)", ""))
  
  # 4.
  
  data_nastere_lista_4 <- wikipedia_raw_data4 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td>span') %>%
    html_text()
  
  # Some small modifications
  data_nastere_4 <- str_trim(data_nastere_lista_4[[1]])
  data_nastere_4 <- str_replace(data_nastere_4, "\\(", "")
  data_nastere_4 <- ymd(str_replace(data_nastere_4, "\\)", ""))
  
  # 5.
  
  data_nastere_lista_5 <- wikipedia_raw_data5 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td>span') %>%
    html_text()
  
  # Some small modifications
  data_nastere_5 <- str_trim(data_nastere_lista_5[[1]])
  data_nastere_5 <- str_replace(data_nastere_5, "\\(", "")
  data_nastere_5 <- ymd(str_replace(data_nastere_5, "\\)", ""))
  
  # Create vector with born dates
  data_nastere <- c(data_nastere_1, data_nastere_2, data_nastere_3, data_nastere_4,
                    data_nastere_5)
  
  
  # data_deces
  
  ## Only Clive Burr died 
  
  data_deces_lista_5 <- wikipedia_raw_data5 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td>span') %>%
    html_text()
  
  # Some small modifications
  data_deces_5 <- str_trim(data_deces_lista_5[[2]])
  data_deces_5 <- str_replace(data_deces_5, "\\(", "")
  data_deces_5 <- ymd(str_replace(data_deces_5, "\\)", ""))
  
  data_deces = structure(rep(NA_real_, 5 ), class="Date")
  
  data_deces[5] <- data_deces_5
  
  # Cod tara
  
  # 1.
  
  tara_nastere_1 <- wikipedia_raw_data1 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td') %>%
    html_text() %>%
    # Take the third element (here is the country)
    .[[3]] %>%
    # Take only the last 7 characters
    str_sub(start = -7)
  
  # 2.
  
  tara_nastere_2 <- wikipedia_raw_data2 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td') %>%
    html_text() %>%
    # Take the third element (here is the country)
    .[[5]] %>%
    # Take only the last 7 characters
    str_sub(start = -7)
  
  # 3.
  
  tara_nastere_3 <- wikipedia_raw_data3 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td') %>%
    html_text() %>%
    # Take the third element (here is the country)
    .[[3]] %>%
    # Take only the last 7 characters
    str_sub(start = -7)
  
  # 4.
  
  tara_nastere_4 <- wikipedia_raw_data4 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td') %>%
    html_text() %>%
    # Take the third element (here is the country)
    .[[5]] %>%
    # Take only the last 7 characters
    str_sub(start = -7)
  
  # 5.
  
  tara_nastere_5 <- wikipedia_raw_data5 %>%
    # td>span means that td is the parent of span in the DOM tree
    rvest::html_nodes('td') %>%
    html_text() %>%
    # Take the third element (here is the country)
    .[[3]] %>%
    # Take only the last 7 characters
    str_sub(start = -7)
  
  # Vector with all countries
  vCountries <- c(tara_nastere_1, tara_nastere_2, tara_nastere_3, tara_nastere_4,
                  tara_nastere_5)
  
  # cod_tara = (UK) for England
  cod_tari_nastere <- str_replace(vCountries, "England", "UK")
  
  muzicieni <- data.frame(id_persoana = id_persoana, prenume_scena = prenume_scena,
                          nume_scena = nume_scena, prenume_real = prenume_real, 
                          nume_real = nume_real, data_nastere = data_nastere,
                          data_deces = data_deces, cod_tara = cod_tari_nastere)
  
  return(muzicieni)
}