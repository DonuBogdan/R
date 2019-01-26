alcatuire_disc <- function(specChar) {
  
  wikipedia_url <- "https://en.wikipedia.org/wiki/Iron_Maiden_(album)"
  content <- xml2::read_html(wikipedia_url) 
  
  #########
  # ID_DISC
  
  # we have 8 songs on this album
  id_disc <- c(rep(1, 8))
  
  ########################################
  # TITLU_PIESA, COMPOZITORI AND NR_ORDINE
  
  tableAD1 <- content %>%
    rvest::html_nodes("table") %>%
    .[[4]] %>%
    rvest::html_table(., fill = TRUE, header = FALSE) %>%
    select(X2, X3) %>%
    slice(3:6)
  
  tableAD2 <- content %>%
    rvest::html_nodes("table") %>%
    .[[5]] %>%
    rvest::html_table(., fill = TRUE, header = FALSE) %>%
    select(X2, X3) %>%
    slice(3:6)
  
  # titlu_piesa + compozitori + nr_ordine
  tableAD <- tableAD1 %>%
    bind_rows(tableAD2) %>%
    mutate(nr_ordine = row_number())
  
  # change the name of the columns
  names(tableAD) <- c("titlu_piesa", "compozitori", "nr_ordine")
  
  # delete the quotes from "titlu_piesa" column
  tableAD$titlu_piesa <- gsub("\"", "", tableAD$titlu_piesa)
  
  # delete "(Instrumental)"
  tableAD$titlu_piesa[5] <- "Transylvania"
  
  # all songs are written and composed by Steven Harris, except where noted.
  tableAD$compozitori <- ifelse(tableAD$compozitori == "", "Steve Harris", tableAD$compozitori)
  
  # small change
  tableAD$compozitori[2] <- "Steve Harris, Paul Di'Anno"
  
  # small change
  tableAD$compozitori[3] <- "Steve Harris, Paul Di'Anno"
  
  # View(tableAD)
  
  ########################################################
  # FATA_DISC(side one means "Da" and side two means "Nu")
  
  fata_disc <- c("Da", "Da", "Da", "Da", "Nu", "Nu", "Nu", "Nu")
  
  ###########
  # VOCALISTI
  
  tableAD3 <- content %>%
    rvest::html_nodes("ul>li") %>%
    rvest::html_text() %>%
    grep("vocals", ., value = TRUE) %>%
    tibble()
  
  names(tableAD3) <- "vocalisti"
  
  # now we have all the vocalists
  tableAD3 <- tableAD3 %>%
    separate(col = vocalisti, into = c("vocalisti", "y"), sep = specChar) %>%
    select(-y) %>%
    spread(key = vocalisti, value = vocalisti) %>%
    # some strange points...
    unite("vocalisti", c(1, 2, 3), sep = ", ")
  
  # replicate 8 times the row obtained above
  tableAD3 <- data.frame(vocalisti = rep(tableAD3$vocalisti, 8))
  
  # View(tableAD3)
  
  ############
  # DURATA_SEC
  
  durata_sec <- content %>%
    rvest::html_nodes("table>tbody>tr>td") %>%
    rvest::html_text() %>%
    # search after the time pattern 
    grep("\\d:\\d", ., value = TRUE) %>%
    # delete quotes
    str_replace_all('"', "") %>%
    tibble() %>%
    mutate(row_number = row_number()) %>%
    # filter data 
    filter(row_number >= 2 & row_number <= 9) %>%
    select(-row_number)
  
  names(durata_sec)[1] <- "durata_sec"
  
  # Transform minutes into seconds
  durata_sec$durata_sec <- lubridate::period_to_seconds(lubridate::ms(durata_sec$durata_sec))
  
  # View(durata_sec)
  
  ###############################################
  #### Merge all tibbles / vectors obtained above
  ###############################################
  
  # firstly, merge the vectors (+ create a common column in order to make merge possible)
  idDisc_fataDisc <- tibble(id_disc = id_disc, fata_disc = fata_disc, colComuna = c(1:8))
  
  # merge tibbles
  
  tableAD <- tableAD %>%
    mutate(colComuna = c(1:8))
  
  tableAD3 <- tableAD3 %>%
    mutate(colComuna = c(1:8))
  
  durata_sec <- durata_sec %>%
    mutate(colComuna = c(1:8))
  
  alcatuire_disc <- tableAD %>%
    inner_join(tableAD3, by = c("colComuna" = "colComuna")) %>%
    inner_join(durata_sec, by = c("colComuna" = "colComuna")) %>%
    inner_join(idDisc_fataDisc, by = c("colComuna" = "colComuna")) %>%
    select(id_disc, nr_ordine, fata_disc, titlu_piesa, compozitori, vocalisti, durata_sec)

  return(alcatuire_disc)
}