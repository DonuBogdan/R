vanzari_totale_tari <- function(url){
  
  #  CREATE TABLE VANZARI_TOTALE_TARI (	
  #    COD_TARA VARCHAR(10), 
  #    NUME_TARA VARCHAR(255), 
  #    NUMAR_VANZARI_ALBUME NUMERIC(20)
  #  ) ;
  
  # https://www.ukmix.org/forums/viewtopic.php?t=39069
  
  content_url <- url
  content <- xml2::read_html(content_url) 
  
  nume_tara <- content %>%
    rvest::html_nodes("span>strong") %>%
    rvest::html_text() %>%
    data.frame()
  
  # small filter on countries 
  nume_tara <- data.frame(nume_tara = nume_tara[c(2,3,4,5,7,8,9,12),])
  
  # add labels for each country ("US", "UK", "CAN","GER","FIN","SWE","AUT","JPN") 
  nume_tara_and_cod_tara <- nume_tara %>%
    mutate(cod_tara = case_when(nume_tara == 'United Kingdom' ~ 'UK',
                                 nume_tara == 'Austria ' ~ 'AUT',
                                 nume_tara == 'Canada' ~ 'CAN',
                                 nume_tara == 'Finland' ~ 'FIN',
                                 nume_tara == 'Germany' ~ 'GER',
                                 nume_tara == 'Japan' ~ 'JPN',
                                 nume_tara == 'Sweden ' ~ 'SWE',
                                 nume_tara == 'United States' ~ 'US')) %>%
    mutate(row_number = row_number()) # for merging


  numar_vanzari_albume <- content %>%
    rvest::html_nodes("strong") %>%
    rvest::html_text() %>%
    grep("\\d,\\d", ., value = TRUE) %>%
    data.frame()
  
  # small filter on total shipments
  numar_vanzari_albume <- data.frame(numar_vanzari_albume = numar_vanzari_albume[c(1,2,3,4,6,7,8,15),])
  
  numar_vanzari_albume <- numar_vanzari_albume %>%
    mutate(row_number = row_number()) # for merging
  
  # build final dataframe
  vanzari_totale_tari <- nume_tara_and_cod_tara %>%
    inner_join(numar_vanzari_albume, by = c("row_number" = "row_number")) %>%
    select(cod_tara, nume_tara, numar_vanzari_albume)
  
  # change some data types
  vanzari_totale_tari$nume_tara <- as.character(vanzari_totale_tari$nume_tara)
  vanzari_totale_tari$numar_vanzari_albume <- as.numeric(gsub("," ,"", numar_vanzari_albume$numar_vanzari_albume))
  
  return(vanzari_totale_tari)
}