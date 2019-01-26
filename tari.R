tari <- function(url) { 
  
  # https://en.wikipedia.org/wiki/Iron_Maiden_discography
  
  wikipedia_url <- url 
  wikipedia_raw_data <- xml2::read_html(wikipedia_url) 
  
  cod_tara <- wikipedia_raw_data %>%
    rvest::html_nodes("table") %>%
    .[[2]] %>%
    rvest::html_table(., fill = TRUE, header = TRUE)
  
  # Change the name of the columns (some arguments have the same name and this creates errors)
  names(cod_tara) <- c("titlu_disc", "detalii_album", "UK", "AUT", "CAN", "FIN", "GER",
                       "ITA", "JPN", "NZ", "NOR", "SWE", "SWI", "US", "GR", "certificari")
  
  # delete unnecessary columns
  cod_tara <- cod_tara %>%
    select(-titlu_disc, -detalii_album, - certificari) %>%
    gather(cod_tara, values = 1:13) %>%
    select(-value) %>%
    distinct()
  
  tari <- cod_tara %>%
    mutate(nume_tara = case_when(cod_tara == 'UK' ~ 'England',
                                 cod_tara == 'AUT' ~ 'Austria',
                                 cod_tara == 'CAN' ~ 'Canada',
                                 cod_tara == 'FIN' ~ 'Finland',
                                 cod_tara == 'GER' ~ 'Germany',
                                 cod_tara == 'ITA' ~ 'Italy',
                                 cod_tara == 'JPN' ~ 'Japan',
                                 cod_tara == 'NZ' ~ 'New Zealand',
                                 cod_tara == 'NOR' ~ 'Norway',
                                 cod_tara == 'SWE' ~ 'Sweden',
                                 cod_tara == 'SWI' ~ 'Switzerland',
                                 cod_tara == 'US' ~ 'United States',
                                 cod_tara == 'GR' ~ 'Greece')) %>%
    mutate(regiune = case_when(cod_tara == 'UK' ~ 'Nort West Europe',
                               cod_tara == 'AUT' ~ 'Central Europe',
                               cod_tara == 'CAN' ~ 'North America',
                               cod_tara == 'FIN' ~ 'North Europe',
                               cod_tara == 'GER' ~ 'Central Europe',
                               cod_tara == 'ITA' ~ 'Central Europe',
                               cod_tara == 'JPN' ~ 'East Asia',
                               cod_tara == 'NZ' ~ 'South East Oceania',
                               cod_tara == 'NOR' ~ 'North Europe',
                               cod_tara == 'SWE' ~ 'North Europe',
                               cod_tara == 'SWI' ~ 'Central Europe',
                               cod_tara == 'US' ~ 'Central America',
                               cod_tara == 'GR' ~ 'South East Europe')) %>%
    mutate(grup_venit = rep("-", 13))
  
  return(tari)
}






