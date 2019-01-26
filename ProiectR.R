getwd()
setwd("D:/MASTER/AN_1/Obiecte/Logica BD/DM 117 - WEB SCRAPER R")

library(RPostgreSQL)
library(tidyverse)
# It loads automatically xml2 package
library(rvest)
library(stringr)
library(tibble)
library(lubridate)

# sourcing all functions
source("formatii.R")
source("tari.R")
source("muzicieni.R")
source("discografie.R")
source("alcatuire_disc.R")
source("discuri_clasamente.R")
source("realizare_disc.R")
source("vanzari_totale_tari.R")

formatii <- formatii("https://en.wikipedia.org/wiki/Iron_Maiden_discography")
tari <- tari("https://en.wikipedia.org/wiki/Iron_Maiden_discography")
muzicieni <- muzicieni()
discografie <- discografie()
alcatuire_disc <- alcatuire_disc(specChar = "-")
discuri_clasamente <- discuri_clasamente("https://en.wikipedia.org/wiki/Iron_Maiden_discography")
realizare_disc = realizare_disc(specChar = "-")
vanzari_totale_tari <- vanzari_totale_tari("https://www.ukmix.org/forums/viewtopic.php?t=39069")    


###########################
###########################
###### DATA BASE PART #####
###########################
###########################
  
# Loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# Creates a connection to the postgres database
con <- dbConnect(drv, dbname = "rock2019ro",
                 host = "localhost", port = ****,
                 user = "postgres", password = "******")

# Insert data into tari table
dbWriteTable(con, "tari",
             value = tari, append = TRUE, row.names = FALSE)


# Insert data into muzicieni table
dbWriteTable(con, "muzicieni",
             value = muzicieni, append = TRUE, row.names = FALSE)


# Insert data into formatii table
dbWriteTable(con, "formatii",
             value = formatii, append = TRUE, row.names = FALSE)

# Insert data into discografie table
dbWriteTable(con, "discografie",
             value = discografie, append = TRUE, row.names = FALSE)


# Insert data into alcatuire_disc table
dbWriteTable(con, "alcatuire_disc",
             value = alcatuire_disc, append = TRUE, row.names = FALSE)

# Change titlu_disc into id_disc 
discuri_clasamente <- discuri_clasamente %>%
  inner_join(discografie, by = c("titlu_disc" = "titlu_disc")) %>%
  select(id_disc, cod_tara, cea_mai_buna_clasare, comentarii)

# Insert data into discuri_clasamente table
dbWriteTable(con, "discuri_clasamente",
             value = discuri_clasamente, append = TRUE, row.names = FALSE)


# Insert data into vanzari_totale_tari table
dbWriteTable(con, "vanzari_totale_tari",
             value = vanzari_totale_tari, append = TRUE, row.names = FALSE)


# Insert data into realizare_disc table
dbWriteTable(con, "realizare_disc",
             value = realizare_disc, append = TRUE, row.names = FALSE)


# Close the connection
dbDisconnect(con)