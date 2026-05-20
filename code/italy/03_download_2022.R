## Download 2022 Italian general election data
## Source: ondata/elezioni-politiche-2022 (processed Eligendo data from Ministry of Interior)
## Also downloads the Cremaschi et al. map file for ISTAT code matching

dir.create("data_raw", showWarnings = FALSE)

# 2022 Camera dei Deputati results (municipality level, with ISTAT codes)
# Source: https://github.com/ondata/elezioni-politiche-2022
url_2022 <- "https://raw.githubusercontent.com/ondata/elezioni-politiche-2022/main/affluenza-risultati/dati/Eligendo/processing/Politiche2022_Scrutini_Camera_Italia.csv"
dest_2022 <- "data_raw/camera_2022_eligendo.csv"

if (!file.exists(dest_2022)) {
  download.file(url_2022, dest_2022)
  cat("Downloaded 2022 election data to", dest_2022, "\n")
} else {
  cat("2022 election data already exists:", dest_2022, "\n")
}

# Cremaschi et al. map_2008 (for PRO_COM -> id08 crosswalk)
# Source: Harvard Dataverse DOI:10.7910/DVN/I3VHZK
url_map <- "https://dataverse.harvard.edu/api/access/datafile/10631427"
dir.create("data_raw/italy/cremaschi_replication", showWarnings = FALSE)
dest_map <- "data_raw/italy/cremaschi_replication/map_2008.tab"

if (!file.exists(dest_map)) {
  download.file(url_map, dest_map)
  cat("Downloaded map_2008 crosswalk to", dest_map, "\n")
} else {
  cat("Map crosswalk already exists:", dest_map, "\n")
}

cat("Done.\n")
