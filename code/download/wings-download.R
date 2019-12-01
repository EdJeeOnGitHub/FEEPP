download.file("https://dataverse.harvard.edu/api/access/datafile/2725073",
              destfile = "data/wings-data.zip")

unzip("data/wings-data.zip",
      exdir = "data/wings-data/",
      overwrite = TRUE)
