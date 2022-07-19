

#Upload files to google drive
#install packages
remotes::install_github("claudiozandonella/trackdown",
                        build_vignettes = TRUE, force=T)

install.packages("googledrive")

#set google doc
googledrive::drive_auth()

#load libraries
library(googledrive)
library(trackdown)

#upload file
upload_file(
  file = "Manuscript/Lanuza_et_al_2022.Rmd", 
  gfile = "Reproductive_traits"
)


#download file 
download_file(
  file = "Manuscript/Lanuza_et_al_2022.Rmd", 
  gfile = "Reproductive_traits"
)
