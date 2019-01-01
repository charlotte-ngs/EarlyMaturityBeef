#Prerequisites
library(dplyr)
library(readr)
setwd("/qualstorzws01/data_tmp/zws/vms/ZWS2018/Msc")

#set loop to get all fill names at once

###
#set prefix of file names
sfnprefix <- "Merge"

#set breed
breed <- "AN"

#set animal group

#group <- "a"

#set list of breed files
vec_current_files <- list.files(pattern = paste(sfnprefix, breed, sep="_"))
vec_current_files
#set initial dataframe with zero entries
tbl_result <- NULL

# loop ueber alle Dateien einer Rasse
for (current_file in vec_current_files) {
  
  cat("Current file: ", current_file, "\n")
  # read data
  # tbl_current <- read.table(file = current_file,sep = "" , header = TRUE, 
  # stringsAsFactors= F, comment.char="")
  tbl_current <- read_delim(file = current_file, delim = " ")
  ### # filter here, if wanted
  ### # minimum number of daughters
  tbl_current <- tbl_current %>% filter(nDau > 20)
  ### # only label CH
  tbl_current <- tbl_current %>% filter(label == "CH")
  ### # only sex male
  tbl_current <- tbl_current %>% filter(sex == "M")
  tbl_current <-  select(tbl_current,idaTvd,estimate)
  
  