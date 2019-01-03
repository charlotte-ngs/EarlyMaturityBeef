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
  
  
  # merge to tbl_result
  if (is.null(tbl_result)){
    tbl_result <- tbl_current
  } else {
    tbl_result <- dplyr::inner_join(tbl_result, tbl_current, by="idaTvd")
  }
}
# adjust names
names(tbl_result) <- c("tvdnr", "CCa","CCc","CFa","CFc","CWa","CWc")
tbl_result
# compute index
## convert table
library(tidyr)
gatbl_result <- tbl_result %>%
  gather(trait, estimate, -tvdnr)
gatbl_result
# set economic weights
economic_weights <- "LM_economic_values"
economic_weights <- read.table(file = economic_weights, sep=";", header=TRUE)
economic_weights <- as.matrix(economic_weights)
economic_weights <- as.vector(economic_weights)
LMvec_ev <- c(economic_weights[2],economic_weights[1],economic_weights[4],economic_weights[3],economic_weights[6],economic_weights[5])
economic_weights <- "AN_economic_values"
economic_weights <- read.table(file = economic_weights, sep=";", header=TRUE)
economic_weights <- as.matrix(economic_weights)
economic_weights <- as.vector(economic_weights)
ANvec_ev <- c(economic_weights[2],economic_weights[1],economic_weights[4],economic_weights[3],economic_weights[6],economic_weights[5])
tbl_ev <- tibble(trait = c("CCa","CCc", "CFa","CFc", "CWa","CWc"), ev=ANvec_ev)
tbl_ev
# compute index
library(tidyr)
library(dplyr)
gatbl_result
tbl_ev
# join economic values according to each breeding value
tbl_idx_comp <- gatbl_result %>%
  inner_join(tbl_ev, by="trait") %>%
  # compute indexed value of each breeding value
  mutate(idx_comp = estimate * ev)
tbl_idx_comp
tbl_idx_comp <- tbl_idx_comp %>%
  # sum all indexed breeding values per tvdnr --> one index number per tvdnr.
  group_by(tvdnr) %>%
  summarise(ca_index = sum(idx_comp))
tbl_idx_comp
# rank all animals by the index number
tbl_idx_comp$ca_index_rank <- rank(-tbl_idx_comp$ca_index,ties.method = "max")
tbl_idx_comp <- tbl_idx_comp %>%
  arrange(tvdnr)

# rank all animals by the adults carcass fat breeding value
tbl_result
tbl_result$cfa_rank <- rank(-tbl_result$CFa,ties.method = "max")
tbl_result$cfc_rank <- rank(-tbl_result$CFc,ties.method = "max")
tbl_result <- tbl_result %>%
  arrange(tvdnr)
# compute correlation across ranks
length(tbl_result$cfa_rank)
tbl_result
length(tbl_idx_comp$ca_index_rank)
tbl_idx_comp
tbl_result
cor(tbl_result$cfa_rank, tbl_idx_comp$ca_index_rank, method = c("spearman"))
cor(tbl_result$cfc_rank, tbl_idx_comp$ca_index_rank, method = c("spearman"))
cor(tbl_result$cfc_rank, tbl_result$cfa_rank, method = c("spearman"))
. # plot correlation across ranks
data <- inner_join(tbl_result,tbl_idx_comp,by="tvdnr")
library("ggplot2")

ggplot(data,aes(cfa_rank,ca_index_rank)) +
  geom_jitter(se=FALSE)

ggplot(data,aes(cfc_rank,ca_index_rank)) +
  geom_jitter(se=FALSE)

# ggplot(data,aes(cfa_rank,ca_index_rank)) +
#   geom_smooth(se=FALSE)
# 
# ggplot(data,aes(cfc_rank,ca_index_rank)) +
#   geom_smooth(se=FALSE)



