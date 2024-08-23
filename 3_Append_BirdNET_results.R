
library(stringr)
library(stringi)
library(dplyr)

getwd()
project_dir <- "C:/Users/StewartL/Documents/Data/Shorebirds/Shorebirds_recognizer/test_directory"


csvs <- dir(project_dir,recursive = T,full.names = T,pattern = ".csv") # find all csvs

results <- read.csv(csvs[1])

i=2
for(i in 2:length(csvs)){
  tmp <- read.csv(csvs[i])
  results <- rbind(results,tmp)
  print(paste("done with i =",i,"of",length(csvs)))
}



starts = stri_locate_last_regex(results$filepath,pattern = "\\\\")[,1]
ends = str_locate(results$filepath,pattern = ".wav")[,1]
rec_name = str_sub(results$filepath,start = starts+1, end = ends-1)


results$time <- str_sub(rec_name,start = nchar(rec_name)-5)

results$time <- paste(str_sub(results$time,1,2),str_sub(results$time,3,4),sep = ":")

results$date <- as.Date(str_sub(rec_name,start = nchar(rec_name)-14,end = nchar(rec_name)-7),
                        format = "%Y%m%d")

results$location<-str_sub(rec_name,start=1,end=nchar(rec_name)-16)

names(results)

results = results %>% select(filepath, location,date,time,start,end,scientific_name,common_name,
                   confidence, -lat,-lon,-week,overlap,sensitivity,min_conf,species_list,model)

snipe = str_detect(results$common_name,pattern = "Snipe")
phalarope = str_detect(results$common_name,pattern = "Phalarope")
godwit = str_detect(results$common_name,pattern = "Godwit")
tringa = str_detect(results$scientific_name,pattern = "Tringa")
dowitcher = str_detect(results$common_name,pattern = "Dowitcher")

shorebirds = results %>% filter(common_name %in% c("Spotted Sandpiper","Yellow Rail",
                                      "Whooping Crane","Common Crane",
                                      "Horned Grebe",
                                      "Red-necked Phalarope",
                                      "Least Sandpiper"
                                      )|snipe | phalarope | godwit | tringa | dowitcher)

write.csv(results,"Results_all_species.csv")
write.csv(shorebirds,"Results_shorebirds.csv")
