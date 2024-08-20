
##### Get set up #####


#install.packages("stringr")
library(stringr)
#install.packages("R.utils")
library(R.utils)
#library(purrr)
#install.packages("filesstrings")
library(filesstrings)
library(stringr)
library(dplyr)


# specify where the recordings are being read from and saved to

dir.create("test_directory")
save_dir <-paste0(getwd(),"/test_directory")
save_dir

read_dir <- "D:/Fort Smith 2022-2023 ARU"

dir(read_dir) # these should be location / station names
n<-length(dir(read_dir))

i=2
for(i in 1:n){
  dir_tmp <- dir(read_dir,full.names = T)[i]
  location <- dir(read_dir,full.names = F)[i]
  file = dir(dir_tmp,recursive = T,pattern = ".wav",full.names = T)
  if(length(file)>0){
    head(file)
    df = data.frame(file)
    
    df = df %>%
      mutate(nchar = nchar(file),
             month = str_sub(file,nchar-14,nchar-13),
             hour = str_sub(file,nchar-9,nchar-8),
             day = str_sub(file,nchar-12,nchar-11)
      )
    
    str(df)
    df$hour = as.numeric(df$hour)
    df = df %>% filter(month=="06" & hour %in% c(1:4,20:24))
    df = df %>% group_by(day) %>% slice_sample(n=3)
    df = df %>% mutate(new_name = paste0(location,"_",str_sub(file,nchar-18,nchar)))
    
    
    if(nrow(df)>0){
      dir_tmp_tmp = paste0(save_dir,"/",location)
      dir.create(dir_tmp_tmp)
      file.copy(from = df$file,to = dir_tmp_tmp)
      
      file.rename(dir(dir_tmp_tmp,full.names = T), paste0(dir_tmp_tmp,"/",df$new_name))
    }
    
  }
  
  
  
  
}
