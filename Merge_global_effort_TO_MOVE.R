
rm(list=ls())

# load libraries - before being able to install dplyr I had to install all of the below packages that 
# were missing from my personal library folder (home/R/x86_64-pc-linux-gnu-library/4.1) which is a new folder created when R was updated to the 4.1 version
# install.packages("purrr")
# install.packages("fansi")
# install.packages("utf8")
# install.packages("R6")
# install.packages("tibble")
# install.packages("pkgconfig")
# install.packages("dplyr")
library("dplyr") 


# to install tidyverse ... 
install.packages("dgest")
install.packages("ps")
install.packages("stringi")
install.packages("stringr")
install.packages("assertthat")
install.packages("gtable")
install.packages("backports")
install.packages("broom")
install.packages("processx")
install.packages("fs")
install.packages("digest")
install.packages("htmltools")
install.packages("prettyunits")
install.packages("evaluate")
install.packages("colorspace")
install.packages("munsell")
install.packages("scales")
install.packages("xml2")
install.packages("highr")
install.packages("knitr")

install.packages("tidyverse")# install.packages("tidyverse", dependencies = TRUE) doing this instead of the above did not make any difference. 
library("tidyverse")

# to install vroom ... 
install.packages("vroom")
library("vroom")


# trial read 1 file 
# a<-read.csv("/../../rd/gem/private/users/yannickr/effort_mapped_bycountry/mapped_2011_I_312.csv")
# head(a)

# define path to effort files
path<-"/../../rd/gem/private/users/yannickr/effort_mapped_bycountry/"

# load LME and Country codes to be merged with effort data 
code_LME<-read.csv("/../../rd/gem/private/users/yannickr/LMESeq.csv")
head(code_LME) # https://www.lmehub.net/
code_SAUP<-read.csv("/../../rd/gem/private/users/yannickr/SAUPcode_to_Country.csv")
head(code_SAUP)
colnames(code_SAUP)<-c("Country", "SAUP", "ISO3", "Region")

# load EEZ codes to be merged with effort 
# still need to figure this out ...
code_EEZ<-read.csv("/../../rd/gem/private/users/yannickr/Cells_LatLon_EEZ.csv")
head(code_EEZ) # https://www.marineregions.org/eezsearch.php. 
unique(code_EEZ[,c(3,5)]) # WARNING - these are not the same not sure what to use as code

# this refers to FAO but look like EEZ
sort(unique(code_EEZ$A_Code))
filter(code_EEZ, A_Code == 8) # this should be montenegro EEZ but is Albania in this file (see EEZ website)
filter(code_EEZ, A_Code == 12) # this should be Spain or Algeria (depending on the lat/lon choosen) but is alsways Algeria in this file 

# explore the codes 
sort(unique(code_LME$Lon))
sort(unique(code_LME$Lat))
sort(unique(code_EEZ$Lon))
sort(unique(code_EEZ$Lat)) # WARNING some missing ... Antarctica? 

# if you are doing the merge ... 
code_EEZ<-select(code_EEZ, c(FAOname, A_Code, Lat, Lon))
colnames(code_EEZ)<-c("EEZ_name","EEZ_ID", "Lat", "Lon")


# STEP 1 - merge Industrial fishing ----
# file names - reconstruct all possible combination of I fishing  
year<-seq(1950, 2017) # WARNING check these are the years 
saup<-unique(code_SAUP$SAUP)
a<-expand.grid(year, saup)
a$names<-paste0("mapped_",a$Var1, "_I_", a$Var2, ".csv")
names<-a$names

# WARNING - try with a subset first
length(names)/20 # let's try with 1/20th 
names<-names[1:830]
names<-names[1:3] # still fast checking so better only 3 

# loop through the files and group effort by year, LME, country, gear and FGroup 
trial3<-list()

# check running time 
start_time <- Sys.time()

# loop through the files ...  
for (i in 1:length(names)){
  
  if(file.exists(paste0(path, names[[i]]))) # if the filename exist, do the calculation below otherwise skip to the next file
  {
    
    # TRIAL 
    # i =1
    
    effort<-read.csv(paste0(path, names[[i]]))
    # head(effort)
    
    # check the mapped values 
    # library("reshape2")
    # a<-acast(effort1[,c(3,4,9)], Lat~Lon)
    # image(a) # WARNING - for some files  (e.g.mapped_2006_I_621.csv), it looks like the country is fishing everywhere ... 
    
    # merge effort with LME and Country 
    trial<-effort %>% 
      left_join(select(code_LME, -Seq)) %>% 
      left_join(code_SAUP) %>% 
      droplevels()

    # check the merging 
    # nrow(trial)
    # head(trial)
    # sort(unique(trial$LME)) # WARNING - as per image(a) above, in some cases the country is fishing in all LMEs (e.g, mapped_2006_I_621.csv -> this is Madeira - LME 25 - Iberian coast)  
    # sort(unique(trial$Country)) 
    
    # summarise effort and other variables by country, gear and FGroup
    trial2<-trial %>% 
      group_by(Gear, FGroup, Sector, LME, Country) %>% # Length_Category
      summarise(NomActive = sum(NomActive, na.rm = TRUE), 
                EffActive = sum(EffActive, na.rm = TRUE), 
                NV = sum(NV, na.rm = TRUE),
                P = sum(P, na.rm = TRUE),
                GT = sum(GT, na.rm = TRUE)) %>% 
      ungroup()
    
    # extract year from file name and add to dataset  
    trial2$Year<-as.numeric(substring(names[[i]], 8, 11)) # WARNING - check the year position in the string is always the same 
    
    # save as list 
    trial3[[i]]<-trial2
  } 
}

# check running time 
end_time <- Sys.time()
time = end_time - start_time # time for 10 files is 6.5 min 
time 

# # total number of files in folder
# a<-29852/3 # one sector 
# b<-a/10 # how many sets of 10 files 
# c<-b*6.5 # each set taking 6.5 min - total minutes 
# d<-c/60 # from minute to h
# e<-d/24 # from h to days

# merge all list data into a dataframe
big_data = do.call(rbind, trial3)
head(big_data)

# Save the end product in gem48 - where? as csv? or Rdata or netcdf (though not spatially resolved so not sure netcdf is the way to go)? 
saveRDS(big_data, file = "/../../rd/gem/private/users/yannickr/merging_cami/effort_year_country_LME_gear_FGroup.rds")
