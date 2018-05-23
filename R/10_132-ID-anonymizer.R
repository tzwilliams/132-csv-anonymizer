## ===================================================== ##
# Title:        User ID Anonymizer for ENGR 132 ####
# Project:      Purdue ENGR 132 
#               https://github.com/tzwilliams/132-csv-anonymizer
# 
# Copyright 2018 Taylor Williams
# 
#     Licensed under the Apache License, Version 2.0 (the "License");
#     you may not use this file except in compliance with the License.
#     You may obtain a copy of the License at
#     
#     http://www.apache.org/licenses/LICENSE-2.0
#     
#     Unless required by applicable law or agreed to in writing, software
#     distributed under the License is distributed on an "AS IS" BASIS,
#     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#     See the License for the specific language governing permissions and
#     limitations under the License.
#
#
#
# Authors:      Taylor Williams
# Affiliation:  Purdue University
# 
# Description:  Removes personally identifyable information (PII) from BlackBoard CSV
#                 files generated in Purdue's ENGR 132.  Students and graders are
#                 both anonymized.  All CSV files in the user selected directory 
#                 are anonymized.
#               The PII fields cleaned are
#                 *  
#               Output CSV files are saved in user selected directory
#                 * anonymized data: 'anon_' prepended on the original CSV filename
#                 * anon. ID to orig. ID conversion tables 
#                   - Student: '_ENGR132_anonymized_student_IDs.csv'
#                   - Grader: '_ENGR132_anonymized_grader_IDs.csv'
# 
# Input stack:  
#     filename                            source
#       one or more ENGR 132 csv            BlackBoard 
# 
# Package dependancies: 
#    library(readr)  #read in csv files
#    library(tibble) #tidy data frames
#    library(tcltk)  #provides an OS independent way to select a folder
#    library(beepr)  #audio notifaction for user input
#    library(dplyr)  #data extraction and transformation
#
# Changelog:
#     2018.05.08. Initial code
#     2018.05.23. looking for IDs that already exist in the anon. tables
#                 ensureing no duplicate anon. IDs are put into the tables
#                   
# Feature wishlist:  (*: planned but not complete)
#     *              
## ===================================================== ##

## ******* script setup ******************************** #####
## Clean the environment ########## 
rm(list=ls())  

## Required libraries ########## 
library(readr)  #read in csv files
library(tibble) #tidy data frames
library(tcltk)  #provides an OS independent way to select a folder
library(beepr)  #audio notifaction for user input
library(dplyr)  #data extraction and transformation

######### Load external functions ##########



######### Internal functions ##########
#Function: Interactively select directory (OS independent, but not available for RStudio Server)
InteractiveSetDir <- function(msgToUser = "IMPORTANT: Select your directory.", 
                              windowTitle = "") {
  beep()
  message(msgToUser)
  message("If a folder choice window doesn't appear, look for it behind your current window.")

  #tcltk package provides an OS independent way to select a folder
  library(tcltk)

  # open a folder selection window (defaults to 'My Documents').  Sometimes this opens in the background.
  dir <- tk_choose.dir(default = getwd(), caption = windowTitle)
  
  return(dir)
}



# end of script setup
## *************************************** #####


######### Main script ####
#set working directory to the folder containing all course folders

## 	Ask user for folder paths ####
#path of folder containing all CSVs to anonymize  
path_dataFolder <- InteractiveSetDir(msgToUser = 
                                       "IMPORTANT: Select directory with CSVs to anonymize.", 
                                     windowTitle = "Choose directory with CSV DATA FILES to anonymize.")

#path of folder to save outputs
path_outputFolder <- InteractiveSetDir(msgToUser = "IMPORTANT: Select your OUTPUT directory.", 
                                       windowTitle = "Choose an output directory")


## Read in the data paths ####
paths_dataFiles <- list.files(path = file.path(path_dataFolder),
                              full.names = T,
                              pattern = ".csv")
num_dataFiles <- length(paths_dataFiles) # Counts the number of data files

#### Buggy code for allowing there to be multiple separate folders within the base data folder ####
# ## Store all CSV file paths to process ####
# names_dataFolders <- list.dirs(path = path_dataFolder,  
#                         full.names = T, 
#                         recursive = T) # Reads all the folder and sub-folder names 
# num_dataFolders <- length(names_dataFolders) # Counts the number of folders
# 
# commonFileName <- ".csv"
# 
# #  check for error in the file ####
# names_errorFolders <- tibble()  #empty tibble for saving folder names that return errors
# names_errorFolders <- add_column(.data = names_errorFolders, 
#                                   "i" = "",
#                                   "errorFolderPath" = "")
# 
# for(i in 1:num_dataFolders){
#   folderFileName <- names_dataFolders[i] 
#   
#   # saves the path to all the files ending with .csv in 'temp'
#   temp <- list.files(path = file.path(num_dataFolders, pattern = ".csv")) 
#   
#   #grepl checks whether 'temp' has any file that matches exactly as
#   #commonFileName. If no file matches then '!any' is true and the code will be
#   #executed
#   if(!any(grepl(pattern = commonFileName, 
#                 x = temp, 
#                 ignore.case = T))){ 
#     #saves the location and name of the folder which has the problem file
#     names_errorFolders <- add_row(names_errorFolders, 
#                                   i = i, 
#                                   errorFolderPath = names_dataFolders[i]) 
#     
#     print(c(i, names_dataFolders[i])) #prints the fileError
#     
#     names_dataFolders <- names_dataFolders[-i]  #remove the folder name from the list
#     # stop("there is an error in the file named above") #command to stop the code from running
#   }
# }



## read or create (de)anonymization tables ####
# see if any CSV files already exist in the output director; if so, save file paths
paths_outputFiles <- list.files(path = file.path(path_outputFolder),
                                full.names = T,
                                pattern = ".csv")
num_outputFiles <- length(paths_outputFiles) # Counts the number of data files



## read or create (de)anonymization tables (from or in output directory)
# read or create STUDENT table
if(any(grepl(pattern = "_ENGR132_anonymized_student_IDs.csv",
             x = paths_outputFiles,
             ignore.case = T))){
  
  # read in table if it already exists
  keyTable_studentIDs <- read_csv(file = 
                                    file.path(path_outputFolder, 
                                              "_ENGR132_anonymized_student_IDs.csv"),
                                  col_names = T)  
}else{ 
  # create empty student (de)anonymization table
  keyTable_studentIDs <- tibble()
  keyTable_studentIDs <- add_column(.data = keyTable_studentIDs, 
                                    "origStuID" = "",
                                    "anonStuID" = "",
                                    "firstName" = "",
                                    "lastName" = "")
  
}

# read or create GRADER table
if(any(grepl(pattern = "_ENGR132_anonymized_grader_IDs.csv",
             x = paths_outputFiles,
             ignore.case = T))){
  
  # read in table if it already exists
  keyTable_graderIDs <- read_csv(file = 
                                    file.path(path_outputFolder, 
                                              "_ENGR132_anonymized_grader_IDs.csv"),
                                  col_names = T)  
  
}else{ 
  # create empty grader (de)anonymization table
  keyTable_graderIDs <- tibble()
  keyTable_graderIDs <- add_column(.data = keyTable_graderIDs, 
                                   "origGraderID" = "",
                                   "anonGraderID" = "")
  
}




## Loop through each CSV data file in a given data folder ####
for (curPath in paths_dataFiles) {
  ### Read in a CSV data file ####
  curData <- read_csv(file = curPath, col_names = T, progress = T)
  
  ### Store list of unique IDs ####
  # STUDENTS (check for ID column existence, then store unique IDs from the correct column)
  if(any(names(curData) == "User ID")){
    stuID_field <- "User ID"
    curStuIDs <- unique(curData$`User ID`)
  }else if(any(names(curData) == "Username")){
    stuID_field <- "Username"
    curStuIDs <- unique(curData$Username)
  }else{
    stuID_field <- NULL
    curStuIDs <- NULL
  }
  # GRADERS (check for ID column existence, if exists then store unique IDs)
  if(any(names(curData) == "Grader")){
    curGraderIDs <- unique(curData$Grader)
  }else{
    curGraderIDs <- NULL
  }
  
  ### TODO(***TW: verify none of the IDs are already in the anon tables,  ####
  #       if they are then skip creating a new value)

  
  ### extract the current semester name ####
  if(any(names(curData) == "Course Name")){
    curSemester <- paste0(regmatches(curData$`Course Name`[1],
                            regexpr(pattern = "^[[:alpha:]]{2}",
                                    text = curData$`Course Name`[1])),
                        regmatches(curData$`Course Name`[1],
                                   regexpr(pattern = "-[[:digit:]]{4}",
                                           text = curData$`Course Name`[1])))
  # abbreviate the current semester (first 2 letters of season + last 2 digits of year)
  curSemester <- paste0(substr(curSemester, 1, 2), 
                        substr(curSemester, 6, 7))
  }else{
    curSemester <- "unknown"
  }
  

  

  ### Add each unique ID to the appropriate table ####
  #save STUDENT information (ID, anon ID, name) if not already in the table
  if(length(curStuIDs) > 0){
    for (i in 1:length(curStuIDs)) {
      #read current id 
      curID <- curStuIDs[i]
      
      #check if current id already exists in the anon table
      if(any(grepl(pattern = curID,
                   x = keyTable_studentIDs$origStuID))){
        curAnonID <- keyTable_studentIDs[keyTable_studentIDs$origStuID == curID,'anonStuID']

      }else{ #if curID is a new ID then add the student's information to the table

        #generate new anon ID and check if it is already in use (if so, generate new ID)
        repeat{
          # generate a 6 digit random integer
          randInteger <- as.character(sample(100000:999999, 1))
          #build anon ID                  
          curAnonID  <- paste0("engr132_", curSemester, "Stu_", randInteger)
          
          # leave the repeat loop if the newAnonID does NOT exist in the current table
          if(!any(keyTable_studentIDs$anonStuID == curAnonID)){
            break
          }
        } # end anon student ID generation loop

        
        #grab data that matches the current id, take the first row, extract the Name field
        firstName = curData[curData$`User ID` == curStuIDs[i],][1,]$`First Name`
        lastName  = curData[curData$`User ID` == curStuIDs[i],][1,]$`Last Name`
        
        #add student information to the anon table
        keyTable_studentIDs <- add_row(.data = keyTable_studentIDs,
                                       origStuID = curID,
                                       anonStuID = curAnonID,
                                       firstName = firstName,
                                       lastName  = lastName)
      }
    }
  } #end student table creation
  
  
  #save GRADER information (ID, anon ID, name) if not already in the table
  if(length(curGraderIDs) > 0){
    for (i in 1:length(curGraderIDs)) {
      #read current id 
      curID <- curGraderIDs[i]
      
      #check if current id already exists in the anon table
      if(any(grepl(pattern = curID,
                   x = keyTable_graderIDs$origGraderID))){
        curAnonID <- keyTable_graderIDs[keyTable_graderIDs$origGraderID == curID,'anonGraderID']
        
      }else{ #if curID is a new ID then add the grader's information to the table
        
        #generate new anon ID and check if it is already in use (if so, generate new ID)
        repeat{
          #generate 4 digit random integer
          randInteger <- as.character(sample(1000:9999, 1))
          #build anon ID 
          #TODO(TW: format should be: 'engr132_Sp18XX_1234'
          # Where XX =
          #   ITS for the course staff
          #   INST for instructors
          #   GTA for grad TAs
          #   PT for undergrad peer teachers
          #   GR for undergrad graders;
          #     I need to find out what the column actually looks like.  
          #     I'm assuming that it is one of these XX codes followed by a number)
          graderType  <- regmatches(curGraderIDs[i], 
                                    regexpr(pattern = "^[[:alpha:]]*", 
                                            text = curGraderIDs[i]))
          curAnonID  <- paste0("engr132_", curSemester, graderType, "_", randInteger)          

          # curAnonID  <- paste0("engr132_", curSemester, "Grader_", randInteger)                    
          
          # leave the repeat loop if the newAnonID does NOT exist in the current table
          if(!any(keyTable_graderIDs$anonGraderID == curAnonID)){
            break
          }
        } # end anon grader ID generation loop
        

        #add grader information to the anon table
        keyTable_graderIDs <- add_row(.data = keyTable_graderIDs,
                                      origGraderID = curID,
                                      anonGraderID = curAnonID)
      }
    }
  }  #end grader table creation

  
  ### Clear student first and last name columns ####
  curData$`First Name` <- ""
  curData$`Last Name` <- ""
  
  ### Loop through each anon table and conduct the ID replacement ####
  # STUDENTS
  for (i in 1:nrow(keyTable_studentIDs)) {
    # find rows where the ID matches the currently indexed ID, 
    #   replace all mathes with the anon ID
    if(stuID_field == "User ID"){
      curData$`User ID`[curData$`User ID` == 
                              keyTable_studentIDs$origStuID[i]] <- 
        keyTable_studentIDs$anonStuID[i]
      
    }else if(stuID_field == "Username"){
      curData$Username[curData$Username == 
                              keyTable_studentIDs$origStuID[i]] <- 
        keyTable_studentIDs$anonStuID[i]      
      
    }#end if-else
  }#end student id replacement loop
  
  #GRADERS
  if(length(curGraderIDs) > 0){
    for (i in 1:nrow(keyTable_graderIDs)) {
      # find rows where the ID matches the currently indexed ID, 
      #   replace all mathes with the anon ID
      curData$Grader[curData$Grader == 
                           keyTable_graderIDs$origGraderID[i]] <- 
        keyTable_graderIDs$anonGraderID[i]
    }#end grader loop
  }#end grader if
  
  ### Save out the data with an appropriate filename ####
  message(paste0("\nSaving CSV file: '", paste0("anon_", basename(curPath), "'")))
  write.csv(file = file.path(path_outputFolder, 
                             paste0("anon_", basename(curPath))),
            x = curData, row.names = FALSE)  
  
}#end of looping through CSV data files


## Save anon tables to file ####
cat("\nSaving CSV files (ID key tables).")
write.csv(file = file.path(path_outputFolder, "_ENGR132_anonymized_student_IDs.csv"),
          x = keyTable_studentIDs, row.names = FALSE)  
write.csv(file = file.path(path_outputFolder, "_ENGR132_anonymized_grader_IDs.csv"),
          x = keyTable_graderIDs, row.names = FALSE)  

