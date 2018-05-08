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
#       any ENGR 132 csv                    BlackBoard 
# 
# Package dependancies: 
#    library(readr)  #read in csv files
#    library(tibble) #tidy data frames
#    library(tcltk)  #provides an OS independent way to select a folder
#    library(beepr)  #audio notifaction for user input
#
# Changelog:
#     2018.05.08.    Initial code
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
                                     windowTitle = "Choose directory with DATA FILES to anonymize.")
  
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
#   #grepl checks whether temp has any file that matches exactly as
#   #commonFileName. If no file matches then !any is true and the code will be
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




## Create blank de-anonymization tables (separate student and grader tables) ####
# create empty student de-anonymization table
keyTable_studentIDs <- tibble()
keyTable_studentIDs <- add_column(.data = keyTable_studentIDs, 
                                  "origStuID" = "",
                                  "anonStuID" = "",
                                  "firstName" = "",
                                  "lastName" = "")

# create empty grader de-anonymization table
keyTable_graderIDs <- tibble()
keyTable_graderIDs <- add_column(.data = keyTable_graderIDs, 
                                 "origGraderID" = "",
                                 "anonGraderID" = "")

## Loop through each CSV file in a given folder ####
for (curPath in paths_dataFiles) {
  ### Read in CSV file ####
  curDataFile <- read_csv(file = curPath, col_names = T, progress = T)
  
  ### Store list of unique IDs ####
  # STUDENTS (check for column existence, then store unique IDs from the correct column)
  if(any(names(curDataFile) == "User ID")){
    stuID_field <- "User ID"
    curStuIDs <- unique(curDataFile$`User ID`)
  }else if(any(names(curDataFile) == "Username")){
    stuID_field <- "Username"
    curStuIDs <- unique(curDataFile$Username)
  }else{
    stuID_field <- NULL
    curStuIDs <- NULL
  }
  # GRADERS (check for column existence, if exists then store unique IDs)
  if(any(names(curDataFile) == "Grader")){
    curGraderIDs <- unique(curDataFile$Grader)
  }else{
    curGraderIDs <- NULL
  }
  
  ### TODO(***TW: verify none of the IDs are already in the anon tables,  ####
  #       if they are then skip creating a new value)

  
  ### extract the current semester name ####
  if(any(names(curDataFile) == "Course Name")){
    curSemester <- paste0(regmatches(curDataFile$`Course Name`[1],
                            regexpr(pattern = "^[[:alpha:]]{2}",
                                    text = curDataFile$`Course Name`[1])),
                        regmatches(curDataFile$`Course Name`[1],
                                   regexpr(pattern = "-[[:digit:]]{4}",
                                           text = curDataFile$`Course Name`[1])))
  # abbreviate the current semester (first 2 letters of season + last 2 digits of year)
  curSemester <- paste0(substr(curSemester, 1, 2), 
                        substr(curSemester, 6, 7))
  }else{
    curSemester <- "unknown"
  }
  
  ### Generate random ID in appropriate form ####
  # STUDENTS: generate 6 digit random integer
  randIntegers <- as.character(sample(100000:999999, length(curStuIDs)))
  #build anon IDs                      
  curStuAnonIDs  <- paste0("engr132_", curSemester, "Stu_", randIntegers)
 
  #GRADERS: generate 4 digit random integer
  randIntegers <- as.character(sample(1000:9999, length(curGraderIDs)))
  #build anon IDs 
  #TODO(TW: format should be: 'engr132_Sp17X_1234'
        # Where X =
        #   ITS for the course staff
        #   INST for instructors
        #   GTA for grad TAs
        #   PT for undergrad peer teachers
        #   GR for undergrad graders)
  curGraderAnonIDs  <- paste0("engr132_", curSemester, "Grader_", randIntegers)
  
  ### TODO(TW: save first and last names for the students and save into the table) ####
  
  ### Add each unique ID to the appropriate table ####
  #save STUDENT ID pairs IF values exist in ID list
  if(length(curStuIDs) > 0){
    keyTable_studentIDs <- add_row(.data = keyTable_studentIDs,
                                 origStuID = curStuIDs,
                                 anonStuID = curStuAnonIDs)
  }

  #save GRADER ID pairs IF values exist in ID list
  if(length(curGraderIDs) > 0){
    keyTable_graderIDs <- add_row(.data = keyTable_graderIDs,
                                origGraderID = curGraderIDs,
                                anonGraderID = curGraderAnonIDs)
  }
  ### TODO(TW:Verify the new ID does not already exist in the table) ####

  
  ### Clear first and last name columns ####
  curDataFile$`First Name` <- ""
  curDataFile$`Last Name` <- ""
  
  ### Loop through each anon table and conduct the ID replacement ####
  # STUDENTS
  for (i in 1:nrow(keyTable_studentIDs)) {
    # find rows where the ID matches the currently indexed ID, 
    #   replace all mathes with the anon ID
    if(stuID_field == "User ID"){
      curDataFile$`User ID`[curDataFile$`User ID` == 
                              keyTable_studentIDs$origStuID[i]] <- 
        keyTable_studentIDs$anonStuID[i]
    }else if(stuID_field == "Username"){
      curDataFile$Username[curDataFile$Username == 
                              keyTable_studentIDs$origStuID[i]] <- 
        keyTable_studentIDs$anonStuID[i]      
    }#end if-else
  }#end student loop
  
  #GRADERS
  if(length(curGraderIDs) > 0){
    for (i in 1:nrow(keyTable_graderIDs)) {
      # find rows where the ID matches the currently indexed ID, 
      #   replace all mathes with the anon ID
      curDataFile$Grader[curDataFile$Grader == 
                           keyTable_graderIDs$origGraderID[i]] <- 
        keyTable_graderIDs$anonGraderID[i]
    }#end grader loop
  }#end grader if
  
  ### Save out the data with an appropriate filename ####
  message(paste0("\nSaving CSV file: '", paste0("anon_", basename(curPath), "'")))
  write.csv(file = file.path(path_outputFolder, paste0("anon_", basename(curPath))),
            x = curDataFile, row.names = FALSE)  
  
}#end of loop through CSV files in the data folder


## Save anon tables to file ####
cat("\nSaving CSV files (ID key tables).")
write.csv(file = file.path(path_outputFolder, "_ENGR132_anonymized_student_IDs.csv"),
          x = keyTable_studentIDs, row.names = FALSE)  
write.csv(file = file.path(path_outputFolder, "_ENGR132_anonymized_grader_IDs.csv"),
          x = keyTable_graderIDs, row.names = FALSE)  

