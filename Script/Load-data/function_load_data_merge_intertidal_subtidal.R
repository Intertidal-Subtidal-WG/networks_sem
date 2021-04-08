#Intertidal-Subtidal CIEE working group
#This script is to load data from different source
# Contributor: Julien Beaulieu and Joey

## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##
## Modified from 
## Intertidal-Subtidal CIEE Working group             ##
## Contributor: Joseph Burant                         ##
## Last updated: 04 March 2021                        ##
## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##

## This script can be used to import data directly from the public
## additional_data_source repository on GitHub (Intertidal-Subtidal-WG/additional_data_source)

# initial set up ----------------------------------------------------------

## load required packages
library(gh)
library(purrr)
library(readr)
suppressMessages(library(dplyr))
library(stringr)


# Create function that takes user name, a list of data frame to load in the environment, and the Sha of the commit of the last update of the repository.

load_data_MERG <- function(user_name, df_list, Sha = "e400ba33ccc8def2f4a8675a57b47d597c046599?recursive=1"){
  
  #create environment for the function
  env <- environment()
  
  # curate file list for import from github repo ----------------------------
  
  dir <- paste("GET /repos/Intertidal-Subtidal-WG/data_merge_intertidal_subtidal/git/trees/", Sha, sep = "")
  
  ## get list of files in SEED repo
  repo_dir <- gh(dir,
                 username = user_name)
  
  ## NOTES: 
  ## change username argument to your personal GitHub name
  ## you will also need your personal access token stored in your .Renviron
  ## you can do this by running:
  # usethis::edit_r_environ()
  ## and adding the line: GITHUB_PAT = "<your personal access token>"
  ## save the file (make sure to leave one empty line at the end)
  ## restarting R
  ## To get your personal access token from GitHub, navigate to 
  ## Settings > Developer settings > Personal access tokens

  
  ## subset to the files you want to import
  repo_files <- map_chr(repo_dir$tree, ~ .x$path) %>% 
    ## select only files in the cleaned_data folder
    subset(str_detect(., pattern = "tidy_data/")) %>% 
    print() %>% ## print out the file names in the subset
    ## paste the rest of the URL onto these files
    paste("http://raw.githubusercontent.com/Intertidal-Subtidal-WG/data_merge_intertidal_subtidal/main/", 
          ## raw.githubusercontent allows access to the raw data
          ., sep = "")
  
  ##Subset .csv files
  
  repo_files_csv <- repo_files %>% ## subset of subtidal files in repo
    grep(".csv", ., value = TRUE, fixed = TRUE)
  length(repo_files_csv)
  
  ##subset .RDS files
  
  repo_files_rds <- repo_files %>% ## subset of subtidal files in repo
    grep(".RDS", ., value = TRUE, fixed = TRUE)
  length(repo_files_rds)
  
  
  #repo_files_rds <- repo_files %>% ## subset of subtidal files in repo
  #  grep(".RDS", ., value = TRUE, fixed = TRUE)
  #length(repo_files_rds)
   
  
  # import data -------------------------------------------------------------
  
  ## strings to remove from data table names after import
  strip <- c("http://raw.githubusercontent.com/Intertidal-Subtidal-WG/data_merge_intertidal_subtidal/main/tidy_data/", 
             ".csv$")
  strip2 <- c("http://raw.githubusercontent.com/Intertidal-Subtidal-WG/data_merge_intertidal_subtidal/main/tidy_data/", 
              ".RDS$")
  
  
  ## import csv data files
  map(repo_files_csv, read.csv) %>% 
    set_names(
      paste("merg__",
            str_remove_all(repo_files_csv, 
                           paste(strip, 
                                 collapse = "|")),
            sep = "")) %>% 
    list2env(envir = env)
  
  
  ## clean up
  #rm(repo_dir, repo_files, strip)
  
 ## import RDS data files
  
  for (i in (1:length(repo_files_rds))) {
    nam <- paste("merg__", str_remove_all(repo_files_rds[i], paste(strip2, collapse = "|")), sep = "")
   assign(nam, readRDS(url(repo_files_rds[i])))
    }
  
  
  # check imported tables ---------------------------------------------------
  
  ## what tables do we have for each community?
  ls(pattern = "merg__") ## 4 tables from the intertidal dataset
  
  
  return(mget(df_list))
  
}


example <- load_data_MERG("Julien-Beaulieu", c("merg__kelp_quads_biomass", "merg__keen_all_methods_merged", "merg__combined_subtidal_abundance"), "e400ba33ccc8def2f4a8675a57b47d597c046599?recursive=1")

### to separate example in different data frames

list2env(example, envir = .GlobalEnv)
