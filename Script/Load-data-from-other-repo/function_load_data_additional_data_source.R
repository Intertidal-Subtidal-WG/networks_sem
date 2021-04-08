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

load_data_ADD <- function(user_name, df_list, Sha = "2e9957fe0b0164afe3d823883bacae5e8851dc1b?recursive=1"){
  
  #create environment for the function
  env <- environment()
  
  # curate file list for import from github repo ----------------------------
  
  dir <- paste("GET /repos/Intertidal-Subtidal-WG/additional_data_sources/git/trees/", Sha, sep = "")
  
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
    subset(str_detect(., pattern = "data/cleaned_data/")) %>% 
    print() %>% ## print out the file names in the subset
    ## paste the rest of the URL onto these files
    paste("https://raw.githubusercontent.com/Intertidal-Subtidal-WG/additional_data_sources/master/", 
          ## raw.githubusercontent allows access to the raw data
          ., sep = "")
  
  
  # import data -------------------------------------------------------------
  
  ## strings to remove from data table names after import
  strip <- c("https://raw.githubusercontent.com/Intertidal-Subtidal-WG/additional_data_sources/master/data/cleaned_data/", 
             ".csv$")
  
  
  ## import all data files
  map(repo_files, read.csv2) %>% 
    set_names(
      paste("add__",
            str_remove_all(repo_files, 
                             paste(strip, 
                                   collapse = "|")),
            sep = "")) %>% 
    list2env(envir = env)
  

  ## clean up
  rm(repo_dir, repo_files, strip)
  
  
  # check imported tables ---------------------------------------------------
  
  ## what tables do we have for each community?
  ls(pattern = "add__") ## 4 tables from the intertidal dataset
  
  
  return(mget(df_list))
  
}


example <- load_data_ADD("Julien-Beaulieu", c("add__plankton_data", "add__lobster_catch_ts"), "2e9957fe0b0164afe3d823883bacae5e8851dc1b?recursive=1")

### to separate example in different data frames

list2env(example, envir = .GlobalEnv)