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
## Subtidal_Apledore_Temps repository on GitHub (Intertidal-Subtidal-WG/additional_data_source)

# initial set up ----------------------------------------------------------

## load required packages
library(gh)
library(purrr)
library(readr)
suppressMessages(library(dplyr))
library(stringr)


# Create function that takes user name, a list of data frame to load in the environment, and the Sha of the commit of the last update of the repository.

load_data_SubTemp <- function(user_name, df_list, Sha = "79199a41b12e262532dcedaa38b163ef826f6f7c?recursive=1"){
  
  #create environment for the function
  env <- environment()
  
  # curate file list for import from github repo ----------------------------
  
  dir <- paste("GET /repos/Intertidal-Subtidal-WG/subtidal_appledore_temps/git/trees/", Sha, sep = "")
  
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
    ## select only files in the processed_data folder
    subset(str_detect(., pattern = "data/processed_data/")) %>% 
    print() %>% ## print out the file names in the subset
    ## paste the rest of the URL onto these files
    paste("https://raw.githubusercontent.com/Intertidal-Subtidal-WG/subtidal_appledore_temps/main/", 
          ## raw.githubusercontent allows access to the raw data
          ., sep = "")
  
  
  # import data -------------------------------------------------------------
  
  ## strings to remove from data table names after import
  strip <- c("https://raw.githubusercontent.com/Intertidal-Subtidal-WG/subtidal_appledore_temps/main/data/processed_data/", 
             ".RDS$")
  
  
  ## import all data files

  for (i in (1:length(repo_files))) {
    nam <- paste("temp__", str_remove_all(repo_files[i], paste(strip, collapse = "|")), sep = "")
    assign(nam, readRDS(url(repo_files[i])))
  }
  
  
  
  # check imported tables ---------------------------------------------------
  
  return(mget(df_list))
  
}


example <- load_data_SubTemp("Julien-Beaulieu", c("temp__buoy_predicted_temps"), "79199a41b12e262532dcedaa38b163ef826f6f7c?recursive=1")

### to separate example in different data frames

list2env(example, envir = .GlobalEnv)