#Intertidal-Subtidal CIEE working group
#This script is to load data from different source
# Contributor: Julien Beaulieu

## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##
## Modified from 
## Intertidal-Subtidal CIEE Working group             ##
## Contributor: Joseph Burant                         ##
## Last updated: 04 March 2021                        ##
## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##

## This script can be used to import data directly from the public
## SEED repository on GitHub (brianscheng/SEED)

# initial set up ----------------------------------------------------------

## load required packages
library(gh)
library(purrr)
library(readr)
suppressMessages(library(dplyr))
library(stringr)


# Create function that takes user name and a list of data frame to load in the environment

load_data_SEED <- function(user_name, df_list, Sha = "main?recursive=1"){
    
  #create environment for the function
  env <- environment()
  
  # curate file list for import from github repo ----------------------------
  
  dir <- paste("GET /repos/brianscheng/SEED/git/trees/", Sha, sep = "")
  
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
  
  ## create a list of URLs for the files in the SEED repo
  repo_files <- map_chr(repo_dir$tree, ~ .x$path) %>% 
    paste("https://raw.githubusercontent.com/brianscheng/SEED/main/", 
          ## raw.githubusercontent allows access to the raw data
          ., sep = "")
  
  ## create subsets of relevant URLs for SEED
  repo_files_int <- repo_files %>% ## subset of intertidal files in repo
  grep("data/intertidal/", ., value = TRUE, fixed = TRUE) %>% 
    grep("_v2.csv", ., value = TRUE, fixed = TRUE)
  length(repo_files_int) ## 4 CSVs to import
  
  repo_files_sub <- repo_files %>% ## subset of subtidal files in repo
    grep("data/keen/", ., value = TRUE, fixed = TRUE) %>% 
    grep(".csv", ., value = TRUE, fixed = TRUE)
  length(repo_files_sub) ## 7 CSVs to import
  

  
  ## strings to drop from data table names on import to create name of the df after import
  strip_i <- c("https://raw.githubusercontent.com/brianscheng/SEED/main/data/intertidal/", 
               "_data_v2.csv$") ## intertidal
  
  strip_s <- c("https://raw.githubusercontent.com/brianscheng/SEED/main/data/keen/keen_", 
               ".csv$") ## subtidal
  
  
  # import data -------------------------------------------------------------
  
  ## import data into a list, then split the list into the environment
  map(repo_files_int, read_csv) %>% ## intertidal
    map(., ~ select(., -X1)) %>% ## drop row number column X1
    set_names(
      paste("i__", 
            str_remove_all(repo_files_int, 
                           paste(strip_i, 
                                 collapse = "|")), 
            sep = "")) %>% 
    list2env(envir = env)
  
  map(repo_files_sub, read_csv) %>% ## subtidal
    # map(., ~ select(., -X1)) %>% ## no row number column
    set_names(
      paste("s__", 
            str_remove_all(repo_files_sub, 
                           paste(strip_s, 
                                 collapse = "|")), 
            sep = "")) %>% 
    list2env(envir = env)
  
  ## one table in intertidal data with a different string format
  s__kelp_quad_biomass <- ## a nicer name
    `s__https://raw.githubusercontent.com/brianscheng/SEED/main/data/keen/kelp_quads_biomass` 
  rm(`s__https://raw.githubusercontent.com/brianscheng/SEED/main/data/keen/kelp_quads_biomass`, envir = environment())
  
  ## clean up
  rm(list = c(ls(pattern = "^strip_"), ls(pattern = "^repo_")))
  
  
  # check imported tables ---------------------------------------------------
  
  ## what tables do we have for each community?
  ls(pattern = "i__") ## 4 tables from the intertidal dataset
  
  ls(pattern = "s__") ## 7 tables from subtidal dataset
  
  
  return(mget(df_list))

}


exemple <- load_data_SEED("Julien-Beaulieu", c("s__kelp", "i__percent_cover"))

### to separate example in different data frames

list2env(example, envir = .GlobalEnv)
