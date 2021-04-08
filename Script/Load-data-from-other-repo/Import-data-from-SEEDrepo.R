## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##
## Intertidal-Subtidal CIEE Working group             ##
## Contributor: Joseph Burant                         ##
## Last updated: 04 March 2021                        ##
## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ## -- ##

## This script can be used to import data directly from the public
## SEED repository on GitHub (brianscheng/SEED)

# initial set up ----------------------------------------------------------

## where am I working?
here::here()

## load required packages
library(gh)
library(purrr)
library(readr)
suppressMessages(library(dplyr))
library(stringr)


# curate file list for import from github repo ----------------------------

## get list of files in SEED repo
repo_dir <- gh("GET /repos/brianscheng/SEED/git/trees/main?recursive=1",
               username ="Julien-Beaulieu")
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

## create subsets of relevant URLs
#repo_files_int <- repo_files %>% ## subset of intertidal files in repo
  grep("data/intertidal/", ., value = TRUE, fixed = TRUE) %>% 
  grep("_v2.csv", ., value = TRUE, fixed = TRUE)
length(repo_files_int) ## 4 CSVs to import

repo_files_sub <- repo_files %>% ## subset of subtidal files in repo
  grep("data/keen/", ., value = TRUE, fixed = TRUE) %>% 
  grep(".csv", ., value = TRUE, fixed = TRUE)
length(repo_files_sub) ## 7 CSVs to import

## strings to drop from data table names on import
strip_i <- c("https://raw.githubusercontent.com/brianscheng/SEED/main/data/intertidal/", 
             "_data_v2.csv$") ## intertidal

strip_s <- c("https://raw.githubusercontent.com/brianscheng/SEED/main/data/keen/keen_", 
             ".csv$") ## subtidal
strip_add <- c()

# import data -------------------------------------------------------------

## import data into a list, then split the list into the environment
map(repo_files_int, read_csv) %>% ## intertidal
  map(., ~ select(., -X1)) %>% ## drop row number column X1
  set_names(
    paste("i_", 
          str_remove_all(repo_files_int, 
                         paste(strip_i, 
                               collapse = "|")), 
          sep = "")) %>% 
  list2env(.GlobalEnv)

map(repo_files_sub, read_csv) %>% ## subtidal
  # map(., ~ select(., -X1)) %>% ## no row number column
  set_names(
    paste("s_", 
          str_remove_all(repo_files_sub, 
                         paste(strip_s, 
                               collapse = "|")), 
          sep = "")) %>% 
  list2env(.GlobalEnv)

## one table in intertidal data with a different string format
s_kelp_quad_biomass <- ## a nicer name
  `s_https://raw.githubusercontent.com/brianscheng/SEED/main/data/keen/kelp_quads_biomass` 
rm(`s_https://raw.githubusercontent.com/brianscheng/SEED/main/data/keen/kelp_quads_biomass`)

## clean up
rm(list = c(ls(pattern = "^strip_"), ls(pattern = "^repo_")))


# check imported tables ---------------------------------------------------

## what tables do we have for each community?
ls(pattern = "i_") ## 4 tables from the intertidal dataset

ls(pattern = "s_") ## 7 tables from subtidal dataset
