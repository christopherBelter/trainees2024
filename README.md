# trainees2024
R code for doing trainee follow-up using the Scopus author APIs

## Setting up
The code in this repo uses two sets of custom R functions, `process_qvr_data` and `scopus_author_dev`. The QVR processing functions are documented in my QVR Processing repo, but are also included here. The Scopus author API functions use the `httr` and `XML` packages, so you'll probably need to install the `XML` package for them to work. They are also expecting your Scopus API key to be saved in your .Renviron file, or be in your current R environment, as `SCOPUS_API_KEY`. 

The code below is an ideal case that assumes that all of your search strings work perfectly. In reality, that often doesn't happen, so I've also included an expanded version of this code `trn_followup_2024.r` that handles cases when the `lapply()` functions run into an http error and exit before getting all of the data.

## Running the code
**Note:** the filepaths in this repo are local to my computer (i.e. I did't use relative filepaths), so you'll need to change all of them to match your local environment. 

First, read in the trainee data files from QVR, combine them into a single data frame, and sort them by training fiscal year and profile person ID.
```r
fnames <- list.files("projects/trainees", pattern = "^trainees_", full.names = TRUE)
trainees <- lapply(fnames, read.csv, stringsAsFactors = FALSE)
trainees <- do.call(rbind, trainees)
trainees <- trainees[order(trainees$TRN.FY, trainees$TRN.PROFILE.PERSON.ID),]
```

Then load the tidyverse for easier data manipulation.
```r
library(tidyverse)
```

Then add columns for the training core project number and type, collapse the multi-row QVR data to have one row per trainee, clean up each trainee's subsequent research grant code, and create a final training year for each trainee. 
```r
trainees <- trainees %>% 
  mutate(core_project_num = paste0(TRN.ACTIVITY.CODE, gsub("-.+", "", TRN.PROJECT.NUM)),
         training_type = gsub("\\d{2}", "XX", TRN.ACTIVITY.CODE))
source("r/functions/qvr_processing.r")
trainees <- process_qvr_data(trainees, "TRN.PROFILE.PERSON.ID")
trainees <- trainees %>% 
  filter(grepl("20[01]\\d", trainees$trn_fy)) %>%
  mutate(trn_subs_rsrch_grant_code = case_when(
    grepl("G", trn_subs_rsrch_grant_code) ~ "G",
    grepl("A", trn_subs_rsrch_grant_code) ~ "A",
    grepl("N", trn_subs_rsrch_grant_code) ~ "N"
  ),
  final_trn_fy = gsub(".+;", "", trn_fy)
  )
trainees$training_type[trainees$trn_activity_code == "K12"] <- "K12"
```

Then do some name cleaning on the trainee organization names and generate the list of search strings to pass to the API. 
```r
trainees <- trainees %>% mutate(org2 = clean_org_names(org_name))
trainees$affil_strings <- paste0("(AFFIL(", gsub(";", ") OR AFFIL(", trainees$org2), "))")
trainees$affil_strings[1:15]
trainees$name_string <- gsub("(.+?, .+? [A-Z])(.+)", "\\1", trainees$trainee_name)
mstrings <- paste0("AUTHFIRST(", gsub(".+, ", "", trainees$name_string), ") AND AUTHLASTNAME(", gsub(", .+", "", trainees$trainee_name), ") AND ", trainees$affil_strings)
mstrings <- gsub("\\.", "", mstrings)
mstrings <- gsub(";", "", mstrings)
mstrings[1:10]
```

With the data and search strings ready to go, retrieve the first set of potential matches. Run the list of search strings through the API, merge the resulting data with the original trainee names, organizations, and PPIDs, and save the resulting .csv file. This is calibrated to run one request (i.e. one trainee) per second to comply with the API rate limits, so this will take a while.
```r
source("r/functions/scopus_author_dev.r")
scopus1 <- lapply(1:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 3, outfile = paste0("projects/trainee followup/search data 2024/", "trainees1_", sprintf("_%05d", x), ".txt")))
#mstrings[3239]
#mstrings[3239] <- "AUTHFIRST(ANDREA K) AND AUTHLASTNAME(KALIS HORNTVEDT) AND (AFFIL(UNIVERSITY OF MINNESOTA))"
#scopus1 <- lapply(3239:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 3, outfile = paste0("projects/trainee followup/search data 2024/", "trainees1_", sprintf("_%05d", x), ".txt")))
#scopus1a <- scopus1
#fnames <- list.files("projects/trainee followup/search data 2024", full.names = TRUE)
#scopus1 <- lapply(fnames, extractSearchXML)
scopus1 <- mapply(cbind, "trainee_name" = trainees$trainee_name, "trainee_ppid" = trainees$trn_profile_person_id, "org_name" = trainees$org2, scopus1, SIMPLIFY = FALSE)
scopus1 <- do.call(rbind, scopus1)
write.csv(scopus1, file = "projects/trainee followup/2024_trainees_part1.csv", row.names = FALSE)
```

Then run the second set of potential matches. Identify the trainees who were not matched in the first result set, generate a new set of search strings, run the new searches, merge the resulting data with the unmatched trainee names, organizations, and PPIDs, and save the resulting .csv file.
```r
nomatch <- scopus1$trainee_ppid[which(is.na(scopus1$scopusAuthEID))]
mstrings <- paste0("AUTHFIRST(", gsub(".+, ", "", trainees$name_string[trainees$trn_profile_person_id %in% nomatch]), ") AND AUTHLASTNAME(", gsub(", .+", "", trainees$trainee_name[trainees$trn_profile_person_id %in% nomatch]), ") AND SUBJAREA(BIOC OR MEDI OR SOCI OR NEUR OR IMMU OR PSYC)")
scopus2 <- lapply(1:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 5, outfile = paste0("projects/trainee followup/search data 2024/", "trainees2_", sprintf("_%05d", x), ".txt")))
#mstrings[1059]
#mstrings[1059] <- "AUTHFIRST(ANDREA K) AND AUTHLASTNAME(KALIS) AND SUBJAREA(BIOC OR MEDI OR SOCI OR NEUR OR IMMU OR PSYC)"
#scopus2 <- lapply(1059:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 5, outfile = paste0("projects/trainee followup/search data 2024/", "trainees2_", sprintf("_%05d", x), ".txt")))
#scopus2a <- scopus2
#fnames <- list.files("projects/trainee followup/search data 2024", pattern = "^trainees2", full.names = TRUE)
#scopus2 <- lapply(fnames, extractSearchXML)
scopus2 <- mapply(cbind, "trainee_name" = trainees$trainee_name[trainees$trn_profile_person_id %in% nomatch], "trainee_ppid" = trainees$trn_profile_person_id[trainees$trn_profile_person_id %in% nomatch], "org_name" = trainees$org2[trainees$trn_profile_person_id %in% nomatch], scopus2, SIMPLIFY = FALSE)
scopus2 <- do.call(rbind, scopus2)
write.csv(scopus2, file = "projects/trainee followup/2024_trainees_part2.csv", row.names = FALSE)
```

Then merge both sets of potential matches into a single data frame and retrieve additional information for each potential match from the scopus author retrieval API.
```r
scopus_search <- rbind(scopus1, scopus2)
rm(scopus1a, scopus2a)
scopus_retrieve <- authorRetrieve(scopus_search$authID[!is.na(scopus_search$authID)], outfile = "projects/trainee followup/search data 2024/trainee_retrieve.txt")
```

Then combine the unique information from the search and retrieve APIs into a single data frame, sort it by trainee PPID, and save the set of all possible matches.
```r
scopus_results <- merge(scopus_search[,c(1:5,15:21)], scopus_retrieve, by = "authID", all.x = TRUE)
scopus_results <- unique(scopus_results)
scopus_results <- scopus_results[order(scopus_results$trainee_ppid),]
write.csv(scopus_results, file = "projects/trainee followup/2024_trainees_all.csv", row.names = FALSE)
```

Then prepare the data for the decision algorithm. Do some additional data cleaning on trainee names to ensure accurate matching. Also see if each potential matche has at least one publication in a relevant field.
```r
scopus_results$authName2 <- paste0(scopus_results$authLast, ", ", scopus_results$authGiven)
scopus_results$authName2 <- stringi::stri_trans_general(scopus_results$authName2, "Latin-ASCII")
scopus_results$trainee_name <- stringi::stri_trans_general(scopus_results$trainee_name, "Latin-ASCII")
scopus_results$trainee_name_match <- gsub("\\.|,", "", scopus_results$trainee_name)
scopus_results$trainee_name_match <- gsub("[ -]", ".*", scopus_results$trainee_name_match)
scopus_results$scopus_name_match <- gsub("\\.|,", "", scopus_results$authName2)
scopus_results$scopus_name_match <- gsub("[ -]", ".*", scopus_results$scopus_name_match)
scopus_results$name_match <- sapply(1:nrow(scopus_results), function(x) any(grepl(scopus_results$trainee_name_match[x], scopus_results$scopus_name_match[x], ignore.case = TRUE), grepl(scopus_results$scopus_name_match[x], scopus_results$trainee_name_match[x], ignore.case = TRUE)))
scopus_results$subject_match <- sapply(1:nrow(scopus_results), function(x) any(grepl("BIOC|MEDI|SOCI|NEUR|IMMU|PSYC|PHAR|NURS", scopus_results[x,c("subjarea1", "subjarea2", "subjarea3")])))
```

Then implement the custom decision alorithm to select the most probable match for each trainee. 
```r
tmp1 <- split(scopus_results, scopus_results$trainee_ppid)
for (i in 1:length(tmp1)) {
  if (nrow(tmp1[[i]]) == 1) {
    tmp1[[i]] <- tmp1[[i]]
  }
  else if (nrow(tmp1[[i]]) > 1 && all(tmp1[[i]]$name_match == FALSE)) {
    tmp1[[i]] <- tmp1[[i]][1,]
    tmp1[[i]][,c(4:ncol(tmp1[[i]]))] <- NA
  }
  else {
    tmp1[[i]] <- tmp1[[i]][tmp1[[i]]$name_match == TRUE,]
    if (nrow(tmp1[[i]]) > 1) tmp1[[i]] <- tmp1[[i]][tmp1[[i]]$subject_match == TRUE,] ## subject areas in relevant fields
    if (nrow(tmp1[[i]]) > 1) tmp1[[i]] <- tmp1[[i]][tmp1[[i]]$pubStartYear > 1989,] ## pub start year > 1989 [?]
    if (nrow(tmp1[[i]]) > 1) tmp1[[i]] <- tmp1[[i]][which.max(tmp1[[i]]$pubCount),]
  }
}
tmp1 <- do.call(rbind, tmp1)
```

Finally, merge the original trainee data with the most probable Scopus author profile for each trainee, add a column for whether they are actively publishing, and save the resulting .csv file.
```r
trainees <- merge(trainees, tmp1, by.x = "trn_profile_person_id", by.y = "trainee_ppid", all.x = TRUE)
trainees <- trainees %>% 
  mutate(active_pubs = ifelse(pubEndYear >= 2023, "Yes", "No"))
trainees$active_pubs[is.na(trainees$active_pubs)] <- "Unknown"
trainees %>% count(active_pubs)
write.csv(trainees, file = "projects/trainee followup/2024_trainees_final.csv", row.names = FALSE)
```
