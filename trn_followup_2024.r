fnames <- list.files("projects/trainees", pattern = "^trainees_", full.names = TRUE)
trainees <- lapply(fnames, read.csv, stringsAsFactors = FALSE)
trainees <- do.call(rbind, trainees)
trainees <- trainees[order(trainees$TRN.FY, trainees$TRN.PROFILE.PERSON.ID),]

library(tidyverse)
mcols <- scan("branding/nichd_palette.txt", what = "varchar", sep = "\n")
trainees <- trainees %>% 
  mutate(core_project_num = paste0(TRN.ACTIVITY.CODE, gsub("-.+", "", TRN.PROJECT.NUM)),
         training_type = gsub("\\d{2}", "XX", TRN.ACTIVITY.CODE))
source("r/functions/qvr_processing.r")
trainees <- process_qvr_data(trainees, "TRN.PROFILE.PERSON.ID")
trainees <- trainees %>% 
  filter(grepl("20[01]\\d", trainees$trn_fy)) %>% ## only 63 K24 trainees, so no need to remove them
  mutate(trn_subs_rsrch_grant_code = case_when(
    grepl("G", trn_subs_rsrch_grant_code) ~ "G",
    grepl("A", trn_subs_rsrch_grant_code) ~ "A",
    grepl("N", trn_subs_rsrch_grant_code) ~ "N"
  ),
  final_trn_fy = gsub(".+;", "", trn_fy)
  )
trainees$training_type[trainees$trn_activity_code == "K12"] <- "K12"
trainees %>% count(trn_subs_rsrch_grant_code)
trainees %>% separate_rows(training_type, sep = ";") %>% count(training_type)
trainees %>% count(final_trn_fy)
trn_fys <- trainees %>% separate_rows(trn_fy, sep = ";") %>% count(trn_fy)

## attempted to search by name
plyr::count(unlist(strsplit(trainees2$trn_fy, ";")))
sum(grepl("19[89]", trainees2$trn_fy)) ## 558 / 10613 = 5%
my_ppids <- split(trainees2$trainee_name, ceiling(seq_along(trainees2$trainee_name) / 500))
my_ppids <- lapply(my_ppids, paste, collapse = "//")
writeLines(unlist(my_ppids), con = "projects/trainee followup/trainees_new.txt")
fnames <- list.files("projects/trainees", pattern = "hd_trainees_allICs", full.names = TRUE)
tmp <- lapply(fnames, read.csv, stringsAsFactors = FALSE)
tmp <- do.call(rbind, tmp)
tmp <- process_qvr_data(tmp, "TRN.PROFILE.PERSON.ID")
tmp <- tmp %>% filter(tmp$pi_profile_person_id %in% trainees2$pi_profile_person_id)
## only retrieved 1514 of the 2500 trainee names I searched for

sprintf("_%05d", 5454)
trainees <- trainees %>% mutate(org2 = clean_org_names(org_name))
trainees$affil_strings <- paste0("(AFFIL(", gsub(";", ") OR AFFIL(", trainees$org2), "))")
trainees$affil_strings[1:15]
trainees$name_string <- gsub("(.+?, .+? [A-Z])(.+)", "\\1", trainees$trainee_name)
mstrings <- paste0("AUTHFIRST(", gsub(".+, ", "", trainees$name_string), ") AND AUTHLASTNAME(", gsub(", .+", "", trainees$trainee_name), ") AND ", trainees$affil_strings)
mstrings <- gsub("\\.", "", mstrings)
mstrings <- gsub(";", "", mstrings)
mstrings[1:10]
source("r/functions/scopus_author_dev.r")
scopus1 <- lapply(1:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 3, outfile = paste0("projects/trainee followup/search data 2024/", "trainees1_", sprintf("_%05d", x), ".txt")))
mstrings[3239]
mstrings[3239] <- "AUTHFIRST(ANDREA K) AND AUTHLASTNAME(KALIS HORNTVEDT) AND (AFFIL(UNIVERSITY OF MINNESOTA))"
scopus1 <- lapply(3239:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 3, outfile = paste0("projects/trainee followup/search data 2024/", "trainees1_", sprintf("_%05d", x), ".txt")))
scopus1a <- scopus1
fnames <- list.files("projects/trainee followup/search data 2024", full.names = TRUE)
scopus1 <- lapply(fnames, extractSearchXML)
scopus1 <- mapply(cbind, "trainee_name" = trainees$trainee_name, "trainee_ppid" = trainees$trn_profile_person_id, "org_name" = trainees$org2, scopus1, SIMPLIFY = FALSE)
scopus1 <- do.call(rbind, scopus1)
write.csv(scopus1, file = "projects/trainee followup/2024_trainees_part1.csv", row.names = FALSE)

nomatch <- scopus1$trainee_ppid[which(is.na(scopus1$scopusAuthEID))]
mstrings <- paste0("AUTHFIRST(", gsub(".+, ", "", trainees$name_string[trainees$trn_profile_person_id %in% nomatch]), ") AND AUTHLASTNAME(", gsub(", .+", "", trainees$trainee_name[trainees$trn_profile_person_id %in% nomatch]), ") AND SUBJAREA(BIOC OR MEDI OR SOCI OR NEUR OR IMMU OR PSYC)")
scopus2 <- lapply(1:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 5, outfile = paste0("projects/trainee followup/search data 2024/", "trainees2_", sprintf("_%05d", x), ".txt")))
mstrings[1059]
mstrings[1059] <- "AUTHFIRST(ANDREA K) AND AUTHLASTNAME(KALIS) AND SUBJAREA(BIOC OR MEDI OR SOCI OR NEUR OR IMMU OR PSYC)"
scopus2 <- lapply(1059:length(mstrings), function(x) authorSearch(mstrings[x], retMax = 5, outfile = paste0("projects/trainee followup/search data 2024/", "trainees2_", sprintf("_%05d", x), ".txt")))
scopus2a <- scopus2
fnames <- list.files("projects/trainee followup/search data 2024", pattern = "^trainees2", full.names = TRUE)
scopus2 <- lapply(fnames, extractSearchXML)
scopus2 <- mapply(cbind, "trainee_name" = trainees$trainee_name[trainees$trn_profile_person_id %in% nomatch], "trainee_ppid" = trainees$trn_profile_person_id[trainees$trn_profile_person_id %in% nomatch], "org_name" = trainees$org2[trainees$trn_profile_person_id %in% nomatch], scopus2, SIMPLIFY = FALSE)
scopus2 <- do.call(rbind, scopus2)
write.csv(scopus2, file = "projects/trainee followup/2024_trainees_part2.csv", row.names = FALSE)

scopus_search <- rbind(scopus1, scopus2)
rm(scopus1a, scopus2a)
scopus_retrieve <- authorRetrieve(scopus_search$authID[!is.na(scopus_search$authID)], outfile = "projects/trainee followup/search data 2024/trainee_retrieve.txt")

scopus_results <- merge(scopus_search[,c(1:5,15:21)], scopus_retrieve, by = "authID", all.x = TRUE)
scopus_results <- unique(scopus_results)
scopus_results <- scopus_results[order(scopus_results$trainee_ppid),]
write.csv(scopus_results, file = "projects/trainee followup/2024_trainees_all.csv", row.names = FALSE)

scopus_results$authName2 <- paste0(scopus_results$authLast, ", ", scopus_results$authGiven)
scopus_results$authName2 <- stringi::stri_trans_general(scopus_results$authName2, "Latin-ASCII")
scopus_results$trainee_name <- stringi::stri_trans_general(scopus_results$trainee_name, "Latin-ASCII")
scopus_results$trainee_name_match <- gsub("\\.|,", "", scopus_results$trainee_name)
scopus_results$trainee_name_match <- gsub("[ -]", ".*", scopus_results$trainee_name_match)
scopus_results$scopus_name_match <- gsub("\\.|,", "", scopus_results$authName2)
scopus_results$scopus_name_match <- gsub("[ -]", ".*", scopus_results$scopus_name_match)
scopus_results$name_match <- sapply(1:nrow(scopus_results), function(x) any(grepl(scopus_results$trainee_name_match[x], scopus_results$scopus_name_match[x], ignore.case = TRUE), grepl(scopus_results$scopus_name_match[x], scopus_results$trainee_name_match[x], ignore.case = TRUE)))
scopus_results$subject_match <- sapply(1:nrow(scopus_results), function(x) any(grepl("BIOC|MEDI|SOCI|NEUR|IMMU|PSYC|PHAR|NURS", scopus_results[x,c("subjarea1", "subjarea2", "subjarea3")])))
sum(scopus_results$name_match)

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
sum(is.na(tmp1$scopusAuthEID.x)) ## not matched = 51 / 1945 = % unmatched
sum(tmp1$subject_match, na.rm = TRUE)

summary(tmp1$pubStartYear)
#program_trainees <- read.csv("projects/op planning 2024/chrcda_trainees.csv", stringsAsFactors = FALSE)
trainees <- merge(trainees, tmp1, by.x = "trn_profile_person_id", by.y = "trainee_ppid", all.x = TRUE)
trainees <- trainees %>% 
  mutate(active_pubs = ifelse(pubEndYear >= 2023, "Yes", "No"))
trainees$active_pubs[is.na(trainees$active_pubs)] <- "Unknown"
trainees %>% count(active_pubs)
write.csv(trainees, file = "projects/trainee followup/2024_trainees_final.csv", row.names = FALSE)

## PDF size: 5.21 x 10.42
trainees <- read.csv("projects/trainee followup/2024_trainees_final.csv", stringsAsFactors = FALSE)
library(tidyverse)
mcols <- scan("branding/nichd_palette.txt", what = "varchar", sep = "\n")

grants_yr_end <- trainees %>% 
  group_by(final_trn_fy, trn_subs_rsrch_grant_code) %>% 
  count() %>% 
  mutate(final_trn_fy = as.numeric(final_trn_fy), training_type = "All") %>% 
  filter(final_trn_fy > 1999, final_trn_fy < 2020) %>% 
  ungroup()
grants_yr_end$trn_subs_rsrch_grant_code <- factor(grants_yr_end$trn_subs_rsrch_grant_code, levels = c("N", "A", "G"))
p1 <- ggplot(grants_yr_end, aes(final_trn_fy, n, fill = trn_subs_rsrch_grant_code))
p1 + geom_col() + scale_fill_manual(values = mcols[3:1], labels = c("None", "Applied", "Awarded")) + 
  scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Final Training Year (FY)", y = "Number of Trainees", fill = "Subsequent\nNIH RPGs")
grants_yr_end2 <- trainees %>% 
  separate_rows(training_type, sep = ";") %>% 
  group_by(final_trn_fy, trn_subs_rsrch_grant_code, training_type) %>% 
  count() %>% 
  mutate(final_trn_fy = as.numeric(final_trn_fy)) %>% 
  filter(final_trn_fy > 1999, final_trn_fy < 2020) %>% 
  ungroup()
grants_yr_end2 <- rbind(grants_yr_end2, grants_yr_end[,c("final_trn_fy", "trn_subs_rsrch_grant_code", "training_type", "n")])
grants_yr_end2$trn_subs_rsrch_grant_code <- factor(grants_yr_end2$trn_subs_rsrch_grant_code, levels = c("N", "A", "G"))
p1 <- ggplot(grants_yr_end2[grants_yr_end2$training_type != "RXX",], aes(final_trn_fy, n, fill = trn_subs_rsrch_grant_code)) + facet_wrap(vars(training_type), scales = "free_y")
p1 + geom_col() + scale_fill_manual(values = mcols[3:1], labels = c("None", "Applied", "Awarded")) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Final Training Year (FY)", y = "Number of Trainees", fill = "Subsequent\nNIH RPGs") + 
  theme(text = element_text(size = 16))
p1 <- ggplot(grants_yr_end2[grants_yr_end2$training_type != "RXX",], aes(final_trn_fy, n, fill = trn_subs_rsrch_grant_code)) + facet_wrap(vars(training_type))
p1 + geom_col(position = "fill") + scale_fill_manual(values = mcols[3:1], labels = c("None", "Applied", "Awarded")) + 
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::label_percent()) + 
  labs(x = "Final Training Year (FY)", y = "Percent of Mechanism Trainees", fill = "Subsequent\nNIH RPGs") + 
  theme(text = element_text(size = 16))

grants_yr_multi <- trainees %>% 
  separate_rows(trn_fy, sep = ";") %>% 
  group_by(trn_fy, trn_subs_rsrch_grant_code) %>% 
  count() %>% 
  mutate(trn_fy = as.numeric(trn_fy), training_type = "All") %>% 
  filter(trn_fy > 1999, trn_fy < 2020) %>% 
  ungroup()
grants_yr_multi$trn_subs_rsrch_grant_code <- factor(grants_yr_multi$trn_subs_rsrch_grant_code, levels = c("N", "A", "G"))
p1 <- ggplot(grants_yr_multi, aes(trn_fy, n, fill = trn_subs_rsrch_grant_code))
p1 + geom_col() + scale_fill_manual(values = mcols[3:1], labels = c("None", "Applied", "Awarded")) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Final Training Year (FY)", y = "Number of Trainees", fill = "Subsequent\nNIH RPGs")
grants_yr_multi2 <- trainees %>% 
  separate_rows(training_type, sep = ";") %>% 
  separate_rows(trn_fy, sep = ";") %>% 
  group_by(trn_fy, trn_subs_rsrch_grant_code, training_type) %>% 
  count() %>% 
  mutate(trn_fy = as.numeric(trn_fy)) %>% 
  filter(trn_fy > 1999, trn_fy < 2020) %>% 
  ungroup()
grants_yr_multi2 <- rbind(grants_yr_multi2, grants_yr_multi[,c("trn_fy", "trn_subs_rsrch_grant_code", "training_type", "n")])
grants_yr_multi2$trn_subs_rsrch_grant_code <- factor(grants_yr_multi2$trn_subs_rsrch_grant_code, levels = c("N", "A", "G"))
p1 <- ggplot(grants_yr_multi2[grants_yr_multi2$training_type != "RXX",], aes(trn_fy, n, fill = trn_subs_rsrch_grant_code)) + facet_wrap(vars(training_type), scales = "free_y")
p1 + geom_col() + scale_fill_manual(values = mcols[3:1], labels = c("None", "Applied", "Awarded")) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Training Fiscal Year", y = "Number of Trainees", fill = "Subsequent\nNIH RPGs") + 
  theme(text = element_text(size = 16))
p1 <- ggplot(grants_yr_multi2[grants_yr_multi2$training_type != "RXX",], aes(trn_fy, n, fill = trn_subs_rsrch_grant_code)) + facet_wrap(vars(training_type))
p1 + geom_col(position = "fill") + scale_fill_manual(values = mcols[3:1], labels = c("None", "Applied", "Awarded")) + 
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::label_percent()) + 
  labs(x = "Training Fiscal Year", y = "Percent of Mechanism Trainees", fill = "Subsequent\nNIH RPGs") + 
  theme(text = element_text(size = 16))

pubs_yr_multi <- trainees %>% 
  separate_rows(trn_fy, sep = ";") %>% 
  group_by(trn_fy, active_pubs) %>% 
  count() %>% 
  mutate(trn_fy = as.numeric(trn_fy), training_type = "All") %>% 
  filter(trn_fy > 1999, trn_fy < 2020) %>% 
  ungroup()
pubs_yr_multi$active_pubs <- factor(pubs_yr_multi$active_pubs, levels = c("Unknown", "No", "Yes"))
p2 <- ggplot(pubs_yr_multi, aes(trn_fy, n, fill = active_pubs))
p2 + geom_col() + scale_fill_manual(values = mcols[3:1]) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Final Training Year (FY)", y = "Number of Trainees", fill = "Actively\nPublishing")
pubs_yr_multi2 <- trainees %>% 
  separate_rows(trn_fy, sep = ";") %>% 
  separate_rows(training_type, sep = ";") %>%
  group_by(trn_fy, active_pubs, training_type) %>% 
  count() %>% 
  mutate(trn_fy = as.numeric(trn_fy)) %>% 
  filter(trn_fy > 1999, trn_fy < 2020) %>% 
  ungroup()
pubs_yr_multi2 <- rbind(pubs_yr_multi2, pubs_yr_multi[,c("trn_fy", "active_pubs", "training_type", "n")])
pubs_yr_multi2$active_pubs <- factor(pubs_yr_multi2$active_pubs, levels = c("Unknown", "No", "Yes"))
p2 <- ggplot(pubs_yr_multi2[pubs_yr_multi2$training_type != "RXX",], aes(trn_fy, n, fill = active_pubs)) + facet_wrap(vars(training_type), scales = "free_y")
p2 + geom_col() + scale_fill_manual(values = mcols[3:1]) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Training Fiscal Year", y = "Number of Trainees", fill = "Actively\nPublishing") + 
  theme(text = element_text(size = 16))
p2 <- ggplot(pubs_yr_multi2[pubs_yr_multi2$training_type != "RXX",], aes(trn_fy, n, fill = active_pubs)) + facet_wrap(vars(training_type))
p2 + geom_col(position = "fill") + scale_fill_manual(values = mcols[3:1]) + 
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::label_percent()) + 
  labs(x = "Training Fiscal Year", y = "Percent of Mechanism Trainees", fill = "Actively\nPublishing") + 
  theme(text = element_text(size = 16))

pubs_yr_end <- trainees %>% 
  group_by(final_trn_fy, active_pubs) %>% 
  count() %>% 
  mutate(final_trn_fy = as.numeric(final_trn_fy), training_type = "All") %>% 
  filter(final_trn_fy > 1999, final_trn_fy < 2020) %>% 
  ungroup()
pubs_yr_end$active_pubs <- factor(pubs_yr_end$active_pubs, levels = c("Unknown", "No", "Yes"))
p2 <- ggplot(pubs_yr_end, aes(final_trn_fy, n, fill = active_pubs))
p2 + geom_col() + scale_fill_manual(values = mcols[3:1]) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Final Training Year (FY)", y = "Number of Trainees", fill = "Actively\nPublishing")
pubs_yr_end2 <- trainees %>% 
  separate_rows(training_type, sep = ";") %>%
  group_by(final_trn_fy, active_pubs, training_type) %>% 
  count() %>% 
  mutate(final_trn_fy = as.numeric(final_trn_fy)) %>% 
  filter(final_trn_fy > 1999, final_trn_fy < 2020) %>% 
  ungroup()
pubs_yr_end2 <- rbind(pubs_yr_end2, pubs_yr_end[,c("final_trn_fy", "active_pubs", "training_type", "n")])
pubs_yr_end2$active_pubs <- factor(pubs_yr_end2$active_pubs, levels = c("Unknown", "No", "Yes"))
p2 <- ggplot(pubs_yr_end2[pubs_yr_end2$training_type != "RXX",], aes(final_trn_fy, n, fill = active_pubs)) + facet_wrap(vars(training_type), scales = "free_y")
p2 + geom_col() + scale_fill_manual(values = mcols[3:1]) + 
  #scale_y_continuous(breaks = seq(0,600,100)) + 
  labs(x = "Final Training Year (FY)", y = "Number of Trainees", fill = "Actively\nPublishing") + 
  theme(text = element_text(size = 16))
p2 <- ggplot(pubs_yr_end2[pubs_yr_end2$training_type != "RXX",], aes(final_trn_fy, n, fill = active_pubs)) + facet_wrap(vars(training_type))
p2 + geom_col(position = "fill") + scale_fill_manual(values = mcols[3:1]) + 
  scale_y_continuous(breaks = seq(0,1,0.2), labels = scales::label_percent()) + 
  labs(x = "Final Training Year (FY)", y = "Percent of Mechanism Trainees", fill = "Actively\nPublishing") + 
  theme(text = element_text(size = 16))

trainees <- trainees %>% 
  mutate(affil_type = case_when(
    grepl("Hosp|(Health|Medical|Cancer) (Care|Center|System)|Clinic\\b|Healthcare|Kaiser Perm|\\bU[A-Z]{1,3} Medical", affilName) ~ "Hospital",
    grepl("Universit|School|College|Departmen|Massachusetts Institute", affilName) ~ "Academic",
    TRUE ~ "Other"
  ))
trainees$affil_type[is.na(trainees$scopusAuthEID.x)] <- "Unmatched"
trainees %>% count(affil_type)
affil_sum <- trainees %>% 
  separate_rows(training_type) %>% 
  group_by(training_type, affil_type) %>% 
  count() %>% 
  ungroup()
affil_sum_all <- trainees %>% 
  separate_rows(training_type) %>% 
  group_by(affil_type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(training_type = "All")
affil_sum <- affil_sum %>% bind_rows(affil_sum_all)
affil_sum$affil_type <- factor(affil_sum$affil_type, levels = sort(unique(affil_sum$affil_type), decreasing = TRUE))
affil_sum$training_type <- factor(affil_sum$training_type, levels = sort(unique(affil_sum$training_type), decreasing = TRUE))
p3 <- ggplot(affil_sum[affil_sum$training_type != "RXX",], aes(n, training_type, fill = affil_type))
p3 + geom_col(position = "fill") + scale_fill_manual(values = mcols[4:1], guide = guide_legend(reverse = TRUE)) + 
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::label_percent(accuracy = 1)) + 
  labs(x = "Percent of Trainees", y = "Training Type", fill = "Most Recent\nAffiliation Type") + 
  theme(text = element_text(size = 16))

measure_comp <- trainees %>% 
  separate_rows(trn_fy, sep = ";") %>% 
  separate_rows(training_type, sep = ";") %>% 
  group_by(training_type, trn_fy) %>% 
  summarise(
    num_trn = n(),
    num_grant = length(trn_profile_person_id[trn_subs_rsrch_grant_code == "G"]),
    num_pub = length(trn_profile_person_id[active_pubs == "Yes"]),
    perc_grant = num_grant / num_trn,
    perc_pub = num_pub / num_trn,
    .groups = "drop"
  ) %>% 
  mutate(trn_fy = as.numeric(trn_fy)) %>% 
  filter(trn_fy < 2020, trn_fy > 1999)
measure_comp_all <- trainees %>% 
  separate_rows(trn_fy, sep = ";") %>% 
  separate_rows(training_type, sep = ";") %>% 
  group_by(trn_fy) %>% 
  summarise(
    num_trn = n(),
    num_grant = length(trn_profile_person_id[trn_subs_rsrch_grant_code == "G"]),
    num_pub = length(trn_profile_person_id[active_pubs == "Yes"]),
    perc_grant = num_grant / num_trn,
    perc_pub = num_pub / num_trn,
    .groups = "drop"
  ) %>% 
  mutate(trn_fy = as.numeric(trn_fy), training_type = "All") %>% 
  filter(trn_fy < 2020, trn_fy > 1999)
measure_comp <- measure_comp %>% bind_rows(measure_comp_all)
measure_comp2 <- measure_comp %>% 
  select(training_type, trn_fy, perc_grant, perc_pub) %>% 
  pivot_longer(!c(training_type, trn_fy), names_to = "series", values_to = "amount")
p5 <- ggplot(measure_comp2[measure_comp2$training_type != "RXX",], aes(trn_fy, amount, color = series)) + facet_wrap(vars(training_type))
p5 + geom_line(size = 3) + scale_color_manual(values = mcols[2:1], labels = c("Subsequent\nNIH RPG\n", "Actively\nPublishing\n"), guide = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(breaks = seq(0,0.8,0.2), labels = scales::label_percent()) + 
  scale_x_continuous(minor_breaks = NULL) + 
  labs(x = "Training Fiscal Year", y = "Percent of Trainees", color = "Success\nMeasure") + 
  theme(text = element_text(size = 16))

measure_comp <- trainees %>% 
  separate_rows(training_type, sep = ";") %>% 
  group_by(training_type, final_trn_fy) %>% 
  summarise(
    num_trn = n(),
    num_grant = length(trn_profile_person_id[trn_subs_rsrch_grant_code == "G"]),
    num_pub = length(trn_profile_person_id[active_pubs == "Yes"]),
    perc_grant = num_grant / num_trn,
    perc_pub = num_pub / num_trn,
    .groups = "drop"
  ) %>% 
  mutate(final_trn_fy = as.numeric(final_trn_fy)) %>% 
  filter(final_trn_fy < 2020, final_trn_fy > 1999)
measure_comp_all <- trainees %>% 
  separate_rows(training_type, sep = ";") %>% 
  group_by(final_trn_fy) %>% 
  summarise(
    num_trn = n(),
    num_grant = length(trn_profile_person_id[trn_subs_rsrch_grant_code == "G"]),
    num_pub = length(trn_profile_person_id[active_pubs == "Yes"]),
    perc_grant = num_grant / num_trn,
    perc_pub = num_pub / num_trn,
    .groups = "drop"
  ) %>% 
  mutate(final_trn_fy = as.numeric(final_trn_fy), training_type = "All") %>% 
  filter(final_trn_fy < 2020, final_trn_fy > 1999)
measure_comp <- measure_comp %>% bind_rows(measure_comp_all)
measure_comp2 <- measure_comp %>% 
  select(training_type, final_trn_fy, perc_grant, perc_pub) %>% 
  pivot_longer(!c(training_type, final_trn_fy), names_to = "series", values_to = "amount")
p5 <- ggplot(measure_comp2[measure_comp2$training_type != "RXX",], aes(final_trn_fy, amount, color = series)) + facet_wrap(vars(training_type))
p5 + geom_line(size = 3) + scale_color_manual(values = mcols[2:1], labels = c("Subsequent\nNIH RPG\n", "Actively\nPublishing\n"), guide = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(breaks = seq(0,0.8,0.2), labels = scales::label_percent()) + 
  scale_x_continuous(minor_breaks = NULL) + 
  labs(x = "Final Training Year (FY)", y = "Percent of Trainees", color = "Success\nMeasure") + 
  theme(text = element_text(size = 16))

trn_flow <- trainees %>% 
  separate_rows(training_type) %>% 
  group_by(training_type, trn_subs_rsrch_grant_code, active_pubs) %>% 
  count() %>% 
  ungroup()
trn_flow$trn_subs_rsrch_grant_code[trn_flow$trn_subs_rsrch_grant_code == "A"] <- "Applied"
trn_flow$trn_subs_rsrch_grant_code[trn_flow$trn_subs_rsrch_grant_code == "G"] <- "Awarded"
trn_flow$trn_subs_rsrch_grant_code[trn_flow$trn_subs_rsrch_grant_code == "N"] <- "None"
trn_flow$trn_subs_rsrch_grant_code <- factor(trn_flow$trn_subs_rsrch_grant_code, levels = c("Awarded", "Applied", "None"))
trn_flow$active_pubs <- factor(trn_flow$active_pubs, levels = c("Yes", "No", "Unknown"))
library(ggalluvial)
p4 <- ggplot(trn_flow[trn_flow$training_type != "RXX",], aes(y = n, axis1 = training_type, axis2 = trn_subs_rsrch_grant_code, axis3 = active_pubs)) + geom_alluvium(aes(fill = training_type)) + geom_stratum(width = 1/3)
p4 + geom_text(stat = "stratum", aes(label = after_stat(stratum))) + scale_fill_manual(values = mcols) + 
  scale_x_discrete(limits = c("Training Type", "NIH Grant Activity", "Actively Publishing"), expand = c(.05, .05)) + 
  #scale_y_continuous(breaks = seq(0,16000,2000)) + 
  labs(y = "Number of Trainees", fill = "Training\nType") + theme_classic(base_size = 16)

trainees %>% count(trn_subs_rsrch_grant_code)
#  trn_subs_rsrch_grant_code    n
#1                         A 1407
#2                         G 2124
#3                         N 7082
trainees %>% count(active_pubs)
#  active_pubs    n
#1          No 3934
#2     Unknown 1136
#3         Yes 5543
trainees %>% separate_rows(training_type) %>% count(training_type)
# training_type     n
#<chr>         <int>
#1 FXX            1101
#2 K12             724
#3 KXX            1282
#4 LXX            1175
#5 RXX             112
#6 TXX            7216
trainees %>% separate_rows(training_type) %>% 
  group_by(training_type) %>% 
  summarise(
    num_trn = n(), 
    num_match = length(trn_profile_person_id[active_pubs != "Unknown"]),
    num_active = length(trn_profile_person_id[active_pubs == "Yes"]), 
    perc_match = num_match / num_trn,
    perc_active = num_active / num_trn, 
    .groups = "drop"
    )
# A tibble: 6 x 6
#training_type num_trn num_match num_active perc_match perc_active
#<chr>           <int>     <int>      <int>      <dbl>       <dbl>
#1 FXX              1101      1008        612      0.916       0.556
#2 K12               724       702        581      0.970       0.802
#3 KXX              1282      1245       1019      0.971       0.795
#4 LXX              1175      1113        842      0.947       0.717
#5 RXX               112        80         40      0.714       0.357
#6 TXX              7216      6287       3193      0.871       0.442
trainees %>% count(affil_type)
#affil_type    n
#1   Academic 6553
#2   Hospital 1606
#3      Other 1318
#4  Unmatched 1136
trainees %>% filter(active_pubs == "Yes") %>% count(affil_type)
#affil_type    n
#1   Academic 3842
#2   Hospital 1013
#3      Other  688
sum(grepl("United States", trainees$affilCountry))
## 9021
trainees %>% count(trn_subs_rsrch_grant_code, active_pubs)
#trn_subs_rsrch_grant_code active_pubs    n
#1                         A          No  329
#2                         A     Unknown   81
#3                         A         Yes  997
#4                         G          No  227
#5                         G     Unknown   59
#6                         G         Yes 1838
#7                         N          No 3378
#8                         N     Unknown  996
#9                         N         Yes 2708