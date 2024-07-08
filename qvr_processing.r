process_qvr_data <- function(the_data, id_column = "Appl.Id", hd_branch = TRUE) {
	the_data <- unique(the_data)
	if (sum(duplicated(the_data[,id_column])) > 0) {
		split_data <- split(the_data, the_data[,id_column])
		tmp <- which(sapply(split_data, nrow) > 1)
		tmp2 <- lapply(tmp, function(y) colnames(split_data[[y]][sapply(1:ncol(split_data[[y]]), function(x) length(unique(split_data[[y]][,x]))) > 1]))
		dupCols <- unique(unlist(tmp2))
		the_data <- the_data[!duplicated(the_data[,id_column]),]
		the_data <- the_data[order(the_data[,id_column]),] ## split_data is ordered by id_column, so need to order the_data the same way to ensure the values match
		for (i in 1:length(dupCols)) {
		  the_data[,dupCols[i]] <- sapply(1:length(split_data), function(x) paste(unique(split_data[[x]][,dupCols[i]]), collapse = ";"))
		}
	}
	if ("Animal" %in% colnames(the_data) == TRUE) {
		the_data$is_animal <- ifelse(grepl("^10$|^35$|^9[8-9]$|^N$", the_data$Animal), "N", "Y")
		the_data$is_animal[the_data$Animal == "-"] <- "U"
	}
	if ("Human" %in% colnames(the_data) == TRUE) {
		the_data$is_human <- ifelse(grepl("^10$|^9[5-9]$|^N$", the_data$Human), "N", "Y")
		the_data$is_human[the_data$Human == "-"] <- "U"
	}
	if (all("Clinical.Trial" %in% colnames(the_data), "Phase.3.Trials" %in% colnames(the_data)) == TRUE) {
		the_data$is_trial <- sapply(1:nrow(the_data), function(x) ifelse(any(the_data$Clinical.Trial[x] %in% c("1", "Y"), the_data$Phase.3.Trials[x] %in% c("1", "Y")), "Y", "N"))  
	}
	if ("Stat.Grp" %in% colnames(the_data) == TRUE) {
		the_data$is_awarded <- ifelse(the_data$Stat.Grp %in% c("A", "TP", "U", "T"), "Y", "N")
	}
	if ("Inst.Type" %in% colnames(the_data) == TRUE) {
		the_data$inst_type_desc[the_data$Inst.Type == 10] <- "Higher education"
		the_data$inst_type_desc[the_data$Inst.Type == 20] <- "Research organization"
		the_data$inst_type_desc[the_data$Inst.Type == 30] <- "Independent hospital"
		the_data$inst_type_desc[the_data$Inst.Type == 40] <- "Other education"
		the_data$inst_type_desc[is.na(the_data$inst_type_desc)] <- "Other"
	}
	if (hd_branch == TRUE && "PCC" %in% colnames(the_data) == TRUE) {
		the_data$branch <- gsub("[ -].+", "", the_data$PCC)
		the_data$branch[the_data$branch %in% c("HLB", "CDB")] <- "CDBB"
		the_data$branch[the_data$branch %in% c("CE", "CD", "CRE", "CARE", "CRH", "CDDB")] <- "CRB"
		the_data$branch[the_data$branch %in% c("GT", "DBGT", "DBSVB")] <- "DBCAB"
		the_data$branch[the_data$branch %in% c("RS")] <- "FI"
		the_data$branch[the_data$branch %in% c("MRDD", "IDD")] <- "IDDB"
		the_data$branch[the_data$branch %in% c("PAMA")] <- "MPIDB"
		the_data$branch[the_data$branch %in% c("CP", "ARMR", "BRMR", "TSR", "BSCD", "SMAD", "BSRT", "BSRE")] <- "NCMRR"
		the_data$branch[the_data$branch %in% c("OPP")] <- "OPPTB"
		the_data$branch[the_data$branch %in% c("DBS", "EA", "OHE")] <- "PDB"
		the_data$branch[the_data$branch %in% c("NE", "ENG")] <- "PGNB"
		the_data$branch[the_data$branch %in% c("PP")] <- "PPB"
		the_data$branch[the_data$branch %in% c("PCCR")] <- "PTCIB"
	}
	colnames(the_data) <- tolower(colnames(the_data))
	colnames(the_data) <- gsub("\\.\\.*", "_", colnames(the_data))
	colnames(the_data) <- gsub("_$", "", colnames(the_data))
	the_data[the_data == "-"] <- NA
	pcols <- which(sapply(1:ncol(the_data), function(x) sum(is.na(the_data[,x]))) == nrow(the_data))
	the_data[,pcols] <- NULL
	return(the_data)
}

# Usage
# appls <- read.csv("qvr_application_data.csv", stringsAsFactors = FALSE)
# appls <- process_qvr_data(appls)


## before doing this for each file, replace the [sub] box with a space " " in notepad++ for each file
## this function also assumes there are only two columns in the abstract data frame [appl_id, abstracts], so this only works for data in that structure
clean_abstracts <- function(filepath, overwrite = FALSE) {
	the_abstracts <- scan(filepath, what = "varchar", sep = "\n", quiet = TRUE, skipNul = TRUE)
	the_abstracts <- sapply(the_abstracts, iconv, to = "ASCII", sub = " ")
	prob_lines <- which(grepl("\",\".+\",\".+", the_abstracts))
	while (length(prob_lines) > 0) {
	  the_abstracts <- gsub("(\",\".+)(\",\")(.+)", "\\1 \\3", the_abstracts)
	  prob_lines <- which(grepl("\",\".+\",\".+", the_abstracts))
	}
	the_abstracts <- gsub("(?<!\",)$", "\",", the_abstracts, perl = TRUE)
	if (overwrite == TRUE) {
	  writeLines(the_abstracts, con = filepath)
	}
	else {
	  filepath <- gsub("(\\.[a-z]{3,4}$)", "_cleaned\\1", filepath)
	  writeLines(the_abstracts, con = filepath)
	}
	message("Done")
}
## what I'm essentially doing here is reading in the csv as a vector of character strings so that the extra commas don't hurt anything
## I then look for the "," string of characters after the first column (i.e. a second occurrence of that string, which indicates an incorrect column break)
## I then gsub the second "," substring out of the main string by retaining just the substrings on either side of the problem substring
## This means I need to do that multiple times if there are multiple "," substrings in the main string, thus the 'while' loop

clean_org_names <- function(x) {
	x <- gsub("\\bUNIV\\b", "UNIVERSITY", x)
	x <- gsub("\\bHOSP\\b", "HOSPITAL", x)
	x <- gsub("\\bRES\\b", "RESEARCH", x)
	x <- gsub("\\bINST\\b", "INSTITUTE", x)
	x <- gsub("\\bMED\\b", "MEDICAL", x)
	x <- gsub("\\bBR\\b", "BRANCH", x)
	x <- gsub("\\bCOLL\\b", "COLLEGE", x)
	x <- gsub("\\bHLTH\\b", "HEALTH", x)
	x <- gsub("\\bSCIS*\\b", "SCIENCE", x)
	x <- gsub("\\bTX\\b", "TEXAS", x)
	x <- gsub("\\bCTR\\b", "CENTER", x)
	x <- gsub("\\bSCH\\b", "SCHOOL", x)
	x <- gsub("\\bCOL\\b", "COLLEGE", x)
	x <- gsub(", INC", "", x)
	#x <- gsub(";LOAN REPAYMENT APPLICATIONS$", "", x)
	x <- gsub("\\(.+\\)", "", x)
	x <- gsub(" AT ", ", ", x)
	x <- gsub("COLUMBIA UNIVERSITY HEALTH SCIENCES", "COLUMBIA UNIVERSITY", x)
	x <- gsub("COLUMBIA UNIVERSITY NEW YORK MORNINGSIDE", "COLUMBIA UNIVERSITY", x)
	x <- gsub("MOUNT SINAI SCHOOL OF MEDICINE OF CUNY", "MOUNT SINAI SCHOOL OF MEDICINE", x)
	x <- gsub("CUNY GRADUATE SCHOOL AND UNIVERSITY CENTER", "CITY UNIVERSITY OF NEW YORK", x)
	x <- gsub("TRUSTEES OF ", "", x)
	x <- gsub("NEW YORK STATE PSYCHIATRIC INSTITUTE dba RESEARCH FOUNDATION FOR MENTAL HYGIENE", "NEW YORK STATE PSYCHIATRIC INSTITUTE", x)
	x <- gsub("UNIVERSITY OF VERMONT & ST AGRIC COLLEGE", "UNIVERSITY OF VERMONT", x)
	x <- gsub("REHABILITATION INSTITUTE OF CHICAGO D/B/A SHIRLEY RYAN ABILITYLAB", "REHABILITATION INSTITUTE OF CHICAGO", x)
	x <- gsub("SANFORD RESEARCH/USD", "SANFORD RESEARCH", x)
	x <- gsub("SANFORD BURNHAM PREBYS MEDICAL DISCOVERY INSTITUTE", "SANFORD RESEARCH", x)
	x <- gsub("UNIVERSITY OF CONNECTICUT SCHOOL OF MEDICAL/DNT", "UNIVERSITY OF CONNECTICUT", x)
	x <- gsub("UNIVERSITY OF MEDICAL/DENT OF NJ-NJ MEDICAL SCHOOL", "UNIVERSITY OF MEDICINE AND DENTISTRY OF NEW JERSEY", x)
	x <- gsub("UNIVERSITY OF MEDICAL/DENT NJ-R W JOHNSON MEDICAL SCHOOL", "UNIVERSITY OF MEDICINE AND DENTISTRY OF NEW JERSEY", x)
	x <- gsub("CHILDREN'S HOSPITAL PITTSBURGH/UPMC HEALTH SYS", "CHILDREN'S HOSPITAL PITTSBURGH", x)
	x <- gsub("LUNDQUIST INSTITUTE FOR BIOMEDICAL INNOVATION, HARBOR-UCLA MEDICAL CENTER", "LUNDQUIST INSTITUTE FOR BIOMEDICAL INNOVATION", x)
	x <- gsub("U.S. NATIONAL INSTITUTE/CHILD HEALTH/HUMAN DEV", "NATIONAL INSTITUTE CHILD HEALTH HUMAN", x)
	x <- gsub("EUNICE KENNEDY SHRIVER CENTER MTL RETARDATN", "NATIONAL INSTITUTE CHILD HEALTH HUMAN", x)
	x <- gsub("VIRGINIA POLYTECHNIC INSTITUTE AND ST UNIVERSITY", "VIRGINIA POLYTECHNIC INSTITUTE", x)
	x <- gsub("MAGEE-WOMEN'S HOSPITAL OF UPMC", "MAGEE-WOMEN'S HOSPITAL", x)
	x <- gsub("BOSTON UNIVERSITY MEDICAL CAMPUS", "BOSTON UNIVERSITY", x)
	x <- gsub("RUTGERS, THE STATE UNIVERSITY OF N.J.", "RUTGERS UNIVERSITY", x)
	x <- gsub("SLOAN-KETTERING INSTITUTE CAN RESEARCH", "SLOAN-KETTERING INSTITUTE CANCER RESEARCH", x)
	x <- gsub("TEMPLE UNIVERSITY OF THE COMMONWEALTH", "TEMPLE UNIVERSITY", x)
	x <- gsub("CLEVELAND CLINIC LERNER COM-CWRU", "CLEVELAND CLINIC", x)
	x <- gsub("LSU PENNINGTON BIOMEDICAL RESEARCH CENTER", "LOUISIANA STATE UNIVERSITY", x)
	x <- gsub("UNIVERSITY OF TEXAS MD ANDERSON CAN CENTER", "UNIVERSITY OF TEXAS MD ANDERSON CANCER CENTER", x)
	x <- gsub("RBHS-ROBERT WOOD JOHNSON MEDICAL SCHOOL", "ROBERT WOOD JOHNSON MEDICAL SCHOOL", x)
	x <- gsub("HENRY M. JACKSON FDN FOR THE ADV MIL/MEDICAL", "HENRY M. JACKSON FOUNDATION", x)
}