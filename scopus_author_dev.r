authorSearch <- function(string, datatype = "application/xml", facet = NULL, sortBy = "-document-count", content = "standard", myStart = 0, retCount = 100, retMax = 500, parsed = TRUE, outfile) {
	if (!datatype %in% c("application/xml", "application/json", "application/atom+xml")) { ## error checking for valid inputs to the function
		stop("Invalid datatype. Valid types are 'application/xml', 'application/json', and 'application/atom+xml'")
	}
	else if (!content %in% c("complete", "standard")) {
		stop("Invalid content value. Valid content values are 'complete', and 'standard'")
	}
	else {
		##library(httr)
		##library(XML)
		key <- Sys.getenv("SCOPUS_API_KEY")
		print("Retrieving records.")
		theData <- list()
		if (retMax < retCount) {
			retCount <- retMax
		}
		theURL <- httr::GET("https://api.elsevier.com/content/search/author", query = list(apiKey = key, query = string, facets = facet, sort = sortBy, httpAccept = datatype, view = content, count = retCount, start = myStart))
		httr::stop_for_status(theURL)
		theData[[1]] <- httr::content(theURL, as = "text")
		newData <- XML::xmlParse(theURL) ## parse the data to extract values
		resultCount <- as.numeric(XML::xpathSApply(newData,"//opensearch:totalResults", XML::xmlValue))
		retrievedCount <- retCount 
		pageNum <- 1
		print(paste("Retrieved", retrievedCount, "of", resultCount, "records."))
		while (resultCount > retrievedCount && retrievedCount < retMax) { 
		myStart <- myStart + retCount
		pageNum <- pageNum + 1
		theURL <- httr::GET("https://api.elsevier.com/content/search/author", query = list(apiKey = key, query = string, facets = facet, sort = sortBy, httpAccept = datatype, view = content, count = retCount, start = myStart))
			theData[[pageNum]] <- httr::content(theURL, as = "text") 
			if (httr::http_error(theURL) == TRUE) { 
				print("Encountered an HTTP error. Details follow.") 
				print(httr::http_status(theURL)) 
				break 
				}
			retrievedCount <- retrievedCount + retCount 
			Sys.sleep(1)
			print(paste("Retrieved", retrievedCount, "of", resultCount, "records. Getting more."))
		} 
		print(paste("Retrieved", retrievedCount, "records. Formatting results."))
		writeLines(unlist(theData), outfile, useBytes = TRUE) 
		Sys.sleep(1)
		#theData <- readChar(outfile, file.info(outfile)$size) 
		#theData <- gsub("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "", theData, fixed = TRUE, useBytes = TRUE)
		#theData <- gsub("<search-results.+?>", "", theData, useBytes = TRUE)
		#theData <- gsub("</search-results>", "", theData, fixed = TRUE) 
		#theData <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<search-results xmlns=\"http://www.w3.org/2005/Atom\" xmlns:atom=\"http://www.w3.org/2005/Atom\" xmlns:prism=\"http://prismstandard.org/namespaces/basic/2.0/\" xmlns:opensearch=\"http://a9.com/-/spec/opensearch/1.1/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">", theData, "</search-results>", sep = "\n")
		#writeLines(theData, outfile, useBytes = TRUE)
		if (parsed == TRUE) {
			theData <- lapply(theData, extractSearchXML)
			theData <- do.call(rbind, theData)
		}
		print("Done")
		return(theData) 
	}
}

authorRetrieve <- function(authEIDs, datatype = "application/xml", content = "enhanced", parsed = TRUE, outfile) {
	if (!datatype %in% c("application/xml", "application/json", "application/atom+xml")) { ## error checking for valid inputs to the function
		stop("Invalid datatype. Valid types are 'application/xml', 'application/json', and 'application/atom+xml'")
	}
	if (!content %in% c("light", "standard", "metrics", "enhanced")) {
		stop("Invalid content value. Valid content values are 'light', 'standard', 'metrics', or 'enhanced'")
	}
	key <- Sys.getenv("SCOPUS_API_KEY")
	resultCount <- as.numeric(length(authEIDs))
	authEIDs <- gsub("9-s2.0-", "", authEIDs)
	idList <- split(as.vector(authEIDs), ceiling(seq_along(authEIDs)/25))
	idList <- lapply(idList, paste0, collapse = ",")
	print(paste("Retrieving", resultCount, "records."))
	theData <- list()
	retrievedCount <- 0
	for (i in 1:length(idList)) {
		string <- idList[i]
		theURL <- httr::GET("https://api.elsevier.com/content/author", query = list(apiKey = key, author_id = string, httpAccept = datatype, view = content))
		if (httr::http_error(theURL) == TRUE) { 
			print("Encountered an HTTP error. Details follow.") 
			print(httr::http_status(theURL)) 
			break
		}
	theData[[i]] <- httr::content(theURL, as = "text")
	Sys.sleep(1)
	retrievedCount <- retrievedCount + 25
	print(paste("Retrieved", retrievedCount, "of", resultCount, "records. Getting more."))
	}
	print(paste("Retrieved", retrievedCount, "records. Formatting results."))
	#writeLines(unlist(theData), outfile, useBytes = TRUE) 
	fout <- lapply(1:length(theData), function(x) writeLines(theData[[x]], gsub("(.+)(\\.[a-z]{3,4}$)", paste0("\\1", formatC(x, width = 2, flag = 0), "\\2"), outfile), useBytes = TRUE))
	if (parsed == TRUE & content == "enhanced") {
		theData <- lapply(theData, extractRetrieveXML)
		theData <- do.call(rbind, theData)
	}
	if (parsed == TRUE & content == "metrics") {
		theData <- lapply(theData, extractMetricsRetrieveXML)
		theData <- do.call(rbind, theData)
	}
	print("Done")
	return(theData) 
}

extractSearchXML <- function(theFile) {
	#library(XML)
	newData <- XML::xmlParse(theFile)
	records <- XML::getNodeSet(newData, "//cto:entry", namespaces = "cto")
	scopusAuthEID <- lapply(records, XML::xpathSApply, "./cto:eid", XML::xmlValue, namespaces = "cto") 
	scopusAuthEID[sapply(scopusAuthEID, is.list)] <- NA
	scopusAuthEID <- unlist(scopusAuthEID)
	authID <- gsub("9-s2.0-", "", scopusAuthEID)
	orcid <- lapply(records, XML::xpathSApply, "./cto:orcid", XML::xmlValue, namespaces = "cto") 
	orcid[sapply(orcid, is.list)] <- NA
	orcid <- unlist(orcid)
	authLast <- lapply(records, XML::xpathSApply, "./cto:preferred-name/cto:surname", XML::xmlValue, namespaces = "cto") 
	authLast[sapply(authLast, is.list)] <- NA
	authLast <- unlist(authLast)
	authGiven <- lapply(records, XML::xpathSApply, "./cto:preferred-name/cto:given-name", XML::xmlValue, namespaces = "cto") 
	authGiven[sapply(authGiven, is.list)] <- NA
	authGiven <- unlist(authGiven)
	authName <- paste(authGiven, authLast, sep = " ")
	pubCount <- lapply(records, XML::xpathSApply, "./cto:document-count", XML::xmlValue, namespaces = "cto")
	pubCount[sapply(pubCount, is.list)] <- NA
	pubCount <- unlist(pubCount)
	affilID <- lapply(records, XML::xpathSApply, "./cto:affiliation-current/cto:affiliation-id", XML::xmlValue, namespaces = "cto")
	affilID[sapply(affilID, is.list)] <- NA
	affilID <- unlist(affilID)
	affilName <- lapply(records, XML::xpathSApply, "./cto:affiliation-current/cto:affiliation-name", XML::xmlValue, namespaces = "cto")
	affilName[sapply(affilName, is.list)] <- NA
	affilName <- unlist(affilName)
	affilCity <- lapply(records, XML::xpathSApply, "./cto:affiliation-current/cto:affiliation-city", XML::xmlValue, namespaces = "cto")
	affilCity[sapply(affilCity, is.list)] <- NA
	affilCity <- unlist(affilCity)
	affilCountry <- lapply(records, XML::xpathSApply, "./cto:affiliation-current/cto:affiliation-country", XML::xmlValue, namespaces = "cto")
	affilCountry[sapply(affilCountry, is.list)] <- NA
	affilCountry <- unlist(affilCountry)
	subjarea1 <- lapply(records, XML::xpathSApply, "./cto:subject-area[1]", XML::xmlGetAttr, name = "abbrev", namespaces = "cto")
	subjarea1[sapply(subjarea1, is.list)] <- NA
	subjarea1 <- unlist(subjarea1)
	area1Pubs <- lapply(records, XML::xpathSApply, "./cto:subject-area[1]", XML::xmlGetAttr, name = "frequency", namespaces = "cto")
	area1Pubs[sapply(area1Pubs, is.list)] <- NA
	area1Pubs <- unlist(area1Pubs)
	subjarea2 <- lapply(records, XML::xpathSApply, "./cto:subject-area[2]", XML::xmlGetAttr, name = "abbrev", namespaces = "cto")
	subjarea2[sapply(subjarea2, is.list)] <- NA
	subjarea2 <- unlist(subjarea2)
	area2Pubs <- lapply(records, XML::xpathSApply, "./cto:subject-area[2]", XML::xmlGetAttr, name = "frequency", namespaces = "cto")
	area2Pubs[sapply(area2Pubs, is.list)] <- NA
	area2Pubs <- unlist(area2Pubs)
	subjarea3 <- lapply(records, XML::xpathSApply, "./cto:subject-area[3]", XML::xmlGetAttr, name = "abbrev", namespaces = "cto")
	subjarea3[sapply(subjarea3, is.list)] <- NA
	subjarea3 <- unlist(subjarea3)
	area3Pubs <- lapply(records, XML::xpathSApply, "./cto:subject-area[3]", XML::xmlGetAttr, name = "frequency", namespaces = "cto")
	area3Pubs[sapply(area3Pubs, is.list)] <- NA
	area3Pubs <- unlist(area3Pubs)
	webURL <- paste0("https://www.scopus.com/authid/detail.uri?authorId=", authID)
	theDF <- data.frame(scopusAuthEID, authID, orcid, authLast, authGiven, authName, pubCount, affilID, affilName, affilCity, affilCountry, subjarea1, area1Pubs, subjarea2, area2Pubs, subjarea3, area3Pubs, webURL, stringsAsFactors = FALSE)
	return(theDF)
}

extractRetrieveXML <- function(theFile) {
	#library(XML)
	newData <- XML::xmlParse(theFile)
	records <- XML::getNodeSet(newData, "//author-retrieval-response")
	scopusAuthEID <- lapply(records, XML::xpathSApply, ".//dc:identifier", XML::xmlValue, namespaces = c(dc = "http://purl.org/dc/elements/1.1/")) 
	scopusAuthEID[sapply(scopusAuthEID, is.list)] <- NA
	scopusAuthEID <- unlist(scopusAuthEID)
	authID <- gsub("AUTHOR_ID:", "", scopusAuthEID)
	orcid <- lapply(records, XML::xpathSApply, ".//orcid", XML::xmlValue) 
	orcid[sapply(orcid, is.list)] <- NA
	orcid <- sapply(orcid, paste, collapse = ";")
	authLast <- lapply(records, XML::xpathSApply, ".//author-profile/preferred-name/surname", XML::xmlValue) 
	authLast[sapply(authLast, is.list)] <- NA
	authLast <- sapply(authLast, paste, collapse = " ")
	authGiven <- lapply(records, XML::xpathSApply, ".//author-profile/preferred-name/given-name", XML::xmlValue) 
	authGiven[sapply(authGiven, is.list)] <- NA
	authGiven <- sapply(authGiven, paste, collapse = " ")
	authName <- paste(authGiven, authLast, sep = " ")
	affilID <- lapply(records, XML::xpathSApply, ".//affiliation-current[@id]", XML::xmlAttrs)
	affilID <- sapply(1:length(affilID), function(x) affilID[[x]][rownames(affilID[[x]]) == "id"])
	affilName <- lapply(records, XML::xpathSApply, ".//affiliation-current/affiliation/ip-doc/parent-preferred-name", XML::xmlValue)
	affilName[sapply(affilName, is.list)] <- sapply(records[sapply(affilName, is.list)], XML::xpathSApply, ".//affiliation-current/affiliation/ip-doc/preferred-name", XML::xmlValue)
	affilName[sapply(affilName, is.list)] <- NA
	affilName <- sapply(affilName, paste, collapse = ";")
	affilCity <- lapply(records, XML::xpathSApply, ".//affiliation-current/affiliation/ip-doc/address/city", XML::xmlValue)
	affilCity[sapply(affilCity, is.list)] <- NA
	affilCity <- sapply(affilCity, paste, collapse = ";")
	affilCountry <- lapply(records, XML::xpathSApply, ".//affiliation-current/affiliation/ip-doc/address/country", XML::xmlValue)
	affilCountry[sapply(affilCountry, is.list)] <- NA
	affilCountry <- sapply(affilCountry, paste, collapse = ";")
	pubCount <- lapply(records, XML::xpathSApply, ".//document-count", XML::xmlValue)
	pubCount[sapply(pubCount, is.list)] <- NA
	pubCount <- as.numeric(unlist(pubCount))
	pubYears <- lapply(records, XML::xpathSApply, ".//publication-range[@start]", XML::xmlAttrs)
	pubStartYear <- sapply(1:length(pubYears), function(x) pubYears[[x]][rownames(pubYears[[x]]) == "start"])
	pubStartYear[sapply(pubStartYear, is.list)] <- NA
	pubStartYear <- lapply(pubStartYear, as.numeric)
	pubStartYear <- lapply(pubStartYear, min)
	pubStartYear <- unlist(pubStartYear)
	pubEndYear <- sapply(1:length(pubYears), function(x) pubYears[[x]][rownames(pubYears[[x]]) == "end"])
	pubEndYear[sapply(pubEndYear, is.list)] <- NA
	pubEndYear <- lapply(pubEndYear, as.numeric)
	pubEndYear <- lapply(pubEndYear, max)
	pubEndYear <- unlist(pubEndYear)
	citedByCount <- lapply(records, XML::xpathSApply, ".//cited-by-count", XML::xmlValue)
	citedByCount[sapply(citedByCount, is.list)] <- NA
	citedByCount <- as.numeric(unlist(citedByCount))
	citationCount <- lapply(records, XML::xpathSApply, ".//citation-count", XML::xmlValue)
	citationCount[sapply(citationCount, is.list)] <- NA
	citationCount <- as.numeric(unlist(citationCount))
	hIndex <- lapply(records, XML::xpathSApply, ".//h-index", XML::xmlValue)
	hIndex[sapply(hIndex, is.list)] <- NA
	hIndex <- as.numeric(unlist(hIndex))
	coAuthorCount <- lapply(records, XML::xpathSApply, ".//coauthor-count", XML::xmlValue)
	coAuthorCount[sapply(coAuthorCount, is.list)] <- NA
	coAuthorCount <- as.numeric(unlist(coAuthorCount))
	theDF <- data.frame(scopusAuthEID, authID, orcid, authLast, authGiven, authName, affilID, affilName, affilCity, affilCountry, pubCount, pubStartYear, pubEndYear, citedByCount, citationCount, hIndex, coAuthorCount, stringsAsFactors = FALSE)
	#theDF <- list(scopusAuthEID, authID, orcid, authLast, authGiven, authName, affilID, affilName, affilCity, affilCountry, pubCount, pubStartYear, pubEndYear, citedByCount, citationCount, hIndex, coAuthorCount, stringsAsFactors = FALSE)
	return(theDF)
}

extractMetricsRetrieveXML <- function(theFile) {
	#library(XML)
	newData <- XML::xmlParse(theFile)
	records <- XML::getNodeSet(newData, "//author-retrieval-response", namespaces = "cto")
	scopusAuthEID <- lapply(records, XML::xpathSApply, ".//dc:identifier", XML::xmlValue, namespaces = c(dc = "http://purl.org/dc/elements/1.1/")) 
	scopusAuthEID[sapply(scopusAuthEID, is.list)] <- NA
	scopusAuthEID <- unlist(scopusAuthEID)
	authID <- gsub("AUTHOR_ID:", "", scopusAuthEID)
	pubCount <- lapply(records, XML::xpathSApply, ".//document-count", XML::xmlValue, namespaces = "cto")
	pubCount[sapply(pubCount, is.list)] <- NA
	pubCount <- as.numeric(unlist(pubCount))
	citedByCount <- lapply(records, XML::xpathSApply, ".//cited-by-count", XML::xmlValue, namespaces = "cto")
	citedByCount[sapply(citedByCount, is.list)] <- NA
	citedByCount <- as.numeric(unlist(citedByCount))
	citationCount <- lapply(records, XML::xpathSApply, ".//citation-count", XML::xmlValue, namespaces = "cto")
	citationCount[sapply(citationCount, is.list)] <- NA
	citationCount <- as.numeric(unlist(citationCount))
	hIndex <- lapply(records, XML::xpathSApply, ".//h-index", XML::xmlValue, namespaces = "cto")
	hIndex[sapply(hIndex, is.list)] <- NA
	hIndex <- as.numeric(unlist(hIndex))
	coAuthorCount <- lapply(records, XML::xpathSApply, ".//coauthor-count", XML::xmlValue, namespaces = "cto")
	coAuthorCount[sapply(coAuthorCount, is.list)] <- NA
	coAuthorCount <- as.numeric(unlist(coAuthorCount))
	theDF <- data.frame(authID, pubCount, citedByCount, citationCount, hIndex, coAuthorCount)
	return(theDF)
}