# install packages
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("stopwords")

# load required libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stopwords)

gainesData <- read.csv("GainesvilleData_clean.csv", stringsAsFactors = FALSE)
miamiData <- read.csv("MiamiData_clean.csv", stringsAsFactors = FALSE)

# lookup table to translate Miami issue_type codes into readable labels
miamiisslook <- data.frame(
  issue_type = c(
    "COMPWTT","GARCONDM","COMSWRCD","COMCECUI","COMCEPKC","COMCENCU","COMTRSHM",
    "COMCEILD","COMPWTH","COMIDPRT","COMSWCB","COMCEOGL","COMSCROSS","COMDANPU",
    "COMCEBLM","COMRCYMS","COMSGRAD","GARBAGE1","GARBCSM","COMCEBRT","COMCEILS",
    "COMTRACO","POCHGRAZ","COMFCUIN","NOISEVIO","COMSWKDM","COMPWPH","BEEWASP",
    "TRLEAN","MANHOLE","ZONCALLB","COMSWSER","COMCEILU","COMBUUST","COMSHOPP",
    "COMPWSF","ROADDEB1","COMGRAFF","COMCEILC","TREDDON","COMOTHER","COMSWRCS",
    "COMCESGN","TREEDOWN","COMTREEBR","COMINPRO","COMSQUAT","TRASH","STRUMPRE",
    "MOWING","COMCEVUS","COMCEABV","COMCEIMR","COMCETRE","COMBIC","COMTREEP",
    "TREEBLOT","COMCECCV","COMCANAL","CAVESINK","COMCEPKU","COMSWIH","TREEFELL",
    "ROADDEBR","COMTRETK","COMGRAMI"
  ),
  issue_label = c(
    "Public Works – Trash Toter Issue", "Garbage – Container Damaged",
    "Solid Waste – Recycling Not Collected",
    "Code Enforcement – Curbside/Container Issue",
    "Code Enforcement – Parking Complaint",
    "Code Enforcement – Nuisance Complaint",
    "Transportation – Trash in Street",
    "Code Enforcement – Illegal Dumping",
    "Public Works – Trash Pickup Missed (Household)",
    "Code Enforcement – Improper Debris Placement",
    "Solid Waste – Container Broken", "Code Enforcement – Overgrown Lot",
    "Traffic – Crosswalk or Signal Issue",
    "Code Enforcement – Dangerous or Unsafe Structure",
    "Code Enforcement – Building Maintenance", "Recycling – Missed Pickup",
    "Stormwater – Street Drainage", "Garbage – General Garbage Issue",
    "Garbage – Missed Household Pickup",
    "Code Enforcement – Brush/Tree Trimming Needed",
    "Code Enforcement – Illegal Sign", "Transportation – Traffic Complaint",
    "Pothole / Grazing Damage",
    "Facilities – Customer Service / Utility Inquiry", "Noise Violation",
    "Solid Waste – Yard Debris Missed", "Public Works – Pothole Repair",
    "Bee or Wasp Infestation", "Transportation – Leaning Sign",
    "Public Works – Manhole Issue", "Zoning – Call Back Requested",
    "Solid Waste – Service Complaint",
    "Code Enforcement – Illegal Use of Property",
    "Code Enforcement – Business Unlicensed",
    "Code Enforcement – Shop/Commercial Complaint",
    "Public Works – Street Flooding", "Roadway – Debris on Roadway",
    "Graffiti Removal", "Code Enforcement – Illegal Construction",
    "Tree Damage on Right-of-Way", "Other / General Service Request",
    "Solid Waste – Recycling Service Complaint",
    "Code Enforcement – Sign Violation", "Tree Down", "Tree Branch/Tree Broken",
    "Code Enforcement – Inoperative Vehicle",
    "Trespassing / Squatter Complaint", "Trash (General Request)",
    "Stump Removal Requested", "Grass or Lawn Mowing Request",
    "Code Enforcement – Vacant/Unsecure Structure",
    "Code Enforcement – Above Ground Violation",
    "Code Enforcement – Improper Maintenance",
    "Code Enforcement – Tree Complaint", "Bike Lane / Bicycle Facility Issue",
    "Tree Planting Request", "Tree Blight / Disease",
    "Code Enforcement – Commercial Vehicle Violation",
    "Canal Drainage / Waterway Issue", "Cave-in / Sinkhole",
    "Code Enforcement – Parking Violation", "Solid Waste – Illegal Hauling",
    "Tree Fell / Fallen Tree", "Road Debris", "Tree Trimming Request",
    "Graffiti – Misdemeanor"
  ),
  stringsAsFactors = FALSE
)

miamiData <- merge(miamiData, miamiisslook, by = "issue_type", all.x = TRUE)

# function to replace en-dashes with spaces
chantoSpace <- content_transformer(function(x, pattern) {
  gsub(pattern, " ", x)
})

# cleaning function
cleanText <- function(corpus_obj) {
  corpus_obj <- tm_map(corpus_obj, chantoSpace, "–")
  corpus_obj <- tm_map(corpus_obj, content_transformer(tolower))
  corpus_obj <- tm_map(corpus_obj, removePunctuation)
  corpus_obj <- tm_map(corpus_obj, removeNumbers)
  corpus_obj <- tm_map(corpus_obj, stripWhitespace)
  
# custom stopword handling
  important <- c("trash","garbage","tree","noise","debris","pothole",
                 "code","enforcement","illegal","repair","cart","container")
  base_stops <- setdiff(stopwords("en", source = "snowball"), important)
  corpus_obj <- tm_map(corpus_obj, removeWords, base_stops)
  return(corpus_obj)
}
# add margin space for labels
par(mar = c(1, 1, 2, 1))

# ------------------------------------
# Gainesville comparison cloud section

# collapse all request types/descriptions into one big string
gainesReqAll  <- paste(gainesData$request_type, collapse = " ")
gainesDescAll <- paste(gainesData$description, collapse = " ")

# create vector with two documents
gainesDocs <- c(RequestType = gainesReqAll, Description = gainesDescAll)

# make a corpus and clean it
gainesCorpus <- Corpus(VectorSource(gainesDocs))
gainesClean  <- cleanText(gainesCorpus)

# TermDocument Matrix
gainesTDM <- TermDocumentMatrix(gainesClean)
gainesM   <- as.matrix(gainesTDM)

# label columns
colnames(gainesM) <- c("Request Type", "Description")

# Gainesville comparison cloud
comparison.cloud(
  gainesM,
  max.words = 100,
  colors = c("steelblue3", "darkorange2"),
  title.size = 1.5
)

# ------------------------------
# Miami comparison cloud section

miamiTypeAll  <- paste(miamiData$issue_label, collapse = " ")
miamiDescAll  <- paste(miamiData$issue_description, collapse = " ")

miamiDocs <- c(IssueType = miamiTypeAll, Description = miamiDescAll)

miamiCorpus <- Corpus(VectorSource(miamiDocs))
miamiClean  <- cleanText(miamiCorpus)

miamiTDM <- TermDocumentMatrix(miamiClean)
miamiM   <- as.matrix(miamiTDM)

colnames(miamiM) <- c("Issue Type", "Description")

comparison.cloud(
  miamiM,
  max.words = 100,
  colors = c("firebrick2", "deepskyblue3"),
  title.size = 1.5
)