library(readxl)
#also need dplyr

jobs <- list(
  faculty = read_excel("./data-raw/ecoevojobs 22-23.xlsx", sheet = 1,
                               range = cell_rows(c(2, NA))),
  postdoc = read_excel("./data-raw/ecoevojobs 22-23.xlsx", sheet = 2,
                               range = cell_rows(c(2, NA))))
jobs <- lapply(jobs, FUN = function(x) {return(x[!is.na(x$Timestamp), ])})

#Read in Carnegie data sheets
carnegie_val <- 
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Values")
colnames(carnegie_val) <- c("Variable", "Label", "Value", "Value_Label")
carnegie_val <- carnegie_val[!is.na(carnegie_val$Value_Label), ]

fill_vals <- function(x) {
  if(is.na(x[1])) {stop("first value must not be NA")}
  for (i in 1:length(x)) {if(is.na(x[i])) {x[i] <- x[i-1]}}
  return(x)
}
carnegie_val$Variable <- fill_vals(carnegie_val$Variable)
carnegie_val$Label <- fill_vals(carnegie_val$Label)

carnegie_val <- dplyr::filter(carnegie_val,
                              !Variable %in% c("Basic2000", "BASIC2005",
                                               "BASIC2010", "BASIC2015",
                                               "BASIC2018"))
#Label R1 and R2
carnegie_val <- dplyr::mutate(
  carnegie_val,
  Value_Label = ifelse(Variable == "BASIC2021",
                       ifelse(Value == 15,
                              paste("R1", Value_Label),
                              ifelse(Value == 16,
                                     paste("R2", Value_Label),
                                     Value_Label)),
                       Value_Label))


carnegie_dat <- 
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Data")
carnegie_dat <- dplyr::select(carnegie_dat,
                              !c(basic2000, basic2005, basic2010,
                              basic2015, basic2018))

carnegie_var <-
  readxl::read_excel("./data-raw/CCIHE2021-PublicData.xlsx", sheet = "Variable")

carnegie_dat <- as.data.frame(carnegie_dat)
carnegie_val <- as.data.frame(carnegie_val)
carnegie_var <- as.data.frame(carnegie_var)

aliases <- read.csv("./data-raw/aliases.csv", fileEncoding = "UTF-8")

#Convert numeric codes into meaningful labels
for(i in 1:ncol(carnegie_dat)) {
  col <- colnames(carnegie_dat)[i]
  if(any(grepl(pattern = paste0("^", col, "$"), carnegie_val$Variable, 
               ignore.case = TRUE))) {
    temp <- carnegie_val[grep(pattern = paste0("^", col, "$"), carnegie_val$Variable, 
                              ignore.case = TRUE), ]
    carnegie_dat[, i] <- temp$Value_Label[match(carnegie_dat[, i], temp$Value)]
  }
}

#Update column names to be long-form understandable labels
colnames(carnegie_dat) <- 
  sapply(colnames(carnegie_dat),
         mytable = carnegie_var$Variable,
         mylabels = carnegie_var$Label,
         function(clnm, mytable, mylabels) {
           mylabels[grep(pattern = paste0("^", clnm, "$"), 
                         x = mytable, ignore.case = TRUE)]})

#Match institution names
uniq_inst <- unique(c(jobs[[1]]$Institution, jobs[[2]]$Institution))
find_matches <- function(jobs, carnegie, aliases) {
  #Jobs should have columns named "Institution" and "Location"
  #Carnegie should have column named "Institution name"
  jobs <- cbind(jobs, NA, NA, NA)
  colnames(jobs)[(ncol(jobs)-2):ncol(jobs)] <- 
    c("Institution name", "partial_matched", "State abbreviation")
  for(i in 1:nrow(jobs)) {
    myinst <- jobs$Institution[i]

    #Skip international institutions
    if(!jobs$Location[i] %in% c(state.name, "District of Columbia", "USA")) {
      jobs$`Institution name`[i] <- "International"
      next
    }
    #Do plain matching
    matches <- grep(paste0("^", myinst, "$"), carnegie$`Institution name`)
    if(length(matches) > 1) {
      #Match by state if there are multiple matches
      matches <-
        matches[match(carnegie$`State abbreviation`[matches], state.abb) ==
                  which(jobs$Location[i] == state.name)]
    }
    if(length(matches) == 1) {
      jobs$`Institution name`[i] <- carnegie$`Institution name`[matches]
      jobs$`State abbreviation`[i] <- carnegie$`State abbreviation`[matches]
      jobs$partial_matched[i] <- FALSE
    }
  }
  
  #For rows where plain matching failed, do partial matching
  #Manually chosen rules include:
  #   Treat "The" from beginning of Carnegie as optional
  #   Treat " at " anywhere in Carnegie as optional
  #   Carnegie uses "-" with no spaces, but ecoevo sometimes 
  #     uses ", " or " " or " - "
  #Change all "St" and "St." to be "Saint"
  jobs_inst_mod <- 
    trimws(gsub("^The |, | at |-| - ", " ",
                gsub("St |St\\. ", "Saint ", jobs$Institution)))
  carnegie_inst_mod <- 
    trimws(gsub("^The |, | at |-| - ", " ",
                gsub("St |St\\. ", "Saint ", carnegie$`Institution name`)))
  
  for(i in which(is.na(jobs$`Institution name`))) {
    myinst <- jobs_inst_mod[i]
    matches <- grep(paste0("^", myinst, "$"), carnegie_inst_mod)
    if(length(matches) > 1) {
      #Match by state if there are multiple matches
      matches <-
        matches[match(carnegie$`State abbreviation`[matches], state.abb) ==
                  which(jobs$Location[i] == state.name)]
    }
    if(length(matches) == 1) {
      jobs$`Institution name`[i] <- carnegie$`Institution name`[matches]
      jobs$`State abbreviation`[i] <- carnegie$`State abbreviation`[matches]
      jobs$partial_matched[i] <- TRUE
    }
  }

  #For rows where plain and partial matching failed, do matching using
  # manually-curated list of aliases
  unmatched_names <- unique(jobs$Institution[is.na(jobs$`Institution name`)])
  
  #original aliases.csv file created by running
  # data.frame("ecoevo_names" = unmatched_names, 
  #            "carnegie_names" = NA, "checked" = "N"))
  
  if(any(!unmatched_names %in% aliases$ecoevo_names)) {
    which_msng <- which(!unmatched_names %in% aliases$ecoevo_names)
    aliases <- 
      rbind(aliases,
            data.frame("ecoevo_names" = unmatched_names[which_msng],
                       "carnegie_names" = NA, "checked" = "N"))
    aliases <- aliases[order(aliases$checked, aliases$ecoevo_names), ]
  }

  myrows <- which(is.na(jobs$`Institution name`))
  jobs$`Institution name`[myrows] <-
    aliases$carnegie_names[match(jobs$Institution[myrows], aliases$ecoevo_names)]
  
  jobs$partial_matched[myrows][!is.na(jobs$`Institution name`[myrows])] <- TRUE

  #Return results
  return(list("jobs" = jobs, "aliases" = aliases))
}

jobs1 <- find_matches(jobs = jobs[[1]], carnegie = carnegie_dat,
                          aliases = aliases)
jobs2 <- find_matches(jobs = jobs[[2]], carnegie = carnegie_dat,
                      aliases = aliases)
                            
write.csv(x = jobs1$aliases, file = "./data-raw/aliases_new1.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(x = jobs2$aliases, file = "./data-raw/aliases_new2.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")

#Join carnegie_dat and jobs using matched institution name
jobs1 <- dplyr::left_join(x = jobs1$jobs, y = carnegie_dat)
jobs2 <- dplyr::left_join(x = jobs2$jobs, y = carnegie_dat)

write.csv(jobs1, "./data-raw/ecoevojobsR_faculty.csv", row.names = FALSE)
write.csv(jobs2, "./data-raw/ecoevojobsR_postdoc.csv", row.names = FALSE)