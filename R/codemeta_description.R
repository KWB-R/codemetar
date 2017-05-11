

## Supporting old versions will be a nuciance
new_codemeta <- function(){
  ## FIXME context should be DOI
  list(`@context` = "https://raw.githubusercontent.com/codemeta/codemeta/master/codemeta.jsonld",
       `@type` = "SoftwareSourceCode")

}


# Can add to an existing codemeta document
codemeta_description <-  function(descr, id = NULL, codemeta = new_codemeta()){


  ## FIXME define an S3 class based on the codemeta list of lists?
  if(is.null(id)){
    id <- descr$Package
  }

  if(is_IRI(id)){
    codemeta$`@id` <- id
  } else {
    codemeta$identifier <- id
  }

  codemeta$title <- descr$Title
  codemeta$description <- descr$Description
  codemeta$name <- descr$Package
  codemeta$codeRepository <- descr$URL # descr$URL isn't necessarily a code repository, but crosswalk says this
  codemeta$issueTracker <- descr$BugReports

  ## According to crosswalk, codemeta$dateModified and codemeta$dateCreated are not crosswalked in R
  codemeta$datePublished <- descr$Date # probably not avaialable as descr$Date.

  ## FIXME consider parsing into a valid SPDX string?
  codemeta$licenseId <- as.character(descr$License)

  ## license is a URL in schema.org, assume SPDX ID (though not all recognized CRAN abbreviations are valid SPDX strings).
  ## FIXME need a function to map known R license strings into SPDX codes
  ## codemeta$license <- paste0("https://spdx.org/licenses/", gsub("^(\\w+).*", "\\1", as.character(descr$License)))

  codemeta$version <- descr$Version
  codemeta$programmingLanguage <-
    list("@type" = "ComputerLanguage",
         name = R.version$language,
         version = paste(R.version$major, R.version$minor, sep = "."), # According to Crosswalk, we just want numvers and not R.version.string
         url = "https://r-project.org")
  ## According to schema.org, programmingLanguage doesn't have a version; but runtimePlatform, a plain string, does.  Of course this is less computable/structured:
  codemeta$runtimePlatform <- R.version.string

  ## FIXME Need to deal with: descr$Author, descr$Maintainer, and descr$Authors@R
  if("Authors@R" %in% names(descr)){
    codemeta <- parse_people(eval(parse(text=descr$`Authors@R`)), codemeta)
  } else {
    codemeta <- parse_people(as.person(descr$Author), codemeta)
    ## maintainer must come second in case Author list also specifies maintainer by role [cre] without email
    codemeta$maintainer <- person_to_schema(as.person(descr$Maintainer))

  }

  codemeta$suggests <- parse_depends(descr$Suggests)
  codemeta$depends <- c(parse_depends(descr$Imports), parse_depends(descr$Depends))

  codemeta

}






## based on devtools::read_dcf
read_dcf <- function(pkg) {

  ## Takes path to DESCRIPTION, to package root, or the package name as an argument
  if(basename(pkg) == "DESCRIPTION"){ #so read_dcf can take a dcf file as argument, instead of just a path.
    dcf <- pkg
  } else {
    dcf <- get_file("DESCRIPTION", pkg)
  }

  fields <- colnames(read.dcf(dcf))
  as.list(read.dcf(dcf, keep.white = fields)[1, ])
}

## Like system.file, but pkg can instead be path to package root directory
get_file <- function(FILE, pkg = "."){
  f <- file.path(pkg, FILE)
  if(file.exists(f))
    f
  else {
    f <- system.file(FILE, package = pkg)
  }
}






is_IRI <- function(string){
  ## FIXME IRI can be many other things too, see https://github.com/dgerber/rfc3987 for more formal implementation
  grepl("^http[s]?://", string)
}