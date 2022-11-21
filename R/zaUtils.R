# *****************************************************************
# Common functions to make your life easier working with FADN data.
#
# Author: Daniel Hoop
# Copyright (c) Agroscope, Switzerland.
# Version: 2019-03-18
# *****************************************************************
#
# Most useful functions
# *************************
# Grouping
# group.by.quantiles / group.by.fix.scale
# group.by.wtd.quantiles (in combination with mean.weight -> to calculate weighted means of upper and lower income quartile)
#
# Calc weighted means / quantiles
# mean.weight (in combination with extract.I.vars & create.cols)
# median.weight (as wrapper for quantile.weight)
#
# Find columns / view data / clipboard copying
# find.col, find.gb.col, find.spa.col (quickly find column names with string pattern)
# read.cb, write.cb, view (read/write from clipboard. save as csv and view in Excel)
#
# Convenience functions
# slash  (convert back slashes to front slashes. Useful if copying file paths from windows to R scripts)
# categ.to.bin / categ.to.ordinal  (transform categorial variables to binary or ordinal)
# char.cols.to.num (check columns of data frame. If something is like " 23.3 " and interpreted as character -> Automatically convert to numeric )
#
# For Agroscope ZA-BH
# *************************
# Load data
# load.spe, load.gb, load.agis, load.cost (quickly load data, e.g. full costs)
# vergleichslohn, vergleichszins (quickly load data, e.g. opportunity costs)
#
# Other (translate coding and decrypt IDs)
# transl.reg, transl.typ, transl.ths, transl.lbf, transl.mm, transl.spb.330col.340row, transl.spb.320row, transl.ref.210row, transl.EB.MM
# id.entschluesseln (decrypt IDs of Referenzbetriebe)
# rekid.zaid (key between REK_ID[LINK] & ZA_ID[AGIS])
#
# -- agsGitlabUtils --
# Wenn agsGitlabUtils nicht zur Verfuegung steht, dann laut dieser Anleitung installieren: https://gitlab.agsad.admin.ch/f80823148/agsGitlabUtils/blob/master/README.md

#### Automatisches Kopieren auf Laufwerke ####

.onLoad <- function (libname, pkgname) {
  # Installing dependencies.
  try(installFromCRAN("fs"))
  if ("agsGitlabUtils" %in% .packages(TRUE)) {
    agsGitlabUtils::updatePackage("agsGitlabUtils")
  } else {
    message("zaUtils: Wenn agsGitlabUtils nicht zur Verfuegung steht, dann laut dieser Anleitung installieren: https://gitlab.agsad.admin.ch/f80823148/agsGitlabUtils/blob/master/README.md")
  }

  # Creating reference to this namespace
  thisNameSpace <- parent.env(environment())

  withTimeout(.copyZaData(), timeout = 60, onTimeout = "silent")
  ## Removing functions that are not used. ##
  rm(.mountZaDrivesOnRStudioServer, .copyZaData, .createLinkFolderAndLinks, # Don't remove these: .agsMachineType, .isZaMember, .getOS,
     .compressSource, .isFunctionDescription,
     envir = thisNameSpace)


  ## Defining functions, only if they don't already exist ##
  if(!exists("dir.exists")){
    dir.exists <- function(x) isTRUE(file.info(x)$isdir)
    assign("dir.exists", dir.exists, envir = thisNameSpace)
  }
  # vec <- c("ab^c", "bcd", NA, ""); str <- "ab^"; startsWith(vec, str); base::startsWith(vec, str)
  if (!exists("startsWith")) {
    startsWith <- function (x, prefix) {
      res <- substr(x, 1, nchar(prefix)) == prefix
      #res[is.na(res)] <- FALSE
      return (res)
    }
    assign("startsWith", startsWith, envir = thisNameSpace)
  }
  # vec <- c("abc", "bc^d", NA, ""); str <- "c^d"; endsWith(vec, str); base::endsWith(vec, str)
  if (!exists("endsWith")) {
    endsWith <- function (x, suffix) {
      nc <- nchar(x)
      res <- substr(x, nc-nchar(suffix)+1, nc) == suffix
      #res[is.na(res)] <- FALSE
      return (res)
    }
    assign("endsWith", endsWith, envir = thisNameSpace)
  }


  if (FALSE)
    .catchOldFuncUsageError <- function(error) {
      errorMsg <- error$message
      if (length(errorMsg) == 0)
        stop (error)
      errorMsg <- errorMsg[1]
      if (grepl("could not find function", errorMsg)) {
        funcName <- gsub("^.*find function \"|\"$", "", errorMsg)
        nameSpc <- find(funcName)
        if (length(nameSpc) == 1 && nameSpc == "package:zaUtils") {
          error$message <- paste0(
            "You tried to access the function `", funcName, "`, but this function is no longer an exported function of the ZA-BH namespace.",
            " You can use the function anyway if you do it like this: `zaUtils:::", funcName, "(...)`.")
          stop (error)
        }
      }
      stop (error)
    }
}

.keyValueStoreEnvir <- new.env()
.keyValueStore <- list(
  initialize = function() {
  }
  ,contains = function(key) {
    return (key %in% base::ls(envir = .keyValueStoreEnvir))
  }
  ,setAndReturn = function(key, value) {
    base::assign(key, value, envir = .keyValueStoreEnvir)
    return (value)
  }
  ,get = function(key) {
    return (base::get(key, envir = .keyValueStoreEnvir))
  }
  # The only function of .KeyValueStore that should be called is "getOrSet". All other functions are helper functions.
  ,getOrSet = function(key, value) {
    if (.keyValueStore$contains(key))
      return (.keyValueStore$get(key))
    return (.keyValueStore$setAndReturn(key, value))
  }
  ,remove = function(key) {
    if (.keyValueStore$contains(key))
      base::rm(list = key, envir = .keyValueStoreEnvir)
  }
)

# # Testing the value store.
# if (FALSE){
#   .keyValueStore$contains("b") # FALSE
#   .keyValueStore$setAndReturn("b", c(1, 2)) # c(1, 2)
#   .keyValueStore$contains("b") # TRUE
#   .keyValueStore$get("b") # c(1, 2)
#   .keyValueStore$getOrSet("a", 1+1) # should return 2
#   .keyValueStore$getOrSet("a", stop("error")) # should return 2. because of lazy evaluation -> no error.
# }


# The method .messageQueue is used to catch errors in tryCatch 'by reference', without the '<<-' operator.
# .messageQueue <- setRefClass(
#   Class="messageQueue",
#
#   fields=list(queue="character"),
#
#   methods=list(
#     # Methods to override.
#     # Initialization of the class.
#     initialize = function(queue) {
#       if (missing(queue)) {
#         .self$queue <- character()
#       } else {
#         .self$queue <- queue
#       }
#     }
#     # print() method.
#     ,show = function() {
#       print(queue)
#     }
#     # own methods
#     ,add = function(message) {
#       .self$queue <- c(queue, message)
#     }
#     ,flush = function() {
#       res <- queue
#       .self$queue <- character()
#       return (res)
#     }
#   )
# )

.onHpdaPc <- function() {
  # On Linux this would be possible: path.expand("~") == "/home/agsad.admin.ch/f80823148"
  return (.keyValueStore$getOrSet(
    ".onHpdaPc",
    grepl("^(u|a|f)80823148", Sys.info()["user"], ignore.case = TRUE)[1]
  ))
}

.dataFolder <- function() {
  return (.keyValueStore$getOrSet(
    ".dataFolder",
    (function(){

      os <- .getOS()
      if (os == "Windows")
        return( paste0("C:/Users/",Sys.info()["user"],"/_/Data/") )
      if (os == "Linux")
        return( paste0("~/data/") )
      stop ("Data folder cannot be found because the system is neither Windows nor Linux.")

    })()
  ))
}

.zaPaths <- function(localData = FALSE,
                     speDataSource = FALSE, spbDataSource = FALSE, refDataSource = FALSE, agisDataSource = FALSE, agLiDataSource = FALSE,
                     speDataCopy = FALSE, spbDataCopy = FALSE, refDataCopy = FALSE, agisDataCopy = FALSE, agLiDataCopy = FALSE,
                     dataMirrorOsLw = FALSE, dataMirrorFola = FALSE, dataNotSensitiveOsLw = FALSE, dataNotSensitiveFola = FALSE) {

  paths <- .keyValueStore$getOrSet(
    ".zaPathsData",
    local({
      os <- .getOS()
      if (os == "Windows") {
        localData <- paste0("C:/Users/",Sys.info()["user"],"/_/Data/")
      } else if (os == "Linux") {
        localData <- paste0("~/data/")
      } else {
        stop ("Data folder cannot be found because the system is neither Windows nor Linux.")
      }

      # ALT
      # dataMirrorOsLw <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/10336/")
      # dataMirrorFola <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Mirror/Data/")

      if (FALSE) { ALT
      if (.agsMachineType(fola=1)) {
        speDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Mirror/Data/SpE/")
        spbDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Mirror/Data/SpB/")
        refDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Mirror/Data/Ref/")
        agisDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Mirror/Data/AGIS/")
        agLiDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Mirror/Data/AGIS_LINK/")

        speDataCopy <- NULL
        spbDataCopy<- NULL
        refDataCopy <- NULL
        agisDataCopy <- NULL
        agLiDataCopy <- NULL

      } else {
        speDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/4276/")
        spbDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/4275/")
        refDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/4273/")
        agisDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/ZADaten/AGIS/")
        agLiDataSource <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/ZADaten/SpE/AGIS_LINK/")

        speDataCopy <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/10336/SpE/")
        spbDataCopy <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/10336/SpB/")
        refDataCopy <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/10336/Ref/")
        agisDataCopy <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/10336/AGIS/")
        agLiDataCopy <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/3/3/10336/AGIS_LINK/")
      }
      }


      dataMirrorOsLw <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11158/Sync_11158/")
      dataMirrorFola <- agsPath("O:/OSLW-SYNC/25_Agricultural_Economics/FG_UW_ZA-BH_p/Sync_11158/")
      dataNotSensitiveOsLw <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11156/DataNotSensitive/")
      dataNotSensitiveFola <- agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/Daten/Spiegel_OS-LW/")

      if (.agsMachineType(fola=1)) {
        speDataSource <-  paste0(dataNotSensitiveFola, "SpE/")
        spbDataSource <-  paste0(dataNotSensitiveFola, "SpB/")
        refDataSource <-  paste0(dataNotSensitiveFola, "Ref/")
        agisDataSource <- paste0(dataNotSensitiveFola, "AGIS/")
        agLiDataSource <- paste0(dataNotSensitiveFola, "AGIS_LINK/")

        speDataCopy <- "THERE_IS_NO_COPY_OF_DATA_ON_FOLA/"
        spbDataCopy <- "THERE_IS_NO_COPY_OF_DATA_ON_FOLA/"
        refDataCopy <- "THERE_IS_NO_COPY_OF_DATA_ON_FOLA/"
        agisDataCopy <- "THERE_IS_NO_COPY_OF_DATA_ON_FOLA/"
        agLiDataCopy <- "THERE_IS_NO_COPY_OF_DATA_ON_FOLA/"

      } else {
        speDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11156/SpE/")
        spbDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11156/SpB/")
        refDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11156/Ref/")
        agisDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11155/AGIS/")
        agLiDataSource <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11155/AGIS_LINK/")

        speDataCopy <- paste0(dataNotSensitiveOsLw, "SpE/")
        spbDataCopy <- paste0(dataNotSensitiveOsLw, "SpB/")
        refDataCopy <- paste0(dataNotSensitiveOsLw, "Ref/")
        agisDataCopy <- paste0(dataMirrorOsLw, "AGIS/")
        agLiDataCopy <- paste0(dataMirrorOsLw, "AGIS_LINK/")
      }

      li <- list(
        localData = localData,
        dataMirrorOsLw = dataMirrorOsLw,
        dataMirrorFola = dataMirrorFola,
        dataNotSensitiveOsLw = dataNotSensitiveOsLw,
        dataNotSensitiveFola = dataNotSensitiveFola,

        speDataSource = speDataSource,
        spbDataSource = spbDataSource,
        refDataSource = refDataSource,
        agisDataSource = agisDataSource,
        agLiDataSource = agLiDataSource,

        speDataCopy = speDataCopy,
        spbDataCopy = spbDataCopy,
        refDataCopyCopy = refDataCopy,
        agisDataCopy = agisDataCopy,
        agLiDataCopy = agLiDataCopy
        )

      lapply(li, function(x) if (!is.null(x) && !endsWith(x, "/")) stop("zaUtils::.zaPaths -> All paths must end with /"))
      return(li)
    }))

  if (localData)
    return(paths$localData)

  if (dataMirrorOsLw)
    return(paths$dataMirrorOsLw)
  if (dataMirrorFola)
    return(paths$dataMirrorFola)

  if (dataNotSensitiveOsLw)
    return(paths$dataNotSensitiveOsLw)
  if (dataNotSensitiveFola)
    return(paths$dataNotSensitiveFola)

  if (speDataSource)
    return(paths$speDataSource)
  if (spbDataSource)
    return(paths$spbDataSource)
  if(refDataSource)
    return(paths$refDataSource)
  if(agisDataSource)
    return(paths$agisDataSource)
  if(agLiDataSource)
    return(paths$agLiDataSource)

  if (speDataCopy)
    return(paths$speDataCopy)
  if (spbDataCopy)
    return(paths$spbDataCopy)
  if(refDataCopy)
    return(paths$refDataCopy)
  if(agisDataCopy)
    return(paths$agisDataCopy)
  if(agLiDataCopy)
    return(paths$agLiDataCopy)
}


#' Convert paths in Agroscope domain from Windows to RStudio format.
#' @export
#' @author Daniel Hoop
#' @param path The path to convert.
#' @seealso \code{\link[agsGitlabUtils:agsPath]{agsGitlabUtils::agsPath}}
#' @return The converted path.
agsPath <- function(path) {
  if ("agsGitlabUtils" %in% .packages(TRUE))
    return(agsGitlabUtils::agsPath(path))
  return(path)
}

# from <- c("C:\\asdf", "D:\\23/ccc"); to <- "E:\\a"
file.copy.readOnly <- function(from, to, overwrite=TRUE, ...){
  # Function to copy a file and set to read only on Windows.
  # Arguments see file.copy()
  if(length(from)>0){
    if (length(from)>1 && length(to)==1) {
      if (!dir.exists(to))
        stop ("The directory to copy to does not exist.")
      to <- paste0(gsub("\\\\","/",to),"/",basename(from))
    }
    if(overwrite) {
      exists <- file.exists(to)
      make.readOnly(to[exists], reverse=TRUE)
    }
    file.copy(from, to, overwrite=overwrite, ...)
    make.readOnly(to)

  } else return (FALSE)
}

make.readOnly <- function(file, reverse=FALSE) {
  if(length(file)>0){
    onWindows <- grepl("window",Sys.info()['sysname'], ignore.case=TRUE)
    if(onWindows){
      file <- winSlashes(file)
      sign <- if(reverse) "-" else "+"
      for (file1 in file)
        system(paste0("attrib ",sign,"R \"",file1,"\""), intern=FALSE)
    } else stop("The function currently works only on Windows operating systems.")

  } else return (FALSE)
}

.getOS <- function () {
  return (.keyValueStore$getOrSet(
    ".getOS",
    local({
      os <- Sys.info()["sysname"] # Sys.getenv(c("OS", "R_PLATFORM"))
      onWin <- any(grepl("windows", os, ignore.case=TRUE))
      onLin <- any(grepl("linux", os, ignore.case=TRUE))
      if (onWin && onLin)
        stop ("Windows and Linux detected. This is not possible.")
      if (onWin)
        return ("Windows")
      if (onLin)
        return ("Linux")
      stop ("Neither Windows nor Linux detected.")
    })
  ))
}


#' Show on which Agroscope machine type the R session operates.
#' @keywords internal
#' @author Daniel Hoop
#' @details All parameters are optional. If one is set to TRUE, then only one logical value is returned. Else a logical vector (see Value).
#' @return A logical vector with names \code{'rstudioserver'}, \code{'laborpc'}, \code{'fola'}, \code{'bitpc'} and \code{'other'}.
#' \code{'fola'} is TRUE, when the username starts with an 'f'. A 'laborpc' can be in FOLA or outside FOLA. 'rstudioserver' is always in FOLA.
#' @examples
#' if (.agsMachineType()["bitpc"]) print("on BIT client") else print ("on other machine.")
#' if (.agsMachineType(fola=1)) print("Inside FOLA.")
.agsMachineType <- function (fola=FALSE, rstudioserver=FALSE, laborpc=FALSE, bitpc=FALSE, other=FALSE) {
  # Determines whether the user is on personal machine or on RStudio-Server

  info <- Sys.info()
  type <- c(fola=FALSE, rstudioserver=FALSE, laborpc=FALSE, bitpc=FALSE, other=FALSE)

  if (grepl("^f[0-9]", info["user"], ignore.case=TRUE)) {
    type["fola"] <- TRUE
    if (grepl("linux", info["sysname"], ignore.case=TRUE)) {
      type["rstudioserver"] <- TRUE
    } else {
      type["laborpc"] <- TRUE
    }
  }
  if (grepl("^a[0-9]", info["user"], ignore.case=TRUE)) {
    type["laborpc"] <- TRUE
  }
  if (grepl("^u[0-9]", info["user"], ignore.case=TRUE) && grepl("window", info["sysname"], ignore.case=TRUE)) {
    type["bitpc"] <- TRUE
  }
  if (sum(unlist(type)) == 0) {
    type["other"] <- TRUE
  }

  if (fola)
    return(type["fola"])
  if (rstudioserver)
    return(type["rstudioserver"])
  if (laborpc)
    return(type["laborpc"])
  if (bitpc)
    return(type["bitpc"])
  if (other)
    return(type["other"])
  return(type)
}


#' Show on which Agroscope machine type the R session operates.
#' @author Daniel Hoop
#' @export
#' @details All parameters are optional. If one is set to TRUE, then only one logical value is returned. Else a logical vector (see Value).
#' @return A logical vector with names \code{'rstudioserver'}, \code{'laborpc'}, \code{'fola'}, \code{'bitpc'} and \code{'other'}.
#' \code{'fola'} is TRUE, when the username starts with an 'f'. A 'laborpc' can be in FOLA or outside FOLA. 'rstudioserver' is always in FOLA.
#' @examples
#' if (agsMachineType()["bitpc"]) print("on BIT client") else print ("on other machine.")
#' if (agsMachineType(fola=1)) print("Inside FOLA.")
agsMachineType <- .agsMachineType


.isZaMember <- function() {
  return (.keyValueStore$getOrSet(
    ".isZaMember",
    dir.exists(agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000")) ||
      dir.exists(agsPath("O:/Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected/ZADaten"))
  ))
}

.mountZaDrivesOnRStudioServer <- function (testDir) {
  # Not needed anymore because ZAMAIN is now accessible via '~/mnt/Data-Work-RE'.
  # In case that would be necessary in the future, the line below would probably mount correctly.
  # <volume options="nosuid,nodev,noserverino,nobrl,vers=1.0" fstype="cifs" server="ags-vzhrec-1034.evdad.admin.ch" path="Data-Work/25_Agricultural_Economics-RE/251_ZA-BH_protected" mountpoint="~/mnt/ZAMAIN" />
  return (NULL)

  # if (!dir.exists(testDir)) {
  #   message("Mounting ZA-BH specific network drives.")
  #   from <- "/home/agsad.admin.ch/f80823148/pam_mount.conf.xml"
  #   to <- "~"
  #   if (file.copy(from, to, overwrite=TRUE)) {
  #     message("*** Success! *** PAM configuration file was copied.")
  #     stop ("*** Bitte RStudio-Server-Session neu starten. ***\nClick the button called 'Sessions' (upper-right corner), then click on the red round 'power-off' symbol (right to the session name).")
  #   } else {
  #     warning ("You seem to meet the ZA-BH requirements, but the pam configuration file could not be copied to your RStudio folder.")
  #   }
  # }
}
.createLinkFolderAndLinks <- function() {
  rstudioLinkFolder <- "~/_links"
  if(!dir.exists(rstudioLinkFolder)) {
    dir.create(rstudioLinkFolder)

    a <- system(paste0("chmod 700 ", rstudioLinkFolder))
    if (a != 0) {
      file.remove(rstudioLinkFolder)
      stop (paste0("Fehler! chmod 700 auf '", rstudioLinkFolder, "' war nicht erfolgreich! *** BITTE HPDA INFORMIEREN ***"))
    }

    lines <- suppressWarnings(readLines("/home/agsad.admin.ch/f80823148/_links/createLinks.sh"))
    if (length(lines) > 0) {
      # Remove the lines that links hpda R folder.
      killNoOfLines <- 5
      killLines <- as.list(grep("# hpda", lines))
      killLines <- unlist(lapply(killLines, function(x) x:(x+killNoOfLines-1)))

      # Write the shell skript and execute it in order to create the links.
      rstudioLinkFile <- paste0(rstudioLinkFolder, "/createLinks.sh")
      write(lines[-killLines], rstudioLinkFile)
      system(paste0("sh ", rstudioLinkFile))
    } else {
      message("The links on the RStudio server could not be created. Probably no access to hpda's \"createLinks.sh\" template.")
    }
  }
}

.copyZaData <- function(){
  # This function copies za data from the network drives to the hard drive of a computer. If all files are up to date, nothing is done.
  # load.spe(), load.spb(), etc. will then access the files on the hard drive instead of the network drives.
  # This function is called each time when func.R is sourced. This way it will be guaranteed that all data files on the hard drive are always up to date!


  datFold <- .zaPaths(localData=1)

  # Make some things on the RStudio Server
  if (.agsMachineType(rstudioserver=1) && .isZaMember()) {
    # Not needed anymore: Mount network drives, if they are not yet mounted.
    # .mountZaDrivesOnRStudioServer(zamain)

    # Make link folder.
    .createLinkFolderAndLinks()
  }

  if(dir.exists(.zaPaths(speDataSource=1))){

    # Create personal data folders.
    if(!dir.exists(datFold)){
      message("Creating folder ", datFold, sep="")
      dir.create(datFold, recursive=TRUE)
      # If on RStudio Server, it is important to give only the user read/write/execute permissions. Otherwise the data can be read by anyone.
      if (dir.exists(datFold) && .agsMachineType(rstudioserver=1)) {
        a <- system(paste0("chmod 700 ", datFold))
        if (a != 0) {
          file.remove(datFold)
          stop (paste0("Fehler! chmod 700 auf '", datFold, "' war nicht erfolgreich! *** BITTE HPDA INFORMIEREN ***"))
        }
      }
    }

    if(!dir.exists(paste0(datFold,"AGIS/"))){
      message("Creating folder ", paste0(datFold,"AGIS/"), sep="")
      dir.create(paste0(datFold,"AGIS/"), recursive=TRUE)
    }

    # Specify files to be copied
    # First AGIS
    agisYears <- .availableAgisYears()
    agisPaths <- character()
    if (length(agisYears) > 0) {
      agisPaths <- c.1b1(
        paste0(.zaPaths(localData = 1), "AGIS/AGIS_BFS_", agisYears, ".RData"), # to
        paste0(.zaPaths(agisDataSource=1), agisYears, "/AGIS_BFS_", agisYears, ".RData")) # from
    } else {
      message("The AGIS data cannot be found on the network drives.")
    }

    # Specify remaining files to copy to local harddrive.
    files <- matrix(c(
      agisPaths,
      paste0(datFold,"GB.RData"), paste0(.zaPaths(refDataSource=1),"GB/GB.RData"),
      paste0(datFold,"SpE.RData"), paste0(.zaPaths(speDataSource=1),"alldata/SpE.RData"),
      paste0(datFold,"SpB.RData"), paste0(.zaPaths(spbDataSource=1),"alldata/SpB.RData"),
      paste0(datFold,"SpE_GB_Einzel.RData"), paste0(.zaPaths(speDataSource=1),"GB_Einzel/SpE_GB_Einzel.RData"),
      paste0(datFold,"SpB_GB_Einzel.RData"), paste0(.zaPaths(spbDataSource=1),"GB_Einzel/SpB_GB_Einzel.RData"),
      paste0(datFold,"SpE_Personen_Indexiert.RData"), paste0(.zaPaths(speDataSource=1),"Personen/SpE_Personen_Indexiert.RData"),
      paste0(datFold,"SpB_Personen_Indexiert.RData"), paste0(.zaPaths(spbDataSource=1),"Personen/SpB_Personen_Indexiert.RData")
    ),ncol=2, byrow=TRUE)

    if ("fs" %in% rownames(installed.packages())) {
      copyFunc <- file.copy
    } else {
      copyFunc <- base::file.copy
    }
    # Copy all files.
    invisible(apply(files,1,function(x){ # x <- files[1,]
      if (file.exists(x[2])) {
        if (!file.exists(x[1]) || file.info(x[1])$mtime < file.info(x[2])$mtime) {
          if (!"fs" %in% rownames(installed.packages())) {
            message(paste0(c(
              "************************************************************",
              "ZA-BH-Daten werden auf die Festplatte kopiert.",
              "Wenn es schneller gehen soll, das package 'fs' installieren.",
              "install.packages('fs')",
              "************************************************************"),
              collapse="\n"))
          }
          if (file.exists(x[1]))
            file.remove(x[1])
          if (!dir.exists(dirname(x[1])))
            dir.create(dirname(x[1]), recursive = TRUE)
          message("Copying file '",x[2], "' to '", x[1], "'. Success=", sep="")
          hasWorked <- copyFunc(x[2], x[1], overwrite=TRUE)
          message(hasWorked)

        }
      } else {
        message("Tried to copy file '", x[2], "' to local hard drive, but could not find it on network drive.")
      }
    }))

  }
}

.compressSource <- function (fileIn, fileOut, leaveStartingComments=TRUE) {
  # This function reads source code in a file "fileIn", compresses it, and saves it to a file "fileOut".

  if (length(fileIn)!=length(fileOut))
    stop ("length(fileIn) must be equal length(fileOut).")
  if (length(fileIn)>1)
    return (apply(cbind(fileIn,fileOut), 1, function(x).compressSource(fileIn=x[1],fileOut=x[2])) )
  if (fileIn==fileOut)
    stop ("fileIn must not be equal to fileOut.")
  replCode <- matrix(c(
    " *|#.*",""
    ," *<\\- *","="
  ),ncol=2,byrow=TRUE); colnames(replCode) <- c("s","r")
  replAll <- matrix(c(
    " *$", ""
  ),ncol=2,byrow=TRUE); colnames(replAll) <- c("s","r")

  # Read in lines
  x <- suppressWarnings(readLines(fileIn))
  # Mark Function comment lines
  x[gsub("^ ","",x)==""] <- ""
  x <- gsub("^ *","",x)
  code <- !.isFunctionDescription(x, leaveStartingComments=leaveStartingComments) & !(grepl("\"",x,fixed=TRUE) | grepl("'",x,fixed=TRUE) | grepl("for *\\(",x) | grepl(" else ",x,fixed=TRUE))
  # Compress coded lines & Kick empty ones or code comments.
  for(i in 1:nrow(replCode))
    x[code] <- gsub(replCode[i,"s"], replCode[i,"r"], x[code])
  x <- x[!code | (code & x!="")]# & !startsWith(x,"#"))]
  # Now compress all lines, independent if code or not.
  for(i in 1:nrow(replAll))
    x <- gsub(replAll[i,"s"], replAll[i,"r"], x)

  write.table(x, fileOut, col.names=FALSE, row.names=FALSE, quote=FALSE)
  return (TRUE)
}

#x <- c("\\*\\*\\* Tutorial \\*\\*\\*","asdf","*x*x* Tutorial *x*x*", "adsf", "asdfwaef")
#x <- c("a","b",gsub("x","","DxONT_COMPRESS_SOURCE_STARxT"), gsub("x","","DxONT_COMPRESS_SOURCE_STARxT"), "c", "d", gsub("x","","DxONT_COMPRESS_SOURCE_ENxD"), gsub("x","","DxONT_COMPRESS_SOURCE_ENxD"))
#x <- c("# startcomment", "# another", "", "x <- function(){", "# func desc 1", "", "# func desc 2", "", "a <- 1", "# delete comment")
#x[.isFunctionDescription(x, leaveStartingComments=TRUE)]
.isFunctionDescription <- function(x, leaveStartingComments=TRUE, dontCompressStart="DONT_COMPRESS_SOURCE_START", dontCompressEnd="DONT_COMPRESS_SOURCE_END") {
  # This function tests if "#"-Commented lines in a function follow directly to the function description signature.
  # If so, then these lines are marked as function description comments. A logical vector is returned.

  tutorialString <- gsub("x","","*x*x* Tutorial *x*x*")
  isTutorial <- grepl(tutorialString, x, fixed=TRUE)
  if (sum(isTutorial)>0) {
    isTutorial[max(which(isTutorial)):length(isTutorial)] <- TRUE
    if (length(x)>200 && sum(isTutorial)/length(x)>0.1)
      warning (paste0("More than 10% of the code was classified as tutorial. Please check the source file for multiple occurences of : ",tutorialString) )
  }
  # dcs <- gsub("x","","DxONT_COMPRESS_SOURCE_STARxT")
  # dce <- gsub("x","","DxONT_COMPRESS_SOURCE_ENxD")
  # isDc <- logical(length(isTutorial))
  # if (length(grep(dcs, x, fixed=TRUE))>1) {
  #   for(i in 2:length(x)) # i <- 4
  #     isDc[i] <- (isDc[i-1] || grepl(x[i-1],dcs,fixed=TRUE)) & !grepl(x[i-1],dce,fixed=TRUE)
  # }
  # First regex is for function description. Second/third is for if/else without curly brackets.
  rgx <- paste0(c("(<\\-|=) *function\\(",
                  "[a-zA-Z0-9] +else",
                  "else +[a-zA-Z0-9]"),
                collapse="|")
  isDesc <- grepl(rgx,x) | isTutorial #| isDc
  hasHash <- grepl(" *#", x)
  if (leaveStartingComments && (grepl(" *#", x[1]) || grepl(" *\n?", x[1])))
    isDesc[1] <- TRUE
  for (i in 2:length(x)) { # i <- 3
    isDesc[i] <- isDesc[i] ||
      (hasHash[i] && (isDesc[i-1])) || # || grepl("function *\\(",x[i-1]))) ||
      (x[i]=="" && isDesc[i-1] && hasHash[i-1])
  }
  return (isDesc)
}


#### Options ####

# Optionen nur bei mir selbst einlesen. Nicht auf anderen Computern (falls Script-Ausfuehrung ueber Laufwerk W:)
if(FALSE && .onHpdaPc())  {
  options(scipen = 3) # mit scipen = 3 geht die Digits-Anzeige bis 0.000001 (also 1e-06). Ab 1e-07 in scientific notation.
  options(help.try.all.packages=TRUE)
  #options(prompt="    ")
  options(stringsAsFactors=FALSE)
  #options(max.print=1000)
  #options(na="")
  message("**********************************************************************")
  message("Options set.")
}

#### GRAPHICS ####

show.pch <- function(show=1:255,mfrow=c(5,5),mar=c(4,1,1,3)){
  # Show what the pch numbers mean (in a graph).
  mar.orig <- par()$mar
  mfrow.orig <- par()$mfrow
  on.exit(
    par(mar=mar.orig, mfrow=mfrow.orig))
  par(mar=mar, mfrow=mfrow)
  for(i in show)
    suppressWarnings( plot(1,pch=i,xlab=i) )
}

####

show.colors <- function(pattern, pick=NULL){
  # This function plots all colors that contain a certain pattern in their name. e.g. pattern='orang' will show all orange colors.
  # Argument 'pick' only returns the chosen colors in a character vector. Otherwise all colors are returned.
  c1 <- colors()[grep(pattern,colors())]
  l1 <- length(c1)
  names(c1) <- 1:l1
  plot(x=1:(l1+1), y=rep(1,l1+1), type="n", ylim=c(1,2), yaxt="n", xaxt="n", xlab="", ylab=paste0("Colors containing '", pattern ,"'"))
  rect(xleft=1:l1, ybottom=rep(1,l1), xright=(1+0.9):(l1+0.9), ytop=rep(2,l1), col=c1)
  at <- (1+0.45):(l1+0.45)
  axis(side=1, labels=c1, at=at, las=2)
  axis(side=3, labels=1:l1, at=at, las=2)
  if(is.null(pick)) return(c1) else return(c1[pick[pick>0 & pick<=length(c1)]])
}
####

color.check <- function(){
  #cols <- c("#FF0000","#00FF00","#0000FF","#FFFF00","#FF00FF","#00FFFF","#FFFFFF")
  cols <- c("#FFFFFF","#FFFF00","#0000FF","#FF0000","#FF00FF","#00FF00","#00FFFF")
  plot(  1:length(cols),rep(1,length(cols)),col=cols,    pch=20, cex=5, xaxt="n")
  points(1:length(cols),rep(1,length(cols)),col="black", pch=21, cex=4)
  #
  points(1:length(cols),rep(1.1,length(cols)),col="black", pch=20, cex=5, xaxt="n")
  points(1:length(cols),rep(1.1,length(cols)),col="black", pch=21, cex=4)
  points(1:length(cols),rep(0.9,length(cols)),col="white", pch=20, cex=5, xaxt="n")
  points(1:length(cols),rep(0.9,length(cols)),col="black", pch=21, cex=4)
  #
  axis(1, at=1:length(cols),labels=cols)
}

####

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
if(FALSE){
  #For example:
  n = 10
  cols = gg_color_hue(4)
  #dev.new(width=4, height=4)
  plot(1:n, pch=16, cex=3, col=cols)
}

####

cornerlegend <- function(v=c("o","m","u")[3],h=c("l","m","r")[3],ply=0,plx=0, ...){
  # This function places the legend in the specified corner of the graph. Give all agruments needed to produce the legend in the ,... argument.
  # v= vertical orientation, h= horizontal orientation. In german! "o","r" means for example: oben-rechts (upper right).
  # ply: plus y coord., plx: plus x coord.
  # Alternative: Try legend() and simply write: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"

  yxlim <- par("usr")
  if(v=="o") {yjust <- 1; y <- yxlim[4];} else if(v=="m") {yjust <- 0.5; y <- mean(c(yxlim[3],yxlim[4]));} else if(v=="u") {yjust <- 0; y <- yxlim[3]+ply;}
  if(h=="l") {xjust <- 0; x <- yxlim[1];} else if(h=="m") {xjust <- 0.5; x <- mean(c(yxlim[1],yxlim[2]));} else if(h=="r") {xjust <- 1; x <- yxlim[2]+plx;}
  legend(x=x,y=y,xjust=xjust,yjust=yjust, ...)
}

####

curveChar <- function(expr, ...) {
  # curve() for character input instead of expression
  # Try this: expr <- "100*(1-exp(-0.025*(40+x)))*(1-exp(-0.0045*(200+400)))"; curveChar(expr, from=1, to=160)
  curve((function(x) eval(parse(text=expr)))(x), ...)
}

linreg.plot <- function(form,data,method=c("lm","rlm"),...){
  # see also curve()

  method <- match.arg(method)
  if(method=="lm") {
    reg <- lm(form, data=data)
    coefs <- reg$coefficient
  } else if(method=="rlm") {
    require.package("MASS")
    reg <- MASS::rlm(form, data=data)
    coefs <- reg$coefficients
  }
  abline(coefs[1],coefs[2],...)
  invisible(summary(reg)$coefficient)
}
####

parcoord.sorted <- function(data, orderByCorrelation=TRUE, colorGradient=TRUE, gradientColors=c("red","green","blue"), col=NULL, alpha=0.4, ...){
  # This function makes a parallel coordinates plot and sorts it according to the correlations between the variables.
  #
  # Arguments
  # orderByCorrelation: Logical value indicating if the variables should ordered by correlation. Like this, patterns can be detected more easily.
  #                     Variables correlating negatively with the first variable (in data) will be placed left to this variable. Those with positive correlation will be right to the first variable.
  # colorGradient:      Logical value indicating if a color gradient should be introduced.
  # gradientColors:     Character. The colors along which the gradient should be created.
  # col:                Character. Of length 1, or length(col)==nrow(data). Specifying the exact color for each observation.
  # alpha:              The transparency value as in rgb(..., alpha=...)

  if(orderByCorrelation){
    cors <- cor(data)[,1]
    negCors <- sort( cors[cors< 0], decreasing=TRUE)
    posCors <- sort( cors[cors>=0], decreasing=TRUE)
    #cors <- sort(cors, decreasing=TRUE)
    plotVars <- c(names(negCors),names(posCors))
  } else {
    plotVars <- colnames(data)
  }

  if(colorGradient & !is.null(col)) stop("Either specifiy colorGradient==TRUE or col. colorGradient==TRUE and !is.null(col) does not work.")

  if(colorGradient){
    if(length(gradientColors)==0) stop("length(gradientColors) must be greater than 0.")
    x <- data[,cvars[1]]
    col2 <- colorRampPalette(gradientColors) (100) [ findInterval(x, seq(min(x),max(x), length.out=100)) ] # color.gradient(data[,cvars[1]], colors)
  } else if(!is.null(col)) {
    if( !length(col)%in%c(1,nrow(data)) ) stop("length(col) must be equal to 1 or nrow(data).")
    col2 <- if(length(col)==1) "black" else col
  }
  col2 <- rgb( t(col2rgb(col2, alpha=FALSE)/255 ), alpha=alpha)

  MASS::parcoord(data[,plotVars], col=col2, ...)
}

####

radarchart.tutorial <- function() {
  # Radar chart / spider chart / Spinnendiagramm
  dat <- as.data.frame(matrix(sample(1:9,9), ncol=3))
  # Data must contain max and min values in the top 2 rows.
  radarData <- rbind(sapply(dat,range)[2:1,,drop=FALSE],
                     dat)
  # Line color
  pcol <- adjustcolor(rainbow(nrow(radarData)-2), alpha=0.5)
  # Filling collor
  pfcol <- adjustcolor(pcol, alpha=0.2)
  # Function
  fmsb::radarchart(
    radarData, title="Radarchart",
    # Settings for lines
    pcol=pcol, pfcol=pfcol,
    plwd=1, plty=1,
    # Settings for axis
    cglcol="grey", cglty=1, cglwd=0.8, axislabcol="black", axistype=2,
    # Settings for points
    vlcex=0.8, palcex=0.8 # use pty=32 to disable points.
  )
}

####

if(FALSE){
  pairs.smooth(cor.data, main="Zusammenhang zw. SDB u. Sampling Rate AG(SO)")
  pch=20; cors=c("no","upper","lower")[2]; abline01=TRUE; pointscol="black"; smoothcol="red"; ablinecol="chartreuse4"; digits=2
  x <- cor.data
}
pairs.smooth <- function(x, pch=20, cors=c("no","upper","lower"), cor.method=c("pearson", "kendall", "spearman"), abline01=TRUE, pointscol="black", smoothcol="red", ablinecol="chartreuse4", digits=2, ...){
  # Try also lattice::xyplot()
  cors <- match.arg(cors)
  cor.method <- match.arg(cor.method)
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round( cor(x, y, method=cor.method), digits=digits) # abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 2#0.5/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor)# * r)
  }
  panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="gray")
  }
  panel.smooth <- function (x, y, col = smoothcol, bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
    points(x, y, pch = pch, col = pointscol, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
      if(abline01) {
        abline(0,1,col=ablinecol)
        mx <- mean(x)
        text(mx, mx, "y=x", col=ablinecol, adj=1.1)
      }
    }
  }
  if(cors=="no"){
    pairs(x,pch=pch, lower.panel=panel.smooth, upper.panel=panel.smooth, diag.panel=panel.hist, ...)
  } else if(cors=="upper"){
    pairs(x,pch=pch, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, ...)
  } else {
    pairs(x,pch=pch, lower.panel=panel.cor, upper.panel=panel.smooth, diag.panel=panel.hist, ...)
  }
}

####

boxplot.multi <- function(data,grouping=NULL,meansd=FALSE,notch=TRUE,mark.notch=NULL,ovrl.median=TRUE,main=NULL,plotrows=3,split=1,mar=c(2,2.6,2,0.4),newwin=FALSE, info=FALSE, ...){
  # Make boxplots for several columns of a data.frame or matrix
  # Attention: The colnames of the data must not contain " ". Fill with "_"

  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))

  if(is.null(dim(data))) {data <- matrix(data,ncol=1); colnames(data) <- "check_variable"}
  if(ncol(data)==1) {
    plotrows <- 1
    if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
  }
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  if(!is.null(mark.notch)) if(!mark.notch%in%unique(grouping)) stop("mark.notch must be one of ",paste(sort(unique(grouping)),collapse=", "))
  if(is.null(colnames(data))) stop("the data must have colnames")
  data.orig <- data

  choose <- list()
  for(s in 1:split) {
    if(s==1) {choose[[s]] <- 1:floor(ncol(data.orig)/split)
    } else {
      if(s<split) { choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : (s*floor(ncol(data.orig)/split))
      } else choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : ncol(data.orig)
    }
    data <- data.orig[,choose[[s]],drop=FALSE]

    if(is.null(main)) mains <- colnames(data) else if(length(main)==1) mains <- rep(main,ncol(data)) else mains <- main[choose[[s]]]
    nvariables <- ncol(data)
    data <- cbind(data,grouping=grouping)
    if(newwin) windows()
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else ceiling(nvariables/plotrows) ))
    for(j in 1:nvariables) {
      if(!meansd) {
        box <- boxplot( formula=as.formula(paste(colnames(data)[j],"~ grouping")) , data=data , main=mains[j], notch=notch, ...)
        if(ovrl.median) {
          if(is.null(mark.notch)) abline(a=median(data[,j]), b=0)
          if(!is.null(mark.notch)) abline(a=median(data[,j]), b=0, lwd=2)
        }
        if(!is.null(mark.notch)) {
          abline(a=box$conf[1,mark.notch], b=0)
          abline(a=box$conf[2,mark.notch], b=0)
        }
      } else {
        box.meansd( data[,j],grouping, main=mains[j], ...)
        if(ovrl.median) abline(a=mean(data[,j],na.rm=TRUE), b=0)
      }
    }
  }
  if(info) message("If the notches of different groups do not overlap this is 'strong evidence' that the two medians differ (Chambers et al., 1983, p. 62). From help(boxplot)")
}
#boxplot.multi(aaa,grouping=c(rep(1,floor(nrow(aaa)/2)),rep(2,ceiling(nrow(aaa)/2))))

####

boxplot.probabilities <- function(y=NULL, grouping=NULL, list=NULL, probs=c(0.025,0.25,0.5,0.75,0.975), outline=FALSE, ...) {
  # Boxplot with probability quantiles (not the original whisker definition).

  if(is.null(list)&any(c(is.null(y),is.null(grouping)))) stop("specify either list or y and grouping")
  if(!is.null(list)&any(c(!is.null(y),!is.null(grouping)))) stop("specify either list or y and grouping")
  if(length(probs)!=5) stop("length(probs) != 5")

  if(is.null(list)) {
    bp <- boxplot(as.formula("y~grouping"),plot=FALSE, ...)
    uniquegrouping <- sort(unique(grouping))
  } else {
    bp <- boxplot(list,plot=FALSE,outline=outline, ...)
    uniquegrouping <- 1:length(list)
  }

  for(i in uniquegrouping) {
    if(is.null(list)) { x <- y[grouping==i]
    } else { x <- list[[uniquegrouping[i]]] }
    quants <- quantile(x,probs=probs,na.rm=TRUE)
    bp$stats[,i] <- quants
  }
  bxp(bp,outline=outline, ...)
  invisible(bp)
}

####

boxplot.3.lines <- function(y, ...){
  # Draws a boxplot with only 3 horizontal lines.
  # Enter a 3 * (number of groups) matrix with upper, middle and lower value for boxplot

  grouping <- 1:ncol(y)
  y1 <- y[c(1),]
  bp <- boxplot(as.formula("y1~grouping"),plot=FALSE)#, ...)
  med <-  y[2,]
  up <-   y[1,]
  low <-  y[3,]
  bp$stats <- unname(rbind(low, med,med,med, up))
  bp$out <- NULL
  bxp(bp, ...)
}

boxplot.meansd <- function(y,grouping, ...){
  bp <- boxplot(as.formula("y~grouping"),plot=FALSE, ...)
  means <-  tapply(y,grouping,mean,na.rm=TRUE)
  sds <-    tapply(y,grouping,sd,na.rm=TRUE)
  bp$stats <- unname(rbind(means-sds, means,means,means,means+sds))
  bp$out <- NULL
  suppressWarnings( bxp(bp, ...))
}

####

histogram.overlap <- function(data, grouping, bins=10, transparency=1/4, ...){
  stop("WORK IN PROGRESS. DOES NOT WORK!")
  h <- mids <- counts <- breaks.list <- list()
  for(j in grp) {
    hi <- hist(data[grouping==j],plot=FALSE)
    breaks <- hi$breaks
    nbreaks <- seq(min(breaks), max(breaks), length.out=bins)
    breaks.list[[j]] <- nbreaks
  }

  h <- mids <- counts <- list()
  for(j in grp) {
    hi <- hist(data[grouping==j],breaks=nbreaks,plot=FALSE)
    h[[j]] <- hi
    mids[[j]] <- hi$mids
    counts[[j]] <- hi$counts
  }

  binwidth <- h[[grp[1]]]$mids[2] - h[[grp[1]]]$mids[1] # Die bins sind f?r alle Gruppen gleich gross.
  xlim <- c(min(mids.call<-do.call("c",mids))-binwidth, max(mids.call)+binwidth)
  ylim <- c(min(counts.call<-do.call("c",counts)), max(counts.call))
  col <- rgb(colsrgb[1,grp[1]],colsrgb[2,grp[1]],colsrgb[3,grp[1]],alpha=colsrgb[4,grp[1]])

  plot(h[[grp[1]]], col=col, xlim=xlim, ylim=ylim, main=mains[i],xlab=xlab[i])
  if(length(grp)>1) for(j in grp[2:length(grp)]){
    col <- rgb(colsrgb[1,j],colsrgb[2,j],colsrgb[3,j],alpha=colsrgb[4,j])
    plot(h[[j]], col=col, xlim=xlim, ylim=ylim, add=TRUE)
  }
}

####

histogram.overlap.multi <- function(data,grouping=NULL,plotrows=3,mar=c(2,2.6,2,0.4),bins=10,cols=NULL,transparency=1/4,main=NULL,xlab="",split=1,newwin=FALSE) {
  # Plot several overlapping histograms in one plot (possible for 1 or more variables)

  if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
  colsrgb <- col2rgb(cols, alpha = 1)
  colsrgb <- colsrgb/255
  colsrgb[4,] <- transparency

  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))

  if(is.null(dim(data))) {data <- matrix(data,ncol=1); colnames(data) <- "check variable"}
  if(ncol(data)==1) {
    plotrows <- 1
    if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
  }
  if(is.null(colnames(data))) stop("the data must have colnames")
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  grp <- sort(unique(grouping))
  data.orig <- data

  choose <- list()
  for(s in 1:split) {
    if(s==1) {choose[[s]] <- 1:floor(ncol(data.orig)/split)
    } else {
      if(s<split) { choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : (s*floor(ncol(data.orig)/split))
      } else choose[[s]] <- ((s-1)*ceiling(ncol(data.orig)/split)) : ncol(data.orig)
    }
    data <- data.orig[,choose[[s]],drop=FALSE]

    if(is.null(main)) mains <- colnames(data) else if(length(main)==1) mains <- rep(main,ncol(data)) else mains <- main[choose[[s]]]
    if(length(xlab)==1) xlab <- rep(xlab,ncol(data.orig))
    if(newwin) windows()
    nvariables <- ncol(data)
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else floor(nvariables/plotrows)+1 ))   # %% gibt Rest wieder

    h <- breaks <- mids <- counts <- list()
    for(i in 1:ncol(data)){
      #h <- list()
      for(j in grp){
        hi <- hist(data[grouping==j,i],plot=FALSE)
        h[[j]] <- hi
        breaks[[j]] <- hi$breaks
      }
      nbreaks <- seq(min(break.call<-do.call("c",breaks)), max(break.call), length.out=bins)
      for(j in grp) {
        hi <- hist(data[grouping==j,i],breaks=nbreaks,plot=FALSE)
        h[[j]] <- hi
        mids[[j]] <- hi$mids
        counts[[j]] <- hi$counts
      }

      binwidth <- h[[grp[1]]]$mids[2] - h[[grp[1]]]$mids[1] # Die bins sind f?r alle Gruppen gleich gross.
      xlim <- c(min(mids.call<-do.call("c",mids))-binwidth, max(mids.call)+binwidth)
      ylim <- c(min(counts.call<-do.call("c",counts)), max(counts.call))
      col <- rgb(colsrgb[1,grp[1]],colsrgb[2,grp[1]],colsrgb[3,grp[1]],alpha=colsrgb[4,grp[1]])

      plot(h[[grp[1]]], col=col, xlim=xlim, ylim=ylim, main=mains[i],xlab=xlab[i])
      if(length(grp)>1) for(j in grp[2:length(grp)]){
        col <- rgb(colsrgb[1,j],colsrgb[2,j],colsrgb[3,j],alpha=colsrgb[4,j])
        plot(h[[j]], col=col, xlim=xlim, ylim=ylim, add=TRUE)
      }
    }
  }
}
####

density.overlap.multi <- function(data,grouping=NULL,bw="nrd0",na.rm=TRUE,cols=NULL,xlab="",main=NULL,legends=TRUE,mar=c(2,2.6,2,0.4),plotrows=3,split=1,newwin=FALSE,...) {
  # plot several overlapping density plots in one plot (possible for 1 or more variables)
  if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))

  if(is.null(dim(data))) {data <- matrix(data,ncol=1); colnames(data) <- "check variable"}
  if(ncol(data)==1) {
    plotrows <- 1
    if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
  }
  if(is.null(colnames(data))) stop("the data must have colnames")
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  grp <- sort(unique(grouping))
  data.orig <- data

  choose <- list()
  for(s in 1:split) {
    if(s==1) {choose[[s]] <-  1:floor(ncol(data.orig)/split)
    } else {
      if(s<split) { choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : (s*floor(ncol(data.orig)/split))
      } else choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : ncol(data.orig)
    }
    data <- data.orig[,choose[[s]],drop=FALSE]

    if(is.null(main)) mains <- colnames(data) else if(length(main)==1) mains <- rep(main,ncol(data)) else mains <- main[choose[[s]]]
    if(length(xlab)==1) xlab <- rep(xlab,ncol(data.orig))
    if(newwin) windows()
    nvariables <- ncol(data)
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else floor(nvariables/plotrows)+1 ))   # %% gibt Rest wieder

    dens <- densx <- densy <- list()
    for(i in 1:ncol(data)) {
      for(j in grp) {
        de <- density(data[grouping==j,i],bw=bw,na.rm=na.rm)
        dens[[j]] <- de
        densx[[j]] <- de$x
        densy[[j]] <- de$y
      }

      xlim <- ylim <- numeric()
      xlim[1] <- min(calldensx <- do.call("c",densx))
      xlim[2] <- max(calldensx)
      ylim[1] <- min(calldensy <- do.call("c",densy))
      ylim[2] <- max(calldensy)

      plot(dens[[grp[1]]],xlim=xlim,ylim=ylim,col=cols[grp[1]],xlab=xlab[i],main=mains[i], ...)#,main="Dens. Plot",xlab="Efficiency");
      for(j in grp[2:length(grp)]) lines(dens[[j]],col=cols[j], ...)
      if(legends & length(grp)>1) legend("topright", legend=grp, col=cols[1:length(grp)], lty=1, bty="n", ...)
    }
  }
}


if(FALSE){
  # Debug color.gradient()
  x <- 1:10
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(x, 10); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(x, 10 , 20); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(1,1,1,1,1,1.2,2,2.2,5,6.5,8,8.6,10,10); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(1,1,1.1,1.2,1,1.3,2,2.2,5,5.1,5.2,5.3,5.4,6.5,8,100); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- seq(1,3.7,length.out=200)
  x <- y
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  plot(1:length(x), y=x, col=color.gradient(x), pch=19,cex=3)
}

color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
  # Create Color Gradient for a given vector x with given colors.
  #
  # The function creates a color function with colorRampPalette().
  # Then it hands over the number of unique elements of x into this function()().
  # From the result of the function()()[ ] only these elements are picked which are most similar to the values in the sequence min(x) to max(x)
  # If length(unique(x)) is relatively small (<15) it is done in a computation intensive matter in order to to achieve better results.
  # Else it is done with findInterval() which is much faster.
  # Example found in the internet: browseURL("http://stackoverflow.com/questions/18827214/one-colour-gradient-according-to-value-in-column-scatterplot-in-r")

  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

#### CONVENIENCE FUNCTIONS ####

#' Showing the data of a farm in a spread sheet like format.
#' @author Daniel Hoop
#' @export
#' @param data The data of the farm. Either a data.frame/matrix or a vector. If the data.frame/matrix has more than 1 row, then the first row will be chosen.
#' @param table The name of the table, e.g. \code{"P320"}.
#' @param filterFunc A function which filters names of figures. E.g. \code{function(x) 10000 <= nMKrow(x) & nMKrow(x) < 50000}
#' @param digits The number of digits (for rounding).
#' @param translate Logical value indicating if the rows should be translated.
#' @param open Logical value indicating if the farm data should be opened (using \code{zaUtils::view}) or only be (invisibly) returned as a matrix.
#' @param sep The separation character for the file to be opened (via \code{\link{view}} function). "EXCEL" for xlsx file.
#' @return An invisible matrix.
viewFarmData <- function(data, table, filterFunc = function(x) x, digits = 2, translate = TRUE, open = TRUE, sep = "EXCEL") {

  if (length(dim(data)) == 0) {
    data <- data.frame(as.list(data), stringsAsFactors = FALSE)
    data <- char.cols.to.num(data)
  }
  if (nrow(data) > 1)
    data <- data[1, , drop = FALSE]

  data <- data[, MKtab(data) %in% table, drop = FALSE]
  suTab <- sort(unique(MKtab(data)))

  if (length(suTab) == 0)
    stop("No figure left after filtering for the table.")
  if (length(suTab) != 1)
    stop("`tableName` must be one table, e.g. 'P320'. Multiple columns where filtered:\n",
         paste0(suTab, collapse = ", "))

  data <- data[, filterFunc(colnames(data)), drop = FALSE]
  data <- data[, isMM(data), drop = FALSE]
  if (ncol(data) == 0)
    stop("After filtering the figures using `filterFunc`, no figure was left.",
         " Keep in mind the function automatically filters for Merkmale that look like 'P320_2210_12000'.",
         " Figures that do not follow this pattern, are removed automatically.")

  data[] <- lapply(data, function(x) if (is.numeric(x)) round(x, digits) else x)

  suCol <- sort(unique(MKcol(data)))
  suRow <- sort(unique(MKrow(data)))

  farmData <- matrix("", nrow = length(suRow), ncol = length(suCol))
  rownames(farmData) <- suRow
  colnames(farmData) <- suCol

  cn <- colnames(data)
  for (i in 1:ncol(data)) {
    farmData[MKrow(cn[i]), MKcol(cn[i])] <- data[1, cn[i]]
  }

  if (translate) {
    rownames(farmData) <- transl.mm(paste(suTab, suCol[1], rownames(farmData), sep = "_"))
    rownames(farmData) <- gsub("P[0-9]+_[0-9]+_", "", rownames(farmData))
  }

  if (open)
    view(farmData, "rowcol", sep = sep)

  return(invisible(farmData))
}

#' Getting the full name of a user, using a mapping between BIT/Agroscope account numbers and real persons names.
#' @author Daniel Hoop
#' @export
#' @return The full name of the user, if it could be found in the mapping, else the username given by \code{Sys.info()["user"]}.
#' If a username starting with \code{"U"} is returned by \code{Sys.info()["user"]}, then the first character is replaced by \code{"A"}.
fullUsername <- function () {
  usernumber <- Sys.info()["user"]
  usernumber <- gsub("^u|^a|^f", "A", usernumber, ignore.case = TRUE)
  pathUsermapping <- zaUtils::agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/UsernumberUsernameMapping.csv")
  username <- usernumber
  if (file.exists(pathUsermapping)) {
    usermapping <- read.table(pathUsermapping, sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", comment.char="")
    if (usernumber %in% usermapping[,"number"]) {
      username <- usermapping[which(usermapping[,"number"] == usernumber)[1], "name"]
      username <- gsub(" A$", "", username)
    }
  }
  return (username)
}

installAgsGitlabUtilsIfNecessary <- function() {
  if (!"agsGitlabUtils" %in% rownames(installed.packages())) {
    local({
      source("https://gitlab.agsad.admin.ch/f80823148/agsGitlabUtils/raw/master/R/agsGitlabUtils.R", local=environment())
      installFromAgsGitlab("agsGitlabUtils", "f80823148")
    })
  }
}

#' @export
#' @author Daniel Hoop
loadSqlUtils <- function(){
  # This function loads the SQL Utility functions to access the ZA database.
  # Because these functions contain confidential login information for the database, they are not part of this file (func.R).
  installAgsGitlabUtilsIfNecessary()
  agsGitlabUtils::requireNewest("dbUtilsZa")
}

#' @export
#' @author Daniel Hoop
loadGbUtils <- function(){
  # This function loads functions to create the Grundlagenbericht (gb).
  installAgsGitlabUtilsIfNecessary()
  agsGitlabUtils::requireNewest("gbUtilsZa")
}

#' @export
#' @author Daniel Hoop
loadVarEstCalibFunc <- function(){
  # The functions being sourced are outsourced from this script because they use a lot of lines of code.
  file <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000/func.R")

  if (file.exists(file)) {
    installAgsGitlabUtilsIfNecessary()
    agsGitlabUtils::requireNewest("VESP")
    assign("variance.estimate", function(...) stop("The function `variance.estimate` is now called `varianceEstimate`. Fore more info, see ?varianceEstimate"), envir = .GlobalEnv)
    assign("varEstZaBh", function(...) stop("The function `varEstZaBh` is now called `varEstim`. Fore more info, see ?varEstim"), envir = .GlobalEnv)
  } else {
    source("https://raw.githubusercontent.com/danielhoop/R/master/VarEstCalibFunc.R")
  }
}
loadVarEstCalibUtils <- loadVarEstCalibFunc


slash <- function(reverse=FALSE){
  # This function changes \ to / in paths (or vice versa if reverse=TRUE)

  cb <- suppressWarnings(readLines("clipboard"))
  if(!any(grepl("\\\\",cb)) && grepl("/",cb)) reverse <- TRUE

  if(!reverse){
    txt <- gsub("\\\\","/",cb)
  } else {
    txt <- gsub("/","\\\\",cb) # Like this: "/{1,10}" you would replace several / with only one \
  }
  write.table(txt,'clipboard',quote=FALSE,col.names=FALSE,row.names=FALSE,eol=""); message("Converted string is in clipboard. Use Ctrl+V:\n",txt,sep="")
}


messageBox <- function (lines, width=1, sign="*") { # messageBox
  # This function  draws a box around a message sucht that is is more likely to be read by the user.
  # Arguments
  # lines = The lines you want to write. A character vector.
  # width = The width of the box that will be drawn.
  # sign  = The character with which the box will be drawn.

  if (any(nchar(lines) - nchar(gsub("\n","",lines,fixed=TRUE)) != 0))
    stop ("Newlines \\n are not allowed in the messsage. Use a new vector place for each line in argument 'lines'.")

  centerMsg <- function(lines) {
    # if (length(lines) == 1)
    #   return (lines)
    nSpacesAdd <- floor((max(nchar(lines)) - nchar(lines)) / 2)
    for (i in 1:length(lines)) { # i <- 1
      # if (nSpacesAdd[i]>0) {
      spacesAdd <- paste0(rep(" ",nSpacesAdd[i]), collapse="")
      lines[i] <- paste0(spacesAdd, lines[i], spacesAdd)
      # }
    }
    return (lines)
  }
  leftRight <- rep(sign, width)
  bottomTop <- as.list(rep(sign, width))
  msg <- do.call("c.matrices",strsplit(centerMsg(lines),""))
  msg <- t(apply(msg,1,function(x){ x[x==""] <- " "; return (x); }))
  msg <- t(apply(msg,1,function(x)c(leftRight, " ", x, " ", leftRight)))
  msgSemiFinal <- c.matrices(c(bottomTop, list(msg), bottomTop), fill=sign)
  msgFinal <- paste(c(apply(msgSemiFinal, 1, paste, collapse=""),""), collapse="\n") # "" at the end for new line.
  return (msgFinal)
}

# a=1, b="A,B", d=c(1,1)
# arg <- function(){
#   txt <- suppressWarnings(readLines("clipboard"))
#   qc <- bc <- 0 # quote counter, bracket counter
#   for(i in 1:nchar(txt)){
#     if(        qc==0 && substr(txt,i,i)%in%c("\"","'")) { qc <- qc+1
#     } else if (qc==1 && substr(txt,i,i)%in%c("\"","'")) { qc <- qc-1 }
#     if(        qc==0 && substr(txt,i,i)%in%c("(")) {      bc <- bc+1
#     } else if (qc==0 && substr(txt,i,i)%in%c(")")) {      bc <- bc-1 }
#     if(        qc==0 && substr(txt,i,i)%in%c("[")) {      bc <- bc+1
#     } else if (qc==0 && substr(txt,i,i)%in%c("]")) {      bc <- bc-1 }
#
#     if(qc==0 && bc==0 && substr(txt,i,i)==",") {
#       substr(txt,i,i) <- ";"
#     }
#   }
#   write.table(txt,'clipboard',quote=FALSE,col.names=FALSE,row.names=FALSE,eol=""); message("Converted string is in clipboard. Use Ctrl+V:\n",txt,sep="")
# }

save.packages <- function(){
  # This function saves all installed R-Packages to a file.
  # Use function recover.R.installation() to recover all packages.

  if (grepl("linux", Sys.info()["sysname"], ignore.case=TRUE))
    stop ("This function is not yet designed to work on Linux systems.")

  pkg_list <- rownames(installed.packages()) #installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]
  save(pkg_list, file=paste0(Sys.getenv("TMP"),"/R_Migration_Package_List.Rdata") )
}

recover.R.installation <- function(){
  # This function loads the names of all previously installed packages (that were saved by function save.packages()) and installs those packages.
  # Afterwards the Rprofile.site is edited, such that my own functions are loaded automatically when starting R.

  if (grepl("linux", Sys.info()["sysname"], ignore.case=TRUE))
    stop ("This function is not yet designed to work on Linux systems.")

  pkg_list <- load2(paste0(Sys.getenv("TMP"),"/R_Migration_Package_List.Rdata"))
  install.packages(pkg_list, lib = .libPaths())
  message("Packages installed!")

  #txt <- scan(paste0(R.home("etc"),"/Rprofile.site"), what=character())
  if(.onHpdaPc()){
    txt <- readLines(paste0(R.home("etc"),"/Rprofile.site"))
    txt <- txt[txt!=""]
    addtxt <- "fortunes::fortune(); source('//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/9/4278/hpda/R/func/func.R')"
    if(txt[length(txt)]!=addtxt){
      write.table(c(txt,addtxt), paste0(R.home("etc"),"/Rprofile.site"), quote=FALSE, col.names=FALSE, row.names=FALSE, append=TRUE)
      message("Rprofile.site updated!")
    } else {
      message("Rprofile.site was already up to date!")
    }
  } else {
    message("Rprofile.site *NOT* updated because not Daniel's computer!")
  }

  message(paste0("Information: If you use RStudio and encounter a warning message like\n***\nIn dir.create(tempPath, recursive = TRUE) :\ncannot create dir '\\\\evdad.admin.ch\\AGROSCOPE_OS', reason 'Permission denied'\n***",
                 "\nat every start of RStudio then you must edit the file  .../RStudio/R/modules/SessionProfiler.R  and delete delete the according line."))
}

list.all.package.functions <- function(package, all.names = FALSE, pattern) {
  # List all functions of a package
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

#' Install a package if not yet installed. Later require the package.
#' @export
#' @author Daniel Hoop
require.package <- function(..., repos = "http://stat.ethz.ch/CRAN/"){
  # This function checks if a package is already installed or loaded. If not, installation is done. Then package is required.

  pkgName <- deparse(substitute(...))
  if (substr(pkgName,1,1)=="\"" && substr(pkgName,nchar(pkgName),nchar(pkgName))=="\"") {
    pkgName <- substr(pkgName,2,nchar(pkgName)-1)
  }
  if (!paste0("package:",pkgName)%in%search()) {
    installFromCRAN(pkgName, repos = repos)
    require(..., quietly=TRUE)
  }
}

#' Install a package if not yet installed.
#' @keywords internal
.install.package <- function (x, repos = options()$repos) {
  installFromCRAN(x, repos = repos)
}

#' Install a package from CRAN, but only if it is not yet installed
#' @keywords internal
#' @author Daniel Hoop
#' @param x The name(s) of the package(s).
#' @param ... Further arguments passed into \code{\link[utils:install.packages]{utils::install.packages}}.
#' @param lib character vector giving the library directories where to install the packages. Recycled as needed. If missing, defaults to the first element of \code{\link[base:.libPaths]{base::.libPaths}()}.
#' @return Logical vector (always FALSE) with length(pkgs), i.e. `return(logical(length(pkgs)))`. For compability with other installation methods of the package.
installFromCRAN <- function (pkgs, lib=NULL, repos = options()$repos, ...) {
  oldOption <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  on.exit(
    options(install.packages.check.source = oldOption))

  lengthPkgs <- length(pkgs)
  lib.loc <- NULL
  if (!is.null(lib)) {
    lib.loc <- lib
  } else {
    lib <- .libPaths()[1]
  }
  # keep non-loaded packages, only.
  pkgs <- pkgs[!pkgs %in% .packages()]
  # keep packages that aren't installed, only.
  pkgs <- pkgs[!pkgs %in% rownames(installed.packages(lib.loc = lib.loc))]
  # Install from binary, if on Windows plattform.
  type <- if (grepl("window", Sys.info()["sysname"], ignore.case=TRUE)) "binary" else "source"
  if (length(pkgs > 0)) {
    install.packages(pkgs, lib = lib, repos = repos, type = type, ...)
  }

  return (invisible(logical(lengthPkgs)))
}

#' Get Vergleichslohn data (opportunity costs of family labor).
#' @export
#' @author Daniel Hoop
#' @param region Optional: The region(s) for which the Vergleichslohn should be returned.
#' @param jahr Optional: The year(s) for which the Vergleichslohn should be returned.
#' @return A matrix which differentiates for regions in rows, and for years in columns.
#' @examples
#' vergleichslohn()
#' vergleichslohn(c(1,3), 2019:2020)
vergleichslohn <- function(region=NULL, jahr=NULL){

  # Source: \\evdad.admin.ch\AGROSCOPE_OS\2\5\2\1\2\1\4341\Vergleichslohn_Zinsen\Zeitreih.xls
  # The following line is only needed to import data from the Excel Area E73:A?76
  # t1 <- zaUtils::read.cb("col"); t1 <- zaUtils::char.cols.to.num(sapply(t1,function(x)gsub("'","",x))); colnames(t1) <- zaUtils::substr.rev(colnames(t1),1,4); t1 <- as.matrix(t1); dput(t1)

  vgl <- structure(c(42302L, 38300L, 35067L, 43789L, 39647L, 36300L, 44800L,
              40563L, 37138L, 46407L, 42017L, 38470L, 48132L, 43579L, 39900L,
              50988L, 46165L, 42267L, 54498L, 49343L, 45177L, 57116L, 51713L,
              47347L, 58663L, 53114L, 48630L, 59496L, 53868L, 49320L, 60269L,
              54568L, 49961L, 61320L, 56328L, 51996L, 61627L, 56610L, 52256L,
              62056L, 57004L, 52620L, 65854L, 60885L, 55129L, 67011L, 61954L,
              56097L, 67630L, 62434L, 56934L, 68230L, 62988L, 57439L, 68939L,
              63085L, 58188L, 69689L, 63772L, 58822L, 71092L, 64520L, 60204L,
              72561L, 65854L, 61448L, 73279L, 66994L, 62387L, 73853L, 67519L,
              62876L, 74199L, 66963L, 62588L, 74786L, 67493L, 63083L, 73712L,
              69108L, 63840L, 74298L, 69657L, 64347L, 74011L, 69035L, 66240L,
              74527L, 69516L, 66702L, 74748L, 69723L, 66900L, 74718L, 70081L,
              66194L, 75094L, 70360L, 65710L, 75689L, 70917L, 66231L, 75615L,
              70848L, 66166L), .Dim = c(3L, 35L), .Dimnames = list(
                c("1", "2", "3"),
                c("1985", "1986", "1987", "1988", "1989", "1990", "1991",
                  "1992", "1993", "1994", "1995", "1996", "1997", "1998", "2001",
                  "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                  "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                  "2018", "2019", "2020", "2021")))

  if(is.null(region)) region <- 1:nrow(vgl)
  if(is.null(jahr)) jahr <- 1:ncol(vgl)
  if(is.numeric(jahr) && min(jahr)>1000) jahr <- as.character(jahr)
  return(vgl[region,jahr,drop=FALSE])
}

#' Get Vergleichszins data ('interest rate for' / 'opportunity cost of' equity).
#' @export
#' @author Daniel Hoop
#' @param jahr Optional: The year(s) for which the Vergleichszins should be returned.
#' @return A matrix with 1 row containing the interest rate for each year in a separate column.
#' @examples
#' vergleichszins()
#' vergleichszins(2018:2020)
vergleichszins <- function(jahr=NULL){

  # Source: \\evdad.admin.ch\AGROSCOPE_OS\2\5\2\1\2\1\4341\Vergleichslohn_Zinsen\Zeitreih.xls
  # The following line is only needed to import data.
  # t1 <- read.cb("col"); colnames(t1) <- substr.rev(colnames(t1),1,4); t1 <- as.matrix(t1); dput(t1)
  vgl <- structure(
    c(4.53, 4.71, 4.24, 4.04, 4, 5.13, 6.4, 6.23, 6.42,
      4.58, 4.93, 4.57, 4, 3.4, 2.81, 3.02, 3.95, 3.36, 3.22, 2.63,
      2.73, 2.11, 2.5, 2.91, 2.93, 2.22, 1.65, 1.48, 0.66, 0.94, 0.73,
      0, 0, 0, 0.05, 0, 0, 0),
    .Dim = c(1L, 38L),
    .Dimnames = list(
      NULL,
      c("1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
        "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001",
        "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
        "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
        "2018", "2019", "2020", "2021")))
  if (is.null(jahr))
    jahr <- colnames(vgl)
  if (is.numeric(jahr) && min(jahr)>1900)
    jahr <- as.character(jahr)
  return(vgl[,jahr,drop=FALSE])
}

#' @title Get the AHV Anteil that is payed by the farm (not the farmer).
#' @description  E.g. 0.60 means that the farm pays 60\% of the BVG, and the farmer pays 40\% him/herself.
#' @export
#' @author Daniel Hoop
ahv.anteil.betrieb <- function(jahr=NULL){
  BHJ <- as.numeric(substr(as.character(Sys.time()), 1, 4))
  vgl <- matrix(rep(0.6,BHJ-2014+1),nrow=1);
  colnames(vgl) <- 2014:BHJ

  if(is.null(jahr)) jahr <- 1:ncol(vgl)
  if(is.numeric(jahr) && min(jahr)>1000) jahr <- as.character(jahr)
  return(vgl[,jahr,drop=FALSE])
}

#' @title Get the AHV Satz that is assumed for Betriebsgemeinschaften.
#' @description E.g. 0.125 means that the rate is 12.5\% of the farm income, i.e. \code{AHV_total = 0.125 * Landw_Einkommen}.
#' @export
#' @author Daniel Hoop
ahv.satz.BG <- function(jahr=NULL){
  BHJ <- as.numeric(substr(as.character(Sys.time()), 1, 4))
  vgl <- matrix(rep(0.125,BHJ-2014+1),nrow=1);
  colnames(vgl) <- 2014:BHJ

  if(is.null(jahr)) jahr <- 1:ncol(vgl)
  if(is.numeric(jahr) && min(jahr)>1000) jahr <- as.character(jahr)
  return(vgl[,jahr,drop=FALSE])
}

find.fun <- function(pattern){
  # This function finds all functions in the workspace that contain a certain pattern.
  allfun <- as.character( lsf.str( envir=environment(find.fun) ) )
  choose <- which(grepl(paste0(pattern,collapse="|"), allfun, ignore.case=TRUE ))
  return(allfun[choose])
}

find.obj <- function(pattern, ignore.case=TRUE){
  # This function finds all objects in the workspace that contain a certain pattern.
  allobj <- as.character( ls( envir=environment(find.fun) ) )
  choose <- grep(paste0(pattern,collapse="|"), allobj , ignore.case=ignore.case)
  return(allobj[choose])
}

#x <- as.data.frame(matrix(1:100, ncol=10))
printzeros <- function(x, zero.sign=".") {
  x[x==0] <- NA
  print(x, na=zero.sign)
}

# http://adv-r.had.co.nz/Functions.html#special-calls
#`%+%` <- function(a, b) paste0(a, b)
#`% %` <- function(a, b) paste (a, b)
#pc <- function(...) paste0(..., collapse=", ") # Try this: # "a" % % "b" % % pc(1:10)

l <- match.fun(length)
# http://adv-r.had.co.nz/Functions.html#replacement-functions
cn <- match.fun(colnames);# `cn<-` <- `colnames<-`
rn <- match.fun(rownames);# `rn<-` <- `rownames<-`
dn <- match.fun(dimnames);# `dn<-` <- `dimnames<-`
nc <- match.fun(ncol)
nr <- match.fun(nrow)
su <- function(x) sort(unique(x))
# x <- data.frame(a=as.factor(c("b","c")),b=1:2,c=c("a","z"),stringsAsFactors=FALSE)

#' @title Short form of \code{head}.
#' @description In addition, shows the mode of each column, when \code{x} is a data.frame.
#' @export
#' @author Daniel Hoop
h <- function(x, n=6) {

  if(is.null(dim(x)) || is.matrix(x))(return(head(x,n)))
  if(n>dim(x)[1]) n <- dim(x)[1]

  if(length(dim(x))==3) {
    return(x[1:n,,])

  } else {
    if(is.data.frame(x)) {
      rows <- if (nrow(x) == 0) numeric() else 1:min(nrow(x), n)
      cn1 <- colnames(x)
      rn1 <- rownames(x)
      x <- as.data.frame(rbind(paste0("<", substr( sapply(x,function(x)class(x)[1]), 1,3), ">"),
                               as.matrix(x[rows,,drop=FALSE])
      ),
      stringsAsFactors=FALSE)
      colnames(x) <- cn1
      rownames(x) <- c("-", rn1[rows])
      return(x)
    } else {
      return(head(x,n))
    }
  }
}

ch <- function(x){
  # Look shortly at the most important properties of a matrix / data.frame
  print(head(x))
  cat("\ncolnames\n")
  print(colnames(x)); cat("\n")
  cat(paste("nrow:\t",nrow(x),"\n"))
  cat(paste("ncol:\t",ncol(x)))
}
####

naF <- function(x){
  x[is.na(x)] <- FALSE
  return(x)
}
naT <- function(x){
  x[is.na(x)] <- TRUE
  return(x)
}
na0 <- function(x){
  x[is.na(x)] <- 0
  return(x)
}


#' Show missing names
#' @author Daniel Hoop
#' @param check The vector which holds the elements that should be checked.
#' @param against The vector or data.frame/matrix which has all available names.
#' @param msgPrefix The message prefix to show when an error occurs. After that prefix, all missing names will be listed. If NULL, then the default prefix will be given.
#' @examples
#' cols <- c("a", "b", "c", "d")
#' data <- data.frame(a = 1, b = 2)
#' showMissingNames(cols, data)
showMissingNames <- function(check, against, msgPrefix = NULL) {
  if (length(msgPrefix) == 0)
    msgPrefix <- c("The following elements cannot be found in the names of `", substitute(against), "`: ")
  if (length(dim(against)) > 0) {
    against <- colnames(against)
  } else if (!is.null(names(against))) {
    against <- names(against)
  }
  missing <- check[!check %in% against]
  if (length(missing) > 0)
    stop(msgPrefix, paste0(paste0("\"", missing, "\""), collapse = ", "))
}


#' Remove the '15' prefix from the ZA2015 type code.
#' @author Daniel Hoop
#' @export
#' @examples
#' x <- c(15234, 15, 1512, 1215)
#' removePrefixFromTypeCode(x)
removePrefixFromTypeCode <- function(x) {
  if (length(x) == 0)
    return(x)
  isNumeric <- mode(x) == "numeric"
  x <- as.character(x)

  whichInd <- grepl("^15", x) & !grepl("^15$", x)
  x[whichInd] <- gsub("^15", "", x[whichInd])

  if (isNumeric)
    x <- as.numeric(x)
  return(x)
}

#### Funktionen, mit denen einfach die Tabellennr. Spaltennr. Zeilennr. etc. aus den Spalten?berschriften des Merkmals
#### Katalogs rausgelesen werden k?nnen.

# REFERENZBETRIEBE
#MKtab <- function(string) {if(is.null(dim(string))) substr(string,1,4) else substr(colnames(string),1,4)}
MKspalte <- function(string) {if(is.null(dim(string))) substr(string,6,9) else substr(colnames(string),6,9)}
MKzeile <- function(string) {if(is.null(dim(string))) substr(string,11,15) else substr(colnames(string),11,15)}

sort.MK <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="M" & nchar(cn.data)>=4 & !is.na(suppressWarnings(as.numeric(substr(cn.data,2,4))))
  data.keep <- data[ , !change_vec, drop=FALSE ]
  data.change <- data[ , change_vec, drop=FALSE ]
  cn.data.change <- colnames(data.change)
  if(order=="zeile") {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKzeile(cn.data.change),MKspalte(cn.data.change)) ]) )
  } else {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKspalte(cn.data.change),MKzeile(cn.data.change)) ] ) )
  }
}
sort.MK.colnames <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="M" & nchar(cn.data)>=4 & !is.na(suppressWarnings(as.numeric(substr(cn.data,2,4))))
  cn.keep <- cn.data[!change_vec]
  cn.change <- cn.data[change_vec]
  if(order=="zeile") {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKzeile(cn.change), MKspalte(cn.change)) ] ) )
  } else {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKspalte(cn.change),MKzeile(cn.change)) ] ) )
  }
}

#' Determine if something looks like a Merkmal (e.g. "P300_2300_30000")
#' @author Daniel Hoop
#' @export
#' @param x Either a data.frame/matrix or a character vector. If a data.frame/matrix is given, then the information is given with respect to the colnames.
#' @param strict Logical value indicating if all identifiers have to be numeric. If \code{strict = TRUE}, then \code{'P340_2399_3000'} would be valid, but \code{'P320_abcd_40000'} would be not. If \code{strict = FALSE}, then also \code{'Pxxx_woidjfoiwe_dcnv823'} is a valid Merkmal name.
#' @details Names starting with \code{"M"} or with \code{"P"} are allowed. As delimiters, \code{"."} and \code{"_"} are possible. E.g. \code{"P300_2000_30000"}, or \code{"M300.200.30000"} are both valid names.
#' @return A logical vector indicating for each element given in \code{x} if it is a Merkmal or not.
#' @examples
#' isMM(c("M230_3000_200aa", "X230_3900_20000"))
#' # [1] TRUE FALSE
#'
#' isMM(data.frame(M230_3000_200aa = 1, P39.2300.200 = 1), strict = TRUE)
#' # [1] FALSE TRUE
isMM <- function(x, strict = FALSE) {
  if (length(dim(x)) != 0)
    x <- colnames(x)
  if (strict)
    return(grepl("^[PM][0-9]+_[0-9]+_[0-9]+$", x) |
             grepl("^[PM][0-9]+\\.[0-9]+\\.[0-9]+$", x))
  return(grepl("^[PM][0-9a-zA-Z]+_[0-9a-zA-Z]+_[0-9a-zA-Z]+$", x) |
           grepl("^[PM][0-9a-zA-Z]+\\.[0-9a-zA-Z]+\\.[0-9a-zA-Z]+$", x))
}

# x <- c("P240_234093_309324", "__", "___", "P234_3")
#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts einen beliebigen Abschnitt wie Tabellen-/Spalten-/Zeilenbezeichnung. Gibt \code{character} vector zurueck.
#' @keywords internal
#' @param x The character vector or matrix/data.frame
#' @param which 1 = table, 2 = column, 3 = row
#' @author Daniel Hoop
.MKtabColRow <- function(x, which) {
  if (is.null(x))
    return(x)
  if (length(x) == 0)
    return(character())
  if (length(dim(x)) > 0)
    return(.MKtabColRow(colnames(x), which))

  a <- sapply(strsplit(x, "_"), function(y) {
    if (length(y) == 3)
      return(y[which])
    return(NA)
  })
  a[is.na(a)] <- x[is.na(a)]

  return(a)
}

#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts die Nummer der ZA-BH-Tabelle. Gibt \code{character} vector zurueck.
#' @export
#' @author Daniel Hoop
MKtab <- function(x) {
  .MKtabColRow(x, 1)
}

#' Setzt in \code{character} vector, oder in colnames eines Objekts die Nummer der ZA-BH-Tabelle ein.
#' @export
#' @author Daniel Hoop
`MKtab<-` <- function(x, value){
  if(is.null(dim(x))) substr(x, 1, 4) <- value
  else substr(colnames(x), 1, 4) <- value
  return (x)
}

#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts die Nummer der ZA-BH-Tabelle. Gibt \code{numeric} vector zurueck.
#' @export
#' @author Daniel Hoop
nMKtab <- function(x) {
  y <- MKtab(x)
  y <- suppressWarnings(as.numeric( substr(y,2,nchar(y)) ))
  y[is.na(y)] <- -1
  return(y)
}

#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts die Nummer der ZA-BH-Spalte. Gibt \code{character} vector zurueck.
#' @export
#' @author Daniel Hoop
MKcol <- function(x) {
  .MKtabColRow(x, 2)
}

#' Setzt in \code{character} vector, oder in colnames eines Objekts die Nummer der ZA-BH-Spalte ein.
#' @export
#' @author Daniel Hoop
`MKcol<-` <- function(x, value){
  if (is.null(dim(x))) substr(x, 6, 9) <- value
  else substr(colnames(x), 6, 9) <- value
  return (x)
}

#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts die Nummer der ZA-BH-Spalte. Gibt \code{numeric} vector zurueck.
#' @export
#' @author Daniel Hoop
nMKcol <- function(x){
  y <- suppressWarnings(as.numeric(MKcol(x)))
  y[is.na(y)] <- -1
  return(y)
}

#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts die Nummer der ZA-BH-Zeile. Gibt \code{character} vector zurueck.
#' @export
#' @author Daniel Hoop
MKrow <- function(x) {
  .MKtabColRow(x, 3)
}

#' Setzt in \code{character} vector, oder in colnames eines Objekts die Nummer der ZA-BH-Zeile ein.
#' @export
#' @author Daniel Hoop
`MKrow<-` <- function(x, value){
  if (is.null(dim(x))) substr(x, 11, 15) <- value
  else substr(colnames(x), 11, 15) <- value
  return (x)
}

#' Extrahiert aus \code{character} vector oder aus colnames eines Objekts die Nummer der ZA-BH-Spalte. Gibt \code{numeric} vector zurueck.
#' @export
#' @author Daniel Hoop
nMKrow <- function(x) {
  y <- suppressWarnings(as.numeric(MKrow(x)))
  y[is.na(y)] <- -1
  return(y)
}

#' Sort a dataset according to the colnames, where Merkmale like 'P430_0100_94000' are treated specially.
#' @keywords internal
#' @author Daniel Hoop
MKsort <- function(data, order=c("row","col")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="P" & nchar(cn.data)>=4 & !is.na(is.numeric(substr(cn.data,2,4)))
  data.keep <- data[ , !change_vec, drop=FALSE ]
  data.change <- data[ , change_vec, drop=FALSE ]
  cn.data.change <- colnames(data.change)
  if(order=="row") {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKrow(cn.data.change),MKcol(cn.data.change)) ]) )
  } else {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKcol(cn.data.change),MKrow(cn.data.change)) ] ) )
  }
}

#' Same as \code{\link{MKsort.colnames}}, but will return the colnames of \code{data} instead of \code{data} itself.
#' @keywords internal
#' @author Daniel Hoop
MKsort.colnames <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="P" & nchar(cn.data)>=4 & !is.na(is.numeric(substr(cn.data,2,4)))
  cn.keep <- cn.data[!change_vec]
  cn.change <- cn.data[change_vec]
  if(order=="zeile") {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKrow(cn.change), MKcol(cn.change)) ] ) )
  } else {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKcol(cn.change),MKrow(cn.change)) ] ) )
  }
}

#' Harmonize columns for enterprise data.
#' @details  This function creates a vector of 'Merkmale' that contain all combinations of Merkmals-Spalten and Merkmals-Zeilen for a given Merkmals-Tabelle.
#' Example: M100.0100.10000
#' It removes all columns in dat that are not given in the list.
#' It adds all columns in dat if they are not already there (and sets their value equal 0).
#' In the end, it sorts the colums of dat by order(cols,rows) or order(rows, cols).
#' If comment==TRUE, it is reported, which columns are removed and added to dat.
add.remove.MK.cols <- function(data, tab, cols, rows, order=c("col","row"), sep="_", verbose=FALSE) {

  if(length(tab)>1) stop("Only choose 1 tab.")
  order <- match.arg(order)
  # if(nchar(tab)!=4) warning("Usually, tab should have 4 characters: M123")
  # if(any(nchar(cols)!=4)) warning("Usually, col should have 4 digits: 0100")

  cols <- sort(unique(cols))
  rows <- sort(unique(rows))

  ncols <- length(cols)
  nrows <- length(rows)
  allcols <- paste(rep(tab,ncols*nrows), rep.1b1(cols,nrows),  rep(rows,ncols), sep=sep)

  ## Kommentare fuer die Spalten ##
  if(verbose) { cat("Diese Spalten (cols) muessen im data.frame vorhanden sein:\n"); print(cols); cat("\n") }

  # Anzeigen, welche Spalten ganz neu hinzugefuegt werden
  names_table_allcols <- sort(unique(c(cols, MKcol(data))))
  table0 <- table.fixed(cols, names.result=names_table_allcols, vector.result=TRUE)
  table1 <- table.fixed(MKcol(colnames(data)), names.result=names_table_allcols, vector.result=TRUE)
  addall <- table0>0 & table1==0
  if(verbose) { cat("Diese Spalten (cols) werden ganz neu in den Datensatz eingefuegt:\n"); print(names(table1)[addall]); cat("\n") }

  # Anzeigen, welche Spalten komplett geloescht werden
  names_table_allcols <- sort(unique(c(cols, MKcol(data))))
  table0 <- table.fixed(MKcol(colnames(data)[!colnames(data)%in%allcols]), names.result=names_table_allcols, vector.result=TRUE)
  table1 <- table.fixed(MKcol(colnames(data)), names.result=names_table_allcols, vector.result=TRUE)
  delall <- table0==table1 & table1>0
  if(verbose) { cat("Diese Spalten (cols) werden komplett aus dem Datensatz geloescht:\n"); print(names(table1)[delall]); cat("\n") }

  ## Kommentare fuer die Zeilen ##
  if(verbose) { cat("Diese Zeilen (rows) muessen im data.frame vorhanden sein:\n"); print(rows); cat("\n") }

  # Anzeigen, welche Zeilen ganz neu hinzugefuegt werden
  names_table_allcols <- sort(unique(c(rows, MKrow(data))))
  table0 <- table.fixed(rows, names.result=names_table_allcols, vector.result=TRUE)
  table1 <- table.fixed(MKrow(colnames(data)), names.result=names_table_allcols, vector.result=TRUE)
  addall <- table0>0 & table1==0
  if(verbose) { cat("Diese Zeilen (rows) werden ganz neu in den Datensatz eingefuegt:\n"); print(names(table1)[addall]); cat("\n") }

  # Anzeigen, welche Zeilen komplett geloescht werden
  names_table_allcols <- sort(unique(c(rows, MKrow(data))))
  table0 <- table.fixed(MKrow(colnames(data)[!colnames(data)%in%allcols]), names.result=names_table_allcols, vector.result=TRUE)
  table1 <- table.fixed(MKrow(colnames(data)), names.result=names_table_allcols, vector.result=TRUE)
  delall <- table0==table1 & table1>0
  if(verbose) { cat("Diese Zeilen (rows) werden komplett aus dem Datensatz geloescht:\n"); print(names(table1)[delall]); cat("\n") }

  # Anzeigen, welche Merkmale (Kombination aus Spalten und Zeilen) geloescht werden
  if(verbose) { cat("Folgene Merkmale werden geloescht:\n"); print(colnames(data)[!colnames(data)%in%allcols], quote=FALSE); cat("\n") }
  data <- data[,colnames(data)%in%allcols]

  # Nun alle Spalten hinzufuegen, die noch fehlen.
  newcols <- allcols[!allcols%in%colnames(data)]
  # Anzeigen, welche Merkmale (Kombination aus Spalten und Zeilen) neu hinzugefuegt werden
  if(verbose) { cat("Folgene Merkmale werden hinzugefuegt:\n"); print(newcols, quote=FALSE); }
  # Umstaendlichere, aber schnellere Variante fuer Matrix:
  dat_add <- matrix(0, nrow=nrow(data), ncol=length(newcols))
  colnames(dat_add) <- newcols
  data <- cbind(data, dat_add)
  if(order=="col") data <- data[,order(MKcol(data), MKrow(data))]
  if(order=="row") data <- data[,order(MKrow(data), MKcol(data))]

  return(data)
}


#' Paste all columns of a \code{data.frame} or \code{matrix} and collapse them.
#' @export
#' @author Daniel Hoop
#' @param dat The \code{data.frame} or \code{matrix} of which all columns will be collapsed.
#' @param cols Optionally: The columns in \code{dat} which should be collapsed.
#' @param sep The \code{character} which should be used as a separator between the collapsed column values.
paste.cols <- function(dat, cols = NULL, sep = "_") {
  # Paste Values of columns of a data frame
  if(is.null(dim(dat))) return(dat)
  if(is.null(colnames(dat))) colnames(dat) <- 1:ncol(dat)
  if(is.null(cols)) cols <- colnames(dat)
  return( eval(parse(text= paste0( "paste(", paste( paste0("dat[,'",cols,"',drop=TRUE]"), collapse=","), ",sep='",sep,"')") )) )
}

paste.IDJahr <- function(dat){
  return(paste.cols(dat=dat, cols=c("ID","Jahr"), sep="_"))
}

# wait <- function(secs) {
#   Sys.sleep(secs)
# }

# try <- function(...) {
#   tryCatch(..., error=function(e)e,warning=function(w)w)
# }

#' Removes and garbage collects all objects in the current environment (can also be within a function) except for the variables defined in keep (charcter vector).
#' @keywords internal
#' @author Daniel Hoop
#' @param keep A character vector naming all objects that should be kept.
rm.gc.keep <- function(keep){
  rm1 <- ls(envir=parent.frame())
  rm1 <- rm1[!rm1%in%keep]
  rm(list=rm1, envir=parent.frame())
  invisible(gc())
}

# x <- data.frame(a=c(1,2,3,4), b=letters[1:4], stringsAsFactors=FALSE)
dput2 <- function (x) {
  if (is.data.frame(x)) {
    str0 <- sapply(x, function (x) {
      if (is.factor(x))
        return (paste0("factor(c('", paste0(x, collapse="', '"), "')",
                       ", levels=c('", paste0(levels(x), collapse="', '"), "')",
                       ")"))
      notChar <- !is.character(x)
      if (is.integer(x))
        x <- paste0(x, "L")
      if (notChar)
        return (paste0("c(", paste0(x, collapse=", "), ")"))
      return (paste0("c('", paste0(x, collapse="', '"), "')"))
    })

    str <- paste0(str0, collapse=", ") #paste0( paste0("`",names(str0), "`=", str0), collapse=", ")
    str <- paste0("local({ asdofiue <- data.frame(", str, ", stringsAsFactors=FALSE); ",
                  "colnames(asdofiue) <- c(", paste0(paste0("'", names(str0), "'", sep=""), collapse=", "), "); ",
                  "rownames(asdofiue) <- c(", paste0(paste0("'", rownames(x), "'", sep=""), collapse=", "), "); ",
                  "return (asdofiue); })\n", sep="")
    cat(str)
  } else {
    dput(x)
  }
}

#' Extract the year from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @return The year as numeric.
#' @examples
#' year(Sys.time())
year <- function(time) {
  as.numeric(format(time, "%Y"))
}

#' Extract the month from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @return The month as numeric.
#' @examples
#' month(Sys.time())
month <- function(time) {
  as.numeric(format(time, "%m"))
}

#' Extract the day of month from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @return The dayOfMonth as numeric.
#' @examples
#' dayOfMonth(Sys.time())
dayOfMonth <- function(time) {
  as.numeric(format(time, "%d"))
}

#' Extract the day of week from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @param numeric Logical value indicating if the return value should be numeric (instead of character). Character output will depend on the locale.
#' @return The dayOfWeek as numeric starting with 1 = Monday, ending with 7 = Sunday. Otherwise a character string depending on the locale.
#' @examples
#' dayOfWeek(Sys.time(), numeric = TRUE)
dayOfWeek <- function(time, numeric = TRUE) {
  if (numeric)
    return(as.numeric(format(time, "%u")))
  return(format(time, "%A"))
}

#' Extract the hour from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @return The hour as numeric.
#' @examples
#' hour(Sys.time())
hour <- function(time) {
  as.numeric(format(time, "%H"))
}

#' Extract the minute from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @return The minute as numeric.
#' @examples
#' minute(Sys.time())
minute <- function(time) {
  as.numeric(format(time, "%M"))
}

#' Extract the second from a POSIX time object.
#' @export
#' @author Daniel Hoop
#' @param time A POSIX time object.
#' @return The second as numeric.
#' @examples
#' second(Sys.time())
second <- function(time) {
  as.numeric(format(time, "%S"))
}

#### CHANGE OBJECT STRUCUTRE ####

#' @title Get columns from object and provide hints in case of failure.
#' @description Tries to extract columns ('col') from an object ('obj').
#' If it fails, then it provides hints on the names in 'obj' that match closest to 'col'.
#' @param obj The object to extract columns from. Can be a matrix or data.frame
#' @param col The columns to extract.
#' @param expr Optionally instead of 'col', an expression can be given that should be evaluated. E.g. you could do this: \code{getCol(df, expr=with(df, a <- b))}
getCol <- function (obj, col=NULL, expr=NULL) {
  if (missing(obj))
    stop ("argument \"obj\" is missing, with no default")
  if (is.null(expr) && is.null(col))
    col <- colnames(obj)

  .extractStringFromBetween <- function(s, before, after) {
    r1 <- regexpr(before, s)
    r2 <- regexpr(after, s)
    if (r1 != -1 && r2 != -1)
      return (substr(s, r1+attr(r1,"match.length"), r2-1))
    return (NULL)
  }

  tryCatch({
    if (is.null(col)) {
      return (expr)
    } else {
      return (obj[,col])
    }
  }, error=function (e) {
    # Extract message and call from error.
    msg <- e$message
    callVec <- as.character(e$call)
    callString <- deparse(e$call)
    objName <- .extractStringFromBetween(msg, "object '", "' not found")
    # Extract the colnames/names from obj
    cn <- if (is.matrix(obj)) colnames(obj) else
      if (is.list(obj)) names(obj) else
        if (mode(obj) == "character") obj else
          stop ("obj must be a matrix, data.frame, named list or charcter vector.")
    if (length(objName) == 0) {
      if (msg == "undefined columns selected") {
        # Error in `[.data.frame`(spb, , a) : undefined columns selected
        if (callVec[1] == "[.data.frame" && length(callVec) == 4) {
          objName <- callVec[4]
          # If no quote was given in the call String, then we must evaluate the given vector containing the colnames
          # Case where: df[,a] -> This produced the error, hence evaluate what's inside a.
          # Else, we already have the string in 'objName'. Nothing more to do.
          # Case where:  df[,"asdf"] -> This produced the error.
          if (!grepl("\"", callString)) {
            objName <- eval(parse(text=objName), envir = if (is.null(expr)) environment() else parent.frame())
          }
        }
      } else if (msg == "subscript out of bounds") {
        # Error in spe[, a] : subscript out of bounds
        # Same logic as for data.frames
        if (callVec[1] == "[" && length(callVec) == 4) {
          objName <- callVec[4]
          if (!grepl("\"", callString)) {
            objName <- eval(parse(text=objName), envir = if (is.null(expr)) environment() else parent.frame())
          }
        }
      }
      if (length(objName) > 1) {
        objName <- objName[!objName%in%cn][1]
      }
    }

    # If an object name could be extracted, carry on.
    if (length(objName)>0) {
      # Alternative functions: utils::adist, utils::agrep
      options(install.packages.check.source = "no")
      .install.package <- function (x) { x <- x[!x%in%rownames(installed.packages())]; if (length(x>0)) install.packages(x, lib = .libPaths()) }
      options(install.packages.check.source = "both")
      installFromCRAN("stringdist")
      # Get the most similar names from cn and create error message.
      if (is.null(cn)) {
        txtToMsg <- NULL
      } else if (TRUE) {
        objNameLower <- tolower(objName)
        cnLower <- tolower(cn)
        similarCn1 <- cn[startsWith(cnLower, objNameLower)]
        similarCn1 <- similarCn1 [order(nchar(similarCn1))]
        if (length(similarCn1) > 0)
          similarCn1 <- similarCn1[1:min(length(similarCn1), 10)]
        similarCn2 <- cn[endsWith(cnLower, objNameLower)]
        similarCn2 <- similarCn2 [order(nchar(similarCn2))]
        if (length(similarCn2) > 0)
          similarCn2 <- similarCn2[1:min(length(similarCn2), 10)]
        dist <- stringdist::stringdist(objNameLower, cnLower, method="jw", p=0)
        similarCn3 <- cn[order(dist)][1:min(length(cn), 10)]
        similarCn <- unique(c(similarCn1, similarCn2, similarCn3))
        txtToMsg <- paste0("\nInstead of '", objName, "', did you mean one of: ", paste0(similarCn, collapse=", "))
      }
      e$message <- paste0(e$message, txtToMsg)
      stop (e)
    } else {
      # If no object name was detected, return the standard error message
      stop (e)
    }
  })
}

# Test of getCol
if (FALSE) {
  spb <- load.spb.gb()
  spe <- as.matrix(load.spe.gb())
  a <- c("GVE_Mutterkuehe","Arbeit")
  getCol(spb, a)
  getCol(spe, a)
  getCol(spb, "Arbeit")
  getCol(spe, "Arbeit")
  getCol(spb, expr=with(spb, a <- Arbeit))
}


#' Integrates the dimnames of an array into the array itself
#' @export
#' @author Daniel Hoop
#' @param array The array of which the dimnames should be integrated.
#' @param sep.sign The separator between the integrated dimnames.
#' @examples
#' # Example 1 - matrix with row & colnames
#' array <- matrix(0, ncol=3, nrow=3); dimnames(array) <- list(1:3, c("a","b","c"));
#' c.dimnames(array)
#'
#' # Example 2 - matrix without rownames
#' array <- matrix(0, ncol=3, nrow=3); colnames(array) <- c("a","b","c");
#' c.dimnames(array)

#' # Example 3 - 3 dimensional array
#' array <- array(0, dim=c(3,4,5), dimnames=list(c("c","b","a"),c(4,3,2,1),c("z","y","x","v","u")))
#' c.dimnames(array)
c.dimnames <- function(array, sep.sign="_"){

  if(is.null(dim(array))) stop("Input has to be an array, not vector.")

  dn1 <- dimnames(array)
  dn1.1 <- dn1[[1]]; if(is.null(dn1.1)) dn1.1 <- rep("",dim(array)[[1]])
  dn1.2 <- dn1[[2]]; if(is.null(dn1.2)) dn1.2 <- rep("",dim(array)[[2]])

  res0 <- paste( rep(dn1.1, length(dn1.2)) , rep(dn1.2, each=length(dn1.1)) , sep=sep.sign)
  if(length(dn1)>2){
    for(i in 3:length(dn1)){
      dn1.1 <- res0
      dn1.2 <- dn1[[i]]; if(is.null(dn1.2)) dn1.2 <- rep("",dim(array)[[i]])
      res0 <- paste( rep(dn1.1, length(dn1.2)) , rep(dn1.2, each=length(dn1.1)) , sep=sep.sign)
    }
  }
  res1 <- array
  res1[] <- res0
  return(res1)
}

#' Integrate dimnames to a matrix.
#' @export
#' @author Daniel Hoop
#' @param x The matrix into which the dimnames should be integrated.
#' @param dimns Character specifying which dimnaes should be integrated. Either \code{"rowcol"}, meaning both, \code{"row"}, meaning rownames, or \code{"col"}, meaning colnames only.
#' @details This function is needed for the function \code{\link{c.matrices}}
#' @examples
#' x <- data.frame(testCol=1:10)
#' y <- data.frame(testCol2=1:5)
#' dimnames.to.mat(x, dims="cpl")
#' c.matrices(x, y, integrate.dimnames="col")
dimnames.to.mat <- function(x, dims=c("rowcol","row","col")){

  dims <- match.arg(dims)
  #if(is.null(dim(x))) x <- as.matrix(x)
  rn1 <- if (is.null(dim(x))) NULL else rownames(x)
  x <- as.matrix(x)
  rownames(x) <- rn1

  nu <- function(x) {
    tryCatch(as.numeric(x), warning = function(e) x)
  }

  if(is.null(dimnames(x))) return(x)
  if(is.null(rownames(x))) rownames(x) <- rep("", nrow(x))
  if(is.null(colnames(x))) colnames(x) <- rep("", ncol(x))
  if(dims=="rowcol") {
    res <- rbind(c(NA, nu(colnames(x))),
                 cbind(nu(rownames(x)),x))
    if (is.character(res))
      res[1] <- ""
  }
  if(dims=="row") res <- cbind(nu(rownames(x)),x)
  if(dims=="col") res <- rbind(nu(colnames(x)),x)
  dimnames(res) <- NULL
  return(res)
}

#' Combine multiple matrices into one large matrix.
#' @export
#' @author Daniel Hoop
#' @param ... The matrices to be combined.
#' @param fill The character that will be written into the separating rows/cols that were added between the combined matrices.
#' @param nbreak The number of rows/cols that should be added between the combined matrices.
#' @param aligned If the matrices don't have the same number of columns/rows, then the rest of the empty cells will be filled with \code{""}. Should all matrices be left- or right-alinged?
#' @param integrate.dimnames Character specifying, if the dimnaes of the matrices should be integrated into the resulting matrix. This can also be done manually by using the function \code{\link{dimnames.to.mat}}.
#' @param func The function to be applied. \code{"rbind"} or \code{"cbind"}.
#' @examples
#' x <- data.frame(testCol=1:10, testC=10:1)
#' y <- data.frame(testCol2=1:5, testCol3=2:6, testcol4=11:15)
#' z <- 1:3; func=c("rbind","cbind")[1]; fill=""
#' c.matrices(x, y, z, nbreak = 1, aligned = "left", integrate.dimnames = "rowcol")
#' c.matrices(list(x, y, z), nbreak = 1, aligned = "right", integrate.dimnames = "col")
c.matrices <- function(..., fill="", nbreak=0, aligned=c("left","right"), integrate.dimnames=c("no","rowcol","row","col"), func=c("rbind","cbind")) {
  # Diese Funktion vergleicht die Anzahl Spalten aller gegebenen Matrizen, gleicht sie an und verbindet alle Matrizen in einer einzigen.
  #
  # Arguments
  # Mit fill kann gewaehlt werden, was fuer ein Zeichen fuer das Auffuellen der zusaetzlichen Spalten verwendet wird.
  # nbreak gibt an, wie viele Zeilen zwischen zwei Ursprungs-Matrizen eingefuegt werden.
  # Mit integrate.dimnames kann man die dimnames in die End-Matrix integrieren, was mittels dimnames.to.mat() geschieht.
  # func: Funktion mit welcher die Matrizen zuammengefuehrt werden sollen.

  func <- match.arg(func)
  aligned <- match.arg(aligned)
  integrate.dimnames <- match.arg(integrate.dimnames)

  li <- list(...)
  if (length(li)==1 && is.list(li[[1]]) && !is.data.frame(li[[1]]))
    li <- li[[1]]

  # Kick NULL elements from the list
  li[sapply(li, is.null)] <- NULL
  # Transform not matrices to matrices & transpose in case of cbind
  li <- lapply(li, function(x) {
    if (is.null(dim(x)))
      x <- t(as.matrix(x))
    if (func == "cbind")
      x <- t(x)
    return(x)
  })
  if (integrate.dimnames!="no")
    li <- lapply(li, function(x) dimnames.to.mat(x, dims=integrate.dimnames))
  ncolmax <- max(unlist(lapply(li, function(x) ncol(x) )))

  tryCatch({
    res <- do.call("rbind",
                   lapply( li,function(x) {
                     if(ncol(x)<ncolmax) {
                       if(aligned=="left") {
                         x <- cbind(x,array(fill,dim=c(nrow(x),ncolmax-ncol(x))))
                       } else {
                         x <- cbind(array(fill,dim=c(nrow(x),ncolmax-ncol(x))),x)
                       }
                     }
                     if(nbreak==0) x else rbind(x,matrix(fill,nrow=nbreak,ncol=ncol(x)))
                   }))
  }, error = function(e) {
    stop("If different colnames are provided, you must choose integrate.names=\"col\" or \"rowcol\" ")
  })
  if (nbreak>0)
    res <- res[ -c((nrow(res)-nbreak+1):nrow(res)), , drop=FALSE]
  if (func=="cbind")
    res <- t(res)
  return(res)
}
#' Deprecated, use \code{\link{c.matrices}} instead
#' @export
#' @author Daniel Hoop
merge.matrices <- function (...) {
  warning("The function `merge.matrices` is deprecated. Use `c.matrices` instead.")
  return (c.matrices(...))
}

#' @title Converts an array with 3 dimension to a matrix (with 2 dimensions).
#' @description This is especially useful if you want to export a 3 dimensional array into a csv file.
#' The matrix shows the third dimension of the original array row by row as 'sub-matrices'.
#' @export
#' @author Daniel Hoop
#' @param x array to be converted. (array with 3 dimensions)
#' @param sep.line Should the sub-matrices be separated by the sep.sign? (logical)
#' @param sep.sign The sign to separate the sub-matrices (character)
#' @param keep.colnames Should the colnames be written into the result matrix as pseudo colnames above each sub-matrix? (logical)
#' @param keep.dim3names Should the dimnames of the 3rd dimension be written into the rownames of the result matrix? (logical)
#' @examples
#' x <- array(0, dim=c(5,5,2), dimnames=list(c("asdf1","asdf2","asdf3","asdf4","asdf5"),c("asdf1","asdf2","asdf3","asdf4","asdf5"),c("dim3.1", "dim3.2")))
#' dim3.to.mat(x, sep.line=TRUE, sep.sign=NA, keep.colnames=TRUE, keep.dim3names=TRUE)
#' dim3.to.mat(list(a=matrix(1:10, ncol=2), b=matrix(11:20, ncol=2), NULL, c=matrix(21:30, ncol=2), NULL, a=matrix(31:40, ncol=2)))
dim3.to.mat <- function(x, sep.line=TRUE, sep.sign=NA, keep.colnames=TRUE, keep.dim3names=TRUE){

  if(length(dim(x))!=3 & !is.list(x))
    stop("x must be an array with 3 dimensions or a list.")

  # Make an 3 dimensional array out of a list.
  if(is.list(x)){
    allDims <- do.call("rbind",lapply(x, function(x) {
      if (is.null(x))
        return (NULL)
      di <- dim(x)
      if (is.null(di))
        stop ("There must be no list places with is.null(dim(x)).")
      return (di)
    }))
    apply(allDims, 2, function(x) if (any(x!=x[1])) stop("All list places must have the same dimensions."))
    xNew <- array(NA, dim=c(allDims[1,],nrow(allDims)), dimnames=list(rownames(x[[1]]), colnames(x[[2]]), rownames(allDims)))
    nNull <- 0
    for (i in 1:length(x)) {
      if (is.null(x[[i]])) {
        nNull <- nNull + 1
        next
      }
      xNew[,,i-nNull] <- as.matrix(x[[i]])
    }
    x <- xNew
    rm(xNew)
  }

  # Preparations
  di <- dim(x)
  cnx <- colnames(x)
  ncx <- ncol(x)

  if(keep.dim3names & !keep.colnames) sep.line <- TRUE
  if(is.null(colnames(x))) keep.colnames <- FALSE
  if(is.null(rownames(x))) rownames(x) <- 1:nrow(x)
  if(is.null(dimnames(x)[[3]]) & keep.dim3names) dimnames(x)[[3]] <- 1:dim(x)[3]

  # Transform colnames to numeric if there are no letters. If there were letters, it would result in a warning "NAs introduced by coercion"
  if( !is( tryCatch(as.numeric(cnx),error=function(e)e,warning=function(w)w), "warning") ) cnx <- as.numeric(cnx)

  res <- NULL
  for(i in 1:dim(x)[3]){
    res <- rbind( res, if(sep.line)rep(sep.sign ,ncx), if(keep.colnames)cnx,  x[,,i])
  }
  if(sep.line & !(keep.dim3names&!keep.colnames)) res <- res[-1,]

  if(keep.dim3names){
    if(!sep.line &  keep.colnames) places <- seq(1, nrow(res), 1+dim(x)[1])
    if( sep.line &  keep.colnames) places <- seq(1, nrow(res), 2+dim(x)[1])
    if( sep.line & !keep.colnames) places <- seq(1, nrow(res), 1+dim(x)[1])
    rownames(res)[places] <- dimnames(x)[[3]]
  }

  return(res)
}

#' @title Repeat each element in vector one by one.
#' @description Same as rep(vector, each = times)
#' @export
#' @author Daniel Hoop
rep.1b1 <- function(vector, times){
  if(length(times)==1) {
    return(rep(vector, each=times))
  } else {
    if(length(vector)!=length(times)) stop("If length(times)>1 then condition length(vector)==length(times) must hold.")
    return(rep(vector, times))
  }
}

#' Repeat rows of matrix (like \code{\link[base:rep]{base::rep}}).
#' @export
#' @author Daniel Hoop
rep.rows <- function(x, times){
  if(length(times)>1) stop("length(times) must be equal 1, else repetition is done one by one.")
  return(x[rep(1:nrow(x),times),,drop=FALSE])
}

#' Repeat rows of matrix (like \code{\link[base:rep]{base::rep}}), but each row one by one.
#' @export
#' @author Daniel Hoop
rep.rows.1b1 <- function(x, times){
  return(x[rep.1b1(1:nrow(x),times),,drop=FALSE])
}

#' Concenates given vectors ... element by element.
#' @export
#' @author Daniel Hoop
#' @inheritParams rbind.1b1
#' @seealso \code{\link{rbind.1b1}}
#' @examples
#' a <- 1:10
#' names(a) <- LETTERS[1:10]
#' b <- c <- a
#' c.1b1(a,b,c,add.names="none", sep.sign="_")
#' c.1b1(a,b,c,add.names="num", names.at.front=FALSE, sep.sign="_")
#' c.1b1(a,b,c,add.names="own.names", own.names=c("x","y","z"), names.at.front=TRUE, sep.sign="_")
c.1b1 <- function(..., add.names=c("char","num","obj.names","own.names","none"), own.names=NULL, names.at.front=FALSE, sep.sign="_") {

  add.names <- match.arg(add.names)

  # Create List and delete NULL elements.
  dat <- list(...)
  dat <- dat[ sapply(dat,function(x)!is.null(x)) ]

  # Test if all length is the same for all arguments.
  n.arg <- length(dat)
  length.arg.logical <- logical()
  for(i in 1:n.arg) length.arg.logical[[i]] <- length(dat[[1]]) != length(dat[[i]])
  if(any(length.arg.logical)) stop("length of all arguments must be equal!")
  length.dat <- length(dat[[1]])

  # Concenate in the wrong order
  res <- do.call("c", dat)
  # Now order correctly
  res <- res [order(rep(1:length.dat, n.arg))]

  if(add.names=="char") {
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(letters[1:n.arg],length.dat))
    if( names.at.front) names(res) <- paste0(rep(letters[1:n.arg],length.dat), sep.sign, names(res))
  } else if(add.names=="num") {
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(1:n.arg,length.dat))
    if( names.at.front) names(res) <- paste0(rep(1:n.arg,length.dat), sep.sign, names(res))
  } else if(add.names=="obj.names") {
    name <- as.list(substitute(list(a,b,c)))[-1L]
    obj.names <- NULL
    for(i in 1:length(name)) obj.names <- c(obj.names, as.character(name[[i]]))
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(obj.names,length.dat))
    if( names.at.front) names(res) <- paste0(rep(obj.names,length.dat), sep.sign, names(res))
  } else if(add.names=="own.names"){
    if(is.null(own.names)) stop("specify own.names!")
    if(length(own.names)!=n.arg) stop("length(own.names) must be equal the number of objects to bind!")
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(own.names,length.dat))
    if( names.at.front) names(res) <- paste0(rep(own.names,length.dat), sep.sign, names(res))
  }
  return(res)
}


#' Binds all given matrices in ... row by row.
#' @param add.names Adds endings to the rownames such that you can see which row comes from which object.
#' @param sep.sign Separates the original rownames from the add.names-endings
#' @param add.names If add.names="own.names" then own.names must be given.
#' @param cbind Is only used for the \code{\link{cbind.1b1}} function (below) which is some kind of wrapper function for \code{rbind.1b1}.
#' @seealso \code{\link{cbind.1b1}}
#' @examples
#' a <- b <- c <- matrix(1:100,nrow=10,dimnames=list(letters[1:10],letters[11:20]) )
#' rbind.1b1(a, b, c, add.names="none")
#' cbind.1b1(a ,b, c, add.names="none")
rbind.1b1 <- function(..., add.names=c("char","num","obj.names","own.names","none"), own.names=NULL, names.at.front=FALSE, sep.sign="_", cbind=FALSE) {

  add.names <- match.arg(add.names)
  # Create List and delete NULL elements.
  dat <- list(...)
  if(!is.data.frame(dat[[1]]) && is.list(dat[[1]]) && length(dat)==1) dat <- dat[[1]]
  dat <- dat[ sapply(dat,function(x)!is.null(x)) ]
  if(cbind) dat <- lapply(dat,function(x)t(x))

  # Test if all nrow is the same for all arguments.
  n.arg <- length(dat)
  nrow.arg.logical <- logical()
  for(i in 1:n.arg) nrow.arg.logical[[i]] <- nrow(dat[[1]]) != nrow(dat[[i]])
  if(any(nrow.arg.logical)) stop("nrow of all arguments must be equal!")
  nrow.dat <- nrow(dat[[1]])

  # Test if all ncol is the same for all arguments.
  ncol.arg.logical <- logical()
  for(i in 1:n.arg) ncol.arg.logical[[i]] <- ncol(dat[[1]]) != ncol(dat[[i]])
  if(any(ncol.arg.logical)) stop("ncol of all arguments must be equal!")
  #ncol.dat <- ncol(dat[[1]])

  # Bind the rows with wrong order
  res <- do.call("rbind", dat)
  # Now order correctly
  res <- res[order(rep(1:nrow.dat, n.arg)),,drop=FALSE]
  # Get original rownames and order correctly.
  rn_res <- unlist(lapply(dat, function(x)rownames(x)))
  rn_res <- rn_res[order(rep(1:nrow.dat, n.arg))]

  if(add.names=="char") {
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(letters[1:n.arg],nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(letters[1:n.arg],nrow.dat), sep.sign, rn_res)
  } else if(add.names=="num") {
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(1:n.arg,nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(1:n.arg,nrow.dat), sep.sign, rn_res)
  } else if(add.names=="obj.names") {
    name <- as.list(substitute(list(a,b,c)))[-1L]
    obj.names <- NULL
    for(i in 1:length(name)) obj.names <- c(obj.names, as.character(name[[i]]))
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(obj.names,nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(obj.names,nrow.dat), sep.sign, rn_res)
  } else if(add.names=="own.names"){
    if(is.null(own.names)) stop("specify own.names!")
    if(length(own.names)!=n.arg) stop("length(own.names) must be equal the number uf rbind arguments")
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(own.names,nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(own.names,nrow.dat), sep.sign, rn_res)
  }
  return(res)
}

#' Binds all given matrices in ... column by column.
#' @export
#' @author Daniel Hoop
#' @inheritParams rbind.1b1
#' @seealso \code{\link{cbind.1b1}}
#'
cbind.1b1 <- function(...){
  return(t( rbind.1b1(...,cbind=TRUE)) )
}

#' Flips matrix upside down
#' @keywords internal
#' @author Daniel Hoop
#'
upside.down <- function(x){
  return(x[nrow(x):1,])
}
#' Flips matrix left to right  (same as \code{\link{right.to.left}}).
#' @keywords internal
#' @author Daniel Hoop
#'
left.to.right <- function(x){
  return(x[,ncol(x):1])
}

#' Flips matrix right to left (same as \code{\link{left.to.right}}).
#' @keywords internal
#' @author Daniel Hoop
#'
right.to.left <- left.to.right # match.fun("left.to.right")

# if(FALSE){
#   what <- 1:10; inobject <- matrix(1:100,nrow=10); where <- 9:10; how <- "rbind"
#   insert(what,inobject,where,how)
#   what <- "X"; inobject <- 1:10; where <- 9:10; how <- "c"
#   insert(what,inobject,where,how)
#   what <- list("x","y","z"); inobject <- list(1,2,3,4,5,6,7,8,9,10); where <- 9:10; how <- "list"
#   insert(what,inobject,where,how)
# }
# insert <- function(what, inobject, where, how=c("c","list","rbind","cbind")){
#   how <- match.arg(how)
#   where <- rev(sort(where))
#   if(is.list(inobject) & !is.data.frame(inobject)) how <- "list" else if(is.null(dim(inobject))) how <- "c"
#   if(how=="c") {
#     for(ii in 1:length(where)){
#       if(where[[ii]]==1) {
#         inobject <- c(what, inobject[1:length(inobject)]      )
#         #} else if(where[[ii]]==length(inobject)) {
#         #  inobject <- c(      inobject[1:length(inobject)], what)
#       } else {
#         inobject <- c(inobject[1:(where[[ii]]-1)], what, inobject[where[[ii]]:length(inobject)])
#       }
#     }
#     return(inobject)
#
#   } else if(how=="list") {
#     for(ii in 1:length(where)) {
#       newlist <- list()
#       #if(where[[ii]]%in%c(1,length(inobject))) stop("where cannot be 1 or length(inobject)")
#       if(where[[ii]]==1){
#         #if(is.list(what)) {
#         #    for(i in 1:length(what)) newlist[[i]] <- what[[i]]
#         #    for(i in 1:length(inobject)) newlist[[i+length(what)]] <- inobject[[i]]
#         #  } else {
#         newlist[[1]] <- what
#         for(i in 1:length(inobject)) newlist[[i+1]] <- inobject[[i]]
#         #  }
#         inobject <- newlist
#
#         #} else if(where[[ii]]==length(inobject)){
#         #newlist <- inobject
#         ##if(is.list(what)) {
#         ##  for(i in 1:length(what)) newlist[[i+length(inobject)]] <- what[[i]]
#         ##} else {
#         #newlist[[length(inobject)+1]] <- what
#         ##}
#         #inobject <- newlist
#
#       } else {
#         for(i in 1:(where[ii]-1)) newlist[[i]] <- inobject[[i]]
#         #if(is.list(what)){
#         #  for(i in     1:length(what)    ) newlist[[i+(where[ii]-1)]]    <- what[[i]]
#         #  for(i in where[ii]:length(inobject)) newlist[[i+length(what)]] <- inobject[[i]]
#         #} else {
#         newlist[[where[ii]]] <- what
#         for(i in where[ii]:(length(inobject))) newlist[[i+1]] <- inobject[[i]]
#         #}
#         inobject <- newlist
#       }
#     }
#     return(newlist)
#
#   } else if(how=="rbind") {
#     for(ii in 1:length(where)){
#       if(where[[ii]]==1) {
#         inobject <- rbind( what, inobject[1:nrow(inobject),,drop=FALSE] )
#         rownames(inobject)[1] <- ""
#         #} else if (where[[ii]]==nrow(inobject)) {
#         #  inobject <- rbind(       inobject[1:nrow(inobject),,drop=FALSE], what )
#       } else {
#         inobject <- rbind( inobject[1:(where[ii]-1),,drop=FALSE], what, inobject[where[ii]:nrow(inobject),,drop=FALSE] )
#         rownames(inobject)[where[[ii]]] <- ""
#       }
#     }
#     return(inobject)
#
#   } else if(how=="cbind") {
#     for(ii in 1:length(where)){
#       if(where[[ii]]==1) {
#         inobject <- cbind( what, inobject[,1:nrow(inobject),drop=FALSE]        )
#         colnames(inobject)[1] <- ""
#         #} else if (where[[ii]]==ncol(inobject)) {
#         #  inobject <- cbind(       inobject[,1:nrow(inobject),drop=FALSE], what  )
#       } else {
#         inobject <- cbind( inobject[,1:(where[ii]-1),drop=FALSE], what, inobject[,where[ii]:ncol(inobject),drop=FALSE] )
#         colnames(inobject)[where[[ii]]] <- ""
#       }
#     }
#     return(inobject)
#
#   }
# }

#### SUMMARIES & MEANS ####

trim <- function(x, probs=c(0.025,0.975)) {
  # This function trims a distribution and gives back only those values that are within the trimming limits.
  # Arguments:
  # x     = vector of values
  # probs = probabilities to exclude extreme values using the quantlie() function. Both quantile probabilities are included like: prob1[1] <= x <= probs[2]

  if(length(probs)!=2) stop("probs must be a vector of length 2")
  q1 <- quantile(x, sort(probs), na.rm=TRUE)
  return(x[ which(x>=q1[1] & x<=q1[2]) ]) # Use which to exclude NA values.
}

summarysd <- function(x,na.rm=TRUE,digits=2,...) {
  if(is.null(x)) {
    result <- rep(NA,7);  names(result) <- c("Min.", "1st Qu.","Media","Mean","3rd Qu.","Max.","SD")
    return(result)
  } else if(length(x)==0) {
    result <- rep(NA,7)
    names(result) <- c("Min.", "1st Qu.","Media","Mean","3rd Qu.","Max.","SD")
    return(result)
  } else if(all(is.na(x))) {
    result <- rep(NA,7)
    names(result) <- c("Min.", "1st Qu.","Media","Mean","3rd Qu.","Max.","SD")
    return(result)
  } else {
    return(round(c(summary(x,na.rm=na.rm,...),"SD"=sd(x,na.rm=na.rm,...))),digits=digits)
  }
}
####
summaryna <- function(x, ...) {
  # Recursive function definition in case of matrix or data.frame.
  if(is.matrix(x)) {
    return(apply(x,2,function(x)summaryna(x, ...)))
  } else if (is.data.frame(x)) {
    return(sapply(x,function(x)summaryna(x, ...)))
  }
  # This is the actual function.
  sum <- summary(x, ...)
  if(length(sum)<7)    sum <- c(sum,"NA's"=0)
  return(sum)
}

####
renumber <- function(x){
  # Integrated function for clarity
  replace.values <- function(search, replace, x){
    return(replace[ match(x, search) ])
  }
  # Replacement here
  ux <- unique(x)
  return(replace.values(ux, 1:length(ux), x))
}

####
if(FALSE){
  data <- cbind(1:1000, 100:1099)
  #data <- as.data.frame(data)
  colnames(data) <- c("asbasdf","asdfgawer")
  data <- data.frame(asbasdf=1:1000, asdfgawer=100:1099, rep("a", 1000))

  weights <- rnorm(1000,1,0); weights[weights<0] <- 0
  index <- list(sample(c(1,2),1000,replace=TRUE))
  #index <- list(sample(c(1,2),1000,replace=TRUE), sample(c(3,4),1000,replace=TRUE))
  #index <- NULL
  na.rm <- TRUE
  summary.long(data)
  summary.long(data[,1,drop=FALSE])

  x <- data; quant=10;digits=2;na.rm=TRUE;margin=2;reverse=FALSE
}

#' 'Long' summary, meaning that more quantiles will be calculated, compared to the \code{\link{summary}} function.
#' @export
#' @author Daniel Hoop
summary.long <- function(x,quant=10,digits=2,na.rm=TRUE,margin=2,reverse=FALSE,...) {
  # Gives a "long" summary with 11 quantiles from 0 to 1 (default).
  # margin is only used when a dataframe/matrix is given. Then apply() is used.
  # See also: describe()

  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)summary.long(x,quant=quant,digits=digits,na.rm=na.rm,margin=2,reverse=TRUE, ...))  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)summary.long(x,quant=quant,digits=digits,na.rm=na.rm,margin=2,reverse=TRUE, ...)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)summary.long(x,quant=quant,digits=digits,na.rm=na.rm,margin=2,reverse=TRUE, ...)))

  if(!is.numeric(x)) x <- rep(0, length(quant+1))

  result <- numeric()
  quants <- seq(0,1,length.out=quant+1)
  if(reverse) quants <- rev(quants)
  result <- round( quantile(x=x, probs=quants, na.rm=na.rm, ...), digits=digits)
  names(result) <- paste0(round(100*quants,digits=2), "%")
  return(result)
}

summary.quart <- function(x,digits=2,na.rm=TRUE,margin=2,reverse=FALSE,...){
  # Gives a specified summary with Quantiles: Min., 5%, 25%, Median, Mean, 75%, 95%, Max
  # margin is only used when a dataframe/matrix is given. Then apply() is used.

  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)summary.quart(x,digits=digits,na.rm=na.rm,margin=margin,reverse=TRUE,...))  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)summary.quart(x,digits=digits,na.rm=na.rm,margin=margin,reverse=TRUE,...)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)summary.quart(x,digits=digits,na.rm=na.rm,margin=margin,reverse=TRUE,...)))

  if(!is.numeric(x)) x <- rep(0, length(quant+1))

  result <- numeric()

  probs <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  result <- round( quantile(x=x, probs=probs, na.rm=na.rm, ...), digits=digits)
  result <- c(result[1:4], "Mean"=mean(x, na.rm=na.rm), result[5:7])
  names(result) <- c("Min.","5%","25%","Median","Mean","75%","95%","Max.")
  #names(result) <- paste0(round(100*probs,digits=2), "%")

  if(reverse) result <- rev(result)

  return(result)
}


# Beispiel und Parametereinstellung unter der Funktion!
stratif.sqrt.f.rule <- function(x, ngrp, interval){
  # Diese Funktion stratifiziert eine Population (teilt eine Population in Gruppen auf),
  # sodass moeglichst wenige Beobachtungen fuer eine Genaue Schaetzung gebraucht werden(?).
  # Siehe Cochran (1977): Sampling Techniques. 3rd edition. Wiley. p127-130
  #
  # Argumente
  #
  # x = variable of interest according to which the sample should be stratified
  # ngrp =     Anzahl Schichten/Gruppen
  # interval = Genauigkeit, mit welcher die Grenze gebildet werden soll bzw.
  #            Intervalle, mit denen die Daten geteilt werden.
  #
  # Value (Ergebnis) = Vector of values of x that should be used for stratifying.
  # Replace first/last value of vecor with -Inf/Inf and stratify by using the function g1roup.by.fix.scale()

  # Fehlende Werte enfternen
  x <- x[!is.na(x)]

  # Anzahl Betriebe in den einzelnen Intervallen berechnen
  tab <- table(ceiling(x/interval)*interval)
  names_tab <- as.numeric(names(tab))
  # Cumulative Summe der Wurzel berechnen
  cusu <- cumsum(sqrt(tab))
  # Division points berechnen
  maxcusu <- max(cusu)
  divpoint <-  maxcusu/ngrp
  divpoint <- seq(divpoint, maxcusu, length.out=ngrp)
  divpoint <- divpoint[-length(divpoint)]  # letzten Punkt weglassen, da dieser keine Grenze sein kann

  # Naechste Intervall-Punkte zu den Division points finden
  res <- apply(as.matrix(divpoint),1,function(x) names_tab[which.min(abs(x-cusu))] )
  res <- c(min(x), res, max(x))
  # Ergebnis ausgeben
  return(res)
}
if(FALSE){
  ## Beispiel aus Buch nachbilden und ausprobieren.
  # Siehe Cochran (1977): Sampling Techniques. 3rd edition. Wiley. p127-130
  x <- c(rep(2.5,3464), rep(7.5,2516), rep(12.5,2157), rep(17.5,1581), rep(22.5,1142), rep(27.5,746),
         rep(32.5,512), rep(37.5,376), rep(42.5,265), rep(47.5,207), rep(52.5,126), rep(57.5,107),
         rep(62.5,82), rep(67.5,50), rep(72.5,39), rep(77.5,25), rep(82.5,16), rep(87.5,19), rep(92.5,2), rep(97.5,3))
  ngrp <- 5; interval <- 5
  borders <- stratif.sqrt.f.rule(x,5,5)
  borders[1] <- -Inf; borders[length(borders)] <- Inf
  grouping <- g1roup.by.fix.scale(x=x, selection.levels=borders)
  table(grouping)

  ## Daten erzeugen
  x0 <- 1:1300
  x <- x0 + rnorm(length(x0),0,seq(1,20,length.out=length(x0)))
  x <- round(x, 1)
  interval=100; ngrp=5
  stratif.sqrt.f.rule(x,5,20)
}

####
if(FALSE){
  # Beispieldaten vorbereiten fuer mean.weight
  data <- data.frame(1:1000, 100:1099, NA)
  colnames(data) <- c("Variable_1","Variable_2", "I(Variable_1/Variable_2)")
  data[,"Variable_1_pro_Variable_2"] <- data[,"Variable_1"]/ data[,"Variable_2"]
  na.rm=TRUE; digits=2
  weights <- rnorm(1000,40,20); weights[weights<0] <- 0

  # Rechnung mit keinem Index
  index <- NULL
  mean.weight(data,weights,index)

  # Rechnung mit 1-Dimensionalem Index (koennte z.B. das Jahr sein)
  index <- list(sample(c(2012,2013),1000,replace=TRUE))
  mean.weight(data,weights,index)

  # Rechnung mit 2-Dimensionalem Index z.B. Jahr x Region
  index <- list(sample(c("1_Tal","2_Huegel","3_Berg"),1000,replace=TRUE), sample(c(2012,2013),1000,replace=TRUE))
  mean.weight(data,weights,index)

  # Rechnung ohne Index
  index <- NULL
  mean.weight(data,weights,index)

  gb <- load.gb()
  gb[,c("I((LE-eigenZinsanspruch)/JAE_FamAK)","I(LE/LN)","_",".")] <- 0
  mean.weight(data=gb[,c("I((LE-eigenZinsanspruch)/JAE_FamAK)","I((LE-eigenZinsanspruch)/JAE_FamAK)","I(LE/LN)","LE","LN","eigenZinsanspruch","JAE_FamAK","_",".","I(LE/LN)")],
              weights=gb[,"Gewicht"],
              index=gb[,"Jahr"],
              edit.I.colnames=TRUE,del.I.help.columns=FALSE)
}

#' Calculates the weighted mean of all variables in a possibly indexed data.frame or matrix.
#' @export
#' @author Daniel Hoop
#' @param data The data of which the weighted means should be calculated. Can be \code{data.frame}, \code{matrix} or \code{vector}.
#' If any colname of data contains an expression like \code{I(a/b)}, then the the "ratio of means" is calculated.
#' @param weights Optionally: A numeric vector containing the weights for each observation given in \code{x}.
#' @param index Optionally: A numeric vector or \code{data.frame}/\code{list} containing the indices. See also \code{\link[base:tapply]{base::tapply}}.
#' @param cols Optionally: The columns in data for which the means should be calculated. If \code{data} contains the columns \code{"a"} and \code{"b"}, then \code{cols} can also be \code{"I(a/b)"}
#' even though the column \code{"I(a/b)"} is not acually contained in \code{data}.
#' @param trunc.probs Optionally: The probability of quantiles within which the truncated mean should be calculated. Specify the outer probabilities that should be included into the calculation of the mean, such as \code{c(0.025, 0.975)}.
#' @param calc.sum Should \code{sum(data*weights)} be calculated, rather than weighted means? TRUE/FALSE
#' @param digits The number of digits according to which the result will be rounded.
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ignore.I Logical value indicating if expressions like \code{I(a/b)} should be ignored in colnames. In that case, *not* "ratio of means" would be calculated, but "mean of ratios".
#' @param edit.I.colnames Should the colnames containing expressions with \code{I()} be edited, such that \code{I()} won't be there anymore? TRUE/FALSE
#' @param del.I.help.columns Deprecated (use \code{cols} instead): Logical value indicating if the columns provided in \code{I.help.columns} should be removed from the results.
#' @param I.help.columns Deprecated (use \code{cols} instead): Character vector giving 'help' columns that are only relevant to calculate other Columns names like \code{I(a/b)} and should be discarded in the final result
#' @param change.output.str Logical value indicating if the output structure should be changed in case more than 1 index is given in \code{index}.
#' @details If the argument \code{cols} is given, then the result will never be dropped to a vector but always be kept as a matrix, even if only 1 columns was chosen from \code{data}.
#' @examples
#' # Prepare input data
#' data <- as.data.frame(matrix(1:15, ncol=3))
#' colnames(data) <- c("I(a+b)","a","b")
#' weights <- 1:5
#' index <- as.data.frame(matrix(c(2014,2014,2014,2015,2016,   1,2,2,1,1,   11,11,12,13,13),ncol=3))
#' index.of.result=c("2014_2_11","2014_1_11","0000_0_00")
#' # Apply function
#' # By default, all columns in data will be computed
#' mean.weight(data, weights, index)
#' # You can also specify the argument `cols`. If cols are given, the
#' mean.weight(data, weights, index, cols = "I(a/b)")
#' # Apply to a vector
#' mean.weight(data[,1], weights, index)
#' # Apply to a vector, but with fixed fixed.index=TRUE, and specifying the index of the result.
#' mean.weight(data[,1], weights, index, fixed.index=TRUE, index.of.result=index.of.result)
mean.weight <- function(data, weights=NULL, index=NULL, cols=NULL,
                        fixed.index=FALSE, index.of.result=NULL, index.sep="_",
                        trunc.probs=NULL, calc.sum=FALSE, digits=NULL, na.rm=TRUE,
                        ignore.I=FALSE, edit.I.colnames=FALSE, del.I.help.columns=FALSE, I.help.columns=NULL,
                        change.output.str=FALSE){

  # Wenn innerhalb eines Indexes mehrere Indexe als Listen abgelegt sind, wird die Berechnung fuer alle Indexe gemacht.
  #if(is.list(index)){
  #  if(any(sapply(index,function(x)is.list(x)))){
  #    return(do.call("rbind", lapply(index, function(x)mean.weight(data=data, weights=weights, index=x, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))))
  #  }
  #}
  use.I <- !ignore.I

  if (is.list(data) && !is.data.frame(data))
    stop("data must be matrix or data.frame but not a list.")

  if(!is.null(dim(data))) {
    if (!is.null(cols)) {
      if (is.null(dim(data)))
        stop("If `cols` are given, then `data` must be a matrix or data.frame, not a vector.")
      if (use.I) {
        # Check if some cols (possibly in I(a+b) columns) are missing. If yes, throw an error.
        .checkMissingICols(cols, colnames(data))
        # Extract all columns from I() cols and create I()-cols if they don't yet exist in data.
        cols_add <- extract.I.vars(cols, keep.only.necessary=TRUE)
      } else {
        cols_add <- NULL
      }
      data <- create.cols(data, cols)
      cols_all <- c(cols, cols_add)
      data <- data[,cols_all,drop=FALSE]
      if (ignore.I) {
        cols_all <- cols
      }
    } else {
      cols <- colnames(data)
      cols_all <- colnames(data)
      if (use.I)
        .checkMissingICols(cols, colnames(data))
    }
  }

  if (!is.null(trunc.probs)){
    if (length(trunc.probs) != 2)
      stop ("If trunc.probs is not NULL, then it must be a numeric vector of length 2.")
    if (min(trunc.probs) < 0 || max(trunc.probs) > 1)
      stop ("If trunc.probs is not NULL, then it must be a numeric vector of length 2 with values ranging from 0 to 1 (inclusive).")
    trunc.probs <- sort(trunc.probs)
  }

  # Fixed index ausschalten, wenn Index ein Vektor ist. Dann bringt es nichts.
  if (fixed.index && is.null(index.of.result) && !is.list(index))
    stop("fixed.index & is.null(index.of.result) & !is.list(index)   -> fixed.index doesn't have any effect this way. Give index as a list!")

  # Im Falle, dass der index fixiert sein soll, hier die rohe Ergebnisstruktur erstellen.
  if (fixed.index){
    if (is.null(dim(data))) {
      rawResult <- .prepare.fixed.index.result(data=data, index=index, names.result=index.of.result, edit.I.colnames=edit.I.colnames)
    } else {
      # In this case it more efficient to create the result here. Recurive function call will be with fixed.index=FALSE!
      rawResult <- .prepare.fixed.index.result(data=data[,cols,drop=FALSE], index=index, names.result=index.of.result, edit.I.colnames=edit.I.colnames)
    }
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
  }

  # Index muss eine List mit folgender Struktur sein:
  isNullIndex <- is.null(index)
  if (!is.list(index))
    index <- list(index)

  # Im Falle, dass !is.null(dim(data)) folgt eine rekursive Funktionsdefinition!
  if (!is.null(dim(data))) {
    if (!isNullIndex) {
      all.na.index <- sapply(index, function(x) all(is.na(x)))
      if (any(all.na.index))
        stop("The following indices contain only NA values. Please change to a different value (not NA): ", paste0(names(all.na.index)[all.na.index],collapse=","))
    }

    if (!change.output.str && length(index) > 1) {
      index <- list(.paste.elements(index, sep="_", errorMsg="All indices must have same length!"))
    }
    # Wenn !is.null(dim(data))
    # & es keinen oder nur einen Index gibt:
    if (is.null(index) || length(index)==1) {

      # Berechnung rekursiv fuer matrix / data.frame
      if (is.matrix(data)) {
        if (nrow(data)==0) stop("nrow of data is 0")
        result <- apply(data[,cols_all,drop=FALSE], 2, function(x) mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=NULL, index.sep=index.sep, trunc.probs=trunc.probs, calc.sum=calc.sum, digits=digits, na.rm=na.rm, ignore.I=ignore.I, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns, change.output.str=change.output.str))
      } else if (is.data.frame(data)) {
        if (nrow(data)==0) stop("nrow of data is 0")
        #result <- sapply(data, function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=index.of.result, index.sep=index.sep, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
        result <- as.matrix(as.data.frame(lapply(data[,cols_all,drop=FALSE], function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=NULL, index.sep=index.sep, trunc.probs=trunc.probs, calc.sum=calc.sum, digits=digits, na.rm=na.rm, ignore.I=ignore.I, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns, change.output.str=change.output.str)), stringsAsFactors=FALSE))
      }
      # Wieder zu Marix machen, falls es ein Vektor ist
      if (is.null(dim(result))) {
        result <- t(as.matrix(result))
        if (!isNullIndex && length(index)==1 && length(unique(index[[1]]))==1)
          rownames(result) <- index[[1]][1]
      }
      if (nrow(result) == 1 && is.null(rownames(result)) && !is.null(index) && !is.null(index[[1]])) {
        rownames(result) <- sort(unique(index[[1]]))
      }
      # Wieder die alten colnames vergeben
      colnames(result) <- cols_all

      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      if (use.I) {
        cn.res <- colnames(result) # cn.res.orig
        icols <- substr(cn.res,1,2)=="I("
        if (any(icols)){
          if (!is.null(digits))
            stop("When rounding (i.e. when `digts != NULL`) and using I() columns, the results will not be accurate.")
          # Wert der I() columns berechnen
          result <- calc.I.cols(result, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
          if (edit.I.colnames)
            cols <- .rm.I.from.names(cols)
        }
      }

      # Ergebnis in fixierte Ergebnisstrutkur einfuegen.
      if (fixed.index){
        result <- result[rownames(result)%in%rownames(rawResult),cols,drop=FALSE]
        rawResult[match(rownames(result),rownames(rawResult)),cols] <- result
        result <- rawResult
      }
      # Resultat ausgeben.
      if (nrow(result)==1 && isNullIndex) {
        return(result[1,cols]) #rownames(result) <- NULL
      } else {
        return(result[,cols,drop=FALSE])
        #return (result[, cols])
      }

      # Wenn !is.null(dim(data))
      # & 2 Indexe eingegeben wurden:
    } else if (length(index)==2) {
      # res1 <- mean.weight(data=data[,1], weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm)

      # Hier keine Fallunterscheidung zwischen matrix und data.frame einfuegen, sonst funktioniert es nicht!!
      res.prov <- apply(data[,cols_all,drop=FALSE], 2, function(x) mean.weight(data=x, weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm, trunc.probs=trunc.probs) )
      if (!"matrix" %in% class(res.prov))
        res.prov <- t(as.matrix(res.prov))

      res.list <- list()
      su.index1 <- sort(unique(index[[1]]))
      su.index2 <- sort(unique(index[[2]]))
      for(i in 1:ncol(res.prov)){
        res.list[[i]] <- matrix(res.prov[,i],nrow=length(su.index1), ncol=length(su.index2))
        dimnames(res.list[[i]]) <- list(su.index1, su.index2)
      }
      names(res.list) <- cols_all

      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      if (use.I) {
        cn.res <- names(res.list)
        icols <- grepl("I\\(", cn.res)
        if (any(icols)){
          if (!is.null(digits))
            stop("When rounding (i.e. when `digts != NULL`) and using I() columns, the results will not be accurate.")
          #if (any(cn.res%in%c("_","."))) stop("When using I() colnames _ and . are not allowed.")
          res.list <- calc.I.cols(res.list, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
          if (edit.I.colnames)
            cols <- .rm.I.from.names(cols)
        }
      }
      return(res.list[cols])

    } else if (length(index)>2) {
      stop("More than 2 indexes not possible if data is a matrix/data.frame. Please enter data as vector.")
    }
  }

  # Tatsaechliche mean.weight() Funktion.
  # Falls es keine numerische Variable ist (weil z.B. ein durchmischter data.frame eingegeben wird),
  # wird daraus eine 0 gemacht, damit die Funktion trotzdem funktioniert.
  if (!(is.numeric(data) || is.logical(data)))
    data <- rep(0, length(data))
  if (is.null(weights))
    weights <- rep(1,length(data))
  if (!is.numeric(weights))
    stop ("`weights` must be numeric.")

  # Falls kein index gegeben wurde, einfache Berechnung (mit weighted.mean)
  if (is.null(index) || is.null(index[[1]])) {
    if (!is.null(trunc.probs)) {
      q12 <- quantile.weight(data, weights = weights, probs = trunc.probs, na.rm=na.rm)
      filter <- q12[1] <= data & data <= q12[2]
      data <- data[filter]
      weights <- weights[filter]
    }
    if (calc.sum){
      result <- sum(data * weights, na.rm=na.rm)
    } else {
      result <- weighted.mean(data, weights, na.rm=na.rm)
    }

    # Sonst muss mit index und tapply() gerechnet werden.
  } else {
    index <- lapply(index, function(x)
      if (length(x)==1) return (rep(x,length(weights))) else return (x))
    length.index <- sapply(index, length)
    if (any(length.index!=length.index[1]))
      stop ("All vectors in the index have to have the same length!")
    if (!all(length(weights)==length.index))
      stop ("length(weights)!=length(index)")

    # Daten auf quantile einschraenken.
    if (!is.null(trunc.probs)) {
      if (length(index) > 1)
        stop ("If trunc.probs is not NULL, then only one dimensional indices can be chosen.")
      newData <- do.call("rbind", by(data.frame(data=data, weights=weights, index=index[[1]]), index[[1]], function (x){
        q12 <- quantile.weight(x[,"data"], weights = x[,"weights"], probs = trunc.probs, na.rm=na.rm)
        filter <- q12[1] <= x[,"data"] & x[,"data"] <= q12[2]
        return (x[filter,])
      }))
      data <- newData[,"data"]
      weights <- newData[,"weights"]
      index <- newData[,"index"]
    }

    # NA Werte in weights uebertragen. Muss so sein, nicht mit na.rm innerhalb der Funktionen, da sonst data und weights evtl. nicht korrespondieren!!
    dataweights <- data*weights
    weights[is.na(dataweights)] <- NA

    if (calc.sum){
      # Resultat = Summe ( Werte * Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm)
    } else {
      # Resultat = Summe ( Werte * Gewichte )                             / Summe( Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm) / tapply(weights,index,  sum,na.rm=na.rm)
    }

    # Resultat in vorgefertige fixierte Index-Struktur einfuegen
    if (fixed.index){
      result <- result[names(result)%in%names(rawResult)]
      rawResult[match(names(result),names(rawResult))] <- result
      result <- rawResult
    }
  }

  # Falls gewuenscht, runden, dann Ergebnis ausgeben.
  if (!is.null(digits)) result <- round(result, digits)
  return(result)
}

#' This function is internally used in \code{\link{mean.weight}}.
#' @keywords internal
#' @author Daniel Hoop
.prepare.fixed.index.result <- function(data, index, names.result, index.sep="_", edit.I.colnames=FALSE){
  if(!is.null(dim(data))){
    rawResult <- tapply.fixed(X=data[,1], INDEX=index, FUN=sum, names.result=names.result, vector.result=TRUE)
    nam <- names(rawResult)
    rawResult <- matrix(NA, nrow=length(rawResult), ncol=ncol(data))
    rownames(rawResult) <- nam;
    if(!edit.I.colnames) {
      colnames(rawResult) <- colnames(data)
    } else {
      colnames(rawResult) <- .rm.I.from.names(colnames(data))
    }
  } else {
    rawResult <- tapply.fixed(X=data, INDEX=index, FUN=sum, names.result=names.result, sep.sign=index.sep, vector.result=TRUE)
    rawResult[] <- NA
  }
  return(rawResult)
}

#' Extracts all variables in a \code{I(a+b*c)} formula seperately. This is useful in combination with the function \code{\link{mean.weight}}.
#' @keywords internal
#' @author Daniel Hoop
#vars <- c("I(asd)","efe+p", "c-1", "f*c", "a/ b", "A^B", "c,d", "a==b", "ifelse(a==b, NA, NaN)", "round  (1, 0)", "if (a==b) {a} else {b}", "ifelse(P100_1100_22100_NA == 1, 1, 0) * 100")
extract.I.vars <- function(vars, keep.original=FALSE, keep.only.necessary=TRUE, original.single.vars.only=FALSE){
  # Keep the original vector in case it should also be returned by the function.
  varsOrig <- vars
  # Remove function calls, or something before curly braces
  # "round(a)" -> "round(a)"; "{a} else {b}" -> "{a} {b}"
  vars <- gsub("[a-zA-Z0-9._]+ *(?=(\\(|\\{))", "", vars, perl = TRUE) # regex positive lookahead
  # Split with operatos
  operatorRegex <- "\\-|/|\\*|\\+|\\^|,|=|!|<|>|\\(|\\)|\\{|\\}"
  vars_all <- vars[grepl(operatorRegex, vars)]
  vars_all <- trimws(unlist(strsplit(vars_all, operatorRegex)))
  vars_all <- unique(vars_all[vars_all != ""])
  # Only non-numbers, i.e. kick: "1", or "2.3"
  vars_all <- vars_all[is.na(suppressWarnings(as.numeric(vars_all)))]
  # Kick reserved words like NA
  length1 <- length(vars_all)
  reservedWords <- c("NA", "NaN")
  vars_all <- vars_all[!grepl(paste0(paste0("^", reservedWords, "$"), collapse="|"), vars_all)]
  if (length1 != length(vars_all))
    message("extract.I.vars: Some reserved words were discarded. It is one of: ", paste0(reservedWords, collapse=", "), ".")
  if(keep.only.necessary)
    vars_all <- vars_all[!vars_all%in%vars]
  if(keep.original) {
    if (original.single.vars.only) {
      vars_all <- unique(c(varsOrig[!grepl(operatorRegex, varsOrig)], vars_all))
    } else {
      vars_all <- unique(c(varsOrig, vars_all))
    }
  }
  return(vars_all)
}

#' Add column(s) to a data.frame/matrix/list after a certain other column (or list place).
#' @param afterName After this column name in \code{data}, add the other columns given in \code{addNames}.
#' @param addNames Optional: The names to be added/rearranged. This only has to be specified when \code{values} has no names.
#' @param values Optional: The values to be added into \code{data}. This only has to be specified when new data should be added rather than only rearranging columns.
#' @param data The data.frame, list, or matrix in which the rearrangement should take place.
#' @return \code{data} with rearranged/added columns.
#' @examples
#' dat <- data.frame(a = 1:3, b = 2:4, z = 3:5)
#' afterName <- "a"
#' values <- list(c = 5:7)
#'
#' # The following examples work without errors.
#' addColsAfter(afterName = afterName, addNames = NULL, values = values, data = dat)
#' addColsAfter(afterName = afterName, addNames = NULL, values = as.data.frame(values), data = dat)
#' addColsAfter(afterName = afterName, addNames = NULL, values = as.matrix(as.data.frame(values)), data = dat)
#' addColsAfter(afterName = afterName, addNames = "x", values = values, data = dat)
#' addColsAfter(afterName = "z", addNames = "x", values = values, data = dat)
#' addColsAfter(afterName = "z", addNames = "z", data = dat)
#' # The following will only rearrange.
#' addColsAfter(afterName = afterName, addNames = "z", values = NULL, data = dat)
#' # The following will rearrange and give new values to column "z".
#' addColsAfter(afterName = afterName, addNames = "z", values = 1:3, data = dat)
#'
#' # The following examples will yield errors.
#' addColsAfter(afterName = afterName, addNames = NULL, values = 1:3, data = dat)
#' values <- list(c = 5:7, d = 6:7)
#' addColsAfter(afterName = afterName, addNames = NULL, values = values, data = dat)
#'
#' # In lists, it is allowed that every list place has different length. Hence, no error.
#' dat <- list(a = 1:3, b = 2:4, z = 1)
#' addColsAfter(afterName = afterName, addNames = NULL, values = values, data = dat)
addColsAfter <- function(afterName, addNames = NULL, values = NULL, data) {
  .addColsBeforeAfter(referenceName = afterName, addNames = addNames, values = values, data = data, before = FALSE)
}


#' Add column(s) to a data.frame/matrix/list before a certain other column (or list place).
#' @param beforeName Before this column name in \code{data}, add the other columns given in \code{addNames}.
#' @param addNames Optional: The names to be added/rearranged. This only has to be specified when \code{values} has no names.
#' @param values Optional: The values to be added into \code{data}. This only has to be specified when new data should be added rather than only rearranging columns.
#' @param data The data.frame, list, or matrix in which the rearrangement should take place.
#' @return \code{data} with rearranged/added columns.
#' @examples
#' dat <- data.frame(a = 1:3, b = 2:4, z = 3:5)
#' beforeName <- "a"
#' values <- list(c = 5:7)
#'
#' # The following examples work without errors.
#' addColsBefore(beforeName = beforeName, addNames = NULL, values = values, data = dat)
#' addColsBefore(beforeName = beforeName, addNames = NULL, values = as.data.frame(values), data = dat)
#' addColsBefore(beforeName = beforeName, addNames = NULL, values = as.matrix(as.data.frame(values)), data = dat)
#' addColsBefore(beforeName = beforeName, addNames = "x", values = values, data = dat)
#' addColsBefore(beforeName = "z", addNames = "x", values = values, data = dat)
#' addColsBefore(beforeName = "z", addNames = "z", data = dat)
#' # The following will only rearrange.
#' addColsBefore(beforeName = beforeName, addNames = "z", values = NULL, data = dat)
#' # The following will rearrange and give new values to column "z".
#' addColsBefore(beforeName = beforeName, addNames = "z", values = 1:3, data = dat)
#'
#' # The following examples will yield errors.
#' addColsBefore(beforeName = beforeName, addNames = NULL, values = 1:3, data = dat)
#' values <- list(c = 5:7, d = 6:7)
#' addColsBefore(beforeName = beforeName, addNames = NULL, values = values, data = dat)
#'
#' # In lists, it is allowed that every list place has different length. Hence, no error.
#' dat <- list(a = 1:3, b = 2:4, z = 1)
#' addColsBefore(beforeName = beforeName, addNames = NULL, values = values, data = dat)
addColsBefore <- function(beforeName, addNames = NULL, values = NULL, data) {
  .addColsBeforeAfter(referenceName = beforeName, addNames = addNames, values = values, data = data, before = TRUE)
}

#' Internal function called by \code{\link{addColsAfter}} or \code{\link{addColsBefore}}
#' @keywords internal
.addColsBeforeAfter <- function(referenceName, addNames = NULL, values = NULL, data, before = TRUE) {

  wasMatrix <- FALSE
  if (is.matrix(data)) {
    wasMatrix <- TRUE
    cn1 <- colnames(data)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    colnames(data) <- cn1
  }
  if (!is.list(data))
    stop("`dat` must either be a data.frame, list or matrix.")

  if (!is.null(values)) {
    if (is.matrix(values)) {
      cn1 <- colnames(values)
      values <- as.data.frame(values, stringsAsFactors = FALSE)
      colnames(values) <- cn1
    }
    if (is.vector(values) && !is.list(values)) {
      if (length(values) != nrow(data))
        stop("`length(value)` must be equal to `nrow(dat)`.")
    }
    if (is.data.frame(data) & is.list(values)) {
      lapply(values, function(x) {
        if (length(x) != nrow(data))
          stop("All entries in `values` must be equal to `nrow(dat)`.")
      })
    }
    if (is.null(addNames)) {
      if (is.null(names(values)))
        stop("Either `values` have to have names, or `addName` has to be specified.")
      addNames <- names(values)
    }
    data[addNames] <- values
  }

  if (is.null(values) &&is.null(addNames))
    stop("Either specify `values` or `addNames` or both.")

  colsBetween <- match(addNames, names(data))
  if (any(is.na(colsBetween)))
    stop("The following columns were not found in `data`: ", paste0(addNames[is.na(colsBetween)], collapse = ", "))

  whichCol <- which(names(data) == referenceName)
  if (length(whichCol) == 0)
    stop('Before/after the column "', referenceName, '", some other column(s) "', paste0(addNames, collapse ='", "'), '" sould be introduced. ',
         'However, the column "', referenceName, '" cannot be found in the dataset (`data`).')
  whichCol <- whichCol[1]
  if (before)
    whichCol <- whichCol - 1
  if (whichCol == 0) {
    colsBefore <- numeric()
  } else {
    colsBefore <- 1:whichCol
    colsBefore <- colsBefore[!colsBefore%in%colsBetween]
  }

  if (whichCol+1 <= length(data)) {
    colsAfter <- (whichCol+1):length(data)
  } else {
    colsAfter <- numeric()
  }
  colsAfter <- colsAfter[!colsAfter%in%colsBetween]

  data <- data[c(colsBefore, colsBetween, colsAfter)]

  if (wasMatrix)
    data <- as.matrix(data)

  return(data)
}

#' Create columns in \code{data} that are not already existent.
#' @keywords internal
#' @author Daniel Hoop
#' @param data The \code{data.frame} to which columns should be added.
#' @param cols Character vector of columns that should be created if the don't exist in \code{data}.
#' @param non.I.value The value that is filled into columns that don't have colnames I().
#' @details If cols like \code{"I(a/b)"} are given, then the function will look for a column called \code{"a"} and a column calles \code{"b"} in \code{data}.
#' The calculation will then be done as specified, and be written into the according column.
#' @return The same \code{data.frame} as given in argument \code{data} but with the additional columns that were specified in \code{cols}.
#' @examples
#' data <- data.frame(a = 1:3, b = 1:3)
#' create.cols(data, c("I(a/b)", "I(a+b)", "c"))
create.cols <- function(data, cols, non.I.value=NA_integer_){

  isMatrix <- is.matrix(data)
  if (isMatrix) {
    cn1 <- colnames(data)
    data <- as.data.frame(data)
    colnames(data) <- cn1
  }

  # Create new columns
  cols_new <- cols[!cols%in%colnames(data)]
  if (length(cols_new)>0) {
    data[,cols_new] <- non.I.value
    # Calculate the value of the new columns with I()
    if(any(substr(cols_new,1,2)=="I("))
      data <- calc.I.cols(data)
  }

  # Return result
  if (isMatrix)
    data <- as.matrix(data)
  return(data)
}

#' @title Calculates the value of all columns with colnames looking like I(a+b) in a matrix/data.frame/list.
#' @keywords internal
#' @author Daniel Hoop
#' @param data \code{matrix}/\code{data.frame}/\code{list}. Columns to be calculated must have names like \code{"I(a+b)"}, \code{"I(a/b)"}. However, \code{"a+b"} will not work.
#' @param edit.I.colnames Should the brackets I() in the colnames be removed after the calculations? E.g. \code{"I(a+b)"} will later be called \code{"a+b"}.
#' @param del.I.help.colums If e.g. \code{"I(a+b)"} is calculated. Should the columns \code{"a"} and \code{"b"} be removed after the calculation because they are of no interest?
#' @param I.help.columns If del.I.help.colums=TRUE: The list of the columns to be deleted can be specified. Otherwise the help columns are dectected automatically.
#' @examples
#' data <- matrix(c(1:30),ncol=3); colnames(data) <- c("a","b","I(a+b)")
#' calc.I.cols(data)
#' calc.I.cols(as.list(as.data.frame(data)))
calc.I.cols <- function(data, edit.I.colnames=FALSE, del.I.help.columns=FALSE, I.help.columns=NULL) {

  ismat <- is.matrix(data)
  if(ismat) data <- as.data.frame(data)

  i_cols <- names(data)
  i_cols <- i_cols[startsWith(i_cols, "I(") & endsWith(i_cols,")")]
  # If there are no i_cols then return the original data without calculations.
  if(length(i_cols)==0) return(data)

  # Calc only if there are elements. Otherwise this would yield an error.
  if(length(data[[1]])>0){
    for(i in 1:length(i_cols)){
      tryCatch({
        data[[i_cols[i]]][] <- as.vector(with(data, eval(parse(text=i_cols[i]))))
      }, error=function(e){
        if(grepl("unexpected", e$message)) {
          stop (paste0("The calculation syntax in a column like 'I(a+b)' is errorneous. See the error message below.\n", gsub("<[^u]+: ","",e$message)), call.=FALSE)
        } else {
          stop (paste0("The calculation in a column like 'I(a+b)' is not possible, probably because a variable is missing. See the error message below.\n", i_cols[i], ", ", e$message), call.=FALSE)
        }
      })

    }
  }

  if(del.I.help.columns){
    i_cols <- startsWith(names(data), "I(") & endsWith(names(data),")")
    if(!is.null(I.help.columns)){
      delnames <- I.help.columns
    } else {
      delnames <- unlist(strsplit(names(data)[i_cols],"-|/|\\*|\\+"))
      delnames <- gsub("I\\(|\\(|)","",delnames)
    }
    data <- data[!names(data)%in%delnames] # ALT, geht nicht fuer Listen: #data <- data[,!names(data)%in%delnames,drop=FALSE]
  }
  if(edit.I.colnames){
    names(data) <- .rm.I.from.names(names(data))
  }

  if(ismat) data <- as.matrix(data)
  return(data)
}

#' This function is internally used in \code{\link{mean.weight}}.
#' @keywords internal
#' @author Daniel Hoop
.rm.I.from.names <- function(x){
  i_x <- startsWith(x, "I(") & endsWith(x,")")
  x[which(i_x)] <- substr( x[which(i_x)], 3, nchar(x[which(i_x)])-1 )
  return(x)
}

#' This function is internally used in \code{\link{mean.weight}}.
#' @keywords internal
#' @author Daniel Hoop
.checkMissingICols <- function (colsToCheck, colsAvailable) {
  if (!is.null(dim(colsAvailable)))
    colsAvailable <- colnames(colsAvailable)
  if (is.null(colsAvailable))
    stop ("colsAvailable must not be NULL")
  colsMissing <- extract.I.vars(colsToCheck, keep.original=TRUE, keep.only.necessary=FALSE, original.single.vars.only=TRUE)
  colsMissing <- colsMissing[!colsMissing%in%colsAvailable]
  if (length(colsMissing)>0) {
    iToShow <- colsToCheck[startsWith(colsToCheck, "I(") & grepl(paste0(colsMissing, collapse="|"), colsToCheck)]
    addTxt <- if (length(iToShow) == 0) NULL else paste0("\nSome of them are located in 'I()' formulas: ", paste0(iToShow, collapse=", "))
    stop (paste0("Some columns don't exist in the given data.frame/matrix: ",
                 paste0(colsMissing, collapse=", "),
                 addTxt))
  }
  return (invisible(NULL))
}

# data <- 1:10; weights = 1:10; index = c(rep(1, 5), rep(2, 5)); cols = NULL; na.rm = TRUE; change.output.str = FALSE; var = FALSE
#' @export
#' @author Daniel Hoop
#' @title Calculate weighted variances
#' @param data The data of which the weighted variances should be calculated. Can be \code{data.frame}, \code{matrix} or \code{vector}.
#' @param cols Optionally: The columns in data for which the variance should be calculated. If \code{data} contains the columns \code{"a"} and \code{"b"}, then \code{cols} can also be \code{"I(a/b)"}
#' @inheritParams sd.weight
#' @seealso \code{\link{mean.weight}} for examples
var.weight <- function(data, weights = NULL, index = NULL, cols = NULL, na.rm = TRUE, change.output.str = FALSE) {
  sd.weight(data=data, weights=weights, index=index, cols = cols, na.rm = na.rm, change.output.str = change.output.str, var = TRUE)
}

#' @export
#' @author Daniel Hoop
#' @title Calculate weighted standard deviations
#' @param data The data of which the weighted standard deviations should be calculated. Can be \code{data.frame}, \code{matrix} or \code{vector}.
#' @param cols Optionally: The columns in data for which the standard deviation should be calculated. If \code{data} contains the columns \code{"a"} and \code{"b"}, then \code{cols} can also be \code{"I(a/b)"}
#' @param var For internal purposes only. If \code{TRUE}, the variance will be calculed. For that purpose, the function \code{\link{var.weight}} can be used.
#' @inheritParams mean.weight
#' @seealso \code{\link{mean.weight}} for examples
sd.weight <- function(data, weights = NULL, index = NULL, cols = NULL, na.rm = TRUE, change.output.str = FALSE, var = FALSE) {
  if (is.list(data) && !is.data.frame(data))
    stop("data must be matrix or data.frame but not a list.")

  sdVarFunc <- if (var) match.fun("var") else match.fun("sd")

  if (!is.null(cols)) {
    if (is.null(dim(data)))
      stop("If `cols` are given, then `data` must be a matrix or data.frame, not a vector.")
    # Check if some cols (possibly in I(a+b) columns) are missing. If yes, throw an error.
    .checkMissingICols(cols, colnames(data))
    # Extract all columns from I() cols and create I()-cols if they don't yet exist in data.
    data <- create.cols(data, cols)
    cols_all <- cols
    data <- data[,cols_all,drop=FALSE]
  } else {
    cols <- colnames(data)
    cols_all <- colnames(data)
  }

  # Index muss eine List mit folgender Struktur sein:
  isNullIndex <- is.null(index)
  if (!is.list(index))
    index <- list(index)

  # Im Falle, dass !is.null(dim(data)) folgt eine rekursive Funktionsdefinition!
  if (!is.null(dim(data))) {
    if (!isNullIndex) {
      all.na.index <- sapply(index, function(x) all(is.na(x)))
      if (any(all.na.index))
        stop("The following indices contain only NA values. Please change to a different value (not NA): ", paste0(names(all.na.index)[all.na.index],collapse=","))
    }

    if (!change.output.str && length(index) > 1) {
      index <- list(.paste.elements(index, sep="_", errorMsg="All indices must have same length!"))
    }

    # Wenn !is.null(dim(data))
    # & es keinen oder nur einen Index gibt:
    if (isNullIndex || length(index)==1) {

      # Berechnung rekursiv fuer matrix / data.frame
      if (is.matrix(data)) {
        if (nrow(data)==0) stop("nrow of data is 0")
        result <- apply(data[,cols_all,drop=FALSE], 2, function(x) sd.weight(data=x, weights=weights, index=index, na.rm = na.rm, change.output.str = change.output.str, var = var))
      } else if (is.data.frame(data)) {
        if (nrow(data)==0) stop("nrow of data is 0")
        #result <- sapply(data, function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=index.of.result, index.sep=index.sep, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
        result <- as.matrix(as.data.frame(lapply(data[,cols_all,drop=FALSE], function(x)  sd.weight(data=x, weights=weights, index=index, na.rm = na.rm, change.output.str = change.output.str, var = var)), stringsAsFactors=FALSE))
      }
      # Wieder zu Marix machen, falls es ein Vektor ist
      if (is.null(dim(result))) {
        result <- t(as.matrix(result))
        if (!isNullIndex && length(index)==1 && length(unique(index[[1]]))==1)
          rownames(result) <- index[[1]][1]
      }
      if (nrow(result) == 1 && is.null(rownames(result)) && !is.null(index) && !is.null(index[[1]])) {
        rownames(result) <- sort(unique(index[[1]]))
      }
      # Wieder die alten colnames vergeben
      colnames(result) <- cols_all
      # Resultat ausgeben.
      if (nrow(result)==1 && isNullIndex) {
        return(result[1,cols]) #rownames(result) <- NULL
      } else {
        return(result[,cols,drop=FALSE])
        #return (result[, cols])
      }


    } else if (length(index)==2) {
      # res1 <- mean.weight(data=data[,1], weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm)

      # Hier keine Fallunterscheidung zwischen matrix und data.frame einfuegen, sonst funktioniert es nicht!!
      res.prov <- apply(data[,cols_all,drop=FALSE], 2, function(x) sd.weight(data=x, weights=weights, index=index, na.rm = na.rm, change.output.str = change.output.str, var = var) )
      if (!"matrix" %in% class(res.prov))
        res.prov <- t(as.matrix(res.prov))

      res.list <- list()
      su.index1 <- sort(unique(index[[1]]))
      su.index2 <- sort(unique(index[[2]]))
      for(i in 1:ncol(res.prov)){
        res.list[[i]] <- matrix(res.prov[,i],nrow=length(su.index1), ncol=length(su.index2))
        dimnames(res.list[[i]]) <- list(su.index1, su.index2)
      }
      names(res.list) <- cols_all

      return(res.list[cols])

    } else if (length(index)>2) {
      stop("More than 2 indexes not possible if data is a matrix/data.frame. Please enter data as vector.")
    }

  }


  ## This is the actual function definition

  # Without index
  if (is.null(index) || is.null(index[[1]])) {
    if (is.null(weights))
      return(sdVarFunc(data, na.rm = na.rm))

    if (na.rm) {
      keep <- !is.na(data)
      data <- data[keep]
      weights <- weights[keep]
    }
    varRes <- sum(weights * (data - stats::weighted.mean(data, weights))^2)/(sum(weights)-(sum(weights^2))/sum(weights))

    # With index
  } else {
    if (is.null(weights))
      return(tapply(data, index, function(x) sdVarFunc(x, na.rm = na.rm)))

    data <-  tapply(data, index,  function(x) x)
    weights <- tapply(weights, index, function(x) x)

    varRes <- mapply(
      data = data,
      weights = weights,
      FUN = function(data, weights) {
        if (na.rm) {
          keep <- !is.na(data)
          data <- data[keep]
          weights <- weights[keep]
        }
        return(sum(weights * (data - stats::weighted.mean(data, weights))^2)/(sum(weights)-(sum(weights^2))/sum(weights)))
      })
  }

  if (isTRUE(var))
    return(varRes)
  return(sqrt(varRes))
}

#' Calculate weighted medians.
#' @export
#' @author Daniel Hoop
#' @inheritDotParams quantile.weight
#' @seealso \code{\link{quantile.weight}}
median.weight <- function(...) return(quantile.weight(..., probs=0.5))

#' Calculate weighted quantiles.
#' @export
#' @author Daniel Hoop
#' @param x The \code{vector}, \code{data.frame}, or \code{matrix} that contains the data for which the quantiles should be calculated.
#' @param weights Optionally: A numeric vector containing the weights for each observation given in \code{x}.
#' @param index Optionally: A numeric vector or \code{data.frame}/\code{list} containing the indices. See also \code{\link[base:tapply]{base::tapply}}.
#' @param probs The probabilities of the quantiles that should be calculated.
#' @param Optionally: The columns in \code{x} for which the calculations should be done.
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds.
#x <- gb[,"ArbVerd_jeFJAE"]; weights <- gb[,"Gewicht"];index <- gb[,c("Jahr","Region","Betriebstyp_S3")]; probs=0.5; na.rm=TRUE
#quantile.weight(x=gb[,c("LE","ArbVerd_jeFJAE")], weights=gb[,"Gewicht"], index=gb[,c("Jahr")], probs=c(0.25,0.5))
quantile.weight <- function(x, weights=NULL, index=NULL, probs=0.5, cols=NULL, na.rm=TRUE) {
  # This function calculates weighted quantiles.
  # Arguments:
  # x       = Vector of numbers of which quantiles should be calculated
  # weights = vector of weights
  # probs   = probabilities of quantiles

  # Original function was cwhmisc::w.median. Alternative function with same result is reldist::wtd.quantile
  # Differents result calculated by these functions: Hmisc::wtd.quantile, matrixStats::weightedMedian (test with RefB, Jahr 2012, gb[,"ArbVerd_jeFJAE"])

  # Recursive function definition if x is given as matrix, data.frame or list
  if(!is.null(dim(x))) {
    if(!is.null(cols)) {
      # Check if some cols (possibly in I(a+b) columns) are missing. If yes, throw an error.
      .checkMissingICols(cols, colnames(x))
      # Extract all columns from I() cols and create I()-cols if they don't yet exist in data.
      cols_add <- extract.I.vars(cols, keep.only.necessary=TRUE)
      x <- create.cols(x, cols)
      cols_all <- c(cols, cols_add)
      x <- x[,cols_all,drop=FALSE]
    } else {
      cols <- colnames(x)
      cols_all <- colnames(x)
    }

    #cn_x <- colnames(x)
    if(is.matrix(x)) res <- apply(
      x[,cols_all,drop=FALSE],
      2,
      function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm))
    if(is.data.frame(x)) res <- as.data.frame(lapply(
      x[,cols_all,drop=FALSE],
      function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm)),
      stringsAsFactors=FALSE)
    #colnames(res) <- cn_x
    colnames(res) <- cols_all
    return (res[,cols,drop=FALSE])
  }
  if(is.list(x)) {
    if (!is.null(cols))
      stop ("x must not be a list, when cols are specified. Use a data.frame or a matrix as input.")
    return( lapply(x,function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm)) )
  }

  # Recursive function definition if index is given
  if(!is.null(index)){
    if (is.factor(x))
      x <- as.character(x)
    res <- by(cbind(x=x, weights=weights),
              index,
              function(x) quantile.weight(x=x[,"x"], weights=if(is.null(weights)) NULL else x[,"weights"], probs=probs, na.rm=na.rm))
    attr(res,"call") <- NULL
    if(length(res[[1]])>1) res <- do.call("rbind", res) else if(length(dim(res))==1) res <- c(res) else class(res) <- "array"
    return(res)
  }

  if(is.null(weights)) weights <- rep(1, length(x))
  w <- weights

  # Recursive function definition for more than 1 probs
  if(length(probs)>1){
    res <- apply(matrix(probs),1,function(probs)  quantile.weight(x=x, weights=w, probs=probs, na.rm=na.rm) )
    names(res) <- paste0(round(probs*100,2),"%")
    return(res)

    # Now follows the actual function to calculate weighted means
  } else {
    if (!is.numeric(x))
      x <- rep(0, length(x))
    if (!is.numeric(w))
      w <- rep(1, length(x))
    if(na.rm) {
      ok <- complete.cases(x, w)
      x <- x[ok]
      w <- w[ok]
    }
    w_not0 <- w!=0
    x <- x[w_not0]
    w <- w[w_not0]

    if(length(x)==0) return(NA)

    ind <- sort.list(x)
    x <- x[ind]
    w <- w[ind]
    ind1 <- min(which(cumsum(w)/sum(w) >= probs))
    ind2 <- if ((w[1]/sum(w)) > probs) {
      1
    } else {
      max(which(cumsum(w)/sum(w) <= probs))
    }
    max(x[ind1], x[ind2])
  }
}

#' @title Inverse quantile function.
#' @description The funciton does not yield exactly the same results as you would get using the \code{\link[stats:quantile]{stats::quantile}} function because the distribution functions differ.
#' @export
#' @author Daniel Hoop
#' @param x All values in the sample.
#' @param value The value of which the probability in the cumulative distribution should be calculated.
quantile.inverse <- function(x, value, na.rm = TRUE){ # quantile.reverse inverse.quantile reverse.quantile
  rangex <- range(x)
  if (value < rangex[1])
    stop("There is no such small value in `x`.")
  if (value > rangex[2])
    stop("There is no such large value in `x`.")
  invProb <- ecdf(x)(value)
  diff <- if (length(x) < 500) 0.1 else 0.01
  invProbs <- c(invProb - 0.01, invProb, min(1, invProb + 0.01))
  invProbs[invProbs < 0] <- 0
  values <- quantile(x, invProbs, na.rm = na.rm)
  return(approx(x = values, y = invProbs, xout = value)$y)
}

#' @title Inverse weighted quantile function.
#' @description Finds the approximate quantile probability for a given value.
#' @export
#' @author Daniel Hoop
#' @param x All values in the sample.
#' @param value The value of which the probability in the cumulative distribution should be calculated.
#' @param diff The maximal relative difference between the real quantile value and the approximated quantile value.
#' @examples
#' probs <- quantile.weight.inverse(x = 1:100, weights = NULL, value = 60, diff = 0.001)
#' print(probs)
#' value <- quantile.weight(x = 1:100, weights = NULL, probs = probs)
#' print(value)
quantile.weight.inverse <- function(x, weights = NULL, value, diff = 0.001, max.iter = 100, na.rm = TRUE) {

  # Recursion
  if(!is.null(dim(x))) {
    cols_all <- colnames(x)

    #cn_x <- colnames(x)
    if(is.matrix(x))
      res <- apply(
        x[ , cols_all, drop=FALSE],
        2,
        function(x) quantile.weight.inverse(x = x, weights = weights, value = value, diff = diff, max.iter = max.iter, na.rm = na.rm))

    if(is.data.frame(x))
      res <- as.data.frame(lapply(
        x[ , cols_all, drop=FALSE],
        function(x) quantile.weight.inverse(x = x, weights = weights, value = value, diff = diff, max.iter = max.iter, na.rm = na.rm)),
        stringsAsFactors=FALSE)

    colnames(res) <- cols_all
    return (res[,cols,drop=FALSE])
  }

  if (diff == 0)
    stop("`diff` must be greater than 0.")

  if (!is.null(weights) && length(x) != length(weights))
    stop("If `weights` is not NULL, then `x` and `weights` must have the same length.")

  # This is the actual function
  p1 <- 0
  p2 <- 0.5
  p3 <- 1
  v1 <- quantile.weight(x, weights = weights, probs = p1, na.rm = na.rm)
  v2 <- quantile.weight(x, weights = weights, probs = p2, na.rm = na.rm)
  v3 <- quantile.weight(x, weights = weights, probs = p3, na.rm = na.rm)

  if (is.na(v2)) {
    warning("There are NA values in the distribution. In that case, the calculation is impossible. Consider specifying `na.rm = TRUE`.")
    return(NA)
  }
  if (value == v1)
    return(p1)
  if (value == v3)
    return(p3)
  if (value < v1) {
    warning("The given value is smaller than the smallest value in the distribution.")
    return(NA)
  }
  if (value > v3) {
    warning("The given value is greater than the largest value in the distribution.")
    return(NA)
  }

  counter <- 0
  while(abs((v2 / value) - 1) > diff) {
    old_p2 <- p2

    if (v2 < value) {
      p1 <- p2
    } else if (v2 > value) {
      p3 <- p2
    }
    p2 <- (p1 + p3) / 2

    v1 <- quantile.weight(x, weights = weights, probs = p1, na.rm = na.rm)
    v2 <- quantile.weight(x, weights = weights, probs = p2, na.rm = na.rm)
    v3 <- quantile.weight(x, weights = weights, probs = p3, na.rm = na.rm)

    #print(abs((v2 / value) - 1))
    #print(c(v1, v2, v3))

    counter <- counter + 1
    if (counter == max.iter) {
      warning("No convergence in ", max.iter, " iterations. The returned probability value might be inaccurate.")
      break
    }
    if (abs(p2 - old_p2) < 0.0000001) {
      # warning("The value is not converging. Using `quantile.inverse` instead, which may not be as accurate.")
      # p2 <- quantile.inverse(x = x, value = value, na.rm = na.rm)
      break
    }
  }

  return(p2)
}

# x <- 1:100; value <- 60

####
meansd <- function(x,na.rm=TRUE) {
  c(Mean=mean(x,na.rm=na.rm),SD=sd(x,na.rm=na.rm))
}
####

mean.geom <- function(x,na.rm=TRUE) {
  if(na.rm==TRUE) x <- x[!is.na(x)]
  result <- (prod(x))^(1/length(x))
  # Wenn das Ergebnis zu gross wird (Inf) m?ssen die Anfangswerte erst verkleinert werten.
  if(is.infinite(result)){
    for(i in seq(1,6,1)){
      result <- mean.geom(x=x*10^(-i),na.rm=na.rm)*10^(i)
      if(!is.infinite(result)) break
    }
  }
  names(result) <- "geom.Mean"
  return(result)
}
####

meansd.geom <- function(x,na.rm=TRUE){
  if(na.rm==TRUE) x <- x[!is.na(x)]
  m <- mean.geom(x=x,na.rm=na.rm)
  sd <- sd(x)
  return(c(Mean=m,SD=sd))
}
####
# data <- tab_ch0; margin=1; front.back=1; method="sum"; name="Total"; digits=NULL; na.rm=FALSE
add.means <- function(data, margin=c(1,2), front.back=c(1,2), method=c("arith","geom","sum","median"), name=NULL, digits=NULL, na.rm=FALSE, ...){

  if(is.list(data) & !is.data.frame(data)) return(lapply(data, function(data)add.means(data=data, margin=margin, front.back=front.back, method=method, name=name, digits=digits, ...)))

  if(na.rm) data[is.na(data)] <- 0

  margin <- margin[1]
  front.back <- front.back[1]
  matrix.opposite <- matrix(c(1,2,2,1),ncol=2)
  margin.opposite <- matrix.opposite[2 ,matrix.opposite[1,margin] ]
  method <- match.arg(method)
  if(is.null(name)){
    if(method=="arith") name <- "mean"
    if(method=="geom") name <- "geom"
    if(method=="sum") name <- "sum"
    if(method=="median") name <- "median"
  }
  name <- as.character(name)
  #if(is.null(name)) name <- as.character(substitute(func))
  mean.geom <- function(x,na.rm=TRUE) {
    if(na.rm==TRUE) x <- x[!is.na(x)]
    result <- (prod(x))^(1/length(x))
    names(result) <- "Mean"
    return(result)
  }
  if(method=="arith") mean.func <- mean else if(method=="geom") mean.func <- mean.geom else if(method=="sum") mean.func <- sum else if(method=="median") mean.func <- median
  if(is.null(digits)) digits <- max(do.call("c",lapply(diag(data),function(x)n.decimals(x))))

  if(margin==1) {
    bind.func <- cbind
    count.func <- ncol
  } else {
    bind.func <- rbind
    count.func <- nrow
  }
  means <- apply(data,margin,function(x)mean.func(x))
  means <- round(means,digits)

  if(front.back==1) {
    res <- bind.func(means, data)
    attributes(res)$dimnames[[margin.opposite]][1] <- name
  } else {
    res <- bind.func(data,means)
    attributes(res)$dimnames[[margin.opposite]][count.func(res)] <- name
  }
  return(res)
}
####

gb.diffs <- function(x,cols=list(c("2013","2014")), # cols=list(c("2013","2014"),c("2005","2014"))
                     short.names=list(c("13","14")), digits=2, percdigits=1) { # short.names=list(c("13","14"),c("05","14"))
  # This function calculates the Grundlagenberichts-differences that are mostly used in the
  # Medienmitteilung and the Hauptbericht
  # First, calculate weighted means with the function mean.weights()
  # Then use gb.diffs() to calculate the differences between years.
  if(is.list(x) & !is.data.frame(x)) return( lapply(x,function(x)gb.diffs(x=x, cols=cols, short.names=short.names, digits=digits)) )

  allcols <- unlist(cols)
  if(any(!allcols%in%colnames(x))){
    stop(paste0("The following columns are not available in the data: ", paste0(allcols[!allcols%in%colnames(x)],collapse="  ")))
  }
  add <- list()
  for(i in 1:length(cols)){
    tp <- cols[[i]][2]
    tm <- cols[[i]][1]
    temp <- cbind( x[,tp]-x[,tm], (x[,tp]/x[,tm]-1)*100 )
    temp[,2] <- round(temp[,2], percdigits)
    if(is.null(short.names)){
      colnames(temp) <- c(paste0(tp,"-",tm), paste0(tp,"/",tm))
    } else {
      tpn <- short.names[[i]][2]
      tmn <- short.names[[i]][1]
      colnames(temp) <- c(paste0(tpn,"-",tmn), paste0(tpn,"/",tmn))
    }
    add[[i]] <- temp
  }
  add <- do.call("cbind", add)
  res <- cbind(x,add)
  if(!is.null(digits)) res <- round(res,digits)
  return(res)
}

####

if(FALSE){
  X=round(runif(100,1,15)); INDEX=c(rep(1,50), rep(2,20), rep(3,30)); FUN=function(x)mean(x); missing.value=NA
  names.result=c(1:5, "1_a"); names.result=NULL
  INDEX <- list( c(rep(1,50), rep(2,20), rep(3,30)) ,
                 c(rep("a",30),rep("b",20), rep("c",50)),
                 c(rep("u",15),rep("v",60), rep("w",25)),
                 c(rep("x",10),rep("y",25), rep("z",65))
  )
  vector.result=FALSE

  X=round(runif(100,1,15));
  INDEX <- list( c(rep(1,50), rep(2,20), rep(3,30)) ,
                 c(rep("a",30),rep("b",20), rep("d",50)),
                 c(rep("u",15),rep("v",60), rep("w",25))
  )
  FUN=function(x)mean(x); missing.value=NA
  names.result <- list(c(1,2,3),c("a","c","d"),c("u","v","w"))
  vector.result=FALSE

  sep.sign="_"
  res <- tapply.fixed(X, INDEX, FUN=function(x)mean(x), names.result=names.result, vector.result=vector.result)
  print(res)
  res[!is.na(res)]
  pasteown <- function(...) paste(..., sep=sep.sign)
  cbind(names(res[!is.na(res)]), sort(unique(do.call(pasteown, INDEX))))
}


#' @title Puts the result of tapply(X,INDEX,FUN) into a fixed given vector with names=names.result.
#' @description This is especially useful if some entries are missing in INDEX but you want them to be displayed as well!
#' Otherwise they would be missing in the result of the tapply() function.
#' @export
#' @author Daniel Hoop
#' @inheritParams base::tapply
#' @seealso \code{\link[base:tapply]{base::tapply}}
#' @param names.result The names of the resulting vector (including all possible 0/NA entries).
#' @param missing.value Which value should be put into the resulting vector if there were no entries in \code{INDEX}?
#' @param sep.sign The sign to separate the new vector index and \code{names.result} if \code{INDEX} is a \code{list}.
#' @param vector.result Should the result be presented in a \code{vector} (one dimension) or in a multidimensional \code{array} (logical)?
#' @param warn Logical value indicating if the function should show a warning if some levels in index get lost because they were not given in \code{vector.result}.
#' @examples
#' X <- c(1,2,3,4,5,6,7,8,9,10)
#' INDEX <- LETTERS[c(1,1,1,1,1,2,2,2,2,2)]
#' tapply.fixed(X, INDEX, mean, names.result = c("A", "B", "C"))
#' INDEX <- list(LETTERS[c(1,1,1,1,1,2,2,2,2,2)],
#'               LETTERS[c(2,2,2,2,2,1,1,1,1,1)])
#' tapply.fixed(X, INDEX, mean, names.result = c("A_A", "A_B", "B_A", "B_B"), vector.result=TRUE)
tapply.fixed <- function(X, INDEX, FUN, names.result=NULL, missing.value=NA, vector.result=FALSE, sep.sign="_", warn=FALSE){

  if(!is.null(dim(X))) stop("This function only works for vectors! dim(X) must be NULL!")
  if(length(INDEX)==1) vector.result=TRUE
  if(is.matrix(INDEX)) INDEX <- as.data.frame(INDEX, stringsAsFactors=FALSE)

  # Falls names.result eine Liste ist, werden alle moeglichen Kombinationen der Listenplaetze
  # zusammengestelt und damit ein Vektor erstellt
  if(is.list(names.result) & vector.result){
    if(length(names.result)==1) {
      names.result <- sort(unique(names.result[[1]]))
    } else {
      su.index1 <- sort(unique(names.result[[1]]))
      su.index2 <- sort(unique(names.result[[2]]))
      nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(names.result)>2){
        for(i in 3:length(names.result)){
          su.index1 <- nres1
          su.index2 <- sort(unique(names.result[[i]]))
          nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
    }
  }

  # Vorbereiten von INDEX und names.result
  # Wenn der Index eine Liste ist...
  if(is.list(INDEX)) {
    l.INDEX <- sapply(INDEX, function(x)if(!is.null(dim(x))) nrow(x) else length(x))
    if(any(l.INDEX!=l.INDEX[1])) stop(paste0("All indexes must have the same length! ", paste0(l.INDEX,collapse=" ") ))
    if(l.INDEX[1]==0 && length(X)>0) stop(paste0("INDEX(es) has/have length=0 or nrow=0."))

    # Wenn das Resultat 1 Dimension haben soll.
    if(vector.result){
      if(is.null(names.result)){
        if(length(l.INDEX)==1) {
          names.result <- sort(unique(INDEX[[1]]))
        } else {
          su.index1 <- sort(unique(INDEX[[1]]))
          su.index2 <- sort(unique(INDEX[[2]]))
          names.result <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
          if(length(INDEX)>2){
            for(i in 3:length(INDEX)){
              su.index1 <- names.result
              su.index2 <- sort(unique(INDEX[[i]]))
              names.result <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
            }
          }
        }
      }
      pasteown <- function(...) paste(..., sep=sep.sign)
      INDEX <- do.call(pasteown, INDEX)

      # Wenn das Resultat mehrere Dimensionen haben soll.
    } else {
      if(is.null(names.result)){
        names.result <- lapply(INDEX,function(x)sort(unique(x)))
      }
    }
    # Wenn der Index keine Liste ist.
  } else {
    if(is.null(names.result)) names.result <- sort(unique(INDEX))
  }


  # Resultate-Berechnung im Falle mehrdimensionaler Ergebnisstruktur.
  if(is.list(INDEX) & !vector.result){
    # Hier Ergebnis-Berechnung
    res0 <- tapply(X,INDEX,FUN)
    if(!is.na(missing.value)) res0[is.na(res0)] <- missing.value

    nres0 <- unlist(dimnames(res0))
    nres1 <- unlist(names.result)
    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(warn) if(any(!nres0%in%nres1))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(nres0[!nres0%in%nres1], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    # ALTE Bedingung: if(length(res0)!=length(nres1) || names(res0)!=nres1){
    if(length(res0)!=length(nres1) || length(names(res0))==0 || any(names(res0)!=nres1)){
      # Ergebnis-Strukturen fuer Matching vorbereiten.
      su.index1 <- sort(unique(INDEX[[1]]))
      su.index2 <- sort(unique(INDEX[[2]]))
      nres0 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(INDEX)>2){
        for(i in 3:length(INDEX)){
          su.index1 <- nres0
          su.index2 <- sort(unique(INDEX[[i]]))
          nres0 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
      su.index1 <- sort(unique(names.result[[1]]))
      su.index2 <- sort(unique(names.result[[2]]))
      nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(names.result)>2){
        for(i in 3:length(names.result)){
          su.index1 <- nres1
          su.index2 <- sort(unique(names.result[[i]]))
          nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
      res1 <- array(missing.value, dim=lapply(names.result,function(x)length(x)), dimnames=names.result)
      res1[nres1%in%nres0] <- res0[nres0%in%nres1]
      if(is.null(dim(res1))) res1 <- as.array(res1); return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      if(is.null(dim(res0))) res0 <- as.array(res0); return(res0)
    }

    # Resultate-Berechnung im Falle von Vektor-Ergebnisstruktur
  } else{
    res0 <- tapply(X,INDEX,FUN)
    if(!is.na(missing.value)) res0[is.na(res0)] <- missing.value

    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(warn) if(any(!names(res0)%in%names.result))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(names(res0)[ !names(res0)%in%names.result ], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    if(length(res0)!=length(names.result) || any(names(res0)!=names.result)){
      res1 <- rep(missing.value, length(names.result))
      names(res1) <- names.result
      ind <- match(names(res0),names(res1))
      if(any(is.na(ind))){
        res0 <- res0[!is.na(ind)]
        ind <- ind[!is.na(ind)]
      }
      res1[ ind ] <- res0
      if(is.null(dim(res1))) res1 <- as.array(res1); return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      if(is.null(dim(res0))) res0 <- as.array(res0); return(res0)
    }
  }
}

#' @title Puts the result of \code{table(X)} into a fixed given vector with specified names.
#' @description It is a wrapper function for \code{\link{tapply.fixed}}.
#' @export
#' @author Daniel Hoop
#' @details For information on the arguments see \code{\link{tapply.fixed}}.
#' @seealso \code{\link{tapply.fixed}}
#' @examples
#' X <- round(runif(100,1,15))
#' table.fixed(X, names.result=1:5)
#' table.fixed(round(runif(100,1,15)), round(runif(100,1,15)), names.result=list(1:20, 5:15), vector.result=FALSE)
table.fixed <- function(..., names.result=NULL, vector.result=FALSE, sep.sign="_") {
  INDEX <- list(...)
  if(is.list(INDEX[[1]])) INDEX <- INDEX[[1]]
  # Kuenstlich Daten erzeugen, falls keine vorhanden sind. Ist noetig, damit es keinen Fehler in tapply.fixed gibt.
  #if(length(INDEX[[1]])==0){
  #  INDEX[[1]] <-
  #}
  tapply.fixed(X=rep(1,length(INDEX[[1]])), INDEX=INDEX, FUN=function(x)length(x), names.result=names.result, missing.value=0, vector.result=vector.result, sep.sign=sep.sign)
}

####
if(FALSE){
  # Debugging data for by.add.df.cols()
  cost <- load.cost(); sampl <- sample(1:200, 200)
  data <- cost[sampl,]
  data[,"orderTester"] <- 1:nrow(data)
  relevantColnames <- c("Gewinn","Groesse","orderTester")
  INDICES <- cost[sampl,c("ID","Jahr")]
  FUN <- function(x) {
    x[,"Gewinn_tot"] <- sum(with(x, Gewinn*Groesse), na.rm=TRUE)
    #return(x[,c("Gewinn_tot","Gewinn"),drop=FALSE])
    return(x[,c("Gewinn_tot"),drop=FALSE])
  }
  by.add.df.cols(data=data, relevantColnames=relevantColnames, INDICES=INDICES, FUN=FUN)
}

#' @title Uses \code{by()} over \code{data[,relevantColnames]} with \code{INDICES} and a defined function \code{FUN}.
#' @description It is designed to be faster than \code{\link[base:by]{base::by}} over the whole \code{data.frame} because is takes only these columns into \code{\link[base:by]{base::by}} that are really needed for the function.
#' After the calculation a data.frame is returned instead of the usual \code{list} that is returned by the \code{\link[base:by]{base::by}} function.
#' The initial order of the rows in \code{data} is kept in the result. The additionally calculated columns from \code{FUN} are added to the original data.frame that was given in \code{data}.
#' @keywords internal
#' @author Daniel Hoop
#' @param data The data frame over which \code{\link[base:by]{base::by}} should be applied.
#' @param relevantColnames The relevant colnames that are needed for the calculations in FUN.
#' @param INDICES The indices for the application of \code{\link[base:by]{base::by}}. This will be pasted to a signle string if \code{!is.null(dim(INDICES))}.
#' @param FUN The function to apply within \code{\link[base:by]{base::by}}.
#' @param autoColSubset Boolean value indicating if only the relevant colnames from data should be used in the calculations in order to speed up the calculations. *Experimental*
#' @seealso \code{\link[base:by]{base::by}}
#' @details The same can be accomplished with much less code, using the package \code{dplyr}.
#' @examples
#' by(data.frame(a = 1:10, b = 1:10),
#'    INDICES=c(1,1,1,1,1,2,2,2,2,2),
#'    function(x) {
#'      x[,"count"] <- nrow(x)
#'      x[,"sum"] <- sum(c(x[,"a"], x[,"b"]))
#'      return (x)
#'    })
by.add.df.cols <- function(data, relevantColnames=NULL, INDICES, FUN, showWarnings=TRUE, autoColSubset = FALSE) {

  # Combine INDICES to a single string if it is given as several columns of a data.frame/matrix
  if(is.matrix(data)) data <- as.data.frame(data)
  if(is.list(INDICES) & !is.data.frame(INDICES)) INDICES <- as.data.frame(INDICES)
  if(!is.null(dim(INDICES))) INDICES <- paste.cols(INDICES, colnames(INDICES))
  if(length(INDICES)!=nrow(data)) stop("INDICES must have the same number of elements as data has rows. Do not enter colnames here, but vectors instead.")
  if(!"function" %in% class(FUN)) stop("`FUN` must be a function.")
  # By...   Add column to restore the original order of the rows.
  # An additional function has to be definded that add will add the "order column" with content=1:nrow(x) and col number=ncol(x) to the result.
  if (is.null(relevantColnames)) {
    if (autoColSubset) {
      fun <- deparse(FUN)
      fun <- unlist(strsplit(fun, " |,|\\+|\\-|\\*|/|\\^|\\(|\\)|\\{|\\}|\\[|\\]|\"|'|\\$"))
      relevantColnames <- unique(fun[fun %in% colnames(data)])
    } else {
      relevantColnames <- colnames(data)
    }
  }
  res <- by( data[,relevantColnames,drop=FALSE], INDICES, FUN )
  if(is.null(res[[1]])) stop("The specified function (in argument 'FUN') returns NULL.")
  if(!is.null(dim(res[[1]]))) {
    res <- do.call("rbind",res)
  } else if (is.list(res)) {
    res <- matrix(do.call("c",res)); colnames(res) <- "byResult"
    if(showWarnings) message("The resulting column was named 'byResult' because FUN returned a vector without dimensions.")
  } else {
    res <- matrix(res); colnames(res) <- "byResult"
    print(res)
    if(showWarnings) message("The resulting column was named 'byResult' because FUN returned a vector without dimensions.")
  }
  # Now remove the relevantColnames and reorder the data. Set rownames as of data.
  tapplyOrder <- unname(unlist( tapply(1:length(INDICES),INDICES,function(x)return(x))))
  if(length(tapplyOrder)!=nrow(res)) stop("The result given by FUN does not contain all rows of the initial data.frame. Don't return a vector, but data.frame (e.g. using drop=FALSE). Please check FUN and correct it.")
  res <- res[order(tapplyOrder),
             !colnames(res)%in%relevantColnames,drop=FALSE]
  if(ncol(res)==0) stop("The colnames of the result must not be named like relevantColnames, otherwise they are deleted and not returned.")
  rownames(res) <- rownames(data)
  # Return original data.frame and ordered additional columns (without the column that was added to restore the initial row order.
  return(cbind(data, res))
}

#### OTHER ####

#' Transform p values to sign
#' @param pVal A numeric vector containing p values (from 0 to 1)
#' @param indicate0.1 Logical value indicating if 0.1 should also be shown as "."
#' @return A character vector indicating significant differences where "***" = 0.001, "**" = 0.01, "*" = 0.05 and "." = 0.1
#' @examples pValToSign(c(0.05, 0.049, 0.01, 0.009))
pValToSign <- function(pVal, indicate0.1 = FALSE) {
  if (length(pVal) == 0)
    return(pVal)
  sign <- rep("", length(pVal))
  if (indicate0.1)
    sign[pVal < 0.1] <- "."
  sign[pVal < 0.05] <- "*"
  sign[pVal < 0.01] <- "**"
  sign[pVal < 0.001] <- "***"
  return(sign)
}

#' Evaluate an R expression and interrupts it if it takes too long
#' @details This function is a slightly adapted version of R.utils::withTimeout (https://cran.r-project.org/package=R.utils)
#' @param expr The R expression to be evaluated.
#' @param substitute If TRUE, argument expr is substitute():ed, otherwise not.
#' @param envir The environment in which the expression should be evaluated.
#' @param timeout A numeric specifying the maximum number of seconds the expression is allowed to run before being interrupted by the timeout. The cpu and elapsed arguments can be used to specify whether time should be measured in CPU time or in wall time.
#' @param cpu A numeric specifying the maximum number of seconds the expression is allowed to run before being interrupted by the timeout. The cpu and elapsed arguments can be used to specify whether time should be measured in CPU time or in wall time.
#' @param elapsed A numeric specifying the maximum number of seconds the expression is allowed to run before being interrupted by the timeout. The cpu and elapsed arguments can be used to specify whether time should be measured in CPU time or in wall time.
#' @param onTimeout A character specifying what action to take if a timeout event occurs.
#' @seealso https://cran.r-project.org/package=R.utils
withTimeout <- function (expr, substitute = TRUE, envir = parent.frame(), timeout, cpu = timeout, elapsed = timeout, onTimeout = c("silent", "error", "warning")) {
  if (substitute)
    expr <- substitute(expr)
  if (!is.environment(envir))
    stop("Argument 'envir' is not a list: ", class(envir)[1L])
  cpu <- max(0, min(cpu, Inf))
  elapsed <- max(0, min(elapsed, Inf))
  onTimeout <- match.arg(onTimeout)
  setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  tryCatch({
    eval(expr, envir = envir)
  }, error = function(ex) {
    msg <- ex$message
    pattern <- gettext("reached elapsed time limit", "reached CPU time limit", domain = "R")
    pattern <- paste(pattern, collapse = "|")
    if (regexpr(pattern, msg) != -1L) {
      if (onTimeout == "error") {
        stop("Timeout reached")
      } else if (onTimeout == "warning") {
        warning(ex$message)
      } else if (onTimeout == "silent") {}
    } else {
      stop(ex$message)
    }
  })
}

#' Calculate the number of available cores such that memory will not be exhausted by parallel processing.
#' @param cpuBuffer Numeric value specifying the share totally available processors that should not be used but left as buffer for other processes.
#' @param memBuffer Numeric value specifying the share of the total memory that should not be used but left as buffer for other processes.
#' @param roundFunc The rounding function for the number of cores. Default is \code{\link[base:floor]{base::floor}}
#' @return The number of cores.
availableCores <- function(cpuBuffer = 0.2, memBuffer = 0.3, roundFunc = base::floor) {

  convertToKiB <- function(x) {
    as.numeric(x) / 1024
  }

  if (Sys.info()["sysname"] == "Windows") {
    if (!"parallel" %in% rownames(installed.packages()))
      install.packages("parallel")

    maxCores <- parallel::detectCores()
  } else {
    # Do not use parallel::detectCores() because it will not show the cores that
    # are available for power users, but for all users.
    maxCores <- as.numeric(system("nproc", intern = TRUE))
  }

  if (!"memuse" %in% rownames(installed.packages()))
    install.packages("memuse")
  memusage <- memuse::Sys.meminfo()
  ramTotal <- convertToKiB(memusage$totalram)
  ramFree <- convertToKiB(memusage$freeram)
  rUsage <- convertToKiB(memuse::Sys.procmem()$size)

  # Add the RAM of usage by R and subtract the buffer.
  ramFree <- ramFree + rUsage - (ramTotal * memBuffer)

  # Calc number of cores.
  nCores <- roundFunc(min((1 - cpuBuffer) * maxCores,
                          max(1, ramFree / rUsage)))

  return(nCores)
}

#' Escapes characters that have special meaning in regular expressions with backslashes.
#' @keywords internal
#' @author Daniel Hoop
#' @param x Character vector containing the strings that should be escaped.
#' @examples
#' escapeStr(c("asdf.asdf", ".asdf", "\\.asdf", "asdf\\.asdf") )
escapeStr <- function(x) {
  if (!is.character(x))
    stop("x must be a character vector.")
  regChars <- c(".","|","(",")","[","]","{","}","^","$","*","+","-","?")
  res <- apply(matrix(x), 1, function(y) {
    if (nchar(y)==1 && y%in%regChars)
      return (paste0("\\", y))
    for (i in nchar(y):2) {
      if ( substr(y, i-1, i-1) != "\\" && substr(y, i, i) %in% regChars )
        y <- paste0(substr(y, 1, i-1), "\\", substr(y, i, nchar(y)))
    }
    if (substring(y, 1, 1) %in% regChars)
      y <- paste0("\\", y)
    return (y)
  })
  return (res)
}
#' Same as \code{\link{escapeStr}}, but deprecated. Will throw an error.
#' @export
#' @author Daniel Hoop
unregex <- function (x) {
  stop ("The function `unregex` is depcrecated. Use the function `escapeStr` instead.")
}

minmax <- function(x, na.rm=TRUE) {
  return(range(x, na.rm=na.rm))
}

#' Change encoding of a file
#' @export
#' @author Daniel Hoop
#' @details This function works only on UNIX systems and is based on the 'iconvs' command.\cr
#' Available encodings are, amongst others,\cr
#' \itemize{
#' \item WINDOWS-1252 (or CP1252)
#' \item ISO-8559-1
#' \item UTF-8
#' }
#' For more encodings, consult: \code{man 7 charsets}
#' @examples
#' con <- file("enctest.txt", encoding = "utf-8")
#' writeLines("a ä o ö u ü", con)
#' close(con)
#'
#' changeFileEncoding(fileIn = "enctest.txt", fileOut = "enctest_CP1252.txt", encodingIn = "UTF-8", encodingOut = "WINDOWS-1252")
#' changeFileEncoding(fileIn = "enctest_CP1252.txt", fileOut = "enctest_UTF-8.txt", encodingIn = "WINDOWS-1252", encodingOut = "UTF-8")
#'
#' print(readLines("enctest.txt")) # OK.
#' print(readLines("enctest_CP1252.txt")) # Not looking good...
#' print(readLines("enctest_UTF-8.txt")) # OK.
#'
#' file.remove(c("enctest.txt", "enctest_CP1252.txt", "enctest_UTF-8.txt"))
changeFileEncoding <- function(fileIn, fileOut, encodingIn = "CP1252", encodingOut = "UTF-8", verbose = FALSE) {
  if (length(fileIn) != 1)
    stop("`length(fileIn)` must be equal to 1.")
  if (length(fileOut) != 1)
    stop("`length(fileOut)` must be equal to 1.")
  if (Sys.info()["sysname"] == "Windows")
    stop("Only works on UNIX based systems.")
  if (fileIn == fileOut) {
    choice <- menu(title = "Are you sure that you want to overwrite the original file?",
                   choices = c("Yes", "No"))
    if (choice != 1)
      stop("Aborted.")
  }
  system(paste0("iconv ", if (verbose) "--verbose ", "-f ", encodingIn, " -t ", encodingOut ," -o ",
                "\"", fileOut, "\" ",
                "\"", fileIn, "\""))
}

#' Move a file
#' @export
#' @author Daniel Hoop
file.move <- function(from, to) {
  todir <- dirname(to)
  if ( any(is.na(file.info(todir)$isdir)) || any(!file.info(todir)$isdir) ) {
    stop("The directory to which shall be copied does not exist.")
    dir.create(todir, recursive=TRUE)
  }
  if( length(to)==1 && length(from)>1 && isTRUE(file.info(to)$isdir) ){
    to <- paste0(to,"/",basename(from))
  }
  success <- file.rename(from = from,  to = to)
  if(any(!success)) stop("Some files were not moved. Files not accessible? Or they might already exist in the copy directory?")
}


#' @export
#' @author Daniel Hoop
#' @title Fast version of file.copy implemented by \code{\link[fs:file_copy]{fs::file_copy}}
#' @description However, it behaves the same as does \code{\link[base:file.copy]{base::file.copy}}. The default arguments are set to match \code{\link[fs:file_copy]{fs::file_copy}} behaviour.
#' If any of \code{recursive}, \code{copy.mode} or '\code{copy.date} are not set to default, then \code{\link[utils::file.copy]{utils::file.copy}} will be called (not \code{\link[fs:file_copy]{fs::file_copy}}).
#' @seealso \code{\link[fs:file_copy]{fs::file_copy}}, \code{\link[base:file.copy]{base::file.copy}}.
file.copy <- function (from, to, overwrite = recursive, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE) {

  # Normal function call, if not default settings for some arguments.
  if (!"fs"%in%rownames(installed.packages())
      || recursive || !copy.mode || !copy.date)
    return (base::file.copy(from=from, to=to, overwrite=overwrite, recursive=recursive, copy.mode=copy.mode, copy.date=copy.date))

  # Handles length of from, to
  if (length(from) == 0 || length(to) == 0)
    return (logical())
  if (length(to) != 1 && length(from) != length(to))
    stop ("length(to) must be either 1 or of length(from)")
  if (length(from) > 1 & length(to) == 1) {
    if (isTRUE(file.info(to)$isdir)) {
      to <- paste0(to, "/", basename(from))
    } else {
      to <- rep(to, length(from))
    }
  } else {
    isDir <- which(is.dir(to))
    to[isDir] <- paste0(to[isDir], "/", basename(from[isDir]))
  }
  # Copy data
  return (
    apply(cbind(from,to), 1, function (x) tryCatch({
      fs::file_copy(path=x[1], new_path=x[2], overwrite=overwrite)
      return (TRUE)
    }, error=function(e) return(FALSE)
    , warning=function(w) return(FALSE))))
}

#fromFile <- agsPath("Y:/ZADaten/SpE/Liste_Plausible/B2016/5_Termin/Plausible_B2016.csv") # //art-settan-1000-evdad.admin.ch
#fromFile="//adb.intra.admin.ch/Agroscope$/Org/Sites/TA/Transfer/hpda/MATLAB_goya/Matlab_Skripte/_Erkl?rungsfile.xlsx";                   toBaseDir="G:/_";                   urlAlias="O"
file.copy.with.dir <- function(fromFile, toBaseDir, urlAlias=NULL){

  if(length(fromFile)>1 && length(toBaseDir)==1){
    toBaseDir <- rep(toBaseDir, length(fromFile))
  }
  if(length(fromFile) != length(toBaseDir)) stop("length(form) must be equal length(toBaseDir).")
  if(!is.null(urlAlias) && length(fromFile) != length(urlAlias)) stop("length(fromFile) must be equal length(urlAlias).")

  # Recursive function definition for vector aruments
  if(length(fromFile)>1 && length(toBaseDir)>1) {
    return(apply(matrix(c(fromFile,toBaseDir,urlAlias),nrow=length(fromFile)),1,function(x)copy.dir.structure(fromFile=x[1], toBaseDir=x[2], urlAlias=x[3])))
  }

  if(!dir.exists(toBaseDir)) stop(paste0(dir.exists, " does not exist (argument 'toBaseDir')"))
  if(!file.exists(fromFile)) stop(paste0(fromFile, " does not exist (argument 'fromFile')"))

  if( !grepl("^[a-z]:",fromFile,ignore.case=TRUE) ) {
    if(is.null(urlAlias)) stop("'fromFile' must start with a letter for the drive. Like C:/")

    fromFileBaseDir <- sub("//[^/]*/",paste0(urlAlias,"/"), gsub("\\\\","/",dirname(fromFile)),ignore.case=TRUE)
  } else {
    fromFileBaseDir <- sub(":","",dirname(fromFile))
  }

  toDir <- paste0(toBaseDir,"/",fromFileBaseDir)
  if(!dir.exists(toDir)) dir.create(toDir, recursive=TRUE)

  file.copy(fromFile, paste0(toDir, "/", basename(fromFile)))
}

#' @export
#' @author Daniel Hoop
#' @title Copy large files safely
#' @description First creates a temporary filename. Upon successfull copy, renames the file
file.copy.safely <- function(from, to, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE) {

  inner <- function(from, to, overwrite, copy.mode, copy.date) {
    if (!overwrite && file.exists(to))
      return(FALSE)
    tmpFile <- paste0(to, ".", random.string(), ".tmp")
    hasWorked <- file.copy(from, tmpFile, overwrite = overwrite, recursive = FALSE, copy.mode = copy.mode, copy.date = copy.date)
    if (!hasWorked)
      return(FALSE)
    hasWorked <- file.rename(tmpFile, to)
    if (!hasWorked)
      suppressWarnings(file.remove(tmpFile))
    return(hasWorked)
  }

  # Handles length of from, to (copied from file.copy function in zaUtils)
  if (length(from) == 0 || length(to) == 0)
    return (logical())
  if (length(to) != 1 && length(from) != length(to))
    stop ("length(to) must be either 1 or of length(from)")
  if (length(from) > 1 & length(to) == 1) {
    if (isTRUE(file.info(to)$isdir)) {
      to <- paste0(to, "/", basename(from))
    } else {
      to <- rep(to, length(from))
    }
  } else {
    isDir <- which(is.dir(to))
    to[isDir] <- paste0(to[isDir], "/", basename(from[isDir]))
  }

  return(apply(cbind(from,to), 1, function (x) {
    inner(from = x[1], to = x[2], overwrite = overwrite, copy.mode = copy.mode, copy.date = copy.date)
  }))
}

#' @export
#' @author Daniel Hoop
#' @title Moves large files safely
#' @description First creates a temporary filename. Upon successfull move, renames the file.
file.move.safely <- function(from, to, overwrite = FALSE) {

  inner <- function(from, to, overwrite) {
    if (!overwrite && file.exists(to))
      return(FALSE)
    tmpFile <- paste0(to, ".", random.string(), ".tmp")
    hasWorked <- file.rename(from, tmpFile)
    if (!hasWorked)
      return(FALSE)
    hasWorked <- file.rename(tmpFile, to)
    if (!hasWorked)
      suppressWarnings(file.remove(tmpFile))
    return(hasWorked)
  }

  # Handles length of from, to (copied from file.copy function in zaUtils)
  if (length(from) == 0 || length(to) == 0)
    return (logical())
  if (length(to) != 1 && length(from) != length(to))
    stop ("length(to) must be either 1 or of length(from)")
  if (length(from) > 1 & length(to) == 1) {
    if (isTRUE(file.info(to)$isdir)) {
      to <- paste0(to, "/", basename(from))
    } else {
      to <- rep(to, length(from))
    }
  } else {
    isDir <- which(is.dir(to))
    to[isDir] <- paste0(to[isDir], "/", basename(from[isDir]))
  }

  return(apply(cbind(from,to), 1, function (x) {
    inner(from = x[1], to = x[2], overwrite = overwrite)
  }))
}


# file <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/2/3583/Resultate/16-06-06/2014/allcosts_info.RData");  setwd.to.file(file)
# file <- "C:/Users/U80823148/_/ME/ME_data_out/data_final/allcosts_info.RData"; setwd.to.file(file)
setwd.to.file <- function(file){
  if(length(file)>1) stop("file must containt only one filename.")
  fil <- strsplit(file, "/|\\\\")[[1]]
  fil <- fil[-length(fil)]
  setwd(paste(fil,collapse="/"))
}


#' Tell if file is a directory.
#' @export
#' @author Daniel Hoop
is.dir <- function(path) {
  return( file.info(path)$isdir )
}

#is.finite.data.frame <- function(x){
#  # Error in is.finite(df) : default method not implemented for type 'list'
#  return(as.matrix(as.data.frame(lapply(x,function(x)is.finite(x)))))
#}
#is.numeric.data.frame <- function(x) {
#  return(as.data.frame(lapply(x,function(x)is.numeric(x)),stringsAsFactors=FALSE))
#}

#' Translate ZA-BH encodings
#' @export
#' @author Daniel Hoop
#' @param x The encoded vector that should be decoded.
#' @param name The name of the encoding, e.g. \code{"region"}.
#' @param length Either the \code{"long"}, or the \code{"short"} version of the decoded value. E.g. "Tal", or "Talregion". Sometimes, the long and the short version do not differ.
#' @param lang The language to decode to.
#' @param sample The sample for which the decoding should be done.
#' @param year Not yet implemented: The year vector which specifies the valid encoding. Each vector place in \code{year} must correspond the same vector place in \code{x}. This could be useful in the future if encodings change.
#' @details The encodings are managed in the following file: \code{//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/MerkmalCodierungen.csv}.
#' @examples
#' transl.za(c(1, 2, 3), "region")
#' transl.za(c(11, 21), "typ_s3", sample = "spe")
#' transl.za(c(11, 21), "typ_s3", sample = "ref")
transl.za <- function(x, name=c("ak_ausbildung", "ak_geschlecht", "ak_rolle", "kanton", "landbauform", "region", "sprache", "ths", "typ_s3", "typ_s4", "zone"),
                      length=c("long", "short"), lang=c("de", "fr", "it", "en"), sample=c("unspecific", "spe", "spb", "ref"), year = NULL) {
  if (length(name) != 1)
    stop ("`name` must be a character vector of length 1.")
  name <- match.arg(name)
  length <- match.arg(length)
  lang <- match.arg(lang)
  sample <- match.arg(sample)
  tab <- .keyValueStore$getOrSet(
    "transl.za",
    read.table(agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/MerkmalCodierungen.csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", comment.char="")
  )
  allNames <- sort(unique(tab[,"name"]))
  tab <- tab[tab[,"name"] == name, , drop=FALSE]
  if (nrow(tab) == 0)
    stop ("No translations found for name='", name, "'. The following are available: ", paste0(allNames, collapse=", "))
  if (length(unique(tab[,"length"])) > 1)
    tab <- tab[tab[,"length"] == length, , drop=FALSE]
  if (length(unique(tab[,"sample"])) > 1) {
    tab[is.na(tab[,"sample"]), "sample"] <- "unspecific"
    tab <- tab[grep(sample, tab[,"sample"]), , drop=FALSE]
  }
  if (all(is.na(tab[,lang])))
    lang <- "de"

  return (replace.values(tab[,"code"], tab[,lang], x))
}

#' Translate the encoding of the ZA-BH Betriebstyp.
#' @export
#' @author Daniel Hoop
#' @examples transl.typ(c(11, 11, 12))
transl.typ <- function(x, short=FALSE, FAT99=FALSE, give.tab=FALSE){

  # Wenn Typennummern in Vektor groesser als 99, dann ist es in der Schreibweise 15..
  s3.numb <- c(   11,           12,           21,          22,               23,                   31,                41,                   51,                          52,                          53,                   54)

  if(!short){
    s3.name <- c("Ackerbau","Spezialkulturen","Milchkuehe", "Mutterkuehe", "Rindvieh gemischt", "Pferde/Schafe/Ziegen", "Veredelung", "Kombiniert Milchkuehe/Ackerbau", "Kombiniert Mutterkuehe", "Kombiniert Veredelung", "Kombiniert Andere")
    if(FAT99) s3.name[c(3,5,8)] <- c("Verkehrsmilch", "Anderes Rindvieh", "Kombiniert Verkehrsmilch/Ackerbau")
  } else {
    s3.name <- c("Ackb","Spez","Milk","MuKu","RiGe","PfSZ","Vere","MiAc","KoMu","KoVe","KoAn")
    if(FAT99) s3.name[c(3,5,8)] <- c("VMil","AnRi","VMAc")
  }
  if(give.tab){ tab <- matrix(s3.numb); dimnames(tab) <- list(s3.name, "code"); return(tab) }

  if(x[1]>99) s3.numb <- s3.numb + 1500
  return(replace.values(s3.numb, s3.name, x))
}

#' Translate the encoding of the ZA-BH Region.
#' @export
#' @author Daniel Hoop
#' @examples transl.reg(c(1, 1, 1, 3))
transl.reg <- function(x, give.tab=FALSE){
  name <- c("Tal","Huegel","Berg")
  numb <- c(  1,     2,      3)
  if(give.tab){ tab <- matrix(numb); dimnames(tab) <- list(name, "code"); return(tab) }
  return(replace.values(numb, name, x))
}

#' Translate the encoding of the ZA-BH Kanton
#' @export
#' @author Daniel Hoop
#' @examples transl.kt(c(1, 20, 20, 19))
transl.kt <- function(x, give.tab=FALSE){
  name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU", "FL")
  numb <- 1:27
  if(give.tab){ tab <- matrix(numb); dimnames(tab) <- list(name, "code"); return(tab) }
  return(replace.values(numb, name, x))
}

#' Translate the encoding of the ZA-BH Landbauform.
#' @export
#' @author Daniel Hoop
#' @examples transl.lbf(c(4, 2, 2, 3))
transl.lbf <- function(x, give.tab=FALSE){
  name <- c("konv.","OeLN","Bio","Bio Umstell.")
  numb <- c(   1,     2,    3,       4)
  if(give.tab){ tab <- matrix(numb); dimnames(tab) <- list(name, "code"); return(tab) }
  return(replace.values(numb, name, x))
}

#' Translate the encoding of the ZA-BH Treuhandstelle.
#' @export
#' @author Daniel Hoop
#' @examples transl.ths(c(101, 101, 102, 110))
transl.ths <- function(x, give.tab=FALSE){
  # dat <- read.cb("no") # Quelle: \\evdad.admin.ch\AGROSCOPE_OS\2\5\2\1\1\1860\C_GB\B2014\A_Vers\Druckerei_Adr&Auflage_f_Versand
  ths.name <- c("Agro-Treuhand Ruetti AG", "Agro-Treuhand Schwand", "Agro-Treuhand Berner-Oberland",  "Agro-Treuhand Emmental", "Agro-Treuhand Aargau", "Agro-Treuhand Thurgau AG",
                "Agro-Treuhand Waldhof", "BBV Treuhand", "Agro-Treuhand Region Zuerich AG",  "BBV Treuhand", "BBV Treuhand", "Agro-Treuhand Schwyz GmbH",
                "SBV Treuhand und Schaetzungen", "Fidasol S.A.", "AgriGeneve",  "Cofida S.A.", "Service des comptabilites agricoles", "Service des comptabilites agricoles",
                "Service de l'Agriculture", "Fiduciaire SEGECA", "Fiduciaire SEGECA",  "Fondation Rurale Interjurassienne", "Landwirtschaftszentrum Visp",
                "Agro-Treuhand Seeland AG", "Agro-Treuhand Sursee", "Agro-Treuhand Uri, Nid- und Obwalden GmbH",  "Agro-Treuhand Uri, Nid- und Obwalden GmbH", "Agro-Treuhand Glarus",
                "Buendner Bauernverband", "Agro-Treuhand Solothurn-Baselland",  "Fessler Treuhand GmbH", "Studer-Korner Treuhand")
  ths.numb <- c(101L, 102L, 103L, 104L, 105L, 110L, 111L, 112L, 113L, 115L, 116L, 117L, 121L, 201L, 202L, 203L, 204L, 224L, 205L, 206L, 226L, 207L, 225L, 336L, 338L, 340L, 342L, 362L, 401L, 402L, 403L, 404L)

  if(give.tab){ tab <- matrix(ths.numb); dimnames(tab) <- list(ths.name, "code"); return(tab) }
  return(replace.values(ths.numb, ths.name, x))
}

#' Translate Za-BH-Merkmale into names.
#' @export
#' @author Daniel Hoop
#' @examples transl.mm("P430_0100_94000")
transl.mm <- function(x, gsub=FALSE, excel.format=FALSE){
  # Diese Funktion uebersetzt die Merkmalsnummern der Merkmalsliste ZA2015 in die Zeilen-Bezeichnungen laut REX.
  # Wenn gsub=TRUE findet suche in Strings statt, statt ganze Character-Vektorplaetze zu uebersetzen.
  # Wenn excel.forma=TRUE wird davor noch ein Abstand gemacht, falls am Anfang ein Rechenoperations-Zeichen steht.

  transmm_list <- .keyValueStore$getOrSet(
    "transl.mm$transmm_list",
    local({
      message("Reading in translation...")
      transmm_list <- as.matrix(read.table(agsPath(paste0("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/4/3/4285/MML/Berechn_Uebersetz_in_R/Data/Data_out/MML_Namen_Nummern_full.txt")), sep="\t", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA")))
      # Check if there are non duplicated tranlsations. If not, then delete the duplicated ones.
      if(any(duplicated(transmm_list[,"number"]))){
        transmm_list_dupl <- transmm_list[ duplicated( transmm_list[,"number"] ) | duplicated( transmm_list[,"number"], fromLast=TRUE ) ,];
        transmm_list_dupl <- transmm_list_dupl[order(transmm_list_dupl[,"number"]),]
        if( any(duplicated(transmm_list_dupl) & !duplicated(transmm_list_dupl)) ) {
          stop("Non duplicated entries in translation matrix.")
        } else {
          transmm_list <- transmm_list[ !duplicated(transmm_list[,"number"]) ,]
        }
      }
      return (transmm_list)
    })
  )

  if(gsub){
    transmm_list_tmp <- transmm_list[ grepl(paste(x,collapse="|") ,transmm_list[,"number"]) ,]
  }
  x <- replace.values(transmm_list[,"number"], transmm_list[,"name"], x, gsub=gsub)

  if(excel.format){
    filt <- substr(x,1,1)%in%c("=","+","-","/","*")
    x[filt] <- paste0(" ",x[filt])
  }
  return(x)
}

#' Translate the rows in the ZA-BH-Referenzbetriebs-Table 210 to the Betriebszweigs-Namen.
#' @export
#' @author Daniel Hoop
#' @examples transl.ref.210row("00200")
transl.ref.210row <- function(x, give.tab=FALSE){
  t1 <- matrix(c(
    "Weizen","00200"     ,"Roggen","00300"    ,"Korn_Dinkel","00400"     ,"Mischel_ua_Brot","00500"
    ,"Gerste","00600"     ,"Hafer","00700"    ,"Triticale","00800"     ,"Mischel_ua_Futt","00900"
    ,"Koernermais","01000"     ,"Koernermais_uebr","01100"    ,"Silomais","01200"     ,"Silomais_alt","01201"
    ,"Kartoffeln","01300"     ,"Zueckerrueben","01400"    ,"Futterrueben","01500"     ,"Futterrueben_alt","01501"
    ,"Raps","01600"     ,"Industrieraps","01700"    ,"Sojabohnen","01800"     ,"Sonnenblumen","01900"
    ,"Oelsaaten_uebr","02000"     ,"einj_nachw_Rohst","02100"    ,"Hanf","02200"     ,"Ackerbohnen","02300"
    ,"Eiweisserbsen","02400"     ,"Koernerlegum_uebr", "02500"    ,"Tabak","02600"     ,"Maschinenbohnen","02700"
    ,"Drescherbsen","02800"     ,"Maschinenkarotten","02900"    ,"Maschinenspinat","03000"     ,"Konservengemuese_uebr","03100"
    ,"Karotten_Freiland","03200"     ,"Zwiebeln_Freiland","03300"    ,"Kabis_Freiland","03400"     ,"Randen_Freiland","03500"
    ,"Blumenkohl_Freiland","03600"     ,"Freilandgemuese_uebr","03700"    ,"einj_Beeren","03800"     ,"einj_Gewuerz_u_Medizin","03900"
    ,"einj_gaertn_Freilandkult","04000"    ,"Spezkult_in_GH","04100"    ,"gaertn_Kult_in_GH","04200"     ,"Buntbrache","04400"
    ,"Rotationsbrache","04500"    ,"Rotationsbrache_alt","04501"    ,"Saum_auf_Ackerfl","04520"     ,"Bluehstreifen_ab2015","04530"
    ,"Ackerschonstreifen","04600"     ,"einj_Ackerkult_uebr","04700"    ,"Gruenland_LeistKost","06000"     ,"Futterbau_LeistKost_alt","06001"
    ,"Kunstwiesen","06100"     ,"oekolAusgl_Kunstwiesen_alt","06101"    ,"oekolAusgl_Naturwiesen_alt","06102"     ,"Wiesen_ext_auf_Aeck","06200"
    ,"Wiesen_ext","06300"     ,"Wiesen_wenig_intens","06400"    ,"Dauerwiesen_andere","06500"     ,"Uferwiesen","06550"
    ,"Weiden_ext","06600"     ,"Waldweiden","06700"    ,"Weiden","06800"     ,"Weiden_f_Schw","06900"
    ,"Heuwiesen_Alp1","07000"     ,"Heuwiesen_Alp2","07010"    ,"Heuwiesen_Alp3","07020"     ,"Gruenfl_uebr","07100"
    ,"Alpweiden_alt","07101"     ,"Wiesen_ZwiFu","07200"    ,"Reben","08000"     ,"Obst_ohne_Flaeche","08100"
    ,"Obst_mit_flaeche","08200"     ,"mehrj_Erdbeeren","08300"    ,"Himbeeren","08400"     ,"Strauchbeeren_uebr","08500"
    ,"mehrj_Gewuerz_Medizin","08600"     ,"Chinaschilf","08700"    ,"mehrj_nachw_Rohst","08800"    ,"Hopfen","08900"
    ,"mehrj_Spezkult_uebr","09000"    ,"Weihnachtsbaeume","09100"    ,"mehrj_gaertn_Freilandkult","09200"     ,"Dauerkult_uebr","09300"
    ,"Streue_Torfland","10000"     ,"HeckFeldUfer_Gehoelz","10100"    ,"uebr_Flaechen_LN","10200"     ,"Wald","10300"
    ,"Strohverkauf","10400"     ,"Pflanzenbau_nicht_zuteilbar","10500"  ), ncol=2, byrow=TRUE)
  rownames(t1) <- t1[,1]
  t1 <- t1[,2,drop=FALSE]
  colnames(t1) <- "code"

  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), x))
  }
}

#' Translate the rows in the ZA-BH-Referenzbetriebs-Table 300 (Tiere) to the Betriebszweigs-Namen.
#' @export
#' @author Daniel Hoop
#' @examples transl.ref.300row("00300")
transl.ref.300row <- function(x, give.tab=FALSE){

  if (FALSE) {
    # Import the data from REX: Copy the colmuns B:G from Tab 300 in MML. Then Perform this little script.
    t1 <- read.cb("no")
    t1 <- t1[, c(ncol(t1):(1:(ncol(t1)-1)))]
    t1 <- cbind(t1[, 1], paste.cols(t1[, -1]))
    t1[, 1] <- gsub("'", "", t1[, 1])
    t1[, 2] <- gsub("^_|_$", "", t1[, 2])
    apply(t1, 1, function (row) cat(row[1], ", \"", row[2], "\"\n", sep = ""))
  }

  t1 <- matrix(c(
    "00100", "Rindviehhaltung",
    "00200", "Milchvieh_und_Aufzucht_(und_gelegentliche_Mast)",
    "00210", "Zuwachs_EA_59_Zu._und_Wiederverkauf_EA_55_(nur_Konversion)",
    "00300", "Kälber__<_4_Monate_alt",
    "00400", "Jungvieh_>_4_Monate_alt_(inkl_Stiere)",
    "00450", "Zukauf_Verkauf_Jungvieh/Stiere_EA_53_51(nur_Konversion)",
    "00500", "Kühe",
    "00510", "Korrektur__Milchvieh_und_Aufzucht",
    "00550", "Kühe_zusammen",
    "00600", "Kühe_zur_Verkehrsmilchproduktion",
    "00700", "Kühe_gemolken_keine_Verkehrsmilchproduktion",
    "00750", "Rinder_Stiere_über_2.jährig",
    "00760", "Rinder_Stiere_1_bis_2.jährig",
    "00800", "Rinder_über_2.jährig",
    "00900", "Rinder_1_bis_2.jährig",
    "01000", "Stiere_über_2.jährig",
    "01100", "Stiere_1_bis_2.jährig",
    "01200", "Rinder_Stiere_und_Ochsen_über_4_Monate_(nur_mast)",
    "01250", "Jungvieh_zur_Zucht_4_bis_12_Monate_alt_w./m.",
    "01300", "Jungvieh_zur_Zucht_4_bis_12_Monate_alt_w.",
    "01400", "Jungvieh_zur_Zucht_4_bis_12_Monate_alt_m.",
    "01450", "Aufzuchtkälber_unter_4_Monate_alt_w./m.",
    "01500", "Aufzuchtkälber_unter_4_Monate_alt_w.",
    "01600", "Aufzuchtkälber_unter_4_Monate_alt_m.",
    "01700", "Kälber_zur_Grossviehmast_unter_4_Monate",
    "01800", "Mastkälber",
    "01850", "Kuhmilch",
    "01860", "Kuhmilch.Produkte",
    "01870", "Verschiedene_Leistungen",
    "01900", "Mutter._und_Ammenkuhhaltung_total_ohne_Details",
    "01910", "Korrektur_Mutter_und_Ammenkuhhaltung",
    "02000", "Mutter._und_Ammenkühe_(ohne_Kälber)",
    "02050", "Zusammenfassung_Jungvieh_(Rinder)_(nur_Konversion)",
    "02060", "Rinder_Stiere_über_2.jährig",
    "02070", "Rinder_Stiere_1_bis_2.jährig",
    "02100", "Rinder_über_2.jährig",
    "02200", "Rinder_1_bis_2.jährig",
    "02300", "Stiere_über_2.jährig",
    "02400", "Stiere_1_bis_2.jährig",
    "02500", "Kälber_von_Mutter_und_Ammenkühen_unter_1.jährig",
    "02600", "Grossviehmast_total_ohne_Details",
    "02610", "Korrektur_Grossviehmast_total_ohne_Details",
    "02700", "Rinder_Stiere_und_Ochsen_über_4_Monate",
    "02800", "Kälber_zur_Grossviehmast_unter_4_Monate",
    "03000", "Kälbermast",
    "03100", "Mastkälber",
    "05000", "Pferdehaltung_(und_andere_Einhufer)",
    "05050", "Pferdehaltung_total_ohne_Details",
    "05100", "Säugende_Stuten",
    "05200", "Fohlen_bei_Fuss",
    "05300", "Andere_Pferde_über_3.jährig",
    "05400", "Andere_Fohlen_unter_3.jährig",
    "05500", "Maultiere_und_Maulesel_jeden_Alters",
    "05600", "Ponys_und_Kleinpferde_jeden_Alters",
    "05700", "Esel_jeden_Alters",
    "06000", "Schafhaltung",
    "06050", "Schafhaltung_total_ohne_Details",
    "06100", "Schafe_gemolken",
    "06200", "Andere_weibliche_Schafe_über_1.jährig",
    "06250", "weibliche_Schafe_(Muttertiere)_(nur_Konversion)",
    "06300", "Widder_über_1.jährig",
    "06400", "Jungschafe_unter_1.jährig_(weiblich_und_männlich)",
    "06550", "Schafmilch",
    "06560", "Schafmilch.Produkte_",
    "06570", "Verschiedene_Leistungen",
    "07000", "Ziegenhaltung",
    "07050", "Ziegenhaltung_total_ohne_Details",
    "07100", "Ziegen_gemolken",
    "07200", "Andere_weibliche_Ziegen_über_1.jährig",
    "07250", "weibliche_Ziegen_(Muttertiere)_(nur_Konversion)",
    "07300", "Ziegenböcke_über_1.jährig",
    "07400", "Jungziegen_unter_1.jährig_(weiblich_und_männlich)",
    "07550", "Ziegenmilch",
    "07560", "Ziegenmilch.Produkte_",
    "07570", "Verschiedene_Leistungen",
    "07800", "Raufutterverzehrer",
    "08000", "Schweine",
    "08100", "Schweine_allgemein",
    "08110", "Korrektur_Schweine_allgemein",
    "08150", "Muttersauen_säugend_+_nicht_säugend_(nur_Konv.)",
    "08200", "Säugende_Zuchtsauen",
    "08300", "Nicht_säugende_Zuchtsauen_über_6_Monate_alt",
    "08400", "Zuchteber",
    "08450", "Abgesetzte_Ferkel_Remonten_b._6_Mon._u.Mastschweine",
    "08500", "Abgesetzte_Ferkel",
    "08600", "Saugferkel",
    "08700", "Remonten_bis_6_Monate_und_Mastschweine",
    "09000", "Schweinezucht_",
    "09010", "Korrektur_Schweinezucht",
    "09050", "Muttersauen_säugend_+_nicht_säugend_",
    "09100", "Säugende_Zuchtsauen",
    "09200", "Nicht_säugende_Zuchtsauen_über_6_Monate_alt",
    "09300", "Zuchteber",
    "09350", "Abgesetzte_Ferkel_Remonten_b._6_Mon._u.Mastschweine",
    "09400", "Abgesetzte_Ferkel",
    "09500", "Saugferkel",
    "09600", "Remonten_b._6_Monate_und_Mastschweine",
    "10000", "Schweinemast",
    "10010", "Korrektur_Schweinemast",
    "10050", "Abgesetzte_Ferkel_Remonten_b._6_Mon._u.Mastschweine",
    "10100", "Abgesetzte_Ferkel_",
    "10200", "Remonten_bis_6_Monate_und_Mastschweine",
    "11000", "Arbeitsteilige_Ferkelproduktion_Deckbetrieb",
    "11100", "Arbeitsteilige_Ferkelproduktion_Abferkelbetrieb",
    "11101", "Korrektur_AFP_Abferkelbetrieb",
    "11105", "Muttersauen_säugend_+_nicht_säugend_",
    "11110", "Säugende_Zuchtsauen",
    "11120", "Nicht_säugende_Zuchtsauen_über_6_Monate_alt",
    "11140", "Abgesetzte_Ferkel",
    "11150", "Saugferkel",
    "11200", "Arbeitsteilige_Ferkelproduktion_Ferkelaufzuchtbetrieb",
    "11300", "Arbeitsteilige_Ferkelproduktion_Wartebetrieb",
    "12000", "Nutzgeflügel",
    "12100", "Hühner",
    "12200", "Zuchthennen_und_.hähne_(Lege_oder_Mastlinie)",
    "12300", "Legehennen",
    "12400", "Eier",
    "12500", "Verschiedene_Leistungen",
    "12600", "Junghennen_Junghähne_und_Kücken",
    "12700", "Mastpoulets_jeden_Alters",
    "12800", "Truten_jeden_Alters",
    "12900", "übriges_Nutzgeflügel",
    "13000", "Andere",
    "13100", "Andere_Raufutterverzehrende_Nutztiere_total_ohne_Details",
    "13200", "Bisons_über_3.jährig",
    "13300", "Bisons_unter_3.jährig",
    "13400", "Damhirsche_jeden_Alters",
    "13500", "Rothirsche_jeden_Alters",
    "13550", "Anzahl_Muttertiere_Hirsche",
    "13600", "Lamas_über_2.jährig",
    "13700", "Lamas_unter_2.jährig",
    "13800", "Alpakas_über_2.jährig",
    "13900", "Alpakas_unter_2.jährig",
    "14000", "andere_Raufutter_verzehrende_Nutztiere",
    "14999", "M77xx3100__übr._Raufutterverzehrende_GVE",
    "15000", "Bienen_(Völker_Honig)",
    "16000", "Andere_Tiere_total_ohne_Details",
    "17000", "Lohnmast_Kälber_(nur_Tierbestände)",
    "17100", "Lohnmast_Schweine_(nur_Tierbestände)",
    "17200", "Lohnmast_Poulets_(nur_Tierbestände)",
    "17300", "Lohnmast_Truten_(nur_Tierbestände)",
    "17400", "Lohnmast_andere_Tiere_(nur_Tierbestände)",
    "17500", "Tierhaltung_nicht_auf_Betriebszweig_zuteilbar",
    "17599", "übrige_nicht_raufutterverzehrende_GVE_M77**5600",
    "90000", "Total_Tierhaltung",
    "90100", "Raufutterverzehrende_Tiere_(inkl._Zeile_Raufutter)",
    "90200", "Schweine_und_Geflügel"
  ), ncol = 2, byrow = TRUE)

  rn1 <- t1[, 2]
  t1 <- as.matrix(as.numeric(t1[, 1]))
  rownames(t1) <- rn1

  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), as.numeric(x)))
  }
}


#' Translate bewteen the columns in ZA-BH-SpB-Table 330 and the rows in Table 340.
#' @export
#' @author Daniel Hoop
#' @examples
#' transl.spb.330col.340row(c(2011, 2012))
#' transl.spb.330col.340row(c(11000), reverse = TRUE)
#' transl.spb.330col.340row(give.tab = TRUE)
transl.spb.330col.340row <- function(x, reverse=FALSE, give.tab=FALSE, nice.names=FALSE){
  # This function translates the numbers from MML Tab 330 columns  to  Tab 340 rows (and vice versa if reverse=TRUE).
  # Arguments
  # x        = The number to be translated from one tab to another
  # reverse  = Translate from 340 row -> 330 col
  # give.tab = Return translation table without translating anything.

  #dput(as.numeric(unname(unlist(read.cb("no")))))
  #dput(unname(as.matrix(read.cb("no"))))
  t1 <- matrix(c(
    2004L, 50000L,
    2011L, 11000L,
    2012L, 13000L,
    2013L, 15000L,
    2014L, 16000L,
    2015L, 17000L,
    2018L, 19000L,
    2019L, 18000L,
    2021L, 61000L,
    2022L, 62000L,
    2023L, 63000L,
    2024L, 64000L,
    2028L, 69000L,
    2031L, 71000L,
    2032L, 72000L,
    2033L, 73000L,
    2034L, 75000L,
    2038L, 79000L,
    2075L, 21000L,
    2080L, 33000L,
    2085L, 34000L,
    2090L, 35000L,
    2095L, 81000L,
    2100L, 83000L)
    , ncol = 2, byrow = TRUE)
  colnames(t1) <- c("Tab330col", "Tab340row")
  rownames(t1) <- c("alleRaufu", "Milch","Muku","Kaelberm","Rindviehm","fremdesRindvieh","Rind/Kaelberm-3","uebrigesRindvieh","SchweineAllg","Schweinezucht","Schweinemast","Ferkelprod","Schweinemast-3","Konsumeier","Bruteier","Pouletmast","Truten","Gefluegel-3","Pferde","Schafe","Ziegen","sonstigeRaufu","Kaninchen","uebrigesGeflu")
  if(nice.names) rownames(t1) <- c("Raufutterverzehrer total", "Milchkuehe","Mutterkuehe","Kaelbermast","Rindviehmast","Haltung fremdes Rindvieh","Rinder- & Kaelbermast fuer Dritte","Uebriges Rindvieh",
                                   "Schweine (Zucht & Mast)","Schweinezucht","Schweinemast","Arbeitsteilige Ferkelproduktion","Schweinemast fuer Dritte","Konsumeierproduktion","Bruteierproduktion","Pouletmast","Truten","Gefluegelmast fuer Dritte","Pferde","Schafe","Ziegen","Andere Raufutter verzehrende Tiere","Kaninchen","Uebriges Gefluegel")
  t1 <- t1[order(t1[,"Tab340row"]),]

  if(give.tab) {
    return(t1)
  }

  if(!reverse) col_in <- 1 else col_in <- 2
  if(!reverse) col_ou <- 2 else col_ou <- 1
  row_ou <- match(x,t1[,col_in])
  res <- t1[row_ou,col_ou]
  names(res) <- rownames(t1)[row_ou]
  return(res)
}

#' Translate the rows in the ZA-BH-SpB-Table 320 to the Betriebszweigs-Namen.
#' @export
#' @author Daniel Hoop
#' @examples transl.spb.320row("11110")
transl.spb.320row <- function(x, give.tab=FALSE){
  # This function translates the numbers from MML Tab 320 rows  to  the german names of the enterprises.
  # Arguments
  # x        = The number to be translated to enterprise name.
  # give.tab = Return translation vector without translating anything.

  # Import the data from MML: Copy the following columns of tab 320 side by side: 1) Zeile [UID, 2nd col], 2) Total Leistung [col no 4000], 3) all cols containing the culture names (no matter how many). They will be pasted together.
  # t1 <- read.cb("no"); t1[is.na(t1)] <- ""; t1 <- t1[t1[,2]!="",]; t1[,1] <- gsub("'", "", t1[,1]); t1 <- cbind(t1[,1:2], apply(t1[,3:ncol(t1)],1,function(x)paste0(x,collapse="")), stringsAsFactors=FALSE); dput(as.numeric(t1[,1])); t1[,3] <- replace.values(c("ae","Ae","oe","Oe","ue","Ue"), c("ae","Ae","oe","Oe","ue","Ue"), t1[,3], gsub=TRUE); df1 <- as.list(t1[,3]); names(df1) <- t1[,3]; df1 <- as.data.frame(df1); dput(gsub("_$", "", gsub("\\.+","_", colnames(df1))))
  # ALT: t1 <- read.cb("no"); t1[is.na(t1)] <- ""; t1 <- t1[t1[,2]!="",]; t1 <- cbind(t1[,1:2], apply(t1[,3:ncol(t1)],1,function(x)paste0(x,collapse="")), stringsAsFactors=FALSE); dput(as.numeric(t1[,1])); dput(t1[,3])

  t1 <- matrix(c(11110, 11120, 11130, 11140, 11180, 11310, 11320, 11330, 11340,
                 11380, 12100, 12200, 12500, 12600, 13000, 14100, 14300, 15100,
                 15200, 15300, 15900, 16100, 16200, 16900, 18100, 18200, 18300,
                 18400, 19000, 20000, 20100, 20200, 21100, 21200, 21300, 21400,
                 30100, 35000, 41000, 42100, 42200, 43000, 43500, 44000, 44500,
                 46000, 48000, 49000, 61000, 65000, 70000, 81000, 85000))
  colnames(t1) <- "code"
  rownames(t1) <- c("Weizen_Brotgetreide", "Roggen_Brotgetreide", "Dinkel_Brotgetreide",
                    "Emmer_Einkorn", "Mischel_Brotgetreide", "Gerste_Futtergetreide",
                    "Hafer_Futtergetreide", "Triticale_Futtergetreide", "Futterweizen",
                    "Mischel_Futtergetreide", "Koernermais", "Silo_Gruenmais", "Saatmais",
                    "Hirse", "Kartoffeln", "Zuckerrueben", "Futterrueben", "Raps_zur_Speiseoelgewinnung",
                    "Soja", "Sonnenblumen_zur_Speiseoelgewinnung", "uebrige_Oelsaaten",
                    "Ackerbohnen_zu_Futterzwecken", "Eiweisserbsen_zu_Futterzwecken",
                    "uebrige_Koernerleguminosen", "Tabak", "Einjaehrige_gaertnerische_Freilandkulturen",
                    "Einjaehrige_nachwachsende_Rohstoffe", "Einjaehrige_Freilandgemuese_ohne_Konservengemuese",
                    "Freiland_Konservengemuese", "Einjaehrige_Beeren_z_B_Erdbeeren",
                    "Einjaehrige_Gewuerz_Medizinalpflanzen", "Uebrige_Ackerkulturen",
                    "Buntbrache", "Rotationsbrache", "Saum_auf_Ackerflaeche", "Ackerschonstreifen",
                    "Futterbau_ohne_Silomais_Futterrueben_und_Samenproduktion", "Samenproduktion_Futterbau",
                    "Reben", "Obst_Streuobst_ohne_Flaeche", "Obstanlagen", "Mehrjaehrige_Beeren",
                    "Mehrjaehrige_nachwachsende_Rohstoffe", "Hopfen", "Mehrjaehrige_Gewuerz_Medizinalpflanzen",
                    "Gemuese_Dauerkulturen", "Christbaeume_Baumschulen_und_aehnliches",
                    "Uebrige_Dauerkulturen", "Gewaechshaus_und_Tunnel_Frischgemuese",
                    "Gaertnerische_Kulturen_in_geschuetztem_Anbau", "Weitere_Flaechen_innerhalb_der_LN",
                    "Wald", "Weitere_Flaechen_ausserhalb_der_LN")

  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), as.numeric(x)))
  }
}

#' Translate the rows in the ZA-BH-SpB-Table 330 to the Betriebszweigs-Namen.
#' @export
#' @author Daniel Hoop
#' @param x The number to be translated to enterprise name.
#' @param give.tab Return translation vector without translating anything.
#' @examples transl.spb.330row("30000")
transl.spb.330row <- function(x, give.tab=FALSE){
  # Import the data from MML: Copy the following columns of tab 330 side by side: 1) Zeile [UID, 2nd col], 2) Total Leistung [col no 4000], 3) all cols containing the animal names (no matter how many). They will be pasted together.
  # t1 <- read.cb("no"); t1[is.na(t1)] <- ""; t1 <- t1[t1[,2]!="",]; t1[,1] <- gsub("'", "", t1[,1]); t1 <- cbind(t1[,1:2], apply(t1[,3:ncol(t1)],1,function(x)paste0(x,collapse="")), stringsAsFactors=FALSE); dput(as.numeric(t1[,1])); t1[,3] <- replace.values(c("ae","Ae","oe","Oe","ue","Ue"), c("ae","Ae","oe","Oe","ue","Ue"), t1[,3], gsub=TRUE); df1 <- as.list(t1[,3]); names(df1) <- t1[,3]; df1 <- as.data.frame(df1); dput(gsub("_$", "", gsub("\\.+","_", colnames(df1))))

  t1 <- matrix(c(
    1000, 10000, 11100, 11200, 11510, 11530, 11550, 11570, 11610,
    11630, 11650, 11670, 11900, 20000, 21100, 21600, 21500, 21700,
    21800, 24100, 24300, 24500, 27100, 27300, 27500, 30000, 33000,
    33100, 33200, 33300, 33500, 33800, 34000, 34100, 34200, 34300,
    34400, 34500, 34800, 35000, 35110, 35150, 35300, 35400, 35610,
    35650, 35810, 35850, 60000, 61100, 61200, 61300, 61500, 61600,
    61800, 70000, 71000, 72100, 72200, 72300, 73000, 75100, 75200,
    75300, 80000, 81100, 81300, 83110, 83150))
  colnames(t1) <- "code"
  rownames(t1) <- c(
    "Tiere_total", "Rindergattung_Wasserbueffel", "Milchkuehe_abgekalbt",
    "Andere_Kuehe_abgekalbt", "Weibliche_Tiere_ueber_730_Tage_alt_bis_zur_1_Abkalbung",
    "Weibliche_Tiere_ueber_365_bis_730_Tage_alt", "Weibliche_Tiere_ueber_160_bis_365_Tage_alt",
    "Weibliche_Tiere_bis_160_Tage_alt", "Maennliche_Tiere_ueber_730_Tage_alt",
    "Maennliche_Tiere_ueber_365_bis_730_Tage_alt", "Maennliche_Tiere_ueber_160_bis_365_Tage_alt",
    "Maennliche_Tiere_bis_160_Tage_alt", "uebrige_Rinder_SpE_Erhebungsbogen",
    "Pferdegattung", "Saeugende_und_traechtige_Stuten", "Andere_weibliche_und_kastrierte_maennliche_Pferde_ueber_30_Monate_alt",
    "Hengste_ueber_30_Monate_alt", "Fohlen_bei_Fuss_im_Faktor_der_Mutter_eingerechnet",
    "Fohlen_bis_30_Monate_alt", "Weibliche_und_maennliche_kastrierte_Maultiere_und_Maulesel_ueber_30_Monate_alt",
    "Hengste_Maultiere_und_Maulesel_ueber_30_Monate_alt", "Maultiere_und_Maulesel_bis_30_Monate_alt",
    "Weibliche_und_maennliche_kastrierte_Ponys_Kleinpferde_und_Esel_ueber_30_Monate_alt",
    "Hengste_Ponys_Kleinpferde_und_Esel_ueber_30_Monate_alt", "Ponys_Kleinpferde_und_Esel_bis_30_Monate_alt",
    "Schafe_Ziegen_und_andere_Raufutter_verzehrende_Nutztiere", "Schafe",
    "Schafe_gemolken", "Andere_weibliche_Schafe_ueber_1_Jahr_alt",
    "Widder_ueber_1_jaehrig", "Weidelaemmer_Mast_unter_1_2_jaehrig_welche_nicht_den_Muttertieren_anzurechnen_sind_ganzjaehrige_Weidelaemmermast",
    "Jungschafe_unter_1_Jaehrig_im_Faktor_der_Mutter_eingerechnet",
    "Ziegen", "Ziegen_gemolken", "Andere_weibliche_Ziegen_ueber_1_Jahr_alt",
    "Ziegenboecke_ueber_1_jaehrig", "Zwergziegen_ueber_1_jaehrig",
    "Zwergziegen_unter_1_jaehrig", "Jungziegen_unter_1_jaehrig_im_Faktor_der_Mutter_eingerechnet",
    "andere_Raufutterverzehrende_Nutztiere", "Bisons_ueber_3_jaehrig_bis_B2017",
    "Bisons_unter_3_jaehrig_bis_B2017", "Damhirsche_jeden_Alters",
    "Rothirsche_jeden_Alters", "Lamas_ueber_2_jaehrig", "Lamas_unter_2_jaehrig",
    "Alpakas_ueber_2_jaehrig", "Alpakas_unter_2_jaehrig", "Schweine",
    "Zuchteber", "Nicht_saeugende_Zuchtsauen_ueber_6_Monate_alt_ca_3_Umtriebe_pro_Platz",
    "Saeugende_Zuchtsauen", "Abgesetzte_Ferkel", "Remonten_und_Mastschweine_ca_3_Umtriebe_pro_Platz",
    "Saugferkel_im_Faktor_der_Mutter_eingerechnet", "Nutzgefluegel",
    "Konsumeier_produzierende_Hennen", "Zuchthennen_haehne_Bruteierproduktion_Mastlinien",
    "Zuchthennen_haehne_Bruteierproduktion_Legelinien", "Junghennen_Junghaehne_und_Kueken",
    "Mastpoulets_jeden_Alters_6_5_bis_7_5_Umtriebe", "Truten_jeden_Alters_ca_3_Umtriebe_pro_Platz",
    "Trutenvormast_ca_6_Umtriebe_pro_Jahr", "Trutenausmast", "Uebrige_Tiere",
    "Produzierende_Zibben_inkl_Jungtiere_bis_ca_35_Tage_alt", "Jungtiere_Mast_bzw_Aufzucht_Alter_ca_35_bis_100_Tage_5_Umtriebe_pro_Platz_und_Jahr",
    "Strausse_aelter_als_13_Monate", "Strausse_bis_13_Monate_alt")

  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), as.numeric(x)))
  }
}

#' Translate the rows in the ZA-BH-SpB-Table 340 to the Betriebszweigs-Namen.
#' @export
#' @author Daniel Hoop
#' @param x The number to be translated to enterprise name.
#' @param give.tab Return translation vector without translating anything.
#' @examples transl.spb.340row("30000")
transl.spb.340row <- function(x, give.tab=FALSE){

  if (FALSE) {
    # Import the data from MML: Copy the colmuns B, C and D from Tab 340 in MML. Then Perform this little script.
    t1 <- read.cb("no")
    t1 <- cbind(t1[, 1], paste.cols(t1[, -1]))
    t1[, 1] <- gsub("'", "", t1[, 1])
    t1[, 2] <- gsub("^_|_$", "", t1[, 2])
    apply(t1, 1, function (row) cat(row[1], ", \"", row[2], "\",\n", sep = ""))
  }

 t1 <- matrix(c(
   10000, "Rinder_u_Bueffel",
   11000, "Milchvieh",
   13000, "Mutterkuehe",
   15000, "Mastkaelber",
   16000, "Rindviehmast",
   17000, "Fremdes_Rindvieh",
   18000, "Uebriges_Rindvieh",
   19000, "Rindv_Kaelbermast_f_Dritte",
   20000, "Pferdegattung",
   21000, "Pferde",
   30000, "Schafe_Ziegen_andereRaufuVerz",
   33000, "Schafe",
   34000, "Ziegen",
   35000, "Andere_RaufuVerz",
   50000, "RaufuVerz_ohne_Detail",
   60000, "Schweine",
   61000, "Schweine_kombiniert",
   62000, "Schweinezucht",
   63000, "Schweinemast",
   64000, "Ferkelproduktion",
   69000, "Schweinemast_f_Dritte",
   70000, "Gefluegel",
   71000, "Konsumeier",
   72000, "Bruteier_u_Jungtiere",
   73000, "Mastpoulets",
   75000, "Truten",
   79000, "Gefluegelmast_f_Dritte",
   80000, "Uebrige_Tiere",
   81000, "Kaninchen",
   83000, "Uebriges_Geflügel",
   85000, "Bienenvoelker",
   88000, "Andere_Tiere"
 ), byrow = TRUE, ncol = 2)

 rn1 <- t1[, 2]
 t1 <- as.matrix(as.numeric(t1[, 1]))
 rownames(t1) <- rn1

  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), as.numeric(x)))
  }
}

#' Translate Merkmalsbezeichnungen between the Merkmalsliste and the Erhebungsbogen.
#' @export
#' @author Daniel Hoop
#' @examples transl.EB.MML("P430_0100_94000", reverse = FALSE)
transl.EB.MML <- function(x, reverse=FALSE, give.tab=FALSE) {
  # Diese Funktion uebersetzt die Merkmalsbezeichnungen zwischen Online-Erhebungsbogen und Merkmalsliste-ZA2015
  # if inverse=TRUE in andere Richtung: MML2015 -> EB

  uebers_tab <- .keyValueStore$getOrSet(
    "transl.EB.MML$uebers_tab",
    local({
      pfad_uebers <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/4/3/4285/MML/transcoding_fuer_R/transcoding_active.csv")
      return (read.table(pfad_uebers, sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!")))
    }))

  if(give.tab) return(uebers_tab)

  if(!reverse) col_in <- 1 else col_in <- 2
  if(!reverse) col_ou <- 2 else col_ou <- 1
  return(uebers_tab[match(x,uebers_tab[,col_in]),col_ou])
}

#' Translate the account number of the ZA-BH-Kontenrahmen from number to name.
#' @export
#' @author Daniel Hoop
#' @param x Numeric vector containing the account numbers.
#' @param lang The language, either `"de"` for german, or `"fr"` for french.
transl.accountName <- function (x, lang=c("de", "fr")) {

  # Set parameters
  lang <- match.arg(lang)
  cols_all <- c("Accountnumber", "Account_DE", "Account_FR")
  col_lang <- if (lang == "de") "Account_DE" else "Account_FR"
  x <- as.character(x)

  # Load the translation from account-number to account name
  xml <- .keyValueStore$getOrSet(
    "transl.accountName$xml",
    local({
      installFromCRAN("XML")
      path <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/ZA-BH_Kontenplan/za-bh-accountsystem.xml")
      if (!file.exists(path))
        stop (paste0("The file containing the ZA-BH acocuntsystem does not exist:\n", path))
      xml <- XML::xmlRoot(XML::xmlParse(path))
      return (XML::xmlToDataFrame(xml[[3]], stringsAsFactors=FALSE)[,cols_all,drop=FALSE])
      # xml <- char.cols.to.num(XML::xmlToDataFrame(xml[[4]], stringsAsFactors=FALSE)) # these are the cost units
    })
  )
  # Tranlsate and return
  res <- xml[match(x,xml[,1]), col_lang] #rownames(xml) <- xml[,1]
  if (length(res) != length(x))
    stop ("Einther less or more translations than entries in 'x' were found. An error happened.")
  return (res)
}

gsub.multi <- function(pattern, replacement, x, ...){
  # gsub implemented for pattern and replacement as vector arguments (recursive implementation).
  # Use: gsub.multiple(c("a","b","c"),c(1,2,3),letters)
  # Agruments: Please consult help page of gsub.
  if(length(replacement)==1) replacement <- rep(replacement, length(pattern))
  if(length(pattern)!=length(replacement)) stop("length(pattern) must be equal length(replacement).")

  if(length(pattern)>1) {
    newo <- order(nchar(pattern))
    pattern <- pattern[newo]
    replacement <- replacement[newo]
    return(gsub(pattern[1], replacement[1], gsub.multiple(pattern[-1], replacement[-1], x, ...), ...))
  } else {
    return(gsub(pattern, replacement, x, ...))
  }
}

gsub.multiple <- function(...){
  stop("gsub.multiple is now called gsub.multi! Please rename the function in script.")
}

####
# pattern="B20"; replace="BH20"; recursive=TRUE;
# gsub.only.dirs("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte/", pattern="BH20", replace="B20", recursive=TRUE)
gsub.only.dirs <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all directories and if recursive=TRUE all subdirectories of a given path.
  # pattern & replace shall be used as is used in the gsub() function.

  # Find all files
  allfiles <- list.files(path, full.names=TRUE, recursive=recursive)
  # Count maximal number of subdirectories by replacing all slashes and counting the difference.
  nsubdirs <- max(nchar(allfiles)-nchar(gsub("/", "", allfiles)))

  # Prepare vector to indicate if the renaming worked.
  worked.fil <- worked.log <- l.name1 <- NULL

  # Loop over all subdirectories.
  i <- 1
  for(i in nsubdirs:1){

    # Rename the highest subdirectory level, then go one step downwards and rename the next subdirectory level, again and again
    # until all directories are renamed.
    remove.downwards.dirs <- function(char){
      splitchar <- unlist(strsplit(char, "/"))
      until <- max( length(splitchar)-i , 1)
      return( paste(splitchar[ 1:until ], collapse="/") )
    }

    name1 <- unique( apply(matrix(allfiles),1,function(x)remove.downwards.dirs(x)) )
    # Shorten directoy vector such that only dirs are renamed that containt pattern.
    name1 <- name1[grepl(pattern, name1)]
    l.name1 <- c(l.name1, length(name1) )

    # Replace pattern.
    name2 <- gsub(pattern, replace, name1)

    # Rename directories and store information about wheter file.rename() was successful.
    worked.fil <- c(worked.fil, name1)
    worked.log <- c(worked.log, file.rename(name1, name2) )
  }

  # Display warning if pattern was not found in any directory and end function
  if(all(l.name1)==0) {
    warning("Pattern was not found. No directories renamed.", call.=FALSE, immediate.=TRUE)
  } else {

    # Look for files where it didn't work.
    worked.fil <- unique(worked.fil[!worked.log])
    # See if in any of them there was the pattern to search an replace
    # worked.fil <- worked.fil[grepl(pattern, worked.fil)]
    # If there is one, display a warning.
    if(length(worked.fil)>0) {
      warning("Not all directories could be renamed. See function output.", call.=FALSE)
      return(worked.fil)
    } else {
      message("All directories with occurencies successfully renamed.")
    }
  }
}

# gsub.only.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte/", pattern="B20", replace="BH20", recursive=TRUE)
gsub.only.files <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all files within a directory (and all files in subdirectories if recursive=TRUE).
  # pattern & replace shall be used as is used in the gsub() function.

  allfiles <- list.files(path, full.names=TRUE, recursive=recursive)
  name1 <- allfiles[ !file.info(allfiles)$isdir ]
  # Shorten file vector such that only dirs are renamed that containt pattern.
  name1 <- name1[grepl(pattern, name1)]

  # Display warning if pattern wasnt found and end function without doing anything.
  if(length(name1)==0) {
    warning("Pattern was not found. No files renamed.", call.=FALSE, immediate.=TRUE)
  } else {

    # Replace pattern.
    name2 <- gsub(pattern, replace, name1)

    # Prepare vector to indicate if the renaming worked & rename files.
    # Rename directories and store information about wheter file.rename() was successful.
    worked.fil <- name1
    worked.log <- file.rename(name1, name2)

    # Look for files where it didn't work.
    worked.fil <- unique(worked.fil[!worked.log])

    # If there is one, display a warning.
    if(length(worked.fil)>0) {
      warning("Not all files could be renamed. See function output.", call.=FALSE)
      return(worked.fil)
    } else {
      message("All files with occurencies successfully renamed.")
    }

  }
}
####

# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/", pattern="B20", replace="BH20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/", pattern="BH20", replace="B20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte", pattern="BH20", replace="B20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte", pattern="B20", replace="BH20", recursive=TRUE)

gsub.dirs.and.files <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all directories and all files within a directory (and all files in subdirectories if recursive=TRUE).
  # pattern & replace shall be used as is used in the gsub() function.
  gsub.only.dirs(path, pattern, replace, recursive)
  gsub.only.files(path, pattern, replace, recursive)
}

#
rename.files <- function(path="."){
  name1 <- list.files(agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/"), pattern="BH20", full.names=TRUE, recursive=TRUE)
  name2 <- gsub("BH20", "B20", name1)
  file.rename(from=name1, to=name2)

  file.rename(from=agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278//SekDaten/Betr_B/AWP/BH2014/"), to=agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278//SekDaten/Betr_B/AWP/B2014/"))

  file.rename(from=agsPath("./Betr_B/AWP/BH2014/001_AuswahlplanRef_Formel_Seite2.pdf"), to=agsPath("./Betr_B/AWP/B2014/001_AuswahlplanRef_Formel_Seite2.pdf"))
}
####


#x <- "Differenz = Privatbezuege fuer Privatkosten"
repl.utf8 <- function(x) {
  # This function recodes UTF8 (e.g. oe to ?) by using a translation matrix & gsub()

  pfad_utf8 <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/UTF8-ANSI/utf8-ansi.csv")
  if(!exists("utf8ansi", envir=globalenv())){
    # Uebersetzungstabelle utf8 & ANSI laden und vorbereiten
    utf8ansi <- as.matrix(read.table(pfad_utf8, sep=";", header=TRUE))
    # Leerzeichen entfernen.
    utf8ansi <- apply(utf8ansi,2,function(x)gsub(" ","",x))
    # Nach Anzahl Zeichen in UTF-8 Codierung sortieren, sonst gibt's bei gsub im Loop probleme
    utf8ansi <- utf8ansi[order(nchar(utf8ansi[,"utf8"]),decreasing=TRUE),]
    # Fuer Debugging hier x definieren.
    # x <- utf8ansi[,"utf8"]
    #? muss speziell codiert werden, damit es mit gsub funktioniert.
    utf8ansi <- apply(utf8ansi,2,function(x)gsub("\\?","\\\\\\?",x))
    # Diejenigen Codiereungen, die aus nur 1 Buchstaben bestehen, separat behandeln. Nicht verdichten
    utf8ansi_singlechar <- utf8ansi[nchar(utf8ansi[,3])==1,]
    utf8ansi <- utf8ansi[nchar(utf8ansi[,3])>1,]
    # Matrix verdichten, damit weniger Loops durchlaufen werden muessen
    utf8ansi.orig <- utf8ansi
    un1 <- sort(unique(utf8ansi[,"sign"]))
    utf8ansi <- utf8ansi[numeric(),c("sign","utf8")]

    for(i in 1:length(un1)){
      utf8ansi <- rbind(utf8ansi, c(un1[i], paste0(utf8ansi.orig[utf8ansi.orig[,"sign"]==un1[i],"utf8"],collapse="|")) )
    }
    utf8ansi <- rbind(utf8ansi,utf8ansi_singlechar[,c("sign","utf8")])
  }

  # Hier Uebersetzung
  for(i in 1:nrow(utf8ansi)){
    x <- gsub(utf8ansi[i,"utf8"],utf8ansi[i,"sign"], x)
  }
  # Ergebnis ausgeben
  #utf8ansi <<- rbind(utf8ansi,utf8ansi_singlechar[,c("sign","utf8")])
  return(x)
}

random.string <- function(n=1, length=12, capitals = NULL, numbers = NULL){
  # Create n random strings of specified number of characters (length), capital letters and numbers.
  if(n>1) return(replicate(n, random.string(n=1, length=length, capitals=capitals, numbers=numbers)))

  if (length(capitals) > 0 && length(numbers) > 0) {
    nlower <- length - capitals - numbers
    if(nlower < 0) stop("`capitals + numbers` must be smaller equal `length`.")

    if(nlower==0)   lower <- character() else  lower <- sample(letters,nlower,replace=TRUE)
    if(capitals==0) upper <- character() else  upper <- sample(LETTERS,capitals,replace=TRUE)
    if(numbers==0)  numbe <- character() else  numbe <- sample(0:9,numbers,replace=TRUE)
    return( paste(sample(c(lower,upper,numbe), length), collapse="") )
  }
  if (sum(length(capitals) > 0, length(numbers) > 0) == 1)
    stop ("Either specify `capitals` and `numbers` or non of both.")

  return ( paste(sample(c(letters,LETTERS,0:9), length, replace=TRUE), collapse="") )
}

set.args <- function(string){
  # This function reads a string of arguments looking like:
  # y=y, trt=grouping, sig=sig, sig.level=sig.level ,digits=digits, median.mean="mean", ranked=ranked, print.result=FALSE
  # and sets the variables accordingly in the global environment. For function debugging.

  string <- gsub("\\=","<<-",string)
  string <- gsub(",",";",string)
  eval(parse(text=string))
}

#hms_to_sec(c("22:06:02","22:06:02"))
#sec_to_hms(hms_to_sec(c("22:06:02","22:06:02")))
hms.to.sec <- function(hms){
  # Convert time format hh:mm:ss to seconds
  return(as.numeric(substr(hms,1,2))*3600 +
           as.numeric(substr(hms,4,5))*60 +
           as.numeric(substr(hms,7,nchar(hms))))
}

#sec.to.hms(c(119000.5,4,3,2,2342234.4),1)
sec.to.hms <- function(sec, digits=0){
  # Convert seconds to time format hh:mm:ss (Digits are possible for seconds)
  h <- floor( sec / 3600 )
  m <- floor( sec%%3600 / 60 )
  s <- as.matrix(round( sec%%60 , digits))

  ind <- nchar(h )==1; h[ind] <- paste0("0",h[ind])
  ind <- nchar(m )==1; m[ind] <- paste0("0",m[ind])
  s1 <- sapply(strsplit(as.character(s),"\\."),function(x)x[[1]])
  ind <- nchar(s1)==1; s[ind] <- paste0("0",s[ind])

  return(paste(h,m,s,sep=":"))
}

covar <- coef.of.variance <- function(x,na.rm=TRUE) {
  sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
}

####
find.deriv.0 <- function(expr, dat=c(-1e20, 1e20), variablename="x") {
  # This function derivates a given expression in form of a string (with 1 unknown variable x) and
  # returns the point where the slope equals zero.

  # If values of variables are passed into the function just use paste0()
  # coefs <- c(2, -0.5)
  # expr <- paste0(coefs[1],"*x + ", coefs[2], "*x^2")
  # find.deriv.0(expr)
  # curve(coefs[1]*x +  coefs[2]*x^2, 0, 5)
  # abline(v=find.deriv.0(expr))

  # dat can be given (optional), in order to determin the interval where the solution is searched.

  d_expr <- D( parse(text=expr) , name=variablename)
  f <- function(x)  eval(d_expr)
  res <- uniroot(f, c(min(dat), max(dat)) )$root
  names(res) <- "deriv0"
  return(res)
}

if(FALSE){
  undebug(find.deriv.0)

  # Beide Varianten funktionieren:
  expr <- "1.0338e-01*x - 1.0941e-02*x^2"
  find.deriv.0(expr)
  expr <- expression( 1.0338e-01*x - 1.0941e-02*x^2 )
  find.deriv.0(expr)
  # Diese nicht:
  find.deriv.0( 1.0338e-01*x - 1.0941e-02*x^2 )
}

####
deriv.value <- function(expr, at, variablename="x", times=1) {
  # This function calculates the value of the derivative of a given expression (expr)
  # at a given point "at"
  # expression must be a String!
  # times = number of derivations

  # Falls times <= 0, wird nichts gemacht. Dann muesse man die Funktion eigentlich auch gar nicht verwenden...
  if(times<=0) {
    d_expr <-  parse(text=expr)

    # Sonst wird abgeleitet
  } else {
    d_expr <- D( parse(text=expr) , name=variablename)
    if(times>1) {
      for(i in 1:(times-1)) {
        d_expr <- D( d_expr, name=variablename)
      }
    }
  }
  assign( variablename,  at)

  return( eval(d_expr) )
}
if(FALSE){
  expr <- "x+x^2"
  deriv.value(expr, at=3, times=2)
  expr <- "x+x^2+x^3"
  deriv.value(expr, at=10, times=2)
}

# x <- c(100,101,102,103); y <- c(0,1,11,20); xout <- c(99, 104); extrapolate <- TRUE; extrapolMethod <- c("lastPoint","linRegr")[2]
# approx.own(x, y, xout, extrapolate, extrapolMethod)
approx.own <- function(x, y, xout, extrapolate=FALSE, extrapolMethod=c("lastPoint","linRegr")) {

  #if (FALSE && extrapolate && extrapolMethod=="linRegr" && length(xout)>1)
  #  return(sapply(xout, function(xout1) approx.own(x = x, y = y, xout = xout1, extrapolate = extrapolate, extrapolMethod = extrapolMethod)))

  if (length(xout) == 0)
    return(xout)

  extrapolMethod <- match.arg(extrapolMethod)
  if (length(x)!=length(y))
    stop("length(x) must be equal to length(y).")

  isnaxy <- is.na(x) | is.na(y)
  if (any(isnaxy)){
    if (all(isnaxy))
      return(rep(NA, length(xout)))
    x <- x[!isnaxy]
    y <- y[!isnaxy]
  }
  if (any(duplicated(x)))
    stop("There must be no duplicated values in x.")
  if (length(x)==1)
    return(y)
  naF <- function(x){
    x[is.na(x)] <- FALSE
    return(x)
  }

  res <- approx(x=x,y=y,xout=xout)$y
  rng <- range(x)
  if (!extrapolate) {
    res[ xout<rng[1] ] <- y[which.min(x)]
    res[ xout>rng[2] ] <- y[which.max(x)]
  } else {
    sma <- naF(xout<rng[1]);
    big <- naF(xout>rng[2]);
    smaFlag <- any(sma)
    bigFlag <- any(big)
    if (extrapolMethod=="lastPoint") {
      if(smaFlag || bigFlag){
        newo <- order(x)
        x <- x[newo]
        y <- y[newo]
        j <- length(x)
      }
      if (smaFlag)
        res[sma] <- y[1] + (x[1]-x[2])*(y[2]-y[1]) * (x[1]-xout[sma])
      if (bigFlag)
        res[big] <- y[j] + (x[j]-x[j-1])*(y[j-1]-y[j]) * (x[j]-xout[big])
    } else if (extrapolMethod=="linRegr") {
      smabig <- sma | big
      mod <- lm(y~x)
      res[smabig] <- predict(mod, newdata=data.frame(x=xout[smabig], y=0))
    }
  }

  return(res)
}

lpsolve <- function(obj_coef, A, LHS_ge=NULL, LHS_le=NULL, opt_val_ge=NULL, opt_val_le=NULL,  maximize=FALSE) {
  # This function transforms a given optimization model in a not very intuitive and rather special form and solves with the
  # functions initProbCLP(), setObjDirCLP(), loadProblemCLP() and solveInitialCLP() of the package clpAPI.
  # Keywords: Linear Programming, LP

  # Arguments:
  # obj_coef = coefficients of the objective function

  # A = matrix containing the restriction coefficients
  #       nrow(A) = no. of restrictions
  #       ncol(A) = no. of coefficients

  # LHS_ge = "Left hand side greater equal ..."
  #       vector of values of the Right Hand Side that should be exceeded
  # LHS_le = "Left hand side lower equal ..."
  #       vector of values of the Left Hand Side that should NOT be exceeded
  # One of LHS_ge or LHS_le must be given!

  # opt_val_ge = "optimal value greater equal"
  #       value of objective function that should be exceeded
  # opt_val_le = "optimal value lower equal"
  #       value of objective function that should NOT be exceeded
  # opt_val_ge and opt_val_le are optional arguments!

  # maximize = TRUE  -> the objective function is maximized
  # maximize = FALSE -> the objective function is minimized

  if(!is.null(dim(obj_coef))) obj_coef <- as.vector(obj_coef)
  if(ncol(A)!=length(obj_coef)) stop("ncol(A) != length(obj_coef)")
  is.null.LHS_ge <- is.null(LHS_ge)
  is.null.LHS_le <- is.null(LHS_le)
  is.null.opt_val_ge <- is.null(opt_val_ge)
  is.null.opt_val_le <- is.null(opt_val_le)
  if(is.null.LHS_ge & is.null.LHS_le) stop("Specify either LHS_ge (left hand side >= b) or LHS_le (left hand side <= b)")
  if(!is.null.LHS_ge){
    if(!is.null(dim(LHS_ge))) LHS_ge <- as.vetor(LHS_ge)
    if(length(LHS_ge)!=nrow(A)) stop("length(LHS_ge) != nrow(A)")
  }
  if(!is.null.LHS_le) {
    if(!is.null(dim(LHS_le))) LHS_le <- as.vetor(LHS_le)
    if(length(LHS_le)!=nrow(A)) stop("length(LHS_le) != nrow(A)")
  }
  if(!is.null.opt_val_ge) {
    if(length(opt_val_ge)==1) opt_val_ge <- rep(opt_val_ge, ncol(A))
    if(length(opt_val_ge)!=ncol(A)) stop("length(opt_val_ge) != ncol(A)")
  }
  if(!is.null.opt_val_le) {
    if(length(opt_val_le)==1) opt_val_le <- rep(opt_val_le, ncol(A))
    if(length(opt_val_le)!=ncol(A)) stop("length(opt_val_le) != ncol(A)")
  }

  require(clpAPI)
  if(maximize) minmax <- (-1) else minmax <- 1
  lp <- initProbCLP() # Create LP object
  setObjDirCLP(lp, minmax) # 1 for minimization. -1 for maximization.
  nrows  <- nrow(A); ncols  <- ncol(A)
  rlower <- LHS_ge      # Lower row bound. Use for >= restriction on LHS
  rupper <- LHS_le      # Upper row bound. Use for <= restriction on LHS
  clower <- opt_val_ge  # Upper column bound. Use for >= restriction on optimization coefficients.
  cupper <- opt_val_le  # Lower column bound. Use for <= restriction on optimization coefficients.

  # constraint matrix (left hand side). Convert matrix into vector. Give row and column indices for every vector place.
  ia <- c(row(A))-1 # Has to start with row 0, not row1. Otherwise you will get an error!
  ja <- seq(0,length(ia)+nrow(A),nrow(A))
  ar <- c(A) # Convert A matrix to a vector.
  if(FALSE){
    nc <- c( apply(A,2,function(x){result<-rep(FALSE,length(x)); if(any(xn0<-x!=0)) result[min(which(xn0))] <- TRUE; return(result)}) )
    ar0 <- ar==0
    ar <- ar[!ar0]
    ia <- ia[!ar0]
    nc <- nc[!ar0]
    nc <- which(nc)-1; nc <- c(nc,length(ar))
    ja <- nc
  }

  # load problem data
  loadProblemCLP(lp=lp, ncols=ncols, nrows=nrows, ia=ia, ja=ja, ra=ar, lb=clower, ub=cupper, obj_coef=obj_coef, rlb=rlower, rub=rupper)
  solveInitialCLP(lp) # Solve the LP.

  result <- list()
  result$lp <- lp
  result$opt_val <- getColPrimCLP(lp)
  result$opt_sol <- getObjValCLP(lp)
  result$info <-  paste("getSolStatusCLP(lp)   # Retrieve solve status of LP",
                        "getObjValCLP(lp)      # Retrieve optimal (minimal/maximal) value of objective function.",
                        "getColPrimCLP(lp)     # Retrieve the primal values of the structural variables (columns) after optimization.",
                        "getColDualCLP(lp)     # Retrieve the dual values of the structural variables (columns) after optimization (reduced costs).",
                        "delProbCLP(lp)        # remove problem object",
                        sep="\n")

  # lp am Schluss loeschen. Wenn man mehrere Optimierungen nacheinander ausfuehrt, kommt es sonst zu Fehlern im solver.
  delProbCLP(lp)
  return(result)
}



#mclapply.own(1:100, function(x)return(x*2), type="PSOCK")
#mclapply.own(1:100, function(x)return(x*y+1), type="PSOCK")
#type <- "PSOCK"; mc.cores=8; X <- as.list(1:100); values <- X; FUN <- function(x)return(x*2)
if (FALSE) mclapply.own <- function(X, FUN, mc.cores=parallel::detectCores(), type=c("PSOCK", "FORK", "MPI")){
  # This function does multi core processing of lapply (parLapply)
  # The function depends on following packages
  # parallel, snow, Rmpi
  # mc.cores = getOption("mc.cores", 8)
  if(!grepl("return",paste0(deparse(FUN),collapse="")))
    stop(paste0("FUN must explicitly return the result by using return(). The entered function looks like this\n",
                "    ",paste0(deparse(FUN),collapse=""), "\n",
                "  But it should look like this:\n",
                "    function(x)return(x*2)"))

  type <- match.arg(type)

  require.package("parallel")
  cl <- makeCluster(min(length(X), mc.cores), type=type)

  #res <- tryCatch({
  #  parLapply(cl, X=X, fun=FUN)
  #}, finally = {
  #  stopCluster(cl)
  #  if(type=="MPI") mpi.exit()
  #})

  res <- parLapply(cl, X=X, fun=FUN)
  stopCluster(cl)
  if(type=="MPI") mpi.exit()
  return(res)
}

## Define the hack
if (FALSE) mclapply.hack <- function(...) {
  ## A script to implement a hackish version of
  ## parallel:mclapply() on Windows machines.
  ## On Linux or Mac, the script has no effect
  ## beyond loading the parallel library.

  # http://www.stat.cmu.edu/~nmv/2014/07/14/implementing-mclapply-on-windows
  # http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R
  require.package("parallel")

  ## Create a cluster
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )

  ## Find out the names of the loaded packages
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))

  tryCatch( {

    ## Copy over all of the objects within scope to
    ## all clusters.
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())

    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require.package(yy , character.only=TRUE)})
    })

    ## Run the lapply in parallel
    return( parLapply( cl, ...) )
  }, finally = {
    ## Stop the cluster
    stopCluster(cl)
  })
}

## If the OS is Windows, set mclapply to the
## the hackish version. Otherwise, leave the
## definition alone.
if(FALSE) mclapply <- switch( Sys.info()[['sysname']],
                              Windows = {mclapply.hack},
                              Linux   = {mclapply},
                              Darwin  = {mclapply})

## end mclapply.hack.R



#### Geo Data - Swisstopo & Google Maps ####
# Dealing with Swisstopo coordinates in the AGIS-Data

#' Convert from internation geographic system (WGS84; N, E) to Swiss geographic system (CH1903; y, x)
#' @param N A vector c(degree, seconds, minutes) North or a decimal number of length 1 (giving only the degrees).
#' @param E A vector c(degree, seconds, minutes) East or a decimal number of length 1 (giving only the degrees).
#' @return The coordinates (x, y) in the Swiss geographic system CH1903.
#' @examples
#' N <- c(46,  2, 38.87)
#' E <- c( 8, 43, 49.79)
#' wgs84.to.ch1903(N, E)
#' c(x = 1e+05, y = 7e+05)
wgs84.to.ch1903 <- function(N, E){
  if(length(N)==1){   x0 <- N
  } else {            x0 <- N[1]*3600 + N[2]*60  + N[3]   }
  x1 <- (x0 - 169028.66) / 10000

  if(length(E)==1){   y0 <- E
  } else {            y0 <- E[1]*3600 + E[2]*60  + E[3]   }
  y1 <- (y0 - 26782.5) / 10000

  x <- 200147.07 + 308807.95 * x1 + 3745.25 * y1^2 + 76.63 * x1^2 + 119.79 * x1^3 - 194.56 * y1^2 * x1  # x0; x1;  x
  y <- 600072.37 + 211455.93 * y1 - 10938.51 * y1 * x1 - 0.36 * y1 * x1^2 - 44.54 * y1^3  # y0;  y1;  y

  x <- round(x)
  y <- round(y)

  res <- c(x=x, y=y)
  return(res)
}

#' Convert from Swiss geographic system (CH1903; y, x) to internation geographic system (WGS84; N, E)
#' @param x The x coordinates
#' @param y The y coordinates
#' @param output A character value describing the output format. Eiter \code{"decimal"} (decimal value of length 1) or \code{"minsec"} (vector of length 3).
#' @return The coordinates (N, E) in the internation geographic system (WGS84).
#' @examples
#' ch1903.to.wgs84(x = 100000, y = 700000, output = "minsec")
#' # N = 46° 02' 38.86"
#' # E =  8° 43' 49.80"
#' @details Note that the coordinates METER_X and METER_Y in the AGIS data are probably interchanged.
ch1903.to.wgs84 <- function(x, y, output=c("decimal", "minsec")) {

  output <- match.arg(output)

  y0 <- (y-600000)/1000000
  x0 <- (x-200000)/1000000

  y1 <-  2.6779094 + 4.728982 * y0 + 0.791484 * y0 * x0 + 0.1306 * y0 * x0^2 - 0.0436 * y0^3
  x1 <- 16.9023892 + 3.238272 * x0 - 0.270978 * y0^2 - 0.002528 * x0^2 - 0.0447 * y0^2 * x0 - 0.0140 * x0^3

  E <- y1*100/36
  N <- x1*100/36

  if(output=="decimal") {
    res <- list()
    res$coord <- c(N=N, E=E)
    res$full <- paste0(round(N, 10), " N,   ", round(E, 10), " E")
    return(res)
  }

  Eg <- floor(E)
  Em <- floor( (E-Eg) * 60 )
  Es <- round( ( (E-Eg) * 60 - Em ) * 60  ,2)

  Ng <- floor(N)
  Nm <- floor( (N-Ng) * 60 )
  Ns <- round( ( (N-Ng) * 60 - Nm ) * 60  ,2)

  res <- list()
  res$N <- c(deg=Ng, min=Nm, sec=Ns)
  res$E <- c(deg=Eg, min=Em, sec=Es)
  res$full <- paste0(
    Ng, "°", Nm, "'", Ns, "'' N,   ",
    Eg, "°", Em, "'", Es, "'' E"
  )
  return(res)
}

# Veraltet und primitiv! Funktion eins weiter unten verwenden!
googlemaps.url <- function(coord, zoom=8, browse=FALSE){
  # Create googlemaps URL and open it in broswer if browse=FALSE
  # zoom is not implementet at the moment.

  url <- paste0("http://maps.google.com/?q=",coord[1],",",coord[2]) #, ",", zoom, "z")
  if(!browse)  return(url)
  browseURL(url)
}

# Frauenfeld auf googlemaps:
# googlemaps_url( ch1903_to_wgs84(709726, 268273)$coord , browse=TRUE)

if(FALSE){
  # N <- gb2[1:10,"N_KOORD"]
  # E <- gb2[1:10,"E_KOORD"]
  # create.googlemaps.markers(N=gb2[,"N_KOORD"], E=gb2[,"E_KOORD"], labels=gb2[,"ArbVerd_jeFJAE"])
  n <- 1000
  N <- rnorm(n, 47.07343  ,0.2); E <- rnorm(n, 7.68218  ,0.5);
  #N <- 47.06; E <- 8.00
  labels="";
  N.center=NULL; E.center=NULL
  zoom=9; map.type=c("HYBRID","ROADMAP","SATELLITE","TERRAIN"); angle=0;
  point.symbol <- c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW")[1]; point.size=2.5; point.lwd=0.5; point.col.in="red"; point.col.out="black"; point.opacity=0.7; del.tmp.file=TRUE

  create.googlemaps.markers(N, E, map.type="HYBRID", point.opacity=1, #point.symbol=sample(c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW"), length(N), replace=TRUE),
                            point.size= 2+2*scale.extreme(1:n), point.col.in=color.gradient(1:n), cat.html=FALSE , del.tmp.file=TRUE)
}

create.googlemaps.markers <- function(N, E, labels="", N.center=NULL, E.center=NULL, zoom=9, map.type=c("HYBRID","ROADMAP","SATELLITE","TERRAIN"), angle=0,
                                      point.symbol=c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW")[1], point.size=2.5, point.lwd=0.5, point.col.in="red", point.col.out="black", point.opacity=0.7, del.tmp.file=TRUE, cat.html=FALSE){
  # N = N coords of points
  # N = E coords of points
  # labels = optional labels of points
  # N.center = N coords of map center
  # E.center = E coords of map center
  #
  # Choose
  # N.center = 47.06, E.center=8.00
  # For the approximated middle of Switzerland.
  #
  # zoom = zoom factor
  # and other options to costumize the map. Just try.
  # Hint: use point.size   = 2+2*scale.extreme(...) and
  #           point.col.in = color.gradient(...)   for nice results.


  if(is.null(N.center)) N.center <- (min(N, na.rm=TRUE) + max(N, na.rm=TRUE))/2
  if(is.null(E.center)) E.center <- (min(E, na.rm=TRUE) + max(E, na.rm=TRUE))/2

  map.type <- match.arg(map.type)
  if(zoom<18 & angle!=0) warning("angle!=0 only works if zoom>18. Even then it's not available for all regions of the world.", immediate.=TRUE)

  point.symbol.choice <- c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW")
  if(any(!point.symbol%in%point.symbol.choice)) stop(paste0("point.symbol must be one of ", paste(point.symbol.choice, collapse=", ")))

  LN <- length(N)
  stopifnot( length(labels)==1 | length(labels)==LN )
  stopifnot( length(point.symbol)==1 | length(point.symbol)==LN )
  stopifnot( length(point.size)==1 | length(point.size)==LN )
  stopifnot( length(point.lwd)==1 | length(point.lwd)==LN )
  stopifnot( length(point.col.in)==1 | length(point.col.in)==LN )
  stopifnot( length(point.col.out)==1 | length(point.col.out)==LN )
  stopifnot( length(point.opacity)==1 | length(point.opacity)==LN )
  # Pferformance Verlgeich:
  #system.time(for(i in 1:1000000) LN)
  #system.time(for(i in 1:1000000) length(N))

  variable.arguments <- c("labels","point.symbol","point.size","point.lwd","point.col.in","point.col.out","point.opacity")
  length1 <- logical(length=length(variable.arguments)); names(length1) <- variable.arguments
  for(i in 1:length(variable.arguments)){
    length1[i] <- length(get(variable.arguments[i]))==1
  }
  char.vars <- c("labels","point.col.in","point.col.out")

  point.symbol <- paste0("google.maps.SymbolPath.", point.symbol)

  #### Erst wird das Skript erstellt

  text <- paste0(
    "
    <!--
    Herkunft des Beispiels:
    https://developers.google.com/maps/documentation/javascript/examples/icon-complex
    Alles was mit
    shape: shape,
    zu tun hat, einfach aus dem Code l?schen.
    -->

    <!DOCTYPE html>
    <html>
    <head>
    <meta name='viewport' content='initial-scale=1.0, user-scalable=no'>
    <meta charset='utf-8'>
    <title>Koordinaten Uebersicht</title>
    <style>
    html, body, #map-canvas {
    height: 100%;
    margin: 0px;
    padding: 0px
}
</style>
<script src='https://maps.googleapis.com/maps/api/js?v=3.exp'></script>
<script>

function initialize() {
var mapOptions = {
zoom: ",
zoom, ",\n",
"center: new google.maps.LatLng(",N.center,", ", E.center, "),\n",
"mapTypeId: google.maps.MapTypeId.", map.type, "\n",
"}
var map = new google.maps.Map(document.getElementById('map-canvas'),  mapOptions);
map.setTilt(", angle ,");

setMarkers(map, locationProperties);
}
")
  # Debugging
  # cat(text)

  # Nun werden die Koordinaten der Punkte ins Skript eingefuegt:
  orders <- 1:length(N)
  # Matrix erzeugen, die alle Kennzahlen enth?lt
  info <- cbind(orders, N, E, labels, point.symbol, point.size, point.lwd, point.col.in, point.col.out, point.opacity)
  # NAs entfernen
  info <- info[!is.na(info[,"N"]),,drop=FALSE]
  # Fehler ausgeben und matrix printen, wenn es noch andere NAs drin hat:
  errors <- apply(info,1,function(x)any(x%in%c("NA", "NaN")))
  if(any(errors)){
    print(info[errors,], quote=FALSE)
    warning("There are NA values in one or more of the arguments. Check the above printed matrix or invisible function output.")
    return(invisible(info[errors,]))
  }
  # Alle String arguments fuer JavaScript mit '' versehen
  info[,char.vars] <- paste0("'", info[,char.vars],"'")
  # Alle Spalten entfernen, die immer dieselben sind.
  info1 <- info[,!colnames(info)%in%names(length1)[length1] ,drop=FALSE]

  js.matrix <- function(x){
    mat <- apply(x,1,function(x)paste("[", paste(x,collapse=", "), "],"))
    mat <- paste0(mat, collapse="\n")
    mat <- substr(mat, 1, nchar(mat)-1)
    mat <- paste0("[\n",mat,"\n];\n")
    return(mat)
  }

  text2 <- paste0( text, "\nvar locationProperties = ", js.matrix(info1))
  # Debugging
  # cat(text2)

  # Rest des Sktips schreiben (inkl. Aussehen der Punkte.)
  text4 <- paste0(
    text2,
    "
    function setMarkers(map, locations) {
    // Add markers to the map

    // Marker sizes are expressed as a Size of X,Y
    // where the origin of the image (0,0) is located
    // in the top left of the image.

    for (var i = 0; i < locations.length; i++) {
    var locationProperty = locations[i];
    var myLatLng = new google.maps.LatLng(locationProperty[1], locationProperty[2]);

    var image = {
    path: ",if(length1["point.symbol"]) info[1,"point.symbol"] else paste0("locationProperty[",which(colnames(info1)%in%"point.symbol")-1,"]"), ",
    scale: ",if(length1["point.size"]) info[1,"point.size"] else paste0("locationProperty[",which(colnames(info1)%in%"point.size")-1,"]"), ",
    strokeWeight: ",if(length1["point.lwd"]) info[1,"point.lwd"] else paste0("locationProperty[",which(colnames(info1)%in%"point.lwd")-1,"]"), ",
    fillColor: ",if(length1["point.col.in"]) info[1,"point.col.in"] else paste0("locationProperty[",which(colnames(info1)%in%"point.col.in")-1,"]"), ",
    strokeColor: ",if(length1["point.col.out"]) info[1,"point.col.out"] else paste0("locationProperty[",which(colnames(info1)%in%"point.col.out")-1,"]"), ",
    fillOpacity: ",if(length1["point.opacity"]) info[1,"point.opacity"] else paste0("locationProperty[",which(colnames(info1)%in%"point.opacity")-1,"]"), "
    };

    var marker = new google.maps.Marker({
    position: myLatLng,
    map: map,
    icon: image,
    title: ",if(length1["labels"]) info[1,"labels"] else paste0("locationProperty[",which(colnames(info1)%in%"labels")-1,"]"), ",
    zIndex: locationProperty[0]
    });
    }
    }

    google.maps.event.addDomListener(window, 'load', initialize);

    </script>
    </head>
    <body>
    <div id='map-canvas'></div>
    </body>
    </html>
    "
  )
  # Debugging:
  if(cat.html) cat(text4)



  #### Skript ist fertig

  # Tempor?re Datei erstellen
  write.table(text4, paste0(tempdir(),"/googlemaps_tmp.html"), col.names=FALSE, row.names=FALSE, quote=FALSE)
  # ?ffnen
  browseURL(paste0(tempdir(),"/googlemaps_tmp.html"))

  if(!del.tmp.file){ # length(unique(labels))>1 & labels[1]!="" |
    cat("File is stored in the folder:   ", tempdir(),"\\\n", sep="")
    cat("                    filename:   googlemaps_tmp.html\n" )
    cat("Use\nfile.remove(paste0(Sys.getenv('TMP'),'\\\\googlemaps_tmp.html'))\nto delete the file when you don't need it anymore\n")

    # Wenn keine Labels gesetzt wurden, macht es keinen Sinn, die Datei l?nger zu behalten.
    # Es wird 8 Sekunden gewartet, bis der Browser die Datei sicher ge?ffnet hat. Dann wird die tempor?re Datei wieder gel?scht.
  } else {
    Sys.sleep(8)
    if( file.remove(paste0(Sys.getenv('TMP'),'\\googlemaps_tmp.html')) ) cat("File was removed. If you want to use it, set argument del.tmp.file=TRUE")
  }
  }

if(FALSE){
  N <- 47.07343; E <- 7.68218; language="de"
  get.googlemaps.adress(N=47.07343, E=7.68218)
  get.googlemaps.adress(N=47.07343, E=7.68218, result="splitted")
}

get.googlemaps.adress <- function(N, E, result=c("full", "splitted"), language="de"){
  # This function finds the approximate adress of a coordinate.
  adr <- unlist( read.table( paste0("http://maps.googleapis.com/maps/api/geocode/json?latlng=", N, "," ,E,"&language=",language), sep=";", stringsAsFactors=FALSE) )

  result <- match.arg(result)
  extract.adr <- function(x)  return(unname(substr(x, 2+gregexpr(":", x)[[1]][1], nchar(x)-1)))

  if(result=="full"){
    return(   extract.adr( adr[grep("formatted_address",adr)[1]] )   )
  } else {
    res <- character()
    res["country"] <- extract.adr( adr[grep("country",adr)[1]-2] )
    res["admin_area_lvl1"] <-  extract.adr( adr[grep("administrative_area_level_1",adr)[1]-2] )
    res["admin_area_lvl2"] <-  extract.adr( adr[grep("administrative_area_level_2",adr)[1]-2] )
    res["postal_code"] <- extract.adr( adr[grep("postal_code",adr)[1]-2] )
    res["locality"] <-  extract.adr( adr[grep("locality",adr)[1]-2] )
    res["route"] <-  extract.adr( adr[grep("route",adr)[1]-2] )
    res["street_number"] <-  extract.adr( adr[grep("street_number",adr)[1]-2] )
    return(res)
  }
}

if(FALSE){
  N <- 47; E <- 8
  get.googlemaps.elevation(N=N, E=E)
}
get.googlemaps.elevation <- function(N, E){
  adr <- unlist( read.table( paste0("http://maps.googleapis.com/maps/api/elevation/json?locations=", N, "," ,E), sep=";", stringsAsFactors=FALSE) )
  extract.adr <- function(x)  return(unname(substr(x, 2+gregexpr(":", x)[[1]][1], nchar(x)-1)))
  return( round(as.numeric(extract.adr( adr[grep("elevation",adr)[1]] ))) )
}

if(FALSE){
  N <- 47.07343; E <- 7.68218
  N <- rep(N, 2); E <- rep(E, 2)
  get.googlemaps.slopes(N=47.07343, E=7.68218)
}
get.googlemaps.slopes <- function(N, E){
  # In dieser Funktion mehrere Positionen in der Naehe des Orts abfragen (gleichzeitig)
  # Dann die absolute Hoehendifferenz zwischen dem Mittelpunkt und allen aeusseren Punkten aufsummieren.
  # z.B. Kreisform, oder Sternform mit bestimmem Radius.

  # google search:   polygonal city boundaries
  #                  worldwide polygonal city boundaries
  #                  gis database locality boundaries
  # http://wiki.openstreetmap.org/wiki/AT/Gemeinden
  # http://www.openstreetmap.org/relation/16239
  # http://www.gadm.org/                             GADM database of Global Administrative Areas
  # http://www.naturalearthdata.com/
  # SRTM-Daten ( http://de.wikipedia.org/wiki/SRTM-Daten )
  # GRASS GIS - http://grass.osgeo.org/
  # Using GRASS with R - http://grasswiki.osgeo.org/wiki/R_statistics

  # http://stackoverflow.com/questions/8135243/finding-towns-within-a-10-mile-radius-of-postcode-google-maps-api
  # http://www.mullie.eu/geographic-searches/
  #  // convert latitude/longitude degrees for both coordinates
  #  // to radians: radian = degree * pi / 180
  #  $lat1 = deg2rad($lat1);
  #  $lng1 = deg2rad($lng1);
  #  $lat2 = deg2rad($lat2);
  #  $lng2 = deg2rad($lng2);
  #
  #  // calculate great-circle distance
  #  $distance = acos(
  #    sin($lat1) * sin($lat2) +
  #      cos($lat1) * cos($lat2) *
  #      cos($lng1 - $lng2)
  #  );
  #
  #  // distance in human-readable format:
  #    // earth's radius in km = ~6371
  # $distance = 6371 * $distance;


  # Runden um Zeichen zu sparen
  N <- round(N, 5)
  E <- round(E, 5)
  # Erst Adresse zusammenstellen. Darf maximal 2048 Zeichen haben. mit nchar pruefen.
  coord <- paste0(N, ",", E)
  coord <- paste(coord, collapse="|")

  # URL Vorlage:
  # http://maps.googleapis.com/maps/api/elevation/json?locations=39.7391536,-104.9847034|36.455556,-116.866667

  url <- paste0("http://maps.googleapis.com/maps/api/elevation/json?locations=", coord)
  if(nchar(url)>2048) {
    print(url, quote=FALSE)
    stop("URL darf maximal 2048 Zeilen lang sein.")
  }

  adr <- unlist( read.table( url , sep=";", stringsAsFactors=FALSE) )
  # extract.adr <- function(x)  return(unname(substr(x, 2+gregexpr(":", x)[[1]][1], nchar(x)-1)))
}


#### OUTLIER DETECION ####
#x <- c(1,2,3,6,5,7,4,2,4,5,6,7,22); p <- 0.05
#remove.outliers(x,p)
normal.outliers <- function(x, p=0.01) {
  # The outliers of a normal distribution are displayed (TRUE, FALSE).
  pvals <- pnorm(x,mean(x,na.rm=TRUE),sd(x,na.rm=TRUE))
  outliers <- pvals < p/2   |   pvals > 1-p/2
  return(outliers)
}
quantile.outliers <- function(x, p=0.01) {
  # The outliers that are defined by the given quantile (p/2) are displayed.
  outliers <- x < quantile(x,p/2)   |   x > quantile(x,1-p/2)
  return(outliers)
}
sd.outliers <- function(x, no.sd=3){
  # The outliers that are above/below  mean(x)+sd(x) / mean(x)-sd(x)  are displayed
  mean.x <- mean(x,na.rm=TRUE)
  sd.x <- sd(x,na.rm=TRUE)
  outliers <- x < mean.x-sd.x | x>mean.x+sd.x
  return(outliers)
}
#which(normal.outliers(1:1000))
#which(quantile.outliers(1:1000))
#which(sd.outliers(1:1000))

if(FALSE) { data=prepareMvOutlData(cost[cost[,"Leist_Saat_Fl"]==0 & index==bz,checkCols]); p.val=0.025; method=c("absMahaDistIncrease","quantile","chisq")[1]; max.p.outl=0.15; na.action=c("median","mean","remove")[1]; make.plot=TRUE }
mahalanobis.outliers <- function(data, p.val=0.025, method=c("absMahaDistIncrease","quantile","chisq"), max.p.outl=0.15,
                                 na.action=c("median","mean","remove"), make.plot=FALSE, plotText=NULL){
  # Define multivariate outliers by mahalanobis distance
  method <- match.arg(method)
  if(is.data.frame(data)) data <- as.matrix(data)
  na.action <- match.arg(na.action)
  is.na.data <- is.na(data)
  if(any(is.na.data)){
    if(na.action=="remove"){
      data <- na.omit(data)
    } else {
      fillnas <- function(x){
        if (na.action=="mean") mean.x <- mean(x,na.rm=TRUE)
        if (na.action=="median") mean.x <- median(x,na.rm=TRUE)
        x[is.na(x)] <- mean.x
        return(x)
      }
      data <- apply(data,2,function(x)fillnas(x))
    }
  }
  # Prepare legend for plot
  if(make.plot){
    legMat <- as.data.frame(matrix(NA, nrow=5, ncol=4)); colnames(legMat) <- c("legend","lty","pch","col")
    legMat[,"legend"] <- c("normal","outlier","regrCrit","diffCrit","plotText")
    legMat[,"lty"] <- c(0,0,2,1,0)
    legMat[,"pch"] <- c(21,20,NA,NA,NA)
    legMat[,"col"] <- c("black","black","red","black",NA)
    if(method!="absMahaDistIncrease") legMat <- legMat[-c(3,4),]
    if(!is.null(plotText)) legMat["5","legend"] <- plotText else legMat <- legMat[-nrow(legMat),]
    drawLegend <- function() legend("topleft", legend=legMat[,"legend"], lty=legMat[,"lty"], pch=legMat[,"pch"], col=legMat[,"col"], bty="n");
  }

  maha <- mahalanobis(data,colMeans(data),cov(data))
  if(method=="quantile") {
    q1 <- quantile(maha,1-p.val)
    outliers <- maha > q1
    if(make.plot) {plot(sort(maha), main=paste0("Mahalanobis outliers,\nmethod=\"quantile\", p.val=",p.val) ); drawLegend(); abline(h=q1)}
  } else if(method=="chisq") {
    p.chisq <- pchisq(q=maha,df=ncol(data)) # df ist richtig. Laut ETH Folien zu "mutivariate outliers"
    outliers <- p.chisq > 1-p.val
    if(make.plot) {plot(sort(p.chisq), main=paste0("Mahalanobis outliers,\nmethod=\"chisq\", p.val=",p.val) ); drawLegend(); abline(h=1-p.val)}
    warning("Attention! By choosing method='chisq' you assume that all variables follow a normal distribution.")
  } else if(method=="absMahaDistIncrease"){
    # Order data according to mahalanobis distance
    #Sort & resort works like this: maha1 <- c(1,3,2,10,5,0); om <- order(maha1); oMaha <- maha1[om]; dput(oMaha[order(om)])
    om <- order(maha)
    oMaha <- unname( maha[om] )
    # Calculate the differences between ordered mahalanobis distances. Where the difference is larger than a certain quantile, it's a potential outlier.
    dMaha <- c(0,diff(oMaha))
    qdMaha <- quantile(dMaha, 1-p.val)
    diffFilt <- (dMaha > qdMaha)
    if(any(diffFilt)){
      diffFilt[which(diffFilt)[1]:length(diffFilt) ] <- TRUE
      # Calculate a regression for the left part of the distribution until the quantile <regr.quant>
      rMaha <- rank(oMaha)
      regr.quant <- 0.75
      lmFilt <- oMaha < quantile(oMaha, regr.quant)
      coef <- lm(oMaha[lmFilt]~rMaha[lmFilt])$coefficient[2]
      # If a value is <regr.diff.fact> times higher than it would be estimated with the regression, and is in the right side of the distribution it is a potential outlier.
      regr.diff.fact <- 2.3
      regrFilt <- (oMaha / (rMaha * coef)) > regr.diff.fact & rMaha>floor(length(rMaha)/2)
    }

    # Combine the criteria from 'estimated mahalanobis from regression' and 'increasing differences of mahalanobis distance'.
    outliers <- (diffFilt & regrFilt)[order(om)]
    names(outliers) <- names(maha)

    col <- rep("white",length(maha)); col[outliers] <- "black"
    if(make.plot) {
      plot(oMaha, main=paste0("Mahalanobis outliers,\nmethod=\"absMahaDistIncrease\", p.val=",p.val), ylab="Mahalanobis distance" )
      lines(oMaha, col=col[om], type="p", pch=20)
      drawLegend()
      if(any(diffFilt)) abline( h=min(oMaha[diffFilt]), lty=1)
      if(any(regrFilt)) abline( h=min(oMaha[regrFilt]), lty=2, col="red")
    }
  }
  return(outliers)
}
#rev(ma( rev(dMaha), n=floor(length(dMaha)/20) ))
#ma <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}

####
remove.minmax.outliers <- function(x,remove=1,lower=NULL,upper=NULL) {
  if(is.null(upper)&is.null(lower)) {
    if(remove==0) return(x) else {
      for(i in 1:remove) {
        x[c(which(x==max(x,na.rm=TRUE))[1],which(x==min(x,na.rm=TRUE))[1])] <- NA
      }
      return(x)
    }
  } else if(!is.null(upper)&!is.null(lower)&is.null(remove)) {
    for(i in 1:lower) {
      x[which(x==min(x,na.rm=TRUE))[1]] <- NA
    }
    for(i in 1:upper) {
      x[which(x==max(x,na.rm=TRUE))[1]] <- NA
    }
    return(x)
  } else {stop("set either remove or lower and upper but not the three arguments together")}
}

#### DATA CHECK ####

#data <- matrix(c(rep(1,20),sample(1:20,10),sample(1:20,10),sample(1:20,10),sample(1:20,10)),nrow=10); data[10,] <- 2;
#highly.correlated(data,0.5,result="colnames"); highly.correlated(data,0.5,result="colnumbers");
highly.correlated1 <- function(data,level=0.8,digits=2,result=c("colnames","colnumbers","kick.out.logical")) {
  # Explanation: This function returns a table of variables which correlate more than the specified level.
  # use result= to specify if colnames or colnumbers are returned. Use data[,-unique(function.output[,2])] to kick out the highly correlated columns (attention. that doesn't work properly)
  result <- result[1]
  if(is.null(colnames(data))) colnames(data) <- 1:ncol(data)
  corrs <- cor(data)
  diag(corrs) <- 0
  select.corrs <- row(corrs)-col(corrs)
  highly.corr <- which(abs(corrs[])>level&select.corrs>0,arr.ind=TRUE)
  Variable1 <- Variable2 <- character(0)
  Correlation <- numeric(0)
  if(result=="colnames") {
    Variable1 <- colnames(data)[highly.corr[,2]]
    Variable2 <- colnames(data)[highly.corr[,1]]
    Correlation <- round(corrs[highly.corr],digits)
  } else {
    Variable1 <- highly.corr[,2]
    Variable2 <- highly.corr[,1]
    Correlation <- round(corrs[highly.corr],digits)
    kick.out.variable1 <- 1:ncol(data)%in%unique(Variable1)
    kick.out.variable2 <- 1:ncol(data)%in%unique(Variable2)
  }
  if(result!="kick.out.logical") {
    results <- cbind(Variable1,Variable2,Correlation);  colnames(results) <- c("Variable 1","Variable 2","Correlation")
    if(nrow(results)<1) results[1,] <- NA
  } else {results <- rbind(kick.out.variable1,kick.out.variable2); rownames(results) <- c("few","many")}
  return(results)
}

####
highly.correlated2 <- function(d, cutoff) {
  # r2test
  if (cutoff > 1 || cutoff <= 0) {
    stop(" 0 <= cutoff < 1")
  }
  if (!is.matrix(d) && !is.data.frame(d)) {
    stop("Must supply a data.frame or matrix")
  }
  r2cut = sqrt(cutoff);
  cormat <- cor(d);
  bad.idx <- which(abs(cormat)>r2cut,arr.ind=T);
  bad.idx <- matrix( bad.idx[bad.idx[,1] > bad.idx[,2]], ncol=2);
  drop.idx <- ifelse(runif(nrow(bad.idx)) > .5, bad.idx[,1], bad.idx [,2]);
  if (length(drop.idx) == 0) {
    1:ncol(d)
  } else {
    (1:ncol(d))[-unique(drop.idx)]
  }
}

####

####
check.variable.constance <- function(data,grouping=NULL,test=c("any","all"),level=0.5,nvalues=NULL,result=c("logical","colnumbers","colnames"), matrix.output=c(FALSE,"full","relevant")) {
  # Explanation: Function to check if more than level*nrow(data) values are the same in one variable. Can also be applied
  # group-wise to check if more than level*length(group) variables are constant. If you chose nvalues=NULL,  all values which occur more than 2 times in the variable
  # are counted. If you specify nvalues=1, e.g. only 1 constant value is counted. If matrix.output=TRUE only logical output is possible.
  test <- test[1]; result <- result[1]; matrix.output <- matrix.output[1]
  if(is.null(dim(data))) { data <- matrix(data); colnames(data) <- "checkvariable" }
  if(is.null(grouping))  grouping <- rep(1,nrow(data))
  if(is.null(nvalues))
    constance <- function(x,level) sum(table(x)[table(x)>1]) > length(x)*level
  if(!is.null(nvalues))
    constance <- function(x,level) {
      tablex <- table(x); lengthtablex <- length(tablex)
      sum(tablex[1:min(nvalues,lengthtablex)] [ tablex[1:min(nvalues,lengthtablex)]>1 ]) > length(x)*level
    }
  kick.outs <- apply(data,2,function(y)tapply(y,grouping,function(y)constance(y,level)))

  if(matrix.output==FALSE) {
    if(all(grouping==1)) kick.out <- rbind(kick.outs,kick.outs)
    if(test=="any") kick.out <- apply(kick.outs,2,function(x) any(x))
    if(test=="all") kick.out <- apply(kick.outs,2,function(x) all(x))
    if(result=="logical") {     results <- kick.out
    } else if(any(kick.out)) {
      if(result=="colnumbers")  results <- which(kick.out)
      if(result=="colnames")    results <- names(kick.out[kick.out])
    } else results <- NA
  } else if(matrix.output=="full") { results <- kick.outs
  } else if(matrix.output=="relevant") {
    results <- kick.outs[, apply(kick.outs,2,function(x) any(x)) ]
    if(ncol(results)<1) results <- NA
  }
  return(results)
}
####

#data <- matrix(1:100,nrow=10); data[5:10,] <- 1; grouping <- c(rep(1,5),rep(2,5)); #sds <- matrix(sample(c(rep(0,50),rep(1,50)),100),nrow=10)
#check.group.sd(data,grouping,matrix.output=TRUE)
check.group.sd <- function(data,grouping=NULL,test=c("all","any",factor=10)[1],result=c("logical","colnumbers","colnames"),matrix.output=c(FALSE,"full","relevant")) {
  # Explanation: Use this function to check if there are any variations in your groups.
  # test=all: report, if all standard deviations are 0, test=any: report if any sd of a group is 0, test=10 report if the sd of a group is more than 10 times the sd of another.
  # matrix.output=TRUE : report, if standard deviation in group is 0 (only logical output possible)
  result <- match.arg(result);
  matrix.output <- match.arg(matrix.output);
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  sds <- apply(data,2,function(x)tapply(x,grouping,function(y)sd(y,na.rm=TRUE)))
  sds[is.na(sds)] <- 0
  if(matrix.output==FALSE){
    if(test=="any") { kick.out <- apply(sds,2,function(x)  any(x==0) )
    } else if(test=="all") { kick.out <- apply(sds,2,function(x) all(x==0) )
    } else { test <- as.numeric(test); kick.out <- apply(sds,2,function(x) max(x)>test*min(x) ) }
    if(result=="logical") { results <- kick.out
    } else if(any(kick.out)) {
      if(result=="colnumbers") results <- which(kick.out)
      if(result=="colnames") results <- names(kick.out[kick.out])
    } else results <- NA

  } else if(matrix.output=="full")     { results <- sds==0
  } else if(matrix.output=="relevant") {
    semi.result <- sds==0
    results <- semi.result [, apply(sds,2,function(x) any(x==0) )]
    if(ncol(results)<1) results <- NA
  }
  return(results)
}

#' Assert that all aggregations defined in \code{aggrInfo} are consistent within \code{data}.
#' @author Daniel Hoop
#' @export
#' @param data The \code{data.frame} within which the aggregation consistency should be asserted.
#' @param aggrInfo The structure containing the aggregation info. Refer to the example to see how it must be structured.
#' @param tol The tolerance which should be allowed when comparing the sum of disaggregated levels with the higher aggregation level.
#' @return An invisible message "All aggregations are consistent.", if no inconsistencies were found. Otherwise the function will throw an error.
#' @seealso \code{\link{assertDatasetEquality}}
#' @examples
#' data <- data.frame(
#'   total = NA,
#'     a = NA,
#'       a_1 = NA,
#'         a_11 = c(1,2,3),
#'         a_12 = c(1,2,3),
#'       a_2 = c(4,5,6),
#'     b = NA,
#'       b_1 = c(3,4,5),
#'       b_2 = c(3,4,5))
#' data <- within(data, {
#'   a_1 <- a_11 + a_12
#'   a <- a_1 + a_2
#'   b <- b_1 + b_2
#'   total <- a + b
#' })
#' aggrInfo <- list(
#'   # The first entry in aggrInfo must always be a character of length 1.
#'   "total",
#'   # Then, nested lists can follow.
#'   list("a",
#'        list("a_1",
#'             c("a_11", "a_12")),
#'        "a_2"),
#'   list("b",
#'        c("b_1", "b_2"))
#' )
#' # This will work because all aggregations are consistent.
#' assertAggregationConsistency(data = as.matrix(data), aggrInfo = aggrInfo)
#' # Now it will fail because we introduce an inconsistency.
#' data[, "a_2"] <- rep(1, nrow(data))
#' assertAggregationConsistency(data = data, aggrInfo = aggrInfo)
assertAggregationConsistency <- function (data, aggrInfo, tol = 1e-1) {

  # Internal function that will perform the control calculations
  reportLargeDiffsOrGiveSum <- function(sumFigure, lowFigures, tol) {
    # In case, there are no lowFigures, then it's all right. Return the sumFigure.
    if (ncol(lowFigures) == 0)
      return (sumFigure)
    # Else, Compare
    sums <- rowSums(lowFigures)
    diffTooLarge <- abs(sumFigure - sums) > tol
    anyIsNaDiffTooLarge <- any(is.na(diffTooLarge))
    if (anyIsNaDiffTooLarge || any(diffTooLarge))
      stop ("Some aggregations are not consistent! The higher level figure is not a sum of the lower level figures.",
            "\nHigher level figure = ", colnames(sumFigure),
            "\nLower level figures = ", paste0(colnames(lowFigures), collapse = ", "),
            if (anyIsNaDiffTooLarge) {
              paste0("\nNo. of NA values = ", sum(is.na(diffTooLarge)))
            } else {
              paste0("\nNo. of large differences = ", sum(diffTooLarge))
            },
            paste0("\nNo. of observations in `data`: ", nrow(data))
      )
    return (sumFigure)
  }

  # Helper function
  checkColAvailability <- function (data, cols) {
    notAvail <- cols[!cols %in% colnames(data)]
    if (length(notAvail) > 0)
      stop ("The following columns that should be checked for consistency are not available in `data`:\n",
            paste0(notAvail, collapse = ", "))
  }

  # Aggregate the `data` given according to `aggrInfo`.
  # Recursively check all lower levels for consistency with the uppernext level.
  # @keywords internal
  # @return If the check was ok, then the actual values of the upper level are returned. Else, an error will be thrown.
  # If the function runs through, without yielding an error, then everything is fine.
  aggregateFunc <- function (data, aggrInfo, tol = 1e-1) {
    if (!is.list(aggrInfo)) {
      if (length(aggrInfo) != 1)
        stop ("If a main entry in `aggrInfo` is not a list, then it must be a vector of length 1. Errorneous entry is: ",
              'c("', paste0(aggrInfo, collapse = '", "'), '")')
      return (data[aggrInfo])
    }
    if (!is.character(aggrInfo[[1]]) || length(aggrInfo[[1]]) != 1) {
      dput(aggrInfo[[1]])
      stop ("The first (nested) entry in `aggrInfo` must always be a character vector of length 1. See the wrong entry printed above.")
    }
    if (!is.list(aggrInfo[[2]])) {
      return (reportLargeDiffsOrGiveSum(sumFigure = data[aggrInfo[[1]]],
                                        lowFigures = data[aggrInfo[[2]]],
                                        tol = tol))
    }
    return (reportLargeDiffsOrGiveSum(sumFigure = data[aggrInfo[[1]]],
                                      lowFigures = as.data.frame(lapply(aggrInfo[-1], function(x) aggregateFunc(data = data, aggrInfo = x, tol = tol))),
                                      tol = tol))
  }

  # This is the actual function
  if (!is.list(aggrInfo))
    stop ("`aggrInfo` must be a list.")
  if (!is.data.frame(data)) {
    cols1 <- colnames(data)
    data <- data.frame(data, stringsAsFactors = FALSE)
    colnames(data) <- cols1
  }
  checkColAvailability(data = data, cols = sort(unique(unlist(aggrInfo))))
  invisible(aggregateFunc(data = data, aggrInfo = aggrInfo, tol = tol))
  msg <- "All aggregations are consistent."
  message(msg)
  return (invisible(msg))
}

#' Helper function for \code{\link{assertDatasetEquality}}.
#' @keywords internal
#' @author Daniel Hoop
diffInDataset <- function (dat1, dat2, colsToCheck=NULL, tolerance=1e-9) {
  if (!is.null(colnames(dat1))) {
    if (is.null(colsToCheck))
      colsToCheck <- colnames(dat1)
    if (!all(colsToCheck%in%colnames(dat1)))
      stop ("some variables in 'colsToCheck' are not available in 'dat1'.")
    if (!all(colsToCheck%in%colnames(dat2)))
      stop ("some variables in 'dat1' or 'colsToCheck' are not available in 'dat2'.")
  }
  if (suppressWarnings(any(dim(dat1) != dim(dat2))))
    stop ("The dimensions of 'dat1' and 'dat2' must be equal.")
  if (is.null(colnames(dat1)) && is.null(colsToCheck))
    colsToCheck <- 1:ncol(dat1)

  dat1 <- dat1[,colsToCheck,drop=FALSE]
  dat2 <- dat2[,colsToCheck,drop=FALSE]

  errors <- matrix(FALSE, nrow=nrow(dat1), ncol=length(colsToCheck))
  if (!is.null(colnames(dat1)))
    colnames(errors) <- colsToCheck
  if (!is.null(rownames(dat1)))
    rownames(errors) <- rownames(dat1)
  for (col in colsToCheck) {
    if (mode(dat1[,col]) != mode(dat2[,col])) {
      errors[,col] <- TRUE
    } else if (is.numeric(dat1[,col])) {
      # For some reason, the rounding does not work as exactly as intended.
      # Therefore, first check if all are equal. If so, then don't do anything.
      areEqual <- all.equal(dat1[,col], dat2[,col], tolerance = tolerance)
      if (!isTRUE(areEqual)) {
        errors[,col] <- abs(dat1[,col] - dat2[,col]) > tolerance
      }
    } else {
      errors[,col] <- dat1[,col] != dat2[,col]
    }
    errors[,col] <- errors[,col] | ( is.na(dat1[,col]) & !is.na(dat2[,col]))
    errors[,col] <- errors[,col] | (!is.na(dat1[,col]) &  is.na(dat2[,col]))
    # In places where there is a NA it results from NA in both data sets, therefore it is no error.
    errors[is.na(errors[,col]),col] <- FALSE
  }

  return (errors)
}

#' @title Asserts that two data.frames/matrices \code{dat1} and \code{dat2} are equal.
#' @description If not, a warning is shown and the differences in the data sets is returned.
#' @export
#' @author Daniel Hoop
#' @param dat1 A data.frame/matrix or the path to a text file that can be read with 'read.table' or the path to a RData file that contains a single R object.
#' @param dat2 A data.frame/matrix or the path to a text file that can be read with 'read.table' or the path to a RData file that contains a single R object.
#' @param idCols Optional: The columns that uniquely identify a observation in each row. If given, then only rows will compared which are available in both datasets. Otherwise, a different number of rows in \code{dat1} and \code{dat2} will be reported as difference.
#' @param compareAvailObsOnly If \code{idCols} is given, should only those observations be compared which are available in both datasets? Logical value.
#' @param colsToCheck The columns that should be checked for differences in dat1 and dat2.
#' @param regexToCheck A regular expression defining the columns that should be checked for differences in dat1 and dat2.
#' @param colsToShowInReport Additional columns that should be shown in the report (apart from the columns named in idCols and the errorneous cols).
#' @param showBothValuesNotDiff Logical value indicating if (in case of disceprancy) both values should be shown instead of showig the difference.
#' @param tolerance The tolerance with which equality of numeric values should be checked.
#' @param sepForDiff The file to separate two different values. On the left side of the \code{sepForDiff} sign, the value from \code{dat1} is shown. On the right side, the value from \code{dat2} is shown.
#' @param outFile Optional: The name of the file that should be created which will contain all errorneous observations and columns.
#' @param sep The column delimiter used in read.table and write.table
#' @param header Logical value indicating if the files to read in have a header
#' @param ... Additional arguments passed to \code{\link[utils:write.table]{utils::write.table}}
#' @seealso \code{\link{assertAggregationConsistency}}
#' @return
#' \code{NULL} if no differences were detected.
#' Else, a \code{data.frame} containing all errorneous rows and columns, if differences were detected. The \code{data.frame} contains only the differences in the cells.
#' @details
#' Either \code{colsToCheck} or \code{regexToCheck} must be given, otherwise an error is thrown.
assertDatasetEquality <- function (dat1, dat2, idCols=NULL, compareAvailObsOnly=FALSE, colsToCheck=NULL, regexToCheck=NULL, colsShowInReport=NULL, showAllDiffCols=FALSE, showBothValuesNotDiff=TRUE, tolerance=1e-6, sepForDiff = " <|> ", outFile=NULL, openOutFile=FALSE, sep=";", header=TRUE, ...) {

  # Read data
  if (is.character(dat1)) {
    if (length(dat1) != 1)
      stop ("If dat1 is a character, then it must be of length 1.")
    dat1 <- if (endsWith(tolower(dat1), "rdata")) load2(dat1) else read.table(dat1, sep=sep, header=header, ...)
  }
  if (is.character(dat2)) {
    if (length(dat2) != 1)
      stop ("If dat2 is a character, then it must be of length 1.")
    dat2 <- if (endsWith(tolower(dat2), "rdata")) load2(dat2) else read.table(dat2, sep=sep, header=header, ...)
  }

  # Check if data is in right format
  if (is.null(dim(dat1)))
    stop ("dat1 must be a data.frame or matrix.")
  if (is.null(dim(dat2)))
    stop ("dat2 must be a data.frame or matrix.")
  # Make to data.frame
  if (is.matrix(dat1)) {
    cn1 <- colnames(dat1)
    dat1 <- as.data.frame(dat1, stringsAsFactors=FALSE)
    colnames(dat1) <- cn1
  }
  if (is.matrix(dat2)) {
    cn2 <- colnames(dat2)
    dat2 <- as.data.frame(dat2, stringsAsFactors=FALSE)
    colnames(dat2) <- cn2
  }

  # Check if dimensions are different
  if (length(colsToCheck) == 0 && ncol(dat1) != ncol(dat2)) {
    colsNotIn1 <- colnames(dat2)[!colnames(dat2) %in% colnames(dat1)]
    colsNotIn2 <- colnames(dat1)[!colnames(dat1) %in% colnames(dat2)]
    returnString <- paste0(
      if (length(colsNotIn1) > 0) paste0("\nColumns not available in `dat1`: \n  ", paste0(colsNotIn1, collapse=", ")),
      if (length(colsNotIn2) > 0) paste0("\nColumns not available in `dat2`: \n  ", paste0(colsNotIn2, collapse=", ")))
    warning ("*** FAILURE *** Compared data sets are not equal. They don't have the same columns.",
             returnString)
    # Return without leading \n
    return (substring(returnString, 2))
  }
  if (length(idCols) == 0 && nrow(dat1) != nrow(dat2)) {
    returnString <- paste0(
      "\nNumber of rows in `dat1`: ", nrow(dat1),
      "\nNumber of rows in `dat2`: ", nrow(dat2))
    warning ("*** FAILURE *** Compared data sets are not equal. They don't have the same number of rows.",
             returnString)
    # Return without leading \n
    return (substring(returnString, 2))
  }
  if (nrow(dat1) == 0)
    warning ("There are no observations in `dat1`.")
  if (nrow(dat2) == 0)
    warning ("There are no observations in `dat2`.")

  # Get the right cols to check
  if (is.null(colsToCheck)) {
    if (is.null(regexToCheck)) {
      colsToCheck <- colnames(dat1)
    } else {
      colsToCheck <- colnames(dat1)[grepl(regexToCheck, colnames(dat1))]
      if (length(colsToCheck) == 0)
        stop ("The regular expression in 'regexToCheck' did not match any colname in dat1.")
    }
  }
  # Assure that all colsToCheck are in dat1 -> error
  colsMissing <- colsToCheck[!colsToCheck%in%colnames(dat1)]
  if (length(colsMissing) > 0)
    stop (paste0("Some columns defined in 'colsToCheck' are missing in dat1: ", paste0(colsMissing, collapse=", ")))
  # Check if all colsToCheck are in dat2 -> warning
  colsMissing <- colsToCheck[!colsToCheck%in%colnames(dat2)]
  if (length(colsMissing) > 0) {
    warnMsg <- paste0("Some columns defined in 'colsToCheck' are missing in dat2: ", paste0(colsMissing, collapse=", "))
    warning (warnMsg)
    return (warnMsg)
  }

  # Make the right column order
  if (showAllDiffCols) {
    dat1 <- dat1[,colnames(dat1)%in%colnames(dat2)]
    dat2 <- dat2[,colnames(dat1)]
  } else {
    showMissingNames(check = idCols, against = colnames(dat1), "The following colnames in `idCols` are not available in `colnames(dat1)`: ")
    showMissingNames(check = idCols, against = colnames(dat1), "The following colnames in `idCols` are not available in `colnames(dat2)`: ")
    dat1 <- dat1[,unique(c(idCols, colsToCheck)),drop=FALSE]
    dat2 <- dat2[,unique(c(idCols, colsToCheck)),drop=FALSE]
  }

  if (length(idCols) > 0) {
    # Check if all idCols are available in dat1 and dat2
    if (!all(idCols%in%colnames(dat1)))
      stop ("Some columns defined in `idCols` are missing in dat1. Notice that colnames should be given as character, not a vector of ids.")
    if (!all(idCols%in%colnames(dat2)))
      stop ("Some columns defined in `idCols` are missing in dat2. Notice that colnames should be given as character, not a vector of ids.")
    if (compareAvailObsOnly) {
      # Keep rows that are comparable.
      uid <- "vnoiujf0__uidCol__dw38fhx"
      dat1[,uid] <- paste.cols(dat1[,idCols])
      dat2[,uid] <- paste.cols(dat2[,idCols])
      #
      allIds <- dat1[,uid][dat1[,uid] %in% dat2[,uid]]
      dat1 <- dat1[dat1[,uid]%in%allIds,,drop=FALSE]
      dat2 <- dat2[dat2[,uid]%in%allIds,,drop=FALSE]
      dat1 <- dat1[order(dat1[,uid]),,drop=FALSE]
      dat2 <- dat2[order(dat2[,uid]),,drop=FALSE]
      if (any(sum(nrow(dat1), nrow(dat2)) == 0)) {
        warnMsg <- "There are no overlapping observations in `dat1` and `dat2`."
        warning (warnMsg)
        return (warnMsg)
      }
    }
  }

  # differs(c(NA, 1, 2), c(NA, 1, 1), 0.1)
  differs <- function(x, y, tolerance) {
    if (!is.numeric(x) || !is.numeric(y)) {
      d <- x != y
    } else {
      d <- abs(x - y) > tolerance
    }
    d[is.na(x) & is.na(y)] <- FALSE
    d[is.na(d)] <- TRUE
    return(d)
  }

  # Find errorneous columns
  errors <- diffInDataset(dat1=dat1, dat2=dat2, colsToCheck=colsToCheck, tolerance=tolerance)

  # Report no error
  errorRow <- rowSums(errors)>0
  if (sum(errorRow) == 0) {
    message("All values equal.")
    return (invisible(NULL))
  }

  # Find cols that differ
  errorCol <- colSums(errors)>0
  errorColnames <- names(errorCol)[errorCol]
  # Find further cols that differ
  if (showAllDiffCols) {
    colsToCheck2 <- colnames(dat1)[!colnames(dat1)%in%c(idCols, uid, colsToCheck)]
    errors2 <- diffInDataset(dat1=dat1[errorRow,], dat2=dat2[errorRow,], colsToCheck=colsToCheck2, tolerance=tolerance)
    errorCol2 <- colSums(errors2)>0
    errorColnames2 <- names(errorCol2)[errorCol2]
    errorColnames <- unique(c(errorColnames, errorColnames2))
  }
  # Make datasets smaller
  left <- dat1[errorRow,unique(c(idCols, colsShowInReport)),drop=FALSE]
  dat1 <- dat1[errorRow,errorColnames,drop=FALSE]
  dat2 <- dat2[errorRow,errorColnames,drop=FALSE]

  # Make difference data.frame
  charCol <- sapply(dat1, function(x)is.character(x) | is.factor(x)) | sapply(dat2, function(x)is.character(x) | is.factor(x))
  diff <- dat1
  invTol <- -log10(tolerance)

  # For non-character cols
  if (any(!charCol)) {
    diff[,!charCol] <- round(dat1[,!charCol] - dat2[,!charCol], digits=invTol)
  }
  if (showBothValuesNotDiff) {
    dat1[!charCol] <- lapply(dat1[!charCol], round, invTol)
    dat2[!charCol] <- lapply(dat2[!charCol], round, invTol)
    charCol <- rep(TRUE, ncol(dat1))
  }

  # For character/factor cols
  if (any(charCol)) {
    diff[,charCol] <- mapply(
      function(x,y) {
        res <- rep("", length(x))
        #fill <- which(x != y) # use 'which' to avoid NA problems.
        fill <- differs(x, y, tolerance)
        res[fill] <- paste(x[fill], y[fill], sep=sepForDiff)
        # paste0("1-> ", x[fill], "|", y[fill], " <-2")
        return (res)
      },
      x=dat1[,charCol],  y=dat2[,charCol]
    )
  }
  # Add left cols to diff.
  diff <- cbind(left, diff)

  # Write file
  warnMsg <- "*** FAILURE *** Compared data sets are not equal. The data.frame containing differences is returned (invisible)."
  if (!is.null(outFile)) {
    warnMsg <- paste0(warnMsg, " See the following file to check all differences:\n", outFile)
    write.table(diff, file=outFile, sep=sep, quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
    if (openOutFile) {
      Sys.sleep(0.3)
      browseURL(outFile)
    }
  }

  # Give warning and return diff.
  warning(warnMsg)
  return (invisible(diff))
}

#### DATA MANIPULATION ####

clean.number.format <- function(dat, to.numeric=TRUE) {
  # This funciton cleans wrongly formatted data. E.g. from 1.000,00 to 1000.00
  # dat = the data.frame/matrix to be formatted
  # to.numeric = should numeric columns that where originally interpreted as characters be coerced to numeric()?

  # Schauen, in welchen Spalten keine Buchstaben drinstehen, aber ' oder,
  # Debugging
  #x <- c("bxfbs","1e+01","10'00","10,0",1)
  #!grepl("[a-zA-Z]",x) & grepl("'|,",x)
  col <- !sapply(dat,function(x)any(grepl( "[a-zA-Z]" , x ))) & sapply(dat,function(x)any(grepl( "'|," , x )))


  # Falsche , und Tausender-Trennzeichen korrigieren
  dat[,col] <- lapply(dat[,col],function(x){
    if( any(grepl(".",x)&grepl(",",x)) )  # Schritt 1: Format 1.000,00 -> 1000,00
      x <- gsub("\\.","",x)
    x <- gsub(" |'","", # Schritt 2: Format 1 000.00 oder 1'000.00 -> 1000.00
              gsub(",",".",x) ) # Schritt 3: Format 1000,00 -> 1000.00
  })

  return(dat)
}

clean.data.columns <- function(dat){
  # This function deletes duplicated columns and removes some part of the colnames that result from SQL execution with Java (instead of BO).

  # Leere Spalten entfernen
  dat <- dat[,!substr(colnames(dat),1,3)%in%paste0("X.",0:9)]
  # Doppelte Spalten entfernen
  #dat <- dat[,!conames(dat)%in%paste0(rep(colnames(dat),each=10),".",0:9)]
  # Teile von datltennahmen entfernen
  colnames(dat) <- gsub(pattern=paste0(
    c(paste0("SUM\\.BM_BE_WERT_",c("01","02","03","04","05","06","07","08","09",10:99),"\\."),
      paste0("SUM\\.RM_BE_WERT_",c("01","02","03","04","05","06","07","08","09",10:99),"\\."),
      "\\.GEWICHT\\.GEWICHT\\."),
    collapse="|"),replacement="",
    x=colnames(dat))
  # Fertige Datei ausgeben.
  return(dat)
}

if(FALSE){
  x <- cbind(ID=as.character(1:25), BZ=LETTERS[1:25], 1:25)
  char.cols.to.num(x)
  summary(char.cols.to.num(x))
}

#' Change character columns to numeric/integer, if they can be coerced as such.
#' @author Daniel Hoop
#' @keywords internal
#' @param x The data.frame/matrix to be converted.
#' @param stringsAsFactors Logical value indicating if character vectors should be converted to factors. Default is \code{FALSE}.
#' @return A data.frame with same dimensions as \code{x}. Colnames will be equal but rownames might have changed.
char.cols.to.num <- function(x, stringsAsFactors=FALSE){
  if (is.matrix(x)) {
    #rownames(x) <- NULL
    x <- as.data.frame(x, stringsAsFactors=stringsAsFactors)
  }
  if (!is.data.frame(x))
    stop ("x must be a data.frame")
  if (ncol(x) == 0)
    return (x)

  rn <- rownames(x)
  cn <- colnames(x)
  x <- as.data.frame(lapply(x, function(x) {
    if (is.character(x))
      type.convert(x, as.is = !stringsAsFactors)
    else
      x
  }),
  stringsAsFactors=stringsAsFactors)
  rownames(x) <- rn
  colnames(x) <- cn
  return(x)
}

# char.cols.to.num <- function(x, checkrowsForInteger=NULL, stringsAsFactors=FALSE){
#   # This function checks in all cols of a data.frame if they can be coerced to numeric without producing NA values.
#   # If it's possible the col is coerced to numeric with as.numeric()
#
#   if (is.matrix(x)) {
#     #rownames(x) <- NULL
#     x <- as.data.frame(x, stringsAsFactors=stringsAsFactors)
#   }
#   if (is.vector(x)) {
#     names(x) <- NULL
#     x <- as.data.frame(as.list(x))
#   }
#   rn <- rownames(x)
#   cn <- colnames(x)
#
#   naT <- function(x){x[is.na(x)] <- TRUE; return(x)}
#   if (!is.null(checkrowsForInteger) && checkrowsForInteger>nrow(x))
#     checkrowsForInteger <- nrow(x)
#   x <- as.data.frame(lapply(x, function(x) {
#     if (is.character(x) || (!is.null(checkrowsForInteger) && all(naT(round(x[1:checkrowsForInteger])==x[1:checkrowsForInteger]))))
#       type.convert(as.character(x), as.is = !stringsAsFactors)
#     else
#       x
#   }),
#   stringsAsFactors=stringsAsFactors)
#   #x <- as.data.frame(lapply(x,function(x)if(is.character(x)) type.convert(x,as.is=!stringsAsFactors) else x), stringsAsFactors=stringsAsFactors)
#   #x <- as.data.frame(lapply(x, function(x) if( is( tryCatch(as.numeric(x[1:checkrowsForInteger]),error=function(e)e,warning=function(w)w), "warning") ) return(x) else return(as.numeric(x))    ), stringsAsFactors=stringsAsFactors)
#   rownames(x) <- rn
#   colnames(x) <- cn
#   invisible(gc())
#   return(x)
# }

numeric.cols <- function(x, checkrows=100) {
  # Identify numeric columns
  checkrows <- min(nrow(x),checkrows)
  return(unname(
    sapply(x,function(x) !is( tryCatch(as.numeric(x[1:checkrows]),error=function(e)e,warning=function(w)w), "warning") )
  ))
}

#' Convert categorial data into binomial data.
#' @export
#' @author Daniel Hoop
#' @seealso \code{\link{categ.to.ordinal}}
#' @examples
#' x <- sample(1:5, 10, replace=TRUE)
#' categ.to.bin(x, varname = "Typ", sep.sign = "")
#' categ.to.bin(x, allnames = paste0("Value_", LETTERS[1:5]))
categ.to.bin <- function(x, varname="var", sep.sign="_", allnames=NULL){

  if(length(dim(x))>1 || is.list(x)) stop("x must be a vector or array with dim(x)<=1")

  sux <- sort(unique(x))
  lux <- length(sux)
  ord <- array(0, dim=c(length(x), ncol=lux));
  if(is.null(allnames)){
    colnames(ord) <- paste(varname, sux, sep=sep.sign)
  } else {
    if(length(allnames)!=lux) stop("length(allnames) != length(unique(x))")
    colnames(ord) <- allnames
  }

  for(i in 1:lux){
    ord[x==sux[i],i] <- 1
  }
  return(ord)
}

#' Convert categorial data into ordinal data.
#' @export
#' @author Daniel Hoop
#' @seealso \code{\link{categ.to.bin}}
#' @examples
#' x <- sample(1:5, 10, replace=TRUE)
#' categ.to.ordinal(x, varname = "Region", sep.sign = "")
categ.to.ordinal <- function(x, varname="var", sep.sign="_", allnames=NULL){
  sux <- sort(unique(x))
  lux <- length(sux)
  ord <- array(0, dim=c(length(x), ncol=lux));
  if(is.null(allnames)){
    colnames(ord) <- paste(varname, sux, sep=sep.sign)
  } else {
    if(length(allnames)!=lux) stop("length(allnames) != length(unique(x))")
  }

  for(i in 1:lux){
    ord[x>=sux[i],i] <- 1
  }
  return(ord)
}

#' @title Erstellt aus einem Breiten data.frame, wo die Betriebszweige in verschiedenen Spalten stehen einen langen wo alle rawIndex untereinander stehen und indexiert sind.
#' @description Das ist vor allem noetig fuer die Konversion von Datensatzen im Skript 020_aggreg_ME_param.R
#' @export
#' @author Daniel Hoop
#' @seealso \code{\link[stats:reshape]{stats::reshape}}
#' @param fixed Datensatz welcher die Betriebsinformationen enthaelt (z.B. `"ID"` und `"Jahr"`). Kann auch `NULL` sein. Dann werden keine Zusatzinfos angehaengt.
#' @param transform = Datensatz, in welchem alle `rawIndex` nebeneinander in Spalten stehen
#' @param rawIndex Index-Rohling
#' @param colnameIndex Name der Index-Spalte im neuen Datensatz.
#' @param colnameValue Name der neuen Spalte, in die die Werte reingeschrieben werden.
#' @seealso \code{\link[reshape:melt]{reshape::melt}}, \code{\link[stats:reshape]{stats::reshape}}, \code{\link{long.to.wide.df}}
wide.to.long.df <- function(fixed=NULL, transform, rawIndex=colnames(transform), colnameIndex="index", colnameValue="value"){
  # Verbesserungsvorschlag:
  # Die table class bietet einen deutlich einfacheren weg. Evtl. das umsetzen.
  #x <- array(NA, dim=c(3,5,2), dimnames=list(letters[1:3],letters[11:15],c("y","z")))
  #z <- x[,,1]; class(z) <- "table"
  #data.frame( z )

  # Tests
  if(!is.null(fixed) && is.null(dim(fixed))) stop("The argument 'fixed' must be a data.frame or matrix. No vector.")
  if(is.null(dim(transform))) stop("The argument 'fixed' must be a data.frame or matrix. No vector.")
  if(length(colnameIndex)!=1) stop("length(colnameIndex) must be equal 1.")
  if(length(colnameValue)!=1) stop("length(colnameValue) must be equal 1.")
  if(colnameIndex==colnameValue) stop("colnameIndex==colnameValue is not allowed. Please choose another colname.")

  # End-Datensatz erstellen
  if(!is.null(fixed)) {
    res <- suppressWarnings(data.frame(rep.rows.1b1(fixed, length(rawIndex)), rep(rawIndex,nrow(transform)), 0, stringsAsFactors=FALSE))
  } else {
    res <- data.frame(rep(rawIndex,nrow(transform)), 0, stringsAsFactors=FALSE)
  }
  colnames(res)[(ncol(res)-1):ncol(res)] <- c(colnameIndex, colnameValue)

  # Umwandlung des Datensatzes
  choose <- (0:(nrow(transform)-1))*ncol(transform)
  for(i in 1:ncol(transform)){
    res[choose+i,colnameValue] <- transform[,i]
  }
  rownames(res) <- NULL
  return(res)
}

#' @title Erstellt aus einem langen Datensatz (mit vielen Zeilen), wo alle \code{rawIndex} untereinander stehen und indexiert sind, einen breiten \code{data.frame}, wo die indexierten Daten stattdessen in verschiedenen Spalten stehen.
#' @export
#' @author Daniel Hoop
#' @seealso \code{\link[stats:reshape]{stats::reshape}}
#' @param dat 'Langer' Datensatz in indexierter Form.
#' @param colnameFixed Spalten welche die Betriebsinformationen enthalten (z.B. `"ID"` und `"Jahr"`). Kann auch `NULL`` sein. Dann werden keine Zusatzinfos angehaengt.
#' @param colnameIndex Name der Index-Spalte in `dat`.
#' @param colnameValue Name der Spalte in `dat`, welche die Werte enthaelt.
#' @seealso \code{\link[reshape:cast]{reshape::cast}}, \code{\link[stats:reshape]{stats::reshape}}, \code{\link{wide.to.long.df}}
long.to.wide.df <- function(dat, colnameFixed=c("ID","Jahr"), colnameIndex="index", colnameValue="Franken"){

  if (nrow(dat) == 0) {
    if (length(colnameFixed) == 0)
      return (data.frame())
    res <- as.data.frame(matrix(NA, ncol=length(colnameFixed)))[-1,]
    colnames(res) <- colnameFixed
    return (res)
  }
  if (length(colnameIndex) != 1 || length(colnameValue) != 1)
    stop ("`colnameIndex` and `colnameValue` must each be character vectors of length 1.")
  if (!all(colnameValue %in% colnames(dat)))
    stop ("The column '", colnameValue, "' given in argument `colnameValue` is not contained in `dat`." )
  if (!all(colnameIndex %in% colnames(dat)))
    stop ("The column '", colnameIndex, "' given in argument `colnameIndex` is not contained in `dat`." )
  if (!all(colnameFixed %in% colnames(dat))) {
    missing <- colnameFixed[!colnameFixed%in%colnames(dat)]
    stop ("The column(s) '", paste0(missing, collapse="', '"), "' given in argument `colnameFixed` are not contained in `dat`." )
  }
  urawIndex <- unique(dat[,colnameIndex])
  lurawIndex <- length(urawIndex)

  # Zu extrahierende Zeilen auswaehlen und End-Datensatz erstellen
  choose <- which(dat[,colnameIndex]==dat[1,colnameIndex])-1
  transform <- matrix(0, ncol=lurawIndex, nrow=length(choose))
  colnames(transform) <- urawIndex
  # Fehlerpruefung
  check <- c(choose,NA)-c(NA,choose); check <- check[!is.na(check)]
  if(any(check!=check[1])) stop("The entries in colnameIndex are not in uniform order for all observertions of colnameFixed.")
  rm(check)

  # Umwandlung des Datensatzes
  for(i in 1:ncol(transform)){
    transform[,i] <- dat[choose+i,colnameValue]
  }
  res <- data.frame(dat[ seq(1,nrow(dat)-lurawIndex+1,lurawIndex) , colnameFixed ], transform)
  rownames(res) <- NULL
  return(res)
}
####

#' Convert decimal number in percentage.
#' @export
#' @author Daniel Hoop
perc <- function(x,digits=1){
  round(x*100,digits)
}
#' Convert decimal number in "percentage change".
#' @export
#' @author Daniel Hoop
perc.change <- function(x,digits=1){
  round(x*100-100,digits)
}

if(FALSE){
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:9, 10.001))
  x <- as.matrix(x)
  x <- as.list(x)
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=rep("a", 10),stringsAsFactors=FALSE)
  #x <- x[,2]
  perc.selective(x)
  digits=1; limits=c(0,1); margin=2
}

#' Transform all decimal columns/rows in a matrix/data.frame into percentages,
#' but only if all elements of the columns/rows are between the limits.
#' This allows distinction between decimal numbers and other numbers that vary in a greater range.
#' @keywords internal
#' @author Daniel Hoop
perc.selective <- function(x, digits=1, limits=c(0,1), add.name="(%)", margin=2){
  if(length(limits)==1) {
    limits.orig <- abs(limits)
    limits <- numeric()
    limits[1] <- -limits.orig;  limits[2] <- limits.orig
  }

  # Rekursive Funktionsdefinition im Fall, dass !is.null(dim(x))
  if(!is.null(dim(x))) {
    if(is.matrix(x)) {
      add.perc.colname <- apply(x,margin,function(x) is.numeric(x) && all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] ))
      res <- apply(x,margin,function(x)perc.selective(x=x,digits=digits,limits=limits,margin=margin))
      colnames(res)[ add.perc.colname ] <- paste0(colnames(res)[ add.perc.colname ], add.name)
      return(res)
    }
    if(is.data.frame(x)) {
      add.perc.colname <- unlist(lapply(x,function(x) is.numeric(x) && all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] )))
      res <- as.data.frame( lapply(x,function(x)perc.selective(x=x,digits=digits,limits=limits,margin=margin)) ,stringsAsFactors=FALSE)
      colnames(res)[ add.perc.colname ] <- paste0(colnames(res)[ add.perc.colname ], add.name)
      return(res)
    }
  }
  if(is.list(x)) {
    add.perc.colname <- unlist(lapply(x,function(x) is.numeric(x) && all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] )))
    res <- lapply(x,function(x)perc.selective(x=x,digits=digits,limits=limits,margin=margin))
    names(res)[ add.perc.colname ] <- paste0(names(res)[ add.perc.colname ], add.name)
    return(res)
  }

  if(!is.numeric(x)) return(x)
  if(all(is.na(x))) return(x)


  if(all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] )) {
    return( round(100*x,digits=digits) )
  } else  {
    return(x)
  }
}

#' Round all elements of vector x to the same number of significant digits.
#' @param margin is only used when a `data.frame`/`matrix` is given. Then \code{\link[base:apply]{base::apply}} is used.
#' @keywords internal
#' @author Daniel Hoop
signif.equally <- function(x, digits=3, max=1, margin=2) {

  if(!is.null(dim(x))) {
    if(is.matrix(x)) {
      res <- apply(x,margin,function(x)signif.equally(x=x,digits=digits,max=max,margin=margin))
      rownames(res) <- rownames(x)
      return( res )
    }
    if(is.data.frame(x)) {
      res <- as.data.frame( lapply(x,function(x)signif.equally(x=x,digits=digits,max=max,margin=margin)) ,stringsAsFactors=FALSE)
      rownames(res) <- rownames(x)
      return( res )
    }
  }
  if(is.list(x)) return(lapply(x,function(x)signif.equally(x=x,digits=digits,max=max,margin=margin)))

  if(!is.numeric(x)) return(x)

  x.orig <- x
  x <- x[!is.na(x)]
  if(length(x)==0) return(x.orig) # Wenn alles NA Werte waren, sollen NA Werte zur?ckgegeben werden.
  digits.min <- floor(log10(abs(min(x))))+1
  if( digits.min <= 1 ) {
    digits2 <- max
  } else {
    digits2 <- min(1, -(digits.min-digits) )
  }
  return( round(x.orig,digits2) )
}


n.decimals <- function(x){
  stopifnot(class(x)%in%c("numeric","integer"))
  if(length(x)>1) return(  unlist( lapply(as.list(x),function(x)n.decimals(x)) )  )

  x <- abs(x)
  if(x%%1==0) {
    return(0)
  } else {
    return(nchar(strsplit(as.character(x), "\\.")[[1]][2]))
  }
}

#' Round all columns in \code{data.frame} or \code{matrix} to the same number of decimal places.
#' @keywords internal
#' @author Daniel Hoop
#' @examples
#' x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
#' equal.n.decimals(x)
#' x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:9, 10.001))
#' equal.n.decimals(x)
#' x <- data.frame(a=seq(0.1, 1, 0.1) , b=rep("a", 10))
#' equal.n.decimals(x)
equal.n.decimals <- function(x, digits=2){
  if(is.data.frame(x)) {
    res <- equal.n.decimals(as.matrix(as.data.frame(x,stringsAsFactors=FALSE)), digits=digits)
    return(apply(res,2,function(x)gsub(" ","",x)))
  }
  return(formatC(x, format="f", digits=digits))
}

#' Round to the next 0.05 digit.
#' @keywords internal
#' @author Daniel Hoop
#' @examples
#' round.currency(12.31, stringOut = TRUE)
#' [1] "12.30"
round.currency <- function (x, stringOut = TRUE) {
  x <- round(x / 0.05) * 0.05
  if (stringOut)
    return (formatC(x, format="f", digits=2))
  return (x)
}

#' @title Finds the maximal relevant digits for a numeric vector 'x'.
#' @description It rounds all numbers from 'startDigits' to 0 and checks if the difference between the original values and the rounded values are smaller than 'tol'.
#' @keywords internal
#' @author Daniel Hoop
#' @examples
#' max.n.digits(c(0.1, 0.111, 10000))
#' # [1] 3
max.n.digits <- function(x, startDigits=5, tol=10^(-startDigits)) {
  rx <- round(x,startDigits)
  nDigReal <- startDigits
  for(d in startDigits:0)
    if(all(abs(rx - round(x, d)) < tol))
      nDigReal <- d
  return(nDigReal)
}

#' Make all entires in vector the same length.
#' @keywords internal
#' @author Daniel Hoop
#' @param add The sign that will be added.
#' @param where Character indicating if `add` should be added at the beginning or at the end of the string.
#' @examples
#' x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
#' equal.length(equal.n.decimals(x))
equal.length <- function(x, add=0, where=c("beginning","end"), minlength=0, margin=2) {
  if(!is.null(dim(x))) {
    if(is.matrix(x)) return(apply(x,margin,function(x)equal.length(x=x, add=add, where=where, minlength=minlength, margin=margin)))
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)equal.length(x=x, add=add, where=where, minlength=minlength, margin=margin)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)equal.length(x=x, add=add, where=where, margin=margin)))

  where <- match.arg(where)
  nchar.x <- nchar(x)
  nchar.x[is.na(nchar.x)] <- 0
  n.add <- max(minlength, nchar.x)  - nchar.x
  x.new <- character()
  if(where=="beginning") {
    for(i in sort(unique(n.add))) x.new[n.add==i] <- paste0(paste0(rep(add,i),collapse=""), x[n.add==i])
  } else if(where=="end") {
    for(i in sort(unique(n.add))) x.new[n.add==i] <- paste0(x[n.add==i], paste0(rep(add,i),collapse="") )
  }
  return(x.new)
}

#' FUNKTION IS FEHLERHAFT!!! Fuegt Tausender-Trennzeichen in Zahlen ein.
#' @keywords internal
#' @author Daniel Hoop
#' @examples
#' trennzeichen1000(seq(1.5, 45000000.345, 900000.5))
trennzeichen1000 <- function(x, sign="'", digits=2, signifequally=FALSE, power=9){


  # Rekursive Funktionsdefinitionen, falls es sich um eine Matrix, Data.Frame oder Liste handelt.
  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)  trennzeichen1000(x=x, sign=sign, digits=digits, signifequally=signifequally, power=power) )  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)  trennzeichen1000(x=x, sign=sign, digits=digits, signifequally=signifequally, power=power) ) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)  trennzeichen1000(x=x, sign=sign, digits=digits, signifequally=signifequally, power=power) ))


  if(is.character(x)) x <- as.numeric(x)

  if(signifequally) {
    x <- signif.equally(x)
  } else {
    x <- round(x, 2)
  }

  chars <- as.character(x)
  if(length(chars)%%2==0) add_1 <- 0 else add_1 <- 1
  find_comma <- (1:length(chars)) %in% grep("\\.", chars)
  if(sum(find_comma)==0) comma_sign <- "" else comma_sign <- "."
  sep_comma <- unlist(strsplit(chars[find_comma], split="\\."))

  after_comma <- rep("", length(chars))
  after_comma[find_comma] <- sep_comma[(1:(length(find_comma)+add_1))%%2==0]

  before_comma <- rep("", length(chars))
  before_comma[find_comma] <- sep_comma[(1:(length(find_comma)+add_1))%%2!=0]
  before_comma[!find_comma] <- chars[!find_comma]

  power_added <- 0
  i <- 1
  for(i in 1:power){
    select <- nchar(before_comma)>i*3+power_added
    if(sum(select)==0) break

    tmp <- before_comma[select]
    tmp <- paste0(substr.rev(tmp,i*3+power_added+1, nchar(tmp)+power_added+1),
                  sign,
                  substr.rev(tmp,1,i*3+power_added) )

    before_comma[select] <- tmp
    power_added <- power_added + 1
  }
  return(paste0(before_comma, comma_sign, after_comma))
}

####
#scale.extreme(c(1,2,3,4,5,6,7,8,9,10))
#scale.extreme(c(1,2,3,4,5,6,7,8,9,10), range=1, center=0)
#x <- scale.extreme(c(1,2,3,4,5,6,7,8,9,10,22,50,100,23),range=1,center=0); x; mean(x)
scale.extreme <- function(x,na.rm=TRUE,range=1,center=NULL){
  # In contrast du the scale() function this function performs a "Extremwertstandardisierung" (Bacher et al., 2010: Clusteranalyse)
  # The default is a standardization between 0 and 1. This can be altered with range=... and center=...
  minx <- min(x,na.rm=na.rm)
  maxx <- max(x,na.rm=na.rm)
  mm <- maxx-minx
  if(mm==0) {
    if(is.null(center)) return(range/2) else return(center)
  }
  res <- (x-minx)/mm * range
  if(!is.null(center)) res <- res + (center - mean(res))
  return(res)
}
####
# x <- 1:10; rescale(x)
rescale <- function(x, from=0, to=1){
  # Almost the same like scale.extreme but a little simpler and faster
  minx <- min(x, na.rm=TRUE)
  maxx <- max(x, na.rm=TRUE)
  return( (x-minx) * abs(to-from)/(maxx-minx) )
}
####
scale.sd <- function(x,na.rm=TRUE){
  # This is the same like the original scale() function
  y <- x-mean(x,na.rm=na.rm)
  y <- y/sd(x,na.rm=na.rm)
  return(y)
}
####
scale.reverse <- function(x,mean,sd){
  # "unscale" data.
  # Also possible for matrix and data.frame

  attributes(x)$`scaled:center` <- attributes(x)$`scaled:scale` <- NULL
  is.null.dim.x <- is.null(dim(x))
  is.data.frame.x <- is.data.frame(x)
  if(!is.null.dim.x) {
    mean <- c(mean)
    sd <- c(sd)
    if(ncol(x)!=length(mean) | ncol(x)!=length(sd)) stop("You must fulfill the condition: ncol(x) == length(mean) == length(sd)")
    if(!is.data.frame.x) { # If the data is a matrix we have to transform if temporarily into a data.frame such that the multiplication is done correctly
      dimnames.x <- dimnames(x)
      x <- as.data.frame(x)
      y <- as.matrix(  x*as.list(sd)+as.list(mean)  )
      dimnames(y) <- dimnames.x
    } else {
      y <- x*as.list(sd)+as.list(mean)
    }
  } else {
    if(length(x)!=length(mean) | length(x)!=length(sd)) stop("You must fulfill the condition: length(x) == length(mean) == length(sd)")
    y <- x*sd+mean
  }
  return(y)
}
#scale.rev(scale(1:50),mean=mean(1:50),sd=sd(1:50))
#sc1 <- scale(scmat <- matrix(1:100,ncol=10)); mean1 <- apply(scmat,2,function(x)mean(x)); sd1 <- apply(scmat,2,function(x)sd(x)); scale.rev(sc1, mean1, sd1)

scaled.value <- function(value,mean,sd){
  y <- (value - mean)/sd
  return(y)
}
#scale(c(0:5)); scaled.value(0,mean(0:5),sd(0:5))

if(FALSE){
  df1=matrix( 1:100, ncol=2); id1=df1[,1]; colnames(df1) <- c("ID1","Value1")
  df2=matrix(21:120, ncol=2); id2=df2[,1]; colnames(df2) <- c("ID2","Value2")
  match.df.by.id(df1=df1, df2=df2, id1=id1, id2=id2, keep.no.matches=TRUE)
  match.df.by.id(df1=df1, df2=df2, id1=id1, id2=id2, keep.no.matches=FALSE)
  merge(df1, df2, by.x="ID1", by.y="ID2")
}

# Moving average
moving.average <- function(x, n=5, dir=c("middle","retro","forward")){
  dir <- match.arg(dir)
  if(dir=="forward") return( rev(as.vector(filter(rev(x),rep(1/n,n), sides=1))) )
  return(as.vector(filter(x,rep(1/n,n), sides=if(dir=="middle") 2 else 1 )))
}
#moving.average(c(1,2,3,4,5,6,7,8,9,10), n=3, dir=c("middle","retro","forward")[3])


#' Matches two data frames by id.
#' @export
#' @author Daniel Hoop
#' @param df1 The first `data.frame`.
#' @param df2 The second `data.frame`.
#' @param id1 A vector of ids for the first `data.frame`.
#' @param id2 A vector of ids for the second `data.frame`.
#' @param keep.no.matches Logical determining whether rows that could not. Default `TRUE`.
#' @param check.duplicated Logical determining whether the ids should be checked for duplicated entries in each vector before matching. Default `TRUE`.
#' @param stringsAsFactors Logical determining wheter character vectors be converted to factors. Default `FALSE`.
#' @return A `data.frame` containing `i1` and `id2` in the first two columns followed by the columns of `df1` and `df2`.
#' @seealso \code{\link[base:merge]{base::merge}}
match.df.by.id <- function(df1, df2, id1, id2, keep.no.matches=TRUE, check.duplicated=TRUE, stringsAsFactors=FALSE) {
  # This function matches two data frames by id.
  # If wished (by default) also no matches are kept.
  # Alternative: merge(df1, df2, by.x="ID1", by.y="ID2"), see also package data.table.

  if(any( colnames(df1)%in%c("id1","id2") )) stop("There must be no colnames(df1) equal 'id1' or 'id2'")
  if(any( colnames(df2)%in%c("id1","id2") )) stop("There must be no colnames(df2) equal 'id1' or 'id2'")

  if(is.null(dim(df1))|is.null(dim(df2))) stop("df1 and df2 must be data.frame or matrix")
  if(nrow(df1)!=length(id1)) stop("length(id1) must be equal nrow(df1)")
  if(nrow(df2)!=length(id2)) stop("length(id2) must be equal nrow(df2)")

  # Check for NA values in the IDs
  is.na.id1 <- is.na(id1)
  is.na.id2 <- is.na(id2)

  # Save original Colnames and restore them in the end of the function
  cn_orig <- c(colnames(df1),colnames(df2))
  cn_new <- colnames(data.frame(df1[1,,drop=FALSE], df2[1,,drop=FALSE]))

  # If there are NA values, store them seperately in order to add them to the result at the end of the function.
  # But only if keep.no.matches=TRUE. Anyway they are dropped for the matching process.
  if(any(is.na.id1)){
    if(keep.no.matches){
      df1.na <- df1[is.na.id1,,drop=FALSE]
    } else {
      warning("NA values in id1 where removed")
    }
    id1 <- id1[!is.na.id1]
    df1 <- df1[!is.na.id1,,drop=FALSE]
  }
  # Same for id2
  if(any(is.na.id2)){
    if(keep.no.matches){
      df2.na <- df2[is.na.id2,,drop=FALSE]
    } else {
      warning("NA values in id2 where removed")
    }
    id2 <- id2[!is.na.id2]
    df2 <- df2[!is.na.id2,,drop=FALSE]
  }

  id1.double <- duplicated(id1)
  id2.double <- duplicated(id2)
  if(any(id1.double)|any(id2.double)){

    prov.return <- list()
    if(check.duplicated & any(id1.double)){
      stop("There are duplicated IDs in id1")
      # Veraltet. Wird nicht mehr zurueckgegeben.
      message("There are duplicated IDs in id1. See function output and return one of each pair.")
      prov.return$which.id1.duplicated <- which(id1%in%id1[id1.double])
      prov.return$id1 <- id1[ prov.return$which.id1.duplicated ]
      prov.return$df1 <- data.frame(id1=id1[prov.return$which.id1.duplicated],df1[prov.return$which.id1.duplicated,,drop=FALSE], stringsAsFactors=stringsAsFactors)
    }
    if(check.duplicated & any(id2.double)){
      stop("There are duplicated IDs in id2")
      # Veraltet. Wird nicht mehr zurueckgegeben.
      message("There are duplicated IDs in id2. See function output and return one of each pair.")
      prov.return$which.id2.duplicated <- which(id2%in%id2[id2.double])
      prov.return$id2 <- id2[ prov.return$which.id2.duplicated ]
      prov.return$df2 <- data.frame(id2=id2[prov.return$which.id2.duplicated],df2[prov.return$which.id2.duplicated,,drop=FALSE], stringsAsFactors=stringsAsFactors)
    }
    class(prov.return) <- "match.df.by.id.prov"
    rm.gc.keep("prov.return"); return(prov.return)
  }

  # Wenn no-matches nicht behalten werden sollen, ist die einfach.
  if(!keep.no.matches){
    df1 <- df1[id1%in%id2,,drop=FALSE]
    df2 <- df2[id2%in%id1,,drop=FALSE]
    id1 <- id1[id1%in%id2]
    id2 <- id2[id2%in%id1]
    df1 <- data.frame(id1=id1, id2=id2[match(id1,id2)], df1, df2[match(id1,id2),,drop=FALSE], stringsAsFactors=stringsAsFactors)
    rm.gc.keep("df1"); return(df1) # Do not create not object "res" to save memory.
    #return(data.frame(id1=id1, id2=id2[match(id1,id2)], df1, df2[match(id1,id2),,drop=FALSE], stringsAsFactors=stringsAsFactors))


    # Kompliziert, wenn no-matches behalten werden sollen.
  } else {
    ncol_df1 <- ncol(df1)
    ncol_df2 <- ncol(df2)
    colnames_df2 <- colnames(df2)
    colnames_df1 <- colnames(df1)

    df1.gt.df2 <- nrow(df1)>nrow(df2)
    if(!df1.gt.df2){

      newo <- match(id1,id2)
      result <- data.frame(id1, df1, id2=id2[newo], df2[newo,,drop=FALSE], stringsAsFactors=stringsAsFactors)

      #if(keep.no.matches){
      id2.in.id2.new <- id2%in%result[,"id2"]
      if(any(!id2.in.id2.new)){
        #add.df2 <- data.frame(id2=id2[!id2.in.id2.new],df2[!id2.in.id2.new,,drop=FALSE], stringsAsFactors=stringsAsFactors)
        #add.df1 <- matrix(NA,nrow=nrow(add.df2),ncol=ncol_df1+1)
        add.df <- data.frame(matrix(NA,nrow=sum(!id2.in.id2.new),ncol=ncol_df1+1),
                             data.frame(id2=id2[!id2.in.id2.new],df2[!id2.in.id2.new,,drop=FALSE], stringsAsFactors=stringsAsFactors),
                             stringsAsFactors=stringsAsFactors)
        colnames(add.df) <- colnames(result)
        result <- rbind(result,add.df)
      }
      suppressWarnings(rm(add.df, id1, id2, df1, df2)); invisible(gc())

      #} else {
      #  result <- result[!is.na(result[,"id2"]),,drop=FALSE]
      #}


    } else { #if(df1.gt.df2)

      # Hinweis: Den Code von oben kopieren. Dann folgende Ersetzungen vornehmen:
      # df1 -> df3
      # df2 -> df4
      # id1 -> id3
      # id2 -> id4
      # Ruecktauschen:
      # df4 -> df1
      # df3 -> df2
      # id4 -> id1
      # id3 -> id2

      ### Block reinkopieren - Anfang ###
      newo <- match(id2,id1)
      result <- data.frame(id2, df2, id1=id1[newo], df1[newo,,drop=FALSE])

      #if(keep.no.matches){
      id1.in.id1.new <- id1%in%result[,"id1"]
      if(any(!id1.in.id1.new)){
        #add.df1 <- data.frame(id1=id1[!id1.in.id1.new],df1[!id1.in.id1.new,,drop=FALSE], stringsAsFactors=stringsAsFactors)
        #add.df2 <- matrix(NA,nrow=nrow(add.df1),ncol=ncol_df2+1)
        add.df <- data.frame(matrix(NA,nrow=sum(!id1.in.id1.new),ncol=ncol_df2+1) ,
                             data.frame(id1=id1[!id1.in.id1.new],df1[!id1.in.id1.new,,drop=FALSE], stringsAsFactors=stringsAsFactors) ,
                             stringsAsFactors=stringsAsFactors)
        colnames(add.df) <- colnames(result)
        result <- rbind(result,add.df)
      }
      suppressWarnings(rm(add.df, id1, id2, df1, df2)); invisible(gc())
      #} else {
      #  result <- result[!is.na(result[,"id1"]),,drop=FALSE]
      #}
      ### Block reinkopieren - Ende ###

      # Schliesslich Reihenfolge zuruecktauschen:
      result <- result[,c( (1+ncol_df2+1)  #id1
                           ,(1+ncol_df2+1+1):ncol(result) #df1
                           ,1 #id2
                           ,2:(1+ncol_df2)) #df2
                       ]
    }



    # Jetzt id1 und id2 an den Anfang stellen, Rest lassen:
    result <- result[,c( which(colnames(result)=="id1"),
                         which(colnames(result)=="id2"),
                         which(!colnames(result)%in%c("id1","id2")) )
                     ]

    # Am Schluss wieder diejenigen Betriebe einfuegen, die bei ihrer eigenen ID NA Values drinstehen hatten:
    if(any(is.na.id1) & keep.no.matches){
      pseudo.df2 <- as.data.frame(matrix(NA, nrow=nrow(df1.na), ncol=ncol(result)-2-ncol(df1.na)))
      colnames(pseudo.df2) <- colnames_df2
      result <- rbind(result,  data.frame(id1=NA, id2=NA, df1.na, pseudo.df2, stringsAsFactors=stringsAsFactors) )
    }
    if(any(is.na.id2) & keep.no.matches){
      pseudo.df1 <- as.data.frame(matrix(NA, nrow=nrow(df2.na), ncol=ncol(result)-2-ncol(df2.na)))
      colnames(pseudo.df1) <- colnames_df1
      result <- rbind(result,  data.frame(id1=NA, id2=NA, pseudo.df1, df2.na, stringsAsFactors=stringsAsFactors) )
    }

    colnames(result) <- replace.values(cn_new, cn_orig, colnames(result))
  }

  rm.gc.keep("result"); return(result)
}
####
print.match.df.by.id.prov <- function(object){
  object$df1 <- NULL
  object$df2 <- NULL
  class(object) <- "list"
  print(object)
  invisible(object)
}

#' @title Match from uniqe ids on the right side, to multiple ids on the left side.
#' @details This is useful if you want to "distribute" data from a smaller dataset to a larger dataset in which the same observations occur several times.
#' @export
#' @author Daniel Hoop
#' @param id_left The vector containing the ids in the "left" dataset (into which the data should be filled).
#' @param id_right The vector containing the ids in the "right" dataset (from which the data will be drawn).
match.multiple.id.left <- function(id_left, id_right) {
  # This function matches unique IDs in the right vector (the vector that provides values) to non-unique IDs in the left vector (the vector that receives values)
  # Ouput is a matrix. First column serves as index for left vector. Second Column serves as index for right vector.

  if(any(duplicated(id_right))) stop("Duplicated IDs in id_right are *not* allowed.")
  m_right <- match(id_left,id_right);
  m_right <- m_right[!is.na(m_right)]; #m_right
  m_left <- which(id_left%in%id_right)
  return(cbind(left=m_left,right=m_right))
}


#' Helper function for \link{balanced.panel}
#' @keywords internal
#' @author Daniel Hoop
.paste.elements <- function(l, sep="_", errorMsg="All list places must have same length!"){
  if(!is.list(l) && !is.matrix(l)){
    return(l)
  }
  if(is.matrix(l) | is.data.frame(l)) {
    return(paste.cols(l, sep=sep))
  }
  if(length(unique(unlist(lapply(l,function(x)length(x)))))>1) {
    stop(errorMsg)
  }
  paste_own <- function(...) paste(..., sep=sep)
  return( do.call("paste_own", l) )
}


#' Creates a vector to filter observations to a balanced panel.
#' @export
#' @author Daniel Hoop
#' @param id Vector of IDs
#' @param year Vector of year (same length as ID)
#' @param YEAR Years that should be selected
#' @param nYearmin Minimum number of years to be selected for the pseudo balanced panel
#' @param index Index for which the balancing should be done.
#' @param output Logical vector or IDs?
#' @examples
#' id <- sample(1:10,100,TRUE)
#' year <- sample(2000:2002,100,TRUE)
#' YEAR <- c(2000,2001);
#' balanced.panel(id = id, year = year, YEAR = YEAR, output = "logical")
balanced.panel <- function(id, year, YEAR=sort(unique(year)), index=NULL, nYEARmin=length(unique(YEAR)), output=c("logical","ID")){



  output <- match.arg(output)
  YEAR <- unique(YEAR)
  if(length(id)!=length(year)) stop("Length id must be equal length year")

  if(!is.null(index)) {
    if(output!="logical") stop("If !is.null(index) only output=logical is possible!")
    # Converting Index to string vector
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")

    # Recursive function definition if !is.null(index)
    # balanced.panel() for each entry in index separately.
    #x <- cbind(counter=1:length(id),id=id,year=year)[index==index[1],]
    res <- do.call("rbind", by(cbind(counter=1:length(id), id=id, year=year), index, function(x){
      data.frame(x, filter=balanced.panel(id=x[,"id"], year=x[,"year"], YEAR=YEAR, index=NULL, nYEARmin=nYEARmin, output=output),stringsAsFactors=FALSE)
    }))
    return( res[order(res[,"counter"]),"filter"] )
  }

  mode.id <- mode(id) # Save mode of id for later output
  IDs <- list()
  for(i in 1:length(YEAR)){
    IDs[[i]] <- id[ year%in%YEAR[i] ]
  }
  IDs <- do.call("c",IDs)
  table.IDs <- table(IDs)
  if(any(table.IDs>length(YEAR))) {
    return.error <- names(table.IDs)[table.IDs>length(YEAR)]
    mode(return.error) <- mode.id
    stop(paste0("The following observations occur several times in several years! Not able to create balanced panel.\n", paste0(return.error, collapse=", ")))
  }
  IDs.final <- names(table.IDs)[table.IDs>=nYEARmin]
  mode(IDs.final) <- mode.id  # Set back the mode to original value (instead of character from names(table())... )
  if(output=="logical") {
    return( id%in%IDs.final & year%in%YEAR )
  } else {
    warning("This output only serves to show which IDs are available in all years. However, it is possible that they are in other years too. E.g. if you choose YEAR=c(0,1,2), some IDs could be in all years c(0,1,2) but also in year 3. If you want to filter only the relevant IDs AND years choose output='logical'")
    return(IDs.final)
  }
}


#' Filters a balanced panel data set such that the criterium is fulfilled in every year for each observation.
#' @export
#' @author Daniel Hoop
#' @param criterium Expression, that formulates which filtering-criterium must be fulfilled. Colnames of the data must be used!
#' @param data Matrix or data.frame that contains all colnames which are used in the criterium expression.
#' @param id.vec Vector of IDs (preferrably something like data[,"ID"] )
#' @param year.vec Vector of years (preferrably something like data[,"year"] )
#' @param output Should the output be in form of a logical vector or IDs? --> Better choose "logical" in order not to have problems!
#' @examples
#' data <- as.data.frame(matrix(1:100, ncol=2))
#' colnames(data) <- c("a","b")
#' id.vec <- rep(1:25, 2)
#' year.vec <- rep.1b1(c(2003, 2004), 25)
#' cbind(data, sum=rowSums(data), id=id.vec, year=year.vec, filter=balanced.panel.filtering(a+b<110, data, id.vec, year.vec))
balanced.panel.filtering <- function(criterium, data, id.vec, year.vec, output=c("logical","ID"), NAasFALSE=TRUE) {

  output <- match.arg(output)
  if(!is.data.frame(data)) data <- as.data.frame(data)
  mf <- match.call() # http://stackoverflow.com/questions/4682709/how-to-write-an-r-function-that-evaluates-an-expression-within-a-data-frame
  # See also http://stackoverflow.com/questions/4692231/r-passing-expression-to-an-inner-function
  # or http://adv-r.had.co.nz/
  criterium <- eval(mf$criterium, envir=data) # It would also work with eval(substitute(criterium), data) without match.call()
  criterium[!is.finite(criterium)] <- !NAasFALSE

  table.id.vec.criterium <- table(id.vec[criterium])
  ID <- as.numeric( names( table.id.vec.criterium )[ table.id.vec.criterium==length(unique(year.vec)) ] )
  if(output=="logical") {
    return( id.vec %in% ID  )
  } else {
    return(ID)
  }
}

if(FALSE){
  #x <- round( runif(100)*100 )
  #id <- rep(1:50, 2)
  #year <- rep.1b1(c(0,1),50)
  gb <- load.gb()
  x <- gb[,"rohPara_tot"]
  id <- gb[,"ID"]
  year <- gb[,"Jahr"]
  weights <- gb[,"Gewicht"]
  YEAR=sort(unique(year));
  baseyear=min(YEAR); geometric=FALSE; absolute.diff=TRUE; filter=FALSE; filter.level=c(1/3,3); return.N=FALSE;

  # Debugging
  mean(x[year==1]) - mean(x[year==0])
  mean(x[year==1]) / mean(x[year==0])
  balanced.panel.development(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=FALSE,absolute.diff=FALSE,filter=FALSE,filter.level=c(1/3,3),return.N=FALSE)
  mean.geom(x[year==1]) - mean.geom(x[year==0])
  mean.geom(x[year==1]) / mean.geom(x[year==0])
  balanced.panel.development(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=TRUE,absolute.diff=TRUE,filter=FALSE,filter.level=c(1/3,3),return.N=FALSE)

  # Weighted
  w <- c(1:10)
  x <- sample(1:100,10,replace=TRUE)
  mean( x[1:5]-x[6:10] )
  mean( (x*w)[1:5]-(x*w)[6:10] )
  weighted.mean( x[1:5]-x[6:10], w[6:10] )
}

####
# Daten zur Kontrolle der Funktion:
# LE Typ 54, vergleichbare 2013/2014, UNGEWICHTET: 2013:61433, 2014:70187, Diff:8754   Werte nicht vergleichbar, gewichtet, 2013:58133, 2014:66865, Diff:8732
balanced.panel.development <- function(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),weights=NULL,geometric=TRUE,absolute.diff=FALSE,absolute.number=c("no","comparable_baseyear","all_baseyear"),filter=TRUE,filter.level=c(1/3,3),return.N=FALSE){
  # Note that there is a safety copy of this function just below.
  # weights are not implemented for geometric means!!!

  # This function calculates an index of an unbalanced time series.
  # This can be useful if you want to follow the delevoptment of yields but the time horizon
  # is so long that a balanced panel over the whole period has 0 observations.
  # The function calculates relative changes of every pair of year (which builds some kind of 2-year balanced panel).
  # The changes from year to year are then multiplied over the hole period ( geometric=TRUE ) or summed up ( geometric=FALSE ).
  # x:               data like e.g. yield
  # id:              Vector of IDs
  # year:            Vector of year (same length as ID)
  # YEAR:            Years that should be selected
  # baseyear:        The baseyear in which the index has value 1
  # weights:         optional weights for the calculations
  # geometric:       See description above
  # absolute.diff:   Should absolute differences instead of relative differences be calculated?
  # absolute.number: Should absolute numbers instead of relative changes be calculated? If yes, which mean should be calculated in the baseyear?
  #                  The mean of the comparable observations in baseyear and baseyear+1 (comparable_baseyear) or the mean of all observations in the baseyear (all_baseyear)?
  # filter:          Should extreme changes (that are probalby not realistic) be filtered out?
  # filter.level:    If yes, which change-factor is the threshold to filter out c(upper,lower)
  # return.N:        Should the number of observations in each year be calculated instead of the index?

  if(is.data.frame(x) | is.list(x)) return( sapply(x,function(x)balanced.panel.development(x=x,id=id,year=year,YEAR=YEAR,baseyear=baseyear,weights=weights, geometric=geometric, absolute.diff=absolute.diff, absolute.number=absolute.number, filter=filter,filter.level=c(1/3,3),return.N=return.N)) )
  if(is.matrix(x)) return( apply(x,2,                function(x)balanced.panel.development(x=x,id=id,year=year,YEAR=YEAR,baseyear=baseyear,weights=weights, geometric=geometric, absolute.diff=absolute.diff, absolute.number=absolute.number, filter=filter,filter.level=c(1/3,3),return.N=return.N)) )

  # weights wird w genannt, damit es im Code einfacher einzubauen ist
  weighted <- !is.null(weights)
  if(weighted){
    w <- weights
    if(length(x)!=length(w)) stop("length(x)!=length(weights)")
  }
  rm(weights)

  absolute.number <- match.arg(absolute.number)

  if(filter) warning("You have chosen filter=TRUE which excludes extreme values from analysis.")
  if(geometric & absolute.diff) warning("The combination of mean.geom and absoulte difference is rather unusual!")
  if(geometric & weighted) warning("Weighting is not implemented for geometric means.")
  if(absolute.number%in%c("comparable_baseyear","all_baseyear")) absolute.diff <- TRUE
  if(length(x)!=length(id))   stop("length(x)!=length(id)")
  if(length(x)!=length(year)) stop("length(x)!=length(year)")
  if(!baseyear%in%YEAR) stop("baseyear must be in the range of YEAR")
  if(any(!YEAR%in%year)) warning("the years ", YEAR[!YEAR%in%year]," are used in YEAR but do not exist in year")

  YEAR <- sort(YEAR)
  d.means <- numeric()
  N <- numeric(length(YEAR)); names(N) <- YEAR; N[N==0] <- NA;

  i <- 1
  # Start loop over all YEAR
  for(i in 1:(length(YEAR)-1)) {
    # Making a balanced panel for 2 years
    ids <- id[year%in%c(YEAR[i],YEAR[i+1])]
    table.ids <- table(ids)
    if(any(table.ids>2)) {
      output <- names(table.ids)[table.ids>2]
      print(output)
      invisible(output)
      stop("Some ids occur several times in the same year.")
    }
    ids.final <- names(table.ids)[table.ids==2]
    year12 <- id%in%ids.final & year%in%c(YEAR[i],YEAR[i+1])
    # Choosing the first and the second year of the balanced panel seperately
    year1 <- year12 & year%in%YEAR[i]
    year2 <- year12 & year%in%YEAR[i+1]
    if(FALSE){ # Test, if it worked
      print(table( year[year12] ))
      print(all(table(id[year12])==2))
    }

    # Calculate the number of observations for the first year.
    if(return.N){
      if(i==1){
        year1.temp <- year1
        is.na.year1 <- is.na(x[year1.temp])
        year1.temp[is.na.year1] <- FALSE
        N[1] <- sum(year1.temp)
      }
    }

    # Filtering of extreme Values is always done with relative changes.
    if(filter) {
      d <- x[year2][order(id[year2])]   /   x[year1][order(id[year1])]
      keep <- d<filter.level[1] | d>filter.level[2]
      keep[is.na(keep)] <- TRUE
    } else {
      keep <- rep(TRUE, length(x))
    }

    if(geometric){
      # Calculate the relative differences between the years.
      # Important: The values have to be ordered such that always the same observations are compared
      d.means[i] <- mean.geom(x[year2][order(id[year2])][keep]) / mean.geom(x[year1][order(id[year1])][keep])

    } else {
      # Calculate the absolute differences between the years.
      if(weighted) {
        d.means[i] <- weighted.mean(x[year2][order(id[year2])][keep], w[year2][order(id[year2])][keep],na.rm=TRUE) -
          weighted.mean(x[year1][order(id[year1])][keep], w[year1][order(id[year1])][keep],na.rm=TRUE)
      } else {
        d.means[i] <- mean(x[year2][order(id[year2])][keep],na.rm=TRUE) - mean(x[year1][order(id[year1])][keep],na.rm=TRUE)
      }

      # If the absolute number is whised (absolute.number==TRUE), the mean of the according year has to be calculated with the comparable observations
      # Falls so gewaehlt, den Mittelwert der Vergleichbaren im Baseyear berechnen.
      if(absolute.number=="comparable_baseyear"){
        if(YEAR[i]==baseyear){
          if(weighted){
            mean.baseyear <- weighted.mean(x[year1][order(id[year1])][keep], w[year1][order(id[year1])][keep],na.rm=TRUE)
          } else {
            mean.baseyear <- mean(x[year1][order(id[year1])][keep],na.rm=TRUE)
          }
        }
      }
    }

    # Calculate the number of observations that were used to build the difference
    if(return.N) N[i+1] <- sum(!is.na(d[[i]]))
  }
  # END OF THE LOOP

  # Bei geometric ist der Wert im 1. Jahr ist der Ausgangswert. Also 1.
  # Bei arithmecid 0
  if(geometric){
    d.means <- c(1,d.means)
  } else {
    d.means <- c(0,d.means)
  }

  if(return.N) return(N)

  if(geometric){
    # Bisher wird jeweils die Ver?nderung zum Vorjahr wiedergegeben.
    # Nun m?ssen die Werte noch miteinander multipliziert werden, damit sich der Verlauf von Anfang an ergibt.
    index <- numeric(length(d.means)); names(index) <- YEAR
    for(i in 1:length(d.means)) index[i] <- prod(d.means[1:i])

    # Alt
    # Alle Zahlen durch den Index im Index-Jahr dividieren, sodass dort der 1-Punkt ensteht.
    #index <- index/index[names(index)==baseyear]
    # Debugging
    # mean.geom(x[year==2012])
    # mean.geom(x[year==2013])

    # Der Inex wird im Jahr 1 = 0 gesetzt. Dann Der Mittelwert im Index-Jahr subtrahiert, sodass dort der 0-Punkt ensteht.
    if(absolute.diff){
      mean.year1 <- mean.geom(x[year1],na.rm=TRUE)
      index <- index*mean.year1 - mean.year1
      index <- index-index[names(index)==baseyear]
      # Der Inex wird im Jahr 1 = 1 gesetzt. Dann alle Zahlen durch den Index im Index-Jahr dividiert, sodass dort der 1-Punkt ensteht.
    } else {
      index <- index/index[names(index)==baseyear]
    }


  } else { # if(!geometric)

    # Bisher wird jeweils die Ver?nderung zum Vorjahr wiedergegeben.
    # Nun m?ssen die Werte noch addiert werden, damit sich der Verlauf von Anfang an ergibt.
    index <- numeric(length(d.means)); names(index) <- YEAR
    for(i in 1:length(d.means)) index[i] <- sum(d.means[1:i])

    # Choose again the balanced Panel of the first year. Then put all numbers in to relation to that mean.
    ids <- id[year%in%c(YEAR[1],YEAR[1+1])]
    table.ids <- table(ids)
    ids.final <- names(table.ids)[table.ids==2]
    year12 <- id%in%ids.final & year%in%c(YEAR[1],YEAR[1+1])
    # Choosing the first and the second year of the balanced panel seperately
    year1 <- year12 & year%in%YEAR[1]
    year2 <- year12 & year%in%YEAR[1+1]
    if(FALSE){
      # So bildet man am Anfang nur den Mean, der Vergleichbaren Betriebe von Jahr 1 und Jahr 2, von denen auch die Differenz berechnet wird.
      is.na.year2 <- is.na(x[year2])
      year2[is.na.year2] <- FALSE
      year1.new <- year1 & year2
      #length(year1); length(year2) # Kontrolle
      mean.year1 <- mean(x[year1.new],na.rm=TRUE)
    }
    # So bildet man am Anfang den Mean, aller Vergleichbaren Betriebe von Jahr 1 und Jahr 2
    if(weighted) {
      mean.year1 <- weighted.mean(x[year1],w[year1],na.rm=TRUE)
    } else {
      mean.year1 <- mean(x[year1],na.rm=TRUE)
    }

    # Der Index wird relativiert durch den Mean des ersten Jahres.
    index <- (index+mean.year1)

    # Debugging
    # mean(x[year==2012])
    # mean(x[year==2013])

    # Der Inex wird im Jahr 1 = 0 gesetzt. Dann Der Mittelwert im Index-Jahr subtrahiert, sodass dort der 0-Punkt ensteht.
    if(absolute.diff){
      index <- index-mean.year1
      index <- index-index[names(index)==baseyear]
      # Wenn absolute Werte, nicht differenzen gegeben werden sollen, dann vom baseyear den mean addieren
      # Falls so gewaehlt, den Mittelwert der Vergleichbaren im Baseyear addieren.
      if(absolute.number=="comparable_baseyear") {
        index <- index + mean.baseyear
        # Falls so gewaehlt, den Mittelwert der aller Beobachtungen im Baseyear addieren.
      } else if(absolute.number=="all_baseyear") {
        choose_baseyear <- year==baseyear
        if(weighted) {
          index <- index + weighted.mean(x[choose_baseyear],w[choose_baseyear],na.rm=TRUE)
        } else {
          index <- index + mean(x[choose_baseyear],na.rm=TRUE)
        }
      }
      # Der Inex wird im Jahr 1 = 1 gesetzt. Dann alle Zahlen durch den Index im Index-Jahr dividiert, sodass dort der 1-Punkt ensteht.
    } else {
      index <- index/mean.year1
      index <- index/index[names(index)==baseyear]
    }
  } # End if

  if(any(is.na(index)) & geometric) message("Note that the geometric mean can only be calculated if all numbers are positive.")
  return(index)
}


#' @title Group elements of a vector according to given absolute scale.
#' @export
#' @author Daniel Hoop
#' @param x The vector containing the values relevant for the grouping.
#' @param selection.levels The levels to seggregate the data in \code{x}.
#' @param method \code{"< x <="} means that the the smallest value of the group is stricly above the lower group boundary.\cr
#' \code{"<= x <"} means that the smallest value of a group can be the same as the lower group boundary.
#' @param include.min.max Logical value indicating if the the min and max should be included in the groups, even if they may be exactly the same as the group boundary level.
#' @param give.names Logical value indicating if the grouping vector should have names. The names will show the group boundaries.
#' @param names.digits The digits to ground the grouping names if \code{give.names = TRUE}.
#' @param names.sep The group boundary separation sign for grouping names if \code{give.names = TRUE}.
#' @param equal.length.names Logical value indicating if the names should be striclty of equal length, e.g. \code{"1-10"} would be changed to \code{" 1-10"}.
#' @seealso \code{\link[base:cut]{base::cut}}
#' @details If \code{include.min.max=TRUE}, then the outermost observations are also included.
#' Otherwise, if you give exactely the min(x) and max(x), one of both would be excluded.
#' If `selection.levels` is only one value, then all values below will be numbered 1 and all values above will be numbered 2.
#' @examples
#' group.by.fix.scale(c(0.2,1,10,1000), c(0.1,9.3,20,2000), give.names=TRUE, equal.length.names=TRUE)
group.by.fix.scale <- function(x, selection.levels, method=c("< x <=", "<= x <"), include.min.max=FALSE, give.names=FALSE, names.digits=2, names.sep="-", equal.length.names=FALSE){

  method <- match.arg(method)
  if(length(dim(x))>1)
    stop("x must be a vector or array with 1 dimension.")

  selection.levels <- sort(selection.levels)
  is.na.x <- is.na(x)
  if(length(selection.levels)==1) {
    include.min.max <- TRUE
    selection.levels <- c(min(x[!is.na.x]), selection.levels, max(x[!is.na.x]))
  }
  length.selection.levels <- length(selection.levels)

  # Define function to give names
  if (give.names) {
    if (equal.length.names) {
      nDigReal <- max.n.digits(selection.levels, startDigits=names.digits)
      slc <- equal.length(equal.n.decimals(selection.levels, nDigReal), add=" ")
      nam <- function(j, k)
        #return( paste(slc[c(j,k)], collapse=names.sep) )
        return( paste(c(slc[j], zapsmall(round(selection.levels[k], names.digits))), collapse=names.sep) )
    } else {
      nam <- function(j, k)
        return( paste(zapsmall(round(c(selection.levels[j],selection.levels[k]), names.digits)), collapse=names.sep) )
    }
  }

  # Group
  grouping <- rep(NA,length(x))
  if(method=="<= x <")  {
    for(i in 1:(length.selection.levels-2)) {
      grouping[ x >= selection.levels[i] & x < selection.levels[i+1] ] <- i
      if(give.names) names(grouping)[ x >= selection.levels[i] & x < selection.levels[i+1] ] <- nam(i, i+1)
    }
    if(include.min.max){
      grouping[ x >= selection.levels[length.selection.levels-1] & x <= selection.levels[length.selection.levels] ] <- length.selection.levels-1
      if(give.names) names(grouping)[ x >= selection.levels[length.selection.levels-1] & x <= selection.levels[length.selection.levels] ] <- nam(length.selection.levels-1, length.selection.levels)
    } else {
      grouping[ x >= selection.levels[length.selection.levels-1] & x < selection.levels[length.selection.levels] ] <- length.selection.levels-1
      if(give.names) names(grouping)[ x >= selection.levels[length.selection.levels-1] & x < selection.levels[length.selection.levels] ] <- nam(length.selection.levels-1, length.selection.levels)
    }
  } else if(method=="< x <=")  {
    if(include.min.max){
      grouping[ x >= selection.levels[1] & x <= selection.levels[2] ] <- 1
      if(give.names) names(grouping)[ x >= selection.levels[1] & x <= selection.levels[2] ] <- nam(1,2)
    } else {
      grouping[ x > selection.levels[1] & x <= selection.levels[2] ] <- 1
      if(give.names) names(grouping)[ x > selection.levels[1] & x <= selection.levels[2] ] <- nam(1,2)
    }
    for(i in 2:(length(selection.levels-1)))  {
      grouping[ x > selection.levels[i] & x <= selection.levels[i+1] ] <- i
      if(give.names) names(grouping)[  x > selection.levels[i] & x <= selection.levels[i+1] ] <- nam(i, i+1)
    }
  }

  if(any(is.na(grouping))) {
    #print(grouping)
    warning("NAs produced")
  }
  return(grouping)
}


#' @title Group elements of a vector by quantiles.
#' @description This is a wrapper for \code{\link{group.by.fix.scale}} with a slightly altered interface.
#' @export
#' @author Daniel Hoop
#' @param probs The quantile probabilities to segregate the vector given in \code{x}.
#' @inheritParams group.by.fix.scale
#' @seealso \code{\link{group.by.fix.scale}}
#' @examples
#' data <- data.frame(x = rnorm(100), weights = rnorm(100, 10))
#' data[,"gr"] <- group.by.quantiles(x = data[,"x"], weights = data[,"weights"])
#' head(data)
group.by.quantiles <- function(x, probs=seq(0,1,0.1), method=c("< x <=", "<= x <"), index=NULL, include.min.max=TRUE, give.names=FALSE, names.digits=2, names.sep="-", equal.length.names=FALSE, weights=NULL, na.rm=FALSE){

  method <- match.arg(method)

  if(!is.null(index)) {
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
    # Recursive function definition if !is.null(index). --> group.by.fix.scale() for each entry in index separately.
    res <- do.call("rbind", by(cbind(counter=1:length(x), x=x), index, function(x) {
      grouping <- group.by.quantiles(x=x[,"x"], probs=probs, method=method, index=NULL, include.min.max=include.min.max, give.names=give.names, names.digits=names.digits, names.sep=names.sep, equal.length.names=equal.length.names, weights=weights, na.rm=na.rm)
      if (give.names)
        return(data.frame(x["counter"], `grouping`=unname(grouping), `names`=names(grouping)))
      return(data.frame(x["counter"], `grouping`=unname(grouping)))
    }))
    if (give.names) {
      res <- res[order(res[,"counter"]), c("grouping", "names")]
      gr <- res[, "grouping"]
      names(gr) <- res[ ,"names"]
      return(gr)
    }

    return(res[order(res[,"counter"]),"grouping"])
  }

  probs <- sort(probs)
  probs.rel <- probs; rm(probs)
  if(any(probs.rel<0) | any(probs.rel>1)) stop("choose 0 >= probs >= 1")
  if(length(probs.rel)==1) {
    include.min.max <- TRUE
    probs.rel <- c(0, probs.rel, 1)
  }

  if(is.null(weights))  {
    probs.abs <- quantile(x,probs.rel)
  } else {
    if(length(weights)!=length(x)) stop("length(x) must be equal length(weights)")
    #require.package(Hmisc)
    #probs.abs <- wtd.quantile(x=x, weights=weights, probs=probs.rel)
    probs.abs <- quantile.weight(x=x, weights=weights, index=index, probs=probs.rel, na.rm=na.rm)
    # Vergleich: quantile(x=x, probs=probs.rel)
    if(min(probs.rel)==0) probs.abs[which.min(probs.rel)] <- min(x)-1
    if(max(probs.rel)==1) probs.abs[which.max(probs.rel)] <- max(x)+1
  }

  grouping <- group.by.fix.scale(x=x, selection.levels=probs.abs, method=method, include.min.max=include.min.max, give.names=give.names, names.digits=names.digits, names.sep=names.sep, equal.length.names=equal.length.names)
  return(grouping)
}

#' @title Group data by quantiles.
#' @description Like \code{\link{group.by.quantiles}}, but with other default argument `probs`.
#' @export
#' @author Daniel Hoop
#' @seealso \code{\link{group.by.quantiles}}
#' @examples
#' data <- data.frame(x = rnorm(100), weights = rnorm(100, 10))
#' data[,"gr"] <- group.by.quantiles(x = data[,"x"], weights = data[,"weights"])
#' head(data)
group.by.quartiles <- function(...){
  group.by.quantiles(..., probs=c(0, 0.25, 0.5, 0.75, 1))
}

#' @title Group elements of a vector by weighted quantiles.
#' @description Like \code{\link{group.by.quantiles}}, but weighted.
#' @export
#' @author Daniel Hoop
#' @inheritParams group.by.quantiles
#' @param weights Either NULL or a numeric vector giving the weights.
#' @param include.zero.weight.obs Logical value indicating if observations with weight 0 should be included into the outer groups in case they had not beed allocated to them.
#' @seealso \code{\link{group.by.quartiles}}
#' @examples
#' data <- data.frame(x = rnorm(100), weights = rnorm(100, 10))
#' data[,"gr"] <- group.by.wtd.quantiles(x = data[,"x"], weights = data[,"weights"])
#' head(data)
group.by.wtd.quantiles <- function(x, weights=NULL, index=NULL, probs=c(0, 0.25, 0.5, 0.75, 1), method=c("< x <=", "<= x <"), na.rm=TRUE, include.zero.weight.obs = FALSE) {
  # This function groups the elements of a vector by weighted quantiles.
  # Weights and index can be given.

  method <- match.arg(method)
  if(!is.null(dim(x)))
    stop("x must be a vector")
  isAnyWeight0 <- !is.null(weights) && any(weights == 0)

  # If an index is given, the quantiles are grouped for each index
  if(!is.null(index)) {
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
    res <- do.call("rbind",
                   as.list(by( cbind(counter=1:length(x), x=x, weights=weights), index, function(x){
                     x[,"q"] <- suppressWarnings( group.by.wtd.quantiles(x=x[,"x"], weights=if(is.null(weights)) NULL else x[,"weights"], index=NULL, probs=probs, method=method, na.rm=na.rm, include.zero.weight.obs = include.zero.weight.obs) )
                     return(x)
                   })))
    grouping <- res[order(res[,"counter"]),"q"]
  } else {

    # This is the actual grouping function
    selection.levels <- quantile.weight(x=x, weights=weights, index=NULL, probs=probs, na.rm=na.rm)
    supWarnFunc <- if (isAnyWeight0) base::suppressWarnings else (function(x) return(x))
    grouping <- supWarnFunc(group.by.fix.scale(x=x, selection.levels=selection.levels, method=method, include.min.max=TRUE))
  }

  if(isAnyWeight0 && any(is.na(grouping))) {
    if (include.zero.weight.obs) {
      if (method == "< x <=") {
        grouping[is.na(grouping) & weights == 0 & x <  min(x[!is.na(grouping)])] <- min(grouping, na.rm = TRUE)
        grouping[is.na(grouping) & weights == 0 & x >= max(x[!is.na(grouping)])] <- max(grouping, na.rm = TRUE)
      } else if (method == "<= x <") {
        grouping[is.na(grouping) & weights == 0 & x <= min(x[!is.na(grouping)])] <- min(grouping, na.rm = TRUE)
        grouping[is.na(grouping) & weights == 0 & x >  max(x[!is.na(grouping)])] <- max(grouping, na.rm = TRUE)
      }
    } else {
      warning("There were weights==0. This caused some groupings to become NA because the observations with weight 0 were located outside the range of all other observations. ",
              "To prevent this from happening, set `include.zero.weight.obs = TRUE`.")
    }
  }
  return(grouping)
}

####
group.to.ngroups <- function(data, variable, ngroups){
  data <- data[order(data[,variable]),,drop=FALSE]
  nrow.data <- nrow(data)
  size <- nrow.data/ngroups
  grouping <- rep(NA,nrow.data)
  grouping[1:round(size)] <- 1
  for(i in 1:(ngroups-1)) grouping[round(size*i+1):round(size*(i+1))] <- i+1
  result <- cbind(data,grouping=grouping)
  return(result)
}

####
randomize.data <- function(x,index=NULL,times.sd=1,greater.zero=FALSE){
  # Randomize data (e.g. Grundlagenbericht) such that the mean of the numbers stays more or less the same
  # But the data is not "real" anymore

  #x <- 1:100; index=NULL; times.sd=1; greater.zero=FALSE
  #randomize.data(1:10,greater.zero=TRUE)
  if(is.matrix(x)) {
    return( apply(x,2,function(x)randomize.data(x=x,index=index,times.sd=times.sd,greater.zero=greater.zero)) )
  } else if(is.data.frame(x)){
    return(  sapply(x,function(x)randomize.data(x=x,index=index,times.sd=times.sd,greater.zero=greater.zero))  )
  }
  if(is.null(index)) index <- rep("a",length(x))
  randomize.inner <- function(x, times.sd, greater.zero){
    #x.new <- x + rnorm(length(x),0,times.sd*sd(x))
    add <- times.sd*sd(x)
    x.new <- x + sample(add*seq(-1,1,0.1),length(x),replace=TRUE)
    if(greater.zero) {
      x.new[x.new<0 & x>=0] <- 0
    }
    return(x.new)
  }
  result <- tapply(x,index,function(x)randomize.inner(x=x, times.sd=times.sd, greater.zero=greater.zero) )
  result <- unlist(result)
  return(result)
}

####
na.replace <- function(x,grouping=NULL,sd=1,warnings=0.5) {
  # Explanation: The NA values are replaced by a random normal variable with the mean and sd of the observations.
  # If grouping is given, the means&st'devs of the groups are used for group-wise NA replacement
  # Note: NAs of variables and groups which only have NAs are kept! If there is one value this one replaces all the NAs with a warning.
  # warnings=... specifies when a warning should be printed. The default warning is printed when 50% of the values in a group are replaced.

  # Definition of na.replace function
  if(any(is.na(x))) {
    replace.na.inner <- function(x,sd) {
      if(any(is.na(x))) {
        nas <- rep(NA,length(which(is.na(x))))
        meanx <- mean(x,na.rm=TRUE)
        sdix <- sd(x,na.rm=TRUE)
        if(is.na(sdix)) sdix <- 0
        if(length(nas)>1) {
          nas <- rnorm(n=length(nas),mean=meanx,sd=sdix*sd)
        } else nas <- meanx
        x[is.na(x)] <- nas
        return(x)
      } else return(x)
    }

    # Report if there are to many NA values or var(x) could not be calculated..
    if(!is.null(dim(x))) {
      if (is.null(grouping)) grouping.1 <- rep(1,nrow(x)) else grouping.1 <- grouping
      n.na <-                  apply(x,2,function(x)tapply(x,grouping.1,function(y)length(which(is.na(y)))>(1-warnings)*length(y)))
      all.na <-                apply(x,2,function(x)tapply(x,grouping.1,function(y)length(which(is.na(y)))==length(y)))
      if(is.null(dim(n.na)))   n.na <- rbind(n.na,n.na)
      if(is.null(dim(all.na))) all.na <- rbind(all.na,all.na)
      n.na.much <-             apply(n.na,2,function(x)any(x))
      all.n.na.much <-         apply(all.na,2,function(x)any(x))

      sds <- apply(x,2,function(x)tapply(x,grouping.1,function(y)sd(y,na.rm=TRUE)))
      sds[is.na(sds)] <- 0
      if(is.null(dim(sds))) sds <- rbind(sds,sds)
      var0.matrix <- apply(sds,2,function(x) x==0)
      var0 <- apply(var0.matrix,2,function(x)any(x))
      if(any(n.na.much&!all.n.na.much)) warning("in the following variables more than ",round(warnings*100)," % of the values were replaced (at least in some groups)\n",paste(names(n.na.much[n.na.much&!all.n.na.much]),collapse=" , "))
      if(any(var0&!all.n.na.much)) warning("in the following variables one value replaced all the others (at least in some groups)\n",paste(names(var0[var0&!all.n.na.much]),collapse=" , "))
      if(any(all.n.na.much)) warning("in the following variables NAs where kept because there wasn't even 1 value to replace the other NA values (at least in some groups)\n",paste(names(all.n.na.much[all.n.na.much]),collapse=" , "))
    }

    # Replace NA.values
    if(is.null(grouping)) {
      if(is.null(dim(x))) return(replace.na.inner(x,sd))
      if(!is.null(dim(x))) return(apply(x,2,function(y)replace.na.inner(y,sd)))
    } else if(!is.null(grouping)) {
      replace.na.inner.grouping <- function(x,grouping,sd) {
        for(i in unique(grouping))  x[grouping==i] <- replace.na.inner( x[grouping==i], sd)
        return(x) }
      if(is.null(dim(x))) return(replace.na.inner.grouping(x,grouping,sd))
      if(!is.null(dim(x))) return(apply(x,2,function(y)replace.na.inner.grouping(y,grouping,sd)))
    }
  } else return(x)
}

#' Replace values with other values.
#' @export
#' @author Daniel Hoop
#' @param search The vector containing values that will be searched.
#' @param replace The vector containing values that will be inserted, instead of those, given in `search`.
#' Each vector place in `search` must correspond to the same vector place in `replace`.
#' @param x The vector to do the replacement in.
#' @param no.match.NA Logical value indicating if elements in `x` that did not match any element in `search` should either stay as they were in `x` (`no.match.NA = FALSE`),
#' or should be set to `NA` instead (`no.match.NA = TRUE`).
#' @param gsub Logical value indicating if partial strings should be searched, using the function \code{\link[base:gsub]{base::gsub}}. If `FALSE`, then only full string matches will be replaced.
#' @param fixed If `gsub = TRUE`, then the characters given in `search` will be interpreted as regular expressions. If you want to avoid this, set `fixed = TRUE`.
#' @seealso \code{\link[grep:base]{grep::base}}
#' @examples
#' search <-  c( 1,  2,  9)
#' replace <- c("a","b","c")
#' x <- c(NA, 0, 1, 1, 2, 3)
#' replace.values(search, replace, x)
#' replace.values(search, replace, x, no.match.NA = TRUE)
#' replace.values(c("a", "b"), c("1", "2"), c("XX_a_XX", "YY_b_YY"), gsub = TRUE)
replace.values <- function(search, replace, x, no.match.NA=FALSE, gsub=FALSE, fixed=FALSE){
  # This function replaces all elements in argument search with the corresponding elements in argument replace.
  # Replacement is done in x.
  # if no.match.NA=TRUE, values that could not be replaced will be NA instead of the old value.

  if(length(search)!=length(replace)) stop("length(search) must be equal length(replace)")
  if(any(duplicated(search))) stop("There must be no duplicated entries in search.")

  if(is.matrix(x)){
    return(apply(x,2,function(x)replace.values(search=search, replace=replace, x=x, no.match.NA=no.match.NA, gsub=gsub, fixed=fixed)))
  } else if(is.data.frame(x)){
    return(as.data.frame(lapply(x,function(x)replace.values(search=search, replace=replace, x=x, no.match.NA=no.match.NA, gsub=gsub, fixed=fixed)),stringsAsFactors=FALSE))
  }

  if (is.factor(search) || is.factor(replace))
    stop ("arguments 'search' and 'replace' must not contain factors.")
  isFac <- is.factor(x)
  if (isFac) {
    lvl <- levels(x)
    x <- as.character(x)
  }

  xnew <- x
  if(!gsub) {
    m1 <- match(x, search)
    xnew <- replace[ m1 ]
    if(!no.match.NA){
      isna <- is.na(m1)
      xnew[isna] <- x[isna]
    }
  } else {
    # Hier erst nach Laenge der Strings ordnen, damit lange Teilstrings vor kurzen ersetzt werden.
    ord <- order(nchar(search),decreasing=TRUE)
    search <- search[ord]
    replace <- replace[ord]
    for(i in 1:length(search)){
      xnew <- gsub(search[i],replace[i],xnew, fixed=fixed)
    }
  }
  if (isFac) {
    xnew <- factor(xnew, levels=lvl)
  }
  return(xnew)
}

#' @title Compares the strings in argument "original" and "distorted".
#' @description It replaces all occurences in `distorted` with matches in `orgininal` if the relative distance is smaller than `max.rel.dist`.
#' @keywords internal
#' @author Daniel Hoop
#' If `no.match.NA == TRUE`, then no matches will be set to `NA` in the output vector. Otherwise no matches will be the same as given in `distorted`.
#' @param ... further arguments to pass into \code{\link[stringist:stringdistmatrix]{stringist::stringdistmatrix}}
#' @examples
#' recover.distorted.string(
#'   original = c("Aadorf", "Schüpfen", "Zürich", "Argau", "Lausanne"),
#'   distorted = c("Adorf", "Sch?pfen", "Bern"), no.match.NA = TRUE)
recover.distorted.string <- function(original, distorted, max.rel.dist=0.7, no.match.NA=TRUE, ...){

  require.package("stringdist")
  distmat <- stringdist::stringdistmatrix(original, distorted)
  corrected <- if(no.match.NA) rep(NA_character_, length(distorted)) else distorted

  for(i in 1:ncol(distmat)){ # i <- 1
    d1 <- distmat[,i] / nchar(distorted[i])
    whichMin <- which(d1==min(d1) & d1<1)
    if(length(whichMin)>0){
      if(length(whichMin)>1) {
        warning(paste0("Several matches found for ", distorted[i], ": ", paste0(original[whichMin],collapse=", "), ". First was taken."), immediate.=TRUE)
        whichMin <- whichMin[1]
      }
      corrected[i] <- original[whichMin]
    }
  }
  return(corrected)
}

#### CSV/RData/XLS - READ/WRITE & MANIPULATION ####

#cost <- load2("C:/Users/U80823148/_/ME/ME_data_out/data_final/allcosts_info.RData")
#file <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/hpda/R/Output/Test_write.table.fast/cost.csv")
#system.time( wr1ite.table(cost[1:1000,],file) ); system.time( utils::wr1ite.table(cost[1:1000,],file) )

#' @title Writes tables much faster compared to \code{\link[utils:write.table]{utils::write.table}} if they should be written onto network drives.
#' @description If a network drive is detected and the file will be larger than approx. 300kb, it first creates a temporary file on the local hard drive. Then it moves the file from local to network drive.\cr
#' Furthermore, it used \code{"ISO-8859-1"} encoding, when data is being written to Agroscope OS-LW.
#' @export
#' @author Daniel Hoop
#' @inheritDotParams utils::write.table
#' @inheritParams utils::write.table
write.table <- function(x, file, fileEncoding="", ...) {

  if (fileEncoding == "" &&
      is.character(file) &&
      grepl(".+mnt/(agroscope|Data-Work-RE).*", file[1]) &&
      .agsMachineType(rstudioserver=1)
  ){
    fileEncoding <- "ISO-8859-1"
  }

  if( ! .isNecessaryToUseTmpDir(x, file)  ){
    utils::write.table(x=x, file=file, fileEncoding=fileEncoding, ...)
  } else {
    utils::write.table(x=1, file=file, fileEncoding=fileEncoding, ...) # First try to write a file. If not possible (e.g. because directory does not exist) this will return a error message.
    suppressWarnings(file.remove(file))
    tryCatch({
      folder <- paste0(tempdir(),"/RFastWrite/") # paste0(Sys.getenv("TMP"), "\\R\\RFastWrite\\")
      dir.create(folder, recursive=TRUE, showWarnings=FALSE)
      file.remove(list.files(folder,full.names=TRUE))
      file2 <- paste0(folder, paste0(sample(letters,4,replace=TRUE),collapse=""), ".csv" )
      utils::write.table(x=x, file=file2, fileEncoding=fileEncoding, ...)
      file.copy(file2, file, overwrite=TRUE)
      suppressWarnings( file.remove(list.files(folder,full.names=TRUE)) )

      unlink(folder)
    }, error = function(e) {
      stop(paste0(e$message, "\nwrite.table() has encountered an error. This is not the original write.table() function but an edited version by Daniel Hoop. It was optimized to save files on network drives.\nTry utils::write.table() to use the default function."))
    })
  }
}

# Performance-Vergleich zwischen eigener zip-Loesung und csv.gz Loesung. Ist gleich schnell.
# Es scheint ausserdem, dass das Einlesen von csv File von Laufwerk gleich schnell ist wie das Einlesen & Entpacken von zip file von Laufwerk.
# Beide Male 10 Sekunden f?r AGIS-Daten 2015.
if(FALSE){
  dir.create("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\", showWarnings=FALSE, recursive=TRUE)
  df <- as.data.frame( matrix(1:1000000, ncol=100) )
  system.time(for(i in 1:1){ # Performanc with buil-tin function. 20 sec
    write.table(df, file=gzfile("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename.csv.gz"), sep=";", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
    dat <- utils::read.table(gzfile("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename.csv.gz"), sep=";", header=TRUE)
  })
  system.time(for(i in 1:1){ # Performance with own function. 25 sec
    write.table.zipped(df, file="C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename2.zip", sep=";", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
    dat2 <- read.table("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename2.zip", sep=";", header=TRUE)
  })
}


#' @title Gives the answer to the question if it is necessary to write x to a temporary dir and move it afterwards.
#' @description Function is e.g. necessary for write.table.zipped & write.table (fast version)
#' @keywords internal
#' @author Daniel Hoop
#' @param x The object to be written to disk. E.g. a matrix or data.frame.
#' @param file The file to which the object should be written.
#' @param sizeThreshold The threshold of the object.size(x) in bits. If the object is greater than that amount & file is on network drive, temp dir will be used first.
.isNecessaryToUseTmpDir <- function(x, file, sizeThreshold=1400000) {
  if (length(file) == 0 || !is.character(file))
    return (FALSE)

  file <- getFullyQualifiedFileName(file[1])

  # OK: "C:/", "/home/", but not: "\\evdad." -> dirname is necessary to transform leading //evdad into \\evdad
  ok <- grepl("^[CD]:|^/", dirname(file), ignore.case = TRUE)
  notOk <- grepl("/mnt/|^http", file, ignore.case = TRUE)

  return ((!ok || notOk) && object.size(x) > sizeThreshold)
}

#' Create a zip file that contains a talbe written by `write.table`.
#' @export
#' @author Daniel Hoop
#' @param x data.frame to be saved
#' @param file file path and name
#' @param RtoolsBin The directory called "bin" inside the installation location of Rtools.
#' @inheritParams utils::write.table
#' @details
#' An alternative would be:\cr
#' `write.table(x, file=gzfile("filename.csv.gz")))`\cr
#' Combined with
#'  `read.table(gzfile("filename.csv.gz"))`
write.table.zipped <- function(x, file, RtoolsBin=NULL, ...){

  if(!is.character(file)) stop("file must be a character. Not a connection.")

  # Get filename and folder
  fil <- strsplit(file, "/|\\\\")[[1]]
  filename <- fil[length(fil)]
  parentdir <- ifelse(length(fil)==1, "", paste0( paste(fil[-length(fil)],collapse="/"), "/"))
  # Remove ending of filename. This is necessary to store csv and zip correctly. Speical paste0(,collapse=".") in case the filename contains several dots.
  fileNameSplitted <- strsplit(filename,"\\.")[[1]]
  if(length(fileNameSplitted)>1) {
    filenameWithoutEnding <- paste0( fileNameSplitted[-length(fileNameSplitted)], collapse=".")
    ending <- fileNameSplitted[length(fileNameSplitted)]
  } else {
    filenameWithoutEnding <- fileNameSplitted
    ending <- ""
  }
  if(ending=="zip") stop("Please use the ending of the data file (e.g. csv), not zip!")

  # Create temporary file
  if(.isNecessaryToUseTmpDir(x, file)){
    folder <- paste0(tempdir(), "/Rzipped/")
    dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  } else {
    folder <- paste0(parentdir,"/")
  }

  tmpCSVfile <- paste0(if(folder!="/") folder, filename )
  write.table(x, tmpCSVfile, ...)

  # Create zip file containing csv. Remove temporary folder.
  if(is.null(RtoolsBin)) {
    zip.nodirs(zipfile=paste0(parentdir,filenameWithoutEnding, ".zip"), files=tmpCSVfile, remove.original=TRUE, showWarnings=FALSE, invoked.internally=TRUE)
  } else {
    zip.nodirs(zipfile=paste0(parentdir,filenameWithoutEnding, ".zip"), files=tmpCSVfile, remove.original=TRUE, showWarnings=FALSE, invoked.internally=TRUE, RtoolsBin=RtoolsBin)
  }
  unlink(folder)
}

#' Like \code{\link[base:source]{base::source}}, but will detect if a file located on Agroscope OS-LW is sourced, and will adapt encoding depending on BIT client / RStudio server.
#' @export
#' @author Daniel Hoop
#' @inheritParams base::source
#' @seealso \code{\link[readr:guess_encoding]{readr::guess_encoding}}
source <- function (file, encoding=getOption("encoding"), ...) {
  # Change file encoding in case funciton is called from RStudio Server of Agroscope.
  if (encoding == "native.enc" &&
      is.character(file) &&
      grepl(".+mnt/(agroscope|Data-Work-RE).*", file[1]) &&
      .agsMachineType(rstudioserver=1)
  ){
    encoding <- "ISO-8859-1"
  }
  base::source(file=file, encoding=encoding, ...)
}

if (FALSE) {
  source_PROBABLY_DELETE <- function (file, encoding = getOption("encoding"), ...) {
    # Change file encoding in case funciton is called from RStudio Server of Agroscope.
    onRstudioServerAndShouldSetIsoEncoding <-
      (encoding == "native.enc" &&
         is.character(file) &&
         grepl(".+mnt/(agroscope|ZAMAIN).*", file[1]) &&
         .agsMachineType(rstudioserver=1))
    # Use 'readLines', then look for the character "Ã" as it indicates wrong encoding
    if (onRstudioServerAndShouldSetIsoEncoding) {
      newEncoding <- "ISO-8859-1"
      txt <- base::readLines(file, encoding = newEncoding)
      if (length(grep("\\x", txt)) == 0) {
        encoding = newEncoding
      }
    }
    # Now source
    base::source(file=file, encoding=encoding, ...)
  }
}

#df=as.data.frame(matrix(1:12,ncol=3)); colnames(df) <- c(letters[1:3]); write.table(df,"table.csv",sep=";",col.names=TRUE, row.names=FALSE); read.table("table.csv", header=TRUE, sep=";"); read.table("table.csv", header=TRUE, sep=";", choose.columns=c("b")); unlink("table.csv")

#' @title This edited versin of \code{\link[utils:read.table]{utils::read.table}} detects compressed files from their file endings and directly reads them without unpacking.
#' @description It is assumed that only one file is within the zip archive. Otherwise the function will give an error message.\cr
#' Furthermore, it used \code{"ISO-8859-1"} encoding, when data is being read from Agroscope OS-LW.
#' @export
#' @author Daniel Hoop
#' @inheritParams utils::read.table
#' @inheritDotParams utils::read.table
#' @param chooseColumns `character`, `numeric` or `logical` indicating which columns of the file should be read in. This saves RAM compared to dropping them afterwards. No `NA` values allowed.
#' @param RtoolsBin If `!is.null(chooseColumns)`, then Rtools must be installed because the `RtoolsBin/cut.exe` and some dll libraries are needed.
#' The argument `RtoolsBin` has to indicate the folder in which the Rtools binaries are located. Something like `"C:/Program Files/Rtools/bin"`.
#' @param keepOriginalColnames Logical value indicating if the original colnames should be kept and special characters should not be replaced by dots '.' which whould be the standard behaviour of the \code{\link[utils:read.table]{utils::read.table}} function.
#' @param chooseColumns may be implemented with `scan(file, what=list(Name="", Age=0, ...), sep=";", quiet=TRUE)`. Try it!
#' @seealso \code{\link[utils:read.table]{utils::read.table}}
#' @references \href{https://www.datacamp.com/community/tutorials/importing-data-r-part-two}{https://www.datacamp.com/community/tutorials/importing-data-r-part-two}
read.table <- function(file, header=FALSE, sep="", fileEncoding="", ..., keepOriginalColnames=FALSE, chooseColumns=NULL, RtoolsBin=agsPath(paste0(c("C:/Program Files","C:/Tools","//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000/driversEtc","//evdad.admin.ch/AGROSCOPE_OS/0/4/5/3811/Dokumente/R_Uebung/sonstigeFiles/Rtools/bin"),"/Rtools/bin"))){

  # Change file encoding in case funciton is called from RStudio Server of Agroscope.
  if (fileEncoding == "" &&
      is.character(file) &&
      grepl(".+mnt/(agroscope|Data-Work-RE).*", file[1]) &&
      .agsMachineType(rstudioserver=1)
  ){
    fileEncoding <- "ISO-8859-1"
  }

  # If it is not a compressed file, then use the normal read.table() function.
  # If the file does not exist, try to read with read.table() so you will get exact the same error message.
  if("connection"%in%class(file) || class(file)!="character" || !grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", file) || !file.exists(file) || !is.null(chooseColumns) ) {
    # Simple reading in without choosing columns
    if(class(file)[1]!="file" || !file.exists(file) || is.null(chooseColumns)){
      dat <- utils::read.table(file=file, header=header, sep=sep, fileEncoding=fileEncoding, ...)
      if (header && keepOriginalColnames && any(grepl("\\.", colnames(dat)))) {
        args <- list(...)
        args <- args[!names(args) %in% c("nrows", "stringsAsFactors")]
        cn <- as.character(do.call("read.table", c(list(file = file, header = FALSE, sep = sep, fileEncoding = fileEncoding, nrows = 1, stringsAsFactors = FALSE), args)))
        colnames(dat) <- cn
      }
      return(dat)
      # Special reading in when certain columns are chosen...
    } else {
      RtoolsBin <- RtoolsBin[file.exists(RtoolsBin)][1] # Choose a RtoolsBin that exists. If none exists, then its NA.
      if(is.na(RtoolsBin)) stop(paste0("If !is.null(chooseColumns), then the R toolset and Cygwin DLLs of Rtools must be available. The Rtools binaries folder was not found at location: ", RtoolsBin))
      RtoolsCut <- paste0(RtoolsBin, "/cut.exe")
      if(is.character(chooseColumns)){
        colNames <- unname(unlist(read.table(file, sep=sep, nrow=1, header=FALSE, stringsAsFactors=FALSE)))
        chooseColumns <- which(colNames%in%chooseColumns)
      }
      if(is.logical(chooseColumns)) chooseColumns <- which(chooseColumns)
      if(any(is.na(chooseColumns))) stop("No NA-values allowed in chooseColumns.")
      return( utils::read.table(pipe(paste0(RtoolsCut, " -f",paste0(chooseColumns,collapse=","), " -d", sep," ", getFullyQualifiedFileName(file)) ), header=header, sep=sep, fileEncoding=fileEncoding, ...))
    }

    # In case of archive ...
  } else {
    tryCatch({
      # List files within the archive and read directly from archive without unpacking.
      withinFile <- unzip(file, list=TRUE)[,"Name"]
      if(length(withinFile)>1) stop("Only 1 file inside archive is allowed. Otherwise try utils::read.table(unz(pathOfZipFile, nameOfCSVwithinZipFile)).")
      return( utils::read.table(unz(file, withinFile), header=header, sep=sep, fileEncoding=fileEncoding, ...) )
    }, error = function(e) {
      stop(paste0(e$message, "\nread.table() has encountered an error. This is not the original read.table() function but an edited version by Daniel Hoop in order to read data from compressed files without unpacking them.\nTry utils::read.table() to use the default function."))
    })
  }
}

# file <- "C:/Users/U80823148/_/ME/ME_data_out/data_final/2014/allcosts_info.zip"
if(FALSE) read.table_OLD_DELETE <- function(file, ..., choose.columns=NULL){
  # This edited versin of read.table detects compressed files from their file endings and directly reads them without unpacking.
  # It is assumed that only one file is within the zip archive. Otherwise the function will give an error message.
  # All arguments are used like in read.table.

  # If it is not a compressed file, then use the normal read.table() function.
  # If the file does not exist, try to read with read.table() so you will get exact the same error message.
  if(class(file)!="character" || !grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", file) || !file.exists(file) ) {
    return( utils::read.table(file, ...) )
  } else {
    tryCatch({
      # List files within the archive and read directly from archive without unpacking.
      withinFile <- unzip(file, list=TRUE)[,"Name"]
      if(length(withinFile)>1) stop("Only 1 file inside archive is allowed. Otherwise try utils::read.table(unz(pathOfZIPFile, nameOfCSVwithinZipFile)).")
      return( utils::read.table(unz(file, withinFile), ...) )
    }, error = function(e) {
      stop(paste0(e$message, "\nread.table() has encountered an error. This is not the original read.table() function but an edited version by Daniel Hoop in order to read data from compressed files without unpacking them.\nTry utils::read.table() to use the default function."))
    })
  }
}

read.table2 <- function(file, header=TRUE, sep=";", fileEncoding=""){
  # This read.table() function works in some special cases of wrongly encoded data.

  dat <- readLines(file, encoding=fileEncoding)
  dat <- do.call("rbind", strsplit(dat,sep))
  if(header) {
    cn1 <- dat[1,]
    dat <- dat[-1,]
    colnames(dat) <- cn1
  }
  dat <- char.cols.to.num(dat)
  return(dat)
}

# file <- "C:/Users/U80823148/_/Data/testfile.csv"
# write.table(1:(1e7+351981), file=file, col.names=FALSE, row.names=FALSE); system.time( print(c1ount.lines(file)) ); file.remove(file)
count.lines <- function(file) {
  # count lines of file.
  if( grepl("window",Sys.info()["sysname"],ignore.case=TRUE) ){
    file <- gsub("/","\\\\",file) # Replace all forward-slashes with backward-slashes
    file <- gsub("(\\\\){2,10}","\\\\",file) # Replace all multiple slashes with unique slashes.
    str <- paste0(system(paste0("find /c /v \"\" \"",file,"\" "), intern=TRUE), collapse="") # Looks for "nonblank" lines but actually also "blank" lines contain "\n" and are therefore counted.
    str <- unlist(strsplit(str, ":"))
    return(as.numeric(str[length(str)]))
  } else if( grepl("linux",Sys.info()["sysname"],ignore.case=TRUE) ){
    file <- gsub("\\\\","/",file) # Replace all backward-slashes with forward-slashes
    file <- gsub("(/){2,10}","/",file) # Replace all multiple slashes with unique slashes.
    str <- paste0(system(paste0("wc -l \"",file,"\""), intern=TRUE), collapse="")
    stop("Linux version not yet fully implemented.")
  }
}

#' Creates a CSV file from a data.frame/matrix and opens it with the default CSV-opening-program of the computer.
#' @export
#' @author Daniel Hoop
#' @param x data.frame/matrix
#' @param names Dimension names to be saved in the file. \code{"col"} for colnames, \code{"rowcol"} for rownames & colnames, \code{"row"} for rownames, and \code{"no"} for no dimnames.
#' @param nrows Maximum number of rows to be saved (for higher speed with large datasets). If \code{n = -1}, all rows will be displayed. See also the help for \code{\link[utils:read.table]{utils::read.table}}
#' @param ncols Maximum number of columns to be saved (for higher speed with large datasets).
#' @param folder Directory, where the temporary file should be saved. If \code{NULL}, then a temporary file will be created using \code{\link[base:tempdir]{base::tempdir}}.
#' @param filename Name of the file. If \code{NULL}, then a generic filename will be given.
#' @param quote Should quotes be written into the csv File? -> See also the help for \code{\link[utils:write.table]{utils::write.table}}.
#' @param na How should NA values be displayed in the csv File? -> See also the help for \code{\link[utils:write.table]{utils::write.table}}.
#' @param openFolder Should the folder with all temporary files be opened after having created the file?
#' @param title The title to show when \code{\link[utils:View]{utils::View}} is called on the RStudio server.
#' @param sep The separator. If \code{"EXCEL"}, then a xlsx file is created and opened.
#' @inheritParams utils::read.table
#' @examples
#' x <- matrix(1:10, ncol = 2)
#' view(x)
view <- function(x, names=c("col","rowcol","row","no"), nrows=-1, ncols=-1, fastViewOfSubset=TRUE, folder=NULL, filename=NULL, quote=FALSE, na="NA", sep="EXCEL", decimal.mark=".", openFolder=FALSE, title = NULL, ...){

  names <- match.arg(names)

  if ("table" %in% class(x)) {
    if (length(dim(x)) == 2) {
      class(x) <- "matrix"
    } else {
      message('Object of class "table" was converted to a matrix.')
      x <- as.matrix(x)
    }
  }

  # If it is only a vector, create matrix and save as text file.
  txtFile <- FALSE
  if (is.null(dim(x))) {
    if (length(x)==1)
      txtFile <- TRUE
    x <- as.matrix(x)
    # If it is an array with 3 dimensions, create a matrix with 2 dimensions.
  } else if (length(dim(x)) > 2) {
    if (length(dim(x)) == 3) {
      x <- dimnames.to.mat(dim3.to.mat(x, keep.colnames = TRUE), dims = names)
      names <- "no"
    } else {
      stop("Arrays with more than 3 dimensions cannot be processed.")
    }
  }
  # If it is a list, create a matrix.
  if (is.list(x) && !is.data.frame(x)) {
    message("Trying to process a list as input (`x`).")
    x <- c.matrices(x, integrate.dimnames = "rowcol", nbreak = 1, fill = NA)
    names <- "no"
  }
  if (is.null(colnames(x)))
    colnames(x) <- paste0("V",1:ncol(x))

  # Replace NaN by NA, because otherwise the argument na=... in write.table() will not work.
  if (!na%in%c("NA","NaN")){
    naReplace <- function(x){ x[is.nan(x)] <- NA ;x }
    if (is.matrix(x)) {
      x <- apply(x,2,naReplace)
    } else {
      dn1 <- dimnames(x)
      x <- as.data.frame(lapply(x, naReplace), stringsAsFactors=FALSE)
      dimnames(x) <- dn1
    }
  }

  # If on RStudio server open the view with the actual name of the data frame.
  if (.agsMachineType(rstudioserver=1) && is.null(folder) && is.null(filename)) {
    objName <- paste0(as.character(substitute(x)), collapse = "")
    objName <- gsub("[^a-zA-Z0-9._]", "", objName)
    View(x, title = if (is.null(title)) objName else title)
    return (invisible(NULL))
  }

  if (sep == "EXCEL")
    installFromCRAN("openxlsx")

  if(txtFile)
    names <- "no"
  names <- match.arg(names)

  # Check if data.frame should be shrinked for faster view
  if(nrows<0 && ncols<0){
    maxSize <- 40000000 # On Intel i7-4770 CPU with 3.40 GHz, approx 12 seconds are necessary to save file as csv and open in Excel.
    objSize <- object.size(x)
    if(fastViewOfSubset && objSize>maxSize){
      nrows <- (floor(nrow(x)*maxSize/objSize))
    }
  }
  if(nrows<0) nrows <- nrow(x)
  if(ncols<0) ncols <- ncol(x)

  # Shrink data.frame such that it can be saved & viewed faster.
  nrows <- min(nrow(x), nrows); if(nrows<nrow(x)) warning(paste0("data.frame displays only ",nrows," of ",nrow(x)," rows. Either change nrows or set fastViewOfSubset=FALSE."))
  if(nrows!=nrow(x)) x <- x[1:nrows,,drop=FALSE]
  ncols <- min(ncol(x), ncols); if(ncols<ncol(x)) warning(paste0("data.frame displays only ",ncols," of ",ncol(x)," cols. Change ncols to view the full data.frame."))
  if(ncols!=ncol(x)) x <- x[,1:ncols,drop=FALSE]

  # Define paths
  # If is.null(folder), wird ein temporaerer Ordner im Windows-Dateisystem angelegt.
  if(is.null(folder)) {
    folder <- paste0(Sys.getenv("TMP"), "/R/Rview/") #   paste0(Sys.getenv("TMP"), "/R/Rview")     paste0(tempdir(),"/Rview")
    dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  }

  # Wenn am Schluss des Pfades kein "/" angefuegt wurde, wird dies gemacht:
  if( !substr(folder,nchar(folder),nchar(folder))%in%c("/","\\") ) {
    folder <- paste0(folder, "/")
  }

  if (is.null(filename)) {
    pfad0 <- folder
    name <- "Rview_tmp"
    nr <- "01"
    csv <- ifelse(txtFile, ".txt",
                  if (sep == "EXCEL") ".xlsx" else ".csv")

    # Check if there are existing files in the folder
    fil <- list.files(pfad0)
    fil <- fil[ substr(fil,nchar(fil)-3,nchar(fil))==csv ]
    # If there are no files in the folder, use the default save path.
    if(length(fil)==0){
      pfad1 <- paste0(pfad0, name, nr, csv)
    } else {
      # Remove all files in the folder (if possible)
      fil <- paste0(pfad0, fil)
      # Only remove old txt files. Because they are not protected from being deleted.
      if(txtFile) fil <- fil[  difftime(Sys.time(), file.info(c(fil))$mtime, units="hours")>1  ]
      suppressWarnings( try( file.remove( fil )  , silent=TRUE) )
      fil <- list.files(pfad0)
      fil <- fil[ substr(fil,nchar(fil)-3,nchar(fil))==csv ]
      # If there are no files anymore use the default save path.
      if( length(fil)==0 ) {
        pfad1 <- paste0(pfad0, name, nr, csv)
      } else {
        # If there are sill files, read out the number of the newest file (with the highest number)
        mx <- max( as.numeric( substr(fil,nchar(fil)-5,nchar(fil)-4) ) )
        # Add 1 to the number of the file
        mxpl1 <- as.character( mx+1 )
        if(nchar(mxpl1)==1) mxpl1 <- paste0("0",mxpl1)
        # Create a new path
        pfad1 <- paste0(pfad0, name, mxpl1, csv)
      }
    }
  } else {
    pfad1 <- paste0(folder, filename)
  }

  # Punkt zu Komma aendern falls gewuenscht.
  if (decimal.mark != ".") {
    if (is.matrix(x)) {
      x[] <- apply(x, 2, function(y){
        y <- trimws(format(y, decimal.mark=decimal.mark))
        y[y %in% c("NA","NaN")] <- NA
        return(y)
      })
    } else if (is.data.frame(x)) {
      x[] <- lapply(x, function(y){
        y <- trimws(format(y, decimal.mark=decimal.mark))
        y[y %in% c("NA","NaN")] <- NA
        return(y)
      })
    } else warning("decimal.mark can only be other an '.' if x is a matrix or data.frame.")
  }

  # Rownames und colnames, die mit +, - oder = anfangen, mit ' am Anfang versehen, dass es von Excel richtig dargestellt wird
  rn1 <- rownames(x)
  cn1 <- colnames(x)
  ind <- substr(rn1,1,1)%in%c("+","-","=")
  if(any(ind)) rownames(x)[ind] <- paste0(" ",rn1[ind])
  ind <- substr(cn1,1,1)%in%c("+","-","=")
  if(any(ind)) colnames(x)[ind] <- paste0(" ",cn1[ind])

  # Write CSV file & open.
  if(names=="row") {
    if (sep == "EXCEL") {
      openxlsx::write.xlsx(x, pfad1, colNames = FALSE, rowNames = TRUE)
    } else {
      # If the first cell of the file is named "ID" Microsoft Excel warns that a SYLK file is opened. Therefore it is renamed.
      if(!is.null(rownames(x)[1]) && !is.na(rownames(x)[1])) if(substr(rownames(x)[1],1,2)=="ID") rownames(x)[1] <- paste0("lD", substring(rownames(x)[1],3,nchar(rownames(x)[1])))
      write.table(x, file=pfad1, sep=sep, col.names=FALSE, row.names=TRUE, quote=quote, na=na, ...)
    }
  } else if (names=="col") {
    if (sep == "EXCEL") {
      openxlsx::write.xlsx(x, pfad1, colNames = TRUE, rowNames = FALSE)
    } else {
      # If the first cell of the file is named "ID" Microsoft Excel warns that a SYLK file is opened. Therefore it is renamed.
      if(!is.null(colnames(x)[1]) && !is.na(colnames(x)[1])) if(substr(colnames(x)[1],1,2)=="ID") colnames(x)[1] <- paste0("lD", substring(colnames(x)[1],3,nchar(colnames(x)[1])))
      write.table(x, file=pfad1, sep=sep, col.names=TRUE, row.names=FALSE, quote=quote, na=na, ...)
    }
  } else if (names=="rowcol") {
    if (sep == "EXCEL") {
      openxlsx::write.xlsx(x, pfad1, colNames = TRUE, rowNames = TRUE)
    } else {
      write.table(x, file=pfad1, sep=sep, col.names=NA, quote=quote, na=na, ...)
    }
  } else {
    if (sep == "EXCEL") {
      openxlsx::write.xlsx(x, pfad1, colNames = FALSE, rowNames = FALSE)
    } else {
      write.table(x, file=pfad1, sep=sep, col.names=FALSE, row.names=FALSE, quote=quote, na=na, ...)
    }
  }

  # If on RStudio server open the view with the actual name of the data frame.
  # Therefore use parse(...)
  if (.agsMachineType(rstudioserver=1)) {
    message("The data was saved, but cannot be displayed in Excel when working on the RStudio Server.\n", pfad1)
    objName <- paste0(as.character(substitute(x)), collapse = "")
    objName <- gsub("[^a-zA-Z0-9._]", "", objName)
    View(x, title = objName)
    return (invisible(NULL))
  }

  # Use 'shell.exec' instead of browseURL (because it does not work anymore)
  shell.exec(zaUtils::winSlashes(pfad1))
  if(openFolder) {
    Sys.sleep(1)
    shell.exec(zaUtils::winSlashes(folder))
  }
}

view.folder <- function() {
  browseURL(paste0(tempdir(),"/Rview/"))# (paste0(Sys.getenv("TMP"), "\\R\\Rview"))
}

#' @title Load an object from an RData file and return it.
#' @description Will only work, if the file contains only 1 object.
#' @export
#' @author Daniel Hoop
#' @inheritParams base::load
#' @param getObjName Logical value indicating if the object name should also be returned.
#' @return If \code{getObjName = FALSE}, the loaded object. Else, a list with two entries "object" (the loaded object), and "name" (the name of the object).
load2 <- function (file, getObjName = FALSE) {
  if (length(file) != 1)
    stop ("`file` must be a character vector of length 1 or a connection.")
  if ("character" %in% class(file)) {
    if (grepl("\\.rds", file, ignore.case = TRUE)) {
      return (readRDS(file))
    }
  }
  load(file)
  ret <- ls()[!ls() %in% c("file", "getObjName")]
  if(length(ret)>1)
    stop(paste0("More than one object was loaded. The function only works with one object.\n",paste0(ret, collapse=", ")) )
  if (getObjName) {
    res <- eval(parse(text=paste0("list(object = ", ret, ", name = \"", ret, "\")")))
  } else {
    res <- eval(parse(text=ret))
  }
  return(res)
}

format.colnames <- function(x) {
  # This function converts all characters to lower case if they don't look like P100..., T100... or K100...
  # Argument x can be a vector of characters or a matrix/data.frame.
  # The converted character vector is returned.

  if(!is.null(dim(x))) x <- colnames(x)
  filtMM <- which( grepl("^([PTK][0-9]{3})",x) )
  x[-filtMM] <- tolower(x[-filtMM]) # To lower for all which are NOT like P100
  x[filtMM] <- gsub("\\.","_",x[filtMM]) # Replace . with _ for all which ARE like P100
  return(x)
}

#' Creates pseudo size cols in P340 as GVE taken from P330.
#' @export
#' @author Daniel Hoop
#' @param data The data.frame to perform manipulations in.
#' @param sizeCol The column for the size in P340. If \code{sizeCol = "size"}, then, e.g. the following Merkmal would be build for the dairy enterprise: P340_size_11000
#' @return The manipulated data.frame.
#' @examples
#' spb <- load.spb()
#' spb <- create.spb.340.size(spb)
#' tail(colnames(spb))
create.spb.340.size <- function (data, sizeCol = "size") {

  if (length(sizeCol) != 1)
    stop("`length(sizeCol)` must be equal to 1.")
  if (nchar(sizeCol) != 4)
    warning("Better choose a `sizeCol` with 4 characters. Otherwise the functions MKrow() and MKcol() will not work as expected.")

  cn1 <- colnames(data)[ MKtab(data)=="P330" & MKrow(data)=="01000" & nMKcol(data)>2000 & nMKcol(data)<2101 ]

  # Nur durchfuehren, wenn die entsprechenden Spalten ueberhaupt vorhanden sind.
  if (length(cn1) > 0) {
    dataAdd <- data[, cn1, drop = FALSE]
    colnames(dataAdd) <- replace.values(
      cn1,
      paste0("P340_", sizeCol, "_", transl.spb.330col.340row(MKcol(cn1))),
      colnames(dataAdd))
    # Error check
    if (any(MKrow(dataAdd) == "NA")) {
      print(cn1[MKrow(dataAdd) == "NA"])
      stop("Not all columns from table 330 could be translated into rows from table 340. Problematic P330-columns are printed above.")
    }
    # Add 0 sizes that are not available in P330.
    allP340Rows <- sort(unique(MKrow(data)[MKtab(data) == "P340"]))
    missingRows <- allP340Rows[!allP340Rows %in% MKrow(dataAdd)]
    dataAdd[paste0("P340_", sizeCol, "_", missingRows)] <- 0
    dataAdd <- dataAdd[order(MKrow(dataAdd))]

    # Add sizes to data
    data <- cbind(data, dataAdd[!colnames(dataAdd) %in% colnames(data)])
  }

  return(data)
}

# filename <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/9/4278/hpda/R/func/gbTestColErsatz.R"; renameGbVariablesInScript(filename)
renameGbVariablesInScript <- function (filename) {
  if (!file.exists(filename))
    stop ("The file specified in 'filename' does not exist.")
  lines <- readLines(filename)
  repl <- matrix(c(
    "GVE_tot", "GVE_Tiere"
    ,"JAE_tot", "JAE_alle"
    ,"NAT_tot", "NAT_alle"
    ,"hh_AT_tot", "hh_AT_alle"
    ,"mfr_tot", "mfr_fluessigeMittel"
  ),ncol=2, byrow=TRUE)
  lines <- replace.values(repl[,1], repl[,2], lines, gsub=TRUE)
  lines <- replace.values("_tot", "", lines, gsub=TRUE)
  repl <- matrix(c(
    "GVE_Tiere", "GVE_tot"
    ,"JAE_alle", "JAE_tot"
    ,"NAT_alle", "NAT_tot"
    ,"hh_AT_alle", "hh_AT_tot"
    ,"Abschreibungenal", "Abschreibungen_total"
    ,"Schuldzinsenal", "Schuldzinsen_total"
  ),ncol=2, byrow=TRUE)
  lines <- replace.values(repl[,1], repl[,2], lines, gsub=TRUE)
  writeLines(lines, con=filename, sep="\n")
  message("Ersetzungen in File vorgenommen!")
}

#' Read conversion rules for Stichprobe Betriebsfuehrung
#' @author Daniel Hoop
#' @export
#' @param file The file containing the conversion rules
#' @param allowFileFormats The file formats that are allowed.\cr\code{"xlsx"}   Excel\cr\code{", sep"}   comma separated\cr\code{"; sep"}   semicolon separated.
#' @param dropInactive Logical value indicating if non-active "Merkmale" should be kicked from the table (this is strongly recommended, therefore, default is \code{TRUE}).
#' @param conversionDate The date when the conversion was exported or when it is applied. If specified, only these rules are kept that are valid at the time of conversion (using the colums 'von' and 'bis' in the conversion table).\cr
#' Use something like \code{as.Date(Sys.time())} or \code{as.Date("01.01.2022", format = "\%d.\%m.\%Y")} to create an object of class "Date". Default is \code{as.Date(paste0(substr(Sys.time(), 1, 4), "-01-01"))}.
#' @param expandRanges Logical value indicating if account ranges should be expaned (e.g. 311-313 -> 311,312,313)
#' @param accountSystemFile If \code{expandRanges == TRUE}, then the file containing the ZA account system has to be provided (usually something like 'za-accountsystem.xml').
#' @param verbose Logical value indicating if warnings should be displayed when some conversion rule ranges do not contain any existing accounts.
#' @details Both, \code{dropInactive} and \code{conversionDate} have to be specified!
readConversionSpb <- function(file, allowFileFormats = c("xlsx", ", sep", "; sep"),
                              dropInactive = TRUE, conversionDate = as.Date(paste0(substr(Sys.time(), 1, 4), "-01-01")),
                              expandRanges = FALSE, accountSystemFile = NULL, verbose = FALSE) {

  if (expandRanges && (is.null(accountSystemFile) || !file.exists(accountSystemFile)))
    stop("If `expandRanges` is TRUE, then `accountSystemFile` must be a valid path to a file.")

  if ("xlsx" %in% allowFileFormats)
    installFromCRAN("openxlsx")

  findDateConversionFunc <- function(x) {
    x1 <- x[!is.na(x)][1]
    # When all are NA, the date format is irrelevant.

    if (is.na(x1))
      return(function(x) as.Date(x, format = "%Y-%m-%d"))

    # Try different date formats.
    time1 <- NA
    if (is.character(x)) {
      if (all(is.na(time1))) {
        timeFormat <- "%d.%m.%Y %H:%M:%S"
        time1 <- as.Date(x1, format = timeFormat)
      }
      if (all(is.na(time1))) {
        timeFormat <- "%d.%m.%Y"
        time1 <- as.Date(x1, format = )
      }
      if (all(is.na(time1))) {
        timeFormat <- "%m/%d/%Y %H:%M:%S"
        time1 <- as.Date(x1, format = timeFormat)
      }
      if (all(is.na(time1))) {
        timeFormat <- "%m/%d/%Y"
        time1 <- as.Date(x1, format = timeFormat)
      }
    } else if (is.numeric(x)) {
      if (all(is.na(time1))) {
        timeFormat <- "1899-12-31"
        time1 <- as.Date(x1, origin = timeFormat)
      }
    }
    if(any(!is.na(x1) & is.na(time1))) {
      stop("Das Datumsformat in Spalten 'von', 'bis', 'geandert' konnte nicht erkannt werden.")
    }

    if (timeFormat == "1899-12-31")
      return(function(y) as.Date(y, origin = timeFormat))
       else return(function(y) as.Date(y, format = timeFormat))
  }

  # Read conversion
  konv <- NULL
  if ("; sep" %in% allowFileFormats) {
    tryCatch({
      konv <- utils::read.table(file, sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", comment.char="")
    }, warning = function(x) NULL
    , error = function(x) NULL)
  }
  if (", sep" %in% allowFileFormats && (is.null(konv) || ncol(konv) < 3)) {
    tryCatch({
      konv <- utils::read.table(file, sep=",", header=TRUE, stringsAsFactors=FALSE, quote = "\"", comment.char="")
    }, warning = function(x) NULL
    , error = function(x) NULL)
  }
  if ("xlsx" %in% allowFileFormats && (is.null(konv) || ncol(konv) < 3)) {
    tryCatch({
      konv <- openxlsx::read.xlsx(file, sheet = 1)
      colnames(konv) <- gsub("^", ".", colnames(konv), fixed = TRUE)
      colnames(konv) <- gsub("+", ".", colnames(konv), fixed = TRUE)
    }, error = function(x) NULL)
  }
  if (is.null(konv) || ncol(konv) < 3)
    stop("`file` must contain the path to a file formatted as ", paste0(allowFileFormats, collapse = "   OR   "))

  # "Ist+1" und "Aktiv" in boolean umwandeln.
  convertLogical <- function(x, columnName) {
    if (!is.logical(x)) {
      suX <- sort(unique(x))
      if (length(suX) == 2 && all(suX == c(-1, 0))) {
        x <- x == -1
      } else if (length(suX) == 2 && all(suX == c("FALSCH", "WAHR"))) {
        x <- x == "WAHR"
      } else {
        stop("The column '", columnName, "' has to be either logical or contain only '0' and '-1' values.")
      }
    }
    return(x)
  }
  konv[, "Ist..1"] <- convertLogical(konv[, "Ist..1"], columnName = "Ist..1")
  if (dropInactive) {
    if (!"Aktiv" %in% colnames(konv))
      stop("If `dropInactive` is TRUE, then the table containing the conversion rules must have a column named 'Aktiv'.")
    konv[, "Aktiv"] <- convertLogical(konv[, "Aktiv"], columnName = "Aktiv")
    konv <- konv[konv[, "Aktiv"], , drop = FALSE]
  }

  # Zeiten in Date-Format umwandeln und nur die Buchungen behalten, die fuer die aktuelle Zeit relevant sind.
  convertToDate <- findDateConversionFunc(konv[, "bis"])
  timeCols <- grepl("^von$|^bis$|^ge.ndert$", colnames(konv))
  konv[timeCols] <- lapply(konv[timeCols], convertToDate)
  rm(timeCols)

  filter <- rep(TRUE, nrow(konv))
  if (!is.null(conversionDate)) {
    if (class(conversionDate) != "Date")
      stop("`conversionDate` must be an object of class 'Date' (created using the function `as.Date`).")
    filter <- filter & naT(konv[,"von"] < conversionDate) & naT(conversionDate < konv[,"bis"])
  }
  konv <- konv[filter, , drop = FALSE]

  # NA auf ""
  # Nur fuer Spalten, die nicht genannt werden:
  charCols <- sapply(konv, class) == "character" # !grepl("^von$|^bis$|^ge.ndert$", colnames(konv))
  konv[charCols] <- lapply(konv[charCols], function(x){
    x[is.na(x)] <- ""
    return(x)
  })

  # Neue Spalten zu Merkmal erzeugen.
  if (!"Merkmal" %in% colnames(konv)) {
    konv[, "Merkmal"] <- sapply(strsplit(konv[,"Gruppe.."], " "), function(x) {
      if (length(x) == 0 || (length(x) == 1 && is.na(x))) "" else x[[1]]
    })
  } else {
    konv[, "Merkmal"] <- trimws(konv[, "Merkmal"])
    konv <- konv[grepl("^P[0-9]", konv[, "Merkmal"]), , drop = FALSE]
  }
  if (any(konv[, "Merkmal"] == ""))
    stop("In der Spalte 'Gruppe..' der Konversionsdatei muss an erster Stelle die Merkmals-UID stehen. Ebenfalls darf es kein Merkmal mit Wert '' geben.")

  konv[, "Konto"] <- paste0(konv[, "Konto.Haben"], konv[, "Konto.Soll"])
  konv[, "Merkmal_Konto"] <- paste0(konv[, "Merkmal"], konv[, "Konto"])

  if (expandRanges) {
    if (any(grepl("\\-|;", konv[, "KTR.Soll"])))
      stop("In Spalte 'KTR.Soll' der Konversionsregeln duerfen keine Bereiche mit '-' oder ';' angegeben werden.")
    if (any(grepl("\\-|;", konv[, "KTR.Haben"])))
      stop("In Spalte 'KTR.Haben' der Konversionsregeln duerfen keine Bereiche mit '-' oder ';' angegeben werden.")

    # Konversionregeln in Bereichen auf alle Konten ausdehnen.
    # Dafuer erst alle verfuegbaren Konten laden.
    installFromCRAN("XML")
    xml <- XML::xmlRoot(XML::xmlParse(accountSystemFile, encoding = "UTF-8"))
    accountIndex <- zaUtils::getXmlNodeIndexByNameAndAttr(xml, name="Table", attr="AccountZA")
    if (length(accountIndex) == 0)
      stop("`accountSystemFile` must contain a property named 'Table' with attribute 'AccountZA'.")
    acc <- XML::xmlToDataFrame(xml[[accountIndex]], stringsAsFactors=FALSE)
    all_acc <- as.numeric(acc[acc[, "IsAccount"] == "Wahr", "Accountnumber"])
    rm(xml, accountIndex, acc)

    # txt <- "31; 35;39; 300 - 365"
    # expandAccounts(txt = "31; 35;39; 300 - 365", allAccounts = all_acc)
    expandAccounts <- function(txt, allAccounts) {
      if (length(txt) == 0)
        return(txt)
      if (trimws(txt) == "")
        return(txt)
      txt <- trimws(strsplit(txt, ";")[[1]])
      unlist(lapply(txt, function(y) {
        x <- trimws(strsplit(y, "\\-")[[1]])
        if (length(x) == 1)
          return(x)
        if (length(x) != 2)
          stop("When splitting an account range (e.g. 300-365), more than 2 accounts were named.")
        all <- as.numeric(x[1]) : as.numeric(x[2])
        all <- all[all %in% allAccounts]
        if (length(all) == 0) {
          msg <- paste0("In the account range ", y, ", there are no valid accounts.")
          if (.keyValueStore$contains("expandAccountMessages")) {
            msgs <- .keyValueStore$get("expandAccountMessages")
            if (!msg %in% msgs) {
              .keyValueStore$setAndReturn("expandAccountMessages", c(msgs, msg))
              message(msg)
            }
          } else {
            .keyValueStore$setAndReturn("expandAccountMessages", msg)
            message(msg)
          }
          # message("In the account range ", y, ", there are no valid accounts.")
        }
        return(all)
      }))
    }

    # row = c(Konto.Soll = "", Konto.Haben = "31; 35;39; 300 - 365", KTR.Soll = "", KTR.Haben = "1111", Merkmal = "P320_4100_11110", `Ist+1` = "true")
    # accName = "Konto.Haben"
    expandRows <- function(row, accName) {
      # Recursion if matrix was given.
      if (!is.null(dim(row)))
        return(t(apply(row, 1, function(x){
          expandRows(row = x, accName = accName)
        })))
      # Actual function definition
      if (is.null(row) || trimws(row[accName]) == "")
        return(row)
      accs <- expandAccounts(row[accName], allAccounts = all_acc)
      if (length(accs) == 0)
        return(NULL)
      if (length(accs) > 1) {
        row <- t(as.matrix(row))[rep(1, length(accs)), ]
        row[, accName] <- accs
      }
      return(row)
    }

    colClasses <- sapply(konv, function(x)class(x)[1])
    names(colClasses) <- colnames(konv)

    konv <- do.call("rbind", apply(konv, 1, function(x) {
      x <- expandRows(row = x, accName = "Konto.Haben")
      x <- expandRows(row = x, accName = "Konto.Soll")
      return(x)
    }))

    if (length(colClasses) != ncol(konv))
      stop("Internal error: length(colClasses) != ncol(konv)")
    konv <- as.data.frame(konv, stringsAsFactors = FALSE)
    for (colName in names(colClasses)) {
      tmp <- konv[,colName]
      class(tmp) <- colClasses[colName]
      konv[colName] <- tmp
    }

    .keyValueStore$remove("expandAccountMessages")
  }

  return(konv)
}

#' Read ZA data from 'raw' CSV file downloaded from 'Erhebungsbogen'.
#' @param file The file containing the data as CSV. It looks like this:\cr\cr
#' \code{betrieb;jahr;merkmal;wert;index;schnittstelle;text;}
#' \code{1010431;2020;P100_1100_03200;;0;3;Software ABC;}
#' \code{1010431;2020;P100_1100_03300;;0;3;max.muster@kreditkasse.ch;}
#' \code{1010431;2020;P100_1100_03400;90;0;3;;}
#' @param returnRaw Logical value indicating if the raw table should be returned.
#' @return A data.frame with one row. The column names contain the "Merkmals UIDs".
#' @examples
#' readCsvZaData("SPA-202000001010431.txt")
readCsvZaData <- function(file, returnRaw = FALSE) {
  dat <- utils::read.table(file, sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", comment.char="", fileEncoding = "UTF-8")
  allCols <- c("betrieb", "jahr", "merkmal", "wert", "index", "schnittstelle", "text")
  showMissingNames(allCols, colnames(dat), "In the file, the following columns must be present, but are missing:\n")
  dat[is.na(dat[, "wert"]), "wert"] <- ""
  dat[, "wert"] <- trimws(dat[, "wert"])

  # X-Merkmale sind alte Merkmale, wo ein neuerer Wert verfuegbar ist. Deshalb loeschen.
  dat <- dat[substr(dat[,"merkmal"],1,1)!="X", ]
  dat <- dat[!is.na(dat[, "jahr"]),]

  if (isTRUE(returnRaw)) {
    return(char.cols.to.num(dat))
  }

  res <- transformRawZaMmToDf(data.frame(
    UID = dat[, "merkmal"],
    Index = dat[, "index"],
    Value = paste0(dat[, "wert"], dat[, "text"])
  ))
  return(cbind(BETRIEB = dat[1, "betrieb"], JAHR = dat[1, "jahr"], res))
}
# readCsvZaData("test/data/spe-raw-file.txt")

#' Read ZA data from XML
#' @param file The file containing the data as XML.
#' @param what \itemize{
#' \item \code{"farm-mm"} Return the converted "Merkmale" from the XML file of a single farm.
#' \item \code{"farm-journal"} Return the journal entries from the XML file of a single farm.
#' \item \code{"za-accounts"} Return the ZA accounts from the file that is usually named "za-accountsystem.xml"
#' \item \code{"za-costunits"} Return the ZA cost units from the file that is usually named "za-accountsystem.xml"
#' }
#' @param transformFarmData Logical value. If \code{TRUE}, the farm data will be transformed using \code{\link{transformRawZaMmToDf}}.
#' @return A data.frame
#' @examples
#' readXmlZaData("/home/agsad.admin.ch/f80823148/mnt/Data-Work-RE/25_Agricultural_Economics-RE/251_ZA-BH_protected/ZADaten/SpB/Dateneinlieferung/2020/apr/DCollectZA-2021-40400039905.xml", what = "farm-mm")
#' readXmlZaData("/home/agsad.admin.ch/f80823148/mnt/Data-Work-RE/25_Agricultural_Economics-RE/251_ZA-BH_protected/ZADaten/SpB/Dateneinlieferung/2020/apr/DCollectZA-2021-40400039905.xml", what = "farm-journal")
#' readXmlZaData("za-accountsystem.xml", what = "za-accounts") # Path is invalid.
readXmlZaData <- function(file = NULL, what = c("farm-mm", "farm-journal", "za-accounts", "za-costunits"), transformFarmData = TRUE) {
  xml <- NULL
  if (is.null(file) && is.null(xml))
    stop("Either `file` or `xml` have to be given.")
  what <- match.arg(what)

  encoding <- if (what %in% c("farm-mm", "farm-journal")) character() else "UTF-8"

  installFromCRAN("XML")
  if (!is.null(file)) {
    if (!is.null(xml))
      stop("Specify either `file` or `xml` but not both.")
    if (what %in% c("farm-mm", "farm-journal")) {
      xml <- XML::xmlParse(file, encoding = encoding) # No xmlRoot!
    } else {
      xml <- XML::xmlRoot(XML::xmlParse(file, encoding = encoding))
    }
  } else {
    if (!all(c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode")) %in% class(xml))
      stop('`xml` must be an object created by using: XML::xmlRoot(XML::xmlParse(file, encoding = "UTF-8"))')
  }

  if (what == "za-accounts") {
    ind <- getXmlNodeIndexByNameAndAttr(xml, name="Table", attr="AccountZA")
    if (length(ind) == 0)
      stop('No element with tag "Table" and attribute "AccountZA" found.')
    dat <- XML::xmlToDataFrame(xml[[ind]], stringsAsFactors=FALSE)

  } else if (what == "za-costunits") {
    ind <- getXmlNodeIndexByNameAndAttr(xml, name="Table", attr="CostunitZA")
    if (length(ind) == 0)
      stop('No element with tag "Table" and attribute "CostunitZA" found.')
    dat <- XML::xmlToDataFrame(xml[[ind]], stringsAsFactors=FALSE)

  } else if (what == "farm-mm") {
    xmlSub <- xml["//AttributeList/Attribute"]
    if (length(xmlSub) == 0)
      stop('No elements in path "//AttributeList/Attribute" found.')
    dat <- XML::xmlToDataFrame(xmlSub, stringsAsFactors=FALSE)
    if (transformFarmData)
      dat <- transformRawZaMmToDf(dat)

  } else if (what == "farm-journal") {
    xmlSub <- xml["//FinancialAccounting/Journalentries"]
    if (length(xmlSub) == 0)
      stop('No elements in path "//FinancialAccounting/Journalentries" found.')
    dat <- XML::xmlToDataFrame(xmlSub[[1]], stringsAsFactors=FALSE)
  }

  return(dat)
}

#' Transform raw delivered ZA data to a dataframe.
#' @param data A data.frame containing the columns "UID", "Index" and "Value". It is the data of one farm only.
#' @return The data converted to a data.frame were the colnames are the "Merkmal UIDs" with one row containing the data of the farm.
#' @details This is a helper function for \code{\link{readXmlZaData}}
#' @examples
#' transformRawMmToDf(data.frame(
#'   UID = c("P220_2020_00100", "P220_2020_00100", "P100_2100_21220"),
#'   Index = c("1", "2", NA),
#'   Value = c("300" ,"250", "2")
#' ))
transformRawZaMmToDf <- function(data) {
  if (!is.data.frame(data) && !is.matrix(data))
    stop("`data` has to be a data.frame or a matrix.")
  missing <- setdiff(c("UID", "Index", "Value"), colnames(data))
  if (length(missing) > 0)
    stop("The following colnames are missing in `data`: ", paste0(missing, collapse = ", "))
  transformed <- apply(data, 1, function(row) {
    if (!row["Index"] %in% c(NA, "")) {
      if (as.numeric(row["Index"]) < 10) {
        row["UID"] <- paste0(substr(row["UID"], 1, nchar(row["UID"])-1), row["Index"])
      } else {
        row["UID"] <- paste0(substr(row["UID"], 1, nchar(row["UID"])-2), row["Index"])
      }
    }
    value <- suppressWarnings(as.numeric(row["Value"]))
    if (is.na(value))
      value <- unname(row["Value"])
    return(list(UID = unname(row["UID"]), Value = value))
  })
  result <- as.data.frame(lapply(transformed, function(x)x[["Value"]]))
  colnames(result) <- sapply(transformed, function(x)x[["UID"]])
  return(result)
}

#' Remove the locally stored data.
#' @author Daniel Hoop
#' @return A logical vector incating which files were deleted.
.removeLocalZaData <- function() {
  file.remove(list.files(.zaPaths(localData=1), full.names = TRUE, recursive = TRUE))
}

#' Load the latest status file for SpE
#' @export
#' @author Daniel Hoop
#' @param BHJ *Optional*: Buchhaltungsjahr
#' @param termin *Optional*: If not given, the latest one will be chosen automatically.
#' @param file *Optional*: If `BHJ` is not given, then `file` must be specified.
load.spe.status <- function(BHJ = NULL, termin = NULL, file = NULL) {

  if (!is.null(BHJ) && !is.null(file))
    stop("Either specify `BHJ` or `file` but not both.")

  if (is.null(file)) {
    pathStatus <- agsPath(paste0("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11157/SpE/Liste_Plausible/B",BHJ,"/"))

    if (is.null(termin)) {
      fold <- list.files(pathStatus)
      fold <- fold[grepl("Termin",fold)]
      fold <- sort(fold[file.info(paste0(pathStatus, fold))$isdir], decreasing=TRUE)[1]
      fileWithoutEnding <- agsPath(paste0("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11157/SpE/Liste_Plausible/B",BHJ,"/",fold,"/Plausible_B",BHJ))
    } else {
      fold <- paste0(pathStatus, termin, "_Termin")
      fileWithoutEnding <- paste0(fold,"/Plausible_B",BHJ)
    }

  } else {
    file <- file[1]
    fileWithoutEnding <- strsplit(file, "\\.")[[1]]
    fileWithoutEnding <- paste0(fileWithoutEnding[-length(fileWithoutEnding)], collapse = ".")
  }

  if (file.exists(paste0(fileWithoutEnding, ".xlsx"))) {
    installFromCRAN("openxlsx")
    message("SpE-Status wird aus folgender Datei geladen: ", paste0(fileWithoutEnding, ".xlsx"))
    status <- openxlsx::read.xlsx(paste0(fileWithoutEnding, ".xlsx"), sheet = "Status", startRow = 3)
  } else if (file.exists(paste0(fileWithoutEnding, ".csv"))) {
    message("SpE-Status wird werden aus folgender Datei geladen: ", paste0(fileWithoutEnding, ".csv"))
    status <- read.table(paste0(fileWithoutEnding, ".csv"),  sep=";", skip=2, header=TRUE, stringsAsFactors=FALSE, quote = "\"")
  } else {
    stop("Weder eine csv noch eine xlsx-Datei existieren: ", fileWithoutEnding)
  }

  return(status)
}

#' Load SpE raw data.
#' @export
#' @author Daniel Hoop
#' @param withGbVars Logical value indicating if the variables that are defined for the "Grundlagenbericht" should be extracted as well.
#' @param assureSameObsInDbAndStatusList Logical value indicating if it should be assured that the number of observations in the database is in accordance with the number of observartions in the status list.
#' The function will take into account the fact that some observations could not be linked with AGIS data, and will not report those missing observations as errors.
#' @param latestBhjForTest The latest book keeping year for the above mentioned test.
load.spe <- function(withGbVars = FALSE, assureSameObsInDbAndStatusList = FALSE, latestBhjForTest = NULL) {
  pfad1 <- paste0(.zaPaths(localData=1),"SpE.RData")
  pfad2 <- paste0(.zaPaths(speDataSource=1), "alldata/SpE.RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("SpE-Daten werden aus folgender Datei geladen: ", pfad))
  dat <- load2(pfad)

  # Testen ob alle plausiblen Betriebe im Datensatz sind
  if (assureSameObsInDbAndStatusList) {
    if (is.null(latestBhjForTest))
      stop("If `assureSameObsInDbAndStatusList = TRUE`, then `latestBhjForTest` must be specified.")
    BHJ <- latestBhjForTest
    # Nach dem finalen Termin zum Dateneinlesen einen Fehler ausgeben, wenn etwas mit den Listen der plausiblen Betriebe nicht stimmt.
    if (Sys.time() > strptime(paste0("14.8.", BHJ + 1, " 15:00"), "%d.%m.%Y %H:%M")) {
      warnStopFunc <- base::stop
    } else {
      warnStopFunc <- base::warning
    }

    plauslist <- load.spe.status(latestBhjForTest)
    ausschlussList <- read.table(agsPath(paste0("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/2/4271/B",BHJ,"/IDBetriebeAusschliessenDB/Ausschliessen_B",BHJ,".csv")),  sep=";", skip=0, header=TRUE, stringsAsFactors=FALSE, quote = "\"")

    id_ok <- as.numeric( plauslist[ plauslist[,"DB_einlesen"]=="Ja" & !plauslist[,"Betriebsnummer"]%in%ausschlussList[,"REK_ID"] ,"Betriebsnummer"] )
    id_not_in_DB <- sort(id_ok[ !id_ok%in%dat[dat[,"JAHR"]==BHJ,"REK_ID"] ])
    # Pruefen, ob alle plausiblen Betriebe im Datensatz sind.
    if(length(id_not_in_DB)>0) {
      notVerknFile <- agsPath(paste0("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11157/SpE/ErhbogTxtExport/nichtVerkn/B",BHJ,"/ID_nVerkn_B",BHJ,".csv"))
      if (!file.exists(notVerknFile)) {
        warnStopFunc(paste0("Es scheinen Betriebe in der Datbenbank zu fehlen, aber laut dem Verknuepfungsprozess ist dieser Fehler nicht aufgetreten. Das folgende File fehlt:\n",
                             notVerknFile, "\n",
                             messageBox("BITTE R NEU STARTEN UND NOCHMAL VERSUCHEN"),
                             "\nDie Daten muessen wahrscheinlich erst auf die lokale Festplatte kopiert werden!"))
      }
      notVerknDat <- read.table(notVerknFile, sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!"))
      id_not_verkn <- sort(notVerknDat[notVerknDat[,1]%in%id_not_in_DB,1])
      if(length(id_not_in_DB)!=length(id_not_verkn) || any(id_not_in_DB!=id_not_verkn)){
        warnStopFunc(paste0("Es gibt Betriebe, die laut OTRS-Liste eigentlich in die Datenbank gehoeren. Sie sind aber nicht im Datensatz drin! Anbei die REK_IDs:\n",
                             paste0(id_not_in_DB,collapse=", "),
                             "\nVon den oben genannten REK_ID, konnten die folgenden nicht mit AGIS verknuepft werden:\n",
                             paste0(id_not_verkn,collapse=", ")))
      }
      if(FALSE){
        files <- list.files(agsPath(paste0("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11157/SpE/ErhbogTxtExport/Rohdat/B",BHJ)), recursive=TRUE)
        files <- unique(files[substr.rev(files,1,4)==".txt"])
        files <- lapply(strsplit(files,"/"),function(x)x[length(x)])
        files <- substr(files,13,19)
        message("Diese IDs kommen nicht in den Rohdaten-Files vor:")
        print(id_not_in_DB[!id_not_in_DB%in%files])
      }
    }

    # Pruefen, ob es Betriebe in DB gibt, die eigentlich nicht plausibel sind.
    spaID <- dat[dat[,"JAHR"]==BHJ,"REK_ID"]
    id_not_in_plauslist <- sort(spaID[!spaID%in%id_ok])
    if(length(id_not_in_plauslist)>0) {
      warnStopFunc (paste0("Einige Betriebe, die in der Datenbank sind, sind laut CRM-OTRS-Liste nicht plausibel! Anbei die REK_IDs:\n",
                           paste0(id_not_in_plauslist,collapse=", ")))
    }
  }

  # Add gb vars if wished
  if (withGbVars) {
    gb <- load.spe.gb()
    keepDat <- paste.cols(dat[,c("BETRIEB", "JAHR")]) %in% paste.cols(gb[,c("ZA_ID", "Jahr")])
    if (sum(keepDat) == 0)
      stop ("Beim Verbinden der 'rohen' Daten und den GB-Daten ist ein Fehler aufgetreten. Es gibt keine uebereinstimmenden Betriebe.")
    if (sum(keepDat) < nrow(dat))
      message("Im rohen Datensatz sind mehr Beobachtungen vorhanden als im GB-Datensatz. Diese Beobachtungen werden verworfen.")
    dat <- dat[keepDat, , drop = FALSE]
    dat <- dat[order(dat[, "BETRIEB"], dat[, "JAHR"]), , drop = FALSE]
    gb <- gb[order(gb[, "ZA_ID"], gb[, "Jahr"]), , drop = FALSE]
    transferCols <- colnames(gb)[!colnames(gb) %in% colnames(dat)]
    dat <- cbind(dat, gb[, transferCols])
  }
  # Datensatz ausgeben.
  return(dat)
}
# Alias erzeugen
load.spa <- load.spe

#' Load SpE raw person data.
#' @export
#' @author Daniel Hoop
load.spe.pers <- function(){
  pfad1 <- paste0(.zaPaths(localData=1),"SpE_Personen_Indexiert.RData")
  pfad2 <- paste0(.zaPaths(speDataSource=1), "Personen/SpE_Personen_Indexiert.RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("SpE-Personendaten werden aus folgender Datei geladen: ", pfad))
  return(load2(pfad))
}

#' Load SpE data with the same columns that are given in the Grundlagenbericht.
#' @export
#' @author Daniel Hoop
load.spe.gb <- function() {
  pfad1 <- paste0(.zaPaths(localData=1),"SpE_GB_Einzel.RData")
  pfad2 <- paste0(.zaPaths(speDataSource=1), "GB_Einzel/SpE_GB_Einzel.RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("SpE-GB-Daten werden aus folgender Datei geladen: ", pfad))
  return(load2(pfad))
}

#' Load SpB raw data.
#' @export
#' @author Daniel Hoop
#' @param withGbVars Logical value indicating if the variables that are defined for the "Grundlagenbericht" should be extracted as well.
load.spb <- function(withGbVars = FALSE) {
  pfad1 <- paste0(.zaPaths(localData=1),"SpB.RData")
  pfad2 <- paste0(.zaPaths(spbDataSource=1), "alldata/SpB.RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("SpB-Daten werden aus folgender Datei geladen: ", pfad))
  dat <- load2(pfad)

  # Add gb vars if wished
  if (withGbVars) {
    gb <- load.spb.gb()
    keepDat <- paste.cols(dat[,c("BETRIEB", "JAHR")]) %in% paste.cols(gb[,c("BETRIEB", "Jahr")])
    if (sum(keepDat) == 0)
      stop ("Beim Verbinden der 'rohen' Daten und den GB-Daten ist ein Fehler aufgetreten. Es gibt keine uebereinstimmenden Betriebe.")
    if (sum(keepDat) < nrow(dat))
      message("Im rohen Datensatz sind mehr Beobachtungen vorhanden als im GB-Datensatz. Diese Beobachtungen werden verworfen.")
    dat <- dat[keepDat, , drop = FALSE]
    dat <- dat[order(dat[, "BETRIEB"], dat[, "JAHR"]), , drop = FALSE]
    gb <- gb[order(gb[, "BETRIEB"], gb[, "Jahr"]), , drop = FALSE]
    transferCols <- colnames(gb)[!colnames(gb) %in% colnames(dat)]
    dat <- cbind(dat, gb[, transferCols])
  }
  return(dat)
}
#' Load SpB data with the same columns that are given in the Grundlagenbericht.
#' @export
#' @author Daniel Hoop
load.spb.gb <- function() {
  pfad1 <- paste0(.zaPaths(localData=1),"SpB_GB_Einzel.RData")
  pfad2 <- paste0(.zaPaths(spbDataSource=1), "GB_Einzel/SpB_GB_Einzel.RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("SpB-GB-Daten werden aus folgender Datei geladen: ", pfad))
  return(load2(pfad))
}

#' Load SpB raw person data.
#' @export
#' @author Daniel Hoop
load.spb.pers <- function(){
  pfad1 <- paste0(.zaPaths(localData=1),"SpB_Personen_Indexiert.RData")
  pfad2 <- paste0(.zaPaths(spbDataSource=1), "Personen/SpB_Personen_Indexiert.RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("SpB-Personendaten werden aus folgender Datei geladen: ", pfad))
  return(load2(pfad))
}

#' Load Referenzbetriebs data (2003 - 2015) with the same columns that are given in the Grundlagenbericht.
#' @export
#' @author Daniel Hoop
#' @details In the year 2015 weights are 0 for all farms because SpE was the official sample in that year.
load.gb <- function(inklBG = FALSE) {
  fileSuffix <- if (inklBG) "GB_inkl_BG.RData" else "GB.RData"
  pfad1 <- paste0(.zaPaths(localData=1), fileSuffix)
  pfad2 <- paste0(.zaPaths(refDataSource=1), "GB/", fileSuffix)
  pfad3 <- agsPath(paste0("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/2/3583/Resultate/00-00-00_Zusatzdaten/Grundlagenbericht_RefB/", fileSuffix))
  pfad <- if(file.exists(pfad1)) pfad1  else if(file.exists(pfad2)) pfad2 else if(file.exists(pfad3)) pfad3 else stop("You don't have permission to load this data set.")
  message(paste0("Ref-GB-Daten werden aus folgender Datei geladen: ", pfad))
  message("**********\nGewichte in Jahr 2015 sind immer 1, da ab dann SpE die offizielle Stichprobe ist.\n**********")
  return(load2(pfad))
}

load.ref.bz <- function(inklBG = FALSE) {
  path <- paste0(.zaPaths(refDataSource=1), "Betriebszweige/BZ_Ref.fst")
  if (!file.exists(path))
    stop("You don't have permission to load this data set.")
  installFromCRAN("fst")
  message(paste0("Ref-Betriebszweigdaten werden aus folgender Datei geladen: ", path))
  dat <- fst::read_fst(path)
  if (!inklBG) {
    dat <- dat[dat[, "Referenzbetrieb"] == 1, , drop = FALSE]
  }
  return(dat)
}

#' Load Testbetriebsnetz data (1990 - 2002) with the same columns that are given in the Grundlagenbericht.
#' @export
#' @author Daniel Hoop
#' @details Some columns that are available from \code{\link{load.gb}} are not available from this function because the< were not converted from Testbetriebsnetz- to Referenzbetriebe-Merkmalskatalog and would therefore always be 0. To avoid misinterpretation of these variables they were been dropped from the dataset.
load.tbn.gb <- function(inklBG = FALSE) {
  fileSuffix <- if (inklBG) "GB_TBN_inkl_BG.RData" else "GB_TBN.RData"
  pfad1 <- paste0(.zaPaths(localData=1), fileSuffix)
  pfad2 <- paste0(.zaPaths(refDataSource=1), "GB_Testbetriebsnetz/", fileSuffix)
  pfad3 <- agsPath(paste0("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/2/3583/Resultate/00-00-00_Zusatzdaten/Grundlagenbericht_RefB/", fileSuffix))
  pfad <- if(file.exists(pfad1)) pfad1  else if(file.exists(pfad2)) pfad2 else if(file.exists(pfad3)) pfad3 else stop("You don't have permission to load this data set.")
  message(paste0("Testbetriebsnetz-GB-Daten werden aus folgender Datei geladen: ", pfad))
  return(load2(pfad))
}

#' Load AGIS data
#' @export
#' @author Daniel Hoop
#' @param year A vector containing all years for which AGIS data should be loaded.
#' @param additional Logical value indicating if addional data such as "Auswahlgesamtheit" should be loaded as well.
#' @param status Logical value indicating if additional data from the recruitment status should be loaded as well.
#' @param keepAllInStatus Logical value indicating if all farms in the 'status' dataset should be kept. If this option is set TRUE, then the number of farms in 'status' dataset and in AGIS dataset must be equal.
#' @param cols Optional: Character vector listing the columns that should be extracted. The rest of the columns will be discarded.\cr
#' Special cases: If \code{NULL}, then all columns will be loaded. If \code{"Rueckverfolg"}, then columns that are relevant for the "Rueckverfolgung" are loaded. Else, just provide the columns you want to load.
#' @param filterFunc Optional: A function to filter the dataset while reading the data from all year. See the example for more details.
#' @examples
#' agis <- load.agis(2017:2018, filterFunc = function(agis) agis[agis[, "ZA_ID"] %in% spe[, "ZA_ID"], ])

load.agis <- function(year, additional = FALSE, status = FALSE, keepAllInStatus = FALSE, cols = NULL, filterFunc = NULL) {

  # Recursive function definition if more than 1 year was given.
  if (length(year) > 1) {
    year <- sort(year, decreasing = TRUE)
    agis <- lapply(year, function(x) load.agis(year = x, additional = additional, status = status, keepAllInStatus = keepAllInStatus, cols = cols, filterFunc = filterFunc))
    joinCols <- unique(unlist(lapply(agis, colnames)))
    # suppressWarnings, because some colnames are badly formatted because of ANSI/UTF-8 issues.
    xCols <- joinCols[suppressWarnings(grepl("^X[0-9]{4}$", joinCols))]
    oCols <- joinCols[!joinCols %in% xCols] # oCols like 'other cols'.
    # X-cols are set to 0
    # Other cols are set to NA, if not available.
    agis <- lapply(agis, function(dat) {
      newXCols <- xCols[!xCols %in% colnames(dat)]
      dat[newXCols] <- 0
      newOCols <- oCols[!oCols %in% colnames(dat)]
      dat[newOCols] <- NA
      return (dat[joinCols])
    })
    return (do.call("rbind", agis))
  }

  # Normal function
  pfad1 <- paste0(.zaPaths(localData=1), "AGIS/AGIS_BFS_", year, ".RData")
  pfad2 <- paste0(.zaPaths(agisDataSource=1), year,"/AGIS_BFS_", year, ".RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  message(paste0("AGIS-Daten werden aus folgender Datei geladen: ", pfad))
  agis <- load2(pfad)

  if (additional) {
    pfad3 <- paste0(.zaPaths(agisDataSource=1), year,"/Zusatzkennzahlen/AGIS_Zusatz.RData")
    if (!file.exists(pfad3))
      stop ("Das File mit den AGIS-Zusatzdaten existiert nicht: ", pfad3)
    message(paste0("AGIS-Zusatz-Daten werden aus folgender Datei geladen: ", pfad3))
    zusatz <- load2(pfad3)
    zusatz <- zusatz[, c("ZA_ID", colnames(zusatz)[!colnames(zusatz) %in% colnames(agis)])]
    nrowAgis <- nrow(agis)
    agis <- merge(x = agis, y = zusatz, by = c("ZA_ID"))
    if (nrowAgis != nrow(agis))
      stop ("Durch das Zusammenfuegen des AGIS-Datensatzes mit den Zusatzinformationen zu AGIS (z.B. Auswahlgesamtheit) hat sich die Anzahl Zeilen in AGIS veraendert, was nicht sein duerfte.")
    rm(pfad3, zusatz, nrowAgis)
  }

  if (status) {
    pfad3 <- paste0(.zaPaths(agLiDataSource=1), "B",year,"/AGIS_LINK.RData")
    if (!file.exists(pfad3))
      stop ("Das File mit den AGIS-LINK-Daten existiert nicht: ", pfad3)
    message(paste0("AGIS-LINK-Daten werden aus folgender Datei geladen: ", pfad3))
    zusatz <- load2(pfad3)
    zusatz <- zusatz[, c("ZA_ID", colnames(zusatz)[!colnames(zusatz) %in% colnames(agis)])]
    nrowAgis <- nrow(agis)
    agis <- merge(x = agis, y = zusatz, by = c("ZA_ID"), all.y = keepAllInStatus)
    if (!keepAllInStatus && nrowAgis != nrow(agis))
      stop ("Durch das Zusammenfuegen des AGIS-Datensatzes mit den LINK-Statusinformationen zu AGIS (z.B. aktivierte Betriebe) hat sich die Anzahl Zeilen in AGIS veraendert, was nicht sein duerfte.")
    if ("JAHR" %in% colnames(agis))
      agis[is.na(agis[, "JAHR"]), "JAHR"] <- year
    rm(pfad3, zusatz, nrowAgis)
  }

  if (!is.null(cols)) {
    if (cols[1] == "Rueckverfolg") {
      cols <- c("JAHR", "ZA_ID", "REK_ID", "REGION", "ZA15TYPS3_AVG", "SO_AVG", "SDB_AVG", "BETFORM", "LEGAL_FORM", "SPRACHE_AGIS",
                "SPRACHE_DL", "GVE_TOT_AVG", "LN", "StatusLINK", "STATUS2", "StatusZA", "group", "Auswahlgesamtheit")
    }
    missing <- cols[!cols %in% colnames(agis)]
    if (length(missing) > 0) {
      cat("\n\n")
      message("Year: ", year, ". Some columns are missing and cannot be extracted. Maybe you have to set `additional = TRUE` and `status = TRUE`? These columns are missing:\n",
           paste0(missing, collapse = ", "))
      cat("\n\n")
      return(NULL)
    }
    agis <- agis[cols]
  }

  if (!is.null(filterFunc)) {
    agis <- filterFunc(agis)
    if (!is.data.frame(agis) && !is.matrix(agis))
      stop("Appling `filterFunc` to the data has resulted in an object which is neither a data.frame nor a matrix.")
  }

  return(agis)
}

.availableAgisYears <- function() {

  timeoutSeconds <- 4
  yearExists <- FALSE
  startTime <- Sys.time()

  # First year of AGIS
  agisStartYear <- 2013
  years <- as.numeric(format(Sys.time(), "%Y"))
  years <- sort(unique(agisStartYear:years))
  for (i in length(years):1) {
    checkFile <- paste0(.zaPaths(agisDataSource=1), years[i], "/AGIS_BFS_", years[i], ".RData")
    yearExists <- yearExists || file.exists(checkFile)
    if (yearExists)
      break
    if (difftime(Sys.time(), startTime, units = "sec") > timeoutSeconds) {
      message("Available AGIS years could not be determined due to a timeout.")
      break
    }
  }

  if (yearExists)
    return(unique(years[1:i]))
  return(numeric())
}

#' Load full cost data
#' @export
#' @author Daniel Hoop
load.cost <- function(years=2014, ignore_P_cols=TRUE, non_aggr=FALSE, filter_expression=NULL, filterFunc=NULL, parentDir=NULL){
  # This function loads full cost data from several years and combines them.
  #
  # Arguments
  # years         = Numerical vector containing the years to be loaded.
  # ignore_P_cols = Logical value indicating if all columns with the ending "_P" (proportional allocation) should be ignored when reading in the data
  # non_aggr      = Logical value indicating if the non aggregated joint costs should be loaded (i.e. animal categories like miku instead of entersprises like Milch)
  # filter_expression = An expressoin() containing a filter expression that is applied to each year while reading in the data.
  #                     this could look like expression(cost[ cost[,"BZ"]=="Milch" ,c("ID","Jahr","BZ","ArbeitNAT","Maschinen")])
  # parentDir     = The parent directory in which the folders of all years are located.

  if (!is.null(filter_expression) && !is.expression(filter_expression)) stop("filter_expression must be an expression! Use e.g. expression(cost[c(1,2),])")
  if (!is.null(filterFunc)) {
    if (!is.null(filter_expression))
      stop ("Either specify filter_expressiona or filtFunc but not both.")
  }

  # Automatically choose the newest folder for the directories of Daniel.
  if(is.null(parentDir)){
    parentDir <- c("C:/Users/U80823148/_/Data/Voko", "C:/Users/f80823148/_/ME/ME_data_out/data_final")
    parentDir <- parentDir[ dir.exists(parentDir) ][1]
    allFolders <- list.files(parentDir,full.names=TRUE); allFolders <- allFolders[file.info(allFolders)$isdir]; allFolders <- allFolders[ order(file.info(allFolders)[,"ctime"], decreasing=TRUE) ]
    take <- 1

    while(TRUE){
      if( length(list.dirs(allFolders[take],recursive=FALSE))==0 && take<=length(allFolders) ){
        take <- take + 1
      } else {
        parentDir <- allFolders[take]
        break
      }
    }
  }

  if(non_aggr) fileName <- "/allcosts_nonaggr.RData" else fileName <- "/allcosts_info.RData"

  # Loop over all years. Read in the data.
  cost1 <- NULL
  for(i in 1:length(years)) {
    pfad1 <- paste0(parentDir,"/",years[i],fileName)
    if(!file.exists(pfad1)) stop(paste("The file does not exist. You might have chosen the wrong parentDir.",
                                       "The following folders/files are available:", paste0(list.files(parentDir,full.names=TRUE),collapse="\n") ,sep="\n"))
    if(is.null(cost1)) message("Vollkostendaten werden aus folgender Datei geladen:")
    message(pfad1)
    cost <- load2(pfad1)
    # Filter data according to special expression
    if(!is.null(filter_expression)){
      cost <- eval(filter_expression)
    }
    if (!is.null(filterFunc)) {
      cost <- filterFunc(cost)
    }
    # Berechnungen mit propoertionaler Zuteilung entfernen.
    if(ignore_P_cols) cost <- cost[,substr.rev(colnames(cost),1,2)!="_P"]
    tryCatch({
      cost1 <- rbind(cost1,cost)
    },
    error=function(e){
      cn1 <- colnames(cost1)
      cn2 <- colnames(cost)
      cn1_noMatch <- cn1[!cn1%in%cn2]
      cn2_noMatch <- cn2[!cn2%in%cn1]
      cat("*** FEHLER! ***\n")
      cat("Die Spalten der verschiedenen Jahre passen nicht zueinander!\n")
      if(length(cn1_noMatch)>0) cat("Spalten in ",years[i-1],", die in ",years[i  ]," nicht vorkommen: ", paste0(cn1_noMatch,collapse=", "),"\n",sep="")
      if(length(cn2_noMatch)>0) cat("Spalten in ",years[i  ],", die in ",years[i-1]," nicht vorkommen: ", paste0(cn2_noMatch,collapse=", "),"\n",sep="")
      cat("*** ******* ***\n")
      stop(e)
    })

    rm(cost); invisible(gc())
  }
  return(cost1);
}

#' Translate between REK_ID and ZA_ID
#' @export
#' @author Daniel Hoop
rekid.zaid <- function(id, reverse=FALSE, BHJ=NULL, no.match.NA=TRUE){

  idMapping <- .keyValueStore$getOrSet(
    "rekid.zaid$idMapping",
    local({
      baseFolder <- agsPath("//oslw-s-pr.wbf.admin.ch/OSLW-PR$/OS/2/5/2/1/5/11157/SpE/ID_Verkn_AGIS_Rekr/")
      files <- list.files(baseFolder)
      if (length(files) == 0)
        stop ("Der Ordner '", baseFolder, "' scheint nicht zu existieren, oder es befinden sich darin keine Unterordner.")
      files <- files[file.info(paste0(baseFolder, "/", files))$isdir]
      # B2012 und B2013 weglassen, weil es dort noch ART_ID war.
      files <- files[!grepl("B2012$|B2013$", files)]
      # In B2016 wurde die ID nachtraeglich veraendert. Deshalb Filename aendern.
      years <- as.numeric(substring(files, 2))
      isB2016 <- grepl("B2016$", files)
      files[!isB2016] <- paste0(files[!isB2016], "/Verkn_", files[!isB2016], ".RData")
      files[ isB2016] <- paste0(files[ isB2016], "/Verkn_", files[ isB2016], "_korr.RData")
      files <- paste0(baseFolder, "/", files)
      # Fehlerpruefung.
      if (any(!file.exists(files)))
        stop ("Die folgenden Dateien muessen vorhanden sein fuer die REK_ID <-> ZA_ID Verknuepfung:\n",
              paste0(files[!file.exists(files)], collapse = "\n"))

      idMapping <- mapply(
        filename = as.list(files),
        year = as.list(years),
        function (filename, year) {
          x <- load2(filename)
          requireNames <- c("ZA_ID", "REK_ID")
          if (!all(requireNames %in% colnames(x)))
            stop ("Die Datei '", filename, "' muss die beiden Spaltennamen ", paste0(requireNames), " enthalten.")
          if (substr(x[1,"ZA_ID"], 1, 2) == "ZA")
            x[,"ZA_ID"] <- as.numeric(substring(x[,"ZA_ID"], 3))
          return (list(cbind(x[requireNames], "JAHR"=year)))
        })
      return (do.call("rbind", idMapping))
    })
  )

  # ALTe Variante, nur basierend auf gelieferten Daten.
  # idMapping <- .keyValueStore$getOrSet(
  #   "rekid.zaid$idMapping",
  #   local({
  #     if(!exists("spa")) spa <- load.spe()
  #
  #     idMapping <- spa[,c("JAHR","REK_ID", colnames(spa)[colnames(spa)%in%c("BETRIEB","ZA_ID")][1] )]
  #     colnames(idMapping)[colnames(idMapping)=="BETRIEB"] <- "ZA_ID"
  #     return (idMapping)
  #   })
  # )

  if(!is.null(BHJ)) {
    idMapping <- idMapping[idMapping[,"JAHR"]%in%BHJ,]
  } else {
    idMapping <- idMapping[order(idMapping[,"JAHR"],decreasing=TRUE),]
  }

  if(!no.match.NA) {
    if(!reverse) return(idMapping[idMapping[,"REK_ID"]%in%id,])
    if( reverse) return(idMapping[idMapping[,"ZA_ID" ]%in%id,])
  } else {
    if(!reverse) matchCol <- "REK_ID" else matchCol <- "ZA_ID"
    idMapping <- idMapping[ match(id, idMapping[,matchCol]) ,]
    idMapping[,matchCol] <- id
    return(idMapping)
  }
}

#' Decode encrypted IDs of Referenzbetriebe.
#' @keywords internal
#' @author Daniel Hoop
id.entschluesseln <- function(...){
  pfad1 <- "C:/Users/U80823148/_/Data/"
  pfad2 <- agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/9/4278/hpda/_ZA/Ref/Data/Grundlagenbericht/")

  pfad3 <- "GB__allg_Einzel_inkl_BZG"
  if(any(file.exists(c(paste0(pfad1,pfad3,".csv"), paste0(pfad1,pfad3,".RData"))))) pfad <- pfad1 else pfad <- pfad2

  # csv.to.rdata(paste0(pfad1,pfad2))
  if(!file.exists(paste0(pfad,pfad3,".RData"))) {
    dat <- read.csv(paste0(pfad,pfad3,".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
    save(dat, file=paste0(pfad,pfad3,".RData") )
  }
  load( paste0(pfad,pfad3,".RData") )

  idCol <- c("ID","X.ID")
  idCol <- idCol[idCol%in%colnames(dat)]
  if (length(idCol) == 0)
    stop("Internal error. No ID column found in GB__allg_Einzel.")
  idCol <- idCol[1]

  id <- c(...)
  res <- dat[match(id, dat[,idCol]),"ID_unverschluesselt"]
  return(res)

  #id <- id[!duplicated(id)]
  #if(length(id)==1){
  #  cbind(id,gb[,"ID_unverschluesselt"])

  #res <- dat[dat[,"ID"]%in%id,"ID_unverschluesselt"]
  #return(res[!duplicated(res)])
  #} else {
  #res <- dat[dat[,"ID"]%in%id,c("ID","ID_unverschluesselt")]
  #return(res[!duplicated(res),])
  #}
}

#' Merge Tables A, B, C, D, E, F into one large einzelbetriebliches Referenzbetriebs-GB dataset.
#' @keywords internal
#' @author Daniel Hoop
#folder <- "P:/_ZA/Ref/Data/Grundlagenbericht/"; filenames=NULL;extra_filename=NULL; update.files=FALSE; save.file=TRUE; save.name="GB"; filetype=c("csv"); NAto0=TRUE; header=TRUE; colnamesrow=1; skiprows=colnamesrow+1
merge.gb <- function(folder, filenames=NULL, extra_filename=NULL, update.files=FALSE, save.file=TRUE, save.name="GB", filetype=c("csv"), NAto0=TRUE, header=TRUE, colnamesrow=1, skiprows=colnamesrow+1, idCol="ID", yearCol="Jahr", ...){
  # Grundlagenbericht importieren, indem die 6 csv Files nach der Vorlage der BO-Extraktionen "GB_A_Einzel, ..." eingelesen und aneinander gebunden werden.
  # F?r aus BO exportierte csv die Einstellung colnamesrow=5 und skiprows=6 beibehalten.
  # Wenn die csv-Datei schon mit colname ist, colnamesrow=1 und skiprows=1 einstellen.
  filetype <- match.arg(filetype)
  if(is.null(filenames))  filenames <- c("GB__allg_Einzel","GB_A_Einzel","GB_B_Einzel","GB_C_Einzel","GB_D_Einzel","GB_E_Einzel","GB_F_Einzel")
  filenames <- c(filenames, extra_filename)
  if(substr(folder,nchar(folder),nchar(folder))!="/") folder <- paste(folder,"/",sep="")
  fullnames <- paste(folder,filenames,sep="")
  gb.list <- list()
  if(filetype=="csv"){
    cat("Reading in files...\n")
    for(i in 1:length(fullnames)) {
      if(header){
        gb <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", stringsAsFactors=FALSE, header=TRUE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        gb <- gb[,!grepl("^X(\\.[0-9]+)?$", colnames(gb))] # Kill NA columns like "X", "X.1", etc.
        if(colnames(gb)[1]=="X.ID") colnames(gb)[1] <- "ID"
      } else {
        stop ("2018-07-09, hpda: I think, the default argument skiprows=colnamesrow+1 is wront and should be identical to colnamesrow. -> skiprows=colnamesrow.")
        headers <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", nrows=colnamesrow, stringsAsFactors=FALSE, header=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        headers <- as.character(headers[colnamesrow,])
        na.cols <- headers=="NA"
        headers <- headers[!na.cols]
        gb <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", skip=skiprows, stringsAsFactors=FALSE, header=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        gb <- gb[,!na.cols]
        colnames(gb) <- headers
        if(colnames(gb)[1]=="X.ID") colnames(gb)[1] <- "ID"
      }
      gb <- gb[!is.na(gb[,1]),]
      # Falls manche Spalten Strings enthalten, werden diese in numeric umgewandelt.
      char.cols <- !sapply(gb,function(x)is.numeric(x))
      gb[,char.cols] <-   suppressWarnings( sapply(gb[,char.cols],function(x)as.numeric(x)) )
      # Falls gew?nscht, werden die NA in 0 umgewandelt (nur bestimmte, bei denen 0 besser passt als NA)
      if(NAto0) {
        if(i%in%c(1,2,4,5,6,7)){
          gb[is.na(gb)] <- 0
        }
        if(i==3) {
          # Funktion definieren, um NAs zu ersetzen (mit lapply-Befehl)
          fillNA <- function(x){
            x[is.na(x)] <- 0
            return(x)
          }
          # Gewisse Spalten sollen nicht in 0 umgewandelt werden, der Rest wird zu 0 gemacht:
          convert.cols.names <- c("HFF_jeRGVE", "Flaeche_jeJAE", "kgMilch_jeKuh", "dtWeizen_jeha")
          convert.cols <- !colnames(gb)%in%convert.cols.names
          if( sum(!convert.cols)!= 4 ) {
            warning("Colnames of any of 'HFF_jeRGVE', 'Flaeche_jeJAE', 'kgMilch_jeKuh', 'dtWeizen_jeha' have changed and therefore the conversion of NA to 0 is not done correctly!", immediate.=TRUE)
            cat("These colnames are not available anymore in the data:\n")
            print(convert.cols.names[!convert.cols.names%in%colnames(gb)])
          }

          gb[, convert.cols ] <- lapply(gb[, convert.cols ],function(x)fillNA(x))
          # Bei den letzten 4 ist es jedoch umgekehrt. Hier geh?ren NAs rein!
          # gb[, !convert.cols ] <- lapply(gb[, !convert.cols ],function(x)fillNA(x,invert=TRUE))
        }
      }

      gb.list[[i]] <- gb
      cat(filenames[i], "complete\n")
    }
  }
  # Pr?fen, ob in allen Tabellen gleich viele Beobachtungen sind
  nrows <- do.call("c",lapply(gb.list,function(x)nrow(x)))
  if(any(nrows!=nrows[1]))  stop(paste0("Not the same year-filters were selected in the different csv files. You created an unbalanced panel.\n ",
                                        paste(paste(filenames, nrows, sep=": "),collapse="\n ")))
  # Pr?fen, ob ID und Jahr immer in der ersten Spalte stehen
  #gb.names <- do.call("rbind",lapply(gb.list,function(x)colnames(x)[1:2]))
  #if(any(c(gb.names[,1]!=idCol, gb.names[,2]!=yearCol))) stop("The first two columns of each Excel file must contain the ID and the accounting year. Names must be 'ID' and 'Jahr'")
  # hpda, 2018.07.09
  lapply(gb.list,function(x){
    if(!all(c(idCol,yearCol)%in%colnames(x))) stop("All excel files must contain the columns called like the arguments 'idCol' and 'yearCol' (in order to merge them).")
  })

  # Pr?fen, ob die Reihenfolge der Beobachtungen in allen Tabellen dieselbe ist
  orders <- list()
  for(i in 1:length(gb.list)){
    orders[[i]] <- match(paste.cols(gb.list[[1]][,c(idCol,yearCol)]),
                         paste.cols(gb.list[[i]][,c(idCol,yearCol)]))
  }
  orders <- do.call("rbind",orders) # orders[1:nrow(orders), 1:10]
  # Reihenfolge angleichen.
  for(i in 1:length(gb.list)) {
    gb.list[[i]] <- gb.list[[i]][orders[i,],]
    if (!all(gb.list[[1]][,c(idCol,yearCol)] == gb.list[[i]][,c(idCol,yearCol)]))  # cbind( gb.list[[1]][,c("Betrieb","Jahr")], gb.list[[i]][orders[i,],c("Betrieb","Jahr")] )
      stop ("Internal matching error.")
  }
  cat("IDs checked...\n")

  # idCol Spalte wird von allen, ausser der ersten Tabelle entfernt.
  for(i in 2:length(gb.list)) {
    gb.list[[i]] <- gb.list[[i]][,!colnames(gb.list[[i]])%in%c(idCol,yearCol)]
  }
  if(update.files){
    cat("Updating files (0 --> NA)...\n")
    for(i in 1:length(gb.list)){
      if(i==1) {
        write.table(gb.list[[i]], paste(fullnames[i],".csv",sep=""), sep = ";", col.names=TRUE, row.names=FALSE, ...)
      } else {
        write.table(cbind(gb.list[[1]][,c(1,2)], gb.list[[i]]), paste(fullnames[i],".csv",sep=""), sep = ";", col.names=TRUE, row.names=FALSE, ...)
      }
      cat(filenames[i], "complete\n")
    }
  }
  # Alle Listenelemente werden zu einer Matrix zusammengefasst
  gb.list <- do.call("cbind",gb.list)

  # Nun werden die SAK berechnet
  SAK_variables <- c("GVE_Milchkuehe","GVE_Schafe","GVE_Ziegen"
                     ,"Stk_Mutterkuehe","GVE_Aufzucht_Miku","GVE_Mastvieh_gross","GVE_MuKuhKalb_1Jminus","GVE_AndereKaelber","GVE_Pferde","GVE_UebrigeRaufuTiere","GVE_Gefluegel_tot","GVE_UebrigeTiere"
                     ,"GVE_Zuchtschweine","GVE_Mastsschweine","GVE_Ferkel"
                     ,"LN","SpezialkulturFlaeche", "rohHangbeitr")
  if(all(SAK_variables%in%colnames(gb.list))) {
    gb.list[,"SAK"] <- with(gb.list,
                            0.043*(GVE_Milchkuehe+GVE_Schafe+GVE_Ziegen) +
                              0.03*(Stk_Mutterkuehe + GVE_Aufzucht_Miku + GVE_Mastvieh_gross + GVE_MuKuhKalb_1Jminus + GVE_AndereKaelber + GVE_Pferde + GVE_UebrigeRaufuTiere + GVE_Gefluegel_tot + GVE_UebrigeTiere) +
                              0.04*GVE_Zuchtschweine + 0.007*(GVE_Mastsschweine + GVE_Ferkel) +
                              0.028*(LN-SpezialkulturFlaeche) + 0.3*SpezialkulturFlaeche + 0.015*rohHangbeitr/370
    )
    bio <- as.numeric(gb.list[,"Landbauform"]%in%c(3,4))
    gb.list[,"SAK"] <- with(gb.list, SAK + bio*0.2* ( 0.028*(LN-SpezialkulturFlaeche) + 0.3*SpezialkulturFlaeche )  )
    rm(bio)
  } else {
    warning("SAK wurden nicht berechnet, da folgende Variablen fehlen:")
    print(SAK_variables[!SAK_variables%in%colnames(gb.list)])
  }
  rm(SAK_variables)

  # Nun noch fuer jeden Kanton das K?rzen anfuegen
  Kanton_col <- colnames(gb.list)%in%"Kanton"
  if(!any(Kanton_col)) {
    warning("Kanton Nr. cannot be converted to name because there is no column name 'Kanton' in the data.", immediate.=TRUE)
  } else {
    kantons <- matrix(c(1:27, "ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU", "FL"), ncol=2)
    kanton_nr <- gb.list[,"Kanton"]
    kanton_names <- character()
    for(i in 1:nrow(kantons)){
      kanton_names[kanton_nr==kantons[i,1]] <- kantons[i,2]
    }
    gb.list <- cbind(gb.list[, 1:which(colnames(gb.list)%in%"Kanton") ],
                     "Kanton2"=kanton_names,
                     gb.list[, (which(colnames(gb.list)%in%"Kanton")+1):ncol(gb.list) ], stringsAsFactors=FALSE)
  }

  # Ersetzen von Umlauten
  uml0 <- c("?", "?", "?", "?", "?", "?")
  uml1 <- c("Ae","Oe","Ue","ae","oe","ue")
  for(i in 1:length(uml0)){
    colnames(gb.list) <- gsub(uml0[i],uml1[i],colnames(gb.list))
  }

  # Speichern
  if(save.file) {
    cat("Saving CSV-file...\n")
    if(any(filenames%in%save.name)) save.path <- paste(folder,save.name, "2.csv",sep="") else save.path <- paste(folder,save.name,".csv",sep="")
    write.table(gb.list, save.path, sep = ";", col.names=TRUE, row.names=FALSE, ...)
    cat("Saving RData-file...\n")
    gb <- gb.list; rm(gb.list)
    if(any(filenames%in%save.name)) save.path <- paste(folder,save.name, "2.RData",sep="") else save.path <- paste(folder,save.name,".RData",sep="")
    save(gb, file=save.path)
  }
  invisible(gb)
  cat("Job done!\n")
}

#' If Excel cannot read a csv file anymore apply this function to the file.
#' @keywords internal
#' @author Daniel Hoop
#' @description By default only the NA rows (and cols) at the end below (right) of the table are removed.
#' The function can also be use as a read.table function that automatically removes NA rows and cols.
#' @examples
#' repair.csv("filepath", header=TRUE, remove.middle.rows.cols=TRUE, save.file=FALSE)
repair.csv <- function(filepath, header=TRUE, remove.middle.rows.cols=FALSE, save.file=TRUE, print.info=TRUE, ...){

  data.in <- read.csv(filepath, sep=";", header=header, stringsAsFactors=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"), ...)

  # Exclue NA cols
  if(is.data.frame(data.in)) { colNAs <- sapply(data.in,function(x)all(is.na(x)))
  } else { colNAs <- apply(data.in,2,function(x)all(is.na(x))) }

  colNAs3 <- integer(0)
  last.col.NA <- FALSE
  if (length(which(colNAs))>0) if(max(which(colNAs))==length(colNAs)) last.col.NA <- TRUE
  if(!remove.middle.rows.cols & last.col.NA){ # Wenn die mittleren Spalten behalten werden sollen, aber die letzte leer ist...
    colNAs1 <- rev(colNAs)
    colNAs2 <- numeric()
    setNA <- 1
    while(setNA!=0){
      if(colNAs1[setNA]) {
        colNAs2[setNA] <- setNA
        setNA <- setNA+1
      } else { setNA <- 0 }
    }
    colNAs3 <- (length(colNAs)-length(colNAs2)+1):length(colNAs)
  } else if(remove.middle.rows.cols) {
    colNAs3 <- which(colNAs)
  }
  if(length(colNAs3)>0) {
    if(print.info){
      cat("The following cols where removed\n")
      print(colnames(data.in)[colNAs3])
    }
    data.in <- data.in[,-colNAs3]
  }

  # Exclue NA rows
  rowNAs <- apply(data.in,1,function(x)all(is.na(x)))

  rowNAs3 <- integer(0)
  last.row.NA <- FALSE
  if (length(which(rowNAs))>0) if(max(which(rowNAs))==length(rowNAs)) last.row.NA <- TRUE
  if(!remove.middle.rows.cols & last.row.NA){ # Wenn die mittleren Spalten behalten werden sollen, aber die letzte leer ist...
    rowNAs1 <- rev(rowNAs)
    rowNAs2 <- numeric()
    setNA <- 1
    while(setNA!=0){
      if(rowNAs1[setNA]) {
        rowNAs2[setNA] <- setNA
        setNA <- setNA+1
      } else { setNA <- 0 }
    }
    rowNAs3 <- (length(rowNAs)-length(rowNAs2)+1):length(rowNAs)
  } else if(remove.middle.rows.cols) {
    rowNAs3 <- which(rowNAs)
  }
  if(length(rowNAs3)>0) {
    if(print.info){
      cat("The following rows where removed\n")
      print(rowNAs3)
    }
    data.in <- data.in[-rowNAs3,]
  }

  if(save.file) write.table(data.in, file=filepath, sep = ";", col.names=header, row.names=FALSE)
  invisible(data.in)
}
#data.in <- matrix(c(
#  NA,NA,NA,NA,NA,NA,
# 10,10,NA,10,NA,NA,
#  10,10,NA,10,NA,NA,
#  10,10,NA,10,NA,NA,
#  NA,NA,NA,NA,NA,NA,
#  10,10,NA,10,NA,NA,
#  NA,NA,NA,NA,NA,NA,
#  NA,NA,NA,NA,NA,NA),
#                  byrow=TRUE,ncol=6)
#remove.middle.rows.cols <- TRUE
####

remove.na.rowcols.of.csv <- function(filepath, print.info=FALSE){
  repair.csv(filepath=filepath, header=TRUE, remove.middle.rows.cols=TRUE, save.file=TRUE, print.info=print.info)
}

#' Convert a CSV file to an RData file that contains the same data.
#' @export
#' @author Daniel Hoop
csv.to.rdata <- function(path, dat=NULL, name="dat", clean.numb.format=FALSE, clean.dat.columns=FALSE, update.csv=FALSE, ...){
  # This function converts csv data to RData (which can be read in much faster)

  # Remove .csv or .RData from string.
  if( tolower(substr.rev(path,1,4))==".csv" ) path <- substr(path,1,nchar(path)-4)
  if( tolower(substr.rev(path,1,6))==".rdata" ) path <- substr(path,1,nchar(path)-6)

  # If a path is given, read in new data. Else the data.frame 'dat' is used for further processing.
  if(is.null(dat)) {
    dat <- read.csv(paste0(path, ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!"))
  } else {
    message("Argument 'dat=' was given. Reading in no new data but directly using 'dat' from workspace!")
  }
  if(colnames(dat)[1]=="X.ID") colnames(dat)[1] <- "ID"

  # Clean columns, if wished
  if(clean.dat.columns) dat <- clean.data.columns(dat, ...)
  # Clean number formats if there are 1'000 formats, if whished
  if(clean.numb.format) dat <- clean.number.format(dat, ...)

  # Save RData
  assign(name, dat)
  eval(parse(text= paste0("save(",name,", file=",paste0("'", path, ".RData'") ,")")  ))
  # Update CSV, if wished
  if(update.csv) write.table(dat, file=paste0(path, ".csv"), sep = ";", eol = "\n", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
}

#' Write an object that is contained in a RData file into a CSV file that contains the same data.
#' @export
#' @author Daniel Hoop
rdata.to.csv <- function(path){
  # This function reads in an RData file and saves it as csv.
  # Remove .csv or .RData from string.
  if( tolower(substr.rev(path,1,4))==".csv" ) path <- substr(path,1,nchar(path)-4)
  if( tolower(substr.rev(path,1,6))==".rdata" ) path <- substr(path,1,nchar(path)-6)
  write.table(load2(paste0(path,".RData")), file=paste0(path,".csv"), sep = ";", eol = "\n", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
}

#' Read data from the clipboard.
#' @export
#' @author Daniel Hoop
#' @inheritParams view
read.cb <- function(names=c("col","rowcol","row","no"), no.data.frame=FALSE, ...) {
  # Read tables that are stored in the clipboard (e.g. copied in excel)

  if(no.data.frame){
    # Suppress the warning incomplete final line found on 'clipboard'
    return(suppressWarnings(readLines('clipboard')))
  }

  names <- match.arg(names)
  if(names=="row") {
    dat <- read.table("clipboard", sep="\t", header=FALSE, stringsAsFactors=FALSE, ...)
    rownames(dat) <- dat[,1]; dat <- dat[,-1]
    return(dat)
  } else if (names=="col") {
    return( read.table("clipboard", sep="\t", quote="", header=TRUE, stringsAsFactors=FALSE, ...) )
  } else if (names=="rowcol") {
    dat <- read.table("clipboard", sep="\t", quote="", header=TRUE, stringsAsFactors=FALSE, ...)
    rownames(dat) <- dat[,1]; dat <- dat[,-1]
    return(dat)
  } else {
    return( read.table("clipboard", sep="\t", quote="", header=FALSE, stringsAsFactors=FALSE, ...) )
  }
}

#' Write data to the clipboard.
#' @export
#' @author Daniel Hoop
#' @inheritParams view
write.cb <- function(data, names=c("col","rowcol","row","no"), ...){
  # Save a matrix into the clipoard.

  # If a vector is given as argument it is convertet to a matrix and rownames are given.
  if(is.null(dim(data))) {
    if(length(names)>1) names <- "row"
    names_data <- names(data)
    data <- as.matrix(data)
    names(data) <- names_data
  }

  names <- match.arg(names)

  if(names=="row") {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=FALSE, row.names=TRUE, ...)
  } else if (names=="col") {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=TRUE, row.names=FALSE, ...) # Nur colnames
  } else if (names=="rowcol") {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=NA, ...)                    # Colnames & Rownames
  } else {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=FALSE, row.names=FALSE, ...)
  }
}

#' Read a sheet from an Excel file
#' @export
#' @author Daniel Hoop
#' @param filename The Excel file.
#' @param ... Further arguments to be passed into \code{\link[openxlsx:read.xlsx]{openxlsx::read.xlsx}}
#' @inheritParams openxlsx::read.xlsx
read.xlsx <- function(filename, sheet = 1, ...) {

  installFromCRAN("openxlsx")

  if (length(args) == 0)
    return(openxlsx::read.xlsx(xlsxFile = filename, sheet = sheet, ...))

  args <- list(...)
  if (!"header" %in% names(args))
    return(openxlsx::read.xlsx(xlsxFile = filename, sheet = sheet, ...))

  # Rest of the function is for backward compatibility with the implementation that used XLConnect.
  names(args)[names(args) == "header"] <- "colNames"

  callString <- "openxlsx::read.xlsx(xlsxFile = filename, sheet = sheet"
  for (i in 1:length(args)) {
    arg <- args[i]
    if (length(arg) > 1) {
      if (is.character(arg))
        arg <- paste0('"', arg, '"')
      arg <- paste0("c(", paste0(arg, collapse = ","), ")")
    }
    callString <- paste0(callString, ", ", names(args), " = ", arg)
  }
  callString <- paste0(callString, ")")

  return(eval(parse(text = callString)))
}

#' Read a sheet from an Excel file (using the XLConnect package)
#' @author Daniel Hoop
#' @inheritParams XLConnect::loadWorkbook
#' @inheritParams XLConnect::readWorksheet
#' @keywords internal
read.xlsx_XLConnect <- function(filename, sheet=1, ...){
  # This function reads xls and xlsx files into a data.frame. Package XLConnect must be installed.
  # Sheet can also be given as character. If all sheets of the workbook should be imported, rather use loadWorkbook{XLConnect}
  # http://www.r-bloggers.com/read-excel-files-from-r/
  installFromCRAN("XLConnect") # #options ( java.parameters = "-Xmx1024m ")

  wb = XLConnect::loadWorkbook(filename, create=FALSE)
  return( XLConnect::readWorksheet(wb, sheet=sheet, ...) )
}

#mat <- matrix(rep(c("\"b\"&char(10)&\"b\""),10), ncol=2); colnames(mat) <- c("test1","test2"); rownames(mat) <- 1:nrow(mat); data <- list(mat,mat); names(data) <- c("sheet1","sheet2");
#file="testfile.xlsx"; sheetName=NULL; row.names=TRUE; col.names=TRUE; append=FALSE; asFormula=TRUE; wrapText=FALSE; columnWidth=100; convertColumnWith="default"
#write.xlsx(file=file, data=data, wrapText=TRUE, columnWidth=list(50, c(100,400)), asFormula=list(c(TRUE,FALSE),FALSE), convertColumnWith="pixel")
#' @title Writes an Excel file from either a matrix/data.frame or from a list conaining several matrices/data.frames.
#' @description It is a convenient version of writeWorksheetToFile{XLConnect}
#' @export
#' @author Daniel Hoop
#' @param data A matrix/data.frame or list containing matrices/data.frames
#' @param file The path to the xlsx file
#' @param sheetName Optional sheet names in the Excel file. If \code{NULL}, then names Sheet1, Sheet2, etc. are given.
#' @param row.names Logical value indicating if rownames should be written or character giving the header for rowname colum. Single value or list.
#' @param col.names Logical value indicating if a header should be written. Single value or list.
#' @param asFormula Logical value indicating if formulas should be written rather than values. Note that no \code{"="} sign at the beginning of the cell is required.
#' @param append Logical value indicating if the excel file should be appended rather than newly created.
#' @param wrapText Logical value indicating if the text in the cells should be wrapped (Zeilenumbruch).
#' @param columnWidth The column width that will be applied to all columns. If -1, then auto-width is applied.
#' @param convertColumnWith If \code{"no"}, then the standard of XLConnect is used. It is the 1/256 part of a character. \code{"default"} will convert in 1 character units. \code{"pixel"} will convert to number of pixel.
write.xlsx <- function(data, file, sheetName=NULL, row.names=FALSE, col.names=TRUE, append=FALSE, asFormula=FALSE, wrapText=FALSE, columnWidth=-1, convertColumnWith=c("default","pixel","no")){

  installFromCRAN("XLConnect") # options ( java.parameters = "-Xmx1024m ")

  convertColumnWith <- match.arg(convertColumnWith)
  if(is.matrix(data) || is.data.frame(data)) data <- list(data)
  if(is.null(names(data))) names(data) <- paste0("Sheet", 1:length(data))
  if(is.null(sheetName) && any(duplicated(names(data)))) stop("Each list place in data must contain a unique name. Otherwise speccify the sheetName argument.")

  if(!is.null(sheetName)){
    if(is.list(sheetName)) sheetName <- unlist(sheetName)
    if(is.list(data) && !is.data.frame(data)) {
      if(length(data)!=length(sheetName)) stop("length(data) has to be equal length(sheetName)")
    }
    if(any(duplicated(unlist(sheetName)))) stop("There must be no duplicated sheet names. Otherwise the sheets will be overwritten.")
  } else {
    sheetName <- names(data)
  }
  expandListPlaces <- function(x){
    for(i in 1:length(x))
      if(!is.null(data[[i]]) && length(x[[i]])==1) x[[i]] <- rep(x[[i]], ncol(data[[i]]))
      return(x)
  }
  convertRowColNames <- function(nam, errorNam="names", expand=FALSE, allowCharacters=FALSE, checkDimFunc=NULL) { # trueVal=TRUE, falseVal=FALSE,
    if(!is.list(nam)) nam <- list(nam)
    if(!allowCharacters) if(any(sapply(nam,function(x)!is.logical(x)))) stop(errorNam, " must be logical.")
    if(length(nam)==1) nam <- lapply(sheetName, function(x)nam[[1]])
    if(expand) nam <- expandListPlaces(nam)
    if(length(nam)!=length(sheetName)) stop(paste0("length(",errorNam,") must be either 1 or length(data). If you want to specify for each column try different things like c(2,10,15) or list(c(5,10,15)) or list(TRUE,FALSE,TRUE)"))
    if(!is.null(checkDimFunc)){
      errorFlag <- FALSE;
      for(i in 1:length(nam)) errorFlag <- errorFlag | ( !is.null(data[[i]]) && length(nam[[i]])>1 && length(nam[[i]])!=ncol(data[[i]]) )
      if(errorFlag) stop(paste0("if ", errorNam, " are specified separately in a list place for each file, then they must be either of length 1 or equal to ncol of data."))
    }
    return(nam)
  }
  rownames <- convertRowColNames(row.names, errorNam="row.names", expand=FALSE, allowCharacters=TRUE, checkDimFunc=NULL)#, trueVal="", falseVal=NULL)
  rownames <- lapply(rownames,
                     function (x) if (is.logical(x) && x) "" else if (is.logical(x) && !x) NULL else x)
  header <- convertRowColNames(col.names, errorNam="col.names", expand=FALSE, allowCharacters=FALSE, checkDimFunc=NULL)#, trueVal=TRUE, falseVal=FALSE)
  asFormula <- convertRowColNames(asFormula, errorNam="asFormula", expand=TRUE, allowCharacters=FALSE, checkDimFunc=ncol)#, trueVal=TRUE, falseVal=FALSE)
  columnWidth <- convertRowColNames(columnWidth, errorNam="columnWidth", expand=FALSE, allowCharacters=TRUE, checkDimFunc=ncol)#, trueVal="", falseVal="",  )
  columnWidth <- lapply(columnWidth, function(x){
    if(x == -1) x else
      if(convertColumnWith=="default") x * 256 else
        if(convertColumnWith=="pixel") x * 256 / 7 else
          x
  })


  if(substr.rev(file,1,4)==".xls") {
    warning("`file` must end with xlsx. The file extension was changed.")
    file <- paste0(file,"x")
  }
  if(file.exists(file) && !append) file.remove(file)

  # Create the file and write...
  if(!wrapText && sum(unlist(asFormula))==0 && all(unlist(columnWidth)==-1)){
      wb <- XLConnect::loadWorkbook(filename=file, create=!append)
      for(i in 1:length(data)) {
        wb$createSheet(name=sheetName[i])
        wb$writeWorksheet(data=data[[i]], sheet=sheetName[i], header=header[[i]], rownames=rownames[[i]])
      }
      XLConnect::saveWorkbook(wb)

  } else {
    wb <- XLConnect::loadWorkbook(filename=file, create=!append)
    if(wrapText){
      cs <- wb$createCellStyle()
      cs$setWrapText(wrap=TRUE)
    }
    for(i in 1:length(data)){ # i <- 1
      wb$createSheet(name=sheetName[i])
      if(!is.null(data[[i]])){

        wb$writeWorksheet(data=data[[i]], sheet=sheetName[i], header=header[[i]], rownames=rownames[[i]])
        addR <- as.numeric(header[[i]])
        if(any(asFormula[[i]]))
          for(c1 in 1:ncol(data[[i]]))
            if(asFormula[[i]][c1])
              for(r1 in 1:nrow(data[[i]]))
                wb$setCellFormula(sheet=sheetName[i], formula=data[[i]][r1,c1], row=r1+addR, col=c1)

        if(wrapText)
          for(c1 in 1:ncol(data[[i]]))
            for(r1 in 1:nrow(data[[i]]))
              wb$setCellStyle(sheet=sheetName[i], cellstyle=cs, row=r1+addR, col=c1)

        if(any(columnWidth[[i]] != -1)){
          if(length(unique(columnWidth[[i]]))==1) {
            wb$setColumnWidth(sheet=sheetName[i], column=1:ncol(data[[i]]), width=columnWidth[[i]])
          } else {
            for(c1 in 1:ncol(data[[i]]))
              if(columnWidth[[i]][c1] != -1)
                wb$setColumnWidth(sheet=sheetName[i], column=c1, width=columnWidth[[i]][c1])
          }
        }

      }
    }
    XLConnect::saveWorkbook(wb)
  }
}

#path <- "C:/Users/U80823148/_/ME/ME_data_out/data_final"
list.nodirs <- function(path = ".", ...){
  # List all files but no directories.
  fil <- list.files(path, ...) # ...
  return( fil[!file.info(fil)$isdir] )
}

#zipfile <- "C:/Users/U80823148/_/ME/ME_data_out/data_final/2014/allcosts_info.zip"
#remove.original=TRUE; showWarnings=TRUE
#z1ip.nodirs(zipfile, files, remove.original=TRUE, showWarnings=FALSE)
zip.nodirs <- function(zipfile, files, remove.original=FALSE, showWarnings=TRUE, invoked.internally=FALSE,
                       RtoolsBin=agsPath(paste0(c("C:/Program Files","C:/Tools","//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000/driversEtc","//evdad.admin.ch/AGROSCOPE_OS/0/4/5/3811/Dokumente/R_Uebung/sonstigeFiles/Rtools/bin"),"/Rtools/bin")), ...){
  # This function packs all files with full path names into a zip file that will not contain the subfolders.
  # Arguments
  # zipfile         = The zipfile containing all compressed files.
  # files           = All files to be packed into the zip file.
  # remove.original = Should original files be removed after they were packed into the zip file? If there was an error while zipping, the original files will not be removed.
  # showWarnings    = Should warnings be showed if the zipfile already exists?
  # Details
  # The implementation of this function could be much easier. The zip() function offers the possibility to hand over options for the zip utility that will be called.
  #    This zip utility can be forced to ignore the folder structures and put all files "flat" into the zip archive.

  # Check if zipfile already exists.
  if(file.exists(zipfile)){
    if(file.info(zipfile)$isdir) stop("zipfile must not be a directory.")
    if(showWarnings) {
      if(remove.original) stop("zipfile already exists. Because remove.original==TRUE, for safety reasons, the zipping was stopped. Set showWarnings=FALSE to ignore this message.")
      warning("zipfile already existed. Content was updated.")
    }
  }

  # Assure fully qualified filenames.
  zipfile <- winSlashes(getFullyQualifiedFileName(zipfile))
  files <- getFullyQualifiedFileName(files)

  # Save original working directory.
  wd <- getwd()
  # Split filenames by / or \
  fil <- strsplit(files, "/|\\\\")

  # Safetycheck for duplicated filenames that would be overwritten within the zipfile.
  # x <- fil[[1]]
  filcheck <- unlist(lapply(fil,function(x)x[length(x)]))
  if(any(duplicated(filcheck))){
    stop(paste0("Some filenames are identical. Duplicated filenames would be overwritten within the zip. Please choose unique filenames of the following files:\n", paste0(unique(filcheck[duplicated(filcheck)]),collapse=", ")))
  }

  # Prepare function to call in case of error or warning
  errorFunc <- function(filename){
    setwd(wd)
    if(invoked.internally && remove.original) suppressWarnings(file.remove(files))
    if( isTRUE(!file.info(zipfile)$isdir) ) suppressWarnings(file.remove(zipfile)) # Remove zip anyway.
    stop(paste0(filename, " was not zipped!", if(!invoked.internally && remove.original) " Original files were *NOT* removed!"))
  }

  # Expand System PATH variable wtih Rtools/bin to ensure that zip.exe is available.
  .expand.Sys.env.path(RtoolsBin)

  # Pack all files into zipfile.
  # x <- fil[[1]]
  lapply(fil, function(x){
    filename <- x[length(x)]
    if(length(x)>1){ # If there is more then 1 part in path, then it's the full path, not only filename.
      parentdir <- paste(x[-length(x)],collapse="/")
      setwd(parentdir)
    }
    tryCatch( zip(zipfile, filename),
              error=function(e)errorFunc(filename),
              warning=function(w)errorFunc(filename))
  })

  # Set original working directory.
  setwd(wd);
  # Remove original files if wished.
  if(remove.original) invisible(file.remove(files))
}

#' Convert xml2 nodes to a matrix.
#' @param nodes The xml nodes.
#' @return A data.frame
#' @examples
#' xml <- "
#' <Root>
#'   <Attributes>
#'     <Attribute><name>a</name><value>1</value></Attribute>
#'     <Attribute><name>b</name><value>2</value></Attribute>
#'   </Attributes>
#' </Root>"
#'
#' xml2NodesToMatrix(xml2::xml_find_all(xml2::read_xml(xml), ".//Attribute"))
xml2NodesToMatrix <- function(nodes) {
  if (class(nodes)[1] == "xml_nodeset")
    nodes <- xml2::as_list(nodes)
  if (class(nodes)[1] != "list")
    stop("`class(nodes)` must be either 'xml_nodeset' or 'list'.")
  allNames <- unique(unlist(lapply(nodes, names)))
  rawVec <- rep("", length(allNames))
  names(rawVec) <- allNames
  return(do.call("rbind", lapply(nodes, function(node) {
    tmp <- rawVec
    nam <- names(node)
    tmp[nam] <- unlist(node)
    tmp
  })))
}

#' Get the index of a xml node by providing the name and attribute of that node.
#' @param xml The xml root node (derived by XML::xmlRoot(XML::xmlParse(filename)))
#' @param name The name of the node that shall be searched. length(name) must be 1.
#' @param attr The attributes of the node that shall be searched. length(attr) can be greater than 1.
#' @return A numeric vector with the indexes that match the given search criteria.

# name <- "Table"; attr <- "CostunitZA"; xml <- XML::xmlRoot(XML::xmlParse(agsPath("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/ZA-BH_Kontenplan/za-bh-accountsystem.xml")))
getXmlNodeIndexByNameAndAttr <- function (xml, name, attr) {
  installFromCRAN("XML")
  if (is.null(names(xml)))
    stop ("The XML does not have any names. You probably need to take the root first by using the function XML::xmlRoot.")
  # Get the matching name. If no name matches, return a empty index of length 0.
  nameMatches <- which(names(xml) %in% names(xml))
  if (length(nameMatches) == 0)
    return (nameMatches)
  # If some name match, then choose those which also match all attributes.
  attr <- sort(attr)
  attrMatches <- nameMatches[ apply(matrix(nameMatches), 1, function(n) {
    att <- XML::xmlAttrs(xml[[n]])
    att <- sort(att)
    if (length(attr) == length(att) && all(attr == att))
      return (TRUE)
    return (FALSE)
  }) ]
  return (attrMatches)
}

.expand.Sys.env.path <- function(path){
  # This function checks if a path exists. If so, and the path is not yet part of the environment PATH variable, then the environment PATH will be expanded.
  # The function is needed, e.g. inside zip.nodirs()
  if (!grepl("window", Sys.info()["sysname"], ignore.case=TRUE))
    stop ("This function currently only works on windows because the Linux PATH is separated using colons instead of semi-colons.")

  path <- path[!is.null(path)]
  path <- path[file.exists(path)]
  if(length(path)>0) {
    PATH <- Sys.getenv("PATH")
    path <- winSlashes(path)
    path <- path[ !path %in% unlist(strsplit(PATH, ";", fixed=TRUE)) ]
    if(length(path)>0)
      Sys.setenv(PATH=paste0(path, ";", PATH))
  }
}

#filename <- "Eink_A/delete"
getFullyQualifiedFileName <- function(filename){
  # This function checks if a fully qualified filename was given.
  # If not, then the working directory is pasted to the filename.
  # Arguments
  # filename = Charactor vector containing the filename(s) to be checked.

  # Recursive definition in case several filenames where given
  if(length(filename)>1) return( apply(matrix(filename),1,getFullyQualifiedFileName) )

  filename <- path.expand(filename)
  filename <- gsub("^file:[\\/]{3}", "", filename)
  # If the splitted filename with / only is of length one, then it can only be a single filename without preceding folder.
  if(length( strsplit(filename, "/|\\\\")[[1]]  )==1){
    return( paste(getwd(),filename,sep="/") )
  } else {
    # Check if the filename is valid only together with the current wd. If so, then expand the filename.
    wd <- getwd() # Store working directory.
    setwd(tempdir()) # Set wd to tmp.
    if(file.exists(filename)){  # If file exists, then it is an absolute path
      setwd(wd)
      return( filename )
    }
    # Now try to write a file. If filename is not fully qualified, then this should give an error.
    newFilename <- NULL
    newFilename <- tryCatch(suppressWarnings( write(1,filename) ),
                            error=function(e) return(paste(wd,filename,sep="/")) )
    suppressWarnings(file.remove(filename))
    if(!is.null(newFilename)) filename <- newFilename
    setwd(wd)
    # Alternative but slower on network drives. Original takes 1.54 secs for 1000 files. This one takes 6.86 secs for 1000 files.
    #appendWd <- tryCatch({ suppressWarnings(write(1,paste0(getwd(),"/",filename))); suppressWarnings(file.remove(filename)); return(FALSE) },  error=function(e) return(TRUE) )
    #if(appendWd) filename <- paste0(getwd(),"/",filename)
  }

  return(filename)
}

splitFileNameIntoParts <- function(filename){
  # This funciton splits a filename into the directory, filename and extension.
  # It returns data.frame(dir=..., file=..., extension=...)

  if(length(filename)>1) return( as.data.frame(do.call("rbind", lapply(filename, splitFileNameIntoParts)), stringsAsFactors=FALSE) )

  # Get the parent directory.
  dir <- dirname(filename)

  # Grasp filename and extension
  fileName1 <- basename(filename)
  fileName2 <- strsplit(fileName1,"\\.")[[1]]
  if(length(fileName2)>1){
    fileName <- paste(fileName2[1:(length(fileName2)-1)],collapse=".")
    extension <- fileName2[length(fileName2)]
  } else {
    fileName <- fileName1
    extension <- ""
  }
  # Grasp filename and extension. ALTERNATIVE. This is not as reliable. Does not work for filenames that start with a dot. Like ".hiddenLinuxFile"
  #filename <- basename(filename) # Without directory
  #file <- tools::file_path_sans_ext(filename)
  #extension <- tools::file_ext(filename)

  # Return result
  return(data.frame(dir=dir, file=fileName, extension=extension, stringsAsFactors=FALSE))
}

#x <- c("O:\\Sites\\TA\\Transfer\\hpda\\R","O:\\Sites\\TA\\Transfer\\hpda\\R", "\\\\evdad.admin.ch\\asdf\\")
winSlashes <- function(x, dontCheckOS=FALSE){
  # This function checks if the system is Windows. If so, then frontslashes are replaced with backslashes.

  if(any(is.na(x)))
    stop("None of paths must be NA.")
  if(!dontCheckOS && !grepl("window",Sys.info()["sysname"],ignore.case=TRUE))
    return(x)

  x <- gsub("/","\\\\",x)

  netDrive <- startsWith(x,"\\")
  x <- gsub("\\\\+","\\\\",x)
  x[netDrive] <- paste0("\\",x[netDrive])

  delEnd <- endsWith(x,"\\")
  x[delEnd] <- substr(x[delEnd], 1, nchar(x[delEnd])-1)
  return(x)
}

makeBackupOfFile <- function(file, backupSubFolder="backup", timeAccuracy=c("s","m","h","d"), removeOriginal=FALSE){
  # This function makes a copy of a file in the backupSubFolder. The copy is appended with the date and time of the last changed file info.
  # E.g. C:/test/file.txt will be moved to C:/test/old/file_2017-01-01_10h.txt
  #
  # Arguments
  # file =         The filename of the file to make a backup of.
  # backupSubFolder = The backup sub directory where the backup file should be put into.
  # tmieAccuracy = The time accuray to put after the backuped filename. s="date-hour-minute-second", m="date-hour-minute", h="date-hour", d="date"

  # Recursive definition in case several arguments are given.
  if(length(file)>1)
    return(apply(matrix(file),1,function(x)makeBackupOfFile(file=x, backupSubFolder=backupSubFolder, timeAccuracy=timeAccuracy, removeOriginal=removeOriginal)))

  if(file.exists(file)) {
    # Time accuracy pre calculations
    timeAccuracy <- match.arg(timeAccuracy)
    timeSubstr <- if(timeAccuracy=="s") 19 else if(timeAccuracy=="m") 16 else if(timeAccuracy=="h") 13 else if(timeAccuracy=="d") 10
    timeExt <- if(timeAccuracy=="s") "" else if(timeAccuracy=="m") "m" else if(timeAccuracy=="h") "h" else if(timeAccuracy=="d") ""
    # Split filename into Parts
    path <- splitFileNameIntoParts(file)
    path[path[,"extension"]!="","extension"] <- paste0(".",path[path[,"extension"]!="","extension"])
    # Create backupSubFolder
    pathBak <- paste0(path[,"dir"],"/",backupSubFolder,"/")
    suppressWarnings(dir.create(pathBak))
    # Append the time
    mtime <- gsub(" ","_",substring(as.character(file.info(file)$mtime),1,timeSubstr) )
    mtime <- gsub(":","-",mtime)
    fileNew <- paste0(pathBak,path[,"file"],"_",mtime,timeExt,path[,"extension"])
    # If there already was an old backup then remove it. Afterwards move/copy the new one.
    suppressWarnings(file.remove(fileNew))
    copyHasWorked <- if(removeOriginal) file.rename(file, fileNew) else file.copy(file, fileNew)
    return(copyHasWorked)
  } else {
    return(FALSE)
  }
}


#### CORRELATIONS, REGRESSIONS ####
####
#data <- gb; sig.level=c(0.1,0.05,0.01,0.001)[2]; method=c("pearson", "kendall", "spearman"); conf.level=NULL; conf.middle=TRUE; conf.middle.sign=TRUE; digits=2; triangle=FALSE; del.diag=TRUE; ignore.nonsig=FALSE; suppressWarnings=FALSE;
#colnames(gb)[183]
#gb[1:10,183]

#' Calculate correlation talbe and show signification signs.
#' @export
#' @author Daniel Hoop
cor.table <- function(data, sig.level=c(0.1, 0.05, 0.01, 0.001)[2], method=c("pearson", "kendall", "spearman"), conf.level=NULL, conf.middle=TRUE, conf.middle.sign=TRUE, digits=2, triangle=FALSE, del.diag=TRUE, ignore.nonsig=FALSE, suppressWarnings=FALSE, count=FALSE, special.sign.before.nonsig.for.excel=FALSE, ...){
  method <- match.arg(method)
  if(!is.null(conf.level)) if(method!="pearson") stop("confidence intervals can only be calculated with the pearson method")

  if(is.matrix(data)) if(!is.numeric(data))     stop("The matrix isn't numeric")
  if(is.data.frame(data)) {
    char.cols <- !sapply(data,function(x)is.numeric(x))
    if(any(char.cols)) {
      invisible(colnames(data)[char.cols])
      cat("\n")
      dput(colnames(data)[char.cols])
      stop("Correlations for characters or factors can't be calculated", call.=FALSE)
    }
  }
  p <- matrix(NA,nrow=ncol(data),ncol=ncol(data)); rownames(p) <- colnames(p) <- colnames(data)
  cors <- sign <- p
  # Calculating the correlations and the p values of the correlations
  for(i in 1:ncol(data)){
    for(j in 1:i){
      if(count)
        print(paste("i=",i,",   j=",j))
      if(!suppressWarnings) {
        ct <- cor.test(data[,i], data[,j], method=method)
      } else {
        ct <- suppressWarnings(cor.test(data[,i], data[,j], method=method))
      }
      p[i,j] <- ct$p.value
      cors[i,j] <- ct$estimate
    }
  }
  # Der Bereich ?ber der Matrix-Diagonale wird mit dem selben unterhalb der Diagonale gef?llt, weil diese Teile der Matrix gleich sind
  tp <- t(p)
  p[which( row(p)-col(p) <0)] <- tp[which( row(tp)-col(tp) <0)]
  tcors <- t(cors)
  cors[which( row(cors)-col(cors) <0)] <- tcors[which( row(tcors)-col(tcors) <0)]

  cors <- round(cors,digits)
  # setting the signs for p-levels
  if(sig.level>=0.1) {
    sign[p<0.1 & cors>=0] <- "  . "
    sign[p<0.1 & cors<0]  <- "  ."
  }
  if(sig.level>=0.05) {
    sign[p<0.05 & cors>=0] <- "  * "
    sign[p<0.05 & cors<0]  <- "  *"
  }
  if(sig.level>=0.01) {
    sign[p<0.01 & cors>=0] <- " ** "
    sign[p<0.01 & cors<0]  <- " **"
  }
  if(sig.level>=0.001) {
    sign[p<0.001 & cors>=0] <- "*** "
    sign[p<0.001 & cors<0]  <- "***"
  }
  if(!special.sign.before.nonsig.for.excel){
    sign[is.na(sign) & cors>=0] <- "    "
    sign[is.na(sign)& cors<0]   <-  "   "
  } else {
    sign[is.na(sign) & cors>=0] <- "????"
    sign[is.na(sign)& cors<0]   <-  "???"
    message("Export the table as .csv to Excel, then replace ? by Alt+255\n")
  }

  # combination of the signs with the correlation values
  comb <- paste(sign,cors)#, ...)
  print.table <- matrix(comb, nrow=ncol(data), ncol=ncol(data), dimnames=list(colnames(cors),rownames(cors)))
  # diag(print.table) <- "     1"

  # Calculating the conficence intervals
  if(!is.null(conf.level)){
    if(method!="pearson") stop("confidence intervals can only be calculated with the pearson method")
    uintv <- matrix(NA, nrow=ncol(data), ncol=ncol(data))
    colnames(uintv) <- colnames(data); rownames(uintv) <- colnames(uintv)
    lintv <- uintv
    for(i in 1:ncol(data)){
      for(j in 1:ncol(data)){
        if(count) print(paste("i=",i,",   j=",j))
        if(!suppressWarnings) {    ct <- cor.test(data[,i], data[,j], method=method, conf.level=conf.level)  #, ...)
        } else {  ct <- suppressWarnings(cor.test(data[,i], data[,j], method=method, conf.level=conf.level))}#, ...)) }
        lintv[i,j] <- ct$conf.int[1]
        uintv[i,j] <- ct$conf.int[2]
      }
    }
    # Der Bereich ?ber der Matrix-Diagonale wird mit dem selben unterhalb der Diagonale gef?llt, weil diese Teile der Matrix gleich sind
    tlintv <- t(lintv);  lintv[which( row(lintv)-col(lintv) <0)] <- tlintv[which( row(tlintv)-col(tlintv) <0)]
    tuintv <- t(uintv);  uintv[which( row(uintv)-col(uintv) <0)] <- tuintv[which( row(tuintv)-col(tuintv) <0)]

    uintv <- round(uintv,digits)
    lintv <- round(lintv,digits)
    conf.int <- NULL
    # Adding the estimate in the middle of the lower and upper interval value
    if(conf.middle) {
      # ... with *** signs
      if(conf.middle.sign) {
        uintv <- matrix(paste("     ",uintv,sep=""),ncol=ncol(data))
        lintv <- matrix(paste("     ",lintv,sep=""),ncol=ncol(data))
        for(i in 1:ncol(uintv)) conf.int <- rbind(conf.int,uintv[i,],print.table[i,],lintv[i,],rep(NA,ncol(data)))
        # ... without *** signs
      } else if (!conf.middle.sign) {
        for(i in 1:ncol(uintv)) conf.int <- rbind(conf.int,uintv[i,],cors[i,],lintv[i,],rep(NA,ncol(data)))
      }
      rnames <- NULL
      for(i in 1:ncol(uintv)) rnames <- c(rnames,rep(colnames(data)[i],3),NA)
      rownames(conf.int) <- rnames
      # Setting values NA which should not be printed
      if(ignore.nonsig) {
        p.long <- NULL
        for(i in 1:ncol(data)) p.long <- rbind(p.long,matrix(rep(p[i,],3),nrow=3,byrow=TRUE),rep(1,ncol(data)))
        conf.int[p.long>=sig.level] <- NA
      }
      if(triangle) {
        del <- NULL
        for(i in 2:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int)+1 )  : ( (i-1)*nrow(conf.int) +(i-1)*4 )  )
        conf.int[del] <- NA
      }
      if(del.diag) {
        del <- NULL
        for(i in 1:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int) +(i)*4 -3 )  : ( (i-1)*nrow(conf.int) +(i)*4 )  )
        conf.int[del] <- NA
      }
      # Table Intervals without the estimates in the middle
    } else if (!conf.middle) {
      rnames <- NULL
      for(i in 1:ncol(uintv)) {conf.int <- rbind(conf.int,uintv[i,],lintv[i,],NA)
      rnames <- c(rnames,rep(colnames(data)[i],2),NA) }
      rownames(conf.int) <- rnames

      # Setting values NA which should not be printed (in the case without estimate in the middle)
      if(ignore.nonsig) {
        p.long <- NULL
        for(i in 1:ncol(data)) p.long <- rbind(p.long,matrix(rep(p[i,],2),nrow=2,byrow=TRUE),rep(1,ncol(data)))
        conf.int[p.long>=sig.level] <- NA
      }
      if(triangle) {
        del <- NULL
        for(i in 2:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int)+1 )  : ( (i-1)*nrow(conf.int) +(i-1)*3 )  )
        conf.int[del] <- NA
      }
      if(del.diag) {
        del <- NULL
        for(i in 1:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int) +(i)*3 -2 )  : ( (i-1)*nrow(conf.int) +(i)*3 )  )
        conf.int[del] <- NA
      }
    }
    conf.int <- conf.int[1:(nrow(conf.int)-1),]
  }

  # Im Fall, dass keine P-Werte berechnet werden konnten, werden diese Pl?tze in der Matrix als NA ausgegeben
  print.table[is.na(p)|is.na(cors)] <- NA
  # Sonstige konditionale NA-Belegunten
  if(ignore.nonsig)  print.table[p>sig.level] <- NA
  if(triangle) print.table[which( row(print.table)-col(print.table) < 0)] <- NA
  if(del.diag) diag(print.table) <- NA

  if(is.null(conf.level)) {
    result <- list(cor=cors,p.val=p,sign=sign,print.table=print.table,conf.int=NULL)
  } else if(!is.null(conf.level)){
    result <- list(cor=cors,p.val=p,sign=sign,print.table=print.table,conf.int=conf.int)
  }
  class(result) <- "cor.table"
  return(result)
}
print.cor.table <- function(x, quote=FALSE, na.print="", ...){
  print(x$print.table, quote=quote, na.print=na.print, ...)
}

#' Deprecated. Moved to wibe::wibe.
#' @examples
#' agsGitlabUtils::updatePackage("wibe")
#' wibe::wibe(...)
plm.within.between <- function(...) {
  stop("This function has been moved to the package 'wibe'. The package is available on Agroscope GitLab, and you can install it by executing `agsGitlabUtils::updatePackage(\"wibe\").`")
}

####
predict.plm <- function(model, newdata=NULL){

  if(is.matrix(newdata)) newdata <- as.data.frame(newdata) else if(!is.null(newdata) & !is.data.frame(newdata)) stop("newdata must be NULL, matrix or data.frame!")
  if(is.null(model$fitted.values)) model$fitted.values <- model$model[,1]-model$residuals

  if(is.null(newdata)) {
    return(model$fitted.values)
  } else {
    # Not elegant: if(is.matrix(newdata)) newdata <- as.data.frame(new.data)
    coef0 <- mean(lme4::fixef(model))
    coefs <- model$coefficients
    # Neuen Datensatz erstellen:
    if( length(coefs)!=ncol(newdata) || any(names(coefs)!=colnames(newdata)) ){
      form <- model$formula
      class(form) <- "formula"
      newdata <- model.matrix(form, newdata)[,-1,drop=FALSE] # ohne Intercept
    }
    # Calculating fitted values:
    y <- as.vector(coefs %*% t(newdata)) + coef0
    # Not elegant but does the same (much slower!)
    # y <- as.vector( apply(coefs*newdata,1,function(x)sum(x)) )
    # y <- y+coef0
    return(y)
  }
}
# Predict rlm wie plm aber mit Intercept, falls vorhanden
#predict.rlm <- predict.plm

#' @title Makes a variable time invariant. E.g. if an observation has mostly value 1, but sometimes 2. The value 2 is rather a measurement error than the true value.
#' @description In this case, the value 2 will be replaced by either 1 (\code{method = "mostFrequent"}), 1.xxx (\code{method = "mean"}) or the median value (\code{method = "median"}).
#' @export
#' @author Daniel Hoop
#' @param x The vector of values that shall be checked. If a matrix/data.frame, then the function will be applied to all columns.
#' @param index The index, e.g. the id of each observation.
#' @param method The method to be applied.
#' @return
#' The corrected vector \code{x}.
#' @examples
#' x <-     c(1,1,2, 3,3,4)
#' index <- c(0,0,0, 1,1,1)
#' make.variable.time.invariant(x, index)
#' # [1] 1 1 1 3 3 3
make.variable.time.invariant <- function(x, index, method=c("mostFrequent","mean","median")){

  if(is.matrix(x)) return(apply(x,2,function(x)make.variable.time.invariant(x=x,index=index,method=method))) else
    if(is.data.frame(x)) return(as.data.frame(lapply(x,function(x)make.variable.time.invariant(x=x,index=index,method=method)),stringsAsFactors=FALSE))

  method <- match.arg(method)
  if(is.factor(x)) stop("The procedure does not work for factors. You have to convert into numeric/integer/character first.")

  index <- .paste.elements(index, sep="_a@:!;q_", errorMsg="All indices must have same length!")
  fun <- if(method=="mostFrequent") function(x) names(table(x)[1]) else if(method=="mean") function(x) mean(x) else if(method=="median") function(x) median(x)

  res <- tapply(x, index, fun)
  res <- res[ match(as.character(index), names(res)) ]

  if(is.integer(x)) return(as.integer(res)) else
    if(is.numeric(x)) return(as.numeric(res)) else
      return(res)
}

####
extract.regression.p <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)}
####
polyregressionplot <- function(data,dependent,independent,degree=NULL,degrees=NULL,root=FALSE,roots=NULL,intercept=FALSE,sig.level=0.05,model=c("ols","robust"),datapoints=1000,na.replace=NULL,invert=FALSE,grid=FALSE,plotrows=1,maxit=1000,psi=c(psi.huber, psi.hampel, psi.bisquare)[1],indeplimit=NULL,deplimit=NULL,main=NULL,sub=NULL,indeplab=NULL,deplab=NULL,cex.main=NULL,cex.sub=NULL,cex.lab=NULL,cex.axis=NULL,linecol="black",linewidth=1.5,scatter=FALSE,dotcol="black",dotsize=0.5, ...){
  # Explanation: This function makes a regression with the dependent and independent variables. If you use degree>1, then a non-linear regression is fitted.
  # witt degrees you can specify if some variables sould have a degree of 1-max(degree). Then a polynom is made from 1-degrees.
  # Use root=TRUE if some variables should be used not with ^degree but with ^(1/degree). With roots you can specify which variables should be calculated as roos (1) and which not (0).
  # Example for 3 variables: degree=2, degrees=c(1,2,2), root=TRUE, roots=c(1,0,0). Use the minimum significance level to kick out the calculated coefficients which are below this value.

  #data=datas;dependent=c("DABS_fk_ArbeitDritte_index");independent=c("DABS_SAK_neu");degree=3;degrees=NULL;root=FALSE;roots=NULL;intercept=TRUE;model=c("ols","robust")[1];sig.level=1;na.replace=NULL;invert=FALSE;grid=FALSE;plotrows=2;maxit=1000;psi=c(psi.huber, psi.hampel, psi.bisquare)[1];indeplimit=NULL;deplimit=NULL;indeplab=NULL;deplab=NULL;linecol="black";linewidth=1;dotcol="black";dotsize=0.5
  #degree=3;degrees=c(0,0,1)
  model <- match.arg(model)
  if(length(dependent)>1) stop("give only 1 dependent variable")
  if(is.null(degree)) degree <- 1
  if(!is.null(sig.level)&model=="robust") warning("there are no significance levels for coefficients calculated with the robust model rlm{MASS}")
  if(class(dependent)!="character"|class(independent)!="character") stop("Dependent and independent must be characters naming the colnames of data to work with")
  if(!all(c( is.null(indeplimit)|is.list(indeplimit)), is.null(deplimit)|is.list(deplimit) )) stop("indeplimit and deplimit must be NULL or a list of length(independent) each containing 2 elements")
  if(!is.null(degrees)&length(degrees)!=length(independent)&length(degrees)!=length(independent)*degree) stop("length(degrees) must be equal length(independent) -> short form. or length(independent)*degree -> long form")
  if(!is.null(degrees)) {
    if(length(degrees)==length(independent)*degree&max(degrees)>1) stop("written in this long form degrees must only contain 0 or 1")
    if(max(degrees)>degree) stop("max(degrees) must be smaller than degree") }
  if(!is.null(roots)&length(roots)!=length(independent)) stop("length(roots) must be equal length(independent)")
  if(!is.null(roots)) if(max(roots)>1) stop("roots must only contain 0 or 1")

  if(is.null(degrees)) degrees <- rep(degree,length(independent))
  if(length(degrees)!=length(independent)*degree) {
    degreesv <- vector("list",length(degrees))
    for (i in 1:length(degrees)) degreesv[[i]] <- c(rep(1,degrees[i]),rep(0,degree-degrees[i]))
    degrees <- do.call("c",degreesv)
  }

  if(!root) {roots <- rep(0,length(independent)*degree)} # ; roots.short <- rep(0,length(independent))    ist unnoetig! es gibt nur das roots, ohne lange version.
  if(is.null(roots)&root) {roots <- rep(1,length(independent)*degree)} # ; roots.short <- rep(1,length(independent))
  if(!is.null(roots)&root){
    rootsv <- vector("list",length(degrees))
    for (i in 1:length(roots)) {
      if (roots[i]==1) rootsv[[i]] <- rep(1,degree)
      if (roots[i]==0) rootsv[[i]] <- rep(0,degree)
    }
    roots <- do.call("c",rootsv)
  }

  independent.multi <- vector("list", length(independent))
  for(i in 1:length(independent)) independent.multi[[i]] <- rep(independent[i],degree)
  independent.multi <- do.call("c",independent.multi)
  degreesform <- rep(1:degree,length(independent))

  counter <- integer(0)
  for (i in length(degrees):1) if(degrees[i]!=0) counter[i] <- i
  counter <- max(counter[!is.na(counter)])
  form <- paste(dependent, "~")
  for (i in 1:length(degrees)) {
    if (degrees[i]==1) {
      if(i!=counter) {
        if(roots[i]==0) form <- paste(form, " I( ",independent.multi[i],"^",degreesform[i]," ) +" ,sep="")
        if(roots[i]==1) form <- paste(form, " I( ",independent.multi[i],"^(1/",degreesform[i],") ) +" ,sep="")
      } else {
        if(roots[i]==0) form <- paste(form, " I( ",independent.multi[i],"^",degreesform[i]," )" , sep="", collapse="")
        if(roots[i]==1) form <- paste(form, " I( ",independent.multi[i],"^(1/",degreesform[i],") )", sep="", collapse="")
      }
    }
  }

  if (model=="robust") {
    require.package("MASS")
    fit <- rlm(form,data=data,maxit=maxit,psi=psi)}
  if (model=="ols")
    fit <- lm(form,data=data)
  co <- fit$coefficients
  if(!is.null(sig.level)&model=="ols")   co[summary(fit)$coefficients[,4]>sig.level] <- 0
  if(intercept) interc <- co[1] else interc <- 0
  co <- co[2:length(co)]
  co[is.na(co)] <- 0

  degrees2 <- degrees; co2 <- co; coefs1 <- integer(0)
  for(i in 1:length(degrees)) {
    if(degrees2[1]==0) coefs1 <- c(coefs1,0)
    if(degrees2[1]==1) {
      coefs1 <- c(coefs1,co2[1])
      if(length(co2)>1) co2 <- co2[2:length(co2)] }
    if(length(degrees2)>1) degrees2 <- degrees2[2:length(degrees2)]
  }
  co.list <- vector("list",length(independent))
  for(i in 1:length(independent)) co.list[[i]] <- coefs1[((i-1)*degree+1):(i*degree)]

  polys <- function(x,degree,root) {
    if(root==0) y <- x^(1:degree)
    if(root==1) y <- x^(1/(1:degree))
    return(y)
  }
  #datapoints <- 1000
  #xval <- matrix(rep(seq(mindata<-min(variable),maxdata<-max(variable),(maxdata-mindata)/datapoints),length(independent)),nrow=datapoints)
  xval <- matrix(NA,nrow=datapoints, ncol=length(independent)); colnames(xval) <- independent
  for(j in 1:length(independent)) {variable <- data[,independent[j],drop=FALSE]; maxdata <- max(variable); mindata <- min(variable); intervall <- (maxdata-mindata)/datapoints; i <- 0; while (i*intervall <= (maxdata-mindata)) {xval[i,j] <- mindata + intervall*i; i <- i + 1}}

  yval <- matrix(NA,nrow=nrow(xval), ncol=ncol(xval)); colnames(yval) <- colnames(xval)
  for(i in 1:ncol(xval)){
    coefs <- unname(co.list[[i]])
    yval[,i] <-
      mapply(function(x,degree,coefs,root)
        interc + sum( mapply(prod,  polys(x,degree=degree,root=roots[i]), coefs) ), # root=roots.short[i] geloescht
        xval[,i],
        MoreArgs=list(degree,coefs,root))
  }
  if(!is.null(na.replace)) yval[is.na(yval)] <- na.replace

  if(!is.list(indeplimit))     indeplimit <- list(indeplimit)
  if(!is.list(indeplimit))     deplimit <- list(deplimit)
  if(is.null(deplab))  name.dependent <- dependent else name.dependent <- deplab
  if(is.null(indeplab))  name.independent <- independent else name.independent <- indeplab
  if(!is.null(main)) if(ncol(xval)!=length(main)) main <- rep(main[1],ncol(xval))
  if(!is.null(sub))  if(ncol(xval)!=length(sub))  sub <- rep(sub[1],ncol(xval))

  if(length(independent)>1) par(mfrow=c(plotrows,if(length(independent)%%plotrows==0) length(independent)/plotrows else floor(length(independent)/plotrows)+1 ))

  if(!invert) for(i in 1:ncol(xval))
  {
    plot(xval[,i],yval[,i], type="n",ylab=name.dependent, xlab=name.independent[i],cex.lab=cex.lab,cex.axis=cex.axis,xlim=indeplimit[[i]],ylim=deplimit[[i]],col=linecol, ...)
    title(main=main[i],sub=sub[i],cex.main=cex.main,cex.sub=cex.sub)
    if(scatter) lines(data[,independent[i]],data[,dependent], type="p", pch=20,col=dotcol,cex=dotsize)
    lines( xval[,i], yval[,i], type="l",col=linecol,lwd=linewidth)
    if(grid) grid()
  }
  if(invert)  for(i in 1:ncol(xval))
  {
    plot(yval[,i],xval[,i],  type="n",xlab=name.dependent, ylab=name.independent[i],cex.lab=cex.lab,cex.axis=cex.axis,xlim=deplimit[[i]],ylim=indeplimit[[i]],col=linecol, ...)
    title(main=main[i],sub=sub[i],cex.main=cex.main,cex.sub=cex.sub)
    if(scatter) lines(data[,dependent],data[,independent[i]], type="p", pch=20,col=dotcol,cex=dotsize)
    lines( yval[,i],xval[,i], type="l",col=linecol,lwd=linewidth)
    if(grid) grid()
  }

  co[co[]==0] <- NA
  if(intercept)  co <- c(interc,co) else co <- c(NA,co)

  p.total <- extract.regression.p(fit)
  r.squared <- summary(fit)$r.squared
  r.adj <- summary(fit)$adj.r.squared
  significance <- c(p.overall=p.total, r.squared=r.squared, r.adjusted=r.adj)
  #if(intercept) select.pvalue <- c(TRUE,!is.na(co)) else select.pvalue <- c(FALSE,!is.na(co))
  p.values <- summary(fit)$coefficients[!is.na(co),4]
  if(length(co)>0) usedcoef <- rbind(coef=co[!is.na(co)],p.values=p.values) else usedcoef <- NA

  if(invert) {
    invertcoef <- usedcoef; invertcoef[1,] <-  1/usedcoef[1,]
    result <- list(xval=xval, yval=yval, lm=fit, significance=significance, usedcoef=usedcoef, invertcoef=invertcoef)
  } else  result <- list(xval=xval, yval=yval, lm=fit, significance=significance, usedcoef=usedcoef)
  class(result) <- "regplot"
  return(result)
  #f.robftest {sfsmisc}
}
print.regplot <- function(x,digits=2, ...) {
  x$xval <- NULL
  x$yval <- NULL
  class(x) <- "list"
  print(x,digits=digits, ...)
  invisible(x)
}
rlm.mod <- function(form, data, ...){
  # Calculate weights with rlm and return all things like R-squared, Sinificance etc. from lm()
  #library(MASS)
  #formula <- string.to.formula(colnames(y),colnames(x))
  #data <- xy
  #weights1 <- rlm(formula=formula,data=data, maxit=1000)$w
  #lm(formula=formula, data=data, weights=weights1)
  weights1 <- MASS::rlm(formula=form,data=data)$w
  return(lm(formula=form, data=data, weights=weights1, ...))
}


#polyregressionplot(data=aaa.all8.alle.zugeteilt,dependent="DABS_JAE_FamAK",independent=c("DABS_SAK_neu","DABS_JAE_FamAK"),degree=2, degrees=c(1,2), root=TRUE, roots=c(0,1) ,intercept=TRUE,model=c("ols","robust")[1],sig.level=0.025,na.replace=NULL,invert=FALSE,grid=FALSE,plotrows=2,maxit=1000,psi=c(psi.huber, psi.hampel, psi.bisquare)[1],xlimit=NULL,ylimit=NULL)
####
nnls.mod <- function(y,x,...){
  # This function calculates regression coefficients restricted to values greater than zero without an intercept!
  # This function was created in order to calculate the causality between costs an revenues of the SWISS FADN data.
  A <- as.matrix(x); rm(x)
  b <- as.matrix(y); rm(y)
  #if(!is.null(weights)){
  #  A <- weights * A
  #  b <- weights * b
  #}
  require(nnls)
  result <- nnls(A,b)#,...)
  class(result) <- "nnls.mod"
  names(result)[names(result)=="x"] <- "coefficients"
  names(result$coefficients) <- colnames(A)
  names(result)[names(result)=="fitted"] <- "fitted.values"
  ## Calculate Variance-Covariance Matrix
  n <- nrow(A)
  k <- ncol(A)
  VCV = 1/(n-k) * as.numeric( t(result$residuals) %*% result$residuals) * solve(t(A)%*%A)
  ## Standard errors of the estimated coefficients
  StdErr = sqrt(diag(VCV))
  ## Calculate p-value for a t-test of coefficient significance
  P.Value = 2*pt(abs( result$coefficients / StdErr ), df=n-k,lower.tail= FALSE)
  result$StdErr <- StdErr
  result$p.value <- P.Value
  result$x <- A
  result$y <- b
  #if(!is.null(weights)){
  #  result$A.weighted <- A
  #  result$b.weighted <- b
  #  result$weights <- weights
  #} else {
  #  result$A.weighted <- result$b.weighted <- result$weights <- NULL
  #}
  return(result)
}
print.nnls.mod <- function(object, digits=2, ...){
  class(object) <- "list"
  print.1 <- data.frame("Est"=object$coefficients, StdErr=object$StdErr, P.Value=object$p.value) # "Coeff"=as.factor(colnames(object$A)),
  rownames(print.1) <- colnames(object$x)
  print.1 <- round(print.1,digits)
  print(print.1)
  invisible(print.1)
}
predict.nnls.mod <- function(model, newdata=NULL){
  if(is.null(newdata)) {
    return(model$fitted.values)
  } else {
    coefs <- model$coefficients
    if(is.null(dim(newdata))) newdata <- as.data.frame(matrix(newdata,nrow=1))
    if(length(coefs)!=ncol(newdata)) stop("length(coefs) != ncol(newdata)")
    y <- as.vector(crossprod(coefs,new.data))
    return(y)
  }
}
#nnls.autoweight <- function(A,b) {
#  library(MASS)
#  weights <- rlm(A,b)$w
#  return( nnls.mod(A, b, weights=weights) )
#}
####

#' @title Find collinear with determinant of the covariance matrix.
#' @description The determinant of the covariance matrix ranges from \strong{0 (Perfect Collinearity) to 1 (No Collinearity)}.
#' @references \href{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}
#' @export
#' @author Daniel Hoop
#' @param formula A formula object. Must be given, if \code{xVars} is not specified.
#' @param xVars Character vector containing the names of independent variables to be checked. Must be given, if \code{formula} is not specified.
#' @param data data.frame that contains the variables (i.e. environment in which the created formula should be evaluated).
#' @param giveThresholdInfo Logical value indicating if information on the threshold should be given as a message in the console.
find.collinear.variables.det.cov <- function (formula = NULL, data, xVars = NULL, giveThresholdInfo = TRUE){

  if (!is.null(formula) && !is.null(xVars))
    stop("Either specify `formula` or `xVars`, but not both.")
  if (!is.null(formula)) {
    if (any(grepl("\\|", formula)))
      stop ("Don't specify random effects, e.g. `(1 | id)`, but only specify fixed effects in `formula`.")
    mm <- model.matrix(as.formula(formula), data=data)
  } else if (!is.null(xVars)) {
    mm <- model.matrix(as.formula(paste0("~ ", string.to.formula(y=NULL, x=xVars))), data=data)
  }
  mm <- mm[, !colnames(mm) %in% "(Intercept)", drop = FALSE]

  txt <- "0 = Perfect Collinearity, 1 = No Collinearity"
  if (giveThresholdInfo)
    message(txt, "\n", rep("-",nchar(txt)))
  return(round(det(cov(mm)), 3))
}

#' @title Find collinear using eigenvalues of covariance matrix.
#' @description Using the fact that the determinant of a diagonal matrix is the product of the eigenvalues => \strong{The presence of one or more small eigenvalues indicates collinearity}.
#' @references \href{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}
#' @export
#' @author Daniel Hoop
#' @param formula A formula object. Must be given, if \code{xVars} is not specified.
#' @param xVars Character vector containing the names of independent variables to be checked. Must be given, if \code{formula} is not specified.
#' @param data data.frame that contains the variables (i.e. environment in which the created formula should be evaluated).
#' @param giveThresholdInfo Logical value indicating if information on the threshold should be given as a message in the console.
find.collinear.variables.eigenv.cov <- function (formula = NULL, data, xVars = NULL, giveThresholdInfo = TRUE){

  if (!is.null(formula) && !is.null(xVars))
    stop("Either specify `formula` or `xVars`, but not both.")
  if (!is.null(formula)) {
    if (any(grepl("\\|", formula)))
      stop ("Don't specify random effects, e.g. `(1 | id)`, but only specify fixed effects in `formula`.")
    mm <- model.matrix(as.formula(formula), data=data)
  } else if (!is.null(xVars)) {
    mm <- model.matrix(as.formula(paste0("~ ", string.to.formula(y=NULL, x=xVars))), data=data)
  }
  mm <- mm[, !colnames(mm) %in% "(Intercept)", drop = FALSE]

  txt <- "The presence of one or more small eigenvalues indicates collinearity"
  if (giveThresholdInfo)
    message(txt, "\n", rep("-",nchar(txt)))

  #eigenvalues <- eigen( t(mm) %*% mm )$values; names(eigenvalues)  <- indep;  eigenvalues <- cbind(round( eigenvalues, digits=3)) # -1 da ohne Intercept
  #txt <- "Variant 1: eigen( t(mm) %*% mm )"
  #if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  #print(eigenvalues)

  eigenvalues <- eigen(cov(mm))$values
  names(eigenvalues)  <- colnames(mm)
  eigenvalues <- sort(eigenvalues)
  eigenvalues <- as.matrix(round(eigenvalues, 4))
  #txt <- "\nVariant 2: eigen(cov(mm))"
  #if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  return(eigenvalues)
}

#' @title Find collinear using "Kappa".
#' @description \strong{A Condition Number (CN) > 30 indicates collinearity.}.
#' @references \href{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}, \code{\link[base:kappa]{base::kappa}}
#' @export
#' @author Daniel Hoop
#' @param formula A formula object. Must be given, if \code{xVars} is not specified.
#' @param xVars Character vector containing the names of independent variables to be checked. Must be given, if \code{formula} is not specified.
#' @param data data.frame that contains the variables (i.e. environment in which the created formula should be evaluated).
#' @param giveThresholdInfo Logical value indicating if information on the threshold should be given as a message in the console.
find.collinear.variables.kappa <- function (formula = NULL, data, xVars = NULL, giveThresholdInfo = TRUE){
  if (!is.null(formula) && !is.null(xVars))
    stop("Either specify `formula` or `xVars`, but not both.")
  if (!is.null(formula)) {
    if (any(grepl("\\|", formula)))
      stop ("Don't specify random effects, e.g. `(1 | id)`, but only specify fixed effects in `formula`.")
    mm <- model.matrix(as.formula(formula), data=data)
  } else if (!is.null(xVars)) {
    mm <- model.matrix(as.formula(paste0("~ ", string.to.formula(y=NULL, x=xVars))), data=data)
  }
  txt <- "A Condition Number (CN) > 30 indicates collinearity."
  if (giveThresholdInfo)
    message(txt, "\n", rep("-",nchar(txt)))
  return(kappa(mm))
}

#' @title Find collinear using R squared.
#' @description A regression of x_i on all other predictors gives R^2_i. Repeat for all predictors. \strong{R^2_i close to one indicates a problem - the offending linear combination may be found}.
#' @references \href{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}{http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model}
#' @export
#' @author Daniel Hoop
#' @param formula A formula object. Must be given, if \code{xVars} is not specified.
#' @param xVars Character vector containing the names of independent variables to be checked. Must be given, if \code{formula} is not specified.
#' @param data data.frame that contains the variables (i.e. environment in which the created formula should be evaluated).
#' @param giveThresholdInfo Logical value indicating if information on the threshold should be given as a message in the console.
find.collinear.variables.rsq <- function (formula = NULL, data, xVars = NULL, giveThresholdInfo = TRUE){
  if (!is.null(formula) && !is.null(xVars))
    stop("Either specify `formula` or `xVars`, but not both.")
  if (!is.null(formula)) {
    if (any(grepl("\\|", formula)))
      stop ("Don't specify random effects, e.g. `(1 | id)`, but only specify fixed effects in `formula`.")
    mm <- model.matrix(as.formula(formula), data=data)
  } else if (!is.null(xVars)) {
    mm <- model.matrix(as.formula(paste0("~ ", string.to.formula(y=NULL, x=xVars))), data=data)
  }
  mm <- mm[, !colnames(mm) %in% "(Intercept)", drop = FALSE]
  mm <- as.data.frame(mm)
  rsq <- numeric(ncol(mm))
  names(rsq) <- colnames(mm)
  colnames(mm) <- gsub(":|/|\\*", "_", colnames(mm))
  for(i in 1:ncol(mm)){
    rsq[i] <- summary( lm( string.to.formula(y=colnames(mm)[i], x=colnames(mm)[-i]) , mm ) )$r.squared
  }
  rsq <- sort(rsq, decreasing=TRUE)
  rsq <- as.matrix(round(rsq, 4))
  txt <- "R Squared close to 1 indigates problems - the offending linear combination may be found."
  if (giveThresholdInfo)
    message(txt, "\n", rep("-",nchar(txt)))
  return(rsq)
}

#' @title Find collinear using the variance inflation factor (VIF).
#' @description \strong{VIF > 5 is of concern}. In that case, parameter estimates and probability values are questionable. However, read the details section!
#' @export
#' @author Daniel Hoop
#' @param model A fitted model. If given, then \code{formula} and \code{data} don't have to be specified.
#' @param formula A formula. Necessary, only if \code{model} was not given.
#' @param data The data to fit the linear model specified in \code{formula}. Necessary, only if \code{model} was not given.
#' @param giveThresholdInfo Logical value indicating if information on the threshold should be given as a message in the console.
#' @details
#' \href{https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/}{https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/}\cr
#' VIFs start at 1 and have no upper limit. A value of 1 indicates that there is no correlation between this independent variable and any others. VIFs between 1 and 5 suggest that there is a moderate correlation, but it is not severe enough to warrant corrective measures. VIFs greater than 5 represent critical levels of multicollinearity where the coefficients are poorly estimated, and the p-values are questionable.\cr
#' \href{https://statisticalhorizons.com/multicollinearity}{https://statisticalhorizons.com/multicollinearity}\cr
#' Personally, I tend to get concerned when a VIF is greater than 2.50, which corresponds to an R2 of .60 with the other variables.\cr
#' Regardless of your criterion for what constitutes a high VIF, there are at least three situations in which a high VIF is not a problem and can be safely ignored:\cr
#' 1. The variables with high VIFs are control variables, and the variables of interest do not have high VIFs.\cr
#' 2. The high VIFs are caused by the inclusion of powers or products of other variables.\cr
#' 3. The variables with high VIFs are indicator (dummy) variables that represent a categorical variable with three or more categories.
#' Futher notes...
#' VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity requiring correction.
#' Tolerance is the reciprocal of VIF.
find.collinear.variables.vif <- function (formula = NULL, data = NULL, model = NULL, giveThresholdInfo = TRUE){

  if (!is.null(model) && !is.null(formula))
    stop ("Either specify `model` or `formula`.")
  if (!is.null(formula) && is.null(data))
    stop ("If `formula` is specified, then `data` must be specified as well.")

  installFromCRAN("car")
  if (!is.null(formula)) {
    if (any(grepl("\\|", formula)))
      stop ("Don't specify random effects, e.g. `(1 | id)`, but only specify fixed effects in `formula`. Else, specify directly `model`, instead of specifying `formula`.")
    vif.val <- car::vif(lm(formula, data))
  } else {
    vif.val <- car::vif(model)
  }
  if (is.null(dim(vif.val))) {
    vif.val <- as.matrix(vif.val)
  }
  vif.val <- vif.val[order(vif.val[, 1], decreasing = TRUE), , drop = FALSE]

  txt <- "VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity requiring correction."
  if (giveThresholdInfo)
    message(txt, "\n", rep("-",nchar(txt)))
  return(vif.val)
}


#### FORMULA & STRING HANDLING ####
string.to.formula <- function(y=NULL, x, intercept=TRUE, func=c("x","x^2","log(2)")[1], help.func=c("no","I","s"), return.formula=TRUE){
  # The string can be fed into a lm(formula=) or glm(formula=) function without being converted to a formula
  help.func <- match.arg(help.func)
  if(is.null(y)) return.formula <- FALSE
  if(substr(func,1,1)=="x"){
    if(help.func=="no") { form <- paste(paste(x,substr(func,2,nchar(func)),sep=""),collapse=" + ")
    } else { form <- paste(paste(help.func,"(",x,substr(func,2,nchar(func)),")",sep=""),collapse=" + ")
    }
  } else {
    func.long <- character(0)
    for(i in 1:nchar(func)) func.long[i] <- substr(func,i,i)
    func.1 <- substr(func,1,which(func.long=="x")-1)
    func.2 <- substr(func,which(func.long=="x")+1,length(func.long))
    form <- paste(paste(help.func,"(",func.1,x,func.2,")",sep=""),collapse=" + ")
  }
  if(!intercept) form <- paste(form,"-1")
  if(!is.null(y)) form <- paste(y,"~",form)
  if(return.formula) form <- formula(form, env=globalenv())
  return(form)
}

#' Take a substring but start at the end (reverse substring).
#' @export
#' @author Daniel Hoop
substr.rev <- function(char, start, end, reverse=FALSE) {
  # Reverse substring

  if(any(start>end)) stop("All start must be smaller equal end.")

  nchar_char <- nchar(char)
  res <- substr(char, nchar_char-end+1, nchar_char-start+1)
  if(reverse) { # Hinweis: Diese Version mit sapply(lapply()) ist rund 25% schneller als wenn man es mit apply() loest.
    res <- sapply(lapply(strsplit(res, NULL), function(x)rev(x)), function(x)paste(x, collapse=""))
  }
  return(res)
}
####

if(FALSE){
  find.string("a", c("A", "B", "C", "a"), TRUE)
  pattern <- "a"; x <- c("A", "B", "C", "a"); ignore.case=TRUE
  pattern <- c("5%","25%","50%","75%","95%"); x <- c("100%", "95%", "90%", "85%", "80%", "75%", "70%", "65%", "60%", "55%", "50%", "45%", "40%", "35%", "30%", "25%", "20%", "15%", "10%", "5%", "0%")
  cn(gb)[ find.string("OAF",cn(gb)) ]
  grepl("OAF",cn(gb))
}

find.string <- function(pattern, x, ignore.case=FALSE, ...){
  # This function indicates all places of a character vector that contain a certain string.
  # Note that not the whole vector place must match but only a part!
  if(length(pattern)>0) pattern <- paste0(pattern, collapse="|")
  return( grepl(pattern=pattern, x=x, ignore.case=ignore.case, ...) )
}
####

#' Find a column in a data.frame using a pattern.
#' @export
#' @author Daniel Hoop
find.col <- function(pattern, dat, ignore.case=TRUE, ...){
  # This function is a convenience function to find columns in a data.frame or matrix.
  # Use it like this: find.col("jae", dat1)
  if(!is.null(dim(pattern))) {
    if(!is.null(dim(dat))) stop("pattern (first argument) must be a value or vector, but not a matrix/data.frame. dat (second) argument must be a matrix/data.frame.")
    return( colnames(pattern)[ find.string(pattern=dat, x=colnames(pattern), ignore.case=ignore.case, ...) ] )
  }
  return( colnames(dat)[ find.string(pattern=pattern, x=colnames(dat), ignore.case=ignore.case, ...) ] )
}
find.gb.col <- function(...) find.col(..., dat=gb)
find.spa.col <- function(...) find.col(..., dat=spa)
find.spe.col <- function(...) find.col(..., dat=spe)
find.cost.col <- function(...) find.col(..., dat=cost)

harmonize.agis.colnames <- function(col.names){
  # This function harmonizes the colnames of AGIS data from different years.
  # At the end of the function all colnames are shifted to upper case letters
  # dat = the name of the agis data.frame

  varmat <- matrix(c(
    "ART_ID","art_id"
    ,"METER_X","BUR_EXT_GKODX"
    ,"METER_Y","BUR_EXT_GKODY"
    ,"METER_X","BUR_COORD_X"
    ,"METER_Y","BUR_COORD_Y"
    ,"SAK_TOT","SAKBLW"
    ,"SAK_TOT","sakblw"
    ,"GDENR","GMDEAKT"
    ,"GDENR", "gmde"
    ,"GVE_SCHWE","GVE_SCHE"
    ,"GVE_SCHWE","GVE_SCHW"
    ,"GVE_SCHWE","gveschw"
    ,"GVE_SCHWE","GVE_SCHWE_09"
    ,"GVE_RINDE","GVE_RIND"
    ,"GVE_RINDE","GVE_RINDE_09"
    ,"GVE_RINDE","gverin"
    ,"GVE_TOT","GVE_TOT_09"
    ,"GVE_TOT","gvetot"
    ,"GVE_GEFLU","GVE_GEFLU_09"
    ,"GVE_GEFLU","gvegef"
    ,"GVE_GEFLU","GVE_GELU"
    ,"HANG18_35","hangu35"
    ,"HANGGT35","hangg35"
    ,"HANGREB30_50","hangru50"
    ,"HANGREBGT50","hangrg50"
    ,"BIO","bio"
    ,"LN","ln"
  ),byrow=TRUE,ncol=2)
  new_vars <- varmat[,1];   old_vars <- varmat[,2];

  for(i in 1:length(new_vars)){
    col.names[col.names%in%old_vars[i]] <- new_vars[i]
  }

  col.names <- toupper( col.names )
  return(col.names)
}



#### TESTS OF SIGNIFICANT DIFFERENCES ####

#' Calculate the Wilcoxon rank sum statistics for continuous variables and the chi-squared statistics for binary variables (automatically).
#' @export
#' @author Daniel Hoop
#' @param data A numeric vector or data.frame
#' @param trt The binary treatment vector (or grouping)
#' @param digits The number of digits to round the p.value for the formatted output.
#' @param pValuesOnly Default: TRUE. Logical value indicating if only the p value or also signs like \code{".", "*", "**", "***"} should be returned as result.
#' @param verbose Default: TRUE. Logical value indicating if the detection of binary variables (and the application of the chisq test) should be commented during calculations.
#' @return A numeric vector if \code{pValuesOnly == TRUE}.
#' Otherwise a list containing the p.values "p.value", signs "sign", and both of them put together "formatted".
#' @examples
#' wilcox.or.chisq(
#'   data.frame(nonSignVar = rnorm(1:100), signVar = c(rep(1,50),rep(2,50))),
#'   trt = c(rep(1,50), rep(2,50)))
wilcox.or.chisq <- function(data, trt, digits=3, pValuesOnly=TRUE, verbose=TRUE) {


  if (length(unique(trt))!=2)
    stop("trt must contain exactly two different values.")

  if (!is.data.frame(data)) {
    if (!is.numeric(data))
      stop("y must be a numeric vector or a data.frame.")
    data <- as.data.frame(data,stringsAsFactors=FALSE)
  }

  # Debugging: y <- y[,"I(Verkauf_kg_Ti/Groesse*0.004)"]
  kruskres <- sapply(data,function(y){
    notNA <- !is.na(y)
    if(length(unique(y[notNA]))>1 && length(unique(trt[notNA]))>1 ) stats::kruskal.test(y, g=as.factor(trt))$p.value else NaN
  })

  # Chisquared Test fuer Kategorielle Variablen.
  #y <- as.data.frame(matrix(c(0,1,1,1,2,3),ncol=2)); colnames(y) <- c("a","b")
  chisqVars <- sapply(data, function(x)length(unique(x))==2)
  chisqVars <- names(chisqVars)[chisqVars]
  if(length(chisqVars)>0){
    chisqRes <- sapply(data[,chisqVars,drop=FALSE],function(x){
      notNA <- !is.na(x)
      if(length(unique(x[notNA]))>1 && length(unique(trt[notNA]))>1 ) chisq.test.2groups(y=x, trt=trt)$p.value else NaN
    })
    if(verbose)
      cat( paste0("Chi-Sqared test was performed for the following variable(s): ", paste0(chisqVars,collapse=", "), "\n") )
    kruskres[chisqVars] <- chisqRes[chisqVars]
  }

  # Return only p.values if this was indicated by settings.
  if (pValuesOnly)
    return(kruskres)

  # Ab hier sind Kruskal-Wallis und Chi-Sq Ergebnisse in einem Vektor vereint-
  kruskres_add <- rep("", length(kruskres))
  kruskres_add[which(kruskres<0.05)] <- "* "
  kruskres_add[which(kruskres<0.01)] <- "** "
  kruskres_add[which(kruskres<0.001)] <- "*** "
  nameskruskres <- names(kruskres)
  comb <- paste0(kruskres_add, equal.n.decimals(kruskres,digits=digits))
  comb <- gsub("NaN|NA","",comb)
  names(comb) <- nameskruskres

  return(list("p.value"=kruskres, "sign"=kruskres_add, "formatted"=comb))
}

#' Conducts zaUtils::kruskal.groups agricolae::kruskal for multiple columns in a data.frame.
#' @export
#' @author Daniel Hoop
#' @param data The data.frame containing the data to test.
#' @param grouping A grouping vector.
#' @param group Logical value indicating if a grouping with letters should be conducted (`zaUtils::kruskal.groups`) or not (`agricolae::kruskal`).
#' @inheritParams kruskal.groups
kruskal.multiple <- function(data, grouping, sig.level=0.05, p.adj="holm", group=TRUE, ...){   #, filtered=FALSE
  .install.package("agricolae")
  if (is.null(dim(data)))
    data <- data.frame(var = data)
  result <- vector("list",ncol(data))
  if(!group){
    for (i in 1:ncol(data)) {
      result[[i]] <- agricolae::kruskal(data[,i], grouping, main=colnames(data)[i], p.adj=p.adj, group=group)
    }
  } else {
    result <- list()
    for(i in 1:ncol(data)) {
      result[[i]] <- kruskal.groups(data[,i], grouping, ...)$summary
    }
    names(result) <- colnames(data)
  }
  return(result)
}
# kruskal.multiple(results,clustering.rep[1:2000])

#' Conducts pgirmess::kruskalmc for multiple columns in a data.frame.
#' @export
#' @author Daniel Hoop
#' @param data The data.frame containing the data to test.
#' @param grouping A grouping vector.
#' kruskalmc.multiple(
#'   data.frame(a = 1:10, b = runif(10)),
#'   c(rep(1, 5), rep(2, 5)))
kruskalmc.multiple <- function(data, grouping, sig.level=0.05){
  .install.package("pgirmess")
  if (is.null(dim(data)))
    data <- data.frame(var = data)
  result <- vector("list", ncol(data))
  for (i in 1:ncol(data))
    result[[i]] <- pgirmess::kruskalmc(data[,i], grouping)
  if(!is.null(colnames(data)))
    names(result) <- colnames(data)
  return(result)
}
#test1 <- kruskalmc.all(gruppiert[,1:2], gruppiert[,"grouping"])

#' Conducts stats::kruskal.test for multiple columns in a data.frame.
#' @export
#' @author Daniel Hoop
#' @param data The data.frame containing the data to test.
#' @param grouping A grouping vector.
#' @param extract.p Logical value if only the p value should be extracted from the result of `stats::kruskal.test`.
#' @param digits The number of digits to round the p values.
#' @examples
#' kruskal.test.multiple(
#'   data.frame(a = 1:10, b = runif(10)),
#'   c(rep(1, 5), rep(2, 5)))
kruskal.test.multiple <- function(data, grouping, extract.p=TRUE, digits=3){
  if (is.null(dim(data)))
    data <- data.frame(var = data)
  result <- vector("list", ncol(data))
  for(i in 1:ncol(data))
    result[[i]] <- stats::kruskal.test(data[,i], grouping)
  if(!is.null(colnames(data)))
    names(result) <- colnames(data)
  if(extract.p) {
    p.value <- numeric()
    for(i in 1:length(result)){
      p.value[i] <- result[[i]]$p.value
    }
    p.value <- round(p.value, digits)
    names(p.value) <- names(result)
    result <- p.value
  }
  return(result)
}

#' Assign groups to character labels. Groups that do not have the same character, are significantly different.
#' @export
#' @author Daniel Hoop
#' @param y The variable of interest.
#' @param trt The treatment vector.
#' @param sig.level The significance level which is relevant for the grouping with letters.
#' @param p.adj The probability adjustment method.
#' @param median.mean UNKNOWN
#' @param digits The number of digits in the result.
#' @param ranked UNKNOWN
#' @return A list with different attributes, the most important of which has the name "summary".
#' @examples
#' n <- 1000
#' y <-   c(rnorm(n, 0, 0.1), rnorm(n, 1, 0.1), rnorm(n, 0,  0.15))
#' trt <- c(rep(1, n),        rep(2, n),        rep(3, n))
#' kruskal.groups(y = y, trt = trt)
kruskal.groups <- function(y, trt, sig.level=0.05, p.adj="holm", median.mean=c("mean","median","both"), digits=2, ranked=c("no","character.left","character.right","rankmean","intern")) {
  if(length(unique(trt))>9)
    stop("Due to the output structure of the underlying function agricolae::kruskal this function only works up to 9 groups.")
  if(any(is.na(trt)))
    stop("NA values in `trt` are now allowed")

  if(is.matrix(y)) { y <- as.data.frame(y) #return( apply(y,2,function(x)kruskal.groups(y=x, trt=trt, sig.level=sig.level, p.adj=p.adj, median.mean=median.mean, digits=digits, ranked=ranked, print.result=print.result)) )
  } else if (is.data.frame(y)) return (return( lapply(y,function(x)kruskal.groups(y=x, trt=trt, sig.level=sig.level, p.adj=p.adj, median.mean=median.mean, digits=digits, ranked=ranked)) ))

  char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  ranked <- match.arg(ranked)
  median.mean <- match.arg(median.mean)
  grouping <- trt
  sizes <- table(grouping); sizename <- names(sizes); sizes <- unname(sizes); names(sizes) <- sizename # for removing "grouping" from the name
  grp <- sort(unique(grouping))
  comb <- combn(grp,2);        comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))

  if(median.mean%in%c("mean","both")) {
    mean <- round(tapply(y,grouping,mean,na.rm=TRUE),digits=digits)
    sd <-   round(tapply(y,grouping,sd,na.rm=TRUE),digits=digits)
  }
  if(median.mean%in%c("median","both")) {
    median <- round(tapply(y,grouping,median,na.rm=TRUE),digits=digits)
    quantiles <- round(do.call("rbind",tapply(y,grouping,function(x)quantile(x,c(0.25,0.5,0.75),na.rm=TRUE))),digits=digits); colnames(quantiles) <- c("25%","50%","75%")
  }

  krsk <- .kruskal2.for.kruskal.groups(y,grouping,group=FALSE,p.adj=p.adj)
  diffs <- krsk[[1]][,1] # Extracting the needed information out of the kruskal()-Output
  sig <- krsk[[1]][,2]   # Extracting the needed information out of the kruskal()-Output
  sig.true <- sig < sig.level

  if( kruskal.test(y,trt)$p.value>=sig.level   |   all(!sig.true) ){
    # Wenn keine signifikanten Unterschiede bestehen, nur "a" ausgeben.
    # Conover 1999: If, and only if, the null hypothesis [of the normal Kruskal-Wallis-Test] is rejected, we may use the following procedure to determine which pairs of populations tend to differ. (Null hypothesis of the normal Kruskal-Wallis Test: All of the length(grp) population distribution functions are identical)
    mat.short <- mat <- matrix("a",nrow=length(grp),ncol=1); rownames(mat.short) <- rownames(mat) <- names(mean)
    if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
    ranking.rankmean <- as.numeric(as.character(krsk[[2]][order(-krsk[[2]][,2]),1]))
    ranking.intern <- ranking.character.left <- ranking.character.right <- ranking.rankmean
    rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","rankmean","intern")
    if(ranked!="no") {
      out <- out[rank.mat[,ranked],]
      mat <- mat[rank.mat[,ranked],,drop=FALSE]
      mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
      krsk[[2]] <- krsk[[2]][rank.mat[,ranked],]
    }
    result <- list(summary=out, groups=mat.short, groups.long=mat, kruskal=krsk, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
    class(result) <- "kruskal.groups"
    return(result)
  }

  sig.value <- numeric()  # Converting the significance information (TRUE / FALSE) into (-1 & 1 / 0) for (TRUE greater & TRUE saller / FALSE)
  for(i in 1:length(sig)){
    if(!sig.true[i]) sig.value[i] <- 0 else if(sig.true[i] & diffs[i]<0) sig.value[i] <- -1 else if(sig.true[i] & diffs[i]>0) sig.value[i] <- 1
  }
  # Signifikanzmatrizen erstellen (1/0/-1)
  sig.value.mat <- matrix(NA,nrow=length(grp),ncol=length(grp))
  for(i in 1:length(sig.true))    sig.value.mat[comb[1,i],comb[2,i]] <- sig.value[i]
  for(i in 1:ncol(sig.value.mat)) sig.value.mat[,i] <- -sig.value.mat[i,]
  diag(sig.value.mat) <- 0

  rank.numbers.pos <- apply(sig.value.mat,1,function(x)sum(x[x>0]))
  rank.numbers.neg <- apply(sig.value.mat,1,function(x)sum(x[x<0]))
  ranking <- order(-rank.numbers.pos, rank.numbers.neg, -krsk[[2]][,2])

  # Schauen, wie viel mal man die n?chste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }

  # THIS IS THE HEART OF THE FUNCTION (makes the grouping a, ab, a, ...)
  # *********************************
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede f?r eine Gruppe genau dieselbe sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese ?berspr?ngen, da diese ja schon bei den positiven Signifikanzen ber?cksichtigt wurden. Dann l?uft der Loop leer durch, bis er fertig ist.
    # Zur Kontrolle:
    # test <- sig.value.mat[ranking,]
    # rownames(test) <- ranking;test
    if
    ( i==1 |
      ( !all( (sig.value.mat[ranking[i],] == sig.value.mat[ranking[i-1],])[ sig.value.mat[ranking[i],]>=0 & sig.value.mat[ranking[i-1],]>=0 ] )
        &  !all(  sig.value.mat[ranking[(i-1):nrow(sig.value.mat)],] <= 0 )
      )
    )
    {
      looki <- apply(comb==ranking[i],2,function(x)any(x))
      for(j in 1:length(grp)){
        lookj <- apply(comb==grp[j],2,function(x)any(x))
        # Es wird dasjenige Element ausgew?hlt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gew?hlte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen]
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Gepr?ften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede f?r eine Gruppe genau dieselben sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen. Siehe if() oben.
      count.auslassen <- count.auslassen + 1
    }
    # Zur Kontrolle
    # print(mat)
    # names(sig.value) <- comb.names; print(sig.value)
  }

  mat <- mat[,apply(mat,2,function(x)!all(is.na(x)))] # Remove empty columns from table
  mat.short <- matrix(NA,ncol=1,nrow=nrow(mat))
  rownames(mat.short) <- rownames(mat) <- names(mean)
  for(i in 1:nrow(mat)){
    mat.short[i,] <- paste(mat[i,],sep="",collapse="")
  }
  if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
  ranking.rankmean <- as.numeric(as.character(krsk[[2]][order(-krsk[[2]][,2]),1]))
  ranking.character.left  <- order(apply(mat,1,function(x)min(which(x!=" "))) , apply(mat,1,function(x)max(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -krsk[[2]][,2])
  ranking.character.right <- order(apply(mat,1,function(x)max(which(x!=" "))) , apply(mat,1,function(x)min(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -krsk[[2]][,2])
  ranking.intern <- ranking
  rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","rankmean","intern")
  if(ranked!="no") {
    out <- out[rank.mat[,ranked],]
    mat <- mat[rank.mat[,ranked],,drop=FALSE]
    mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
    krsk[[2]] <- krsk[[2]][rank.mat[,ranked],]
  }
  result <- list(summary=out, groups=mat.short, groups.long=mat, kruskal=krsk, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
  class(result) <- "kruskal.groups"
  return(result)
}
print.kruskal.groups <- function(x,...){
  print(x$summary, ...)
}
####

#y <- rnorm(100,10,5); trt <- round(runif(100,1,10))
#x <- .kruskal2.for.kruskal.groups(y,trt); x
####
.kruskal2.for.kruskal.groups <- function(y, trt,alpha=0.05,p.adj = c("none","holm", "hochberg", "bonferroni", "BH", "BY", "fdr"),group=FALSE,main=NULL) {
  require.package("agricolae")
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  p.adj <- match.arg(p.adj)
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  N<- nrow(junto)
  junto[, 1] <- rank(junto[, 1])
  means <- tapply.stat(junto[,1],junto[,2],stat="sum")  #change
  sds <-   tapply.stat(junto[,1],junto[,2], stat="sd")  #change
  nn <-   tapply.stat(junto[,1],junto[,2],stat="length") #change
  means<-data.frame(means,replication=nn[,2])
  names(means)[1:2]<-c(name.t,name.y)
  # row.names(means)<-means[,1]
  ntr<-nrow(means)
  nk <- choose(ntr, 2)
  DFerror<-N - ntr
  rs<- 0
  U <- 0
  for (i in 1:ntr) {
    rs <- rs + means[i, 2]^2/means[i, 3]
    U <- U + 1/means[i, 3]
  }
  S <- (sum(junto[, 1]^2) - (N * (N + 1)^2)/4)/(N - 1)
  H <- (rs - (N * (N + 1)^2)/4)/S
  #cat("\nStudy:",main)
  #cat("\nKruskal-Wallis test's\nTies or no Ties\n")
  #cat("\nValue:", H)
  #cat("\ndegrees of freedom:", ntr - 1)
  p.chisq <- 1 - pchisq(H, ntr - 1)
  #cat("\nPvalue chisq  :", p.chisq,"\n\n")
  DFerror <- N - ntr
  Tprob <- qt(1 - alpha/2, DFerror)
  MSerror <- S * ((N - 1 - H)/(N - ntr))
  #cat("\nComparison of treatments")
  #...............
  means[,2]<- means[, 2]/means[, 3]
  #cat(paste(name.t,",",sep="")," means of the ranks\n\n")
  #print(data.frame(row.names = means[,1], means[,-1]))
  if (p.adj != "none")
  {
    #cat("\nP value adjustment method:", p.adj)
    a <- 1e-06
    b <- 1
    for (i in 1:100) {
      x <- (b + a)/2
      xr <- rep(x,nk)
      d <- p.adjust(xr, p.adj)[1] - alpha
      ar <- rep(a,nk)
      fa <- p.adjust(ar, p.adj)[1] - alpha
      if (d * fa < 0)
        b <- x
      if (d * fa > 0)
        a <- x
    }
    Tprob <- qt(1 - x/2, DFerror)
  }
  nr <- unique(means[,3])
  if (group) {
    Tprob<-qt(1-alpha/2,DFerror)
    #cat("\nt-Student:", Tprob)
    #cat("\nAlpha    :",alpha)
    if (length(nr) == 1) {
      LSD <- Tprob * sqrt(2 * MSerror/nr)
      #cat("\nLSD      :", LSD,"\n")
    }
    else {
      nr1 <- 1/mean(1/nn[, 2])
      LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
      #cat("\nLSD      :", LSD1,"\n")
      #cat("\nHarmonic Mean of Cell Sizes ", nr1)
    }
    #cat("\nMeans with the same letter are not significantly different\n")
    #cat("\nGroups, Treatments and mean of the ranks\n")
    output <- order.group(means[,1], means[,2], means[,3], MSerror, Tprob,std.err=sqrt(MSerror/ means[,3]))
  }
  if (!group) {
    comb <-combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    LCL<-dif
    UCL<-dif
    pvalue<-dif
    sdtdif <- dif
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      #if (means[i, 2] < means[j, 2]){
      #comb[1, k]<-j
      #comb[2, k]<-i
      #}
      dif[k]<-means[i,2]-means[j,2]
      sdtdif[k]<- sqrt(S*((N-1-H)/(N-ntr))*(1/means[i,3]+1/means[j,3]))
      pvalue[k]<- 2*round(1-pt(abs(dif[k])/sdtdif[k],DFerror),6)
    }
    if (p.adj != "none")pvalue <- round(p.adjust(pvalue, p.adj),6)
    LCL <- dif - Tprob*sdtdif
    UCL <- dif + Tprob*sdtdif
    sig<-rep(" ",nn)
    for (k in 1:nn) {
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else if (pvalue[k] <= 0.01) sig[k]<-"**"
      else if (pvalue[k] <= 0.05) sig[k]<-"*"
      else if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    tr.i <- means[comb[1, ],1]
    tr.j <- means[comb[2, ],1]
    output<-data.frame("Difference" = dif, pvalue=pvalue,sig,LCL,UCL)
    rownames(output)<-paste(tr.i,tr.j,sep=" - ")
    #cat("\nComparison between treatments mean of the ranks\n\n")
    #print(output)

    output2<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
    #output  <-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
  }
  invisible(list(output,output2))
  #     invisible(output)
}

###

#sig.groups( y=c(0,1,0,0,0,1,0,1,1,0,rep(3,10),rep(4,10)), trt=sample(c(0,1,2),30, replace=TRUE), sig=c(0.02,0.04,0.05), sig.level=0.05, digits=2, median.mean=c("mean","median","both")[2], ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE )

sig.groups <- function(y, trt, sig, sig.level=0.05, digits=2, median.mean=c("mean","median","both"), ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE) {
  if(length(y)!=length(trt)) stop("length(y)!=length(trt)")
  ranked <- match.arg(ranked)
  median.mean <- match.arg(median.mean)

  char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  grouping <- trt
  sizes <- table(grouping); sizename <- names(sizes); sizes <- unname(sizes); names(sizes) <- sizename # for removing "grouping" from the name
  grp <- sort(unique(grouping))
  comb <- combn(grp,2);        comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  #mean <- round(tapply(y,grouping,mean,na.rm=TRUE),digits=digits)

  if(median.mean%in%c("mean","both")) {
    mean <- round(tapply(y,grouping,mean,na.rm=TRUE),digits=digits)
    sd <-   round(tapply(y,grouping,sd,na.rm=TRUE),digits=digits)
  }
  if(median.mean%in%c("median","both")) {
    median <- round(tapply(y,grouping,median,na.rm=TRUE),digits=digits)
    quantiles <- round(do.call("rbind",tapply(y,grouping,function(x)quantile(x,c(0.25,0.5,0.75),na.rm=TRUE))),digits=digits); colnames(quantiles) <- c("25%","50%","75%")
  }

  diffs <- rep(NA,ncol(comb)); names(diffs) <- comb.names
  for(i in 1:ncol(comb)) diffs[i] <- mean[comb[1,i]] - mean[comb[2,i]]
  sig.true <- rep(FALSE,length(sig));
  sig.true <- sig < sig.level; names(sig.true) <- comb.names


  if(all(!sig.true)){
    # Wenn keine signifikanten Unterschiede bestehen, nur "a" ausgeben.
    mat.short <- mat <- matrix("a",nrow=length(grp),ncol=1); rownames(mat.short) <- rownames(mat) <- names(mean)
    if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
    ranking.rankmean <- as.numeric(as.character(names(mean)[order(-mean)]))
    ranking.intern <- ranking.character.left <- ranking.character.right <- ranking.rankmean
    rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","mean","intern")
    if(ranked!="no") {
      out <- out[rank.mat[,ranked],]
      mat <- mat[rank.mat[,ranked],,drop=FALSE]
      mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
    }
    result <- list(summary=out, groups=mat.short, groups.long=mat, p.value=sig, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
    class(result) <- "sig.groups"
    if(print.result) print(result)
    return(invisible(result))
  }

  sig.value <- numeric()  # Converting the significance information (TRUE / FALSE) into (-1 & 1 / 0) for (TRUE greater & TRUE saller / FALSE)
  for(i in 1:length(sig)){
    if(!sig.true[i]) sig.value[i] <- 0 else if(sig.true[i] & diffs[i]<0) sig.value[i] <- -1 else if(sig.true[i] & diffs[i]>0) sig.value[i] <- 1
  }
  # Signifikanzmatrizen erstellen (1/0/-1)
  sig.value.mat <- matrix(NA,nrow=length(grp),ncol=length(grp))
  for(i in 1:length(sig.true))    sig.value.mat[comb[1,i],comb[2,i]] <- sig.value[i]
  for(i in 1:ncol(sig.value.mat)) sig.value.mat[,i] <- -sig.value.mat[i,]
  diag(sig.value.mat) <- 0

  rank.numbers.pos <- apply(sig.value.mat,1,function(x)sum(x[x>0]))
  rank.numbers.neg <- apply(sig.value.mat,1,function(x)sum(x[x<0]))
  ranking <- order(-rank.numbers.pos, rank.numbers.neg, -mean)

  # Schauen, wie viel mal man die n?chste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }

  # THIS IS THE HEART OF THE FUNCTION (makes the grouping a, ab, a, ...)
  # *********************************
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede f?r eine Gruppe genau dieselbe sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese ?berspr?ngen, da diese ja schon bei den positiven Signifikanzen ber?cksichtigt wurden. Dann l?uft der Loop leer durch, bis er fertig ist.
    # Zur Kontrolle:
    # test <- sig.value.mat[ranking,]
    # rownames(test) <- ranking;test
    if
    ( i==1 |
      ( !all( (sig.value.mat[ranking[i],] == sig.value.mat[ranking[i-1],])[ sig.value.mat[ranking[i],]>=0 & sig.value.mat[ranking[i-1],]>=0 ] )
        &  !all(  sig.value.mat[ranking[(i-1):nrow(sig.value.mat)],] <= 0 )
      )
    )
    {
      looki <- apply(comb==ranking[i],2,function(x)any(x))
      for(j in 1:length(grp)){
        lookj <- apply(comb==grp[j],2,function(x)any(x))
        # Es wird dasjenige Element ausgew?hlt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gew?hlte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen]
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Gepr?ften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede f?r eine Gruppe genau dieselben sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen. Siehe if() oben.
      count.auslassen <- count.auslassen + 1
    }
    # Zur Kontrolle
    # print(mat)
    # names(sig.value) <- comb.names; print(sig.value)
  }

  mat <- mat[,apply(mat,2,function(x)!all(is.na(x)))] # Remove empty columns from table
  mat.short <- matrix(NA,ncol=1,nrow=nrow(mat))
  rownames(mat.short) <- rownames(mat) <- names(mean)
  for(i in 1:nrow(mat)){
    mat.short[i,] <- paste(mat[i,],sep="",collapse="")
  }
  if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
  ranking.rankmean <- as.numeric(as.character(names(mean)[order(-mean)]))
  ranking.character.left  <- order(apply(mat,1,function(x)min(which(x!=" "))) , apply(mat,1,function(x)max(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -mean)
  ranking.character.right <- order(apply(mat,1,function(x)max(which(x!=" "))) , apply(mat,1,function(x)min(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -mean)
  ranking.intern <- ranking
  rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","mean","intern")
  if(ranked!="no") {
    out <- out[rank.mat[,ranked],]
    mat <- mat[rank.mat[,ranked],,drop=FALSE]
    mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
  }
  result <- list(summary=out, groups=mat.short, groups.long=mat, p.value=sig, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
  class(result) <- "sig.groups"
  if(print.result) print(result)
  invisible(result)
}
print.sig.groups <- function(x,...){
  print(x$summary, ...)
}


####
sig.groups.by.interval <- function(diffs=NULL,sig.true=NULL,comb=NULL,special.input=NULL,input.type=c("normal","malm.ci"),digits=2,ranked=c("no","character.left","character.right","intern"),print.result=TRUE) {
  # For intervals like the malmquist output give the following arguments:
  #malmi <- matrix(c(1,1,1.5,3,2,2,1,4),nrow=2,byrow=TRUE)
  #comb <- combn(1:ncol(malmi),2);  comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  #diffs <- numeric();
  #for(i in 1:ncol(comb)) diffs[i] <- max(malmi[,comb[1,i]]) - min(malmi[,comb[2,i]])
  #names(diffs) <- comb.names
  #sig.true <- diffs<0
  ranked <- match.arg(ranked)
  input.type <- match.arg(input.type)
  if(input.type=="malm.ci"){
    if(is.null(special.input)) stop("Please enter groupwise mean values of confidence intervals (value of dea.malm(...,nrep>1))")
    comb <- combn(1:ncol(special.input),2)
    diffs <- numeric()
    for(i in 1:ncol(comb)) diffs[i] <- max(special.input[,comb[1,i]]) - min(special.input[,comb[2,i]])
    sig.true <- diffs<0
  } else if (any(c( is.null(diffs),is.null(sig.true),is.null(comb) ))) stop("specify arguments diffs, sig.true and comb")

  char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  grp <- unique(sort(comb))
  if( !all(combn(grp,2)==comb) ) stop("Combine the differences with combn(  unique(grouplabels)  ,2)")

  if(all(!sig.true)){
    # Wenn keine signifikanten Unterschiede bestehen, nur "a" ausgeben.
    mat.short <- mat <- matrix("a",nrow=length(grp),ncol=1); rownames(mat.short) <- rownames(mat) <- names(mean)
    rownames(mat) <- rownames(mat.short) <- grp
    ranking.intern <- ranking.character.left <- ranking.character.right <- grp
    rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","intern")
    if(ranked!="no") {
      mat <- mat[rank.mat[,ranked],,drop=FALSE]
      mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
    }
    result <- list(groups=mat.short, groups.long=mat)
    class(result) <- "sig.groups.by.interval"
    if(print.result) print(result)
    return(invisible(result))
  }

  sig.value <- numeric()
  for(i in 1:length(sig.true)){
    if(!sig.true[i]) sig.value[i] <- 0 else if(sig.true[i] & diffs[i]<0) sig.value[i] <- -1 else if(sig.true[i] & diffs[i]>0) sig.value[i] <- 1
  }
  # Signifikanzmatrizen erstellen (1/0/-1)
  sig.value.mat <- matrix(NA,nrow=length(grp),ncol=length(grp))
  for(i in 1:length(sig.true))    sig.value.mat[comb[1,i],comb[2,i]] <- sig.value[i]
  for(i in 1:ncol(sig.value.mat)) sig.value.mat[,i] <- -sig.value.mat[i,]
  diag(sig.value.mat) <- 0

  rank.numbers.pos <- apply(sig.value.mat,1,function(x)sum(x[x>0]))
  rank.numbers.neg <- apply(sig.value.mat,1,function(x)sum(x[x<0]))
  ranking <- order(-rank.numbers.pos, rank.numbers.neg)

  # Schauen, wie viel mal man die n?chste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }

  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede f?r eine Gruppe genau dieselbe sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese ?berspr?ngen, da diese ja schon bei den positiven Signifikanzen ber?cksichtigt wurden. Dann l?uft der Loop leer durch, bis er fertig ist.
    # Zur Kontrolle:
    # test <- sig.value.mat[ranking,]
    # rownames(test) <- ranking;test
    if
    ( i==1 |
      ( !all( (sig.value.mat[ranking[i],] == sig.value.mat[ranking[i-1],])[ sig.value.mat[ranking[i],]>=0 & sig.value.mat[ranking[i-1],]>=0 ] )
        &  !all(  sig.value.mat[ranking[(i-1):nrow(sig.value.mat)],] <= 0 )
      )
    )
    {
      looki <- apply(comb==ranking[i],2,function(x)any(x))
      for(j in 1:length(grp)){
        lookj <- apply(comb==grp[j],2,function(x)any(x))
        # Es wird dasjenige Element ausgew?hlt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gew?hlte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen]
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Gepr?ften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede f?r eine Gruppe genau dieselben sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen. Siehe if() oben.
      count.auslassen <- count.auslassen + 1
    }
    # Zur Kontrolle
    # print(mat)
  }

  mat <- mat[,apply(mat,2,function(x)!all(is.na(x)))]
  mat.short <- matrix(NA,ncol=1,nrow=nrow(mat))
  rownames(mat.short) <- rownames(mat) <- names(mean)
  for(i in 1:nrow(mat)){
    mat.short[i,] <- paste(mat[i,],sep="",collapse="")
  }
  rownames(mat) <- rownames(mat.short) <- grp
  ranking.character.left  <- order(apply(mat,1,function(x)min(which(x!=" "))) , apply(mat,1,function(x)max(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg)
  ranking.character.right <- order(apply(mat,1,function(x)max(which(x!=" "))) , apply(mat,1,function(x)min(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg)
  ranking.intern <- ranking
  rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","intern")
  if(ranked!="no") {
    mat <- mat[rank.mat[,ranked],,drop=FALSE]
    mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
  }
  result <- list(groups=mat.short, groups.long=mat)
  class(result) <- "sig.groups.by.interval"
  if(print.result) print(result)
  invisible(result)
}
print.sig.groups.by.interval <- function(x, ...){
  class(x) <- "list"
  x$groups.long <- NULL
  print(x,quote=FALSE)
}

####
if(FALSE) {
  y <- sample(c(TRUE, FALSE), 100, TRUE)
  y <- sample(c(1, 2), 100, TRUE)
  trt <- c(rep(TRUE, 50), rep(FALSE, 50))
  chisq.test.2groups(y, trt)
}
chisq.test.2groups <- function(y, trt) {
  # This function checks if a categorial varialbes y (0,1 odr TRUE,FALSE) is significantly different for
  # two different groups (treatments) trt
  # The p-value is returned

  if(length(unique(trt))>2) stop("For more than 2 groups choose the function chisq.groups()")
  if(length(unique(y))>2) stop("There are more than 2 values in y (the variable is not binary!). -> length(unique(y)) must be 2!")

  tables <- table(list(trt=trt, y=y))

  result <- list()
  result[["table"]]   <- tables
  result[["proportions"]] <- tables/apply(tables,1,function(x)sum(x))
  result[["p.value"]] <- chisq.test(tables)$p.value

  return(result)
}
#y <- sample(c(0,1),30, replace=TRUE); trt <- sample(c(1,2,3),30, replace=TRUE); p.adj="holm"; perc=TRUE; digits=1; sig.level=0.05; median.mean=c("mean"); ranked=c("no"); print.result=TRUE# k1 <- kruskal.groups(y,trt); print.default(k1); k1
# y=dat[,"Out_soueringofmilk"]; trt=dat[,"reg"]
#a <- chisq.groups(y,trt,p.adj="holm"); a
chisq.groups <- function(y, trt, sig.level=0.05, p.adj="holm", perc=TRUE, digits=1, ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE ) {
  # Do the grouping (a, ab, b, bc,...) for grouped data (1,2,3,4,5,...) with binary categorial variables (0,1). For example a cluster analysis was conducted and now in every cluster there is a certain proportion of observations in the one category and in the other category.
  # i.e. a 2x2 contingency table is checked for significant differences with the chisq.test() function. The data is grouped by the sig.groups() function.
  # y: the data vector, trt (treatment): the grouping vecotr, perc=TRUE: The proportions are given as percentages, digits: digits, ranked: should the output table be sorted according to the proportions..?, print.result: should the result be printed?
  grouping <- trt
  ranked <- match.arg(ranked)

  grp <- sort(unique(grouping))
  comb <- combn(grp,2); comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  comb <- apply(comb,2,function(x)as.character(x))
  mean <- tapply(y,grouping,function(x)mean(x,na.rm=TRUE))
  tables <- table(list(grouping=grouping, proportion=y))

  diffs <- sig <- rep(NA,ncol(comb)); names(diffs) <- names(sig) <- comb.names
  for(i in 1:ncol(comb)) {
    diffs[i] <- mean[comb[1,i]] - mean[comb[2,i]]
    sig[i] <- chisq.test( tables[comb[,i],] )$p.value
  }
  sig <- p.adjust(sig, method=p.adj)
  # Same procedure as in the Kruskal-Wallis-Test: Multiple comparisons between pairs of groups are only done if there is any difference between all of the groups. IF all sig[] are set to 1, then the grouping c(a,a,a,a,...) is produced in the sig.group() function.
  all.nonsig <- chisq.test(tables)$p.value>=sig.level
  # Wenn nur 1 y-Wert gegeben wurde, sind die Werte zwangsl?ufig auch nicht unterschiedlich.
  all.nonsig <- all.nonsig | length(sort(unique(y)))==1

  if(all.nonsig) {
    sig.orig <- sig
    sig[] <- 1
  }

  if(perc==TRUE) digits <- digits+2
  # set.args("y=y, trt=grouping, sig=sig, sig.level=sig.level ,digits=digits, median.mean='mean', ranked=ranked, print.result=FALSE")
  char.grouping <- sig.groups(y=y, trt=grouping, sig=sig, sig.level=sig.level ,digits=digits, median.mean="mean", ranked=ranked, print.result=FALSE)

  if(perc) char.grouping$summary[,c(1,2)] <- char.grouping$summary[,c(1,2)]*100
  char.grouping$summary <- char.grouping$summary[,c(1,3)]
  colnames(char.grouping$summary)[1] <- "prop."
  if(all.nonsig) {
    char.grouping$p.value <- sig.orig
  }
  if(print.result) print(char.grouping)
  invisible(char.grouping)
}

####


#sigdivar(aov.data, aov.factor, minsize=10)
#data(mtcars); data <- mtcars; attach(data); cyl=factor(cyl); aov.factor <- cyl; aov.data <- as.matrix(data[,c(3:11)]); conf.level <- 0.01; a <- 1
sigdivar <- function (aov.data, aov.factor, conf.level=0.05, round=TRUE, digits=3, minsize=NULL) {
  # Explanation: Extract all variables which which reveal significant differences at once out of your (huge) data.frame.
  # The significance is calculated with an ANOVA. Don't forget that you must fullfil the assumptions of normal distribution and homogeneity of variance.
  aov.data <- as.matrix( aov.data )
  nclust <- max(as.numeric(aov.factor))
  aov.factor <- as.factor(as.numeric(as.factor(aov.factor)))

  if(!is.null(minsize)) {         # Cluster Minsizes
    aov.factor <- as.numeric(aov.factor)
    aov.data <- as.matrix(cbind(aov.data, aov.factor))
    colnames(aov.data)[ncol(aov.data)] <- "aov.factor"

    clustersizes <- matrix(nrow=1, ncol=nclust)
    for(i in 1:nclust)
      clustersizes[,i] <- length(which(aov.data[,"aov.factor"]==i))
    if(max(clustersizes)<minsize) stop("No group has enough members to fullfil the minsize restriction")
    clustersizes.column <- matrix(nrow=nrow(aov.data), ncol=1)
    for (i in 1:nrow(clustersizes.column))
      clustersizes.column[i,] <- clustersizes[,aov.data[i,"aov.factor"]]
    aov.data <- as.matrix(cbind(aov.data, clustersizes.column))
    colnames(aov.data)[ncol(aov.data)] <- "clustersizes.column"
    aov.data <- aov.data[which(aov.data[,"clustersizes.column"]>=minsize),]
    aov.factor <- as.factor(as.numeric(as.factor(aov.data[,"aov.factor"])))

    nclust.diff <- nclust - max(as.numeric(aov.factor))
    nclust <- max(as.numeric(aov.factor))
    aov.data <- as.matrix(aov.data[,1:(ncol(aov.data)-2)])
  }

  clustersizes <- matrix(nrow=1, ncol=nclust)
  colnames(clustersizes) <- paste("Cluster", 1:nclust, sep=" "); rownames(clustersizes) <- "Clustersizes"
  for(i in 1:nclust)
    clustersizes[,i] <- length(which(aov.factor[]==i))

  aov.result <- aov(aov.data ~ aov.factor)
  sigdivar <- matrix(nrow=length(summary(aov.result)), ncol=(2+nclust)); rownames(sigdivar) <- colnames(aov.data); colnames(sigdivar) <- c("F value","Pr(>F)", paste("Cluster", 1:nclust, sep=" "))
  sigmat <- do.call("rbind",summary(aov.result))
  sigdivar[,1] <- sigmat[!is.na(sigmat[,"F value"]),"F value"]
  sigdivar[,2] <- sigmat[!is.na(sigmat[,"Pr(>F)"]),"Pr(>F)"]
  for(i in 1:nclust)
    sigdivar[,i+2] <- colMeans(aov.data[aov.factor==i, , drop=FALSE], na.rm=TRUE)
  if (round) sigdivar <- round(sigdivar, digits=3)
  sigdivar.true <- sigdivar[which(sigdivar[,2]<=conf.level), ,drop=FALSE]
  message <- if(min(clustersizes)<10) {"Warning: there are small groups constisting of less than 10 members. This could distort the analysis. Use the argument minsize=... to kick groups smaller than minsize before doing the ANOVA."
  } else if (!is.null(minsize)) {paste(nclust.diff,"groups were excluded from ANOVA due to small sizes.")
  } else if (is.null(minsize)) {"All groups have more than 10 members. Don't forget to fullfil the assumptions for an ANOVA (normality and equal variances)."}

  sigdivar.output <- list(full=sigdivar, sign=sigdivar.true, sizes=clustersizes, message=message)
  return(sigdivar.output)
}

####
tukeyhsd.multiple <- function(aov.data, aov.factor, conf.level=0.05, round=TRUE, digits=3, minsize=NULL){
  # Explanation: Extract all significant differences in variables, like with sigdivar()-function but in this case
  # post-hoc like which means: differences between the individual treatments are also given.
  # Don't forget to fullfill the assumptions of normal distribution and homogeneity of variances for the ANOVA.

  aov.data <- aov.data; aov.factor <- aov.factor
  aov.data <- as.matrix(aov.data)
  nclust <- max(as.numeric(aov.factor))
  if(!is.factor(aov.factor)) aov.factor <- factor(aov.factor)
  result <- vector("list", ncol(aov.data))

  if(!is.null(minsize)) {         # Cluster Minsizes
    aov.factor <- as.numeric(aov.factor)
    aov.data <- as.matrix(cbind(aov.data, aov.factor))
    colnames(aov.data)[ncol(aov.data)] <- "aov.factor"

    clustersizes <- matrix(nrow=1, ncol=nclust)
    for(i in 1:nclust)
      clustersizes[,i] <- length(which(aov.data[,"aov.factor"]==i))
    if(max(clustersizes)<minsize) stop("No group has enough members to fullfil the minsize restriction")
    clustersizes.column <- matrix(nrow=nrow(aov.data), ncol=1)
    for (i in 1:nrow(clustersizes.column))
      clustersizes.column[i,] <- clustersizes[,aov.data[i,"aov.factor"]]
    aov.data <- as.matrix(cbind(aov.data, clustersizes.column))
    colnames(aov.data)[ncol(aov.data)] <- "clustersizes.column"
    aov.data <- aov.data[which(aov.data[,"clustersizes.column"]>=minsize),]
    aov.factor <- as.factor(as.numeric(as.factor(aov.data[,"aov.factor"])))

    nclust.diff <- nclust - max(as.numeric(aov.factor))
    nclust <- max(as.numeric(aov.factor))
    aov.data <- as.matrix(aov.data[,1:(ncol(aov.data)-2)])
  }

  clustersizes <- matrix(nrow=1, ncol=nclust)
  colnames(clustersizes) <- paste("Cluster", 1:nclust, sep=" "); rownames(clustersizes) <- "Clustersizes"
  for(i in 1:nclust)
    clustersizes[,i] <- length(which(aov.factor[]==i))
  message <- if(min(clustersizes)<10) {"Warning: there are small groups constisting of less than 10 members. This could distort the analysis. Use the argument minsize=... to kick groups smaller than minsize before doing the ANOVA."
  } else if (!is.null(minsize)) {paste(nclust.diff,"groups were excluded from ANOVA due to small sizes.")
  } else if (is.null(minsize)) {"All groups have more than 10 members. Don't forget to fullfil all assumptions for the ANOVA (e.g.) equal variances."}


  for (a in 1:ncol(aov.data)) {
    aov.r <- aov(aov.data[,a] ~ aov.factor)
    tuk <- TukeyHSD(aov.r)$aov.factor  # , conf.level=conf.level
    result[[a]] <- tuk[which(tuk[,"p adj"]<=conf.level), ,drop=FALSE]

    if(nrow(result[[a]])==0) result[[a]] <- NA
    names(result)[[a]] <- paste(colnames(aov.data)[a],sep="")
  }
  result <- result[which(!is.na(result))]
  result <- list(result=result, sizes=clustersizes, message=message)
  return(result)
}
#data(mtcars); data <- mtcars; attach(data); cyl=factor(cyl); aov.factor <- cyl; aov.data <- as.matrix(data[,c(3:11)]); conf.level <- 0.01; a <- 1
#tukeyhsd.multiple(aov.data, aov.factor, conf.level=1, minsize=10)

# Sortieren der Ergebnisse nach Gruppennummer
#if (all(nchar(rownames(result.a))==3)) {result.a <- as.matrix(cbind(result.a, as.numeric(substr(rownames(result.a),1,1)), as.numeric(substr(rownames(result.a),3,3)))); doublesort <- TRUE
#} else {result.a <- as.matrix(cbind(result.a, as.numeric(substr(rownames(result.a),1,1)))); doublesort<-FALSE}
#if(doublesort) {colnames(result.a)[ncol(result.a)-1] <- "colname1"; colnames(result.a)[ncol(result.a)] <- "colname2"
#} else {colnames(result.a)[ncol(result.a)] <- "colname1"}
#if(doublesort) { if(nrow(result.a)>1) result.a <- result.a[order(result.a[,"colname1"],result.a[,"colname2"]),]
#                 result.a <- result.a[,1:(ncol(result.a)-2),drop=FALSE]
#} else { if(nrow(result.a)>1) result.a <- result.a[order(result.a[,"colname1"]),]
#         result.a <- result.a[,1:(ncol(result.a)-1), drop=FALSE] }
#result[[a]] <- result.a

# Altvernative without adding columns to the dataframe. But doesn't work!
#if(!is.null(minsize)) {         # Cluster Minsizes
#  aov.factor.m <- as.numeric(aov.factor)
#  clustersizes <- rep(0,nclust)
#  for(i in 1:nclust)
#    clustersizes[i] <- length(which(aov.factor.m[]==i))
#  clustersizes.column <- rep(0,nrow(aov.data))
#  for (i in 1:length(clustersizes.column))
#    clustersizes.column[i] <- clustersizes[aov.factor.m[i]]
#  aov.data <- aov.data[which(clustersizes.column[]>=minsize),]
#  aov.factor.m <- as.factor(as.numeric(aov.factor.m))
#
#  nclust.diff <- nclust - max(as.numeric(aov.factor.m))
#  nclust <- max(as.numeric(aov.factor.m))
#  aov.data <- as.matrix(aov.data)
#}

####
sigtest.1vsall <- function(x,trt,method=c("kruskal","wilcoxon","t.test")) {
  if(!is.vector(x)) stop(paste("x must be a vector. Your argument is of class",class(x),sep=""))
  method <- match.arg(method)
  uniquetrt <- sort(unique(trt))
  trt.new <- list()
  for(i in 1:length(uniquetrt)) trt.new[[i]] <- as.numeric( trt==uniquetrt[i] )

  sig <- rep(0,length(uniquetrt))
  sig.sign <- rep("",length(uniquetrt))

  if(method=="kruskal") {
    for(i in 1:length(uniquetrt)) sig[i] <- kruskal.test(x,trt.new[[i]])$p.value
  } else if (method=="wilcoxon") {
    for(i in 1:length(uniquetrt)) sig[i] <- wilcox.test(x,trt.new[[i]])$p.value
  } else if (method=="t.test") {
    for(i in 1:length(uniquetrt)) sig[i] <- t.test(x,trt.new[[i]])$p.value
  }
}
####

# normalize.qqplot <- function(data,replace.Inf=TRUE,window=FALSE,mar=c(2.1,2.1,2.1,2.1),...){
#   data <- as.data.frame(data)
#
#   data.new <- data
#   colnames(data.new) <- paste0(colnames(data.new)," original")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="orig",...)
#
#   data.new <- data^(-2)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0(colnames(data.new),"^(-2)")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-2)",mar=mar,...)
#
#   data.new <- data^(-1)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0(colnames(data.new),"^(-1)")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-1)",mar=mar,...)
#
#   data.new <- data^(-0.5)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0(colnames(data.new),"^(-0.5)")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-0.5)",mar=mar,...)
#
#   data.new <- data^(0.5)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0(colnames(data.new),"^0.5")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(0.5)",mar=mar,...)
#
#   data.new <- data^(2)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0(colnames(data.new),"^2")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(2)",mar=mar,...)
#
#   data.new <- log(data)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0("log(",colnames(data.new),")")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="log()",mar=mar,...)
#
#   data.new <- exp(data)
#   if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
#   colnames(data.new) <- paste0("exp(",colnames(data.new),")")
#   qqplot.multiple(data.new,colnames(data.new),window=window,mt="exp()",mar=mar,...)
# }

## insert ad.test() to test for normality (and rank() as a additional kind of transformation?)
## insert also: Erst positiv machen, dann transformationen durchfuehren.GEMACHT!
if(FALSE){
  m <- 10; sd = 2
  dat <- matrix(NA,nrow=1000, ncol=9)
  dat[,1] <- rnorm(1000,m,sd)
  dat[,2] <- rnorm(1000,m,sd)^(-0.5);     (m^(-0.5))^-2
  dat[,3] <- rnorm(1000,m,sd)^(-1);       (m^(-1))^-1
  dat[,4] <- rnorm(1000,m,sd)^(-2);       (m^(-2))^-(0.5)
  dat[,5] <- rnorm(1000,m,sd)^2;          (m^(2))^(0.5)
  dat[,6] <- rnorm(1000,m,sd)^(0.5);      (m^(0.5))^(2)
  dat[,7] <- exp(rnorm(1000,m,sd));       exp(log(m))
  dat[,8] <- log(rnorm(1000,m,sd));       log(exp(m))
  dat[,9] <- 1:1000;
  colnames(dat) <- c("x",
                     "x^(-0.5)",
                     "x^(-1)",
                     "x^(-2)",
                     "x^2",
                     "x^(0.5)",
                     "exp(x)",
                     "log(x)",
                     "never")
  normalize(dat)
  normalize.qqplot(dat,window=FALSE)
}

#' Tries to normalize each columng of a given data.frame by applying different functions.
#' @export
#' @author Daniel Hoop
#' @param data \code{data.frame} or \code{matrix} with data to be normalized.
#' @param qq Logical value indicating if qq plot should be plotted.
#' @param window Logical value indicating if a new window should be opened for the qq plot.
#' @param funcs The functions that will be tried in order to make the data "more" normally distributed than it is.
#' @return A list containing
#' \item{data}{The new data that has potentially been normalized.}
#' \item{pValsPerCol}{The p values that were calculated by the lillifors test -> to check normal distribution. Values \emph{above} 0.05 indicate normal distribution.}
#' \item{useFuncsPerCol}{The functions that were applied on all columns of data to come closer to normal distribution.}
#' @details Functions might be applied to different columns even though the lilliefors test did not yield a p.value > 0.05.
#' This will be the case if the p.value gets larger compared to the original distribution.
#'
normalize <- function(data, qq=TRUE, window=FALSE,
                      funcs = list("x^(-2)" = function(x) x^(-2),
                                   "x^(-1)" = function(x) x^(-1),
                                   "x^(-0.5)" = function(x) x^(-0.5),
                                   "log(x)" = function(x) log(x),
                                   "x^(0.5)" = function(x) x^(0.5),
                                   "x" = function(x) x,
                                   "x^(2)" = function(x) x^(2),
                                   "exp(x)" = function(x) exp(x)),
                      ...) {

  # Check if all functions are valid and can be executed
  if (!is.list(funcs) || is.null(names(funcs)))
    stop ("`funcs` must be a named list.")

  mapply(funcDef = funcs,
         funcName = names(funcs),
         FUN = function (funcDef, funcName) {
           hasWorked <- tryCatch({
             input <- 1:2
             result <- funcDef(input)
             if (length(result) != length(input))
               stop ("length(result) != length(input)")
             TRUE # return
           }, error = function (e) {
             FALSE # return
           })
           if (!hasWorked)
             stop ("The function named '", funcName, "' yields an error.",
                   " It must be a function that takes 1 single argument (usually a numeric vector).",
                   " It must return a vector of equal length as the input vector.")
         })

  lillie.test <- function (x) {        # lillie.test{nortest} modified. Now it also works with data containing always the same number.
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (n > 4) {                                                   # change from original here

      p <- pnorm((x - mean(x))/sd(x))
      Dplus <- max(seq(1:n)/n - p)
      Dminus <- max(p - (seq(1:n) - 1)/n)
      K <- max(Dplus, Dminus)
      if (n <= 100) {
        Kd <- K
        nd <- n
      }  else {
        Kd <- K * ((n/100)^0.49)
        nd <- 100
      }
      pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
                      Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
                      1.67997/nd)
      if (!is.na(pvalue)) {                                        # change from original here
        if (pvalue > 0.1) {
          KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
          if (KK <= 0.302) {
            pvalue <- 1
          }    else if (KK <= 0.5) {
            pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
              KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
          }    else if (KK <= 0.9) {
            pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
              KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
          }    else if (KK <= 1.31) {
            pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
              KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
          }    else {
            pvalue <- 0
          }
        }
      } else { pvalue <- 0 }                                  # change from original here
    } else {
      pvalue <- 0                                             # change from original here
      K <- 0                                                  # change from original here
      D <- 0                                                  # change from original here
    }
    RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }

  # # Experimenatl part. Does not work as intended...
  # # Function to calculate the qqline, and giving back the result instead of plotting it.
  # qqlineData <- function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7, ...) {
  #   stopifnot(length(probs) == 2, is.function(distribution))
  #   y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
  #   x <- distribution(probs)
  #   if (datax) {
  #     slope <- diff(x)/diff(y)
  #     int <- x[1L] - slope * y[1L]
  #   }
  #   else {
  #     slope <- diff(y)/diff(x)
  #     int <- y[1L] - slope * x[1L]
  #   }
  #   return (list(int = int, slope = slope))
  # }
  #
  # # Function to trim a distribution and discard the extreme values
  # trim <- function (x, probs = c(0.025, 0.975)) {
  #   if (length(probs) != 2)
  #     stop("probs must be a vector of length 2")
  #   q1 <- quantile(x, sort(probs), na.rm = TRUE)
  #   return(x[which(x >= q1[1] & x <= q1[2])])
  # }
  # # Calculate p values per column...
  # # ... AND Calculate the absolute differences between qqline and the actual curve of the qqplot - only along the Y axis!
  # # Take the sum of all differences.
  # # See the drawing below that illustrate differences along y axis for 4 points.
  # # |       o
  # # |     o |
  # # |   o | |
  # # | o | | |
  # # |_o_o_o_o___
  # #
  # metrics <- lapply(funcs, function (func){
  #   dataTrans <- apply(data, 2, function (x) func(x))
  #   pVals <- apply(dataTrans, 2, function (x) {
  #     lillie.test(x)$p.value
  #   })
  #   diffs <- apply(dataTrans, 2, function (x) {
  #     qqN <- qqnorm(x, plot.it = FALSE)
  #     x <- qqN[["x"]]
  #     y <- qqN[["y"]]
  #     qqL <- qqlineData(y)
  #     yLine <- qqL[["int"]] + qqL[["slope"]] * x
  #     # Of the differences, kick the 1 % that are the most extreme, then take the sum.
  #     diffFromQqLine <- sum(trim(abs(y / yLine - 1), probs = c(0, 0.99)))
  #     diffFromQqLine
  #     # Divide by length to make comparable between figures.
  #     # diffFromQqLine / length(x)
  #   })
  #   return (list(pVals = pVals,
  #                diffs = diffs))
  # })
  # pValsPerCol <- do.call("cbind", lapply(metrics, function (x) x[["pVals"]]))
  # diffsPerCol <- do.call("cbind", lapply(metrics, function (x) x[["diffs"]]))

  if (is.null(dim(data))) {
    data <- matrix(data)
  }
  if (is.null(colnames(data))) {
    colnames(data) <- paste0("V", 1:ncol(data))
  }
  # Calculate p values per column
  pValsPerCol <- lapply(funcs, function(func){
    apply( apply(data,2,function(x)func(x)) ,2,function(x)lillie.test(x)$p.value)
  })
  pValsPerCol <- do.call("cbind", pValsPerCol)

  useFuncsPerCol <- apply(pValsPerCol, 1, function (x) names(funcs)[which.max(x)] )
  #useFuncsPerCol <- apply(diffsPerCol, 1, function (x) names(funcs)[which.min(x)] )

  data.new <- data
  for(i in sort(unique(useFuncsPerCol))){ # i <- sort(unique(useFuncsPerCol))[1]
    transformCol <- names(useFuncsPerCol)[useFuncsPerCol==i]
    data.new[,transformCol] <- funcs[[i]]( data.new[,transformCol] )
  }

  if(qq) qqplot.multiple(data.new, colnames(data.new), window=window, mt=useFuncsPerCol)
  return(list(data = data.new,
              pValsPerCol=pValsPerCol,
              #diffsPerCol = diffsPerCol,
              useFuncsPerCol = useFuncsPerCol
  ) )
}

####
qqplot.multiple <- function(data, variables=NULL, plotrows=5, mar=c(2.1,2.1,2.1,2.1), window=FALSE, mt=NULL,...){
  if (is.null(variables)) {
    if (is.null(colnames(data)))
      stop ("colnames of data must not be NULL.")
    variables <- colnames(data)
  }
  if (length(variables) < plotrows) {
    plotrows <- length(variables)
  }

  par.orig <- par()$mar
  mfrow.orig <- par()$mfrow
  on.exit(par(mar=par.orig, mfrow=mfrow.orig))

  if (length(mt) == 1)
    mt <- rep(mt,ncol(data))
  if (window)
    windows()
  nvariables <- length(variables)
  mfrow <- c(plotrows, if(nvariables%%plotrows == 0) nvariables/plotrows else floor(nvariables/plotrows) + 1)
  par(mar=mar, mfrow=mfrow)
  for(i in 1:nvariables) {
    dat <- as.numeric( data[,variables[i]] )
    qqnorm(dat, pch=20, main=paste0(variables[i], "  ->  use:", mt[i]))
    qqline(dat)
  }
}



#### DELETE FUNCTIONS ####


#### Anzeige, dass Funktionen geladen wurden ####
#message("**********************************************************************\nFunctions loaded\n**********************************************************************")
