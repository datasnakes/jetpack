# helpers

checkInsecureRepos <- function() {
  repos <- getOption("repos")
  if (is.list(repos)) {
    repos <- unlist(repos, use.names = FALSE)
  }
  insecure_repos <- repos[startsWith(repos, "http://")]
  for (repo in insecure_repos) {
    warn(paste0("Insecure CRAN repo: ", repo))
  }
}


findDir <- function(path) {
  if (file.exists(file.path(path, "DESCRIPTION"))) {
    path
  } else if (dirname(path) == path) {
    NULL
  } else {
    findDir(dirname(path))
  }
}


getName <- function(package) {
  parts <- strsplit(package, "@")[[1]]
  if (length(parts) != 1) {
    package <- parts[1]
  }
  package
}


globalAdd <- function(packages, remotes) {
  globalInstallHelper(packages, remotes)

  for (package in packages) {
    package <- getName(package)
    success(paste0("Installed ", package, " ", packageVersion(package)))
  }
}

globalInstallHelper <- function(packages, remotes = c()) {
  unversioned <- c()
  for (package in packages) {
    parts <- strsplit(package, "@")[[1]]
    if (length(parts) != 1) {
      package <- parts[1]
      version <- parts[2]
      devtools::install_version(package, version = version, reload = FALSE)
    } else {
      unversioned <- c(unversioned, package)
    }
  }

  if (length(unversioned) > 0) {
    # create temporary directory, write description, install deps
    dir <- tempDir()
    desc <- desc::desc("!new")
    for (remote in remotes) {
      desc$add_remotes(remote)
    }
    for (package in unversioned) {
      desc$set_dep(package, "Imports")
    }
    desc$write(file.path(dir, "DESCRIPTION"))

    # TODO don't remove for add command
    for (package in unversioned) {
      if (package %in% rownames(installed.packages())) {
        suppressMessages(remove.packages(package))
      }
    }

    devtools::install_deps(dir, reload = FALSE)
  }
}

globalList <- function() {
  packages <- as.data.frame(installed.packages())
  packages <- packages[order(tolower(packages$Package)), ]
  for (i in 1:nrow(packages)) {
    row <- packages[i, ]
    message(paste0("Using ", row$Package, " ", row$Version))
  }
}

globalRemove <- function(packages) {
  for (package in packages) {
    suppressMessages(remove.packages(package))
  }
  for (package in packages) {
    success(paste0("Removed ", package, "!"))
  }
}

globalUpdate <- function(packages, remotes) {
  versions <- list()
  for (package in packages) {
    package <- getName(package)
    versions[package] <- as.character(packageVersion(package))
  }

  globalInstallHelper(packages, remotes)

  for (package in packages) {
    package <- getName(package)
    currentVersion <- versions[package]
    newVersion <- as.character(packageVersion(package))
    success(paste0("Updated ", package, " to ", newVersion, " (was ", currentVersion, ")"))
  }
}


isWindows <- function() {
  .Platform$OS.type != "unix"
}

oneLine <- function(x) {
  gsub("\n", " ", x)
}


#' @importFrom utils installed.packages remove.packages
pkgRemove <- function(name) {
  if (name %in% rownames(installed.packages())) {
    suppressMessages(remove.packages(name))
  }
}

ensureRepos <- function() {
  repos <- getOption("repos")
  if (repos["CRAN"] == "@CRAN@") {
    repos["CRAN"] <- "https://cloud.r-project.org/"
    options(repos = repos)
  }
}

prepGlobal <- function() {
  ensureRepos()
  checkInsecureRepos()
}



success <- function(msg) {
  cat(crayon::green(paste0(msg, "\n")))
}

tempDir <- function() {
  dir <- file.path(tempdir(), sub("\\.", "", paste0("jetpack", as.numeric(Sys.time()))))
  dir.create(dir)
  dir
}

updateDesc <- function(packages, remotes) {
  desc <- getDesc()

  for (remote in remotes) {
    desc$add_remotes(remote)
  }

  for (package in packages) {
    parts <- strsplit(package, "@")[[1]]
    version <- NULL
    version_str <- "*"
    if (length(parts) != 1) {
      package <- parts[1]
      version <- parts[2]
      version_str <- paste("==", version)
    }

    desc$set_dep(package, "Imports", version = version_str)
  }

  desc
}

#' @importFrom utils packageVersion
version <- function() {
  message(paste0("Jetpack version ", utils::packageVersion("jetpack")))
}

warn <- function(msg) {
  cat(crayon::yellow(paste0(msg, "\n")))
}

windowsPath <- function(path) {
  gsub("/", "\\\\", path)
}


initRprofile <- function() {
  rprofile <- file.exists(".Rprofile")
  if (!rprofile || !any(grepl("jetpack", readLines(".Rprofile")))) {
    str <- "if (requireNamespace(\"jetpack\", quietly=TRUE)) {
  jetpack::load()
} else {
  message(\"Install Jetpack to use a virtual environment for this project\")
}"

    if (rprofile) {
      # space it out
      str <- paste0("\n", str)
    }

    write(str, file = ".Rprofile", append = TRUE)
  }
}

venvDir <- function(dir) {
  # similar logic as Pipenv
  if (isWindows()) {
    venv_dir <- "~/.renvs"
  } else {
    venv_dir <- file.path(Sys.getenv("XDG_DATA_HOME", "~/.local/share"), "renvs")
  }

  # TODO better algorithm, but keep dependency free
  dir_hash <- sum(utf8ToInt(dir))
  venv_name <- paste0(basename(dir), "-", dir_hash)
  file.path(venv_dir, venv_name)
}

#' Add a package
#'
#' @param packages Packages to add
#' @param remotes Remotes to add
#' @export
#' @examples \dontrun{
#'
#' jetpack::add("randomForest")
#'
#' jetpack::add(c("randomForest", "DBI"))
#'
#' jetpack::add("DBI@1.0.0")
#'
#' jetpack::add("plyr", remote="hadley/plyr")
#'
#' jetpack::add("plyr", remote="local::/path/to/plyr")
#' }
add <- function(packages, remotes = c()) {
  sandbox({
    prepCommand()

    desc <- updateDesc(packages, remotes)

    installHelper(desc = desc, show_status = TRUE)

    success("Pack complete!")
  })
}

#' Remove a package
#'
#' @param packages Packages to remove
#' @param remotes Remotes to remove
#' @export
#' @examples \dontrun{
#'
#' jetpack::remove("randomForest")
#'
#' jetpack::remove(c("randomForest", "DBI"))
#'
#' jetpack::remove("plyr", remote="hadley/plyr")
#' }
remove <- function(packages, remotes = c()) {
  sandbox({
    prepCommand()

    desc <- getDesc()

    for (package in packages) {
      if (!desc$has_dep(package)) {
        stop(paste0("Cannot find package '", package, "' in DESCRIPTION file"))
      }

      desc$del_dep(package)
    }

    if (length(remotes) > 0) {
      for (remote in remotes) {
        desc$del_remotes(remote)
      }
    }

    installHelper(desc = desc)

    for (package in packages) {
      success(paste0("Removed ", package, "!"))
    }
  })
}

#' Update a package
#'
#' @param packages Packages to update
#' @param remotes Remotes to update
#' @export
#' @examples \dontrun{
#'
#' jetpack::update("randomForest")
#'
#' jetpack::update(c("randomForest", "DBI"))
#' }
update <- function(packages, remotes = c()) {
  sandbox({
    prepCommand()

    # store starting versions
    status <- getStatus()
    versions <- list()
    for (package in packages) {
      package <- getName(package)
      versions[package] <- pkgVersion(status, package)
    }

    desc <- updateDesc(packages, remotes)

    installHelper(remove = packages, desc = desc)

    # show updated versions
    status <- getStatus()
    for (package in packages) {
      package <- getName(package)
      currentVersion <- versions[package]
      newVersion <- pkgVersion(status, package)
      success(paste0("Updated ", package, " to ", newVersion, " (was ", currentVersion, ")"))
    }
  })
}

#' Check that all dependencies are installed
#'
#' @export
#' @examples \dontrun{
#'
#' jetpack::check()
#' }
check <- function() {
  sandbox({
    prepCommand()

    status <- getStatus()
    missing <- status[is.na(status$library.version), ]
    if (nrow(missing) > 0) {
      message(paste("Missing packages:", paste(missing$package, collapse = ", ")))
      if (!interactive()) {
        warn("Run 'jetpack install' to install them")
      } else {
        warn("Run 'jetpack::install()' to install them")
      }
      invisible(FALSE)
    } else {
      success("All dependencies are satisfied")
      invisible(TRUE)
    }
  })
}

#' Get info for a package
#'
#' @param package Package to get info for
#' @importFrom utils URLencode
#' @export
#' @examples \dontrun{
#'
#' jetpack::info("stringr")
#'
#' jetpack::info("stringr@1.0.0")
#' }
info <- function(package) {
  sandbox({
    parts <- strsplit(package, "@")[[1]]
    version <- NULL
    if (length(parts) != 1) {
      package <- parts[1]
      version <- parts[2]
    }
    url <- paste0("https://crandb.r-pkg.org/", URLencode(package))
    if (!is.null(version)) {
      url <- paste0(url, "/", URLencode(version))
    }
    r <- httr::GET(url)
    error <- httr::http_error(r)
    if (error) {
      stop("Package not found")
    }
    body <- httr::content(r, "parsed")
    message(paste(body$Package, body$Version))
    message(paste("Title:", body$Title))
    message(paste("Date:", body$Date))
    message(paste("Author:", oneLine(body$Author)))
    message(paste("Maintainer:", oneLine(body$Maintainer)))
    message(paste("License:", body$License))
  })
}

#' Search for packages
#'
#' @param query Search query
#' @export
#' @examples \dontrun{
#'
#' jetpack::search("xgboost")
#' }
search <- function(query = NULL) {
  # hack for R CMD check bug in share/R/examples-header.R
  if (is.null(query) && exists("cleanEx")) {
    return(base::search())
  }

  sandbox({
    post_body <- list(
      query = list(
        function_score = list(
          query = list(multi_match = list(query = query, fields = c("Package^10", "_all"), operator = "and")),
          functions = list(list(script_score = list(script = "cran_search_score")))
        )
      ),
      size = 1000
    )
    r <- httr::POST("http://seer.r-pkg.org:9200/_search", body = post_body, encode = "json")
    error <- httr::http_error(r)
    if (error) {
      stop("Network error")
    }
    body <- httr::content(r, "parsed")
    hits <- body$hits$hits
    if (length(hits) > 0) {
      for (i in 1:length(hits)) {
        hit <- hits[i][[1]]
        message(paste0(hit$`_id`, " ", hit$`_source`$Version, ": ", oneLine(hit$`_source`$Title)))
      }
    }
  })
}

#' Install the command line interface
#'
#' @param file The file to create
#' @export
#' @examples \dontrun{
#'
#' jetpack::cli()
#' }
cli <- function(file = NULL) {
  if (isWindows()) {
    if (is.null(file)) {
      file <- "C:/ProgramData/jetpack/bin/jetpack.cmd"
    }
    rscript <- file.path(R.home("bin"), "Rscript.exe")
    dir <- dirname(file)
    if (!file.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    write(paste0("@", rscript, " -e \"library(methods); jetpack::run()\" %* "), file = file)
    message(paste("Wrote", windowsPath(file)))
    message(paste0("Be sure to add '", windowsPath(dir), "' to your PATH"))
  } else {
    if (is.null(file)) {
      user_home <- Sys.getenv('HOME')
      file <- paste0(user_home, "/bin/jetpack")
    }
    write("#!/usr/bin/env Rscript\n\nlibrary(methods)\njetpack::run()", file = file)
    Sys.chmod(file, "755")
    message(paste("Wrote", file))
  }
}

#' Run the command line interface
#'
#' @export
#' @keywords internal
run <- function() {
  sandbox({
    doc <- "Usage:
    jetpack version
    jetpack help
    jetpack global add <package>... [--remote=<remote>]...
    jetpack global remove <package>... [--remote=<remote>]...
    jetpack global update <package>... [--remote=<remote>]...
    jetpack global list"

    opts <- NULL
    tryCatch({
      opts <- docopt::docopt(doc)
    }, error = function(err) {
      msg <- conditionMessage(err)
      if (!grepl("usage:", msg)) {
        warn(msg)
      }
      message(doc)
      quit(status = 1)
    })

    tryCatch({
      if (opts$global) {
        prepGlobal()
        if (opts$add) {
          globalAdd(opts$package, opts$remote)
        } else if (opts$remove) {
          # do nothing with remote
          # keep so it's consistent with remove
          # and easy to reverse global add
          globalRemove(opts$package)
        } else if (opts$update) {
          globalUpdate(opts$package, opts$remote)
        } else {
          globalList()
        }
      } else if (opts$check) {
        if (!check()) {
          quit(status = 1)
        }
      } else if (opts$version) {
        version()
      } else if (opts$help) {
        message(doc)
      } else if (opts$info) {
        info(opts$package)
      } else if (opts$search) {
        search(opts$query)
      } else {
        install(deployment = opts$deployment)
      }
    }, error = function(err) {
      msg <- conditionMessage(err)
      cat(crayon::red(paste0(msg, "\n")))
      quit(status = 1)
    })
  })
}

sandbox <- function(code) {
  libs <- c("jsonlite", "withr", "devtools", "httr", "curl", "git2r", "desc", "docopt")
  if (!interactive()) {
    suppressMessages(packrat::extlib(libs))
    invisible(eval(code))
  } else {
    invisible(packrat::with_extlib(libs, code))
  }
}
