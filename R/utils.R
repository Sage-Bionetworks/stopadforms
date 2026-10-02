#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

get_display_name <- function(syn, id) {
  purrr::map_chr(
    id,
    function(x) {
      display_name <- tryCatch(
        {
          syn$getUserProfile(x)$displayName
        },
        error = function(err) {}
      )
      user_name <- tryCatch(
        {
          syn$getUserProfile(x)$userName
        },
        error = function(err) {}
      )
      display_name %||% user_name
    }
  )
}

#' @title Attempt to log into Synapse
#'
#' @description Attempt to log into Synapse. Will first try using authentication
#' credentials written to a .synapseConfig file. If that fails, will try using
#' any credentials passed to the function. Will return `NULL` if not all
#' attempts failed.
#'
#' @noRd
#' @param syn Synapse client object
#' @param ... Synapse credentials, such as `authToken` or `email` with a
#' `password` or `apiKey`.
attempt_login <- function(syn, ...) {
  is_logged_in <- FALSE
  
  ## Try logging in with .synapseConfig
  try(
    {
      syn$login()
      is_logged_in <- TRUE
    },
    silent = TRUE
  )
  ## If failed to login, try using credentials provided
  if (!is_logged_in) {
    
    
    tryCatch(
      {
        print(is_logged_in)
        syn$login(...)
      },
      error = function(e) {
        stop("There was a problem logging in.")
      }
    )
  }
}

#' @title Check if logged in as user
#'
#' @description Check if logged into Synapse as a non-anonymous user.
#'
#' @noRd
#' @param syn Synapse client object.
#' @return FALSE if not logged in at all or if logged in anonymously, else TRUE.
logged_in <- function(syn) {
  stopifnot(inherits(syn, "synapseclient.client.Synapse"))
  if (is.null(syn) || is.null(syn$username) || (syn$username == "anonymous")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# https://ryouready.wordpress.com/2008/12/18/generate-random-string-name/
###############################################################
#
# MHmakeRandomString(n, length)
# function generates a random string random string of the
# length (length), made up of numbers, small and capital letters

MHmakeRandomString <- function(n=1, length=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

#  > MHmakeRandomString()
#  [1] "XM2xjggXX19r"

###############################################################
#' @title Notify users of submissions that could not be loaded
#'
#' @description Show a persistent warning notification listing the form data
#' IDs, and names where known, of any submissions that were skipped by
#' [process_submissions()]. Does nothing if no submissions failed.
#'
#' @noRd
#' @param failed_ids Character vector of form data IDs that failed to load
#' @param failed_names Character vector of the matching submission names, `NA`
#'   where not known; or `NULL` if no names are known
#' @param session Shiny session to show the notification in
show_failed_submissions_notice <- function(failed_ids, failed_names = NULL,
                                           session = getDefaultReactiveDomain()) {
  if (length(failed_ids) == 0) {
    return(invisible(NULL))
  }
  n <- length(failed_ids)
  shiny::showNotification(
    shiny::tagList(
      shiny::strong("Some submissions could not be loaded"),
      shiny::p(
        sprintf(
          "%d %s hidden because %s could not be read:",
          n,
          ifelse(n == 1, "submission is", "submissions are"),
          ifelse(n == 1, "it", "they")
        )
      ),
      shiny::tags$ul(
        lapply(format_failed_submissions(failed_ids, failed_names), shiny::tags$li)
      ),
      shiny::p(
        "The other submissions are unaffected. Please notify an administrator."
      )
    ),
    type = "warning",
    duration = NULL,
    closeButton = TRUE,
    session = session
  )
}

#' @title Label skipped submissions for display
#'
#' @noRd
#' @inheritParams show_failed_submissions_notice
#' @return A character vector of labels such as `"Title: CNS4, ID: 774"`
#'   (names are shown without their ".json" suffix), or just `"ID: 774"`
#'   where the name is not known.
format_failed_submissions <- function(failed_ids, failed_names = NULL) {
  if (!is.null(failed_names) && length(failed_names) != length(failed_ids)) {
    ## Never pair an ID with the wrong name; still show the IDs rather than
    ## failing while the notice is shown
    warning(
      "Ignoring submission names: got ", length(failed_names), " names for ",
      length(failed_ids), " form data IDs",
      call. = FALSE
    )
    failed_names <- NULL
  }
  if (is.null(failed_names)) {
    failed_names <- rep(NA_character_, length(failed_ids))
  }
  ## Submission names are file names, e.g. "CNS4.json"; show them without the
  ## ".json" suffix, which means nothing to users
  failed_names <- trimws(sub("\\.json$", "", failed_names, ignore.case = TRUE))
  ifelse(
    is.na(failed_names) | failed_names == "",
    paste0("ID: ", failed_ids),
    paste0("Title: ", failed_names, ", ID: ", failed_ids)
  )
}
