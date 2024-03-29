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