#' Lookup table of section and question names
#'
#' A data frame containing all of the section names and questions from the
#' STOP-AD submission form. "step" and "label" are the user-friendly, human
#' readable versions. "section" and "variable" are the original names that are
#' in the JSON data.
#'
#' @export
#'
#' @format A data frame with 148 rows and 4 columns:
#' \describe{
#'   \item{step}{Name of the section of the form (user-friendly), e.g. "Naming"}
#'   \item{label}{Field within the form (user-friendly), e.g. "Is this compound available for off label use?"}
#'   \item{section}{Name of the section as it appears in the raw data, e.g. "naming"}
#'   \item{variable}{Name of the field as it appears in the raw data, e.g. "is_off_label"}
#' }
"lookup_table"


#' Partial beta weights of certain variables
#'
#' The partial beta weight is one of the multipliers used to calculate the score
#' for a section.
#' 
#' @export
#'
#' @format A data frame with 23 rows and 4 columns:
#' \describe{
#'   \item{section}{Name of the section as it appears in the raw data, e.g. "naming"}
#'   \item{variable}{Name of the field as it appears in the raw data, e.g. "is_off_label"}
#'   \item{partial_beta}{Partial beta weight}
#'   \item{notes}{Notes about the calculations for that variable}
#' }
"partial_betas"
