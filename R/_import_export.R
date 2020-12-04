#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Pseudo-function to import \strong{dplyr}'s common functions.
#'
#' @importFrom dplyr select rename mutate mutate_at filter filter_at arrange distinct summarise
#'     do group_by group_by_at ungroup rowwise do left_join inner_join everything bind_rows
#'     pull tibble as_tibble rowwise any_vars all_vars vars collect
NULL
