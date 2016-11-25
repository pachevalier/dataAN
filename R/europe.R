

#' Make a table with the question and all answers
#'
#' @param list the name of the list
#' @param i an index
#'
#' @return a tibble
#' @export
#'
#' @examples
#' make_table_question_reponse_europe(i = 1)
make_table_question_reponse_europe <- function(list, i) {
  magrittr::extract2(magrittr::extract2(list, "Réponses"), i) %>%
    dplyr::mutate(
      question = magrittr::extract2(
        magrittr::extract2(list, "Question"),
        i)
      ) %>%
    dplyr::select(question, critere = Critère, n = `Nombre de réponses`)
}

#' Make the table woth the first 5 questions and corresponding answers
#'
#' @return a tibble
#' @export
#'
#' @examples
#' table_europe <- make_table_europe()

make_table_europe <- function() {
  list_europe <- jsonlite::fromJSON("http://data.assemblee-nationale.fr/static/openData/repository/CONSULTATIONS_CITOYENNES/EUROPE/Europe.json")
  plyr::ldply(.data = 1:5, .fun = make_table_question_reponse_europe, list = list_europe)
}

