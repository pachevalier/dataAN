

#' Transform answers to a question into a tibble
#'
#' @param list
#'
#' @return a tibble
#' @export
#'
#' @examples
#' make_table_egalite_reponses(extract2(extract2(json_egalite, "questions"), 1))

make_table_egalite_reponses <- function(list) {
  magrittr::extract2(
    magrittr::extract2(
      magrittr::extract2(list, "Reponse"),
      1),
    1) %>%
    dplyr::rename(
      id_critere = id,
      texte_critere = texte)
  }



#' Create a table with all answers to a question
#'
#' @param list
#'
#' @return a tibble
#' @export
#'
#' @examples
#' make_table_egalite_question(extract2(extract2(json_egalite, "questions"), 1))  %>% glimpse()
#' table_questions_reponses <- map_df(.x = extract2(json_egalite, "questions"), make_table_egalite_question, .id = "id_enquete")

make_table_egalite_question <- function(list) {
  make_table_egalite_reponses(list = list) %>%
    dplyr::mutate(
      question_texte = magrittr::extract2(
        magrittr::extract2(
          magrittr::extract2(
            json_egalite, "questions"),
          1),
        "texte"
        )
    )
}

