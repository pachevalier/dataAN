

#' Make a table with the theme and the title of each question
#'
#' @param x a list at the theme level
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(xml2)
#' library(dplyr)
#' library(purrr)
#' list_institutions <- read_xml(x = "raw-data/Institutions.xml") %>% as_list()
#' make_question_table(list_institutions$themes[[1]]) %>% glimpse()
#' map_df(list_institutions$themes, make_question_table) %>% glimpse()

make_question_table <- function(x) {
  tibble::tibble(
    theme = magrittr::extract2(
      magrittr::extract2(x, "titre"), 1
      ),
    question = purrr::map_chr(purrr::map(`[[`(x, "questions") , .f = "texte"), 1)
  )
}

#' Make a table from a response list
#'
#' @param x
#'
#' @return a tibble with two columns : reponse and n
#' @export
#'
#' @examples
#' library(xml2)
#' library(dplyr)
#' library(purrr)
#' list_institutions <- read_xml(x = "raw-data/Institutions.xml") %>% as_list()
#' make_reponse_table(list_institutions$themes[[1]]$questions[[1]]$reponses[[1]])

make_reponse_table <- function(x) {
  tibble::tibble(
    reponse = magrittr::extract2(magrittr::extract2(x, "texte"), 1),
    n = magrittr::extract2(magrittr::extract2(x, "nombre"), 1)
  )
}

#' Make a table with all answers to one questions
#'
#' @param x
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(xml2)
#' library(dplyr)
#' library(purrr)
#' list_institutions <- read_xml(x = "raw-data/Institutions.xml") %>% as_list()
#' make_reponses_table(x = list_institutions$themes[[1]]$questions[[1]])
#' map_df(extract2(list_institutions$themes[[1]], "questions"), make_reponses_table) %>% glimpse()


make_reponses_table <- function(x) {
  purrr::map_df(magrittr::extract2(x, "reponses"), .f = make_reponse_table) %>%
    dplyr::mutate(
      question = magrittr::extract2(magrittr::extract2(x, "texte"), 1)
    ) %>%
    dplyr::select(question, reponse, n)
}


#' Make a table with all answers to all questions in a theme
#'
#' @param x
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(xml2)
#' library(dplyr)
#' library(purrr)
#' list_institutions <- read_xml(x = "raw-data/Institutions.xml") %>% as_list()
#' make_questions_reponses_table(x = list_institutions$themes[[1]]) %>% glimpse()
#' map_df(.x = list_institutions$themes, make_questions_reponses_table) %>% glimpse()

make_questions_reponses_table <- function(x) {
  map_df(extract2(x, "questions"), make_reponses_table) %>%
    dplyr::mutate(theme = magrittr::extract2(magrittr::extract2(x, "titre"), 1)) %>%
    dplyr::select(theme, question, reponse, n)
  }
