#' Extract names of townns in Skåne from strings
#'
#' @param sjh A character vector
#' @param replacements A character vector of replacement strings
#' @param nas A string to put where none of the replacements fit
#'
#' @return A character vector
#' @export
skanesjh <- function(sjh, 
                     replacements = 'Lund|Malmö|Kristianstad|Helsingborg|Halmstad|Växjö|Ystad',
                     nas = 'övr') {
    skane <- stringr::str_extract(sjh, replacements) |> tidyr::replace_na(nas)
}

#' Import data from one or more excel files from SNQ (or elsewhere)
#'
#' @param paths A character vector of basenames of files
#' @param ... Arguments forwarded to read_excel
#'
#' @return A data frame
#' @export
filesToDf <- function(paths, ...) {
    paths |>
        rlang::set_names(basename) |>
        purrr::map(\(f) readxl::read_excel(file.path(findDataPath(), f), ...)) |>
        purrr::list_rbind(names_to = 'fil')
}

#' Find path where data files are kept
#'
#' @return A character string, either "data/" or "/home/johans/externadata/"
#' @export
findDataPath <- function() {
    if(Sys.info()['nodename'] == 'm93p') return('/home/johans/externadata/')
    else return('data/')
}

#' Find and prefix variables from follow-up visits at 2 and 5.5 yrs of age
#'
#' @param uppfdf A data frame of follow-up data from SNQ
#' @param vilka A vector of strings specifying one or more of 'demo'graphic variables, '2år' follow up at 2 yrs, '5,5år' follow up at 5.5 yrs
#' @param brytvar A string identifying the variable at the start of both blocks of follow-up variables
#' 
#' @return A character vector
uppf_vilkaVar <- function(uppfdf,
                          vilka = c('demo', '2år', '5,5år'),
                          brytvar = 'Uppföljning skapad av'
                          ) {
    namnen <- names(uppfdf) |> stringr::str_replace('\\.\\.\\.\\d+', '')
    bryt <- namnen |> str_detect(brytvar) |> which()
    demo <- stringr::str_c('demo:', namnen)[1:(bryt[1] -1)]
    två <- stringr::str_c('2år:', namnen)[bryt[1]:(bryt[2] -1)]
    femkommafem <- stringr::str_c('5,5år:', namnen)[bryt[2]:length(namnen)]
    utregex <- stringr::str_c(vilka, ':', collapse = '|')
    utnamnen <- c(demo, två, femkommafem)
    utnamnen[stringr::str_detect(utnamnen, utregex)]
}

uppf_fixaVar <- function(uppfdf) {
    namnen <- names(uppfdf)
    bryt <- tibble::tibble(namnen) |>
        dplyr::filter(stringr::str_detect(namnen, 'Uppföljning skapad av')) |>
        dplyr::pull() |>
        stringr::str_replace('.*\\.(\\d+)', '\\1') |>
        as.numeric()
    demo <- tibble::tibble(namnen) |>
        dplyr::slice(1:(bryt[1] - 2)) |>
        dplyr::mutate(namnen = stringr::str_c('demo: ', namnen))
    två <- tibble::tibble(namnen) |>
        dplyr::slice((bryt[1] + 1):bryt[2]) |>
        dplyr::mutate(namnen = namnen |>
                   stringr::str_replace('\\.\\.\\.\\d+', ''),
               namnen = stringr::str_c('2år: ', namnen))
    femkommafem <- tibble::tibble(namnen) |>
        dplyr::slice((bryt[2] + 1):dplyr::n()) |>
        dplyr::mutate(namnen = namnen |>
                   stringr::str_replace('\\.\\.\\.\\d+', ''),
               namnen = stringr::str_c('5,5år: ', namnen))
    fixade <- dplyr::bind_rows(demo, två, femkommafem) |> dplyr::pull(namnen)
    names(uppfdf) <- fixade
    uppfdf
}

uppf_grupper <- function(grupp = 1:6) {
    print('grupp 1: < GV 26')
    print('grupp 2: GV 26+0 -- 27+6')
    print('grupp 3: GV 28+0 -- 32+0')
    print('grupp 4: < -3 SD')
    print('grupp 5: svårt sjuk, ex. NEC, VOC, ECMO, NO, stor kir')
    print('grupp 6: kyl, kramp, stroke, hjärnblödn')
    tribble(~gr,       ~bed,  ~GV40, ~GV44, ~m2,   ~m3,   ~m10,  ~m18,   ~m24,  ~m66,
            'gr1,6',   'ssk',   'x',   'x',   'x',   'x',    'x',    'x',    'x',    'x',
            'gr1,6',   'fys',    '',    '',   'x',   'x',    'x',    'x',     '',    'x',
            'gr1,6',   'neo',    '',    '',    '',    '',     '',     '',    'x',    'x',
            'gr1,6',   'neu',    '',    '',  'vb',   'x',    'x',    'x',     '',     '',
            'gr1,6',   'psy',    '',    '',    '',    '',     '',     '',    'x',    'x',
            'gr2,4,5', 'ssk',   'x',   'x',   'x',   'x',    'x',    'x',    'x',    'x',
            'gr2,4,5', 'fys',    '',    '',   'x',   'x',    'x',    'x',     '',    'x',
            'gr2,4,5', 'neo',    '',    '',    '',   'x',    'x',    'x',    'x',    'x',
            'gr2,4,5', 'neu',    '',    '',    '',    '',     '',     '',     '',     '',
            'gr2,4,5', 'psy',    '',    '',    '',    '',     '',     '',    'x',    'x',
            'gr3',     'ssk',   'x',   'x',   'x',   'x',    'x',     '',     '',     '',
            'gr3',     'fys',    '',    '',   'x',   'x',    'x',     '',     '',     '',
            'gr3',     'neo',    '',    '',    '',   'x',    'x',     '',     '',     '',
            'gr3',     'neu',    '',    '',  'vb',   'x',    'x',     '',     '',     '',
            'gr3',     'psy',    '',    '',    '',    '',     '',     '',     '',     '')
}
