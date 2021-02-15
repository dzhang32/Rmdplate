#' Automatically print data.frames as reactable html widget
#'
#' This is intended to replace the default `knitr` printing method for
#' `data.frame` objects and is used in the "rmd_template_dz" to automatically
#' render `data.frame`s as `reactable`s. This also adds a caption to enable the
#' a workaround to allow table cross-referencing in the .Rmd using
#' `\@ref(tab:KEY)`. The solution uses `htmltools` to manually add a caption
#' inputted through the chunk option `fig.cap` and was inspired from
#' https://github.com/glin/reactable/issues/15.
#'
#' @param df a `data.frame`
#' @param options this will be taken as a list of a parameters inputted into the
#'   chunk. Specifically, the `fig.cap` chunk parameter is used to input to
#'   label tables with a caption enabling referencing them in the text.
#' @param ... Included based off this tutorial
#'   https://cran.r-project.org/web/packages/knitr/vignettes/knit_print.html.
#'   Required for `knitr::knit_print`.
#'
#' @return reactable html widget with a caption.
#' @export
#'
#' @importFrom reactable reactable colDef colFormat
#' @importFrom knitr knit_print
#' @importFrom magrittr `%>%`
#'
#' @examples
#'
#' \dontrun{
#' # add this line to the setup chunk of your .Rmd
#' # to overwrite the existing method for printing data.frames
#' registerS3method("knit_print", "data.frame", Rmdplate::knit_print.data.frame)
#' }
knit_print_df <- function(df, options, ...) {

    # convert df to reactable
    res <- df %>%
        reactable(
            searchable = FALSE,
            filterable = FALSE,
            showSortable = TRUE,
            sortable = TRUE,
            highlight = TRUE,
            defaultPageSize = 5,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(5, 10),
            paginationType = "simple",
            defaultColDef = colDef(
                align = "left",
                format = colFormat(digits = 3))
        )

    # add caption using htmltools such that table can be
    # cross references in text
    res <- res %>%
        .add_caption(caption = options$fig.cap)

    res %>% knit_print(...)

}

#' Default printing method for `reactable`s
#'
#' This is added to ensure that, say you convert the `data.frame` to a
#' `reactable` in a code chunk (therefore this will not be passed through
#' `knit_print.data.frame`), you can still use `fig.cap`. This adds a caption to
#' the `reactable` html widget.
#'
#' @inheritParams knit_print_df
#'
#' @param df_react `reactable` data table.
#'
#' @return reactable html widget with a caption.
#' @export
#'
#' @importFrom knitr knit_print
#' @importFrom magrittr `%>%`
#' @importFrom htmltools tagList tags
knit_print_reactable <- function(df_react, options, ...) {

    # add caption using htmltools such that table can be
    # cross references in text
    res <- df_react %>%
        .add_caption(caption = options$fig.cap)

    res %>% knit_print(...)

}

#' @importFrom magrittr `%>%`
#' @importFrom stringr str_split
#' @noRd
.add_caption <- function(df_react, caption = NULL){

    if(is.null(caption)){

        stop("fig.cap must be set to  the format 'KEY: Table caption'")

    }

    caption <- stringr::str_split(caption, ": ") %>%
        unlist()

    if(length(caption) != 2){

        stop("fig.cap must be set to  the format 'KEY: Table caption'")

    }

    caption_tidy <- stringr::str_c("(#tab:",
                                   caption[[1]],
                                   ")",
                                   caption[[2]])

    # use htmltools to add caption manually based off
    # https://github.com/glin/reactable/issues/15
    df_react_w_cap <- htmltools::tagList(
        htmltools::tags$caption(caption_tidy),
        df_react
    )

    return(df_react_w_cap)

}
