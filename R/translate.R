translate <- function(x, to, from = "en") {
  key <- Sys.getenv("MS_TRANSLATE_KEY")
  if (identical(key, ""))
    stop("Please define MS_TRANSLATE_KEY env var", call. = FALSE)
  secret <- Sys.getenv("MS_TRANSLATE_SECRET")
  if (identical(secret, ""))
    stop("Please define MS_TRANSLATE_SECRET env var", call. = FALSE)

  suppressWarnings(translateR::translate(content.vec = x, source.lang = "en", target.lang = to,
    microsoft.client.id = key, microsoft.client.secret = secret))
}

#' Translate text with a shiny gadget.
#'
#' This uses the MS translation API - you need your key and secret
#' set as \code{MS_TRANSLATE_KEY} and \code{MS_TRANSLATE_SECRET}
#' environment variables.
#'
#' @param text Default text to translate.
#' @param to Default language to translate to. See \code{translate_lang()}
#'   to get a full list of options.
#' @return A length one character vector containing the translated text.
#' @export
#' @examples
#' if (interactive())
#'   translate_gadget("My name is Hadley", "fr")
translate_gadget <- function(text = "", to = NULL) {
  langs <- translate_langs()

  ui <- miniUI::miniPage(
    miniUI::miniContentPanel(
      shiny::textInput("text", "Translate: ", value = text, width = "100%"),
      shiny::selectizeInput("to", "To:", langs, selected = to, width = "100%"),
      shiny::div(
        shiny::tags$label("Results:" ),
        shiny::textOutput("translated")
      )
    ),
    miniUI::gadgetTitleBar("")
  )

  server <- function(input, output) {
    translation <- shiny::reactive(translate(input$text, input$to))
    output$translated <- shiny::renderText(translation())

    shiny::observeEvent(input$done, shiny::stopApp(translation()))
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Translation"))
}

#' @export
#' @rdname translate_gadget
translate_langs <- function() {
  unlist(getNamespace("translateR")$languageCodes()$Microsoft)
}


translate_text_addin <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  text <- con$selection[[1]]$text

  translated <- translate_gadget(text)

  rstudioapi::insertText(con$selection[[1]]$range, translated)
}
