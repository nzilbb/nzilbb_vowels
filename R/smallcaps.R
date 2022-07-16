smallcaps <- function() {

  # Get currently selected ranges.
  doc_selections <- rstudioapi::getSourceEditorContext()['selection']

  for (i in seq_along(doc_selections$selection)) {
    # Add text
    rstudioapi::modifyRange(
      location=doc_selections$selection[[i]]$range,
      text=paste0(
        '<span style="font-variant: small-caps;">',
        doc_selections$selection[[i]]$text,
        '</span>'
      )
    )
  }

  # rstudioapi::setCursorPosition(
  #   rstudioapi::document_position(
  #     row=1,
  #     column=1)
  #   )


  doc_ranges <- list()

  for (i in seq_along(doc_selections$selection)) {

    shifted_start <- doc_selections$selection[[i]]$range$start + c(0, 40)
    shifted_end <- doc_selections$selection[[i]]$range$end + c(0, 40)

    new_range = rstudioapi::document_range(
      start = rstudioapi::document_position(
        row = shifted_start[[1]],
        column = shifted_start[[2]]
      ),
      end = rstudioapi::document_position(
        row = shifted_end[[1]],
        column = shifted_end[[2]]
      )
    )

    doc_ranges <- append(doc_ranges, list(new_range))

  }

  # Select content within span.
  rstudioapi::setSelectionRanges(
    ranges = doc_ranges
  )

}
