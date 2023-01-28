# This function generates the client-side HTML for a date input
bootstrapDateInput <- function(inputId, class = class, value = "") {
  tagList(
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page.
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "bootstrap-date-input-binding.js")
      )
    ),
    shiny::tags$input(id = inputId, type = "date", value = value, class = class)
  )
}
