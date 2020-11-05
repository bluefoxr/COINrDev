#' Generate a unit report as a Word Doc
#'
#' Biulds an R Markdown document and knits to Word, for a given unit.
#'
#' @param country The country/unit to build the doc for.
#' 
#' @examples coin_unitreport(country = "Italy")
#'
#' @return Outputs Word docs.
#'
#' @export

coin_unitreport <- function(country){
  rmarkdown::render('country_report_source.Rmd', params = list(country = country),
                    output_file = paste0(country, '_report', '.docx'))
}