#' Write data to Excel
#'
#' Takes the COIN object and writes all main data tables and other things to an Excel file. At the moment this is just a placeholder to be developed further.
#' 
#' @param COINobj A COIN object
#' @param fname The file name to write to
#'
#' @examples coin_2excel(COINobj, fname="COINresults.xlsx")
#'
#' @return An Excel workbook with each table on a separate named tab.
#'
#' @export

coin_2excel <- function(COINobj, fname = "COINresults.xlsx"){
  
  if (exists("data",COINlist)){
    write.xlsx(COINlist$data, file = fname)
  }
}
  