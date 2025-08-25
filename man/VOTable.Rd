\name{VOTable}
\alias{VOTable}
\alias{read_VOTable}
\alias{write_VOTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VOTable Read and Write
}
\description{
Basically what the title says: functions to read and write VOTable files!
}
\usage{
read_VOTable(filename, meta_col_read = TRUE, meta_tab_read = TRUE, meta_only = FALSE,
  asText = FALSE, data.table = TRUE, ...)

write_VOTable(table, filename = NULL, meta_col = NULL, meta_only = FALSE, version = '1.3',
  ns = "http://www.ivoa.net/xml/VOTable/v1.3")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to VOTable file, either to be written to or read. Can also be a string containing the full VOTable when setting \option{asText} to TRUE. The latter mode is probably only useful for very small VOTables or when processing VOTable format headers like we do for Parquet VOTable format data. If \option{filename} is NULL when writing with \code{write_VOTable} then the VOTable will be returned from the function for use in R directly (can be useful for inspecting the contents).
}
  \item{table}{
Data.frame; the table to be writen out.
}
  \item{meta_col_read}{
Logical; should we read in columns meta data? This is attached as meta_col to the \code{\link{attributes}} of the returned table.
}
  \item{meta_tab_read}{
Logical; should we read in table meta data? This is attached as meta_tab to the \code{\link{attributes}} of the returned table.
}
  \item{meta_only}{
Logical; should we only read in (and return) meta data? This is useful if you just want to know about the meta data (a bit like the FITS header) rather than load the actual contents.
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{meta_col}{
Data.frame; optional way to load meta data into the VOParquet output. Data.frame can optionally contain the columns: Name (column names); Units (column units, optional); Description (verbose column description, optional); UCD (unique column descriptors using IVOA standards, optional); Datatype (column data types, but usually these should be detected from the \option{table}); Arraysize (String length, optional).
}
  \item{asText}{
Logical; should \option{filename} be a string containing the full VOTable?
}
  \item{version}{
Character scalar; version of VOTable (i.e. the IVOA VOTable version) being used. Usually leave as the default.
}
  \item{ns}{
Character scalar; namespace of the XML VOTable main node (usually leave as the default).  
}
  \item{\dots}{
Other arguments to pass to \code{\link{xmlParse}}.
}
}
\details{
Details to come...
}
\value{
\code{read_VOTable}: A data.frame with relevant meta data attached to the attributes 'meta_col' and 'meta_tab'.

\code{write_VOTable}: If \option{filename} is NULL when writing with \code{write_VOTable} then the VOTable will be returned from the function for use in R directly (can be useful for inspecting the contents).
}
\author{
Aaron Robotham
}
\note{
See \url{https://www.ivoa.net/documents/VOTable/}.
}
\seealso{
\code{\link{read_VOParquet}}, \code{\link{write_VOParquet}}
}
\examples{
file_VOT = system.file('extdata', 'VOTable_ex.vot', package = "VOTableR")
table = read_VOTable(file_VOT)

print(table)

print(attributes(table)$meta_col)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
