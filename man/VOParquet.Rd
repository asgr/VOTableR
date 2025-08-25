\name{VOParquet}
\alias{VOParquet}
\alias{read_VOParquet}
\alias{write_VOParquet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VOParquet Read and Write
}
\description{
Basically what the title says: functions to read and write VOParquet files!
}
\usage{
read_VOParquet(filename, meta_col_read = TRUE, meta_tab_read = FALSE, data.table = TRUE, ...)

write_VOParquet(table, filename, meta_col = NULL, meta_extra = NULL,
  meta_overwrite = TRUE, version = '1.0', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to VOParquet file, either to be written to or read.
}
  \item{table}{
Data.frame; the table to be writen out.
}
  \item{meta_col_read}{
Logical; should we read in columns meta data? This is attached as meta_col to the \code{\link{attributes}} of the returned table.
}
  \item{meta_tab_read}{
Logical; should we read in table meta data? This is attached as meta_tab to the \code{\link{attributes}} of the returned table. For VOParquet this should usually be FALSE since the top level meta data for the table is usually included in the Parquet meta data directly, not in the embedded VOTable header (which normally only contains column meta data).
}
  \item{data.table}{
Logical; should a \code{data.table} be returned?
}
  \item{meta_col}{
Data.frame; optional way to load meta data into the VOParquet output. Data.frame can optionally contain the columns: Name (column names); Units (column units, optional); Description (verbose column description); UCD (unique column descriptors using IVOA standards, optional); Datatype (column data types, but usually these should be detected from the \option{table}); Arraysize (String length, optional).
}
  \item{meta_extra}{
List; optional list of additional meta data to include when writing out the Parquet file. This must be a simple list(name1 = 'sting1', name2 = 'string2') dictionary style list due to Parquet meta data limitations.
}
\item{meta_overwrite}{
Logical; if there is a name clash between the embedded meta data of \option{table} and the extra meta data of \option{meta_extra}, should the embedded data take precedence (FALSE) or the extra meta data (TRUE).
}
  \item{version}{
Character scalar; version of VOparquet (i.e. the IVOA VOTable Parquet version) being used. Usually leave as the default.
}
  \item{\dots}{
Other arguments to pass to \code{\link{xmlParse}} (\code{read_VOParquet}) or \code{write_parquet} (\code{write_VOParquet}).
}
}
\details{
Details to come...
}
\value{
\code{read_VOParquet}: A data.frame with relevant meta data attached to the attributes 'meta_col' and 'meta_tab'. The full unedited original metadata of the source Parquet file (which is un-parsed by \code{XML}) is attached to 'metadata'.

\code{write_VOTable}: Run for the side effect of saving a table to disk.
}
\author{
Aaron Robotham
}
\note{
See \url{https://www.ivoa.net/documents/Notes/VOParquet/}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read_VOTable}}, \code{\link{write_VOTable}}
}
\examples{
file_VOP = system.file('extdata', 'VOParquet_ex.parquet', package = "VOTableR")
table = read_VOParquet(file_VOP)

print(table[1:5,])

print(attributes(table)$meta_col)

# The full metadata of the original file:
print(attributes(table)$metadata)
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
