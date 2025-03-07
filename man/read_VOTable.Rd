\name{read_VOTable}
\alias{read_VOTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VOTable Read and Write
}
\description{
Basically what the title says: functions to read and write VOTables!
}
\usage{
read_VOTable(filename, meta_col = TRUE, meta_tab = TRUE, meta_only = FALSE,
  asText = FALSE, ...)

write_VOTable(table, filename = NULL, meta_only = FALSE, version = '1.3',
  ns = "http://www.ivoa.net/xml/VOTable/v1.3")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character scalar; path to VOTable file, either to be written to or read. Can also be a string containing the full VOTable when setting \option{asText} to TRUE. The latter mode is probably only useful for very small VOTables or when processing VOTable format headers like we do for Parquet VOTable format data.
}
  \item{meta_col}{
Logical; should we read in columns meta data? This is attached as meta_col to the \code{\link{attributes}} of the returned table.
}
  \item{meta_tab}{
Logical; should we read in table meta data? This is attached as meta_tab to the \code{\link{attributes}} of the returned table.
}
  \item{meta_only}{
Logical; should we only read in (and return) meta data? This is useful if you just want to know about the meta data (a bit like the FITS header) rather than load the actual contents.
}
  \item{asText}{
Logical; should \option{filename} be a string containing the full VOTable?
}
  \item{\dots}{
Other arguments to pass to \code{\link{xmlParse}}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{read_VOTable}: A data.frame with relevant meta data attached to the attributes (meta_col and meta_tab).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Aaron Robotham
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read_VOTparq}}, \code{\link{write_VOTparq}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (filename, meta_col = TRUE, meta_tab = TRUE, meta_only = FALSE, 
    asText = FALSE, ...) 
{
    doc = xmlParse(filename, asText = asText, addAttributeNamespaces = TRUE, 
        ...)
    ns = xmlNamespaceDefinitions(xmlRoot(doc), recursive = F)[[1]]$uri
    if (is.null(ns)) {
        ns = c(ns = "")
        FIELD = "//FIELD"
        DESCRIP = "//TABLE/DESCRIPTION"
        TR = "//TR"
        TD = ".//TD"
    }
    else {
        ns = c(ns = xmlNamespaceDefinitions(xmlRoot(doc), recursive = F)[[1]]$uri)
        FIELD = "//ns:FIELD"
        DESCRIP = "//ns:TABLE/DESCRIPTION"
        TR = "//ns:TR"
        TD = ".//ns:TD"
    }
    table_data = xpathApply(doc, TR, function(row) {
        cells = xpathSApply(row, TD, xmlValue, namespaces = ns)
        return(cells)
    }, namespaces = ns)
    field_name = xpathSApply(doc, FIELD, xmlGetAttr, name = "name", 
        default = NA, namespaces = ns)
    if (meta_col) {
        field_datatype = xpathSApply(doc, FIELD, xmlGetAttr, 
            name = "datatype", default = NA, namespaces = ns)
        field_ucd = xpathSApply(doc, FIELD, xmlGetAttr, name = "ucd", 
            default = NA, namespaces = ns)
        field_unit = xpathSApply(doc, FIELD, xmlGetAttr, name = "unit", 
            default = NA, namespaces = ns)
        meta_col = data.frame(name = field_name, datatype = field_datatype, 
            ucd = field_ucd, unit = field_unit)
    }
    else {
        meta_col = NULL
    }
    if (meta_tab) {
        meta_tab = xmlValue(getNodeSet(doc, DESCRIP)[[1]])
    }
    else {
        meta_tab = NULL
    }
    if (meta_only) {
        return(list(meta_col = meta_col, meta_tab = meta_tab))
    }
    which_numeric = which(field_datatype \%in\% c("single", "double", 
        "float"))
    which_integer = which(field_datatype \%in\% c("short", "long"))
    if (length(table_data) > 0) {
        table = as.data.frame(do.call(rbind, table_data), stringsAsFactors = FALSE)
        colnames(table) = field_name
        if (length(which_numeric) > 0) {
            for (i in which_numeric) {
                table[, i] = as.numeric(table[, i])
            }
        }
        if (length(which_integer) > 0) {
            for (i in which_integer) {
                table[, i] = as.integer(table[, i])
            }
        }
        attributes(table)$meta_col = meta_col
        attributes(table)$meta_tab = meta_tab
        return(table)
    }
    else {
        warning("No data found in the VOTable.")
        return(NULL)
    }
  }
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
