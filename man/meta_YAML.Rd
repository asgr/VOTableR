\name{meta_YAML}
\alias{meta_YAML}
\title{Generate Metadata YAML from a Data Frame}
\description{
  Creates a metadata YAML structure based on the columns and types of a given data.frame, Parquet file or Arrow Dataset.
  Optionally returns the metadata as a YAML-formatted string.
}
\usage{
meta_YAML(data, yaml_out = TRUE, input = 'table', dataset = 'dataset', ...)
}
\arguments{
  \item{data}{
\code{data.frame}, \code{tibble} or Arrow Dataset containing the table or meta data to describe.
}
  \item{yaml_out}{
Logical. If \code{TRUE}, returns the metadata as a YAML-formatted string. If \code{FALSE}, returns a named list.
}
  \item{input}{
Character scalar; input type. Allowed are 'table' (a 2D view of the data table) or 'meta' (a data.frame view of fields meta data, see Examples). 
}
  \item{dataset}{
Character scalar. The schema name for a dataset (e.g. 'DMU' for GAMA and 'RIP' for WAVES).
}
  \item{\dots}{
Modify default entries (see Examples).
}
}
\details{
  This function inspects the schema of the input data using the \code{arrow} package (if available),
  and constructs a metadata header including dataset name, version, authorship, dependencies, and
  a description of each field. Field types are inferred either from the \code{arrow::schema()} or
  from base R classes.

  The resulting metadata is suitable for use in data documentation, archiving, or interoperability
  with systems that consume YAML metadata.
}
\value{
  A character string containing YAML-formatted metadata if \code{yaml_out = TRUE}, otherwise a named list.
}
\author{
  Aaron Robotham
}
\seealso{
  \code{\link[yaml]{as.yaml}}, \code{\link[arrow]{schema}}
}
\examples{
df = data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  Value = c(10.5, 20.3, 15.2, 18.8, 22.1)
)

cat(meta_YAML(df))
cat(meta_YAML(df, dataset = 'RIP'))
cat(meta_YAML(df, dataset = 'RIP', RIP = 'GroupFinding'))
cat(meta_YAML(df, table = 'Example Name'))
cat(meta_YAML(df, table = 'Example Name', comment = NULL)) #delete comment field
cat(meta_YAML(df, table = 'Example Name', comment = list('Comment 1', 'Comment 2')))

#The data.frame view of the fields meta data:
meta = rbindlist(meta_YAML(df, yaml_out=FALSE)$fields)
print(meta) 

#Convert back to the YAML format:
cat(meta_YAML(meta, input = 'meta'))
}
