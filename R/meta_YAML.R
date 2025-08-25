meta_YAML = function(data, yaml_out=TRUE, input='table', dataset='dataset', ...){
  
  if(!requireNamespace("arrow", quietly = TRUE)){
    stop('The arrow package is needed for parquet files. Please install from CRAN.', call. = FALSE)
  }
  
  i = NULL
  
  if(input == 'table'){
    temp_schema = arrow::schema(data)$fields
    col_names = names(data)
    fields = foreach(i = 1:dim(data)[2])%do%{
      list(
        name = col_names[i],
        unit = NULL,
        description = NULL,
        ucd = NULL,
        data_type = if(is.null(temp_schema)){
          switch(class(data[[i]])[1],
                 integer = "int",
                 integer64 = "long",
                 numeric = "double",
                 character = "char",
                 "char") 
        }else{
          temp_schema[[i]]$type$ToString()
        }
      )
    }
  }else if(input == 'meta'){
    fields = apply(data, MARGIN=1, as.list)
  }else{
    stop('input must be table or meta!')
  }
  
  header = list(
    survey = "Survey Name",
    dataset = "Dataset Name",
    table = "Table Name",
    version = "0.0",
    date = as.character(Sys.Date()),
    author = "Lead Author <email>",
    coauthors = list(
      "Co-Author 1 <email1>",
      "Co-Author 2 <email2>"
    ),
    depend = list(
      "Dataset 1 this depends on [optional]",
      "Dataset 2 this depends on [optional]"
    ),
    comment = list(
      "Something interesting about the data [optional]",
      "Something else [optional]"
    ),
    fields = fields
  )
  
  names(header)[2] = dataset
  header[[2]] = paste(dataset,'Name')
  
  dots = list(...)
  
  if(length(dots) > 0){
    for(i in 1:length(dots)){
      header[[names(dots)[i]]] = dots[[i]]
    }
  }
  
  if(yaml_out){
    if(!requireNamespace("yaml", quietly = TRUE)){
      stop('The yaml package is needed for output files. Please install from CRAN.', call. = FALSE)
    }
    header = yaml::as.yaml(header)
    header = gsub(": ''\n", ": \n", header)
    header = gsub(": ~\n", ": \n", header)
    header = gsub(": .na\n", ": \n", header)
    header = gsub(": .na.integer\n", ": \n", header)
    header = gsub(": .na.real\n", ": \n", header)
    header = gsub(": .na.character\n", ": \n", header)
    header = gsub(": .nan\n", ": \n", header)
  }
  
  return(header)
}
