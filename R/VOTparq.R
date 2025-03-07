read_VOTparq = function(filename, meta_col = TRUE, meta_tab = FALSE, ...){

  if(!requireNamespace("arrow", quietly = TRUE)){
    stop('The arrow package is needed for parquet files. Please install from CRAN.', call. = FALSE)
  }

  parq = arrow::read_parquet(filename, as_data_frame = FALSE)
  VOTraw = parq$metadata$`IVOA.VOTable-Parquet.content`

  header = read_VOTable(VOTraw, asText = TRUE, meta_col = meta_col, meta_tab = meta_tab, meta_only = TRUE)

  table = as.data.frame(parq)
  colnames(table) = header$meta_col$name

  attributes(table)$metadata = parq$metadata
  attributes(table)$meta_col = header$meta_col
  attributes(table)$meta_tab = header$meta_tab

  return(table)
}

write_VOTparq = function(table, filename, meta_extra = NULL, meta_overwrite = TRUE, ...){

  lapply(list(), assert_character, len=1, null.ok=TRUE)

  if(!requireNamespace("arrow", quietly = TRUE)){
    stop('The arrow package is needed for parquet files. Please install from CRAN.', call. = FALSE)
  }

  if(!is.data.frame(table)){
    stop('Input table must be a data.frame!')
  }

  header = write_VOTable(table, meta_only = TRUE)

  if(!inherits(table, 'arrow_table')){
    metadata_orig = attributes(table)$metadata
    table = arrow::as_arrow_table(table)
    table$metadata = metadata_orig
  }

  if(!is.null(meta_extra)){
    #need to loop and add
    meta_extra_names = names(meta_extra)
    for(i in seq_along(meta_extra_names)){
      if(meta_overwrite){
        table$metadata[[meta_extra_names[i]]] = meta_extra[i]
      }else{
        if(!meta_extra_names[i] %in% table$metadata){
          table$metadata[[meta_extra_names[i]]] = meta_extra[i]
        }
      }
    }
  }



  table$metadata$`IVOA.VOTable-Parquet.content` = header
  table$metadata$`IVOA.VOTable-Parquet.version` = '1.0'

  arrow::write_parquet(table, filename, ...)
}
