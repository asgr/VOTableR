read_VOTable = function(filename, meta_col=TRUE, meta_tab=TRUE, meta_only=FALSE, asText=FALSE, ...){
  # Parse the XML file
  doc = xmlParse(filename, asText=asText, addAttributeNamespaces=TRUE,  ...)

  ns = xmlNamespaceDefinitions(xmlRoot(doc), recursive = F)[[1]]$uri

  if(is.null(ns)){
    ns = c(ns = '')
    FIELD = "//FIELD"
    DESCRIP = "//TABLE/DESCRIPTION"
    TR = "//TR"
    TD = ".//TD"
  }else{
    ns = c(ns = xmlNamespaceDefinitions(xmlRoot(doc), recursive = F)[[1]]$uri)
    FIELD = "//ns:FIELD"
    DESCRIP = "//ns:TABLE/DESCRIPTION"
    TR = "//ns:TR"
    TD = ".//ns:TD"
  }

  # Extract the table data
  table_data = xpathApply(doc, TR, function(row) {
    # Extract each cell in the row
    cells = xpathSApply(row, TD, xmlValue, namespaces=ns)
    return(cells)
  }, namespaces=ns)

  
  allnodes = getNodeSet(doc, FIELD, namespaces=ns)
  
  # Get field names from FIELD elements
  #field_name = xpathSApply(doc, FIELD, xmlGetAttr, name="name", default=NA, namespaces=ns)
  node = NULL
  
  field_name = foreach(node = allnodes, .combine='c')%do%{
    xmlGetAttr(node, name="name", default=NA)
  }

  if(meta_col){
    field_descrip = foreach(node = allnodes, .combine='c')%do%{
      xmlValue(node)
    }
    
    field_datatype = foreach(node = allnodes, .combine='c')%do%{
      xmlGetAttr(node, name="datatype", default=NA)
    }
    
    field_ucd = foreach(node = allnodes, .combine='c')%do%{
      xmlGetAttr(node, name="ucd", default=NA)
    }
    
    field_unit = foreach(node = allnodes, .combine='c')%do%{
      xmlGetAttr(node, name="unit", default=NA)
    }
    
    field_arraysize = foreach(node = allnodes, .combine='c')%do%{
      xmlGetAttr(node, name="arraysize", default=NA)
    }
    
    #field_datatype = xpathSApply(doc, FIELD, xmlGetAttr, name="datatype", default=NA, namespaces=ns)
    #field_ucd = xpathSApply(doc, FIELD, xmlGetAttr, name="ucd", default=NA, namespaces=ns)
    #field_unit = xpathSApply(doc, FIELD, xmlGetAttr, name="unit", default=NA, namespaces=ns)
    meta_col = data.frame(Name = field_name,
                          Units = field_unit,
                          Description = field_descrip,
                          UCD = field_ucd,
                          Datatype = field_datatype,
                          Arraysize = field_arraysize)
  }else{
    meta_col = NULL
  }

  if(meta_tab){
    meta_tab = xmlValue(getNodeSet(doc, DESCRIP)[[1]])
  }else{
    meta_tab = NULL
  }

  if(meta_only){
    return(list(meta_col=meta_col, meta_tab=meta_tab))
  }

  which_numeric = which(field_datatype %in% c('single', 'double', 'float'))
  which_integer = which(field_datatype %in% c('short', 'long'))

  # Convert the list of rows to a data frame
  if (length(table_data) > 0) {
    table = as.data.frame(do.call(rbind, table_data), stringsAsFactors = FALSE)
    colnames(table) = field_name

    if(length(which_numeric) > 0){
      for(i in which_numeric){
        table[,i] = as.numeric(table[,i])
      }
    }

    if(length(which_integer) > 0){
      for(i in which_integer){
        table[,i] = as.integer(table[,i])
      }
    }

    attributes(table)$meta_col = meta_col
    attributes(table)$meta_tab = meta_tab

    return(table)
  } else {
    warning("No data found in the VOTable.")
    return(NULL)
  }
}

write_VOTable = function(table, filename=NULL, meta_only=FALSE,
                         version = '1.3',
                         ns = "http://www.ivoa.net/xml/VOTable/v1.3") {
  # Create the root VOTable node
  votable_node = newXMLNode("VOTABLE", attrs = c(version = version),
                            namespaceDefinitions = ns)

  # Create the RESOURCE node
  resource_node = newXMLNode("RESOURCE", parent = votable_node)

  # Create the TABLE node
  table_node = newXMLNode("TABLE", parent = resource_node)

  if(!is.null(attributes(table)$meta_tab)){
    descrip_node = newXMLNode("DESCRIPTION", attributes(table)$meta_tab, parent = table_node)
    #newXMLTextNode(attributes(table)$meta_tab, parent=descrip_node)
  }

  if(is.null(attributes(table)$meta_col)){
    # Add FIELD elements based on data.frame columns
    for (col_name in colnames(table)) {
      field_type = switch(class(table[[col_name]])[1],
                          integer = "int",
                          integer64 = "long",
                          numeric = "double",
                          character = "char",
                          "char")
      newXMLNode("FIELD", attrs = c(name = col_name, datatype = field_type), parent = table_node)
    }
  }else{
    meta_col = attributes(table)$meta_col
    for(i in 1:dim(meta_col)[1]){
      new_node = newXMLNode("FIELD", attrs = c(
        arraysize = if(is.na(meta_col[i,'Arraysize'])){NULL}else{meta_col[i,'Arraysize']},
        datatype = switch(class(table[[i]])[1], #regardless of the meta data, we need to use the R data types when writing out
                          integer = "int",
                          integer64 = "long",
                          numeric = "double",
                          character = "char",
                          "char"),
        name = if(is.na(meta_col[i,'Name'])){NULL}else{meta_col[i,'Name']},
        ucd = if(is.na(meta_col[i,'UCD'])){NULL}else{meta_col[i,'UCD']},
        unit = if(is.na(meta_col[i,'Units'])){NULL}else{meta_col[i,'Units']}
      ),
      parent = table_node)
      if(!is.na(meta_col[i,'Description'])){
        newXMLNode('DESCRIPTION', meta_col[i,'Description'], parent=new_node)
      }
    }
  }

  if(meta_only){
    if(!is.null(filename)){
      cat("VOTable meta data saved to:", filename, "\n")
    }

    return(saveXML(votable_node, file = filename, indent = TRUE))
  }

  # Add DATA and TABLEDATA nodes
  data_node = newXMLNode("DATA", parent = table_node)
  tabledata_node = newXMLNode("TABLEDATA", parent = data_node)

  # Add rows and cells to the TABLEDATA
  for (i in seq_len(nrow(table))) {
    tr_node = newXMLNode("TR", parent = tabledata_node)
    for (j in seq_len(ncol(table))) {
      newXMLNode("TD", as.character(table[i, j]), parent = tr_node)
    }
  }

  # Save the XML document to a file
  if(!is.null(filename)){
    cat("VOTable saved to:", filename, "\n")
  }

  return(saveXML(votable_node, file = filename, indent = TRUE))
}
