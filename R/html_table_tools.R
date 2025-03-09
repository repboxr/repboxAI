example = function() {
  library(xml2)
  # Example usage:
  html_text <- "
<table>
  <tr><td>1</td> <td>2</td>   <td>3</td></tr>
  <tr><td colspan='2'>45</td> <td>6</td></tr>
</table>
  "

  cat(html_table_add_rows_cols(html_text))
  cat(html_table_add_cell_num_row_col(html_text))

  html_text <- "
  <h1>My table</h1>
<table>
  <tr><td>1</td> <td>2</td>   <td rowspan='2'>2x1</td> <td>3</td></tr>
  <tr><td colspan='2'>45</td> <td>6</td></tr>
</table>
  "
  html_tab = html_table_add_cell_num_row_col(html_text)
  cat(html_tab)
}

xml_all_text <- function(node) {
  return(rvest::html_text2(node))
  
  # Select all nodes (elements, text nodes, etc.) in the subtree including the node itself.
  nodes <- xml_find_all(node, "descendant-or-self::node()")
  
  # For each node, extract its text content (trimming any extra whitespace)
  texts <- vapply(nodes, function(n) xml_text(n, trim = TRUE), character(1))
  
  # Remove any empty strings and return the result as a character vector.
  texts[nzchar(texts)]
}
  
example = function() {
  tabhtml = "<table><td> My text <span>is here</span>, right?</td></table>"
  tabhtml = '<table><td style="text-align: right;"><span class="math inline">(16.7)</span></td></table>'
  html_table_cells_to_text(tabhtml, all_text=TRUE)
}

html_table_cells_to_text <- function(tabhtml, all_text = TRUE) {
  restore.point("html_table_cells_to_text")
  # Parse the input HTML string.
  
  doc = read_html(tabhtml)
  table_node = rvest::html_element(doc, "table")

  # Find all <td> and <th> cells in the table.
  cell_nodes <- xml2::xml_find_all(table_node, ".//td | .//th")
  node = cell_nodes[[1]]
  for (node in cell_nodes) {
    if (all_text) {
      text_content = paste0(unique(trimws(xml_all_text(node))), collapse="")
    } else {
      text_content <- xml2::xml_text(node)
    }
    # Remove all child nodes
    xml_remove(xml_children(node))
    # Set the new text content
    xml_text(node) <- text_content
    as.character(node)
  }
  as.character(table_node)
}

html_table_add_cellnum_row_col <- function(html, id_prefix = "cell-") {
  library(xml2)
  restore.point("html_table_add_cellnum_row_col")
  if (is.null(html)) return(NULL)
  
  # Parse the HTML string into an XML document
  doc <- read_html(html)

  # Find the first table in the document
  table_node <- xml_find_first(doc, ".//table")

  # Get all row nodes in the table
  rows <- xml_find_all(table_node, ".//tr")

  cell_num <- 1
  # Preallocate a list for occupancy, one element per row
  occupied <- vector("list", length(rows))

  # Process each row
  for (r in seq_along(rows)) {
    # Initialize occupancy for the current row if needed
    if (is.null(occupied[[r]])) {
      occupied[[r]] <- integer(0)
    }

    tr = rows[[r]]
    # Remove any data-* attributes
    data_attrs <- names(xml_attrs(tr))[grep("^data-", names(xml_attrs(tr)))]
    for (attr in data_attrs) {
      xml_attr(tr, attr) <- NULL
    }
    
    
    col <- 1  # starting column index for this row
    # Get all cells (<td> or <th>) in the row
    cells <- xml_find_all(rows[[r]], "./th|./td")

    for (cell in cells) {
      # Skip columns already occupied by a previous rowspan cell
      while (col %in% occupied[[r]]) {
        col <- col + 1
      }
      cell_col <- col  # starting column for this cell

      # Get colspan and rowspan attributes (default to 1 if not provided)
      colspan_attr <- xml_attr(cell, "colspan")
      colspan <- if (!is.na(colspan_attr)) as.integer(colspan_attr) else 1

      rowspan_attr <- xml_attr(cell, "rowspan")
      rowspan <- if (!is.na(rowspan_attr)) as.integer(rowspan_attr) else 1

      # Remove any existing id or class attributes
      xml_attr(cell, "id") <- NULL
      xml_attr(cell, "class") <- NULL
      xml_attr(cell, "style") <- NULL
      
      # Remove any data-* attributes
      data_attrs <- names(xml_attrs(cell))[grep("^data-", names(xml_attrs(cell)))]
      for (attr in data_attrs) {
        xml_attr(cell, attr) <- NULL
      }
      
      # Add new attributes:
      xml_set_attr(cell, "id", paste0(id_prefix, cell_num))
      xml_set_attr(cell, "class", paste0("row-", r, " col-", cell_col))
      cell_num <- cell_num + 1

      # Determine which columns this cell occupies
      cols_occupied <- seq(cell_col, cell_col + colspan - 1)

      # Mark these columns as occupied in the current row
      occupied[[r]] <- union(occupied[[r]], cols_occupied)

      # For cells with rowspan, mark these columns as occupied
      # in subsequent rows—but only up to the number of rows in the table.
      if (rowspan > 1) {
        for (i in seq(from = r + 1,
                      to = min(r + rowspan - 1, length(rows)))) {
          if (is.null(occupied[[i]])) {
            occupied[[i]] <- integer(0)
          }
          occupied[[i]] <- union(occupied[[i]], cols_occupied)
        }
      }

      # Advance the column pointer by the colspan amount
      col <- cell_col + colspan
    }
  }

  # Return the modified table as an HTML string
  as.character(table_node)
}

library(rvest)
library(dplyr)

normalized_html_tab_to_cell_df <- function(html) {
  restore.point("normalized_html_tab_to_cell_df")
  # Parse HTML
  doc <- read_html(html)
  
  # Extract all td and th elements
  #cells <- html_elements(doc, "td")
  cells <- html_elements(doc, "td, th")
  
  # Extract attributes and text content vectorized
  cell_ids <- html_attr(cells, "id")
  cell_classes <- html_attr(cells, "class")
  cell_html <- as.character(cells)
  cell_inner_html <- stri_match_first_regex(cell_html, "<(?:td|th)[^>]*>(.*?)</(?:td|th)>")[,2]
  cell_inner_html = na_val(cell_inner_html, "")
  cell_text = html_text2(cells)
  cell_colspan <- as.integer(html_attr(cells, "colspan"))
  cell_colspan[is.na(cell_colspan)] <- 1  # Default colspan is 1

  cell_rowspan <- as.integer(html_attr(cells, "rowspan"))
  cell_rowspan[is.na(cell_rowspan)] <- 1  # Default rowspan is 1
  
  # Extract row and column numbers from classes
  # Using vectorized string operations
  row_nums <- as.integer(sub(".*row-(\\d+).*", "\\1", cell_classes))
  col_nums <- as.integer(sub(".*col-(\\d+).*", "\\1", cell_classes))
  
  # Create the dataframe
  df <- data.frame(
    cellid = cell_ids,
    row = row_nums,
    col = col_nums,
    colspan = cell_colspan,
    rowspan = cell_rowspan,
    html = cell_html,
    inner_html = cell_inner_html,
    text = cell_text,
    stringsAsFactors = FALSE
  )
  
  # Sort by row and column
  #df <- df[order(df$row, df$col), ]
  
  # Reset row names
  rownames(df) <- NULL
  
  return(df)
}

rai_write_all_tables_html = function(tab_df,html_file=NULL, html_col = "tabhtml", out_dir=NULL, info=NULL, title=NULL) {
  restore.point("rai_write_all_tables_html")
  style = "<style> 
  table { border-spacing: 0px; border-collapse: collapse;}
  table td {padding-left: 4px; padding-right: 4px; padding-top: 2px; padding-bottom: 2px; border: 1px solid lightgray;} </style>"
  meta_head = '<head><meta charset="UTF-8"></head>'
  head = paste0(meta_head, "\n", style)
  if (!is.null(title)) head = paste0(head,"<h1>", title, "</h1>")
  if (!is.null(out_dir)) head = paste0(head,"<p><pre>", out_dir, "</pre></p>")
  tab_html = paste0(paste0("<h2>Table ", tab_df$tabid,"</h2>", tab_df$tabhtml,"<br>", collapse = "\n"))
  foot = ""
  if (!is.null(info) & !is.null(out_dir)) {
    info = as.list(info)
    foot = paste0(foot, "<ul>", paste0("<li>",names(info), ": ", info, " </li>", collapse="\n"),"</ul>")
  }

  
  html = paste0(head, "\n", tab_html, foot)
  if (is.null(html_file)) return(invisible(html))
  if (basename(html_file)==html_file & !is.null(out_dir)) html_file = file.path(out_dir, html_file)
  writeUtf8(html, html_file)
  #writeLines(html, html_file)
  invisible(html)
  
}


html_escape <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(text)
  }
  
  # Convert to character if not already
  text <- as.character(text)
  
  # Use stringi's vectorization capabilities directly
  # Note: Order matters - & must be replaced first to avoid double-escaping
  text <- stringi::stri_replace_all_fixed(text, "&", "&amp;")
  text <- stringi::stri_replace_all_fixed(text, "<", "&lt;")
  text <- stringi::stri_replace_all_fixed(text, ">", "&gt;")
  text <- stringi::stri_replace_all_fixed(text, "\"", "&quot;")
  text <- stringi::stri_replace_all_fixed(text, "'", "&#39;")
  
  return(text)
}


html_unescape <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return(text)
  }
  
  # Convert to character if not already
  text <- as.character(text)
  
  # Replace HTML entities with their corresponding characters
  # Note: Order matters - & must be replaced last to avoid issues with other entities
  text <- stringi::stri_replace_all_fixed(text, "&quot;", "\"")
  text <- stringi::stri_replace_all_fixed(text, "&#39;", "'")
  text <- stringi::stri_replace_all_fixed(text, "&lt;", "<")
  text <- stringi::stri_replace_all_fixed(text, "&gt;", ">")
  text <- stringi::stri_replace_all_fixed(text, "&amp;", "&")
  
  return(text)
}
