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


html_table_add_cellnum_row_col <- function(html) {
  library(xml2)
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

      # Add new attributes:
      xml_set_attr(cell, "id", paste0("cell-", cell_num))
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
