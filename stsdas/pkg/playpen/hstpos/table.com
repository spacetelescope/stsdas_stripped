# TABLE.COM -- Common block for HSTPOS table I/O.

int     row                                 # Current row of output table.

char    col_names[SZ_COLNAME, MAX_COLUMNS]  # Output table column names.

pointer columns[MAX_COLUMNS]                # Output table column descriptors.
pointer table                               # Table descriptor.

# Define the common block.
common /table/ row, col_names, columns, table
