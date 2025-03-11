# VOTableR

## Overview

**VOTableR** is an R package with basic tools to read/write VOTable with embedded TABLEDATA and also to read/write VOParquet files with a VOTable header in the metadata.

## Installation

You can install **VOTableR** from CRAN using:

```r
install.packages("VOTableR")
```

Or install the development version from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

devtools::install_github("asgr/VOTableR")
```

## Usage

### Load the package

```r
library(VOTableR)
```

### Read a VOTable file

```r
file_VOT = system.file('extdata', 'VOTable_ex.vot', package = "VOTableR")
votable <- read_VOTable(file_VOT)
print(votable)
```

### Write a VOTable file

```r
write_VOTable(votable, "output.vot")
```

### Read a VOParquet file

```r
file_VOP = system.file('extdata', 'VOParquet_ex.parquet', package = "VOTableR")
VOP_table = read_VOParquet(file_VOP)
print(VOP_table)
```

### Write a VOParquet file

```r
write_VOParquet(VOP_table, "output.parquet")
```

## Features

- Supports parsing VOTable XML format.
- Extracts metadata and table content.
- Converts VOTable data to R-friendly formats (data frames, lists).
- Reads/Writes structured VOTable XML files.
- Reads/Writes VOParquet (with VOTable header) files.

## Contributing

Contributions, bug reports, and feature requests are welcome! Feel free to open an issue or submit a pull request on the [GitHub repository](https://github.com/asgr/VOTableR).

Written largely by Aaron Robotham, but thanks for helpful comments to Mark Taylor (TOPCAT, STILTS etc) and Jochen Liske (WAVES PI and database TWG lead).

## License

This package is licensed under the LGPL-3 License.

## References

- VOTable Format: [IVOA Standard](https://www.ivoa.net/documents/VOTable/)
- VOParquet Format: [IVOA Standard](https://www.ivoa.net/documents/Notes/VOParquet/)

