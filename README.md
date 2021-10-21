# vedar
A collection of functions to enable the import and analysis of the outputs of Veda TIMES runs in R.


## Installation

Install vedar from GitHub with:

    # install.packages("devtools")
    devtools::install_github("datasciencescotland/vedar", build_vignettes = T)

If the above does not work, you can install from source:

1. Go to the vedar repository on GitHub
2. Click Clone or download then Download ZIP
3. Save the file locally  and Unzip
4. Install with install.packages()

    install.packages("your/directory/vedar", repos = NULL,
                 type="source")
                 
## Usage

- import_vd("vd_filename.VD") imports the VD data to a tibble
- import_vds("vd_filename.VDS") imports the VDS set information to tibble
- import_vde("vd_filename.VDE") imports the VDE  description information to tibble
- prep_data("vd_filename_base") calls the three import_* functions, joins data, and standardises case and some missing data. If use_sector_def_strings == T, then also appends sectors based on string specification in define_sectors(). Else, no sector information appended
- prep_sector_dat(sector_dat) converts cases to lower and removes spaces from column names in sector_dat. sector_dat needs to exist as a tibble. Can be imported with read_*()
- define_sector_from_list(dat, join_variable_name, sector_dat, sector_info_column, sector_dat_join_variable_colsector_dat) appends the sector information in sector_dat to dat joining on join_variable_name. See ?define_sector_from_list
- make_res(dat, period_select, region_select, sector_select, node_labels, edge_labels, font_size)  creates a RES for selected region, period and sector
- make_graph_from_veda_df(dat) creates an igraph graph object from a veda data dataframe. If only a single period in data, the weights of the edges are set to the value of a var_fout of the commodity
- syscost(dat) extract the system cost by region
