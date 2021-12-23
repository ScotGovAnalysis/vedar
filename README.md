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
- import_vdt("vd_filename.VDT") imports the structural data that is available to the model, as well as user constraints. The latter is indicated by `direction == "uc"`.
- prep_data("vd_filename_base") calls the three import_* functions, joins data, and standardises case and missing data. 
- prep_sector_dat(sector_dat) converts cases to lower and removes spaces from column names in sector_dat. sector_dat needs to exist as a tibble. Can be imported with read_*()
- prep_vdt_data calls import_vdt() and import_vde joins the data, standardises case and missing data.
- define_sector_from_list(dat, join_variable_name, sector_dat, sector_info_column, sector_dat_join_variable_colsector_dat) appends the sector information in sector_dat to dat joining on join_variable_name. See ?define_sector_from_list
- make_res(dat, node_labels, edge_labels, sankey_width,  sankey_height, font_size, use_weights, input_data_type)  creates a RES from vd or vdt tibble. Data must only contain a single region. Magnitudes of flows are represented if data includes only a single period, and if use_weights = F.
- make_graph_from_veda_df(dat, node_labels, edge_labels, input_data_type) creates an igraph graph object from a vd or vdt tibble. If only a single period in vd data, the weights of the edges are set to the value of a var_fout of the commodity. Data must only contain single region in case process names are repeated across regions.
- make_res_from_graph(g,  edge_labels, sankey_width,  sankey_height, font_size) creates a RES from an igraph object created by make_graph_from_veda_df, or a subgraph. Sankey edge widths represent the edge_weights.
- check_in_path(node_regex, path): Check whether a process (graph node) containing a specified regular expression is present in a list of paths
- syscost(dat) extract the system cost by region
