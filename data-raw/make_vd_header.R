vd_filename <- "demos_001.vd"
vde_filename <- "demos_001.vde"
vds_filename <- "demos_001.vds"

vd_filename <- system.file("extdata",
            vd_filename,
            package = "vedar")

vde_filename <- system.file("extdata",
                           vde_filename,
                           package = "vedar")

vds_filename <- system.file("extdata",
                           vds_filename,
                           package = "vedar")


vd_header <- scan(vd_filename, skip = 2, what = character(),  nmax = 35 )

vde_file <- utils::read.csv(vde_filename, sep = ",", header = F)

vds_file <- utils::read.csv(vds_filename, sep = ",", header = F)

.vde_reference_structure <- list(vd_header = vd_header,
                                 vde_file = vde_file,
                                 vds_file = vds_file)

usethis::use_data(.vde_reference_structure, internal = T, overwrite = T)
