# 03-upload.R
# Copy the panel to S3 with a manifest (file, rows, bytes, md5) and the
# package versions the build used.
#
#   s3://nccs-efile/dev/synthid/partvii-v3_1/
#     README.md                     what the files are (from this folder)
#     manifest.csv                  one row per data file
#     build-info-{classify,people}.csv
#     people/people-<year>.parquet          linker input, one row per person per filing
#     classified/classified-<year>.parquet  every person-title row, all columns
#
# Read it back with arrow:  open_dataset("s3://nccs-efile/dev/synthid/partvii-v3_1/people")
#
#   Rscript dev/partvii-panel/03-upload.R

source("dev/partvii-panel/_panel.R")
files <- c(ppl_file(YEARS), cls_file(YEARS))
stopifnot(all(file.exists(files)))

man <- rbindlist(lapply(files, function(f) data.table(
  file = sub(paste0(pv_root, "/"), "", f, fixed = TRUE),
  rows = arrow::ParquetFileReader$create(f)$num_rows,
  bytes = file.size(f), md5 = unname(tools::md5sum(f)))))
fwrite(man, file.path(pv_root, "manifest.csv"))
file.copy(file.path(pv_repo, "README.md"), file.path(pv_root, "README.md"), overwrite = TRUE)
print(man[, .(files = .N, rows = sum(rows), GB = round(sum(bytes) / 1e9, 2)), by = .(dir = dirname(file))])

rc <- system2("aws", c("s3", "sync", shQuote(pv_root), S3_DEST, "--only-show-errors"))
if (rc != 0) stop("aws s3 sync failed with status ", rc)
system2("aws", c("s3", "ls", paste0(S3_DEST, "/"), "--recursive", "--summarize", "--human-readable"))
stamp("uploaded to %s", S3_DEST)
