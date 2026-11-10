# =====================================================================
# Dating precision in the vHMML Reading Room catalogue
#
# Reproduction code for:
#   Tosun, A. U. and Karaman, F. How precisely are manuscripts dated?
#   Time, script tradition, and cataloguing practice in the vHMML Reading
#   Room catalogue. Submitted to Digital Scholarship in the Humanities.
#
# Version 7 (November 2026), written and tested with R 4.5.2.
#
# Data: vHMML Reading Room full dataset, Hill Museum & Manuscript Library,
#   licensed CC BY 4.0 (https://www.vhmml.org/datasets/). The analysis in
#   the paper uses the version downloaded on 8 November 2026.
# Packages: jsonlite, data.table, lme4, ggplot2, and optionally ragg
#   (for the high-resolution PNG files). Missing packages are installed.
#
# How to run
#   1. In step00, set `bases` to the folder that should hold the project
#      folder "paper_hmml" (the first existing folder in the list is used).
#   2. Source the whole file. Each step skips work that is already done:
#      the download, the parsed table, and every fitted model are cached,
#      so a rerun only rebuilds the tables and figures.
#   The first full run (download, parsing, and all model fits) can take
#   several hours on a standard laptop; later runs are much faster.
#
# Steps
#   step00  setup: packages, project folder, csv helper
#   step01  download and unzip the full vHMML dataset (once)
#   step02  parse the JSON records into one row per manuscript part (once)
#   step03  precision classes, placeholder block, heaping diagnostics
#   step04  sample flow, coding conventions, multilevel model, temporal
#           reconstructions, Figures 1-4
#   step05  robustness checks and Figure 5 (curves by script group)
#   step06  observed-scale ICC, script x country model, random-draw
#           century assignment
#   step07  full Laplace fit, printed and papyrus parts excluded,
#           centre-weighted aoristic weights
#
# Outputs (all written to the project folder)
#   hmml_sNN_*.csv  tables; the comment after each out() call names the
#                   table or section of the paper that the file supports
#   hmml_figN_*     Figures 1-5, each as PNG (1600 dpi), TIFF (600 dpi),
#                   PDF, and EPS
#   *.rds           cached model fits (delete a file to refit the models it holds)
# Terminology: the "default block" in the code is the "placeholder block"
#   of the paper (Mali, 1700-1950).
# =====================================================================

## ---- step00: setup ------------------------------------------------
pkgs <- c("jsonlite", "data.table")                         # packages used
for (p in pkgs) {                                           # loop over packages
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)  # install only if missing
}
library(jsonlite)                                           # JSON reading
library(data.table)                                         # fast tables and csv writing

options(timeout = 3600)                                     # allow a long download (48 MB)

# Set this to the folder(s) on your machine; the first existing one is used.
bases <- c("C:/Users/Alper.Tosun/Desktop/cru130",
           "C:/Users/autos/OneDrive/Desktop/cru130")        # candidate base folders
base  <- bases[dir.exists(bases)][1]                        # take the first folder that exists
if (is.na(base)) stop("cru130 folder not found on this machine")  # stop if none exists
proj  <- file.path(base, "paper_hmml")                      # project folder of this paper
if (!dir.exists(proj)) dir.create(proj)                     # create it once (never deleted)
setwd(proj)                                                 # every output goes here

out <- function(x, name) {                                  # helper: write a csv into proj
  fwrite(x, file.path(proj, name), bom = TRUE)              # bom = TRUE -> Excel reads UTF-8 correctly
  message("written: ", name)                                # print the file name
}

## ---- step01: download and unzip (only if the files are missing) ---
zip_url  <- "https://www.vhmml.org/datasets/vhmml_rr_fulldata.zip"  # official CC BY 4.0 dump
zip_file <- file.path(proj, "vhmml_rr_fulldata.zip")        # local zip path

zip_ok <- function(f) {                                     # helper: is the zip complete?
  if (!file.exists(f)) return(FALSE)                        # no file -> not ok
  !is.null(tryCatch(unzip(f, list = TRUE), error = function(e) NULL))  # readable index -> ok
}

if (!zip_ok(zip_file)) {                                    # download only if missing or broken
  okA <- tryCatch({                                         # Option A: base R download.file
    download.file(zip_url, zip_file, mode = "wb")           # binary mode (needed on Windows)
    zip_ok(zip_file)                                        # TRUE if the zip is readable
  }, error = function(e) FALSE)                             # any error -> FALSE
  if (!okA) {                                               # Option B: curl package
    if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")  # install curl if needed
    curl::curl_download(zip_url, zip_file, mode = "wb")     # overwrite the broken file
  }
  if (!zip_ok(zip_file)) stop("Download failed with both Option A and Option B")  # give up clearly
}

zl        <- unzip(zip_file, list = TRUE)                   # list files inside the zip
json_name <- zl$Name[which.max(zl$Length)]                  # the largest file = the data file
json_file <- file.path(proj, basename(json_name))           # where it is unpacked
if (!file.exists(json_file)) {                              # unzip only once
  unzip(zip_file, files = json_name, exdir = proj, junkpaths = TRUE)  # extract into proj
}

## ---- step02: parse to one row per manuscript part (only once) ------
parts_csv <- file.path(proj, "hmml_s02_parts.csv")          # parsed table (large, keep locally)
need_cols <- c("repository", "collection", "country_origin")  # columns that a complete parse must contain

g <- function(x, field) {                                   # EXACT field access (no partial match)
  if (!is.list(x)) return(NULL)                             # not a list -> nothing
  x[[field, exact = TRUE]]                                  # exact = TRUE blocks partial matching
}
nm <- function(x) {                                         # take "name" when x is an object
  if (is.list(x) && !is.null(g(x, "name"))) g(x, "name") else x  # else keep x as it is
}
val1 <- function(x) {                                       # first value as text, else NA
  if (is.null(x) || length(x) == 0) return(NA_character_)   # empty -> NA
  x <- unlist(x, use.names = FALSE)                         # flatten lists
  if (length(x) == 0 || is.na(x[1])) return(NA_character_)  # nothing left -> NA
  as.character(x[1])                                        # first element as text
}
valc <- function(x) {                                       # all values joined with ";"
  if (is.null(x) || length(x) == 0) return(NA_character_)   # empty -> NA
  x <- as.character(unlist(lapply(x, nm), use.names = FALSE))  # names of objects, flattened
  x <- x[!is.na(x) & nzchar(x)]                             # drop blanks
  if (length(x) == 0) NA_character_ else paste(unique(x), collapse = ";")  # join unique values
}

parse_record <- function(r) {                               # one catalogue object -> its parts
  obj <- list(obj_id      = val1(g(r, "id")),               # vHMML id
              object_type = val1(g(r, "objectType")),       # MANUSCRIPT / PRINT / ARCHIVAL ...
              country_now = val1(nm(g(r, "country"))),      # current country of the object
              city_now    = val1(nm(g(r, "city"))),         # current city
              repository  = val1(nm(g(r, "repository"))),   # library / archive holding it
              collection  = val1(nm(g(r, "collection"))),   # HMML collection prefix
              project_no  = val1(g(r, "hmmlProjectNumber")),  # HMML project number
              processed_by = val1(g(r, "processedBy")),     # who catalogued it
              support_obj = val1(g(r, "support")))          # parchment / paper (object level)
  parts <- g(r, "parts")                                    # list of parts
  if (is.null(parts) || length(parts) == 0) parts <- list(list())  # keep objects without parts
  rows <- lapply(seq_along(parts), function(i) {            # loop over parts
    p <- parts[[i]]                                         # one part
    langs <- unlist(lapply(g(p, "contents"), function(ct)   # languages of all items in the part
      lapply(g(ct, "languages"), nm)), use.names = FALSE)
    c(obj, list(
      part_no           = as.character(i),                  # part number within object
      part_type         = val1(g(p, "type")),               # Manuscript / Printed
      centuries         = valc(g(p, "centuries")),          # e.g. "15" or "14;15"
      century_uncertain = val1(g(p, "centuryUncertain")),   # TRUE = provisional century
      century_imported  = val1(g(p, "centuryImported")),    # legacy century text
      begin_date        = val1(g(p, "beginDate")),          # earliest possible year
      end_date          = val1(g(p, "endDate")),            # latest possible year
      precise_year      = val1(g(p, "datePreciseYear")),    # exact year (e.g. from a colophon)
      precise_month     = val1(g(p, "datePreciseMonth")),   # exact month
      ymd_imported      = val1(g(p, "ymdDateImported")),    # legacy exact date text
      native_date       = val1(g(p, "nativeDatePrecise")),  # date in the native calendar
      writing_system    = val1(g(p, "writingSystem")),      # e.g. Arabic, Syriac, Roman
      script            = val1(g(p, "script")),             # script style
      country_origin    = val1(g(p, "countryOfOrigin")),    # where it was written
      support_part      = val1(g(p, "support")),            # parchment / paper (part level)
      languages         = valc(langs)))                     # languages joined with ";"
  })
  rbindlist(rows, fill = TRUE)                              # parts -> table
}

as_records <- function(lst) {                               # make a flat list of records
  unlist(lapply(lst, function(x)                            # a JSON array gives an unnamed list
    if (is.list(x) && is.null(names(x))) x else list(x)), recursive = FALSE)
}

parse_optionA <- function(f) {                                 # Option A: whole file as one JSON array
  recs <- as_records(list(read_json(f, simplifyVector = FALSE)))  # read everything at once
  if (length(recs) == 1 && is.null(g(recs[[1]], "parts"))) {  # wrapped object such as {"docs":[...]}
    big  <- which.max(vapply(recs[[1]], length, 0))         # take its largest element
    recs <- as_records(recs[[1]][big])                      # use that as the record list
  }
  message("Option A: records = ", length(recs))                # progress
  res <- vector("list", ceiling(length(recs) / 5000))       # chunks of 5000 records
  for (k in seq_along(res)) {                               # parse chunk by chunk
    idx <- ((k - 1) * 5000 + 1):min(k * 5000, length(recs)) # record numbers of this chunk
    res[[k]] <- rbindlist(lapply(recs[idx], parse_record), fill = TRUE)  # parse the chunk
    if (k %% 10 == 0) message("  parsed ", max(idx))        # progress every 50,000 records
  }
  rbindlist(res, fill = TRUE)                               # all chunks -> one table
}

parse_optionB <- function(f) {                                 # Option B: line-delimited JSON, in chunks
  con <- file(f, open = "r", encoding = "UTF-8")            # open file as UTF-8 text
  on.exit(close(con))                                       # close it at the end
  res <- list(); k <- 0; n_rec <- 0                         # containers and counters
  repeat {                                                  # read chunk by chunk
    lines <- readLines(con, n = 5000, warn = FALSE)         # 5000 lines at a time
    if (length(lines) == 0) break                           # end of file
    recs  <- lapply(lines[nzchar(trimws(lines))], function(l)  # parse each non-empty line
      tryCatch(parse_json(l), error = function(e) NULL))    # bad line -> NULL
    recs  <- as_records(recs[!vapply(recs, is.null, NA)])   # remove NULLs, flatten arrays
    if (length(recs) > 0) {
      k <- k + 1                                            # chunk counter
      res[[k]] <- rbindlist(lapply(recs, parse_record), fill = TRUE)  # parse records of chunk
      n_rec <- n_rec + length(recs)                         # count records
      message("Option B: records parsed = ", n_rec)            # progress
    }
  }
  rbindlist(res, fill = TRUE)                               # all chunks -> one table
}

has_dates <- function(d) {                                  # did parsing find any date field?
  !is.null(d) && nrow(d) > 0 &&
    any(!is.na(d$centuries) | !is.na(d$begin_date) | !is.na(d$precise_year))
}

old_ok <- FALSE                                             # is there a complete parse already?
if (file.exists(parts_csv)) {                               # a parsed file exists
  hdr    <- names(fread(parts_csv, nrows = 0))              # read only its header
  old_ok <- all(need_cols %in% hdr)                         # all required columns present?
}
if (old_ok) {                                               # complete file -> just read it
  d <- fread(parts_csv, colClasses = "character", encoding = "UTF-8")
  message("read existing ", basename(parts_csv))
} else {                                                    # incomplete file or none -> parse again
  d <- tryCatch(parse_optionA(json_file),                      # Option A first
                error = function(e) { message("Option A error: ", conditionMessage(e)); NULL })
  if (!has_dates(d)) {                                      # failed or empty -> Option B
    message("Switching to Option B")
    d <- parse_optionB(json_file)
  }
  if (!has_dates(d)) stop("No date fields found")           # give up clearly
  fwrite(d, parts_csv, bom = TRUE)                          # save parsed table (large, kept locally)
  d <- fread(parts_csv, colClasses = "character", encoding = "UTF-8")  # re-read: all columns as text
}
message("parts: ", nrow(d), " | objects: ", uniqueN(d$obj_id))  # size of the table
invisible(gc())                                             # free memory after parsing

## ---- step03: dating-precision diagnostics -------------------------
num <- function(x) suppressWarnings(as.numeric(x))          # text -> number (NA if not numeric)
d[, begin := num(begin_date)]                               # earliest year
d[, end   := num(end_date)]                                 # latest year
d[, pyear := num(precise_year)]                             # exact year
d[begin == 0 & end == 0, c("begin", "end") := .(NA_real_, NA_real_)]  # 0/0 is a placeholder
d[!is.na(begin) & !is.na(end) & end < begin,
  c("begin", "end") := .(end, begin)]                       # fix reversed intervals
d[, width := end - begin]                                   # interval width in years

cent <- lapply(strsplit(d$centuries, ";"), num)             # centuries as numbers
d[, cent_min := vapply(cent, function(z) if (all(is.na(z))) NA_real_ else min(z, na.rm = TRUE), 0)]
d[, cent_max := vapply(cent, function(z) if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE), 0)]
rm(cent)                                                    # free memory

d[, precision := fcase(                                     # dating precision class
  !is.na(pyear),                         "1_exact_year",     # exact year given
  !is.na(width) & width == 0,            "1_exact_year",     # begin = end
  !is.na(width) & width <= 10,           "2_span_1_10",      # within a decade
  !is.na(width) & width <= 25,           "3_span_11_25",     # quarter century
  !is.na(width) & width <= 50,           "4_span_26_50",     # half century
  !is.na(width) & width <= 98,           "5_span_51_98",     # less than a century
  !is.na(width) & width <= 101,          "6_span_one_century", # one century (99-101)
  !is.na(width),                         "7_span_multi_century", # more than a century
  !is.na(cent_min),                      "8_century_field_only", # only the century field
  default =                              "9_undated")]       # no date information

d[, mid := fcase(                                           # best single year for grouping
  !is.na(pyear),                 pyear,                     # exact year
  !is.na(width),                 (begin + end) / 2,         # interval midpoint
  !is.na(cent_min),              ((cent_min - 1) * 100 + cent_max * 100) / 2,  # century midpoint
  default = NA_real_)]
d[, mid_century := floor((mid - 1) / 100) + 1]              # century of the midpoint
d[, exact := as.integer(precision == "1_exact_year")]       # 1 = exact year

# 3a. placeholder ("default") block: one wide interval used for most of a country's parts (Section 3.3)
blk <- d[!is.na(width) & width >= 100, .N, by = .(country_now, begin, end)]  # wide intervals per country
blk[, n_country := d[!is.na(width)][, .N, by = country_now][blk, on = "country_now", N]]  # dated parts in country
blk[, share_in_country := round(N / n_country, 4)]          # share of that interval in the country
blk[, default_block := N >= 500 & share_in_country >= 0.5 & (end - begin) >= 200]  # rule for a default block
setorder(blk, -N)                                           # biggest first
out(blk[N >= 500], "hmml_s03_default_blocks.csv")           # Section 4.1 (all large intervals, flagged)
d[blk[default_block == TRUE], on = .(country_now, begin, end), default_block := TRUE]  # mark parts
d[is.na(default_block), default_block := FALSE]             # everything else = genuine date
message("parts in default blocks: ", d[default_block == TRUE, .N])  # how many removed

# 3b. overall precision shares: full data vs. data without default blocks
t_all <- rbind(
  d[, .N, by = precision][, sample := "all"],               # full data
  d[default_block == FALSE, .N, by = precision][, sample := "no_default_blocks"])  # cleaned data
t_all[, share := round(N / sum(N), 4), by = sample]         # shares within sample
t_all[, share_dated := fifelse(precision == "9_undated", NA_real_,
          round(N / sum(N[precision != "9_undated"]), 4)), by = sample]  # shares among dated
setorder(t_all, sample, precision)                          # tidy order
out(t_all, "hmml_s03_precision_overall.csv")                # Table 1, Section 4.1

c0 <- d[default_block == FALSE]                             # main analysis sample from here on

# 3c. precision shares by group (century, writing system, origin, support, language)
top_k <- function(x, k = 15) {                              # keep the k most frequent, rest = Other
  x[is.na(x) | x == ""] <- "(missing)"                      # label missing
  keep <- names(sort(table(x), decreasing = TRUE))[seq_len(min(k, uniqueN(x)))]
  fifelse(x %in% keep, x, "Other")                          # collapse the rest
}
c0[, g_century := fifelse(!is.na(mid_century) & mid_century >= 1 & mid_century <= 21,
                          sprintf("C%02d", as.integer(mid_century)), "(outside 1-21 / missing)")]
c0[, g_writing := top_k(writing_system)]                    # top writing systems
c0[, g_origin  := top_k(country_origin)]                    # top countries of origin
c0[, g_support := top_k(toupper(fcoalesce(support_part, support_obj)), 6)]  # PAPER = Paper
c0[, g_lang    := top_k(sub(";.*", "", languages))]         # first language of the part
c0[, g_country := top_k(country_now, 20)]                   # current country (holding)

dated <- c0[precision != "9_undated"]                       # dated parts only
grp <- rbindlist(lapply(c("g_century", "g_writing", "g_origin", "g_support", "g_lang", "g_country"),
  function(gv) {
    x <- dated[, .N, by = c(gv, "precision")]               # counts per group x class
    setnames(x, gv, "group")                                # common column name
    x[, n_group := sum(N), by = group]                      # group total
    x[, share := round(N / n_group, 4)]                     # share within group
    x[, group_type := sub("g_", "", gv)]                    # which grouping
    x
  }))
setcolorder(grp, c("group_type", "group", "n_group", "precision", "N", "share"))
setorder(grp, group_type, group, precision)                 # tidy order
out(grp, "hmml_s03_precision_by_group.csv")                 # Figure 1 (shares by century)

# 3d. precision by repository (cataloguing practice of each library)
rp <- dated[, .(country    = names(which.max(table(country_now))),  # country of the repository
                n_dated    = .N,                            # dated parts
                exact      = round(mean(precision == "1_exact_year"), 4),        # share exact
                le50       = round(mean(precision %in% c("2_span_1_10", "3_span_11_25",
                                                         "4_span_26_50")), 4),   # share <= 50 years
                one_cent   = round(mean(precision == "6_span_one_century"), 4),  # share one century
                multi_cent = round(mean(precision == "7_span_multi_century"), 4), # share > 1 century
                conv_x00_x00 = round(mean((begin %% 100 == 0) & (end %% 100 == 0) &
                                          width == 100, na.rm = TRUE), 4),  # 1400-1500 style
                conv_x00_x99 = round(mean((begin %% 100 == 0) & (end %% 100 == 99) &
                                          width == 99, na.rm = TRUE), 4),   # 1400-1499 style
                conv_x01_x00 = round(mean((begin %% 100 == 1) & (end %% 100 == 0) &
                                          width == 99, na.rm = TRUE), 4)),  # 1401-1500 style
            by = repository][n_dated >= 100][order(-n_dated)]   # repositories with >= 100 dated parts
out(rp, "hmml_s03_precision_by_repo.csv")                   # descriptive, by repository

# 3e. distribution of interval widths (without default blocks)
wd <- c0[!is.na(width) & width > 0, .N, by = width][order(-N)]  # frequency of each width
wd[, share := round(N / sum(N), 4)]                         # share among intervals
out(head(wd, 60), "hmml_s03_width_dist.csv")                # Section 4.2 (top 60 widths)

# 3f. boundary heaping of begin/end years (without default blocks)
iv <- c0[!is.na(width) & width > 0]                         # proper intervals only
heap_row <- function(v, base, hits, endpoint) {             # observed vs uniform expectation
  v <- v[!is.na(v)]                                         # drop NA
  obs <- mean((v %% base) %in% hits)                        # observed share on the heap
  exp <- length(hits) / base                                # expected share if no heaping
  data.table(endpoint, base, residues = paste(hits, collapse = "/"),
             n = length(v), observed = round(obs, 4), expected = round(exp, 4),
             ratio = round(obs / exp, 2))                   # ratio > 1 = heaping
}
bh <- rbind(
  heap_row(iv$begin, 100, c(0, 1),  "begin"),               # 1400 or 1401
  heap_row(iv$begin,  50, c(0, 1),  "begin"),               # 1450 or 1451
  heap_row(iv$begin,  25, c(0, 1),  "begin"),               # quarter centuries
  heap_row(iv$begin,  10, c(0, 1),  "begin"),               # decades
  heap_row(iv$end,   100, c(0, 99), "end"),                 # 1500 or 1499
  heap_row(iv$end,    50, c(0, 49), "end"),                 # 1450 or 1449
  heap_row(iv$end,    25, c(0, 24), "end"),                 # quarter centuries
  heap_row(iv$end,    10, c(0, 9),  "end"))                 # decades
out(bh, "hmml_s03_boundary_heaping.csv")                    # Supplementary Table S3

# 3g. last digit of exact years: Gregorian year and native-calendar year
lastdig <- function(y, label) {                             # last-digit table for a year vector
  y <- y[!is.na(y) & y > 0]                                 # valid years only
  x <- data.table(calendar = label, last_digit = 0:9,
                  n = as.integer(table(factor(y %% 10, levels = 0:9))))  # counts per digit
  x[, share := round(n / sum(n), 4)]                        # share (expected 0.10)
  x[, whipple_0_5 := round(100 * sum(share[last_digit %in% c(0, 5)]) / 0.2, 1)]  # 100 = no heaping
  x[, n_years := length(y)]                                 # number of years used
  x
}
c0[, native_year := num(sub(".*?(\\d{3,4})\\D*$", "\\1", native_date))]  # last 3-4 digit number in the native date
c0[, native_cal := fcase(grepl("AH", native_date), "AH (Hijri)",          # Islamic calendar
                         grepl("mundi", native_date, ignore.case = TRUE), "Anno mundi",  # Byzantine era
                         !is.na(native_date) & native_date != "", "other native",
                         default = NA_character_)]
ld <- rbind(
  lastdig(c0$pyear, "Gregorian exact year (all)"),          # all exact years
  lastdig(c0[is.na(native_cal), pyear], "Gregorian exact year, no native date"),  # Western-dated
  lastdig(c0[native_cal == "AH (Hijri)", native_year], "AH year as written"),     # Hijri years
  lastdig(c0[native_cal == "Anno mundi", native_year], "Anno mundi year as written"),
  lastdig(c0[native_cal == "other native", native_year], "other native year as written"))
out(ld, "hmml_s03_lastdigit_exact.csv")                     # Table 2

if (!file.exists(file.path(proj, "hmml_s03_glm_exact_lrtest.csv"))) {  # 3h only if not done yet
  # 3h. is exact dating related to time, tradition and repository? (logistic models)
  m <- dated[g_century != "(outside 1-21 / missing)"]         # dated parts with a usable century
  m <- m[, if (.N >= 30) .SD, by = g_century]                 # drop centuries with < 30 parts
  m[, cent_f := relevel(factor(g_century), ref = "C15")]      # century, reference = 15th century
  m[, writ_f := relevel(factor(top_k(writing_system, 8)),     # 8 writing systems + Other
                        ref = names(which.max(table(top_k(writing_system, 8)))))]
  m[, supp_f := factor(g_support)]                            # support material
  m[, repo_f := factor(top_k(repository, 60))]                # 60 largest repositories + Other

  fitA <- tryCatch(                                           # Option A: with repository fixed effects
    glm(exact ~ cent_f + writ_f + supp_f + repo_f, family = binomial, data = m),
    error = function(e) { message("Option A glm error: ", conditionMessage(e)); NULL })
  fitB <- glm(exact ~ cent_f + writ_f + supp_f, family = binomial, data = m)  # Option B: no repository effects

  co <- rbindlist(lapply(list(A_with_repo = fitA, B_no_repo = fitB), function(f) {
    if (is.null(f)) return(NULL)                              # skip a failed model
    x <- as.data.table(summary(f)$coefficients, keep.rownames = "term")  # coefficient table
    setnames(x, c("term", "estimate", "se", "z", "p"))        # short names
    x[, odds_ratio := round(exp(estimate), 3)]                # odds ratios
    x[, n_used := nobs(f)]                                    # sample size
    x
  }), idcol = "model")
  out(co[!grepl("^repo_f", term)], "hmml_s03_glm_exact_coef.csv")  # Section 4.4, fixed-effects check (repository dummies left out)

  lr <- rbindlist(lapply(list(A_with_repo = fitA, B_no_repo = fitB), function(f) {
    if (is.null(f)) return(NULL)                              # skip a failed model
    as.data.table(drop1(f, test = "Chisq"), keep.rownames = "term")  # LR test per block
  }), idcol = "model", fill = TRUE)
  out(lr, "hmml_s03_glm_exact_lrtest.csv")                    # Section 4.4, likelihood-ratio tests
}

## ---- step04: models, tables, and Figures 1-4 --------------------
for (p in c("lme4", "ggplot2")) {                           # extra packages for step04
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)  # install only if missing
}
if (!requireNamespace("ragg", quietly = TRUE))              # optional: memory-efficient PNG device
  tryCatch(install.packages("ragg"), error = function(e) message("ragg not installed; base png() is used"))
library(lme4)                                               # multilevel logistic model
library(ggplot2)                                            # figures

# 4a. sample flow (how many parts are removed at each stage)
flow <- data.table(
  stage = c("all parts in the catalogue", "in the default block (Mali, 1700-1950)",
            "undated (no year, interval or century)", "dated parts analysed"),
  n = c(nrow(d), d[default_block == TRUE, .N],
        c0[precision == "9_undated", .N], nrow(dated)))     # counts per stage
flow[, share_of_all := round(n / nrow(d), 4)]               # share of the whole catalogue
out(flow, "hmml_s04_sample_flow.csv")                       # Section 4.1

# 4b. century-coding conventions of one-century intervals, per repository
oc <- dated[precision == "6_span_one_century" & !is.na(repository)]  # one-century intervals
oc[, style := fcase(begin %% 100 == 0 & end %% 100 == 0,  "x00-x00 (e.g. 1400-1500)",
                    begin %% 100 == 0 & end %% 100 == 99, "x00-x99 (e.g. 1400-1499)",
                    begin %% 100 == 1 & end %% 100 == 0,  "x01-x00 (e.g. 1401-1500)",
                    default = "other")]                     # coding style of each interval
rs <- oc[, .N, by = .(repository, style)]                   # styles per repository
rs[, `:=`(share = N / sum(N), tot = sum(N)), by = repository]  # share within repository
dom <- rs[order(-share), .SD[1], by = repository]           # dominant style of each repository
dom[, mixed := share < 0.9]                                 # repository mixes styles (< 90% one style)
conv <- dom[, .(repositories = .N, mixed_repositories = sum(mixed),
                one_century_intervals = sum(tot)),
            by = .(dominant_style = style)]                 # summary by dominant style
out(conv, "hmml_s04_conventions.csv")                       # Supplementary Table S4

# 4c. multilevel logistic model: exact dating ~ century + writing system + support + (1 | repository)
mm <- dated[g_century != "(outside 1-21 / missing)"]        # dated parts with a usable century
mm <- mm[, if (.N >= 30) .SD, by = g_century]               # drop centuries with < 30 parts
mm[is.na(repository) | repository == "", repository := "(missing)"]  # label missing repository
mm[, cent_f := relevel(factor(g_century), ref = "C15")]     # reference: 15th century
mm[, writ_f := relevel(factor(top_k(writing_system, 8)), ref = "Roman")]  # reference: Roman script
mm[, supp_f := relevel(factor(g_support), ref = "PAPER")]   # reference: paper
mm[, repo_f := factor(repository)]                          # all repositories

mfile <- file.path(proj, "hmml_s04_glmer_fit.rds")          # cached model (skip refitting)
if (file.exists(mfile)) {
  fit <- readRDS(mfile); message("read cached multilevel model")
} else {
  fit <- tryCatch(                                          # Option A: glmer, fast Laplace (nAGQ = 0)
    glmer(exact ~ cent_f + writ_f + supp_f + (1 | repo_f), data = mm, family = binomial,
          nAGQ = 0, control = glmerControl(optimizer = "nloptwrap")),
    error = function(e) { message("Option A glmer error: ", conditionMessage(e)); NULL })
  if (is.null(fit)) {                                       # Option B: bobyqa optimiser, nAGQ = 1
    fit <- glmer(exact ~ cent_f + writ_f + supp_f + (1 | repo_f), data = mm, family = binomial,
                 control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
  }
  saveRDS(fit, mfile)                                       # save the fitted model
}

fe <- as.data.table(summary(fit)$coefficients, keep.rownames = "term")  # fixed effects
setnames(fe, c("term", "estimate", "se", "z", "p"))         # short names
fe[, `:=`(odds_ratio = round(exp(estimate), 3),
          or_low  = round(exp(estimate - 1.96 * se), 3),    # 95% interval
          or_high = round(exp(estimate + 1.96 * se), 3))]
vr  <- as.numeric(VarCorr(fit)$repo_f)                      # between-repository variance
icc <- vr / (vr + pi^2 / 3)                                 # latent-scale intra-class correlation
fe[, `:=`(repo_var = round(vr, 3), icc_latent = round(icc, 3),
          n_parts = nobs(fit), n_repositories = nlevels(mm$repo_f))]
out(fe, "hmml_s04_glmer_fixed.csv")                         # Table 3, Supplementary Table S5

# predicted probability of an exact date by century (typical repository, Roman script, paper)
cl  <- levels(mm$cent_f)                                    # centuries in the model
nd  <- data.table(cent_f = factor(cl, levels = cl),
                  writ_f = factor("Roman", levels = levels(mm$writ_f)),
                  supp_f = factor("PAPER", levels = levels(mm$supp_f)))  # reference profile
X   <- model.matrix(~ cent_f + writ_f + supp_f, nd)         # design rows
eta <- drop(X %*% fixef(fit))                               # linear predictor
se  <- sqrt(rowSums((X %*% as.matrix(vcov(fit))) * X))      # delta-method standard error
pc  <- data.table(century = cl, p = plogis(eta),
                  low = plogis(eta - 1.96 * se), high = plogis(eta + 1.96 * se))
pc[, century_num := as.integer(sub("C", "", century))]      # number for plotting
out(pc, "hmml_s04_pred_by_century.csv")                     # Figure 2

# repository effects (random intercepts with 95% intervals)
re  <- as.data.table(as.data.frame(ranef(fit, condVar = TRUE)))  # random effects with conditional sd
rp2 <- re[, .(repository = as.character(grp), effect = condval, se = condsd)]
rp2 <- merge(rp2, mm[, .(n = .N, country = names(which.max(table(country_now))),
                         script = names(which.max(table(top_k(writing_system, 8))))),
                     by = .(repository = as.character(repo_f))], by = "repository")
setorder(rp2, effect)                                       # lowest to highest
out(rp2, "hmml_s04_repo_effects.csv")                       # all repository effects

# 4d. temporal reconstructions: exact years only vs standard vs phase-corrected aoristic sum
yrs <- 500:2025                                             # computation window (wider than the plot, no edge pile-up)
iv2 <- copy(dated)
iv2[, `:=`(bb = fifelse(!is.na(pyear), pyear, begin),       # exact year counts as a 1-year interval
           ee = fifelse(!is.na(pyear), pyear, end))]
iv2 <- iv2[!is.na(bb) & !is.na(ee) & ee >= 500 & bb <= 2025]  # parts with a usable interval
iv2[, `:=`(b = pmax(bb, 500), e = pmin(ee, 2025))]          # clip to the computation window
ex2 <- dated[!is.na(pyear) & pyear >= 500 & pyear <= 2025]  # exact years

std_curve <- numeric(length(yrs))                           # standard aoristic sum
u <- iv2[, .N, by = .(b, e)]                                # unique intervals with counts
for (k in seq_len(nrow(u))) {
  idx <- (u$b[k]:u$e[k]) - 499                              # positions in yrs
  std_curve[idx] <- std_curve[idx] + u$N[k] / length(idx)   # spread evenly
}
hz <- copy(u)                                               # phase-corrected (Hinz and Mueller-Scheessel)
hz[, `:=`(hb = fifelse(b %% 50 == 1, b - 1, b),             # merge 1401-style starts with 1400
          he = fifelse(e %% 50 == 49, e + 1, e))]           # merge 1499-style ends with 1500
lam <- numeric(length(yrs))                                 # lambda(t): distinct labels covering t
uq  <- unique(hz[, .(hb = pmax(hb, 500), he = pmin(he, 2025))])   # distinct labels after merging
for (k in seq_len(nrow(uq))) {
  idx <- (uq$hb[k]:uq$he[k]) - 499                          # years covered by the label
  lam[idx] <- lam[idx] + 1                                  # count it
}
hz_curve <- numeric(length(yrs))
for (k in seq_len(nrow(u))) {
  idx <- (u$b[k]:u$e[k]) - 499                              # years of the interval
  w   <- 1 / pmax(lam[idx], 1)                              # inverse overlap weights
  hz_curve[idx] <- hz_curve[idx] + u$N[k] * w / sum(w)      # spread with 1/lambda
}
ex_curve <- as.numeric(table(factor(ex2$pyear, levels = yrs)))  # exact years only

dec <- (yrs %/% 10) * 10                                    # decade of each year
curves <- rbind(
  data.table(decade = dec, method = "Exact years only", v = ex_curve),
  data.table(decade = dec, method = "Standard aoristic (all dated)", v = std_curve),
  data.table(decade = dec, method = "Phase-corrected aoristic (all dated)", v = hz_curve))
curves <- curves[decade >= 1000 & decade <= 1990, .(v = sum(v)), by = .(decade, method)]  # decades 1000s-1990s
curves[, share := v / sum(v), by = method]                  # share of each curve
out(curves, "hmml_s04_curves.csv")                          # Figure 4, Table 4

# 4e. figures; each one is written in four formats and a rerun overwrites the same files:
#     PNG 1600 dpi (repository and screen), TIFF 600 dpi with LZW compression,
#     and vector PDF and EPS (journal submission). No captions are drawn in the files.
save_fig <- function(g, stem, width, height) {              # one figure -> png, tiff, pdf, eps
  f <- function(ext) file.path(proj, paste0(stem, ".", ext))  # full file name
  try_dev <- function(label, a, b) {                        # Option A, then Option B
    ok <- tryCatch({ a(); TRUE }, error = function(e) {
      message(label, " Option A failed: ", conditionMessage(e)); FALSE })
    if (!ok) tryCatch(b(), error = function(e) message(label, " Option B failed: ", conditionMessage(e)))
  }
  try_dev("png",                                            # A: ragg (low memory), B: base png
    function() ggsave(f("png"), g, device = ragg::agg_png, width = width, height = height, dpi = 1600),
    function() ggsave(f("png"), g, device = "png", width = width, height = height, dpi = 1600))
  try_dev("tiff",                                           # A: ragg, B: base tiff
    function() ggsave(f("tiff"), g, device = ragg::agg_tiff, width = width, height = height, dpi = 600,
                      compression = "lzw"),
    function() ggsave(f("tiff"), g, device = "tiff", width = width, height = height, dpi = 600,
                      compression = "lzw"))
  try_dev("pdf",                                            # A: cairo (full Unicode), B: base pdf (may not draw the less-or-equal sign)
    function() ggsave(f("pdf"), g, device = cairo_pdf, width = width, height = height),
    function() ggsave(f("pdf"), g, device = "pdf", width = width, height = height, useDingbats = FALSE))
  try_dev("eps",                                            # A: cairo (full Unicode), B: base postscript (same limitation)
    function() ggsave(f("eps"), g, device = cairo_ps, width = width, height = height),
    function() ggsave(f("eps"), g, device = "eps", width = width, height = height))
  message("figure written: ", stem, " (png, tiff, pdf, eps)")
}

th <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(),
                                            legend.position = "bottom")
cent_lab <- function(x) paste0(x, ifelse(x %% 100 %in% 11:13, "th",
  c("th", "st", "nd", "rd", rep("th", 6))[x %% 10 + 1]))      # 15 -> "15th", 21 -> "21st"

# Figure 1: composition of dating precision by century (four merged classes)
f1 <- dated[g_century %in% sprintf("C%02d", 9:20)]          # 9th-20th centuries
f1[, cls := fcase(precision == "1_exact_year", "Exact year",
                  precision %in% c("2_span_1_10", "3_span_11_25", "4_span_26_50"), "Interval \u2264 50 years",
                  precision %in% c("5_span_51_98", "6_span_one_century"), "Interval 51\u2013101 years",
                  default = "Interval > 101 years or century only")]
f1 <- f1[, .N, by = .(g_century, cls)][, share := N / sum(N), by = g_century]
f1[, century := as.integer(sub("C", "", g_century))]
f1[, cls := factor(cls, levels = c("Exact year", "Interval \u2264 50 years",       # \u2264 = less-or-equal sign
                                   "Interval 51\u2013101 years", "Interval > 101 years or century only"))]  # \u2013 = en dash
g1 <- ggplot(f1, aes(century, share, fill = cls)) +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = 9:20, labels = cent_lab) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  scale_fill_grey(start = 0.15, end = 0.85, name = NULL) +
  labs(x = "Century (midpoint of the dating)", y = "Share of dated parts") + th
save_fig(g1, "hmml_fig1_precision_by_century", width = 7, height = 4)

# Figure 2: conditional predicted probability of an exact year
pc2 <- pc[century_num >= 9 & century_num <= 20]           # 9th-20th (8th and 21st: too few parts, kept in the csv)
g2 <- ggplot(pc2, aes(century_num, p)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "grey80") +
  geom_line() + geom_point(size = 1.5) +
  scale_x_continuous(breaks = pc2$century_num, labels = cent_lab) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  labs(x = "Century", y = "Probability of an exact year\n(typical repository, Roman script, paper)") + th
save_fig(g2, "hmml_fig2_pred_exact_by_century", width = 7, height = 4)

# Figure 3: repository effects (random intercepts) with 95% intervals
f3 <- rp2[n >= 200]                                         # repositories with >= 200 dated parts
setorder(f3, effect)                                        # lowest to highest
f3[, rank := .I]                                            # position on the y axis (names in the supplement)
f3[, script := fifelse(script == "(missing)", "Not recorded", script)]  # clearer legend label
g3 <- ggplot(f3, aes(effect, rank, colour = script)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_segment(aes(x = effect - 1.96 * se, xend = effect + 1.96 * se, yend = rank), alpha = 0.7) +
  geom_point(size = 1.3) +
  scale_colour_brewer(palette = "Dark2", name = "Main script") +
  scale_y_continuous(breaks = NULL) +
  labs(x = "Repository effect on the log-odds of an exact year (95% interval)",
       y = paste0("Repositories with at least 200 dated parts (n = ", nrow(f3), "), ranked")) + th
save_fig(g3, "hmml_fig3_repository_effects", width = 7, height = 5)
out(f3[, .(rank, repository, country, script, n, effect = round(effect, 3), se = round(se, 3))],
    "hmml_s04_repo_effects_fig3.csv")                        # Supplementary Table S7 (names behind Figure 3)

# Figure 4: three reconstructions; legend in the order used in the text
f4 <- copy(curves)[, method := factor(method, levels = c("Exact years only",
                     "Standard aoristic (all dated)", "Phase-corrected aoristic (all dated)"))]
g4 <- ggplot(f4, aes(decade, share, linetype = method)) +
  geom_vline(xintercept = seq(1100, 1900, 100), colour = "grey85", linewidth = 0.3) +  # century starts
  geom_line() +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100)) +
  labs(x = "Decade", y = "Share of the reconstructed total", linetype = NULL) + th
save_fig(g4, "hmml_fig4_curves", width = 7, height = 4)


## ---- step05: robustness checks and Figure 5 ------------------------
# 5a placeholder-rule sensitivity          5f one part per object
# 5b alternative century assignment        5g population-averaged probabilities
# 5c "not recorded" writing system         5h curves by script group (the 1500 drop)
# 5d script x holding country              5i overlap count lambda(t) by century
# 5e within-repository century effect (repository mean century added)
# All multilevel fits are cached in hmml_s05_models.rds (only missing fits are run).

cent_of <- function(y) floor((y - 1) / 100) + 1             # year -> century (1500 -> 15th)

# common preparation for model variants (works for any set of dated parts)
prep_m <- function(x, cent_var) {
  x <- copy(x)
  x[, cnum := get(cent_var)]                                # century number used in this variant
  x <- x[!is.na(cnum) & cnum >= 1 & cnum <= 21]             # usable centuries
  x[, cf := sprintf("C%02d", as.integer(cnum))]             # factor label
  x <- x[, if (.N >= 30) .SD, by = cf]                      # centuries with >= 30 parts
  x[is.na(repository) | repository == "", repository := "(missing)"]
  x[, cent_f := relevel(factor(cf), ref = "C15")]           # reference: 15th century
  x[, writ_f := relevel(factor(top_k(writing_system, 8)), ref = "Roman")]  # reference: Roman
  x[, supp_f := relevel(factor(top_k(toupper(fcoalesce(support_part, support_obj)), 6)), ref = "PAPER")]
  x[, repo_f := factor(repository)]
  x
}
fit_glmm <- function(fml, data) {                           # Option A nAGQ = 0, Option B bobyqa
  f <- tryCatch(glmer(fml, data = data, family = binomial, nAGQ = 0,
                      control = glmerControl(optimizer = "nloptwrap")),
                error = function(e) { message("Option A error: ", conditionMessage(e)); NULL })
  if (is.null(f)) f <- glmer(fml, data = data, family = binomial,
                             control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
  f
}

# century variables for the dated parts (placeholder block excluded = 'dated')
add_cent <- function(x) {
  x[, c_mid   := mid_century]                               # main: midpoint
  x[, c_begin := fcase(!is.na(pyear), cent_of(pyear), !is.na(begin), floor(begin / 100) + 1,
                       rep(TRUE, .N), cent_min)]            # earliest-year century (1400 -> 15th)
  x[, c_end   := fcase(!is.na(pyear), cent_of(pyear), !is.na(end), cent_of(end),
                       rep(TRUE, .N), cent_max)]            # latest-year century (1500 -> 15th)
  x
}
dated <- add_cent(dated)

# dated parts INCLUDING the placeholder block (for sensitivity B)
dall <- d[precision != "9_undated"]
dall <- add_cent(dall)

mods_file <- file.path(proj, "hmml_s05_models.rds")         # cache of fitted variants
mods <- if (file.exists(mods_file)) readRDS(mods_file) else list()
base_f <- exact ~ cent_f + writ_f + supp_f + (1 | repo_f)   # main formula

variants <- list(                                           # name -> data + formula
  M1_block_included    = list(dat = function() prep_m(dall, "c_mid"), f = base_f),
  M2_century_by_begin  = list(dat = function() prep_m(dated, "c_begin"), f = base_f),
  M3_century_by_end    = list(dat = function() prep_m(dated, "c_end"), f = base_f),
  M4_max_one_century   = list(dat = function() prep_m(dated[precision != "7_span_multi_century" &
                                  !(precision == "8_century_field_only" & cent_min != cent_max)], "c_mid"), f = base_f),
  M5_no_missing_script = list(dat = function() prep_m(dated[!is.na(writing_system) & writing_system != ""], "c_mid"), f = base_f),
  M6_country_fe        = list(dat = function() { x <- prep_m(dated, "c_mid"); x[, ctry_f := factor(top_k(country_now, 15))]; x },
                              f = exact ~ cent_f + writ_f + supp_f + ctry_f + (1 | repo_f)),
  M7_repo_mean_century = list(dat = function() { x <- prep_m(dated, "c_mid"); x[, repo_mean_c := mean(cnum), by = repo_f]; x },
                              f = exact ~ cent_f + writ_f + supp_f + repo_mean_c + (1 | repo_f)),
  M8_one_part_per_obj  = list(dat = function() prep_m(unique(dated[order(obj_id, as.integer(part_no))], by = "obj_id"), "c_mid"), f = base_f))

for (v in setdiff(names(variants), names(mods))) {          # fit only what is missing
  message("step05 fitting ", v)
  dv <- variants[[v]]$dat()
  mods[[v]] <- fit_glmm(variants[[v]]$f, dv)
  saveRDS(mods, mods_file)                                  # save after every fit
}
mods_all <- c(list(M0_main = fit), mods)                    # main model from step04

or_tab <- rbindlist(lapply(names(mods_all), function(nm) {  # odds ratios of every variant
  f  <- mods_all[[nm]]
  x  <- as.data.table(summary(f)$coefficients, keep.rownames = "term")
  setnames(x, c("term", "est", "se", "z", "p"))
  vr <- as.numeric(VarCorr(f)$repo_f)
  x[, .(model = nm, term, odds_ratio = round(exp(est), 3),
        or_low = round(exp(est - 1.96 * se), 3), or_high = round(exp(est + 1.96 * se), 3),
        repo_var = round(vr, 3), icc_latent = round(vr / (vr + pi^2 / 3), 3),
        n_parts = nobs(f), n_repositories = nlevels(f@frame$repo_f))]
}))
out(or_tab, "hmml_s05_sensitivity_models.csv")              # Supplementary Table S1

# 5a. placeholder-rule grid: which blocks are flagged under other thresholds?
grid_rules <- CJ(level = c("country", "repository"), min_width = c(100, 200, 300),
                 min_share = c(0.3, 0.5, 0.7), min_n = c(200, 500, 1000))
wide <- d[!is.na(width) & width >= 100]                     # wide intervals
blk_c <- wide[, .N, by = .(unit = country_now, begin, end)][
  , n_unit := d[!is.na(width)][, .N, by = .(unit = country_now)][.SD, on = "unit", N]]
blk_r <- wide[, .N, by = .(unit = repository, begin, end)][
  , n_unit := d[!is.na(width)][, .N, by = .(unit = repository)][.SD, on = "unit", N]]
rule_res <- rbindlist(lapply(seq_len(nrow(grid_rules)), function(k) {
  g0 <- grid_rules[k]; b <- if (g0$level == "country") blk_c else blk_r
  hit <- b[(end - begin) >= g0$min_width & N >= g0$min_n & N / n_unit >= g0$min_share]
  data.table(g0, blocks = nrow(hit), parts_flagged = sum(hit$N),
             units = paste(unique(hit$unit), collapse = "; "),
             intervals = paste(unique(paste0(hit$begin, "-", hit$end)), collapse = "; "))
}))
out(rule_res, "hmml_s05_placeholder_rules.csv")             # Section 3.3 (18 threshold combinations)

# precision shares and curves WITH the placeholder block (sensitivity B)
make_curves <- function(x) {                                # exact-only, standard, phase-corrected
  yy <- 500:2025
  x <- copy(x)
  x[, `:=`(bb = fifelse(!is.na(pyear), pyear, begin), ee = fifelse(!is.na(pyear), pyear, end))]
  x <- x[!is.na(bb) & !is.na(ee) & ee >= 500 & bb <= 2025]
  x[, `:=`(b = pmax(bb, 500), e = pmin(ee, 2025))]
  uu <- x[, .N, by = .(b, e)]
  st <- numeric(length(yy))
  for (k in seq_len(nrow(uu))) { i <- (uu$b[k]:uu$e[k]) - 499; st[i] <- st[i] + uu$N[k] / length(i) }
  hq <- unique(uu[, .(hb = fifelse(b %% 50 == 1, b - 1, b), he = fifelse(e %% 50 == 49, e + 1, e))])
  lm_ <- numeric(length(yy))
  for (k in seq_len(nrow(hq))) { i <- (max(hq$hb[k], 500):min(hq$he[k], 2025)) - 499; lm_[i] <- lm_[i] + 1 }
  hc <- numeric(length(yy))
  for (k in seq_len(nrow(uu))) { i <- (uu$b[k]:uu$e[k]) - 499; w <- 1 / pmax(lm_[i], 1); hc[i] <- hc[i] + uu$N[k] * w / sum(w) }
  ex <- as.numeric(table(factor(x[!is.na(pyear)]$pyear, levels = yy)))
  dec <- (yy %/% 10) * 10
  cv <- rbind(data.table(decade = dec, method = "Exact years only", v = ex),
              data.table(decade = dec, method = "Standard aoristic", v = st),
              data.table(decade = dec, method = "Phase-corrected aoristic", v = hc))
  cv <- cv[decade >= 1000 & decade <= 1990, .(v = sum(v)), by = .(decade, method)]
  cv[, share := v / sum(v), by = method]
  list(curves = cv, lambda = data.table(year = yy, lambda = lm_))
}
period_of <- function(dec) cut(dec, c(999, 1299, 1499, 1699, 1899, 1999),
                               labels = c("1000-1299", "1300-1499", "1500-1699", "1700-1899", "1900-1999"))
cvA <- make_curves(dated)                                   # main sample
cvB <- make_curves(dall)                                    # with the placeholder block
per <- rbind(cvA$curves[, .(share = round(100 * sum(share), 2)), by = .(period = period_of(decade), method)][, sample := "A_block_excluded"],
             cvB$curves[, .(share = round(100 * sum(share), 2)), by = .(period = period_of(decade), method)][, sample := "B_block_included"])
out(per, "hmml_s05_period_shares_AB.csv")                   # Table 4, Supplementary Table S2

# 5c. who are the parts with no recorded writing system?
nr <- dated[is.na(writing_system) | writing_system == ""]
tab1 <- function(v, lab) {                                  # counts of one dimension
  if (nrow(nr) == 0) return(data.table(dimension = character(), value = character(), N = integer()))
  data.table(value = v)[, .N, by = value][, dimension := lab][order(-N)]
}
nr_tab <- rbind(tab1(nr$object_type, "object_type"), tab1(nr$part_type, "part_type"),
                tab1(top_k(nr$country_now, 10), "country"), tab1(top_k(nr$repository, 10), "repository"),
                tab1(nr$precision, "precision"), fill = TRUE)
nr_tab[, share := round(N / max(nrow(nr), 1), 4)]          # share within the 'not recorded' parts
out(nr_tab, "hmml_s05_not_recorded_script.csv")             # Section 4.4, note to Supplementary Table S6

# 5d. script x holding country (dated parts)
sc <- dated[, .(n = .N, exact_share = round(mean(exact), 4)),
            by = .(script = top_k(writing_system, 8), country = top_k(country_now, 12))][n >= 50]
setorder(sc, script, -n)
out(sc, "hmml_s05_script_by_country.csv")                   # Supplementary Table S6

# 5f. how common are multi-part objects?
mp <- dated[, .N, by = obj_id][, .(objects = .N, parts = sum(N),
                                     objects_multi = sum(N > 1), parts_in_multi = sum(N[N > 1]))]
out(mp, "hmml_s05_multipart.csv")                           # Section 3.2 (multi-part objects)

# 5g. population-averaged (marginal) probability of an exact year by century
sd_r <- sqrt(as.numeric(VarCorr(fit)$repo_f))               # repository standard deviation
qq   <- qnorm((1:400 - 0.5) / 400)                          # quadrature points of N(0,1)
pc[, eta := qlogis(p)]                                      # linear predictor of the typical repository
pc[, p_marginal := sapply(eta, function(e) mean(plogis(e + sd_r * qq)))]  # average over repositories
out(pc[, .(century, century_num, p_typical = round(p, 4), p_marginal = round(p_marginal, 4))],
    "hmml_s05_pred_marginal.csv")                           # Section 4.4 (population-averaged)

# 5h. the drop at 1500 by script group (Roman vs Arabic vs other scripts); Figure 5
dated[, sg := fcase(writing_system == "Roman", "Roman script",
                              writing_system == "Arabic", "Arabic script", default = "Other scripts")]
cg <- rbindlist(lapply(c("Roman script", "Arabic script", "Other scripts"), function(s)
  make_curves(dated[sg == s])$curves[, group := s]))
out(cg, "hmml_s05_curves_by_script.csv")                    # Figure 5
jumps <- cg[, .(boundary = decade[-1], change_pct = round(100 * (share[-1] / share[-.N] - 1))),
            by = .(group, method)][boundary %% 100 == 0]
out(jumps, "hmml_s05_century_jumps.csv")                    # Section 4.5 (changes at century boundaries)
gS1 <- ggplot(cg[method != "Phase-corrected aoristic"], aes(decade, share, linetype = method)) +
  geom_line() + facet_wrap(~ group, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
  scale_x_continuous(breaks = seq(1000, 2000, 100)) +
  labs(x = "Decade", y = "Share of the reconstructed total", linetype = NULL) + th
save_fig(gS1, "hmml_fig5_curves_by_script", width = 7, height = 7)  # Figure 5
old_s1 <- file.path(proj, "hmml_figS1_curves_by_script.png")  # earlier name of Figure 5
if (file.exists(old_s1)) invisible(file.remove(old_s1))   # remove the outdated copy

# 5i. overlap count lambda(t): distinct labels covering each year, by century
lam_c <- cvA$lambda[year >= 1000 & year <= 1999][, .(lambda_mean = round(mean(lambda), 1),
                                                      lambda_min = min(lambda), lambda_max = max(lambda)),
                                                  by = .(century = cent_of(year))]
std_y <- cvA$curves[method == "Standard aoristic"]
lam_c[, cor_lambda_std := round(cor(cvA$lambda[year >= 1000 & year <= 1999 & year %% 10 == 0, lambda],
                                    std_y$share), 3)]       # same value in every row
out(lam_c, "hmml_s05_lambda_by_century.csv")                # Supplementary Table S8


## ---- step06: further robustness checks -----------------------------
# 6a ICC on the observed (probability) scale       (no refit, fast)
# 6b script x holding-country interaction          (1 extra fit, cached)
# 6c century from random draws within each interval, 5 draws  (5 fits, cached)
# All new fits are cached in hmml_s06_models.rds (only missing fits are run).

m6_file <- file.path(proj, "hmml_s06_models.rds")           # cache of step06 fits
m6 <- if (file.exists(m6_file)) readRDS(m6_file) else list()  # read old fits if they exist

# 6a. observed-scale ICC by simulation (Goldstein, Browne and Rasbash 2002)
icc_obs <- function(eta0, sd_u) {                           # eta0 = linear predictor of one profile
  p  <- plogis(eta0 + sd_u * qq)                            # probability in repositories (quadrature over N(0,1))
  vb <- mean((p - mean(p))^2)                               # between-repository variance of the probability
  vw <- mean(p * (1 - p))                                   # within-repository (binomial) variance
  vb / (vb + vw)                                            # share of observed variance between repositories
}
eta_data <- drop(model.matrix(fit) %*% fixef(fit))          # linear predictor of every part in the model
get_eta <- function(cc) { v <- pc[century == cc, eta]; if (length(v)) v[1] else NA_real_ }  # NA if the century is not modelled
icc6 <- data.table(
  profile = c("typical profile: 15th century, Roman, paper",
              "typical profile: 12th century, Roman, paper",
              "typical profile: 20th century, Roman, paper",
              "average linear predictor of all modelled parts"),
  eta = c(get_eta("C15"), get_eta("C12"), get_eta("C20"), mean(eta_data)))  # four covariate profiles
icc6[, icc_observed := round(sapply(eta, function(e) if (is.na(e)) NA_real_ else icc_obs(e, sd_r)), 3)]   # observed-scale ICC
icc6[, icc_latent := round(sd_r^2 / (sd_r^2 + pi^2 / 3), 3)]          # latent ICC for comparison
icc6[, mor := round(exp(sqrt(2) * sd_r * qnorm(0.75)), 2)]            # median odds ratio
out(icc6[, .(profile, eta = round(eta, 3), icc_observed, icc_latent, mor)],
    "hmml_s06_icc_observed.csv")                            # Section 4.4 (ICC, median odds ratio)

# 6b. script x holding-country interaction (does the century gradient survive?)
d6b <- prep_m(dated, "c_mid")                               # same parts as the main model
d6b[, ctry_f := factor(top_k(country_now, 15))]             # 15 largest countries, others pooled
d6b[, combo := paste(writ_f, ctry_f, sep = " | ")]          # script-country combination
big <- d6b[, .N, by = combo][N >= 200, combo]               # combinations with >= 200 parts
d6b[, sc_f := factor(fifelse(combo %in% big, combo, "pooled"), levels = c("pooled", sort(big)))]  # interaction cells, reference = pooled
f_add <- exact ~ cent_f + writ_f + supp_f + ctry_f + (1 | repo_f)           # additive (as in M6)
f_int <- exact ~ cent_f + writ_f + supp_f + ctry_f + sc_f + (1 | repo_f)    # + interaction cells
if (is.null(m6$add)) { message("step06 fitting additive model"); m6$add <- fit_glmm(f_add, d6b); saveRDS(m6, m6_file) }
if (is.null(m6$int)) { message("step06 fitting interaction model"); m6$int <- fit_glmm(f_int, d6b); saveRDS(m6, m6_file) }
lr  <- 2 * (as.numeric(logLik(m6$int)) - as.numeric(logLik(m6$add)))   # likelihood-ratio statistic
ldf <- length(fixef(m6$int)) - length(fixef(m6$add))       # extra parameters actually estimated
cent_or <- function(f, lab) {                               # century odds ratios of one fit
  x <- as.data.table(summary(f)$coefficients, keep.rownames = "term")[grepl("^cent_f", term)]
  setnames(x, c("term", "est", "se", "z", "p"))
  vr <- as.numeric(VarCorr(f)$repo_f)
  x[, .(model = lab, term, odds_ratio = round(exp(est), 3),
        or_low = round(exp(est - 1.96 * se), 3), or_high = round(exp(est + 1.96 * se), 3),
        icc_latent = round(vr / (vr + pi^2 / 3), 3))]
}
sc_tab <- rbind(cent_or(m6$add, "additive script + country"),
                cent_or(m6$int, "with script x country cells"))
sc_tab[, `:=`(lr_chisq = round(lr, 1), lr_df = ldf,
              lr_p = signif(pchisq(lr, ldf, lower.tail = FALSE), 3),
              n_cells = length(big), n_parts = nobs(m6$int))]  # test of the interaction (same rows)
out(sc_tab, "hmml_s06_script_country_model.csv")            # Section 4.4, Supplementary Table S1

# 6c. century from a random year inside each interval (5 draws, pooled with Rubin's rules)
lo <- with(dated, fcoalesce(as.numeric(pyear), as.numeric(begin), as.numeric(end), (cent_min - 1) * 100 + 1))  # earliest year
hi <- with(dated, fcoalesce(as.numeric(pyear), as.numeric(end), as.numeric(begin), cent_max * 100))             # latest year
sw <- !is.na(lo) & !is.na(hi) & hi < lo                     # reversed intervals
tmp <- lo[sw]; lo[sw] <- hi[sw]; hi[sw] <- tmp              # swap them
n_draw <- 5                                                 # number of random draws
for (k in seq_len(n_draw)) {
  nmk <- paste0("draw", k)
  if (!is.null(m6[[nmk]])) next                             # already fitted -> skip
  set.seed(2026 + k)                                        # reproducible draw
  yk <- lo + floor(runif(length(lo)) * (hi - lo + 1))       # uniform year inside the interval
  dk <- copy(dated)[, c_draw := cent_of(yk)]                # century of the drawn year
  message("step06 fitting ", nmk)
  m6[[nmk]] <- fit_glmm(base_f, prep_m(dk, "c_draw"))       # same model as the main one
  saveRDS(m6, m6_file)                                      # save after every fit
}
dr <- rbindlist(lapply(paste0("draw", seq_len(n_draw)), function(nmk) {
  f <- m6[[nmk]]
  x <- as.data.table(summary(f)$coefficients, keep.rownames = "term")[grepl("^cent_f", term)]
  setnames(x, c("term", "est", "se", "z", "p"))
  x[, `:=`(draw = nmk, repo_var = as.numeric(VarCorr(f)$repo_f), n_parts = nobs(f))]
}))
pool <- dr[, if (.N == n_draw) .(                           # Rubin's rules (terms present in every draw)
  est = mean(est),
  se  = sqrt(mean(se^2) + (1 + 1 / n_draw) * var(est)),     # within + between variance
  repo_var = mean(repo_var), n_parts = round(mean(n_parts))), by = term]
pool[, `:=`(odds_ratio = round(exp(est), 3),
            or_low = round(exp(est - 1.96 * se), 3), or_high = round(exp(est + 1.96 * se), 3),
            icc_latent = round(repo_var / (repo_var + pi^2 / 3), 3))]
out(pool[, .(term, odds_ratio, or_low, or_high, icc_latent, n_parts)],
    "hmml_s06_century_random_draws.csv")                    # Supplementary Table S1 (random draw)


## ---- step07: estimation and weighting checks ------------------------
# 7a main model refitted with the full Laplace approximation (nAGQ = 1)
# 7b main model without printed parts       7c main model without papyrus parts
# 7d centre-weighted (triangular) aoristic reconstruction vs uniform weights
# All new fits are cached in hmml_s07_models.rds (only missing fits are run).

m7_file <- file.path(proj, "hmml_s07_models.rds")           # cache of step07 fits
m7 <- if (file.exists(m7_file)) readRDS(m7_file) else list()  # read old fits if they exist

# 7a. full Laplace approximation: Option A bobyqa, Option B nloptwrap
if (is.null(m7$laplace)) {
  message("step07 fitting full Laplace model (slow)")
  m7$laplace <- tryCatch(
    glmer(base_f, data = mm, family = binomial, nAGQ = 1,
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))),
    error = function(e) { message("Option A error: ", conditionMessage(e)); NULL })
  if (is.null(m7$laplace))                                  # Option B: other optimiser
    m7$laplace <- glmer(base_f, data = mm, family = binomial, nAGQ = 1,
                        control = glmerControl(optimizer = "nloptwrap"))
  saveRDS(m7, m7_file)                                      # save the fit
}

# 7b. printed parts excluded (part type 'Printed')
if (is.null(m7$no_printed)) {
  message("step07 fitting model without printed parts")
  d7b <- dated[is.na(part_type) | part_type != "Printed"]   # keep manuscript and unknown parts
  m7$no_printed <- fit_glmm(base_f, prep_m(d7b, "c_mid"))
  saveRDS(m7, m7_file)
}

# 7c. papyrus parts excluded (their coefficient is not estimable)
if (is.null(m7$no_papyrus)) {
  message("step07 fitting model without papyrus parts")
  sup <- toupper(fcoalesce(dated$support_part, dated$support_obj))  # support of each part
  d7c <- dated[is.na(sup) | sup != "PAPYRUS"]                # drop papyrus only
  m7$no_papyrus <- fit_glmm(base_f, prep_m(d7c, "c_mid"))
  saveRDS(m7, m7_file)
}

conv <- function(f) {                                       # convergence messages of a fit (empty = none)
  msg <- f@optinfo$conv$lme4$messages
  if (is.null(msg)) "none" else paste(msg, collapse = " | ")
}
chk <- rbindlist(lapply(c("main (fast Laplace)", "laplace", "no_printed", "no_papyrus"), function(nm) {
  f <- if (nm == "main (fast Laplace)") fit else m7[[nm]]
  x <- as.data.table(summary(f)$coefficients, keep.rownames = "term")[grepl("^cent_f|^writ_f", term)]
  setnames(x, c("term", "est", "se", "z", "p"))
  vr <- as.numeric(VarCorr(f)$repo_f)
  x[, .(model = nm, term, odds_ratio = round(exp(est), 3),
        or_low = round(exp(est - 1.96 * se), 3), or_high = round(exp(est + 1.96 * se), 3),
        repo_var = round(vr, 3), icc_latent = round(vr / (vr + pi^2 / 3), 3),
        n_parts = nobs(f), convergence = conv(f))]
}))
out(chk, "hmml_s07_checks_models.csv")                      # Supplementary Table S1 (last three rows)

# 7d. centre-weighted aoristic reconstruction (triangular weights inside each interval)
curve_w <- function(x, kernel = c("uniform", "triangular")) {
  kernel <- match.arg(kernel)
  yy <- 500:2025
  x <- copy(x)
  x[, `:=`(bb = fifelse(!is.na(pyear), pyear, begin), ee = fifelse(!is.na(pyear), pyear, end))]
  x <- x[!is.na(bb) & !is.na(ee) & ee >= 500 & bb <= 2025]
  x[, `:=`(b = pmax(bb, 500), e = pmin(ee, 2025))]
  uu <- x[, .N, by = .(b, e)]                               # unique intervals with counts
  cv <- numeric(length(yy))
  for (k in seq_len(nrow(uu))) {
    t <- uu$b[k]:uu$e[k]                                    # years of the interval
    w <- if (kernel == "uniform") rep(1, length(t)) else {
      m <- (uu$b[k] + uu$e[k]) / 2; h <- (uu$e[k] - uu$b[k]) / 2 + 1
      1 - abs(t - m) / h                                    # highest at the midpoint, > 0 at the ends
    }
    cv[t - 499] <- cv[t - 499] + uu$N[k] * w / sum(w)      # each part keeps a total weight of one
  }
  dec <- (yy %/% 10) * 10
  r <- data.table(decade = dec, v = cv)[decade >= 1000 & decade <= 1990, .(v = sum(v)), by = decade]
  r[, share := v / sum(v)][, method := kernel]
  r
}
cw <- rbind(curve_w(dated, "uniform"), curve_w(dated, "triangular"))
per_w <- cw[, .(share = round(100 * sum(share), 2)), by = .(period = period_of(decade), method)]
out(per_w, "hmml_s07_period_shares_weights.csv")            # Section 4.5, note to Supplementary Table S2
jw <- cw[order(method, decade)][, .(boundary = decade[-1], change_pct = round(100 * (share[-1] / share[-.N] - 1))),
                                by = method][boundary %% 100 == 0]
out(jw, "hmml_s07_century_jumps_weights.csv")               # Section 4.5 (centre-weighted check)

message("step00-07 finished.")                              # done
