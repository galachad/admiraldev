roxygen_clean_default <- function(path) {
  assert_character_scalar(path)

  # check that file exists
  if (!file.exists(path)) {
    stop("Invalid file path, the file does not exist.")
  }

  # check that file is in ./R/
  uses_r_file <- grepl("R/", path, fixed = T)
  if (!uses_r_file) {
    stop("This Addin works only on R Scripts that build the package functions.")
  }

  all_lines <- readLines(path) %>%
    data.frame(lines = .) %>%
    mutate(line_number = seq(1, n()))

  function_arg_lines <- all_lines %>%
    filter(!str_detect(lines, "#'")) %>%
    filter(str_detect(lines, "<- function\\(") |
           str_detect(lines, "\\) \\{") |
           str_detect(lines, "<- function\\(.*\\) \\{")) %>%
    mutate(fun = ifelse(str_detect(lines, " <- function\\("), str_split_fixed(lines, " <- function\\(", 2), NA)) %>%
    fill(fun) %>%
    mutate(headerfl = case_when(str_detect(lines, " <- function\\(") == TRUE ~ "open",
                                str_detect(lines, "\\) \\{") == TRUE ~ "close"),
           oneline = ifelse(str_detect(lines, "<- function\\(.*\\) \\{"), "Y", NA)) %>%
    pivot_wider(., names_from = headerfl, values_from = line_number)%>%
    group_by(fun) %>%
    mutate(open = min(open, na.rm = TRUE),
           close = ifelse(is.na(oneline), min(close, na.rm = TRUE), open),
           openclose = map2(.x = open, .y = close, .f = function(x, y) seq(x, y)),
           index = seq(1,n())) %>%
    filter(index == 1) %>%
    ungroup() %>%
    unnest(openclose) %>%
    pull(openclose)

  function_defaults <- all_lines %>%
    filter(line_number %in% function_arg_lines) %>%
    mutate(fun = ifelse(str_detect(lines, " <- function\\("), str_split_fixed(lines, " <- function\\(", 2), NA)) %>%
    fill(fun) %>%
    group_by(fun) %>%
    mutate(pivotid = seq(1,n())) %>%
    ungroup() %>%
    left_join(., pivot_wider(select(., -line_number), names_from = pivotid, values_from = lines), by = "fun") %>%
    unite(., alllines,-c(lines, line_number, fun, pivotid), sep = " ") %>%
    mutate(alllines = str_replace_all(alllines, ".*\\(| |\\).*", ""),
           paramclean1 = str_split(alllines, ",")) %>%
    select(fun, paramclean1) %>%
    distinct() %>%
    unnest(paramclean1) %>%
    mutate(paramclean2 = str_split(paramclean1, "=")) %>%
    unnest(paramclean2) %>%
    group_by(fun, paramclean1) %>%
    mutate(pivotid = seq(1, n()),
           pivotid = ifelse(pivotid == 1, "param", "defaultval")) %>%
    ungroup() %>%
    distinct() %>%
    pivot_wider(., names_from = pivotid, values_from = paramclean2)

  if(!("defaultval" %in% names(function_defaults))){
    function_defaults <- function_defaults %>%
      mutate(defaultval = NA)%>%
      select(fun, param, defaultval)
  }

  if("defaultval" %in% names(function_defaults)) {
    function_defaults <- function_defaults %>%
    select(fun, param, defaultval)
  }


  roxygen_key <- all_lines %>%
    filter(str_detect(lines, "#'") | (!str_detect(lines, "#'") & str_detect(lines, "<- function\\("))) %>%
    mutate(fun = ifelse(str_detect(lines, " <- function\\("),
                        str_split_fixed(lines, " <- function\\(", 2),
                        NA
    )) %>%
    fill(fun, .direction = "up") %>%
    mutate(
      lines_split = str_split(lines, " "),
      tag_param = map(.x = lines_split, .f = function(x) x[c(str_which(x, "@"), str_which(x, "@") + 1)])
    ) %>%
    unnest(tag_param) %>%
    group_by(line_number) %>%
    mutate(
      pivot_id = seq(1, n()),
      pivot_id = ifelse(pivot_id == 1, "tag", "param")
    ) %>%
    ungroup() %>%
    pivot_wider(., names_from = pivot_id, values_from = tag_param) %>%
    left_join(., function_defaults, by = c("fun", "param")) %>%
    select(line_number, fun, tag, param, defaultval)

  roxygen_chunk <- roxygen_key %>%
    mutate(paramfl = ifelse(tag == "@param", "open", "close")) %>%
    group_by(fun, paramfl) %>%
    mutate(keep_id = seq(1, n())) %>%
    filter(keep_id == 1) %>%
    select(fun, paramfl, line_number) %>%
    pivot_wider(., names_from = paramfl, values_from = line_number) %>%
    mutate(
      open = ifelse(is.na(open), close, open),
      close = ifelse(is.na(close), open, close) - 1,
      openclose = map2(.x = open, .y = close, .f = function(x, y) seq(x, y))
    ) %>%
    unnest(openclose) %>%
    pull(openclose)

  final <- all_lines %>%
    filter(line_number %in% roxygen_chunk) %>%
    left_join(., roxygen_key, by = c("line_number")) %>%
    select(lines, line_number, fun, param, defaultval) %>%
    fill(fun) %>%
    group_by(fun)%>%
    fill(param) %>%
    ungroup() %>%
    group_by(fun, param) %>%
    fill(defaultval) %>%
    mutate(defaultstatement =
             ifelse(
               is.na(defaultval),
               lines,
               paste("#'   *Default*: ", "`", defaultval, "`", sep = "")
             )
    ) %>%
    ungroup() %>%
    filter(str_detect(tolower(lines), "default") & (lines != defaultstatement))

  recommended_fix <- final %>%
    select(defaultstatement, line_number) %>%
    mutate(line_number = line_number + 0.01) %>%
    rename(lines = defaultstatement)

  file_content_new <- all_lines %>%
    rbind(., recommended_fix) %>%
    arrange(line_number) %>%
    pull(lines)

  list(file_content = file_content_new)
}

# Function for the RStudio Addin, see inst/rstudio/addins.dcf.
# Wrapper of prepare_test_that_file.
format_function_default <- function() {
  file_info <- rstudioapi::getActiveDocumentContext()
  rstudioapi::documentSave(id = file_info$id)
  result <- roxygen_clean_default(path = file_info$path)
  rstudioapi::setDocumentContents(paste0(result$file_content, collapse = "\n"), id = file_info$id)
  rstudioapi::documentSave(id = file_info$id)
}
