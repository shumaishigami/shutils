#' Put a given vector in parentheses
#' @examples
#' put_in_paren(1:3)
#' @export
put_in_paren <- function(x, sep = "'") paste0("(", paste0(sep, x, sep, collapse = ", "), ")")


#' Replace a certain character in a text
#' @param txt text
#' @param var_sep a character to separete a variable from a context. Default "?" as, "?var?"
#' @param var_list a list of variable names and characters to replace variables
#' @examples
#' sql <- "
#' select
#'   user_id,
#'   names
#' from user_master
#' where 1=1
#'   and user_id = ?user_id?
#' ;
#' "
#' cat(sql)
#' cat(replace_var(sql, user_id = 123))
#' @export
replace_var <- function(txt, var_sep = "?", var_list, ...) {
  if (missing(var_list)) var_list <- list(...)
  for (var in names(var_list)) {
    txt <- gsub(
      pattern = paste0(var_sep, var, var_sep),
      replacement = var_list[[var]],
      x = txt,
      fixed = TRUE
    )
  }
  return(txt)
}

check_s3_connection <- function (aws_bucket){
  if (is_try_error(try(aws.s3::bucket_exists(aws_bucket)))) {
    stop(sprintf("Connection to %s failed", aws_bucket))
  }
}

#' Connect to Redshift
#' @export
connect_to_db <- function (db_config){
  stopifnot(is.list(db_config))
  for (itm in c("db_host", "db_user", "db_password", "db_dbname", "db_port")) {
    if (is.null(db_config[[itm]])) {
      stop(sprintf("%s is null", itm))
    }
  }

  con <- RPostgreSQL::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = db_config[["db_host"]],
    user = db_config[["db_user"]],
    password = db_config[["db_password"]],
    dbname = db_config[["db_dbname"]],
    port = db_config[["db_port"]]
  )
  return(con)
}

#' Connect to S3
#' @export
connect_to_s3 <- function(aws_config) {
  stopifnot(is.list(aws_config))
  for (itm in c("aws_region", "aws_access_key_id", "aws_secret_access_key", "aws_bucket")) {
    if (is.null(aws_config[[itm]])) {
      stop(sprintf("%s is null", itm))
    }
  }

  Sys.setenv(
    "AWS_DEFAULT_REGION" = aws_config[["aws_region"]],
    "AWS_ACCESS_KEY_ID" = aws_config[["aws_access_key_id"]],
    "AWS_SECRET_ACCESS_KEY" = aws_config[["aws_secret_access_key"]],
    "AWS_BUCKET" = aws_config[["aws_bucket"]]
  )

  if (aws.s3::bucket_exists(aws_config[["aws_bucket"]])) {
    sprintf("Connected to %s", aws_config[["aws_bucket"]])
  } else {
    stop("Connection failed. Check AWS configuration. ")
  }
}

#' List files from S3
#' @export
list_s3_files <- function(path = NULL, aws_bucket = NULL) {
  if(is.null(path) | path == "") stop("path is null")

  if(is.null(aws_bucket)){
    aws_bucket <- Sys.getenv("AWS_BUCKET")
  }
  check_s3_connection(aws_bucket)
  return(aws.s3::get_bucket_df(aws_bucket, prefix = path, max = Inf)[["Key"]])
}

#' Read file in S3
#' @export
read_from_s3 <- function(path = NULL, aws_bucket = NULL, delim = "\t", ...) {
  if(is.null(path) | path == "") stop("path is null")
  if(is.null(aws_bucket)) {
    aws_bucket <- Sys.getenv("AWS_BUCKET")
  }
  check_s3_connection(aws_bucket)
  aws.s3::s3read_using(FUN = readr::read_delim, delim = delim, ..., object = path, bucket = aws_bucket)
}

#' Export to S3
#' @export
write_to_s3 <- function(x, path = NULL, aws_bucket = NULL, delim = "\t", ...) {
  stopifnot(is.data.frame(x))
  if(is.null(path) | path == "") stop("path is null")
  if(is.null(aws_bucket)) {
    aws_bucket <- Sys.getenv("AWS_BUCKET")
  }
  check_s3_connection(aws_bucket)
  aws.s3::s3write_using(x = x, FUN = readr::write_delim, delim = delim, ..., bucket = aws_bucket, object = path)
}
