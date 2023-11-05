check_ornament_length <- function(x, arg_nm = caller_arg(x),
                                  call = caller_env()) {
  check_number_decimal(
    x,
    min = 0,
    allow_infinite = FALSE,
    allow_na       = FALSE,
    allow_null     = FALSE,
    arg = arg_nm, call = call
  )
}

validate_length <- function(base = NULL, head = NULL, fins = NULL, mid = NULL,
                            default = 4, call = caller_env()) {
  base <- base %||% default
  check_ornament_length(base, arg_nm = "length",      call = call)
  head <- head %||% base
  check_ornament_length(head, arg_nm = "length_head", call = call)
  fins <- fins %||% base
  check_ornament_length(fins, arg_nm = "length_fins", call = call)
  mid  <- mid  %||% base
  check_ornament_length(mid,  arg_nm = "length_mid",  call = call)
  list(
    head = head,
    fins = fins,
    mid  = mid
  )
}
