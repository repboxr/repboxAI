

schema_empty_instance = function(x) {
  if (is.null(x))  return(NULL)
  if (is_schema_obj(x)) {
    return(lapply(x$properties, schema_empty_instance))
  }
  if (is_schema_arr(x)) {
    empty_item = schema_empty_instance(x$items)
    if (is.list(empty_item)) empty_item = as.data.frame(empty_item)
    return(empty_item)
  }
  if (is_schema_str(x)) return(character(0))
  if (is_schema_int(x)) return(integer(0))
  if (is_schema_num(x)) return(numeric(0))
  if (is_schema_bool(x)) return(logical(0))
  restore.point("schema_empty_instance_err")
  stop(no_schema_obj_msg(x))
}
