#function to return a matrix of fields by area for a table #module2
stacks_matrix <- function(areas, db, feeldz, field_namez, area_namez) {
  df_list <- map(1:length(feeldz), function(x)
    map_df(areas, function(y)
      setNames(data.frame(field1 = get_stacks_data(db, feeldz[x], y)), field_namez[x])
    )
  )
  cbind(Area = area_namez, bind_cols(df_list))
}
#make a function for the new query
stacks_matrix2 <- function(areas_df, db, fields, field_names) {
  formatted_areas <- areas_df[,1] %>% map_chr(function(x)
    str_c(paste0("'", x, "'"), sep = "", collapse = ", ")
  ) %>% str_c(sep = "", collapse = ", ")
  formatted_fields <- str_c(fields, sep = "", collapse = ", ")
  q <- dbGetQuery(stacks,
                  paste0("SELECT AREAID,", formatted_fields, 
                         " FROM [dbo].[", db, "]
                  WHERE AREAID IN (", formatted_areas, ")")) %>%
    mutate(AREAID = trimws(AREAID))
  q <- left_join(areas_df, q, by = "AREAID")
  names(q) <- c("areaid", "Area", "% in Area", field_names)
  return(q[,-1])
}
#make a function for the new query (removes the area detail columns)
stacks_matrix3 <- function(areas_df, db, fields, field_names) {
  formatted_areas <- areas_df[,1] %>% map_chr(function(x)
    str_c(paste0("'", x, "'"), sep = "", collapse = ", ")
  ) %>% str_c(sep = "", collapse = ", ")
  formatted_fields <- str_c(fields, sep = "", collapse = ", ")
  q <- dbGetQuery(stacks,
                  paste0("SELECT AREAID,", formatted_fields, 
                         " FROM [dbo].[", db, "]
                  WHERE AREAID IN (", formatted_areas, ")")) %>%
    mutate(AREAID = trimws(AREAID))
  q <- left_join(areas_df, q, by = "AREAID")
  names(q) <- c("areaid", "Area", "% in Area", field_names)
  return((q[,-c(1, 2, 3), drop = FALSE]))
}
