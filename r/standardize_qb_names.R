#standardize_qb_names.R

standardize_qb_names <- function(data, name_col) {
  name_col <- rlang::ensym(name_col)
  
  data %>%
    mutate(
      {{ name_col }} := str_replace({{ name_col }}, "Cameron Ward", "Cam Ward"),
      {{ name_col }} := str_replace({{ name_col }}, "Mitch Trubisky", "Mitchell Trubisky"),
      {{ name_col }} := ifelse(str_detect({{ name_col }}, "M.+Penix"), 
                               "Michael Penix", 
                               {{ name_col }}),
      {{ name_col }} := ifelse(
        {{ name_col }} |> tolower() |> str_remove_all("[[:punct:]]| ") |> str_detect("robertgriffin"),
        "Robert Griffin III",
        {{ name_col }}
      ),
      {{ name_col }} := ifelse(
        {{ name_col }} |> tolower() |> str_remove_all("[[:punct:]]| ") |> str_detect("ajmccarron"),
        "A.J. McCarron",
        {{ name_col }}
      )
    )
}