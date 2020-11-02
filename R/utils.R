#' @export
# ---
# Column-Typ Guesser Function
guess_column <- function(data){

  # ---
  # Initial alpha-numeric columns
  # ---

  # Identify
  chars <- sapply(data, is.character)

  # High variability of values indicate character
  factors_char <- names(which(sapply(data[, names(data[,chars])], function(x) length(table(x))) > 20))

  # "ID" in variable indicate chracter
  ids_char <- names(data[,chars][which(str_detect(names(data[ ,chars]), "ID|iD|Id|id_|_id_|_id"))])

  # Combine
  notrans_char <- unique(c(factors_char,ids_char))

  # Label variables not to be transformed as F (no factor)
  chars[names(chars) %in% notrans_char] <- F

  # ---
  # Initial numeric columns
  # ---

  # Identify
  nums <- sapply(data, is.numeric)

  # Low variability due to zero-point mass can mislead to factor
  zeros_num <- names(which(sapply(data[, names(data[,nums])], function(x) length(table(x))) == 1 ))

  # High variability of values indicate numeric
  factors_num <- names(which(sapply(data[, names(data[,nums])],  function(x) length(table(x))) > 20))


  # Combine
  notrans_num <- unique(c(factors_num, zeros_num))

  # Label them as F (not factor)
  nums[names(nums) %in% notrans_num] <- F
  # ---

  # Factors
  guess_factor <- c(chars[chars==TRUE],  nums[nums == TRUE],   nums[nums == F & chars == F])

  # Convert all guessed Elements To Factors
  b <- data %>%
    mutate_at(names(data[,colnames(data) %in% names(guess_factor[guess_factor==TRUE])]), funs(factor))

  # Detect Sortable Factors (do not contain character elements)
  x <- lapply(data[,names(data[,guess_factor])], str_detect, pattern=regex("[a-z]|[A-Z]|[:punct:]|[^0+]"))
  y <- lapply(x, any)
  z <- data.frame(unlist(y))
  z$var <- row.names(z)
  sortable <- z %>%
    filter(unlist.y. == F) %>%
    select(var) %>%
    pull()

  # Sort sortable Factors
  data.set <- mutate_at(b, sortable, funs(factor(., levels = paste(sort(as.integer(levels(.)))))))
  return(data.set)

}
# ---

# ---
guess_id <- function(data){


  # Factors with many levels indicate character
  #factors <- sapply(data, is.factor)
  #chars_factor <- names(which(sapply(data[,factors], ltable) > 80))

  # "ID" in variable indicate chracter
  chars_id <- names(data[which(str_detect(names(data), "ID|iD|Id|id_|_id_|_id"))])

  # Combine
  chars_trans <- unique(chars_id)

  # Transform
  guessed_ids <-
    data %>%
    mutate_at(chars_trans, funs(as.character))

  return(guessed_ids)
}
# ---

# ---
# Pull safely, if a user-input element is not specified
pull_safely <- safely(pull)
# ---

# ---
# Logs Abrunden
roundDown <- function(x){
  if (x == -Inf | is.na(x)) {
    return(0)
  }
  else {
    return(floor(x))
  }
}
# ---

# ---
# Logs Aufrunden
roundUp <- function(x) ceiling(x)
# ---

# ---
# Calculate length of a table
variable_variability <- function(x){
  if (is.character(x)){
    i <- length(table(x)) / length(x != "")
  }

  if (is.numeric(x)){
    i <- length(table(x)) / length(!is.na(x))
  }
  return(i)
}
# ---

