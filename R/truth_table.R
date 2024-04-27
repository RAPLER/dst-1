#'  Helper to the construction of a truth table
#' 
#'  The \code{truth_table} function is used to generate the truth table of a logic proposition
#'  @param x The character string  of a valid logic formula. See examples for the writing syntax.
#'  @return the truth table of the propositional formula.
#'  @details Operators follow R syntax. Implication is written with negation and or operators. Si example 1.
#'  @author Claude Boivin
#'  @export
#' @references  \itemize{
#' \item "https://rosettacode.org/wiki/Truth_table#R"
#' \item "https://www.r-bloggers.com/2021/05/learning-r-creating-truth-tables/"
#' }
#' @examples
#' tt_pqs <- truth_table("!(p & q) | s") # implication
#' truth_table("!A") # not A
#' truth_table("A | B") # A or B
#' truth_table("A & B") # A and B
#' "%^%" <- xor # define unary xor operator
#' truth_table("A %^% B") # xor 
#' truth_table("S | (T %^% U)") # 3 variables with brackets 
#' 
truth_table <- function(x) {
  vars <- unique(unlist(strsplit(x, "[^a-zA-Z]+")))
  vars <- vars[vars != ""]
  perm <- expand.grid(rep(list(c(FALSE, TRUE)), length(vars)))
  names(perm) <- vars
  perm[ , x] <- with(perm, eval(parse(text = x)))
  return(perm)
}