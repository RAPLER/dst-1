#' The Captain's Problem. \code{ads}: Relation between variables Arrival (A), Departure delay (D) and Sailing delay (S)
#' 
#' This dataset is the \code{tt} matrix establishing the relation A = D + S, where A = 0:6, D = 0:3 and S = 0:3. The subset made of all the triplets (a,d,s) of (A x D x S) where a = d + s is true has a mass value of 1. To construct the \code{tt} matrix, we put the variables A, D, S side by side, as in a truth table representation. Each triplet of the subset is described by a row of the matrix as a vector of zeros and ones.
#' @author Claude Boivin, Stat.ASSQ
#' @format An integer matrix with 18 rows and 17 columns
#' \describe{
#'   \item{[1, c(1,2)]}{value = 0, not used}
#'   \item{[1, 3:17]}{Identification numbers of the three variables. Column 3 to 9: variable 1; column 10 to 13: variable 2; column 14 to 17: variable 3.}
#'   \item{nospec}{identification number of the specification}
#'   \item{m}{the value of the specification, a number between 0 and 1}
#'   \item{6}{1 if 6 is part of the specification, 0 otherwise}
#'   \item{5}{1 if 5 is part of the specification, 0 otherwise}
#'   \item{4}{1 if 4 is part of the specification, 0 otherwise}
#'   \item{3}{1 if 3 is part of the specification, 0 otherwise}
#'   \item{2}{1 if 2 is part of the specification, 0 otherwise}
#'   \item{1}{1 if 1 is part of the specification, 0 otherwise}
#'   \item{0}{1 if 0 is part of the specification, 0 otherwise}
#'   }
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"ads"
#' 
#' The Captain's Problem. \code{dlfm}: Relation between variables Departure delay (D), Loading delay (L), Forecast of the weather (F), Maintenance delay (M)
#' 
#' This dataset is the \code{tt} matrix establishing the relation between the four variables. Each event (loading = true, forecast = foul, Maintenance = true) adds one day of Departure Delay. The elements (d,l, f, m) of (D x L x F x M) satisfying the relation form a subset with a mass value of 1. To construct the \code{tt} matrix, we put the variables D,L,F,M side by side, as in a truth table representation. Each 4-tuple of the subset is described by a row of the matrix as a vector of zeros and ones.
#' @author Claude Boivin, Stat.ASSQ
#' @format An integer matrix with 10 rows and 12 columns.
#' \describe{
#'   \item{[1,c(1,2)]}{value = 0, not used}
#'   \item{[1,3:12]}{Identification numbers of the four variables. Column 3 to 6: variable 2; columns 7,8: variable 4; columns 9, 10: variable 5: columns 11,12: variable 6.}
#'   \item{nospec}{identification number of the specification}
#'   \item{m}{the value of the specification, a number between 0 and 1}
#'   \item{3}{1 if d3 is part of the specification, 0 otherwise}
#'   \item{2}{1 if d2 is part of the specification, 0 otherwise}
#'   \item{1}{1 if d1 is part of the specification, 0 otherwise}
#'   \item{0}{1 if d0 is part of the specification, 0 otherwise}
#'   \item{true}{1 if true is part of the specification, 0 otherwise}
#'   \item{false}{1 if false is part of the specification, 0 otherwise}
#'   \item{foul}{1 if foul is part of the specification, 0 otherwise}
#'   \item{fair}{1 if fair is part of the specification, 0 otherwise}
#'   }
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"dlfm"
#' 
#' The Captain's Problem. \code{fw}: Relation between variables Forecast of the weather (F) and  Weather at sea (W)
#' 
#' This dataset is the \code{tt} matrix establishing the relation between the two variables. An accurate forecast is described by this subset of two event:  (Forecast = foul, Weather = foul) and (Forecast = fair, Weather = fair). We assign a mass value of 0.8 to this subset. The remaining mass of 0.2 is allotted to the frame. To construct the \code{tt} matrix, we put the variables F and W side by side, as in a truth table representation. Each pair of the subset is described by a row of the matrix as a vector of zeros and ones.
#' 
#' @author Claude Boivin
#' @format An integer matrix with 4 rows and 6 columns.
#' \describe{
#'   \item{[1,c(1,2)]}{value = 0, not used}
#'   \item{[1,3:6]}{Identification numbers of the two variables. Column 3,6: variable 5; columns 5,6: variable 7.}
#'   \item{nospec}{identification number of the specification}
#'   \item{m}{the value of the specification, a number between 0 and 1}
#'   \item{foul}{1 if foul is part of the specification, 0 otherwise}
#'   \item{fair}{1 if fair is part of the specification, 0 otherwise}
#'   }
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"fw"
#' 
#' The Captain's Problem. \code{mrf}: Relation between variables No Maintenance (M = false) and  Repairs at sea (R)
#' 
#' This dataset is the \code{tt} matrix establishing a set of two relations between the two variables. First, Repairs = true if Maintenance = false in (M x R). We are 20\% sure that there will be Repairs if no maintenance. Second, Repairs = false if Maintenance = false in (M x R). We are 20\% sure that there will be no repairs if no maintenance. 
#' 
#' These two relations are implication rules. The remaining mass of 0.6 is allotted to the frame. To construct the \code{tt} matrix, we put the variables M and R side by side, as in a truth table representation. Each pair of the subset is described by a row of the matrix as a vector of zeros and ones.
#' @author Claude Boivin, Stat.ASSQ
#' @format A (0,1) matrix with 4 rows and 6 columns.
#' \describe{
#'   \item{[1,c(1,2)]}{value = 0, not used}
#'   \item{[1,3:6]}{Identification numbers of the two variables. Column 3,4: variable 6; columns 5,6: variable 8}
#'   \item{nospec}{identification number of the specification}
#'   \item{m}{the value of the specification, a number between 0 and 1}
#'   \item{true}{1 if true is part of the specification, 0 otherwise}
#'   \item{false}{1 if false is part of the specification, 0 otherwise}
#'   }
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"mrf"
#' 
#'The Captain's Problem. \code{mrt}: Relation between variables Maintenance done (M = true) and  Repairs at sea (R)
#' 
#' This dataset is the \code{tt} matrix establishing a set of two relations between the two variables. First, Repairs = true if Maintenance = true in (M x R). We are 10\% sure that there will be Repairs if maintenance is done. Second, Repairs = false if Maintenance = true in (M x R). We are 70\% sure that there will be no repairs if maintenance is done. 
#' 
#' These two relations are implication rules. The remaining mass of 0.2 is allotted to the frame. To construct the \code{tt} matrix, we put the variables M and R side by side, as in a truth table representation. Each pair of the subset is described by a row of the matrix as a vector of zeros and ones.
#' @author Claude Boivin, Stat.ASSQ
#' @format A (0,1) matrix with 4 rows and 6 columns.
#' \describe{
#'   \item{[1,c(1,2)]}{value = 0, not used}
#'   \item{[1,3:6]}{Identification numbers of the two variables. Column 3,4: variable 6; columns 5,6: variable 8}
#'   \item{nospec}{identification number of the specification}
#'   \item{m}{the value of the specification, a number between 0 and 1}
#'   \item{true}{1 if true is part of the specification, 0 otherwise}
#'   \item{false}{1 if false is part of the specification, 0 otherwise}
#'   }
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"mrt"
#' 
#' The Captain's Problem. \code{swr}: Relation between variables Sailing delay (S), Weather at sea (W), and Repairs at sea (R)
#' 
#' This dataset is the \code{tt} matrix establishing a relation between S, W and R, where S = 0:3, W = (foul, fair) and R = (true, false). The goal of this relation is to account for other causes of sailing delay. All the elements (s,w,r) of (S x W x R) where W or R is true add one day of sailing delay. We put a mass value of 0.9 to this subset. The remaining mass of 0.1 is allotted to the frame.
#' 
#' To construct the \code{tt} matrix, we put the variables S, W, R side by side, as in a truth table representation. Each triplet of the subset is described by a row of the matrix as a vector of zeros and ones.
#' 
#' @author Claude Boivin, Stat.ASSQ
#' @format An integer matrix with 6 rows and 10 columns.
#' \describe{
#'   \item{[1,c(1,2)]}{value = 0, not used}
#'   \item{[1,3:10]}{Identification numbers of the three variables. Column 3 to 6: variable 3; columns 7,8: variable 7, columns 9,10: variable 8}
#'   \item{nospec}{identification number of the specification}
#'   \item{m}{the value of the specification, a number between 0 and 1}
#'   \item{3}{1 if 3 is part of the specification, 0 otherwise}
#'   \item{2}{1 if 2 is part of the specification, 0 otherwise}
#'   \item{1}{1 if 1 is part of the specification, 0 otherwise}
#'   \item{0}{1 if 0 is part of the specification, 0 otherwise}
#'   \item{foul}{1 if foul is part of the specification, 0 otherwise}
#'   \item{fair}{1 if fair is part of the specification, 0 otherwise}
#'   \item{true}{1 if true is part of the specification, 0 otherwise}
#'   \item{false}{1 if false is part of the specification, 0 otherwise}
#'   }
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"swr"
#' 
#' The Captain's Problem. \code{swr}: Result of the evaluation of the Hypergraph at node Arrival (A)
#' 
#' This dataset is the \code{tt} bca resulting from the combination of the relations of the hypergraph and marginalization at node Arrival (A).
#'
#' @author Claude Boivin, Stat.ASSQ
#' @format A list of 8 elements, of class bcaspec.
#' @source {Almond, R.G. [1988] Fusion and Propagation in Graphical Belief Models. Computing Science and Statistics: Proceedings of the 20th Symposium on the Interface. Wegman, Edward J., Gantz, Donald T. and Miller, John J. (ed.). American Statistical Association, Alexandria, Virginia. pp 365--370.}
"captain_result"
