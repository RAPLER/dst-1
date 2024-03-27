# dst 1.6.0

This version contains many improvements on earlier versions, namely functions to help construct bca parameters, partition of description (tt) matrices, improve numerical stability and other functions to help work with large mass functions.

* New function: ttmatrixPartition: construct partition matrix 

* New functions: ttmatrixFromMarginal, mFromMarginal: construct bca parameters from a vector of probabilities

* New functions: nzdsrLogsumexp, dsrwonLogsumexp, belplauLogsumexp, belplauHLogsumexp, logsum: using logsumexp for nzdsr, dsrwon, belplau, improving numerical stability

* New function: belplauEval: evaluate type I, II, III error

* New function: bcaNorm: evaluate Lp norm between two bcas

* new function: intersBySSName: Intersect two vectors of subsets names. Utility function called when using subsets names instead of intersecting description mmatrices to obtain combinations.

* New function: bcaPrintLarge: print bca statistics with a large number of subsets

# dst 1.5.1.9011

* Function bcaTrunc: Parameter "use_ssnames" added to allow use of subsets names to do truncation instead of description (tt) matrix.

* Function bca: parameter ssnames is reinstalled in the function call

* Name of function ssnnames changed for DoSSnames to avoid confusion with parameter ssnames oof bca object.

* Added a `NEWS.md` file to track changes to the package.

## Changes and additions
* Function nzdsr: Call to function ttmatrix to reconstruct description matrix from subsets names

* New function: ttmatrix: Construct a description matrix from a list of subsets names.

* New function: belplauH:  Efficient computation of the measures of belief, disbelief and plausibility.

* Function belplau: Modified to call belplauH (computation more efficient).

## Changes to function dsrwon :
* Parameter sparseM removed. Not necessary to specify this. Replaced byy parametter use_ssnames.

* New parameter: use_ssnames = c(TRUE, FALSE). Default = FALSE. use "TRUE" when you want to work specifically with subsets names to do the intersections. Work only with the univariate frame of discernment. Does'nt work yet with product spaces.

* New parameter: skpt_tt = c(FALSE, TRUE). Default = FALSE". Choose TRUE if you don't want to save this table. Useful if you apply the function repeatedly, and you want to save the last result only.

## Function bca.
* Parameter ssnames added.
* test for duplicate column names added

# dst 1.5.1.9010
* Same as 9009

# dst 1.5.1.9009
*  Same as .9008

# dst 1.5.1.9008

* Temporarily removed the possibility of using subsets label (parameter ssnames) instead of a description matrix (parameter tt). This possibility will retested later.

# dst 1.5.1.9007

* addTobca: A parameter "status" has been added to the "spec" of the bca in order to distinguish the new subsets added from the ones already present (documentation to be updated with an example).

* bca: parametetrs ssnames and sfod removed. It is not necessary to supply subsets names. They are obtained from the columns names of tt (0,1) matrix. See the new function "ssnames" designed for that.

* dsrwon: correction to call to dst package for parallel processing.

* nameRows: Allow sparse matrix as input. Removed stopping the function if there is no column names.Generate column names instead.

## nzdsr:  
*  A call to function "dsrwon" has been removed to speed up the function. If the empty set is in first position in the matrix of subsets, as it should be, the normalization wil be done correctly.

*  Removed the test with the conflict indice. This test would stop the function. The conflict indice does not play a role in the combination by Dempster's rule. This is only a decision aid in the analysis of conflicting evidence

* Added the possibility of using subsets label (parameter ssnames) instead of a description matrix (parameter tt).

# dst 1.5.1.9006

* ssnames: New function. Construct subsets names from the columns names of tt matrix. Subsets names will be used to do intersections instead of (0,1) matrices.

* dsrwon. New version allowing the use of sparse matrices. use of subset names to do intetrsections.

* dotprod, inters nameRows. Update of functions to allow the use of sparse matrices

# dst 1.5.2

Version 1.5.1.9004 published 2023-11-16 on CRAN tagged 1.5.2

# dst 1.5.1.9004

## nzdsr: 
* 2023-04-17.  Added another missing specification to variable vacuous

* 2023-04-01. Added a missing specification to variable vacuous.

## bcaTrunc: 
* 2023-01-08. New. Function to allow truncation of a belief function containing a large number of focal elements to a threshold value.

## bcaRel, dsrwon, elim, peeling: 
* Removed a recursive function and simplified code. 

## tabresul: 
* 2023-01-03. Function revised. Removed a lag between row labels and data

## dotprod: 
* 2023-01-04. Check to input parameters added.

## dsrwon: 
* Added parallel computing capability, using multiple cores.

# dst 1.5.1

* addTobca: 2021-12-13. Added a check to remove duplicate subsets.

* tabresul: 2021-12-14. Correction to the ordering of the three last rows of the table of results.

* Vignette Zadeh's Example: 2021-12-21. Done with the peeling algorithm.

# dst 1.5.0

*  Functions revised. All functions : code simplification and documentation
* Functions revised. All functions : code simplification and documentation

* dsrwon: 2021-04-23. Calculation of the measure of conflict revised to take into account the case of the combination with the vacuous belief function. This case is used at the beginning of nzdsr (normalization) to update the I12 matrix.

* elim: 2021-05-30: example revised; 2021-07-06: call to fn matrixToMarray changed.  Call to new fn "nameCols_prod"

* extmin: 2021-08-19: call to fn matrixToMarray changed.

* marrayToMatrix: 2021-06-29. Function call changed. Call to new fn "nameCols_prod".

* matrixToMarray: 2021-06-18. Function call changed

* nameCols: 2021-06-28. Function call changed

* nameCols_prod: 2021-06-28. New function. Used in functions extmin and elim.

* tabresul: 2021-07-30. Sort the results in order to always show the frame at the last line of the table

* peeling: New. 2021-09-06. Compute the belief function of a variable of interest by successive elimination of all the other variables of an hypergraph.

# dst 1.4.1.9002

* Correction to function plautrans: The check for the presence of the empty set did not work properly.

# dst v1.2.1.9000

* function extmin: If there is no missing variable, simply take the input as the result.

# dst v1.4.1
*  version 1.4.1 published on CRAN 2020-03-28. As of 2020-02-23

* added utility function bcaPrint for printing of the subsets of non-zero  mass of a belief function.

## added examples vignettes: 
* Zadeh's example, 
* Captain's problem

* function dsrwon: added a check on values names to ensure that the two bca's combined are defined on the same space.

* function extmin: added checks on names of variables, as well as their number and their position in a product space

# dst 1.4.0
* version 1.4.0 published on CRAN 2019-08-20
* Correction to fn extmin: Added a test for variable names missing.

* Correction to productSpace function. Transpose of array removed at the end of the function.

# dst v 1.3.0 (on CRAN 2018-12-05)
* added utility functions 'matrixToMarray' and 'marrayToMatrix' to execute the product space conversion of multidimensional data represented by a matrix, and vice-versa.

* debugging of function 'extmin' (extension of data to a larger product space).

* debugging of function 'elim' (reduction of a product space by elimination of one variable).

# dst v1.2.0.9001
* 2018-07-07. 
* Correction to fn dsrwon: added the parameter relnb in the call of the function, to allow for specification the result with a new relation number.

# dst v1.2.0.9000

dst v1.1.0.9000
* 2018-05-01. 

* Correction to fn belplau: added a check for the case of the empty set present with m_empty = 0
- Re-writed the referencing of source code taken on Rpubs

# dst v1.0.0 (Release date on CRAN: 2018-04-25)

* Added a vignette: The Monty hall Game, an introduction to belief functions.

## functions (new and updated):

* addTobca: New function. Adds some elements of 0 mass to an existing mass function.

*  bca: New version. Sets a class named "bcaspec". Parameters added to work with definitions on product spaces (relations).

* 2018-05-01. 
* Correction to fn tabresul: added a check for the case of the empty set present with m_empty = 0

# dst v1.1.0.9000
* 2018-05-01. 
* Correction to fn belplau: added a check for the case of the empty set present with m_empty = 0
* Re-writed the referencing of source code taken on Rpubs

# dst v1.0.0 (Release date on CRAN: 2018-04-25)

* Changes from version 0.3:
* Added a vignette: The Monty hall Game, an introduction to belief functions.

* functions (new and updated):

* addTobca: New function. Adds some elements of 0 mass to an existing mass function.

* bca: New version. Sets a class named "bcaspec". Parameters added to work with definitions on product spaces (relations).

* bcaRel: New function to define a belief function on a product space.

* belplau: Calculation of measures of belief, plausibility and ratio of plausibility.

* decode: utility function
* dotprod: utility function
* double: utility function
* shape: utility function
* encode: New utility function. Convert a value to its representation in another chosen base.
* reduction: utility function to obtain the summary of a vector for any operator

* dsrwon: Combination of two mass functions

* elim: This is a new function. This function works on a relation defined on a product of two variables or more.  Having fixed a variable to eliminate from the relation,  the reduced product space is determined and the corresponding reduced bca is computed.This operation is also called "marginalization".

* extmin: Extension of a relation to a greater product space

* inters: Intersection of two tables of propositions

* nameRows: New function: Using the column names of a matrix to construct names for the rows

* nzdsr: Normalization of results from Dempster's rule of combination.

* productSpace: New function. Product space representation of a relation

* plautrans:Plausibility transformation of the singletons of a frame

* tabresul:  Prepare a table of results.

* Removal of obsolete functions:

butLast, combmasses, dempster, initsing, rplau, transfo

# dst v0.3: Initial release of package on CRAN (2015-01-13)
