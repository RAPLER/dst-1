buildTrees <- function(tt, qq) {
  card <- Matrix::rowSums(tt)
  sort_order <- order(card)
  card_s <- card[sort_order]
  card_nodup <- card_s[!duplicated(card_s)]
  
  trees <- list()
  
  for (i in 1:length(card_nodup)) {
    
    idx <- (card==card_nodup[i])
    trees[[i]] <- buildTree(tt[idx,],qq[idx])
    
  }
  trees$card_nodup <- card_nodup
  return(trees)
}