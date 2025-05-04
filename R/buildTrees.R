buildTrees <- function(tt, qq) {
  card <- Matrix::rowSums(tt)
  sort_order <- order(card)
  card_s <- card[sort_order]
  card_nodup <- card_s[!duplicated(card_s)]
  
  trees <- list()
  
  for (i in seq_along(card_nodup)) {
    idx <- which(card == card_nodup[i])
    tt_sub <- tt[idx, , drop = FALSE]
    qq_sub <- qq[idx]
    
    # Pass the original indices explicitly
    trees[[i]] <- if (card_nodup[i] == 0) buildTree(tt_sub, qq, indices = idx)[[1]] else buildTree(tt_sub, qq, indices = idx)
  }
  
  trees$card_nodup <- card_nodup
  return(trees)
}
