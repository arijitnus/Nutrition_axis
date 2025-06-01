install.packages('GA')
library(GA)

gene_df<-read_excel('/Users/arijitm/Downloads/Genomes_info.xlsx',sheet = "one_replaced",
                    col_names = T,skip = 0)
metadata<-read_excel('/Users/arijitm/Downloads/Genomes_info.xlsx',sheet = "metdata",
                     col_names = T,skip = 0)
gene_df<-as.data.frame(gene_df)
metadata<-as.data.frame(metadata)
rownames(gene_df)<-gene_df$Bacteria
gene_df<-gene_df[,-1]

meta_df <- metadata[metadata$Bacteria %in% rownames(gene_df), ]

proteo <- meta_df$Bacteria[meta_df$Phylum == "Proteobacteria"]
actino <- meta_df$Bacteria[meta_df$Phylum == "Actinobacteria"]
firmi  <- meta_df$Bacteria[meta_df$Phylum == "Firmicutes"]

# === FITNESS FUNCTION ===
calculate_gene_coverage <- function(selected_bacteria) {
  sum(colSums(gene_df[selected_bacteria, , drop = FALSE]) > 0)
}

fitness_func_factory <- function(proteo, actino, firmi) {
  function(x) {
    x <- round(x)
    i_prot <- unique(x[1:8])
    i_act  <- unique(x[9:13])
    i_firm <- unique(x[14:18])
    
    if (length(i_prot) < 8 || length(i_act) < 5 || length(i_firm) < 5) return(-1e6)
    
    sel_bacteria <- unique(c(
      proteo[i_prot],
      actino[i_act],
      firmi[i_firm]
    ))
    
    if (length(sel_bacteria) < 18) return(-1e6)
    
    -calculate_gene_coverage(sel_bacteria)  # GA maximizes
  }
}

# === MULTI-RUN OPTIMIZATION ===

top_solutions <- list()
top_scores <- numeric()

for (i in 1:10) {
  cat("Running GA round", i, "...\n")
  
  ga_res <- ga(
    type = "real-valued",
    fitness = fitness_func_factory(proteo, actino, firmi),
    lower = c(rep(1, 18)),
    upper = c(rep(length(proteo), 8), rep(length(actino), 5), rep(length(firmi), 5)),
    popSize = 150,
    maxiter = 300,
    run = 100,
    seed = i * 100 + 7,
    pmutation = 0.2
  )
  
  sol_idx <- round(ga_res@solution[1, ])
  i_prot <- unique(sol_idx[1:8])
  i_act  <- unique(sol_idx[9:13])
  i_firm <- unique(sol_idx[14:18])
  
  sel_bacteria <- unique(c(
    proteo[i_prot],
    actino[i_act],
    firmi[i_firm]
  ))
  
  if (length(sel_bacteria) == 18) {
    score <- calculate_gene_coverage(sel_bacteria)
    
    # Avoid duplicates
    if (!(any(sapply(top_solutions, function(x) identical(sort(x), sort(sel_bacteria)))))) {
      top_solutions[[length(top_solutions) + 1]] <- sel_bacteria
      top_scores <- c(top_scores, score)
    }
  }
  
  if (length(top_solutions) >= 3) break
}

# === FINAL OUTPUT ===
sorted_indices <- order(top_scores)
top_solutions <- top_solutions[sorted_indices]
top_scores <- top_scores[sorted_indices]

for (i in 1:length(top_solutions)) {
  cat(sprintf("\n--- Top Solution #%d ---\n", i))
  cat("KEGG genes covered:", top_scores[i], "\n")
  print(top_solutions[[i]])
}



