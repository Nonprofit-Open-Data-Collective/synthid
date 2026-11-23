# Variant chains: the network the first-name variant table forms.
#
# Each pair in first-name-variants.csv is an edge between two names. A chain is
# a connected component: names linked to each other through one or more
# pairs. KATE links KATHLEEN and KATHERINE, so the three form one chain even
# though KATHLEEN/KATHERINE is not a pair (it is in the questionable file).
# Chains show how far a match would spread if the comparator followed links
# transitively, which it must not do.
#
# Adds to first-name-variants.csv:
#   chain_id     component id (1 = largest)
#   chain_names  names in the chain
#   chain_pairs  pairs (edges) in the chain
#   chain_unlinked  name pairs in the chain that are NOT a listed pair, i.e.
#                   the matches a transitive rule would add
# Writes dev/name-variants/first-name-variant-chains.csv (one row per chain)
# and vignettes/figures/name-chains-*.png. Run from the package root after
# 02-classify.R.
suppressMessages({library(data.table); library(igraph); library(ggraph); library(ggplot2)})
f <- "dev/name-variants/first-name-variants.csv"
t <- fread(f)
t[, intersect(names(t), c("chain_id", "chain_names", "chain_pairs", "chain_unlinked")) := NULL]
# chains are built from the matching lexicon only; rows outside it (added by
# 06-lexicon.R) keep empty chain columns
rest <- if ("lexicon" %in% names(t)) t[lexicon == FALSE] else t[0]
if ("lexicon" %in% names(t)) t <- t[lexicon == TRUE]

g <- graph_from_data_frame(t[, .(name_short, name_long, type, n_emp_ids)], directed = FALSE)
cmp <- components(g)
ord <- order(-cmp$csize)                        # renumber: 1 = largest chain
rank <- match(seq_along(cmp$csize), ord)
V(g)$chain <- rank[cmp$membership]

ch <- rbindlist(lapply(seq_along(ord), function(k) {
  vs <- V(g)[chain == k]; sg <- induced_subgraph(g, vs)
  n <- vcount(sg); e <- ecount(sg)
  data.table(chain_id = k, chain_names = n, chain_pairs = e,
             chain_unlinked = n * (n - 1) / 2 - e,
             diameter = if (n > 1) diameter(sg, weights = NA) else 0,
             emp_ids = sum(E(sg)$n_emp_ids),
             hub = names(which.max(degree(sg))),
             names = paste(sort(V(sg)$name), collapse = " "))
}))
t[, chain_id := V(g)$chain[match(name_short, V(g)$name)]]
t <- ch[, .(chain_id, chain_names, chain_pairs, chain_unlinked)][t, on = "chain_id"]
setcolorder(t, setdiff(names(t), c("chain_id", "chain_names", "chain_pairs", "chain_unlinked")))
fwrite(rbind(t[order(-n_emp_ids)], rest, fill = TRUE)[, names(t), with = FALSE], f)
fwrite(ch, "dev/name-variants/first-name-variant-chains.csv")

cat("Names:", vcount(g), " pairs:", ecount(g), " chains:", nrow(ch), "\n")
print(ch[, .(chains = .N, names = sum(chain_names)), keyby = .(size = fcase(chain_names == 2, "2 (one pair)",
          chain_names <= 4, "3-4", chain_names <= 9, "5-9", default = "10+"))])
cat("\nTop 10 chains:\n"); print(ch[1:10, .(chain_id, chain_names, chain_pairs, chain_unlinked, diameter, hub)])

# ---- figures: the five largest chains ------------------------------------
dir.create("vignettes/figures", showWarnings = FALSE)
type_col <- c(nickname = "#1f6feb", spelling = "#2da44e", cognate = "#bf8700", compound = "#8250df")
plot_chain <- function(k, file, width = 9, height = 7) {
  sg <- induced_subgraph(g, V(g)[chain == k])
  V(sg)$deg <- degree(sg)
  set.seed(k)
  p <- ggraph(sg, layout = "stress") +
    geom_edge_link(aes(colour = type, width = n_emp_ids), alpha = 0.7) +
    scale_edge_colour_manual(values = type_col, name = "Pair type") +
    scale_edge_width(range = c(0.3, 3), trans = "sqrt", name = "EMP_IDs") +
    geom_node_point(aes(size = deg), colour = "grey25", fill = "white", shape = 21, stroke = 0.6) +
    scale_size(range = c(1.5, 6), guide = "none") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3, family = "sans") +
    labs(title = sprintf("Chain %d: %d names, %d pairs", k, vcount(sg), ecount(sg)),
         subtitle = sprintf("Hub: %s. %d name pairs in the chain are not listed pairs.",
                            ch$hub[k], ch$chain_unlinked[k])) +
    theme_graph(base_family = "sans") + theme(legend.position = "bottom")
  ggsave(file, p, width = width, height = height, dpi = 110, bg = "white")
}
for (k in 1:5) plot_chain(k, sprintf("vignettes/figures/name-chains-%d.png", k))

# chain size distribution
p <- ggplot(ch, aes(chain_names)) + geom_histogram(binwidth = 1, fill = "#1f6feb") +
  scale_x_continuous(trans = "log2", breaks = c(2, 3, 4, 8, 16, 32, 64, 128)) +
  labs(x = "Names in the chain (log scale)", y = "Chains", title = "Most chains are a single pair") +
  theme_minimal(base_size = 12)
ggsave("vignettes/figures/name-chains-sizes.png", p, width = 7, height = 4, dpi = 110, bg = "white")
cat("\nWrote figures to vignettes/figures/\n")
