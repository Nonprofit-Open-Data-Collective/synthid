# 06e-people-networks.R
# Board members as bridges, and the inner circle, for one CSA.
#
# Input: 06c-csa-networks.R's collapsed meta-orgs and board seats (person =
# person rebuilt here from matches at score >= PERSON_TH, default 10). Only people on 2+ boards (meta-orgs) count.
#
# 1. Bridge rankings. Per person:
#      boards       meta-orgs whose board they sit on
#      sectors      distinct MAJ10 sectors among those boards (Unknown not counted)
#      cbsas        distinct CBSAs among those boards
#      sole_links   pairs of their organizations that no one else links (remove
#                   the person and these organizations no longer share anyone)
#      brokerage    betweenness in the person-organization network: how often
#                   they sit on the shortest path between two organizations
#    Ranked by brokerage; a dot chart for the top 25 and, for the top 10, a small
#    diagram of their boards and the other interlockers who share 2+ of them.
# 2. Inner circle. Two people tie only when they sit together on 2+ boards
#    (any years). Groups: Louvain communities on that network, labeled by the
#    organizations where most group members sit.
#
# Names individual board members, so everything is written to the data folder,
# not the repository: SYNTHID_DIR/partvii-v3_1/blocks/csa-test/csa=<CSA>/people/
#
#   Rscript dev/partvii-panel/06e-people-networks.R [csa=450]

source("dev/partvii-panel/_panel.R")
build_step("06e-people-networks")
suppressPackageStartupMessages({ library(igraph); library(ggplot2) })
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
TOP_RANK <- 25L; TOP_EGO <- 10L; MIN_GROUP <- 6L

d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
out <- file.path(d, "people"); dir.create(out, showWarnings = FALSE)
x <- readRDS(file.path(d, "meta-collapsed.rds")); mattr <- x$mattr; mseats <- x$mseats; persons <- x$persons
prof <- readRDS(file.path(d, "profiles.rds"))$prof
dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
xw <- unique(dl[`CSA Code` == CSA, .(cbsa = `CBSA Code`, cbsa_title = sub(", NC$", "", `CBSA Title`), csa_title = `CSA Title`)])

mode1 <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (!length(v)) NA_character_ else names(which.max(table(v))) }
persons[, name := paste(prof$first_name, prof$last_name)[match(EMP_ID, prof$EMP_ID)]]
pname <- persons[, .(name = tools::toTitleCase(tolower(mode1(name)))), by = person]
pname <- setNames(pname$name, pname$person)

mattr[, maj10 := fcase(maj12 == "UNI", "EDU", maj12 == "HOS", "HEL", default = maj12)]
SECPAL <- c(ART = "#e87ba4", EDU = "#2a78d6", ENV = "#008300", HEL = "#1baf7a", HMS = "#eb6834",
            IFA = "#6250d6", PSB = "#eda100", REL = "#e34948", MMB = "#6f6e69", UNU = "#c3c2b7")
SECTORS <- c(ART = "Arts", EDU = "Education", ENV = "Environment", HEL = "Health", HMS = "Human services",
             IFA = "International", PSB = "Public & societal benefit", REL = "Religion", MMB = "Mutual benefit",
             UNU = "Unknown NTEE")
SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; EDGE <- "#9b9a95"
fmt <- function(x) trimws(format(x, big.mark = ","))
base_theme <- theme_void(base_size = 9) +
  theme(plot.background = element_rect(fill = SURF, colour = NA),
        plot.title = element_text(colour = INK, face = "bold", size = 12, margin = margin(b = 3)),
        plot.subtitle = element_text(colour = INK2, size = 8.5, margin = margin(b = 8)),
        plot.caption = element_text(colour = INK2, size = 7, hjust = 0),
        legend.text = element_text(colour = INK2, size = 7.5), legend.title = element_text(colour = INK, size = 8),
        plot.margin = margin(12, 12, 12, 12))

# Person identity for this analysis: a stricter match than the organization
# graphs use. At score 7 an exact common first + last name with no middle
# initial (JOHN SMITH) scores ~8.8-9.2 and chains into one "person" sitting on
# dozens of boards -- a false super-bridge. Rare names and matches backed by a
# middle initial score 10+.
PERSON_TH <- as.numeric(Sys.getenv("PERSON_TH", "10"))
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet"), col_select = c("emp_a", "emp_b", "score", "csa")))
e <- cand[csa == TRUE & !is.na(score) & score >= PERSON_TH, .(emp_a, emp_b, score)]
rm(cand); invisible(gc())
xo <- as.data.table(synthid:::resolve_cross_org(as.data.frame(e), prof[prof$EMP_ID %in% c(e$emp_a, e$emp_b), , drop = FALSE]))
persons[, person := EMP_ID]
persons[xo, on = "EMP_ID", person := i.XORG_ID]
pname <- persons[, .(name = tools::toTitleCase(tolower(mode1(name)))), by = person]
pname <- setNames(pname$name, pname$person)
persons[, meta := x$META[org]]
mseats <- unique(persons[board == TRUE & !is.na(meta), .(meta, person)])
stamp("persons at score >= %g: %s cross-org persons", PERSON_TH, fmt(xo[XORG_N_ORGS > 1, uniqueN(XORG_ID)]))

S <- unique(mseats[, .(meta, person)])
S <- S[S[, .N, by = person][N >= 2], on = "person"][, N := NULL]
S[mattr, on = "meta", `:=`(maj10 = i.maj10, cbsa = i.cbsa, org = i.name)]
stamp("%s people on 2+ boards, %s seats, %s organizations", fmt(uniqueN(S$person)), fmt(nrow(S)), fmt(uniqueN(S$meta)))

# ---- 1. bridges ------------------------------------------------------------------------
pairs <- S[S, on = "person", allow.cartesian = TRUE, nomatch = 0][meta < i.meta, .(u1 = meta, u2 = i.meta, person)]
W <- pairs[, .(w = .N), by = .(u1, u2)]
pairs[W, on = c("u1", "u2"), w := i.w]
B <- S[, .(boards = .N, sectors = uniqueN(maj10[maj10 != "UNU"]), cbsas = uniqueN(cbsa)), by = person]
B[pairs[, .(sole_links = sum(w == 1)), by = person], on = "person", sole_links := i.sole_links]
B[is.na(sole_links), sole_links := 0L]

# person-organization network: betweenness of each person
g2 <- graph_from_data_frame(S[, .(paste0("p:", person), paste0("o:", meta))], directed = FALSE)
t0 <- Sys.time()
bt <- betweenness(g2, directed = FALSE, normalized = TRUE)
stamp("betweenness on %s nodes in %.1f min", fmt(vcount(g2)), as.numeric(Sys.time() - t0, units = "mins"))
bt <- bt[startsWith(names(bt), "p:")]
B[, brokerage := bt[paste0("p:", person)]]
B[, name := pname[person]]
B <- B[order(-brokerage)]
B[, rank := .I]
top_orgs <- S[, .(top_boards = paste(head(org[order(-mattr$board[match(meta, mattr$meta)])], 4), collapse = " | ")), by = person]
B[top_orgs, on = "person", top_boards := i.top_boards]
fwrite(B[1:200], file.path(out, "bridges-top200.csv"))
print(B[1:15, .(rank, name, boards, sectors, cbsas, sole_links, brokerage = signif(brokerage, 3))])

# dot chart: top 25 by brokerage, four measures side by side
T <- B[1:TOP_RANK]
T[, who := factor(sprintf("%s", name), levels = rev(unique(name)))]
long <- melt(T[, .(who, `Boards` = boards, `Sectors spanned` = sectors, `CBSAs spanned` = cbsas,
                   `Organization pairs only they link` = sole_links, `Brokerage (betweenness x 1,000)` = round(1000 * brokerage, 2))],
             id.vars = "who", variable.factor = TRUE)
p1 <- ggplot(long, aes(x = value, y = who)) +
  geom_segment(aes(x = 0, xend = value, yend = who), colour = "#c3c2b7", linewidth = 0.5) +
  geom_point(colour = "#2a78d6", size = 2.2) +
  geom_text(aes(label = ifelse(value == round(value), format(round(value), big.mark = ","), sprintf("%.1f", value))),
            hjust = -0.45, size = 2.3, colour = INK2) +
  facet_wrap(~variable, nrow = 1, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = sprintf("%s: the 25 board members who bridge the most organizations", xw$csa_title[1]),
       subtitle = sprintf("Ranked by brokerage: how often a person sits on the shortest path between two organizations in the person-organization board network (%s people on 2+ boards).",
                          fmt(uniqueN(S$person))),
       caption = paste0("Board members matched across filings at score >= ", PERSON_TH, "; meta-orgs collapsed; boards counted over 2010-2024. A wrongly merged common name can create a false bridge: check names before use."),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(plot.background = element_rect(fill = SURF, colour = NA), panel.grid = element_blank(),
        strip.text = element_text(colour = INK, face = "bold", size = 8, hjust = 0),
        axis.text.x = element_blank(), axis.text.y = element_text(colour = INK, size = 8),
        plot.title = element_text(colour = INK, face = "bold", size = 12),
        plot.subtitle = element_text(colour = INK2, size = 8.5), plot.caption = element_text(colour = INK2, size = 7, hjust = 0),
        plot.margin = margin(12, 12, 12, 12))
ggsave(file.path(out, sprintf("csa%s_6-bridges.png", CSA)), p1, width = 14, height = 7.5, dpi = 220, bg = SURF)

# small board diagrams for the top 10
EGO_N <- list(); EGO_E <- list()
for (i in seq_len(TOP_EGO)) {
  p <- B$person[i]; bd <- S[person == p][order(maj10, org)]
  n <- nrow(bd); a <- pi / 2 - 2 * pi * (seq_len(n) - 1) / n
  boards <- data.table(panel = i, kind = "board", id = bd$meta, x = cos(a), y = sin(a), maj10 = bd$maj10,
                       label = substr(bd$org, 1, 26), ang = a)
  co <- S[meta %in% bd$meta & person != p, .(k = .N, ms = list(meta)), by = person][k >= 2]
  EGO_E[[length(EGO_E) + 1]] <- data.table(panel = i, x = 0, y = 0, xend = boards$x, yend = boards$y, kind = "self")
  if (nrow(co)) {
    cxy <- co[, {m <- match(unlist(ms), boards$id); .(x = mean(boards$x[m]) * 0.55, y = mean(boards$y[m]) * 0.55)}, by = person]
    set.seed(i); cxy[, `:=`(x = x + runif(.N, -0.07, 0.07), y = y + runif(.N, -0.07, 0.07))]
    EGO_N[[length(EGO_N) + 1]] <- data.table(panel = i, kind = "co", id = cxy$person, x = cxy$x, y = cxy$y,
                                             maj10 = NA_character_, label = "", ang = NA_real_)
    ce <- co[, .(meta = unlist(ms)), by = person][cxy, on = "person"]
    EGO_E[[length(EGO_E) + 1]] <- ce[, .(panel = i, x, y, xend = boards$x[match(meta, boards$id)],
                                         yend = boards$y[match(meta, boards$id)], kind = "co")]
  }
  EGO_N[[length(EGO_N) + 1]] <- boards
  EGO_N[[length(EGO_N) + 1]] <- data.table(panel = i, kind = "self", id = p, x = 0, y = 0, maj10 = NA_character_, label = "", ang = NA_real_)
}
EN <- rbindlist(EGO_N); EE <- rbindlist(EGO_E)
strip <- B[1:TOP_EGO, sprintf("%d. %s\n%d boards | %d sectors | %d CBSAs", rank, name, boards, sectors, cbsas)]
EN[, panel_f := factor(strip[panel], strip)]; EE[, panel_f := factor(strip[panel], strip)]
bn <- EN[kind == "board"]
bn[, `:=`(lx = 1.12 * cos(ang), ly = 1.12 * sin(ang), hj = ifelse(abs(cos(ang)) < 0.2, 0.5, ifelse(cos(ang) > 0, 0, 1)))]
bn[, maj10 := factor(maj10, names(SECPAL))]
p2 <- ggplot() +
  geom_segment(data = EE[kind == "co"], aes(x, y, xend = xend, yend = yend), colour = EDGE, alpha = 0.35, linewidth = 0.2) +
  geom_segment(data = EE[kind == "self"], aes(x, y, xend = xend, yend = yend), colour = INK2, linewidth = 0.45) +
  geom_point(data = EN[kind == "co"], aes(x, y), colour = "#9b9a95", size = 0.7) +
  geom_point(data = bn, aes(x, y, fill = maj10), shape = 21, colour = SURF, size = 3.2, stroke = 0.3) +
  geom_point(data = EN[kind == "self"], aes(x, y), shape = 21, fill = INK, colour = SURF, size = 3.6) +
  geom_text(data = bn, aes(lx, ly, label = label, hjust = hj), size = 1.75, colour = INK) +
  scale_fill_manual(values = SECPAL, labels = SECTORS, drop = TRUE, name = "board's sector") +
  facet_wrap(~panel_f, nrow = 2) +
  coord_equal(xlim = c(-2.1, 2.1), ylim = c(-1.35, 1.35), clip = "off") +
  guides(fill = guide_legend(override.aes = list(size = 3), nrow = 1)) +
  labs(title = sprintf("%s: the boards of the 10 strongest bridges", xw$csa_title[1]),
       subtitle = "Black: the person. Colored ring: their boards. Gray dots inside: other board members who share 2+ of those boards, drawn between the boards they share.",
       caption = paste0("Matches at score >= ", PERSON_TH, "; meta-orgs collapsed; boards over 2010-2024.")) +
  base_theme + theme(strip.text = element_text(colour = INK, size = 8, face = "bold", margin = margin(b = 2)),
                     legend.position = "bottom", panel.spacing = unit(1.2, "lines"))
ggsave(file.path(out, sprintf("csa%s_6-bridges-boards.png", CSA)), p2, width = 20, height = 10, dpi = 200, bg = SURF)

# ---- 2. inner circle ------------------------------------------------------------------
pp <- S[, .(meta, person)]
pp <- pp[pp, on = "meta", allow.cartesian = TRUE, nomatch = 0][person < i.person, .(a = person, b = i.person)][, .(shared = .N), by = .(a, b)][shared >= 2]
gi <- graph_from_data_frame(pp[, .(a, b, weight = shared)], directed = FALSE)
set.seed(1); com <- cluster_louvain(gi)
V(gi)$group <- membership(com)
gs <- table(V(gi)$group)
keep <- as.integer(names(gs)[gs >= MIN_GROUP])
gk <- induced_subgraph(gi, which(V(gi)$group %in% keep))
stamp("inner circle: %s people with a 2+ board tie (%s ties); %d groups of %d+ people hold %s of them",
      fmt(vcount(gi)), fmt(ecount(gi)), length(keep), MIN_GROUP, fmt(vcount(gk)))

G <- rbindlist(lapply(keep, function(k) {
  m <- V(gi)$name[V(gi)$group == k]
  sk <- S[person %in% m]
  orgs <- sk[, .N, by = .(meta, org, maj10, cbsa)][order(-N)]
  h <- induced_subgraph(gi, m)
  data.table(group = k, people = length(m), ties = ecount(h), density = round(edge_density(h), 3),
             mean_shared_boards = round(mean(E(h)$weight), 2),
             top_orgs = paste(sprintf("%s (%d)", head(orgs$org, 4), head(orgs$N, 4)), collapse = " | "),
             main_sector = SECTORS[[{ ms <- mode1(sk$maj10[sk$maj10 != "UNU"]); if (is.na(ms)) "UNU" else ms }]],
             main_cbsa = xw$cbsa_title[match(mode1(sk$cbsa), xw$cbsa)],
             members = paste(head(pname[m][order(-degree(h))], 8), collapse = "; "))
}))
G <- G[order(-people)]
G[, gid := sprintf("G%d", .I)]
fwrite(G, file.path(out, "inner-circle-groups.csv"))
print(G[, .(gid, people, density, main_sector, main_cbsa, top_orgs = substr(top_orgs, 1, 90))][1:min(15, .N)])

gmap <- setNames(G$gid, G$group)
V(gk)$gid <- gmap[as.character(V(gk)$group)]
top8 <- G$gid[1:min(8, nrow(G))]
PAL8 <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")[seq_along(top8)], top8)
# Draw the largest connected group only; far-off small pieces would squeeze it
# into a dot. Every group is still in inner-circle-groups.csv.
cmk <- components(gk)
gk_all <- vcount(gk)
gk <- induced_subgraph(gk, which(cmk$membership == which.max(cmk$csize)))
top8 <- intersect(G$gid, unique(V(gk)$gid))[1:min(8, length(unique(V(gk)$gid)))]
PAL8 <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")[seq_along(top8)], top8)
V(gk)$col <- ifelse(V(gk)$gid %in% top8, V(gk)$gid, "other")
set.seed(42); L <- if (vcount(gk) <= 4000) layout_with_fr(gk, weights = E(gk)$weight) else layout_with_drl(gk)
nd <- data.table(name = V(gk)$name, gid = V(gk)$gid, col = V(gk)$col, x = L[, 1], y = L[, 2], deg = degree(gk))
el <- as_edgelist(gk)
ed <- data.table(x = nd$x[match(el[, 1], nd$name)], y = nd$y[match(el[, 1], nd$name)],
                 xend = nd$x[match(el[, 2], nd$name)], yend = nd$y[match(el[, 2], nd$name)], w = E(gk)$weight)
gl <- nd[gid %in% top8, .(x = median(x), y = median(y)), by = gid][G, on = "gid", nomatch = 0]
gl[, text := sprintf("%s: %s people, %s\n%s", gid, people, main_cbsa,
                     vapply(strsplit(gsub(" \\(\\d+\\)", "", top_orgs), " \\| "),
                            function(o) paste(substr(head(o, 2), 1, 32), collapse = "\n"), ""))]
p3 <- ggplot() +
  geom_segment(data = ed, aes(x, y, xend = xend, yend = yend, linewidth = w), colour = EDGE, alpha = 0.25) +
  scale_linewidth(range = c(0.12, 1.2), name = "boards shared", breaks = c(2, 3, 5)) +
  geom_point(data = nd, aes(x, y, fill = col, size = deg), shape = 21, colour = SURF, stroke = 0.15) +
  scale_fill_manual(values = c(PAL8, other = "#c3c2b7"), breaks = top8, name = "largest groups") +
  scale_size_area(max_size = 3.5, name = "inner-circle ties") +
  ggrepel::geom_label_repel(data = gl, aes(x, y, label = text), size = 2.3, colour = INK, fill = SURF,
                            label.size = 0.2, label.r = unit(0.1, "lines"), max.overlaps = Inf, seed = 1,
                            box.padding = 0.8, segment.colour = INK2, segment.size = 0.3, lineheight = 0.95) +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  labs(title = sprintf("%s: the inner circle of board members", xw$csa_title[1]),
       subtitle = sprintf("Two people are tied only when they sit together on 2+ boards. Shown: the largest connected group, %s of the %s people in %d groups of %d+ (Louvain communities).\nThe 8 largest groups are colored and labeled by the boards most of their members share.",
                          fmt(vcount(gk)), fmt(gk_all), length(keep), MIN_GROUP),
       caption = paste0("Matches at score >= ", PERSON_TH, "; meta-orgs collapsed, so the entities of one organization count as one board; boards over 2010-2024. Group details in inner-circle-groups.csv.")) +
  base_theme
ggsave(file.path(out, sprintf("csa%s_7-inner-circle.png", CSA)), p3, width = 12, height = 10, dpi = 220, bg = SURF)
stamp("people figures in %s", out)
