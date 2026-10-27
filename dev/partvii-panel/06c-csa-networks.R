# 06c-csa-networks.R
# Board-interlock networks for one CSA, from the 06b-csa-test.R pairs.
#
# 1. Cross-org persons: 06b's surname + CSA pairs at score >= THRESHOLD,
#    resolved into one id per person (synthid's resolve_cross_org()).
# 2. Board seats: (organization, person) for every board role, with years.
# 3. Meta-orgs: one organization filed as several legal entities with the same
#    governance team. Two entities are linked when they share >= MIN_SHARED board
#    members AND those shared members are most of both boards (Jaccard >= JACCARD,
#    boards counted over all years); a meta-org is a connected set of such links.
#    Reports how much of the interlock network sits inside meta-orgs, then
#    collapses each meta-org to one node.
# 4. Three networks, for the CSA and for each of its four largest CBSAs (each
#    built from that geography's organizations only):
#      orgs    organizations sharing 2+ board members, colored by NTEE MAJ12:
#              the core only (largest connected group, then the smallest k-core
#              with <= CORE_MAX organizations), sized by eigenvector centrality
#      groups  the 25 NTEE major groups A-Y in a circle (Z / UNDEFINED left
#              out); an edge is the density of organization pairs across two
#              groups sharing 1+ board member
#      people  board members who sit on 2+ boards; a tie is two of them on the
#              same board in the same year
#
# Writes dev/partvii-panel/csa-test-<CSA>/: <geo>_1-orgs.png, <geo>_2-groups.png,
#   <geo>_3-people.png, meta-orgs.csv, meta-share.csv, networks.csv
#
#   Rscript dev/partvii-panel/06c-csa-networks.R [csa=450] [threshold=7]

source("dev/partvii-panel/_panel.R")
build_step("06c-csa-networks")
suppressPackageStartupMessages({ library(igraph); library(ggraph); library(ggplot2) })
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
TH  <- if (length(args) >= 2) as.numeric(args[2]) else 7
MIN_SHARED <- 3L
JACCARD <- 0.5
CORE_MAX <- 160L          # orgs graph: smallest k-core with at most this many organizations
core_sizes <- list()      # k-core size at every k, per geography -> core-sizes.csv

d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA)); dir.create(fig, showWarnings = FALSE)
prof <- readRDS(file.path(d, "profiles.rds"))$prof
cand <- as.data.table(read_parquet(file.path(d, "pairs.parquet")))
onames <- as.data.table(read_parquet(file.path(pv_root, "orgs", "orgs.parquet"), col_select = c("ein", "org.name")))
onames <- setNames(onames$org.name, onames$ein)
dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
xw <- unique(dl[`CSA Code` == CSA, .(cbsa = `CBSA Code`, cbsa_title = `CBSA Title`, csa_title = `CSA Title`)])
CBSAS <- xw$cbsa

# ---- 1. cross-org persons -----------------------------------------------------------
e <- cand[csa == TRUE & !is.na(score) & score >= TH, .(emp_a, emp_b, score)]
xo <- as.data.table(synthid:::resolve_cross_org(as.data.frame(e), prof[prof$EMP_ID %in% c(e$emp_a, e$emp_b), , drop = FALSE]))
person <- setNames(prof$EMP_ID, prof$EMP_ID); person[xo$EMP_ID] <- xo$XORG_ID
stamp("%s edges at score >= %g -> %s cross-org persons", format(nrow(e), big.mark = ","), TH,
      format(xo[XORG_N_ORGS > 1, uniqueN(XORG_ID)], big.mark = ","))

mode1 <- function(v) { v <- v[!is.na(v) & nzchar(v)]; if (!length(v)) NA_character_ else names(which.max(table(v))) }
P <- data.table(EMP_ID = prof$EMP_ID,
                org = vapply(prof$orgs, function(o) if (length(o)) o[[1]] else NA_character_, ""),
                board = prof$is_board %in% TRUE,
                years = prof$years,
                ntee = vapply(prof$ntee, mode1, ""),
                cbsa = vapply(prof$cbsa, function(v) mode1(v[v %in% CBSAS]), ""))
P[, person := person[EMP_ID]]
org_attr <- P[, .(ntee = mode1(ntee), cbsa = mode1(cbsa), people = .N), by = org]

# ---- 2. board seats ------------------------------------------------------------------
seats <- P[board == TRUE, .(years = list(sort(unique(unlist(years))))), by = .(org, person)]
bsize <- seats[, .N, by = org]; bsize <- setNames(bsize$N, bsize$org)

org_pairs <- function(s, unit) {   # unit pairs sharing board members: w = shared persons
  ip <- s[, .(k = uniqueN(get(unit))), by = person][k >= 2, person]
  x <- unique(s[person %in% ip, .(u = get(unit), person)])
  p <- x[x, on = "person", allow.cartesian = TRUE, nomatch = 0][u < i.u, .(u1 = u, u2 = i.u, person)]
  p[, .(w = uniqueN(person)), by = .(u1, u2)]
}
W <- org_pairs(seats, "org")
W[, `:=`(s1 = bsize[u1], s2 = bsize[u2])]
W[, `:=`(jaccard = w / (s1 + s2 - w), overlap = w / pmin(s1, s2))]

# ---- 3. meta-orgs --------------------------------------------------------------------
meta_of <- function(links) {
  g <- graph_from_data_frame(links[, .(u1, u2)], directed = FALSE, vertices = data.frame(name = org_attr$org))
  cm <- components(g)$membership
  # name each meta-org by its member with the largest board
  m <- data.table(org = names(cm), comp = cm, b = bsize[names(cm)]); m[is.na(b), b := 0L]
  m[order(-b), meta := org[1], by = comp]
  setNames(m$meta, m$org)
}
share_row <- function(rule, meta) {
  inside <- meta[W$u1] == meta[W$u2]
  ip <- seats[, .(k = uniqueN(org), km = uniqueN(meta[org])), by = person][k >= 2]
  data.table(rule = rule,
             meta_orgs = sum(table(meta) > 1), entities_in_meta = sum(table(meta)[table(meta) > 1]),
             ties_1plus = nrow(W), ties_1plus_inside_meta = sum(inside), share_1plus = mean(inside),
             ties_2plus = sum(W$w >= 2), ties_2plus_inside_meta = sum(inside & W$w >= 2), share_2plus = mean(inside[W$w >= 2]),
             interlockers = nrow(ip), interlockers_only_inside_meta = sum(ip$km == 1), share_interlockers = mean(ip$km == 1))
}
META <- meta_of(W[w >= MIN_SHARED & jaccard >= JACCARD])
ms <- rbind(share_row(sprintf("w>=%d & jaccard>=%.1f (used)", MIN_SHARED, JACCARD), META),
            share_row("w>=3 & jaccard>=0.7 (stricter)", meta_of(W[w >= 3 & jaccard >= 0.7])),
            share_row("w>=3 & overlap>=0.6 (looser: most of the smaller board)", meta_of(W[w >= 3 & overlap >= 0.6])))
fwrite(ms, file.path(fig, "meta-share.csv"))
print(ms)

mo <- data.table(org = names(META), meta = META)[, .(entities = .N, members = list(org)), by = meta][entities > 1]
mo[, `:=`(name = onames[meta], board = bsize[meta], ntee = org_attr$ntee[match(meta, org_attr$org)],
          example_entities = vapply(members, function(m) paste(head(onames[setdiff(m, meta[1])], 3), collapse = " | "), ""))]
mo <- mo[order(-entities)]
fwrite(mo[, .(meta, name, entities, board, ntee, example_entities)], file.path(fig, "meta-orgs.csv"))
stamp("%d meta-orgs covering %d entities; largest: %s (%d entities)", nrow(mo), sum(mo$entities), mo$name[1], mo$entities[1])

# ---- collapse ------------------------------------------------------------------------
seats[, meta := META[org]]
mseats <- seats[, .(years = list(sort(unique(unlist(years))))), by = .(meta, person)]
mattr <- org_attr[, .(meta = META[org], org, ntee, cbsa, people)][order(-people)][, .(ntee = ntee[1], cbsa = cbsa[1], entities = .N), by = meta]
mattr[, name := onames[meta]]
mattr[, board := mseats[, .N, by = meta][match(mattr$meta, meta), N]]
mattr[is.na(board), board := 0L]

# NTEE -> NCCS MAJ12
maj12 <- function(n) {
  l <- substr(n, 1, 1); c3 <- substr(n, 1, 3)
  out <- rep("UNU", length(n))
  out[l == "A"] <- "ART"; out[l == "B"] <- "EDU"; out[c3 %in% c("B40", "B41", "B42", "B43", "B50")] <- "UNI"
  out[l %in% c("C", "D")] <- "ENV"; out[l %in% c("E", "F", "G", "H")] <- "HEL"
  out[c3 %in% c("E20", "E21", "E22", "E24")] <- "HOS"; out[l %in% LETTERS[9:16]] <- "HMS"
  out[l == "Q"] <- "IFA"; out[l %in% c("R", "S", "T", "U", "V", "W")] <- "PSB"; out[l == "X"] <- "REL"
  out[l == "Y"] <- "MMB"; out[is.na(n) | startsWith(n, "UND") | l == "Z"] <- "UNU"   # UNDEFINED, not U
  out
}
MAJ12_LAB <- c(ART = "Arts", EDU = "Education", UNI = "Universities", ENV = "Environment & animals",
               HEL = "Health", HOS = "Hospitals", HMS = "Human services", IFA = "International",
               PSB = "Public & societal benefit", REL = "Religion", MMB = "Mutual benefit", UNU = "Unknown")
# Validated categorical hues (dataviz reference palette); paired groups share a hue
# in two steps, the two residual groups are neutral grays. A legend is always on.
PAL <- c(ART = "#e87ba4", EDU = "#2a78d6", UNI = "#184f95", ENV = "#008300", HEL = "#1baf7a",
         HOS = "#0b6b49", HMS = "#eb6834", IFA = "#6250d6", PSB = "#eda100", REL = "#e34948",
         MMB = "#6f6e69", UNU = "#c3c2b7")
NTEE26 <- c(A = "Arts", B = "Education", C = "Environment", D = "Animals", E = "Health care",
            F = "Mental health", G = "Diseases", H = "Medical research", I = "Crime & legal",
            J = "Employment", K = "Food & agriculture", L = "Housing", M = "Public safety",
            N = "Recreation & sports", O = "Youth development", P = "Human services",
            Q = "International", R = "Civil rights", S = "Community improvement", T = "Philanthropy",
            U = "Science & tech", V = "Social science", W = "Public benefit", X = "Religion",
            Y = "Mutual benefit", Z = "Unknown")
mattr[, `:=`(maj12 = maj12(ntee), letter = fifelse(is.na(ntee) | startsWith(ntee, "UND"), "Z", substr(ntee, 1, 1)))]
saveRDS(list(mattr = mattr, mseats = mseats, persons = P[, .(EMP_ID, person, org, board, years)], META = META),
        file.path(d, "meta-collapsed.rds"))   # input to 06d and 06e

SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; EDGE <- "#9b9a95"
base_theme <- theme_void(base_size = 9) +
  theme(plot.background = element_rect(fill = SURF, colour = NA),
        plot.title = element_text(colour = INK, face = "bold", size = 12, margin = margin(b = 3)),
        plot.subtitle = element_text(colour = INK2, size = 8.5, margin = margin(b = 6)),
        plot.caption = element_text(colour = INK2, size = 7, hjust = 0),
        legend.text = element_text(colour = INK2, size = 7.5), legend.title = element_text(colour = INK, size = 8),
        plot.margin = margin(12, 12, 12, 12))
lay <- function(g) { set.seed(42); if (vcount(g) <= 4000) layout_with_fr(g) else layout_with_drl(g) }
fmt <- function(x) trimws(format(x, big.mark = ","))

# ---- 4. networks per geography -------------------------------------------------------
networks <- function(geo_id, geo_name, metas) {
  s <- mseats[meta %in% metas]
  ip <- s[, .N, by = person][N >= 2, person]
  M <- org_pairs(s, "meta")
  out <- list(geo = geo_id, name = geo_name, orgs = length(metas), interlockers = length(ip),
              ties_1plus = nrow(M), ties_2plus = sum(M$w >= 2))

  # (1) organizations sharing 2+ board members
  E1 <- M[w >= 2]
  V1 <- mattr[meta %in% c(E1$u1, E1$u2), .(name = meta, label = name, maj12, board, entities)]
  g1 <- graph_from_data_frame(E1[, .(u1, u2, w)], directed = FALSE, vertices = V1)
  # Core only: the largest connected group, then the smallest k-core (every
  # organization keeps k+ ties to the others left) small enough to read.
  biggest <- function(g) { cm <- components(g); induced_subgraph(g, which(cm$membership == which.max(cm$csize))) }
  g1 <- biggest(g1)
  cn <- coreness(g1)
  ksz <- vapply(seq_len(max(cn)), function(k) sum(cn >= k), 0L)
  core_sizes[[geo_id]] <<- data.table(geo = geo_id, k = seq_along(ksz), orgs = ksz,
                                      ties = vapply(seq_along(ksz), function(k) ecount(induced_subgraph(g1, which(cn >= k))), 0))
  k <- which(ksz <= CORE_MAX)[1]; if (is.na(k)) k <- max(cn)
  gc1 <- biggest(induced_subgraph(g1, which(cn >= k)))
  V(gc1)$ev <- eigen_centrality(gc1, weights = E(gc1)$w)$vector
  top <- order(-V(gc1)$ev)[seq_len(min(15, vcount(gc1)))]
  V(gc1)$lab <- ""; V(gc1)$lab[top] <- substr(V(gc1)$label[top], 1, 34)
  V(gc1)$maj12 <- factor(V(gc1)$maj12, names(PAL))
  set.seed(42)
  p1 <- ggraph(gc1, layout = "stress", weights = NA) +
    geom_edge_link0(aes(edge_width = w), edge_colour = EDGE, edge_alpha = 0.4) +
    scale_edge_width(range = c(0.15, 1.4), name = "shared board members", breaks = c(2, 5, 10)) +
    geom_node_point(aes(fill = maj12, size = ev), shape = 21, colour = SURF, stroke = 0.3) +
    scale_fill_manual(values = PAL, labels = MAJ12_LAB, drop = TRUE, name = "NTEE MAJ12") +
    scale_size_area(max_size = 9, name = "eigenvector\ncentrality", breaks = c(0.25, 0.5, 1)) +
    geom_node_text(aes(label = lab), repel = TRUE, size = 2.3, colour = INK, max.overlaps = Inf,
                   segment.colour = INK2, segment.size = 0.2, bg.colour = SURF, bg.r = 0.1) +
    guides(fill = guide_legend(override.aes = list(size = 3))) +
    labs(title = paste0(geo_name, ": core network of organizations sharing 2+ board members"),
         subtitle = sprintf("%s organizations, %s ties: the %d-core of the largest connected group, where every organization shares 2+ board members with %d+ others shown.\nOf %s organizations with any such tie. Labels: the 15 most central.",
                            fmt(vcount(gc1)), fmt(ecount(gc1)), k, k, fmt(nrow(V1))),
         caption = sprintf("Board members matched across organizations at score >= %g; meta-orgs collapsed. Node size: eigenvector centrality, weighted by shared board members (1 = most central).", TH)) +
    base_theme
  ggsave(file.path(fig, paste0(geo_id, "_1-orgs.png")), p1, width = 10, height = 9, dpi = 220, bg = SURF)

  # (2) 26 NTEE major groups: density of 1+ shared-board-member ties
  la <- setNames(mattr$letter, mattr$meta)
  n_l <- table(factor(la[metas], LETTERS))
  T2 <- M[, .(a = pmin(la[u1], la[u2]), b = pmax(la[u1], la[u2]))][, .(ties = .N), by = .(a, b)]
  T2[, possible := fifelse(a == b, as.numeric(n_l[a]) * (as.numeric(n_l[a]) - 1) / 2, as.numeric(n_l[a]) * as.numeric(n_l[b]))]
  T2[, density := 1000 * ties / possible]
  fwrite(T2[order(a, b)], file.path(fig, paste0(geo_id, "_2-groups.csv")))
  within <- setNames(T2[a == b, density], T2[a == b, a])
  LET <- LETTERS[1:25]   # Z (unknown / UNDEFINED) is left off the circle
  V2 <- data.table(name = LET, orgs = as.integer(n_l[LET]), within = within[LET])
  V2[is.na(within), within := 0]
  V2[, maj12 := factor(maj12(paste0(name, "00")), names(PAL))]
  ang <- pi / 2 - 2 * pi * (seq_along(LET) - 1) / length(LET)
  V2[, `:=`(x = cos(ang), y = sin(ang))]
  V2[, lab := sprintf("%s %s (%s)", name, NTEE26[name], fmt(orgs))]
  E2 <- T2[a != b & ties > 0 & a != "Z" & b != "Z"]
  g2 <- graph_from_data_frame(E2[, .(a, b, ties, density)], directed = FALSE, vertices = V2)
  p2 <- ggraph(g2, layout = "manual", x = V2$x, y = V2$y) +
    geom_edge_link0(aes(edge_width = density, edge_alpha = density), edge_colour = "#2a78d6") +
    scale_edge_width(range = c(0.1, 3.5), name = "ties per 1,000\norganization pairs") +
    scale_edge_alpha(range = c(0.15, 0.85), guide = "none") +
    geom_node_point(aes(fill = maj12, size = orgs), shape = 21, colour = SURF, stroke = 0.4) +
    scale_fill_manual(values = PAL, labels = MAJ12_LAB, drop = TRUE, name = "NTEE MAJ12") +
    scale_size_area(max_size = 9, name = "organizations") +
    geom_text(data = V2, aes(x = x * 1.13, y = y * 1.13, label = lab,
                             hjust = ifelse(abs(x) < 0.02, 0.5, ifelse(x > 0, 0, 1))),
              size = 2.4, colour = INK, inherit.aes = FALSE) +
    coord_equal(xlim = c(-1.75, 1.75), ylim = c(-1.25, 1.25), clip = "off") +
    guides(fill = guide_legend(override.aes = list(size = 3))) +
    labs(title = paste0(geo_name, ": board ties across NTEE major groups"),
         subtitle = sprintf("Edge: share of organization pairs in two groups that share 1+ board member (%s ties among %s organizations with an NTEE code).\nWithin-group density in %s_2-groups.csv.",
                            fmt(sum(E2$ties) + T2[a == b & a != "Z", sum(ties)]), fmt(sum(V2$orgs)), geo_id),
         caption = sprintf("Meta-orgs collapsed. Left out: %s organizations with no NTEE code (UNDEFINED).", fmt(as.integer(n_l["Z"])))) +
    base_theme
  ggsave(file.path(fig, paste0(geo_id, "_2-groups.png")), p2, width = 10, height = 8, dpi = 220, bg = SURF)

  # (3) interlocking board members tied by serving on one board in the same year
  y <- s[person %in% ip, .(year = unlist(years)), by = .(meta, person)]
  E3 <- unique(y[y, on = c("meta", "year"), allow.cartesian = TRUE, nomatch = 0][person < i.person, .(p1 = person, p2 = i.person)])
  nb <- s[person %in% ip, .N, by = person]
  V3 <- data.table(name = ip)[, boards := nb$N[match(name, nb$person)]]
  V3[, bcat := factor(fcase(boards == 2, "2", boards == 3, "3", boards <= 5, "4-5", default = "6+"), c("2", "3", "4-5", "6+"))]
  g3 <- graph_from_data_frame(E3, directed = FALSE, vertices = V3)
  g3 <- induced_subgraph(g3, V(g3)[degree(g3) > 0])
  L3 <- lay(g3)
  p3 <- ggraph(g3, layout = "manual", x = L3[, 1], y = L3[, 2]) +
    geom_edge_link0(edge_colour = EDGE, edge_alpha = if (ecount(g3) > 20000) 0.06 else 0.2, edge_width = 0.15) +
    geom_node_point(aes(fill = bcat), shape = 21, colour = SURF, stroke = 0.1,
                    size = if (vcount(g3) > 3000) 0.9 else 1.6) +
    scale_fill_manual(values = c("2" = "#86b6ef", "3" = "#4d94e3", "4-5" = "#2a78d6", "6+" = "#123e75"),
                      name = "boards served", drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 3))) +
    labs(title = paste0(geo_name, ": interlocking board members"),
         subtitle = sprintf("%s people who sit on 2+ boards; %s ties, each two of them on the same board in the same year.",
                            fmt(vcount(g3)), fmt(ecount(g3))),
         caption = "Meta-orgs collapsed, so serving on several entities of one meta-org counts as one board.") +
    base_theme
  ggsave(file.path(fig, paste0(geo_id, "_3-people.png")), p3, width = 10, height = 9, dpi = 220, bg = SURF)

  comp3 <- components(g3)
  c(out, list(n1_orgs = nrow(V1), n1_ties = nrow(E1), n1_largest_component = vcount(g1),
              n1_core_k = k, n1_core_orgs = vcount(gc1), n1_core_ties = ecount(gc1),
              n3_people = vcount(g3), n3_ties = ecount(g3), n3_largest_component = max(c(0, comp3$csize)),
              n3_mean_degree = round(2 * ecount(g3) / max(1, vcount(g3)), 1)))
}

geos <- list(list(id = paste0("csa", CSA), name = xw$csa_title[1], metas = mattr$meta))
top4 <- mattr[!is.na(cbsa), .N, by = cbsa][order(-N)][1:4, cbsa]
for (cb in top4) geos[[length(geos) + 1]] <- list(id = paste0("cbsa", cb), name = xw[cbsa == cb, cbsa_title][1],
                                                 metas = mattr[cbsa == cb, meta])
if (Sys.getenv("ONLY_CLUSTER") != "1") {   # ONLY_CLUSTER=1 redoes just section 5
res <- rbindlist(lapply(geos, function(g) { stamp("drawing %s (%s organizations)", g$name, fmt(length(g$metas)))
  as.data.table(networks(g$id, g$name, g$metas)) }))
fwrite(res, file.path(fig, "networks.csv"))
fwrite(rbindlist(core_sizes), file.path(fig, "core-sizes.csv"))
options(width = 200); print(res)
}

# ---- 5. across the CSA: do organizations cluster by CBSA or by industry? ------------
# On the whole CSA network (not only the core), for the 1+ and 2+ shared-member
# networks: the share of ties inside one CBSA / one MAJ12 group against what
# random wiring with the same degrees would give, nominal assortativity,
# modularity of each partition, and how well detected communities (Louvain)
# line up with each (normalized mutual information). MAJ12 measures leave out
# organizations with an unknown NTEE code.
Mcsa <- org_pairs(mseats, "meta")
cb_of <- setNames(mattr$cbsa, mattr$meta); mj_of <- setNames(mattr$maj12, mattr$meta)
cluster_stats <- function(E, label) {
  g <- graph_from_data_frame(E[, .(u1, u2, weight = w)], directed = FALSE)
  one <- function(attr, drop_unknown) {
    gg <- if (drop_unknown) induced_subgraph(g, which(mj_of[V(g)$name] != "UNU")) else g
    ty <- (if (attr == "CBSA") cb_of else mj_of)[V(gg)$name]
    ok <- !is.na(ty); gg <- induced_subgraph(gg, which(ok)); ty <- ty[ok]
    el <- as_edgelist(gg)
    same <- mean(ty[match(el[, 1], V(gg)$name)] == ty[match(el[, 2], V(gg)$name)])
    dshare <- tapply(degree(gg), ty, sum) / (2 * ecount(gg))
    f <- as.integer(factor(ty))
    set.seed(1); com <- cluster_louvain(gg, weights = NA)$membership
    data.table(network = label, by = attr, orgs = vcount(gg), ties = ecount(gg),
               ties_within = round(same, 3), random_expectation = round(sum(dshare^2), 3),
               assortativity = round(assortativity_nominal(gg, f, directed = FALSE), 3),
               modularity = round(modularity(gg, f, weights = NA), 3),
               communities = max(com), nmi_with_communities = round(compare(com, f, method = "nmi"), 3))
  }
  rbind(one("CBSA", FALSE), one("MAJ12", TRUE))
}
cl <- rbind(cluster_stats(Mcsa, "1+ shared board members"), cluster_stats(Mcsa[w >= 2], "2+ shared board members"))
fwrite(cl, file.path(fig, "cluster-by-cbsa-or-maj12.csv"))
options(width = 200); print(cl)

# Same layout, two colorings: the 3-core of the 2+ network's largest connected group.
E4 <- Mcsa[w >= 2]
g4 <- graph_from_data_frame(E4[, .(u1, u2, w)], directed = FALSE,
                            vertices = mattr[meta %in% c(E4$u1, E4$u2), .(name = meta, label = name, maj12, cbsa)])
cm <- components(g4); g4 <- induced_subgraph(g4, which(cm$membership == which.max(cm$csize)))
K4 <- 3L
g4 <- induced_subgraph(g4, which(coreness(g4) >= K4))
cm <- components(g4); g4 <- induced_subgraph(g4, which(cm$membership == which.max(cm$csize)))
V(g4)$ev <- eigen_centrality(g4, weights = E(g4)$w)$vector
top <- order(-V(g4)$ev)[1:12]; V(g4)$lab <- ""; V(g4)$lab[top] <- substr(V(g4)$label[top], 1, 30)
set.seed(42); L4 <- graphlayouts::layout_with_stress(g4, weights = NA)   # unweighted: positions from shared ties alone
cbsa_lab <- setNames(sub(", NC$", "", xw$cbsa_title), xw$cbsa)
cbsa_n <- table(V(g4)$cbsa)
cbsa_order <- names(sort(cbsa_n, decreasing = TRUE))
CBPAL <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")[seq_along(cbsa_order)], cbsa_order)
V(g4)$cbsa_f <- factor(V(g4)$cbsa, cbsa_order); V(g4)$maj12 <- factor(V(g4)$maj12, names(PAL))
s4 <- cl[network == "2+ shared board members"]
draw4 <- function(fill_var, values, labels, legend, file, note) {
  p <- ggraph(g4, layout = "manual", x = L4[, 1], y = L4[, 2]) +
    geom_edge_link0(edge_colour = EDGE, edge_alpha = 0.3, edge_width = 0.2) +
    geom_node_point(aes(fill = .data[[fill_var]], size = ev), shape = 21, colour = SURF, stroke = 0.25) +
    scale_fill_manual(values = values, labels = labels, drop = TRUE, name = legend) +
    scale_size_area(max_size = 7, name = "eigenvector\ncentrality", breaks = c(0.25, 0.5, 1)) +
    geom_node_text(aes(label = lab), repel = TRUE, size = 2.2, colour = INK, max.overlaps = Inf,
                   segment.colour = INK2, segment.size = 0.2, bg.colour = SURF, bg.r = 0.1) +
    guides(fill = guide_legend(override.aes = list(size = 3))) +
    labs(title = sprintf("%s: organizations sharing 2+ board members, by %s", xw$csa_title[1], legend),
         subtitle = sprintf("%s organizations, %s ties (the %d-core of the largest connected group). Same layout in both versions.\n%s",
                            fmt(vcount(g4)), fmt(ecount(g4)), K4, note),
         caption = sprintf("Whole-CSA measures in cluster-by-cbsa-or-maj12.csv. Matches at score >= %g; meta-orgs collapsed. Node size: eigenvector centrality.", TH)) +
    base_theme
  ggsave(file.path(fig, file), p, width = 10, height = 9, dpi = 220, bg = SURF)
}
draw4("cbsa_f", CBPAL, cbsa_lab[cbsa_order], "CBSA", paste0("csa", CSA, "_4-orgs-by-cbsa.png"),
      sprintf("Across the CSA, %.0f%% of these ties stay within one CBSA (%.0f%% expected at random); assortativity %.2f.",
              100 * s4[by == "CBSA", ties_within], 100 * s4[by == "CBSA", random_expectation], s4[by == "CBSA", assortativity]))
draw4("maj12", PAL, MAJ12_LAB, "NTEE MAJ12", paste0("csa", CSA, "_4-orgs-by-maj12.png"),
      sprintf("Across the CSA, %.0f%% of ties between organizations with a known NTEE code stay within one MAJ12 group (%.0f%% expected at random); assortativity %.2f.",
              100 * s4[by == "MAJ12", ties_within], 100 * s4[by == "MAJ12", random_expectation], s4[by == "MAJ12", assortativity]))
stamp("figures in %s", fig)
