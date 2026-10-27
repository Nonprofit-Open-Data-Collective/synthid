# 06d-sector-networks.R
# Prototype: one board-interlock graph per NTEE MAJ10 sector for one CBSA.
#
#   - MAJ10 = MAJ12 with universities in education and hospitals in health;
#     organizations with no NTEE code are left out.
#   - Organizations tie when they share 1+ board member (meta-orgs collapsed,
#     from 06c-csa-networks.R).
#   - Only the main components of a sector's network are drawn: its largest
#     connected group plus any other group of MIN_COMPONENT+ organizations.
#   - Node size: eigenvector centrality within the sector (weighted by shared
#     board members).
#   - Each subsector is a circle holding its organizations and the ties among
#     them. Lines between circles carry the count of ties between subsectors.
#     Subsectors are NTEE major groups for sectors that span several letters
#     (health E-H, human services I-P, public benefit R-W, environment C-D),
#     and NTEE divisions (A2, A5, A6, ...) for single-letter sectors.
#   - Each circle reports its within share: ties inside the subsector over all
#     ties of its organizations within the sector (inside + to other subsectors).
#
# Writes dev/partvii-panel/csa-test-<CSA>/cbsa<CBSA>_5-sector-<MAJ10>.png and
#   cbsa<CBSA>_5-sectors.csv (one row per subsector)
#
#   Rscript dev/partvii-panel/06d-sector-networks.R [csa=450] [cbsa=39580]

source("dev/partvii-panel/_panel.R")
build_step("06d-sector-networks")
suppressPackageStartupMessages({ library(igraph); library(ggplot2) })
args <- commandArgs(trailingOnly = TRUE)
CSA  <- if (length(args) >= 1) args[1] else "450"
CBSA <- if (length(args) >= 2) args[2] else "39580"
MIN_COMPONENT <- 5L

d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
x <- readRDS(file.path(d, "meta-collapsed.rds")); mattr <- x$mattr; mseats <- x$mseats
dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
cbsa_name <- sub(", NC$", "", unique(dl[`CBSA Code` == CBSA, `CBSA Title`]))

A <- mattr[cbsa == CBSA & maj12 != "UNU"]
A[, maj10 := fcase(maj12 == "UNI", "EDU", maj12 == "HOS", "HEL", default = maj12)]
MULTI <- c("ENV", "HEL", "HMS", "PSB")   # sectors spanning several NTEE letters
A[, sub := fifelse(maj10 %in% MULTI, letter,
                   fifelse(substr(ntee, 2, 2) %in% c("0", "1"), paste0(letter, "0"), substr(ntee, 1, 2)))]

# Board-member overlap between organizations in this CBSA (all sectors).
s <- mseats[meta %in% mattr[cbsa == CBSA, meta]]
ip <- s[, .N, by = person][N >= 2, person]
xx <- unique(s[person %in% ip, .(u = meta, person)])
M <- xx[xx, on = "person", allow.cartesian = TRUE, nomatch = 0][u < i.u, .(u1 = u, u2 = i.u, person)][, .(w = uniqueN(person)), by = .(u1, u2)]

NTEE26 <- c(C = "Environment", D = "Animals", E = "Health care", F = "Mental health", G = "Diseases",
            H = "Medical research", I = "Crime & legal", J = "Employment", K = "Food & agriculture",
            L = "Housing", M = "Public safety", N = "Recreation & sports", O = "Youth development",
            P = "Human services", R = "Civil rights", S = "Community improvement", T = "Philanthropy",
            U = "Science & tech", V = "Social science", W = "Public benefit")
DIV <- c(A0 = "Arts support", A2 = "Arts & culture", A3 = "Media", A4 = "Visual arts", A5 = "Museums",
         A6 = "Performing arts", A7 = "Humanities", A8 = "Historical", A9 = "Arts n.e.c.",
         B0 = "Education support", B2 = "K-12 schools", B3 = "Vocational", B4 = "Higher education",
         B5 = "Graduate schools", B6 = "Adult education", B7 = "Libraries", B8 = "Student services",
         B9 = "Educational services", Q0 = "International support", Q2 = "Exchange", Q3 = "Development & relief",
         Q4 = "Peace & security", Q5 = "Foreign affairs", Q7 = "Human rights", Q9 = "International n.e.c.",
         X0 = "Religion support", X2 = "Christian", X3 = "Jewish", X4 = "Islamic", X5 = "Buddhist",
         X7 = "Hindu", X8 = "Religious media", X9 = "Interfaith", Y0 = "Mutual support", Y2 = "Insurance",
         Y3 = "Pension & retirement", Y4 = "Fraternal", Y5 = "Cemeteries")
sub_name <- function(k) ifelse(k %in% names(NTEE26), NTEE26[k], ifelse(k %in% names(DIV), DIV[k], k))
SECTORS <- c(ART = "Arts", EDU = "Education (incl. universities)", ENV = "Environment & animals",
             HEL = "Health (incl. hospitals)", HMS = "Human services", IFA = "International",
             PSB = "Public & societal benefit", REL = "Religion", MMB = "Mutual benefit")
SECPAL <- c(ART = "#e87ba4", EDU = "#2a78d6", ENV = "#008300", HEL = "#1baf7a", HMS = "#eb6834",
            IFA = "#6250d6", PSB = "#eda100", REL = "#e34948", MMB = "#6f6e69")
SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; EDGE <- "#9b9a95"; RING <- "#c3c2b7"
fmt <- function(x) trimws(format(x, big.mark = ","))

circle_xy <- function(x0, y0, r, n = 120) { t <- seq(0, 2 * pi, length.out = n); data.frame(x = x0 + r * cos(t), y = y0 + r * sin(t)) }

# sec = "ALL" draws one circle per sector (all organizations with an NTEE code).
draw_sector <- function(sec) {
  top_level <- sec == "ALL"
  O <- if (top_level) copy(A)[, sub := maj10] else A[maj10 == sec]
  sub_of <- setNames(O$sub, O$meta)
  E <- M[u1 %in% O$meta & u2 %in% O$meta]
  out_ties <- M[xor(u1 %in% O$meta, u2 %in% O$meta), .N]
  if (nrow(E) < 3) { stamp("%s: %d ties, skipped", sec, nrow(E)); return(NULL) }

  # within share per subsector, from every tie in the sector
  E[, `:=`(s1 = sub_of[u1], s2 = sub_of[u2])]
  subs <- if (top_level) intersect(names(SECTORS), O$sub) else sort(unique(O$sub))
  metric <- rbindlist(lapply(subs, function(k) data.table(sub = k, orgs = sum(O$sub == k),
    within = E[s1 == k & s2 == k, .N], across = E[xor(s1 == k, s2 == k), .N])))
  metric[, within_share := within / (within + across)]

  # main components
  g <- graph_from_data_frame(E[, .(u1, u2, w)], directed = FALSE, vertices = O[, .(name = meta, label = name, sub)])
  cm <- components(g)
  keep_c <- unique(c(which.max(cm$csize), which(cm$csize >= MIN_COMPONENT)))
  g <- induced_subgraph(g, which(cm$membership %in% keep_c))
  V(g)$ev <- eigen_centrality(g, weights = E(g)$w)$vector

  # circles: radius by organizations drawn, placed on a ring in NTEE order
  drawn <- table(V(g)$sub)
  ks <- intersect(subs, names(drawn))
  r <- setNames(0.55 + 0.32 * sqrt(as.numeric(drawn[ks])), ks)
  gap <- 0.9
  if (length(ks) == 1) { cx <- setNames(0, ks); cy <- setNames(0, ks) } else {
    R <- max((sum(2 * r) + gap * length(ks)) / (2 * pi), max(r) * 1.25)
    arc <- cumsum(2 * r + gap) - r - gap / 2
    th <- pi / 2 - 2 * pi * arc / sum(2 * r + gap)
    cx <- setNames(R * cos(th), ks); cy <- setNames(R * sin(th), ks)
  }

  # Organizations inside their circle: those tied to others in the same
  # subsector as packed clusters in the middle; those tied only to other
  # subsectors on the inner rim, ordered by the angle toward their partners.
  pos <- matrix(0, vcount(g), 2)
  V(g)$rim <- FALSE
  for (k in ks) {
    idx <- which(V(g)$sub == k)
    h <- induced_subgraph(g, idx)
    conn <- idx[degree(h) > 0]; lone <- idx[degree(h) == 0]
    rin <- if (length(lone)) 0.62 else 0.84
    if (length(conn)) {
      hc <- induced_subgraph(g, conn)
      set.seed(42); L <- layout_components(hc, layout = layout_with_fr)
      L <- sweep(L, 2, colMeans(L)); m <- max(sqrt(rowSums(L^2)), 1e-9)
      L <- L / m * r[k] * rin
      pos[conn, ] <- cbind(L[, 1] + cx[k], L[, 2] + cy[k])
    }
    if (length(lone)) {
      # evenly spaced, in the order of the direction of their partner circles
      ang <- vapply(lone, function(v) {
        nb <- neighbors(g, v)$sub
        atan2(mean(cy[nb] - cy[k]), mean(cx[nb] - cx[k]))
      }, 0)
      o <- order(ang)
      a <- ang[o[1]] + 2 * pi * (seq_along(lone) - 1) / length(lone)
      pos[lone[o], ] <- cbind(cx[k] + 0.88 * r[k] * cos(a), cy[k] + 0.88 * r[k] * sin(a))
      V(g)$rim[lone] <- TRUE
    }
  }
  nodes <- data.table(name = V(g)$name, label = V(g)$label, sub = V(g)$sub, ev = V(g)$ev, x = pos[, 1], y = pos[, 2])
  el <- as_edgelist(g)
  ed <- data.table(a = el[, 1], b = el[, 2], w = E(g)$w)
  ed[, `:=`(sa = sub_of[a], sb = sub_of[b])]
  inner <- ed[sa == sb][, `:=`(x = nodes$x[match(a, nodes$name)], y = nodes$y[match(a, nodes$name)],
                               xend = nodes$x[match(b, nodes$name)], yend = nodes$y[match(b, nodes$name)])]

  # lines between circles: tie counts between subsectors (all ties in the sector)
  X <- E[s1 != s2 & s1 %in% ks & s2 %in% ks, .(n = .N), by = .(p = pmin(s1, s2), q = pmax(s1, s2))]
  if (nrow(X)) {
    X[, `:=`(dx = cx[q] - cx[p], dy = cy[q] - cy[p])]
    X[, len := sqrt(dx^2 + dy^2)]
    X[, `:=`(x = cx[p] + dx / len * r[p], y = cy[p] + dy / len * r[p],
             xend = cx[q] - dx / len * r[q], yend = cy[q] - dy / len * r[q])]
    X[, f := ifelse(seq_len(.N) %% 2 == 1, 0.4, 0.6)]   # stagger count labels where lines cross
    X[, `:=`(mx = x + f * (xend - x), my = y + f * (yend - y))]
  }

  circ <- rbindlist(lapply(ks, function(k) cbind(as.data.table(circle_xy(cx[k], cy[k], r[k])), sub = k)))
  mt <- metric[match(ks, sub)]
  lab <- data.table(sub = ks, cx = cx[ks], cy = cy[ks], r = r[ks])
  lab[, ang := atan2(cy, cx)]
  if (length(ks) == 1) lab[, ang := pi / 2]
  lab[, `:=`(lx = cx + (r + 0.35) * cos(ang), ly = cy + (r + 0.35) * sin(ang),
             hj = ifelse(abs(cos(ang)) < 0.3, 0.5, ifelse(cos(ang) > 0, 0, 1)),
             vj = ifelse(sin(ang) > 0.3, 0, ifelse(sin(ang) < -0.3, 1, 0.5)))]
  lab[, text := sprintf("%s\n%s orgs | %s within",
                        if (top_level) SECTORS[sub] else paste(sub, sub_name(sub)), fmt(mt$orgs),
                        ifelse(is.na(mt$within_share), "n/a", sprintf("%.0f%%", 100 * mt$within_share)))]
  top <- nodes[order(-ev)][seq_len(min(if (top_level) 15 else 10, .N))]
  top[, label := substr(label, 1, 28)]

  col <- if (top_level) "#8a8984" else SECPAL[[sec]]           # lines between circles
  nodes[, fill := if (top_level) SECPAL[sub] else SECPAL[[sec]]]
  unit_word <- if (top_level) "sectors" else "subsectors"
  title <- if (top_level) sprintf("%s: board network across nonprofit sectors", cbsa_name) else
    sprintf("%s: %s board network", cbsa_name, SECTORS[[sec]])
  subtitle <- if (top_level)
    sprintf("%s of %s organizations with an NTEE code, in the main connected groups; %s ties sharing 1+ board member.\nCircles are MAJ10 sectors (universities in education, hospitals in health; Unknown left out); lines between them count ties between sectors.\n\"Within\": ties inside a sector over all its ties to sectors shown.",
            fmt(vcount(g)), fmt(nrow(O)), fmt(nrow(E))) else
    sprintf("%s of %s organizations, in the main connected groups; %s ties sharing 1+ board member. %.0f%% of these organizations' ties stay in the sector.\nCircles are subsectors; lines between them count ties between subsectors. \"Within\": ties inside a subsector over all its ties in the sector.",
            fmt(vcount(g)), fmt(nrow(O)), fmt(nrow(E)), 100 * nrow(E) / (nrow(E) + out_ties))
  p <- ggplot() +
    { if (nrow(X)) geom_segment(data = X, aes(x = x, y = y, xend = xend, yend = yend, linewidth = n),
                                colour = col, alpha = 0.35, lineend = "round") } +
    { if (nrow(X)) geom_label(data = X, aes(x = mx, y = my, label = n), size = 2.3, colour = INK2,
                              fill = SURF, label.size = 0, label.padding = unit(0.08, "lines")) } +
    scale_linewidth(range = c(0.3, 5), name = paste0("ties between\n", unit_word)) +
    geom_polygon(data = circ, aes(x, y, group = sub), fill = SURF, colour = RING, linewidth = 0.4) +
    geom_segment(data = inner, aes(x = x, y = y, xend = xend, yend = yend), colour = EDGE, alpha = 0.5, linewidth = 0.2) +
    geom_point(data = nodes, aes(x, y, size = ev, fill = fill), shape = 21, colour = SURF, stroke = 0.25) +
    scale_fill_identity() +
    guides(size = guide_legend(override.aes = list(fill = if (top_level) "#8a8984" else SECPAL[[sec]]))) +
    scale_size(range = c(0.7, 6.5), limits = c(0, 1), name = "eigenvector\ncentrality", breaks = c(0, 0.25, 0.5, 1)) +
    ggrepel::geom_text_repel(data = top, aes(x, y, label = label), size = 2.1, colour = INK, max.overlaps = Inf,
                             segment.colour = INK2, segment.size = 0.2, bg.colour = SURF, bg.r = 0.12, seed = 1) +
    geom_text(data = lab, aes(lx, ly, label = text, hjust = hj, vjust = vj), size = 2.5, colour = INK, lineheight = 0.95) +
    coord_equal(clip = "off") +
    scale_x_continuous(expand = expansion(mult = 0.22)) + scale_y_continuous(expand = expansion(mult = 0.12)) +
    labs(title = title, subtitle = subtitle,
         caption = sprintf("Matches at score >= 7; meta-orgs collapsed. Node size: eigenvector centrality in the network shown. Labels: the %d most central organizations.\nInside a circle: organizations tied within the %s in the middle; organizations tied only to other %s on the rim.",
                           nrow(top), sub("s$", "", unit_word), unit_word)) +
    theme_void(base_size = 9) +
    theme(plot.background = element_rect(fill = SURF, colour = NA),
          plot.title = element_text(colour = INK, face = "bold", size = 12, margin = margin(b = 3)),
          plot.subtitle = element_text(colour = INK2, size = 8, margin = margin(b = 6)),
          plot.caption = element_text(colour = INK2, size = 7, hjust = 0),
          legend.text = element_text(colour = INK2, size = 7.5), legend.title = element_text(colour = INK, size = 8),
          plot.margin = margin(12, 12, 12, 12))
  ggsave(file.path(fig, sprintf("cbsa%s_5-sector-%s.png", CBSA, sec)), p, width = 10, height = 9, dpi = 220, bg = SURF)
  stamp("%s: %d of %d orgs drawn, %d subsectors", sec, vcount(g), nrow(O), length(ks))
  cbind(sector = sec, metric, sector_ties = nrow(E), ties_to_other_sectors = out_ties)
}

res <- rbindlist(lapply(c("ALL", names(SECTORS)), draw_sector))
fwrite(res, file.path(fig, sprintf("cbsa%s_5-sectors.csv", CBSA)))
options(width = 200); print(res)
