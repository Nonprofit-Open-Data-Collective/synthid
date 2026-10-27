# 06f-cbsa-spanners.R
# Organizations that bridge the CSA's two largest CBSAs: an organization in one
# that shares 2+ board members with an organization in the other. Draws their
# network and compares them with the other organizations in those two CBSAs.
#
# Input: 06c-csa-networks.R's collapsed meta-orgs and board seats (score >= 7).
# Writes dev/partvii-panel/csa-test-<CSA>/csa<CSA>_8-cbsa-spanners.png,
#   cbsa-spanners.csv (one row per spanner) and cbsa-spanners-compare.csv
#
#   Rscript dev/partvii-panel/06f-cbsa-spanners.R [csa=450]

source("dev/partvii-panel/_panel.R")
build_step("06f-cbsa-spanners")
suppressPackageStartupMessages({ library(igraph); library(ggplot2) })
args <- commandArgs(trailingOnly = TRUE)
CSA <- if (length(args) >= 1) args[1] else "450"
d <- file.path(pv_root, "blocks", "csa-test", paste0("csa=", CSA))
fig <- file.path(pv_repo, paste0("csa-test-", CSA))
x <- readRDS(file.path(d, "meta-collapsed.rds")); mattr <- x$mattr; mseats <- x$mseats
dl <- as.data.table(readxl::read_excel(file.path(Sys.getenv("USERPROFILE"), "Documents/SYNTHID/cbsa/list1_2023.xlsx"), skip = 2))
xw <- unique(dl[`CSA Code` == CSA, .(cbsa = `CBSA Code`, cbsa_title = sub(", NC$", "", `CBSA Title`), csa_title = `CSA Title`)])

TWO <- mattr[!is.na(cbsa), .N, by = cbsa][order(-N)][1:2, cbsa]
cname <- setNames(xw$cbsa_title[match(TWO, xw$cbsa)], TWO)
mattr[, maj10 := fcase(maj12 == "UNI", "EDU", maj12 == "HOS", "HEL", default = maj12)]
A <- mattr[cbsa %in% TWO]
cb <- setNames(A$cbsa, A$meta)

S <- unique(mseats[meta %in% A$meta, .(meta, person)])
ip <- S[, .N, by = person][N >= 2, person]
xx <- S[person %in% ip]
M <- xx[xx, on = "person", allow.cartesian = TRUE, nomatch = 0][meta < i.meta, .(u1 = meta, u2 = i.meta, person)][, .(w = uniqueN(person)), by = .(u1, u2)]
M[, cross := cb[u1] != cb[u2]]
X <- M[w >= 2 & cross == TRUE]
sp <- unique(c(X$u1, X$u2))
stamp("%d organization pairs across %s and %s share 2+ board members; %d spanner organizations",
      nrow(X), cname[1], cname[2], length(sp))

# ---- what is distinct about them ------------------------------------------------------
kw <- function(n) {
  n <- toupper(n)
  fcase(grepl("CHAMBER OF COMMERCE|CHAMBER FOUNDATION|BUSINESS COMMITTEE|ECONOMIC DEV|PARTNERSHIP|ALLIANCE", n), "Chamber, business or economic development",
        grepl("\\bDUKE\\b|\\bUNC\\b|UNIVERSITY|COLLEGE|\\bNC STATE\\b|N C STATE", n), "University or college",
        grepl("RESEARCH TRIANGLE|TRIANGLE|REGIONAL", n), "Triangle / regional in name",
        grepl("^NORTH CAROLINA|^N ?C |CAROLINA|^NC[A-Z]|\\bNC\\b", n), "Statewide (North Carolina) in name",
        default = "Other")
}
A[, kind := kw(name)]
ends <- function(D) rbind(D[, .(meta = u1, w)], D[, .(meta = u2, w)])   # one row per tie end
deg <- ends(M[w >= 2])[, .(ties_2plus = .N), by = meta]
A[deg, on = "meta", ties_2plus := i.ties_2plus]; A[is.na(ties_2plus), ties_2plus := 0L]
xdeg <- ends(X)[, .(cross_ties = .N, cross_members = sum(w)), by = meta]
A[xdeg, on = "meta", `:=`(cross_ties = i.cross_ties, cross_members = i.cross_members)]
A[is.na(cross_ties), `:=`(cross_ties = 0L, cross_members = 0L)]
A[, group := fifelse(meta %in% sp, "Cross-metro spanners", fifelse(ties_2plus > 0, "Tied only within its metro", "No 2+ ties"))]

cmp <- A[, .(organizations = .N,
             median_board = as.numeric(median(board)), mean_entities = round(mean(entities), 2),
             statewide_name = mean(kind == "Statewide (North Carolina) in name"),
             triangle_regional_name = mean(kind == "Triangle / regional in name"),
             chamber_business = mean(kind == "Chamber, business or economic development"),
             university = mean(kind == "University or college"),
             unknown_ntee = mean(maj10 == "UNU"),
             human_services = mean(maj10 == "HMS"), education = mean(maj10 == "EDU"),
             health = mean(maj10 == "HEL"), public_benefit = mean(maj10 == "PSB"), arts = mean(maj10 == "ART")),
         by = group]
num <- names(cmp)[sapply(cmp, is.numeric) & !names(cmp) %in% c("organizations", "median_board", "mean_entities")]
cmp[, (num) := lapply(.SD, function(v) round(100 * v, 1)), .SDcols = num]
fwrite(cmp, file.path(fig, "cbsa-spanners-compare.csv"))
options(width = 220); print(cmp)

spt <- A[meta %in% sp][order(-cross_ties, -cross_members)][, .(name, cbsa = cname[cbsa], maj10, ntee, kind, board, cross_ties, cross_members, ties_2plus)]
fwrite(spt, file.path(fig, "cbsa-spanners.csv"))
print(spt[1:30])
print(spt[, .N, by = kind][order(-N)])

# ---- the graphic: a two-column bridge diagram -----------------------------------------
# The full spanner network (hundreds of organizations) is a hairball. Show the
# TOP_SIDE organizations per metro with the most cross-metro ties, and only the
# cross-metro ties among them. Rows are ordered by the barycenter of their
# partners on the other side, so tied organizations sit across from each other.
TOP_SIDE <- 30L
side_top <- function(cbx) A[cbsa == cbx & meta %in% sp][order(-cross_ties, -cross_members)][1:min(TOP_SIDE, .N), meta]
R <- side_top(TWO[1]); D <- side_top(TWO[2])
XE <- X[(u1 %in% R & u2 %in% D) | (u1 %in% D & u2 %in% R)]
XE[, `:=`(r = fifelse(u1 %in% R, u1, u2), d = fifelse(u1 %in% R, u2, u1))]
R <- intersect(R, XE$r); D <- intersect(D, XE$d)
yr <- setNames(seq_along(R), R)
for (it in 1:4) {   # barycenter ordering, alternating sides
  yd <- XE[, .(y = weighted.mean(yr[r], w)), by = d]; yd <- yd[order(y)]; yd <- setNames(seq_len(nrow(yd)), yd$d)
  yr2 <- XE[, .(y = weighted.mean(yd[d], w)), by = r]; yr2 <- yr2[order(y)]; yr <- setNames(seq_len(nrow(yr2)), yr2$r)
}
sc <- function(v) 1 - (v - 1) / max(1, max(v) - 1)   # top to bottom, 1..0
nd <- rbind(A[meta %in% names(yr), .(meta, name, kind, cross_ties, x = -1, y = sc(yr[meta]) * (length(yr) - 1) / max(length(yr), length(yd)))],
            A[meta %in% names(yd), .(meta, name, kind, cross_ties, x = 1, y = sc(yd[meta]) * (length(yd) - 1) / max(length(yr), length(yd)))])
nd[, label := substr(name, 1, 44)]
ed <- XE[, .(x = -1, y = nd$y[match(r, nd$meta)], xend = 1, yend = nd$y[match(d, nd$meta)], w)]
KIND_PAL <- c("Statewide (North Carolina) in name" = "#eb6834", "Chamber, business or economic development" = "#eda100",
              "Triangle / regional in name" = "#2a78d6", "University or college" = "#6250d6", "Other" = "#9b9a95")
SURF <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"
heads <- data.table(x = c(-1, 1), y = max(nd$y) + 0.05, t = cname[TWO], hj = c(1, 0))
p <- ggplot() +
  geom_curve(data = ed, aes(x, y, xend = xend, yend = yend, linewidth = w), colour = "#52514e", alpha = 0.22, curvature = 0) +
  scale_linewidth(range = c(0.25, 2.2), name = "shared board members", breaks = c(2, 4, 8)) +
  geom_point(data = nd, aes(x, y, fill = kind, size = cross_ties), shape = 21, colour = SURF, stroke = 0.4) +
  scale_fill_manual(values = KIND_PAL, name = "organization (from its name)") +
  scale_size_area(max_size = 6.5, name = "cross-metro ties\n(all organizations)") +
  geom_text(data = nd[x < 0], aes(x - 0.04, y, label = label), hjust = 1, size = 2.35, colour = INK) +
  geom_text(data = nd[x > 0], aes(x + 0.04, y, label = label), hjust = 0, size = 2.35, colour = INK) +
  geom_text(data = heads, aes(x, y, label = t, hjust = hj), size = 4, fontface = "bold", colour = INK, nudge_y = 0.02) +
  scale_x_continuous(limits = c(-2.15, 2.15)) +
  guides(fill = guide_legend(override.aes = list(size = 3.5), ncol = 1),
         size = guide_legend(override.aes = list(fill = "#9b9a95"))) +
  labs(title = sprintf("%s: the organizations that tie %s to %s", xw$csa_title[1], cname[1], cname[2]),
       subtitle = sprintf("%d organizations in one metro share 2+ board members with an organization in the other (%d such ties). Shown: the %d on each side with the most\ncross-metro ties, and the ties between them. Rows are ordered so organizations sit across from those they share board members with.",
                          length(sp), nrow(X), TOP_SIDE),
       caption = "Board members matched at score >= 7; meta-orgs collapsed; boards over 2010-2024. Organization type from keywords in its name. All spanners in cbsa-spanners.csv.") +
  theme_void(base_size = 9) +
  theme(plot.background = element_rect(fill = SURF, colour = NA),
        plot.title = element_text(colour = INK, face = "bold", size = 12, margin = margin(b = 3)),
        plot.subtitle = element_text(colour = INK2, size = 8.5, margin = margin(b = 10)),
        plot.caption = element_text(colour = INK2, size = 7, hjust = 0),
        legend.position = "bottom", legend.box = "horizontal",
        legend.text = element_text(colour = INK2, size = 7.5), legend.title = element_text(colour = INK, size = 8),
        plot.margin = margin(12, 12, 12, 12))
ggsave(file.path(fig, sprintf("csa%s_8-cbsa-spanners.png", CSA)), p, width = 13, height = 11, dpi = 220, bg = SURF)
stamp("figure in %s", fig)
