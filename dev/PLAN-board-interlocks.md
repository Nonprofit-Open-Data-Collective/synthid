# Plan: board-interlock data transformations and descriptive metrics

Status: draft, 2026-10-27. Built from the Raleigh-Durham-Cary (CSA 450) prototype
in `dev/partvii-panel/06b`-`06e`.

## Why

synthid ends at linkage: `link_panel()` gives each person a within-org `EMP_ID`,
`link_cross_org()` gives a cross-org `XORG_ID`. Every network analysis after
that was hand-written in the prototype scripts, and the same steps came up each
time:

1. pick board seats out of the linked panel,
2. collapse organizations that are one governance team filed as several legal
   entities (meta-orgs),
3. project the person-organization data into an organization network (shared
   board members) or a people network (shared board service),
4. filter by tie strength, and
5. describe how ties fall within and across groups (NTEE sector, CBSA, CSA).

These steps decided whether the results meant anything: without meta-org
collapse, 21% of 2+ shared-member org ties were one organization's own
entities; without a stricter person match, the "top bridges" were JOHN SMITH
chains. They belong in the package, with the defaults the prototype found.

## What the prototype found (CSA 450)

- **Geography sorts ties more than industry.** 78% of 2+ shared-member org ties
  stay within one CBSA (48% expected at random; assortativity 0.56) vs 31% within
  one MAJ12 group (17% expected; 0.16).
- **Most board ties cross sectors.** In Raleigh-Cary, the share of a sector's
  ties that stay inside it runs from 2% (international) to 20% (human services).
- **NTEE blocking loses most cross-org matches**: 74% of surname + CBSA matches
  cross NTEE groups (step 6b).
- **24% of organizations have NTEE "UNDEFINED"**, which a first-letter rule
  files under U (science & technology).
- **Meta-orgs**: 75 meta-orgs covering 212 entities (shared >= 3 board members,
  Jaccard >= 0.5); they hold 21% of 2+ shared-member org ties but 2% of 1+ ties.
  The rule misses organization + foundation pairs whose boards differ in size
  over 15 years (NC Advocates for Justice and its foundation; NCTA and its
  education foundation; WakeMed and its group return).
- **People networks need a stricter person match than org networks.** At score
  7 a bare common first + last name scores ~9 and chains (one "Michael Smith"
  on 44 boards). At score >= 10 the top bridges are distinctive names on 10-21
  boards.
- **Projections are hairballs unfiltered.** Every board is a clique in the
  people projection (14,875 people, mean degree 36). The k-core of the org
  projection, people tied by 2+ shared boards, and within/between-sector
  nesting made them readable.

## Proposed functions

All take and return plain data frames (edge lists, membership tables), so they
work without a graph library; igraph stays in Suggests and is used only where
named. Column names follow `synthid_cols()`.

### 1. Board seats

`board_seats(linked, xorg = NULL, roles = "BOARD", cols = synthid_cols())`
-> one row per person x organization: `org_id`, `person_id`, `first_year`,
`last_year`, `years` (list), `titles` (list).
- `person_id` is `XORG_ID` when `xorg` (from `link_cross_org()`/`stamp_xorg()`)
  is given, else `EMP_ID`.
- `roles` uses `title_role()`; `"ANY"` keeps every seat.

### 2. Meta-orgs

`detect_meta_orgs(seats, min_shared = 3, jaccard = 0.5, overlap = NULL, by_year = FALSE)`
-> `org_id`, `meta_id`, `meta_size`, plus the pair table with `shared`,
`jaccard`, `overlap` (shared / smaller board) for review.
- A meta-org is a connected set of organization pairs passing the rule; named
  by its member with the largest board.
- `by_year = TRUE` compares boards year by year and requires the rule in most
  co-filing years. This should catch organization + foundation pairs that the
  pooled 15-year Jaccard misses. Test it against the three missed pairs above.

`collapse_meta_orgs(seats, meta)` -> seats keyed by `meta_id`, years and
titles unioned.

`meta_org_share(seats, meta)` -> share of 1+ and 2+ org ties, and of
interlockers, that sit inside meta-orgs (the "how much do they inflate" table).

### 3. Projections

`org_projection(seats, min_shared = 1, same_year = FALSE)`
-> `org_a`, `org_b`, `shared` (distinct people), `shared_years`.

`person_projection(seats, min_shared = 1, same_year = TRUE, interlockers_only = TRUE)`
-> `person_a`, `person_b`, `shared` (boards served together).
- `same_year = TRUE` counts a board only if both served in a common year.
- `interlockers_only` keeps people on 2+ boards; `min_shared = 2` is the
  "inner circle" network.
- Guard: warn when the projection will exceed a row budget (a 100-member board
  is 4,950 pairs) and report the largest boards.

`network_core(edges, max_nodes = 160, k = NULL)` -> the largest connected
group, then the smallest k-core with at most `max_nodes` (or a given `k`);
returns edges plus `attr(, "k")` and the size at every k. (igraph)

### 4. Group mixing and the E-I index

`ei_index(edges, groups, level = c("network", "group", "node"), weight = NULL,
rescale = FALSE, permutations = 0, seed = NULL)`

Krackhardt and Stern (1988): `EI = (E - I) / (E + I)`, where E counts ties
between groups and I ties within. -1 means all ties are internal, +1 all
external.
- `level = "group"`: per group, from the ties its members have;
  `"node"`: per organization or person; `"network"`: one value.
- `rescale = TRUE`: replaces counts with densities (ties over possible ties
  within / between), so large groups are not internal by size alone (as in
  egor's `EI(rescale = TRUE)`).
- `permutations > 0`: shuffles group labels to give the expected EI and a
  p-value. A group of sector-crossers reads very differently once you know
  random mixing would give, say, -0.1.
- `weight`: uses tie strength (`shared`) instead of counts.
- Also returns `I`, `E`, `within_share = I / (I + E) = (1 - EI) / 2` (the
  "within" share on the prototype's sector circles) and the expected values.
- Units are anything in `groups`: NTEE letter, MAJ10/MAJ12, CBSA, CSA, a
  meta-org, or a community. CBSA vs CSA is one call each with the same edges.
- netseg (`ei()`, plus `orwg()`, `ssi()`, `freeman()`) and egor (`EI()`)
  implement it. Write our own on data frames, without those dependencies, and
  check it against `netseg::ei()` in tests.

`group_mixing(edges, groups, sizes = NULL)` -> the group x group table:
`ties`, `possible`, `density` (per 1,000 pairs), within and between. This is
the data behind the 26-node circle graph and the nested sector graphs.

`assortativity_by(edges, groups)` -> nominal assortativity and modularity of a
partition (igraph), with the random-wiring expectation for the within share.

### 5. People

`bridge_scores(seats, org_attrs, by = c("maj10", "cbsa"))` -> per person:
boards, distinct groups spanned per `by`, `sole_links` (org pairs only this
person links), brokerage (betweenness in the person-organization graph,
igraph), and E-I over the boards they link.

`inner_circle(seats, min_shared = 2, method = "louvain", min_group = 6)` ->
membership (`person_id`, `group`) and a group table labeled by the boards most
members share.

`name_risk(xorg, profiles)` -> per cross-org person: surname rarity, whether a
middle initial backed the match, organizations per person. Bridge and
inner-circle results should report it, and the docs should recommend a
stricter `link_cross_org(threshold = 10)` for people-level analysis.

### 6. NTEE and geography helpers

- `ntee_major(ntee)`: the letter, with `"UNDEFINED"` and `"Z99"` -> `"Z"`.
- `ntee_maj10(ntee)`, `ntee_maj12(ntee)`: NCCS groupings (universities and
  hospitals split out in MAJ12).
- `ntee_division(ntee)`: A2, A5, ...; the subsector for single-letter sectors.
- A CBSA -> CSA crosswalk as package data (from the 2023 delineation file), so
  `groups = cbsa_to_csa(cbsa)` works.

### Out of scope for the package (vignette or dev only)

The nested circle graph, the bridge dot chart and the board diagrams use
ggraph, graphlayouts and ggrepel; keep the plotting in the vignette. If a plot
helper is wanted later, `plot_group_mixing()` on `group_mixing()` output is the
first candidate.

## Order of work

1. NTEE and geography helpers, `board_seats()`, `org_projection()`,
   `person_projection()`. Tests on the `make_panel()` fixtures plus a small
   hand-built seats table with a known meta-org and a known bridge.
2. `ei_index()` and `group_mixing()`. Tests: hand-computed EI on a 6-node
   example, rescaled EI, agreement with `netseg::ei()` (skip if not
   installed), and a permutation expectation near the analytic one.
3. `detect_meta_orgs()`, `collapse_meta_orgs()`, `meta_org_share()`, including
   `by_year`, checked against the 06c meta-org list and the three missed pairs.
4. `network_core()`, `assortativity_by()`, `bridge_scores()`,
   `inner_circle()`, `name_risk()`.
5. Re-run 06c-06e on the new functions and confirm the same numbers: 75
   meta-orgs, 78% within-CBSA, the sector within shares.
6. The vignette (below).

## Vignette: board interlocks

Turn the CSA 450 analysis into `vignettes/board-interlocks.Rmd`: "From Part VII
records to board-interlock networks". Outline:

1. Board seats from a linked panel; why role filtering matters.
2. Meta-orgs: detect, review, collapse; how much they inflate ties.
3. Two projections: organizations sharing board members, and people sharing
   boards; filtering by tie strength and the k-core.
4. Within vs between groups: E-I index by MAJ10, CBSA and CSA; geography vs
   industry; the 25-group circle and the nested sector graph.
5. People: bridges and the inner circle, with the stricter person match and
   the name-risk check.
6. Caveats: NTEE UNDEFINED, pooled years vs single-year snapshots, false
   merges of common names, national associations with a local address.

Data: vignettes cannot read the full panel. Ship a small extract (one CBSA,
organization-level edges and seats with hashed person ids), or simulate a panel
with the same structure. Do not print individual names in the vignette, even
though they come from public filings: show organizations, and person ids or
synthetic names. Add it to the "Articles" menu in `_pkgdown.yml`.

## Open questions

- Pooled 2010-2024 vs year-by-year networks: offer both (`year =` argument on
  projections) or one default?
- Person threshold: a separate stricter `XORG_ID` (a second `link_cross_org()`
  run at 10), or a per-edge score kept so projections can filter later?
- National associations filing from a local address (AICPA in Durham) form
  their own inner-circle groups; flag them by board geography (most members
  outside the CSA)?

## References

- Krackhardt, D., & Stern, R. N. (1988). Informal networks and organizational
  crises: An experimental simulation. *Social Psychology Quarterly*, 51(2),
  123-140. doi:10.2307/2786835
- netseg: `ei()` and related segregation measures (Bojanowski).
- egor: `EI()`, with the density-rescaled variant.
- Hanneman & Riddle, *Introduction to Social Network Methods*, ch. 8 (group-
  external and group-internal ties).
