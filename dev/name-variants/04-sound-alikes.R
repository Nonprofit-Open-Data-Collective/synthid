# Sound-alike spellings among the first-name pairs NOT in the draft table
# (the 52,467 pairs seen in fewer than 25 EMP_IDs).
#
# Soundex alone is too loose for names: DAN/DON, TIM/TOM, JAN/JEN all share a
# code. This uses a stricter key: spelling normalized to sound (PH=F, V=F,
# C=K or S, Q=K, X=KS, Z=S, Y=I, silent H, doubled letters, final E), then the
# first vowel kept exactly and the later vowels dropped. Two names sound alike
# when their keys are equal. JEFFERY/JEFFREY, KAREN/KARIN, CHRISTINA/KRISTINA
# pass; DAN/DON and JAN/JEN do not.
#
# Reads  SYNTHID_DIR/partvii-v3_1/name-variants/first-name-variants-candidates.csv
# Writes dev/name-variants/first-name-sound-alikes.csv
suppressMessages({library(data.table); library(stringdist)})
ev <- file.path(Sys.getenv("SYNTHID_DIR", "C:/Users/jlecy/Documents/SYNTHID"), "partvii-v3_1", "name-variants")
c <- fread(file.path(ev, "first-name-variants-candidates.csv"))

sound <- function(x) {
  x <- gsub("[^A-Z]", "", toupper(x))
  x <- gsub("PH", "F", x); x <- gsub("V", "F", x); x <- gsub("GH", "", x)
  x <- gsub("CK", "K", x); x <- gsub("C(?=[EIY])", "S", x, perl = TRUE); x <- gsub("C", "K", x)
  x <- gsub("Q", "K", x); x <- gsub("X", "KS", x); x <- gsub("Z", "S", x)
  x <- sub("^KN", "N", x); x <- sub("^WR", "R", x)
  x <- gsub("(?<=.)H", "", x, perl = TRUE)          # H after the first letter is silent (KATHY, CHRISTY, JOHN)
  x <- gsub("Y", "I", x)
  x <- gsub("(.)\\1+", "\\1", x)                    # doubled letters
  x <- sub("(IE|EI|EE|EA|EY|E)$", "I", x)            # final -IE/-EY/-EE sound like -I; drop other final E
  x <- sub("([^AEIOU])I$", "\\1I", x)
  x
}
skeleton <- function(k) {
  first_v <- regmatches(k, regexpr("[AEIOU]", k)); first_v[!nzchar(first_v)] <- ""
  pos <- regexpr("[AEIOU]", k)
  head <- ifelse(pos > 0, substr(k, 1, pos), k)
  tail <- ifelse(pos > 0, gsub("[AEIOU]", "", substring(k, pos + 1)), "")
  paste0(head, gsub("(.)\\1+", "\\1", tail))
}
c <- c[grepl("^[A-Z]+$", name_short) & grepl("^[A-Z]+$", name_long) & nchar(name_short) >= 3]
c[, `:=`(key_short = skeleton(sound(name_short)), key_long = skeleton(sound(name_long)))]
c[, soundex_same := phonetic(name_short) == phonetic(name_long)]
c[, sound_alike := key_short == key_long]
c[, len_gap := abs(nchar(name_short) - nchar(name_long))]

out <- c[sound_alike == TRUE][order(-n_emp_ids)]
out[, kind := fcase(gate == "excluded: rule" & type == "typo", "misspelling that sounds right",
                    gate == "excluded: rule", "excluded by a rule (check)",
                    startsWith(gate, "ready"), "already promote-ready",
                    startsWith(gate, "hold: red flag"), "sound-alike, red flag",
                    default = "sound-alike spelling")]
fwrite(out[, .(name_short, name_long, sound_key = key_long, n_emp_ids, type, gate, kind, flags,
               n_filings_together, freq_short, freq_long)],
       "dev/name-variants/first-name-sound-alikes.csv")

cat("Under-25 alphabetic pairs (short form 3+ letters):", nrow(c), "\n")
cat("  Soundex agrees:", sum(c$soundex_same), "\n")
cat("  Strict sound key agrees:", nrow(out), " (", sum(out$n_emp_ids), "EMP_ID rows )\n\n")
print(out[, .(pairs = .N, emp_ids = sum(n_emp_ids), seen_2plus = sum(n_emp_ids >= 2)), by = kind][order(-pairs)])
cat("\nTop 30 'sound-alike spelling' (not yet promote-ready):\n")
print(out[kind == "sound-alike spelling"][1:30, .(name_short, name_long, n_emp_ids, gate)])
cat("\nSoundex agrees but strict key does not, top 20 (what the strict key rejects):\n")
print(c[soundex_same & !sound_alike][order(-n_emp_ids)][1:20, .(name_short, name_long, n_emp_ids, key_short, key_long)])
