# draft_waiting_for.R
#
# Rewrites the "Required elements (Waiting for)" section of gh_progress_report.qmd
# by querying open PRs authored by hrlai that have pending reviewers.
#
# Usage (locally):   Rscript .github/scripts/draft_waiting_for.R
# Usage (CI):        called from weekly_soil_report.yml
#
# Requires: gh, dplyr, purrr, lubridate, glue
# Needs:    GITHUB_TOKEN env var (set automatically in Actions)

library(gh)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)

owner <- "ImperialCollegeLondon"
repo <- "ve_data_science"
user <- "hrlai"
qmd <- "analysis/soil/issue_tracker/gh_progress_report.qmd"

# ── Fetch open PRs with pending reviewers ─────────────────────────────────────
raw_prs <- gh(
  "/repos/{owner}/{repo}/pulls",
  owner = owner,
  repo = repo,
  state = "open",
  .limit = Inf
)

prs <-
  tibble(
    number = map_int(raw_prs, "number"),
    title = map_chr(raw_prs, "title"),
    html_url = map_chr(raw_prs, "html_url"),
    author = map_chr(raw_prs, c("user", "login")),
    created_at = map_chr(raw_prs, "created_at") |> ymd_hms(),
    reviewers = map(raw_prs, \(p) map_chr(p$requested_reviewers, "login"))
  ) |>
  filter(
    author == user,
    map_lgl(reviewers, \(r) length(r) > 0)
  ) |>
  mutate(
    age_days = as.numeric(difftime(Sys.time(), created_at, units = "days")),
    urgent = age_days > 7,
    reviewer_list = map_chr(reviewers, paste, collapse = ", ")
  ) |>
  arrange(desc(urgent), desc(age_days))

# ── Build replacement Markdown ────────────────────────────────────────────────
build_bullets <- function(df, label) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  items <- map_chr(seq_len(nrow(df)), \(i) {
    r <- df[i, ]
    glue(
      "  - {r$reviewer_list} to review PR [#{r$number}]({r$html_url}) — *{r$title}*"
    )
  })
  c(label, items)
}

new_section <- c(
  '## Required elements ("Waiting for")',
  "",
  build_bullets(filter(prs, urgent), "- Urgent"),
  build_bullets(filter(prs, !urgent), "- Not urgent"),
  ""
)

# ── Splice into qmd ───────────────────────────────────────────────────────────
lines <- readLines(qmd)

start <- which(grepl('^## Required elements', lines))
end <- which(grepl('^## ', lines))
end <- min(end[end > start]) - 1L

if (length(start) != 1) {
  stop("Could not find '## Required elements' section in qmd.")
}

lines <- c(
  lines[seq_len(start - 1)],
  new_section,
  lines[seq(end + 1, length(lines))]
)
writeLines(lines, qmd)

message("Updated '", qmd, "' with ", nrow(prs), " pending PR(s).")

# ── TODO: Hybrid ellmer summary (planning notes) ──────────────────────────────
# Context:
# - Keep current deterministic "Waiting for" generation as-is.
# - Add optional LLM-generated sections for:
#   1) planned work (from newly opened issues)
#   2) recent achievements/outcomes (from recent PR activity)
#
# Actionable options/tasks:
# 1) Prompt + schema design
#    - Define a strict prompt and fixed output template with sections:
#      "Planned work", "Recent achievements", and "Waiting for".
#    - Add validation checks; regenerate or fallback if required headings/bullets are missing.
#
# 2) Data context window
#    - Fetch bounded context (e.g., issues opened in last 7–14 days,
#      PRs merged/updated recently) to reduce noise and token usage.
#
# 3) Incremental implementation path
#    - Keep this script for deterministic waiting-for bullets.
#    - Add a separate script to generate only LLM-backed narrative sections.
#    - Splice only the targeted qmd section(s), not the whole report.
#
# 4) CI safety and reliability
#    - Run LLM step only on trusted events (schedule/workflow_dispatch).
#    - Store provider credentials in GitHub Secrets.
#    - Add timeout/error handling and deterministic fallback text when LLM fails.
#
# 5) Optional quality controls
#    - Track prompt/model version in comments for reproducibility.
#    - Keep generated text concise and require references to issue/PR numbers.
