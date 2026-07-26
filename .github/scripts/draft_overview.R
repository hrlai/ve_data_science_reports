# draft_overview.R
#
# Rewrites two sections of docs/soil_progress_report.qmd:
#   1. "## Lastest description"  — LLM-assisted summary via ellmer
#   2. '## Required elements ("Waiting for")' — deterministic from open PRs
#
# Usage (locally):  Rscript .github/scripts/draft_overview.R
# Usage (CI):       called from weekly_soil_report.yml
#
# Requires: gh, dplyr, purrr, lubridate, glue, ellmer
# Needs:    GITHUB_TOKEN, GOOGLE_API_KEY env vars

library(gh)
library(dplyr)
library(purrr)
library(lubridate)
library(glue)
library(ellmer)

owner <- "ImperialCollegeLondon"
repo <- "ve_data_science"
user <- "hrlai"
qmd <- "docs/soil_progress_report.qmd"

# ── Shared helper ─────────────────────────────────────────────────────────────

splice_section <- function(lines, heading, new_content) {
  # Replaces content between `heading` and the next `## ` heading (exclusive)
  start <- which(lines == heading)
  if (length(start) != 1) {
    stop("Could not find heading: ", heading)
  }
  ends <- which(grepl("^## ", lines))
  end <- min(ends[ends > start]) - 1L
  c(
    lines[seq_len(start - 1)],
    heading,
    "",
    new_content,
    "",
    lines[seq(end + 1, length(lines))]
  )
}

# ── Read the current .qmd ────────────────────────────────────────────────────
lines <- readLines(qmd)

# ══════════════════════════════════════════════════════════════════════════════
# Section B — LLM-assisted: rewrite "## Lastest description"
# ══════════════════════════════════════════════════════════════════════════════

since_7d <- format(Sys.time() - days(7), "%Y-%m-%dT%H:%M:%SZ")

or_else <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

bind_rows_or_empty <- function(x, .f, empty) {
  if (length(x) == 0) {
    return(empty)
  }

  map_dfr(x, .f)
}

normalize_excerpt <- function(x, width = 140) {
  x <- or_else(x, "")
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)

  if (identical(x, "")) {
    return(NA_character_)
  }

  if (nchar(x) > width) {
    paste0(substr(x, 1, width - 1), "…")
  } else {
    x
  }
}

issue_number_from_url <- function(x) {
  as.integer(sub(".*/issues/([0-9]+)$", "\\1", x))
}

# Fetch recently merged PRs (last 7 days, by hrlai)
raw_merged_prs <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    state = "closed",
    sort = "updated",
    direction = "desc",
    .limit = Inf
  ),
  error = function(e) list()
)

merged_prs <- bind_rows_or_empty(
  raw_merged_prs,
  \(p) {
    tibble(
      number = p$number,
      title = p$title,
      html_url = p$html_url,
      author = p$user$login,
      merged_at = or_else(p$merged_at, NA_character_)
    )
  },
  empty = tibble(
    number = integer(),
    title = character(),
    html_url = character(),
    author = character(),
    merged_at = character()
  )
) |>
  filter(
    author == user,
    !is.na(merged_at),
    ymd_hms(merged_at) >= ymd_hms(since_7d)
  )

# Fetch issues closed in the last 7 days (by hrlai)
raw_closed_issues <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    state = "closed",
    since = since_7d,
    .limit = Inf
  ),
  error = function(e) list()
)

closed_issues <- bind_rows_or_empty(
  raw_closed_issues,
  \(i) {
    tibble(
      number = i$number,
      title = i$title,
      html_url = i$html_url,
      author = i$user$login,
      is_pr = !is.null(i$pull_request)
    )
  },
  empty = tibble(
    number = integer(),
    title = character(),
    html_url = character(),
    author = character(),
    is_pr = logical()
  )
) |>
  filter(author == user, !is_pr)

# Fetch issues opened in the last 7 days (by hrlai)
raw_new_issues <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    state = "open",
    since = since_7d,
    .limit = Inf
  ),
  error = function(e) list()
)

new_issues <- bind_rows_or_empty(
  raw_new_issues,
  \(i) {
    tibble(
      number = i$number,
      title = i$title,
      html_url = i$html_url,
      author = i$user$login,
      created_at = i$created_at,
      is_pr = !is.null(i$pull_request)
    )
  },
  empty = tibble(
    number = integer(),
    title = character(),
    html_url = character(),
    author = character(),
    created_at = character(),
    is_pr = logical()
  )
) |>
  filter(
    author == user,
    !is_pr,
    ymd_hms(created_at) >= ymd_hms(since_7d)
  )

# Fetch PRs opened in the last 7 days (by hrlai)
raw_opened_prs <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    state = "open",
    sort = "created",
    direction = "desc",
    .limit = Inf
  ),
  error = function(e) list()
)

opened_prs <- bind_rows_or_empty(
  raw_opened_prs,
  \(p) {
    tibble(
      number = p$number,
      title = p$title,
      html_url = p$html_url,
      author = p$user$login,
      created_at = p$created_at
    )
  },
  empty = tibble(
    number = integer(),
    title = character(),
    html_url = character(),
    author = character(),
    created_at = character()
  )
) |>
  filter(author == user, ymd_hms(created_at) >= ymd_hms(since_7d))

# Fetch recent issue comments and recent issue metadata for enrichment
raw_recent_items <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    state = "all",
    since = since_7d,
    .limit = Inf
  ),
  error = function(e) list()
)

recent_items <- bind_rows_or_empty(
  raw_recent_items,
  \(i) {
    tibble(
      number = i$number,
      title = i$title,
      html_url = i$html_url,
      is_pr = !is.null(i$pull_request)
    )
  },
  empty = tibble(
    number = integer(),
    title = character(),
    html_url = character(),
    is_pr = logical()
  )
)

raw_issue_comments <- tryCatch(
  gh(
    "/repos/{owner}/{repo}/issues/comments",
    owner = owner,
    repo = repo,
    since = since_7d,
    sort = "updated",
    direction = "desc",
    .limit = Inf
  ),
  error = function(e) list()
)

issue_comments <- bind_rows_or_empty(
  raw_issue_comments,
  \(comment) {
    tibble(
      comment_id = comment$id,
      issue_number = issue_number_from_url(comment$issue_url),
      comment_url = comment$html_url,
      comment_author = comment$user$login,
      created_at = comment$created_at,
      body_excerpt = normalize_excerpt(comment$body)
    )
  },
  empty = tibble(
    comment_id = integer(),
    issue_number = integer(),
    comment_url = character(),
    comment_author = character(),
    created_at = character(),
    body_excerpt = character()
  )
) |>
  filter(comment_author == user, ymd_hms(created_at) >= ymd_hms(since_7d)) |>
  left_join(recent_items, by = c("issue_number" = "number")) |>
  filter(is.na(is_pr) | !is_pr) |>
  mutate(
    title = coalesce(title, paste("Issue", issue_number)),
    html_url = coalesce(
      html_url,
      glue("https://github.com/{owner}/{repo}/issues/{issue_number}")
    )
  ) |>
  select(issue_number, title, html_url, comment_url, body_excerpt, created_at)

# Build context summary for the LLM
fmt_items <- function(
  df,
  number_col = "number",
  title_col = "title",
  url_col = "html_url"
) {
  if (nrow(df) == 0) {
    return("(none)")
  }
  map_chr(seq_len(nrow(df)), function(i) {
    glue(
      "  - #{df[[number_col]][i]}: {df[[title_col]][i]} ({df[[url_col]][i]})"
    )
  }) |>
    paste(collapse = "\n")
}

fmt_comment_items <- function(df) {
  if (nrow(df) == 0) {
    return("(none)")
  }

  map_chr(seq_len(nrow(df)), function(i) {
    excerpt <- if (is.na(df$body_excerpt[i])) {
      ""
    } else {
      glue(" — {df$body_excerpt[i]}")
    }

    glue(
      "  - #{df$issue_number[i]}: {df$title[i]}{excerpt} ({df$comment_url[i]})"
    )
  }) |>
    paste(collapse = "\n")
}

context_block <- glue(
  "Merged PRs (last 7 days):\n{fmt_items(merged_prs)}\n\n",
  "Closed issues (last 7 days):\n{fmt_items(closed_issues)}\n\n",
  "Newly opened issues (last 7 days):\n{fmt_items(new_issues)}\n\n",
  "Opened PRs (last 7 days):\n{fmt_items(opened_prs)}\n\n",
  "Issue comments by {user} (last 7 days):\n{fmt_comment_items(issue_comments)}"
)

prompt <- glue(
  "
  You are helping an ecologist draft the opening paragraph of a weekly progress report.

  Write a single paragraph of 3 to 5 sentences in the first person. Keep the tone terse, professional, and factual. Do not use self-congratulation, filler, bullet points, headings, or markdown.

  Base the paragraph only on the source material provided below. Summarize both recent achievements (based on closed Issues and PRs) and near-term planned focus (based on open Issues, comments and open PRs). Organize the content implicitly by grouping related work where appropriate across these categories: input data, model parameterisation, calibration, validation, and general tools. Do this naturally in prose rather than by naming the categories mechanically if that would make the paragraph awkward.

  When an issue, pull request, or issue comment is relevant, reference the actual number exactly as given, for example #123. If the output format supports links, render each issue or PR number as a hyperlink to its URL and use the issue or PR title as hover text; if the output format does not support that, keep the plain reference number and do not invent formatting. If a recent issue references older issues or PRs that materially matter for context, mention those older references briefly and only if they help explain the current work.

  Treat issue comments as evidence of recent discussion, clarification, or coordination. Do not present commenting activity by itself as a completed outcome unless the surrounding source material supports that interpretation.

  If the source material shows that a task involved learning a new method, tool, or workflow, mention that briefly, especially when it appears relevant for shared team learning. Do not infer progress, intent, or significance beyond what the source material supports.

  Prioritize specificity, compression, and accurate grouping over completeness. If the source material is thin or uneven, write a restrained paragraph that reflects that rather than overfilling gaps.
  \n\n
  ",
  "{context_block}"
)

description_content <- tryCatch(
  {
    chat <- ellmer::chat_google_gemini()
    response <- chat$chat(prompt)
    trimws(response)
  },
  error = function(e) {
    message("LLM call failed: ", conditionMessage(e))
    "*[Auto-draft failed — please update manually.]*"
  }
)

lines <- splice_section(lines, "## Lastest description", description_content)

# ══════════════════════════════════════════════════════════════════════════════
# Section A — Deterministic: rewrite '## Required elements ("Waiting for")'
# ══════════════════════════════════════════════════════════════════════════════

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

waiting_for_content <- c(
  build_bullets(filter(prs, urgent), "- Urgent"),
  build_bullets(filter(prs, !urgent), "- Not urgent")
)

lines <- splice_section(
  lines,
  '## Required elements ("Waiting for")',
  waiting_for_content
)

# ── Write back ────────────────────────────────────────────────────────────────
writeLines(lines, qmd)
message(
  "Updated '",
  qmd,
  "' — Lastest description and Waiting for sections refreshed."
)
