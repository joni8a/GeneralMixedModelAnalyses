test_that("all_intervals returns every pair in from-major order", {
  expect_equal(all_intervals(c(1.5, 3, 6)),
               list(c(1.5, 3), c(1.5, 6), c(3, 6)))
  expect_length(all_intervals(c(1.5, 3, 6, 12, 24, 60)), 15)   # choose(6, 2)
})

test_that("all_intervals keeps the type of its input", {
  expect_type(all_intervals(c(1.5, 3))[[1]], "double")
  expect_equal(all_intervals(c("0", "24", "60"))[[2]], c("0", "60"))
})

test_that("all_intervals rejects degenerate input", {
  expect_error(all_intervals(6), "at least 2")
  expect_error(all_intervals(c(1, 1, 2)), "duplicates")
})

test_that("consecutive_intervals is a strict subset of all_intervals", {
  fu <- c(1.5, 3, 6, 12, 24, 60)
  expect_true(all(consecutive_intervals(fu) %in% all_intervals(fu)))
  expect_length(consecutive_intervals(fu), 5)
})


# ---- fixture: a 2-group, 4-visit model with a known shape -----------------
fit_fixture <- function(seed = 42) {
  skip_if_not_installed("lme4")
  set.seed(seed)
  fu <- c(0, 6, 12, 24)
  d  <- expand.grid(ID = factor(1:40), Month = fu)
  d$Group <- ifelse(as.integer(d$ID) <= 20, "A", "B")
  # Group A climbs with Month, group B is flat: A should show change, B not.
  d$y <- ifelse(d$Group == "A", 0.05 * d$Month, 0) +
    rnorm(nrow(d), 0, 0.3) + rep(rnorm(40, 0, 0.3), times = length(fu))

  m <- lme4::lmer(y ~ as.factor(Group) * as.factor(Month) + (1 | ID), data = d)
  mc <- NewModelContainer(
    name = "fixture", predictor_variable = "y", lm = m, data = d, group = "Group",
    group_mapping = list(labels = c(A = "A", B = "B"), colors = c(A = "#000000", B = "#FF0000")),
    log_transform = FALSE, log_offset = 1
  )
  list(mc = mc, fu = fu)
}

test_that("table_change_matrix returns one flextable per group", {
  f <- fit_fixture()
  out <- table_change_matrix(f$mc, ~ Group * Month, f$fu)

  expect_named(out, c("A", "B"))
  expect_s3_class(out[[1]], "flextable")
})

test_that("the matrix is triangular and correctly shaped", {
  f <- fit_fixture()
  out <- table_change_matrix(f$mc, ~ Group * Month, f$fu)
  tab <- out[["A"]]$body$dataset

  n <- length(f$fu)
  expect_equal(nrow(tab), n - 1L)            # one row per "from"
  expect_equal(ncol(tab), n)                 # "From" + one column per "to"

  # Lower triangle empty: row i, column j is filled only when to > from.
  body <- as.matrix(tab[, -1, drop = FALSE])
  for (i in seq_len(nrow(body)))
    for (j in seq_len(ncol(body)))
      if (j < i) expect_identical(unname(body[i, j]), "")
      else       expect_true(nzchar(body[i, j]))
})

test_that("cells match a directly computed emmeans contrast", {
  f   <- fit_fixture()
  out <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "none")
  tab <- out[["A"]]$body$dataset

  # 0 -> 24 is the top-right cell.
  emm <- emmeans::emmeans(f$mc@lm, ~ Group * Month,
                          at = list(Month = f$fu))
  g   <- as.data.frame(emm)
  v   <- numeric(nrow(g))
  v[g$Group == "A" & g$Month == 24] <-  1
  v[g$Group == "A" & g$Month == 0]  <- -1
  ref <- as.data.frame(summary(emmeans::contrast(emm, list(d = v)),
                               infer = c(TRUE, TRUE)))

  # Without lmerTest attached emmeans falls back to asymptotic df and names the
  # interval asymp.LCL/asymp.UCL. The function normalises this via
  # .standardize_emm_cols(); the test has to cope with both spellings too.
  lo <- if ("lower.CL" %in% names(ref)) ref$lower.CL else ref$asymp.LCL
  hi <- if ("upper.CL" %in% names(ref)) ref$upper.CL else ref$asymp.UCL

  expect_equal(tab[1, ncol(tab)][[1]],
               sprintf("%.2f (%.2f to %.2f)", ref$estimate, lo, hi))
})

test_that("holm never reports a smaller p than unadjusted", {
  f     <- fit_fixture()
  prs   <- all_intervals(f$fu)
  # With by = "Group" the coefficients are per by-group, so each vector has one
  # entry per follow-up level -- exactly as table_change_matrix() builds them.
  custom <- stats::setNames(
    lapply(prs, function(pr) {
      v <- numeric(length(f$fu))
      v[f$fu == pr[2]] <-  1
      v[f$fu == pr[1]] <- -1
      v
    }), paste0("c", seq_along(prs)))

  p <- function(adj) {
    emm <- emmeans::emmeans(f$mc@lm, ~ Group * Month, at = list(Month = f$fu))
    summary(emmeans::contrast(emm, method = custom, by = "Group"),
            adjust = adj)$p.value
  }

  expect_true(all(p("holm") >= p("none") - 1e-12))
  expect_false(isTRUE(all.equal(p("holm"), p("none"))))   # adjustment does something
})

test_that("intervals stay unadjusted when p-values are adjusted", {
  # emmeans widens intervals when `adjust` is set (holm falls back to
  # Bonferroni-width). The cells must show the unadjusted interval regardless.
  f    <- fit_fixture()
  none <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "none")
  holm <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "holm")

  expect_equal(none[["A"]]$body$dataset, holm[["A"]]$body$dataset)
  expect_equal(none[["B"]]$body$dataset, holm[["B"]]$body$dataset)
})

test_that("bolding follows the adjusted p, not the interval", {
  f    <- fit_fixture()
  none <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "none")
  bonf <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "bonferroni")

  nbold <- function(ft) sum(unlist(ft$body$styles$text$bold$data))
  # Same cells, but a stricter adjustment can only bold fewer of them.
  expect_lte(nbold(bonf[["A"]]), nbold(none[["A"]]))
})

test_that("the footnote names the adjustment and the family size", {
  f <- fit_fixture()

  holm <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "holm")
  note <- holm[["A"]]$footer$dataset[[1]][1]
  expect_match(note, "Holm-adjusted")
  expect_match(note, "6 visit pairs")          # choose(4, 2)

  none <- table_change_matrix(f$mc, ~ Group * Month, f$fu, adjust = "none")
  expect_match(none[["A"]]$footer$dataset[[1]][1], "unadjusted across")
})

test_that("an extra note is appended after the generated footnote", {
  f   <- fit_fixture()
  msg <- "Pairs spanning the 24-month visit cross a device change."
  out <- table_change_matrix(f$mc, ~ Group * Month, f$fu, note = msg)

  foot <- out[["A"]]$footer$dataset[[1]]
  expect_length(foot, 2)
  expect_match(foot[1], "Holm-adjusted")   # generated line stays first
  expect_identical(foot[2], msg)

  # and it reaches every matrix, including the between-group one
  both <- table_change_matrix(f$mc, ~ Group * Month, f$fu,
                              note = msg, include_between = TRUE)
  for (nm in names(both))
    expect_identical(both[[nm]]$footer$dataset[[1]][2], msg)
})

test_that("include_between adds exactly one table and needs 2 groups", {
  f <- fit_fixture()
  out <- table_change_matrix(f$mc, ~ Group * Month, f$fu, include_between = TRUE)

  expect_named(out, c("A", "B", "Difference in change"))
  expect_s3_class(out[["Difference in change"]], "flextable")
})

test_that("labels can be overridden", {
  f <- fit_fixture()
  out <- table_change_matrix(f$mc, ~ Group * Month, f$fu,
                             group_labels = c("Cemented", "Cementless"),
                             phase_labels = c("baseline", "6 mo", "1 yr", "2 yr"))

  expect_named(out, c("Cemented", "Cementless"))
  expect_equal(out[["Cemented"]]$body$dataset$From, c("baseline", "6 mo", "1 yr"))
  expect_equal(names(out[["Cemented"]]$body$dataset)[-1], c("6 mo", "1 yr", "2 yr"))
})

test_that("phase_labels of the wrong length is rejected", {
  f <- fit_fixture()
  expect_error(
    table_change_matrix(f$mc, ~ Group * Month, f$fu, phase_labels = c("a", "b")),
    "must have 4 entries")
})
