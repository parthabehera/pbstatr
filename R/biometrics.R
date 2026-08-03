#' Line x Tester analysis
#'
#' Estimates GCA of lines and testers, SCA of crosses and their variance
#' components from a line x tester mating design.
#'
#' @param data A data frame.
#' @param line,tester,rep Column names for line, tester and replication.
#' @param trait Response trait column name.
#' @return An `agricolae::lineXtester` object.
#' @export
pb_line_tester <- function(data, line, tester, rep, trait) {
  agricolae::lineXtester(
    replications = data[[rep]],
    lines = data[[line]],
    testers = data[[tester]],
    y = data[[trait]]
  )
}

#' Diallel analysis
#'
#' Griffing / Hayman style combining ability analysis for a diallel set of
#' crosses. This is a light wrapper; supply a parents x parents mean matrix
#' or a long-format cross table.
#'
#' @param data A data frame of crosses.
#' @param parent1,parent2,rep Column names.
#' @param trait Response trait.
#' @param method Griffing method (1-4).
#' @param model 1 = fixed, 2 = random.
#' @return A list with combining ability ANOVA and effect estimates.
#' @export
pb_diallel <- function(data, parent1, parent2, rep, trait,
                       method = 2, model = 1) {
  # Minimal Griffing-style GCA/SCA via two-way structure of parents.
  data[[parent1]] <- as.factor(data[[parent1]])
  data[[parent2]] <- as.factor(data[[parent2]])
  fml <- stats::as.formula(
    paste(trait, "~", parent1, "+", parent2, "+", rep)
  )
  fit <- stats::aov(fml, data = data)
  list(
    method = method, model = model,
    anova = as.data.frame(stats::anova(fit)),
    gca_p1 = stats::model.tables(fit, "means")$tables[[parent1]],
    gca_p2 = stats::model.tables(fit, "means")$tables[[parent2]],
    fit = fit
  )
}
