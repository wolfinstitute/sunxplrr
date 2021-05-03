#' @title Trace and visualize workflow
#'
#' @description Trace and visualize workflow
#'
#' @author [Christoph Sax](mailto:christoph@christophsax.com)
#' @export
trace_flow <- function(expr) {
  op <- options(trace.env = new.env())
  on.exit(options(op)) # restore options on exit
  assign("TRACE_DF", NULL, envir = getOption("trace.env", NULL))

  eval(expr, env = sys.frame(-1))

  TRACE_DF <- get("TRACE_DF", envir = getOption("trace.env", NULL))

  # options(trace.env = NULL)
  as.data.frame(TRACE_DF)
}


# DF <- structure(list(df = c("bevoelkerung", "zivilstand", "rentenbetrag",
# "param", "bevoelkerung", "zivilstand", "rentenbetrag", "rentensumme_total",
# "rentensumme_nach_typ", "rentensumme_total", "param", "rentensumme_power",
# "rentensumme_power"), at = c("input", "input", "input", "input",
# "input", "input", "input", "output", "output", "input", "input",
# "output", "output"), mod = c("calc_all", "calc_all", "calc_all",
# "calc_all", "calc_beitrag", "calc_beitrag", "calc_beitrag", "calc_beitrag",
# "calc_beitrag", "calc_power", "calc_power", "calc_power", "calc_all"
# ), nframe = c(5L, 5L, 5L, 5L, 9L, 9L, 9L, 9L, 9L, 14L, 14L, 14L,
# 5L)), .Names = c("df", "at", "mod", "nframe"), row.names = c(1L,
# 6L, 10L, 13L, 15L, 20L, 24L, 27L, 29L, 36L, 38L, 40L, 42L), class = "data.frame")

#' @title Draw workflow
#'
#' @description Draw workflow
#'
#' @author [Christoph Sax](mailto:christoph@christophsax.com)
#' @export
draw_flow <- function(DF) {
  if (nrow(DF) == 0) stop("DF has no rows.")

  # sufficient, as long as we don't use var names
  DF <- unique(mutate(DF, var = NULL))

  if (length(unique(DF$nframe)) > 1) {
    top.level <- min(DF$nframe)
    TOP_LEVEL <- filter(DF, nframe == top.level)
    DF0 <- filter(DF, nframe > top.level)
  } else {
    TOP_LEVEL <- DF
    DF0 <- DF
  }

  ndf0 <- rbind(
    data.frame(nodes = unique(DF0$mod), class = "mod", stringsAsFactors = FALSE),
    data.frame(nodes = unique(DF0$df), class = "df", stringsAsFactors = FALSE)
  )

  ndf0$shape <- "box"
  ndf0$shape[ndf0$class == "mod"] <- "ellipse"

  top.level.output <- filter(TOP_LEVEL, at == "output")$df
  top.level.input <- filter(TOP_LEVEL, at == "input")$df

  ndf0$fillcolor <- "red"
  ndf0$fillcolor[ndf0$nodes %in% top.level.input] <- "LightGreen"
  ndf0$fillcolor[ndf0$nodes %in% top.level.output] <- "blue"

  ndf0$fillcolor[ndf0$class == "mod"] <- "DarkGreen"
  ndf0$style <- "filled"

  ndf <- DiagrammeR::create_node_df(
    n = nrow(ndf0),
    type = "type_1",
    label = ndf0$nodes,
    shape = ndf0$shape,
    fillcolor = ndf0$fillcolor,
    style = "filled"
  )

  DF0_OUT <- filter(DF0, at == "output")
  DF0_IN <- filter(DF0, at == "input")

  edf0 <- rbind(
    data.frame(from = DF0_IN$df, to = DF0_IN$mod, stringsAsFactors = FALSE),
    data.frame(from = DF0_OUT$mod, to = DF0_OUT$df, stringsAsFactors = FALSE)
  )

  edf <- DiagrammeR::create_edge_df(
    from = match(edf0$from, ndf$label),
    to = match(edf0$to, ndf$label)
  )

  DiagrammeR::create_graph(
    nodes_df = ndf,
    edges_df = edf,
    directed = TRUE,
    attr_theme = NULL
  ) %>%
    add_global_graph_attrs(
      attr = "overlap",
      value = "true",
      attr_type = "node"
    ) %>%
    # DiagrammeR::set_global_graph_attrs("node", "fontname", "Helvetica") %>%
    # DiagrammeR::add_global_graph_attrs("node", "fontname", "Helvetica") %>%
    DiagrammeR::render_graph()
}
