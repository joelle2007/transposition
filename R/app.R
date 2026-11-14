#' Launch the interactive application
#'
#' Opens a Shiny application with three tabs. The first one lets the user
#' type a matrix and shows T_ij, A and the product side by side. The second
#' one ranks zones by power-cut priority: zones can be typed, loaded from a
#' CSV file or from a small SQLite database, and the swaps T_ij used for the
#' sort are shown step by step. The third one describes the method.
#'
#' @return No return value, called for its side effect.
#' @examples
#' if (interactive()) {
#'   lancer_app()
#' }
#' @export
lancer_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required: install.packages('shiny')",
         call. = FALSE)
  }
  base_ok <- requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)

  demo <- transposition::coupures_bertoua

  onglet_tij <- shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput("nr", "Rows of A", 4, min = 1, max = 8),
      shiny::numericInput("nc", "Columns of A", 3, min = 1, max = 8),
      shiny::radioButtons("cote", "Swap", c("rows", "columns")),
      shiny::numericInput("i", "i", 1, min = 1),
      shiny::numericInput("j", "j", 3, min = 1),
      shiny::actionButton("aleatoire", "Fill A with random values")
    ),
    shiny::mainPanel(
      shiny::h5("Type the values of A:"),
      shiny::uiOutput("grille_A"),
      shiny::plotOutput("produit", height = "380px"),
      shiny::textOutput("explication")
    )
  )

  onglet_bertoua <- shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h5("1. Add a zone by hand"),
      shiny::textInput("z_nom", "Zone name", ""),
      shiny::numericInput("z_f", "Cuts per month", 8, min = 0),
      shiny::numericInput("z_d", "Average duration (hours)", 2, min = 0),
      shiny::numericInput("z_g", "Severity (1 to 5)", 3, min = 1, max = 5),
      shiny::numericInput("z_n", "People interviewed", 10, min = 0),
      shiny::actionButton("ajouter", "Add zone"),
      shiny::actionButton("retirer", "Remove last"),
      shiny::actionButton("demo", "Demo data"),
      shiny::hr(),
      shiny::h5("2. Or load a CSV file"),
      shiny::fileInput("fichier", NULL, accept = ".csv"),
      shiny::radioButtons("sep", "Separator",
                          c("Semicolon (;)" = ";", "Comma (,)" = ",")),
      shiny::hr(),
      shiny::h5("3. Database"),
      shiny::uiOutput("base_ui"),
      shiny::hr(),
      shiny::h5("4. Weights of the score"),
      shiny::sliderInput("w_f", "Frequency", 0, 10, 1, step = 1),
      shiny::sliderInput("w_d", "Duration", 0, 10, 1, step = 1),
      shiny::sliderInput("w_g", "Severity", 0, 10, 1, step = 1),
      shiny::downloadButton("telecharger", "Download the ranking")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Ranking",
                        shiny::plotOutput("classement_graphique"),
                        shiny::tableOutput("classement_table")),
        shiny::tabPanel("Swaps step by step",
                        shiny::uiOutput("etape_ui"),
                        shiny::plotOutput("etapes_graphique", height = "420px"),
                        shiny::verbatimTextOutput("etapes_texte")),
        shiny::tabPanel("Data", shiny::tableOutput("donnees_table"))
      )
    )
  )

  onglet_info <- shiny::fluidRow(shiny::column(
    8,
    shiny::h4("Transposition matrix"),
    shiny::p("T_ij = I_n - E_ii - E_jj + E_ij + E_ji (i different from j).",
             "T_ij A swaps rows i and j of A, A T_ij swaps columns i and j."),
    shiny::h4("Ranking of zones"),
    shiny::p("Each variable is divided by its maximum, then the weighted",
             "mean gives a score between 0 and 1. The zones are sorted by a",
             "sequence of row swaps, each one a product by a matrix T_ij."),
    shiny::h4("Limits"),
    shiny::p("The data come from an exploratory survey on a small sample.",
             "The ranking shows tendencies, not a final diagnosis.")
  ))

  ui <- shiny::fluidPage(
    shiny::titlePanel("Transposition matrix T_ij"),
    shiny::tabsetPanel(
      shiny::tabPanel("Matrix Tij", onglet_tij),
      shiny::tabPanel("Bertoua", onglet_bertoua),
      shiny::tabPanel("About", onglet_info)
    )
  )

  server <- function(input, output, session) {
    # ---- Tab 1: typed matrix and product ----
    output$grille_A <- shiny::renderUI({
      nr <- shiny::req(input$nr)
      nc <- shiny::req(input$nc)
      shiny::tagList(lapply(seq_len(nr), function(i) {
        shiny::div(style = "display:flex; gap:4px;",
                   lapply(seq_len(nc), function(j) {
                     id <- paste0("a_", i, "_", j)
                     v <- shiny::isolate(input[[id]])
                     if (is.null(v)) v <- (i - 1) * nc + j
                     shiny::numericInput(id, NULL, v, width = "75px")
                   }))
      }))
    })

    shiny::observeEvent(input$aleatoire, {
      for (i in seq_len(input$nr)) {
        for (j in seq_len(input$nc)) {
          shiny::updateNumericInput(session, paste0("a_", i, "_", j),
                                    value = sample(1:20, 1))
        }
      }
    })

    A <- shiny::reactive({
      nr <- shiny::req(input$nr)
      nc <- shiny::req(input$nc)
      M <- matrix(0, nr, nc)
      for (i in seq_len(nr)) {
        for (j in seq_len(nc)) {
          v <- input[[paste0("a_", i, "_", j)]]
          shiny::req(!is.null(v), !is.na(v))
          M[i, j] <- v
        }
      }
      M
    })

    verifier <- function() {
      n <- if (input$cote == "rows") input$nr else input$nc
      shiny::validate(
        shiny::need(!is.na(input$i) && !is.na(input$j), "Enter i and j"),
        shiny::need(input$i == round(input$i) && input$j == round(input$j),
                    "i and j must be integers"),
        shiny::need(input$i != input$j, "i and j must be different"),
        shiny::need(input$i >= 1 && input$i <= n &&
                      input$j >= 1 && input$j <= n,
                    paste("i and j must be between 1 and", n))
      )
      n
    }

    output$produit <- shiny::renderPlot({
      n <- verifier()
      M <- A()
      i <- input$i
      j <- input$j
      Tm <- matrice_T(n, i, j)
      nom_T <- paste0("T", i, ",", j, " (", n, " x ", n, ")")
      old <- graphics::par(mfrow = c(1, 3))
      on.exit(graphics::par(old))
      if (input$cote == "rows") {
        dessiner_grille(Tm, nom_T)
        dessiner_grille(M, "A", marquer_l = c(i, j))
        dessiner_grille(Tm %*% M, "T x A", marquer_l = c(i, j))
      } else {
        dessiner_grille(M, "A", marquer_c = c(i, j))
        dessiner_grille(Tm, nom_T)
        dessiner_grille(M %*% Tm, "A x T", marquer_c = c(i, j))
      }
    })

    output$explication <- shiny::renderText({
      verifier()
      i <- input$i
      j <- input$j
      if (input$cote == "rows") {
        paste0("Row ", i, " of T has its 1 in column ", j, ": row ", i,
               " of the result is row ", j, " of A, and conversely.")
      } else {
        paste0("Column ", i, " of T has its 1 in row ", j, ": column ", i,
               " of the result is column ", j, " of A, and conversely.")
      }
    })

    # ---- Tab 2: data sources ----
    zones <- shiny::reactiveVal(demo)

    shiny::observeEvent(input$demo, zones(demo))

    shiny::observeEvent(input$ajouter, {
      nom <- trimws(input$z_nom)
      vals <- c(input$z_f, input$z_d, input$z_g, input$z_n)
      if (nom == "") {
        shiny::showNotification("Enter a zone name", type = "error")
        return()
      }
      if (anyNA(vals) || any(vals < 0)) {
        shiny::showNotification("Values must be non-negative numbers",
                                type = "error")
        return()
      }
      nouvelle <- data.frame(quartier = nom, frequence = input$z_f,
                             duree = input$z_d, gravite = input$z_g,
                             nb_reponses = input$z_n,
                             stringsAsFactors = FALSE)
      d <- zones()
      if (!"nb_reponses" %in% names(d)) d$nb_reponses <- NA
      zones(rbind(d[, names(nouvelle)], nouvelle))
      shiny::updateTextInput(session, "z_nom", value = "")
    })

    shiny::observeEvent(input$retirer, {
      d <- zones()
      if (nrow(d) > 0) zones(d[-nrow(d), , drop = FALSE])
    })

    shiny::observeEvent(list(input$fichier, input$sep), {
      shiny::req(input$fichier)
      d <- tryCatch(
        utils::read.csv(input$fichier$datapath, sep = input$sep,
                        dec = if (input$sep == ";") "," else ".",
                        fileEncoding = "UTF-8-sig", stringsAsFactors = FALSE),
        error = function(e) NULL)
      if (is.data.frame(d)) {
        zones(d)
      } else {
        shiny::showNotification("The file could not be read", type = "error")
      }
    })

    output$base_ui <- shiny::renderUI({
      if (!base_ok) {
        return(shiny::helpText("Database tools missing: ",
                               "install.packages(c('DBI', 'RSQLite'))"))
      }
      shiny::tagList(
        shiny::actionButton("base_save", "Save to database"),
        shiny::actionButton("base_load", "Load from database"),
        shiny::helpText(chemin_base())
      )
    })

    shiny::observeEvent(input$base_save, {
      tryCatch({
        base_ecrire(zones())
        shiny::showNotification("Saved to the database")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$base_load, {
      d <- tryCatch(base_lire(), error = function(e) NULL)
      if (is.data.frame(d)) {
        zones(d)
        shiny::showNotification("Loaded from the database")
      } else {
        shiny::showNotification("The database is empty", type = "error")
      }
    })

    # ---- Tab 2: ranking ----
    poids <- shiny::reactive(c(frequence = input$w_f, duree = input$w_d,
                               gravite = input$w_g))

    classement <- shiny::reactive({
      d <- zones()
      shiny::validate(shiny::need(nrow(d) >= 2, "Add at least two zones"))
      res <- tryCatch(classer_zones(d, poids()),
                      error = function(e) conditionMessage(e))
      shiny::validate(shiny::need(is.data.frame(res), res))
      res
    })

    output$classement_graphique <- shiny::renderPlot({
      visualiser_classement(classement(), "Priority ranking of zones")
    })
    output$classement_table <- shiny::renderTable(classement())
    output$donnees_table <- shiny::renderTable(zones())
    output$telecharger <- shiny::downloadHandler(
      filename = function() "classement_bertoua.csv",
      content = function(file) {
        utils::write.csv2(classement(), file, row.names = FALSE)
      }
    )

    # ---- Tab 2: swaps step by step ----
    paires <- shiny::reactive(attr(classement(), "echanges"))

    output$etape_ui <- shiny::renderUI({
      shiny::sliderInput("etape", "Swaps applied", 0,
                         max(1, length(paires())), 0, step = 1,
                         animate = shiny::animationOptions(interval = 900))
    })

    etat <- shiny::reactive({
      shiny::req(!is.null(input$etape))
      classement()
      M <- matrice_depart(zones(), poids())
      p <- paires()
      n <- nrow(M)
      s <- min(input$etape, length(p))
      if (s == 0) {
        return(list(s = 0, avant = M, apres = M, Tm = diag(n),
                    pr = NULL, n = n))
      }
      for (t in seq_len(s - 1)) M <- echange(M, p[[t]][1], p[[t]][2])
      pr <- p[[s]]
      list(s = s, avant = M, apres = echange(M, pr[1], pr[2]),
           Tm = matrice_T(n, pr[1], pr[2]), pr = pr, n = n)
    })

    output$etapes_graphique <- shiny::renderPlot({
      et <- etat()
      old <- graphics::par(mfrow = c(1, 3), mar = c(3, 7, 3, 1))
      on.exit(graphics::par(old))
      nom_T <- if (et$s == 0) "T = identity (no swap yet)" else
        paste0("T", et$pr[1], ",", et$pr[2], " (", et$n, " x ", et$n, ")")
      dessiner_grille(et$avant, paste("Before (step", max(et$s - 1, 0), ")"),
                      marquer_l = et$pr, zlim = c(0, 1))
      dessiner_grille(et$Tm, nom_T, zlim = c(0, 1))
      dessiner_grille(et$apres, paste("After (step", et$s, ") = T x before"),
                      marquer_l = et$pr, zlim = c(0, 1))
    })

    output$etapes_texte <- shiny::renderPrint({
      et <- etat()
      cat("Swaps applied:", et$s, "of", length(paires()), "\n")
      if (et$s > 0) {
        a <- rownames(et$avant)[et$pr[1]]
        b <- rownames(et$avant)[et$pr[2]]
        cat("Step ", et$s, ": '", a, "' (row ", et$pr[1], ") and '", b,
            "' (row ", et$pr[2], ") are exchanged by T", et$pr[1], ",",
            et$pr[2], ".\n", sep = "")
      }
    })
  }

  shiny::shinyApp(ui, server)
}

# ---- Internal helpers (not exported) ----

# Draw a matrix as a coloured grid, with optional red frames
dessiner_grille <- function(M, titre = "", marquer_l = NULL,
                            marquer_c = NULL, zlim = NULL) {
  M <- as.matrix(M)
  nr <- nrow(M)
  nc <- ncol(M)
  if (is.null(zlim)) zlim <- range(M)
  if (diff(zlim) == 0) zlim <- zlim + c(-1, 0)
  graphics::image(seq_len(nc), seq_len(nr), t(M)[, nr:1, drop = FALSE],
                  col = grDevices::colorRampPalette(c("white", "#9ecae1"))(12),
                  zlim = zlim, axes = FALSE, xlab = "", ylab = "",
                  main = titre)
  noms_l <- if (is.null(rownames(M))) seq_len(nr) else rownames(M)
  noms_c <- if (is.null(colnames(M))) seq_len(nc) else colnames(M)
  graphics::axis(1, at = seq_len(nc), labels = noms_c, cex.axis = 0.8)
  graphics::axis(2, at = seq_len(nr), labels = rev(noms_l), las = 1,
                 cex.axis = 0.8)
  graphics::box()
  g <- expand.grid(r = seq_len(nr), c = seq_len(nc))
  graphics::text(g$c, nr - g$r + 1, labels = round(M[cbind(g$r, g$c)], 2),
                 cex = max(0.5, min(1, 8 / max(nr, nc))))
  for (r in marquer_l) {
    graphics::rect(0.5, nr - r + 0.5, nc + 0.5, nr - r + 1.5,
                   border = "#d62728", lwd = 3)
  }
  for (k in marquer_c) {
    graphics::rect(k - 0.5, 0.5, k + 0.5, nr + 0.5,
                   border = "#d62728", lwd = 3)
  }
}

# Matrix (score, normalised variables) in the original order of the zones
matrice_depart <- function(d, w) {
  vars <- c("frequence", "duree", "gravite")
  num <- as.matrix(d[, vars])
  mx <- apply(num, 2, max)
  mx[mx == 0] <- 1
  N <- sweep(num, 2, mx, "/")
  w <- w[vars] / sum(w[vars])
  M <- cbind(score = drop(N %*% w), N)
  rownames(M) <- as.character(d$quartier)
  M
}

# Small SQLite database kept in the user's data folder
chemin_base <- function() {
  file.path(tools::R_user_dir("transposition", "data"), "coupures.sqlite")
}

base_ecrire <- function(d) {
  chemin <- chemin_base()
  dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), chemin)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "coupures", d, overwrite = TRUE)
}

base_lire <- function() {
  chemin <- chemin_base()
  if (!file.exists(chemin)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), chemin)
  on.exit(DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, "coupures")) return(NULL)
  DBI::dbReadTable(con, "coupures")
}
