# app.R — One-sample t-test (Intro Stats) • Evaluate a completed test
# -------------------------------------------------------------------
# Practical mode: students enter the observed t and n from their test.
# The app computes d_hat = t/sqrt(n), shows power at the observed n,
# and plots Power vs n assuming that effect (for replication planning).
# Optional: override with a SESOI (small/medium/large) instead of d_hat.
# SESOI mode shows an orange "required n for 80% power" marker.

# Detect Shinylive/webR (Wasm)
is_webr <- grepl("wasm", R.version$platform)

# Install needed packages in the browser (no-op on desktop R)
if (requireNamespace("webr", quietly = TRUE)) {
  webr::install(c(
    "shiny",
    "bslib",
    "ggplot2",
    "scales",
    "munsell"
  ))
}

library(shiny)
library(ggplot2)
library(bslib)

# Optional theming helper: avoid runtime Google font downloads in Shinylive
# In Shinylive we use system font stacks; on desktop we use Google fonts.
if (!is_webr) {
  # Only load thematic + Google fonts when NOT in Shinylive
  if (requireNamespace("thematic", quietly = TRUE)) {
    thematic::thematic_shiny()
  }
  base_font_val <- bslib::font_google("Lexend")
  code_font_val <- bslib::font_google("Recursive")
} else {
  # System font stacks work everywhere and require no downloads
  base_font_val <- "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif"
  code_font_val <- "ui-monospace, SFMono-Regular, Menlo, Consolas, 'Liberation Mono', monospace"
}

# --- Tokens ---------------------------------------------------------------
LIGHT_BLUE <- "#767da2"  # inputs + slider + borders
ORANGE     <- "#cd6118"  # brand / primary

# --- Theme + CSS ----------------------------------------------------------
dark_theme <- bs_theme(
  version   = 5,
  bg        = "#1d1f2b",
  fg        = "#e1e2ea",
  primary   = ORANGE,
  secondary = "#5a5f80",
  base_font = base_font_val,
  code_font = code_font_val
)

css_rules <- paste(
  ".container-fluid { max-width: 1100px; }",
  ".shiny-input-container { margin-bottom: 10px; }",
  "/* Top breathing room + iOS safe-area support */",
  ".container-fluid{ max-width: 1100px; padding-top: 16px;",
  "  padding-top: calc(16px + env(safe-area-inset-top)); }",
  "@media (max-width: 576px){ .container-fluid{ padding-top: 20px;",
  "  padding-top: calc(20px + env(safe-area-inset-top)); } }",
  "/* Title size */",
  "h2.title { font-size: 20px; line-height: 1.25; margin: 6px 0 10px; font-weight: 600; }",
  "@media (max-width: 576px) { h2.title { font-size: 18px; } }",
  "/* Side-by-side layout that stacks on mobile */",
  ".no-stack { display: flex; gap: 16px; align-items: flex-start; }",
  ".no-stack .left  { flex: 0 0 320px; min-width: 260px; }",
  ".no-stack .right { flex: 1 1 auto; min-width: 0; }",
  "@media (max-width: 576px) {",
  "  .no-stack { flex-direction: column; gap: 12px; }",
  "  .no-stack .left, .no-stack .right { flex: 1 1 100%; min-width: 0; }",
  "  .container-fluid { padding-left: 12px; padding-right: 12px; }",
  "}",
  sprintf("#plotBox{ background: var(--bs-body-bg) !important; border: 1.5px solid %s !important; padding: 12px; border-radius: .6rem; box-shadow: none; }", LIGHT_BLUE),
  sprintf(".form-control, .form-select, .selectize-input{ background-color: %s !important; color: var(--bs-body-color) !important; border: 1px solid %s !important; box-shadow: none !important; }", LIGHT_BLUE, LIGHT_BLUE),
  sprintf(".form-control:focus, .form-select:focus, .selectize-input.focus{ border-color: %s !important; outline: 0 !important; box-shadow: 0 0 0 .15rem rgba(205,97,24,.35) !important; }", ORANGE),
  ".selectize-dropdown{ background: #2b2f49 !important; color: var(--bs-body-color) !important; border-color: #5a5f80 !important; }",
  ".selectize-dropdown .active, .selectize-dropdown .option:hover{ background: #5a5f80 !important; color: #ffffff !important; }",
  "/* Slider typography + thickness */",
  ".irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-min, .irs--shiny .irs-max { font-size: 16px; font-weight: 600; }",
  ".irs--shiny .irs-grid-text { font-size: 14px; }",
  ".irs--shiny .irs-line, .irs--shiny .irs-bar, .irs--shiny .irs-bar-edge { height: 6px; }",
  sprintf(".irs--shiny .irs-bar, .irs--shiny .irs-handle>i:first-child, .irs--shiny .irs-handle{ background: %s !important; border-color: %s !important; }", LIGHT_BLUE, LIGHT_BLUE),
  ".irs--shiny .irs-line, .irs--shiny .irs-bar-edge{ background: #5a5f80 !important; border-color: #5a5f80 !important; }",
  ".irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-grid-text{ color: var(--bs-body-color) !important; }",
  "/* Orange-when-checked checkboxes */",
  "input.form-check-input:checked{",
  sprintf("  background-color: %s !important; border-color: %s !important;", ORANGE, ORANGE),
  "  box-shadow: none !important;",
  "}",
  "/* Compact tables */",
  "#rule_table table, #summary_table table { font-size: 13px; }",
  "#rule_table table th, #rule_table table td, #summary_table table th, #summary_table table td { padding: 4px 8px; line-height: 1.6; }",
  "@media (max-width: 576px){ #rule_table table, #summary_table table { font-size: 12px; } }",
  "/* Align table header baselines across the two tables */",
  "#rule_table table thead th,",
  "#summary_table table thead th {",
  "  white-space: nowrap;",
  "  font-size: 12px;",
  "  line-height: 1.1;",
  "  padding-top: 4px;",
  "  padding-bottom: 6px;",
  "  vertical-align: bottom;",
  "}",
  "@media (max-width: 576px){",
  "  #rule_table table thead th, #summary_table table thead th { font-size: 11px; }",
  "}",
  "/* Bottom breathing room + iOS safe-area */",
  ".container-fluid{ padding-bottom: 18px; padding-bottom: calc(18px + env(safe-area-inset-bottom)); }",
  "#plotBox{ padding-bottom: 16px; margin-bottom: 10px; }",
  sep = "\n", collapse = "\n"
)
dark_theme <- bs_add_rules(dark_theme, css_rules)

# --- Helper: one-sample power --------------------------------------------
power_one_sample <- function(d, n, alpha = 0.05) {
  stats::power.t.test(
    n = n, delta = d, sd = 1,
    sig.level = alpha, type = "one.sample", alternative = "two.sided"
  )$power
}

# --- UI -------------------------------------------------------------------
ui <- fluidPage(
  theme = dark_theme,
  div(
    class = "no-stack",
    
    # LEFT PANEL ------------------------------------------------------------
    div(
      class = "left",
      h2(class = "title", "One-sample t-test: Evaluate a completed test"),
      
      numericInput("t_obs", "Observed t-statistic", value = 2.0, step = 0.1),
      numericInput("n_obs", "Observed sample size n", value = 30, min = 3, step = 1),
      
      checkboxInput("use_sesoi", "Use smallest effect size of interest (override observed)", value = FALSE),
      conditionalPanel(
        condition = "input.use_sesoi",
        tagList(
          selectInput(
            "sesoi", "Choose SESOI (Cohen's d)",
            choices  = c("Small (0.2)" = 0.2, "Medium (0.5)" = 0.5, "Large (0.8)" = 0.8),
            selected = 0.5, selectize = FALSE
          ),
          tags$div(
            style = "font-size:12.5px; opacity:.9; margin-top:-6px;",
            "When SESOI is on, the curve ignores the observed t and uses this d to plan sample size."
          )
        )
      ),
      
      selectInput("alpha", HTML("Significance level (&alpha;)"),
                  choices = c(0.10, 0.05, 0.01), selected = 0.05),
      
      tags$details(
        tags$summary("What does this show?"),
        tags$ul(
          tags$li(HTML("Observed effect (standardized): <code>d̂ = t/√n</code>.")),
          tags$li(HTML("Power at the observed n is computed using <code>d̂</code> (or SESOI if selected).")),
          tags$li(HTML(paste0(
            "The curve shows power vs n for a 'fixed effect':",
            "<br>&nbsp;&nbsp;&bull; with SESOI off this 'fixed effect' is <code>d̂ = t/√n</code> from your observed test;",
            "<br>&nbsp;&nbsp;&bull; with SESOI on, the 'fixed effect' is your chosen SESOI <code>d</code>. The plot shows the required <code>n</code> for 80% power."
          )))
        )
      )
    ),
    
    # RIGHT PANEL -----------------------------------------------------------
    div(
      class = "right",
      uiOutput("power_card"),
      uiOutput("link_card"),
      div(id = "plotBox", plotOutput("power_plot", height = "360px")),
      br(),
      fluidRow(
        column(6, tableOutput("rule_table")),
        column(6, tableOutput("summary_table"))
      )
    )
  )
)

# --- Server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive state: observed, effect used for power, and validity guard
  rv <- reactive({
    n_obs <- suppressWarnings(as.integer(input$n_obs))
    t_obs <- suppressWarnings(as.numeric(input$t_obs))
    alpha <- suppressWarnings(as.numeric(input$alpha))
    
    ok <- is.finite(n_obs) && n_obs >= 3 &&
      is.finite(t_obs) && is.finite(alpha)
    
    d_hat <- if (ok) t_obs / sqrt(n_obs) else NA_real_
    use_d <- if (!ok) {
      NA_real_
    } else if (isTRUE(input$use_sesoi)) {
      as.numeric(input$sesoi)
    } else {
      d_hat
    }
    
    list(
      n_obs = n_obs, t_obs = t_obs, alpha = alpha,
      d_hat = d_hat, d_eff = use_d,
      ok = ok
    )
  })
  
  # Power card at observed n (guarded for NA while typing)
  output$power_card <- renderUI({
    x <- rv()
    if (!isTRUE(x$ok)) {
      return(
        wellPanel(
          tags$div(HTML(
            "<div style='font-size:1.1em'>Enter a numeric t and n ≥ 3 to see power.</div>"
          ))
        )
      )
    }
    
    pow <- power_one_sample(x$d_eff, x$n_obs, x$alpha)
    label <- if (pow < 0.5) "Low" else if (pow < 0.8) "Moderate" else "High"
    bg <- if (pow < 0.5) "#603b53" else if (pow < 0.8) "#6b5735" else "#2f4740"
    
    src_label <- if (isTRUE(input$use_sesoi)) {
      sprintf("SESOI d = %.2f", x$d_eff)
    } else {
      sprintf("Observed d̂ = %.2f (from t = %.2f, n = %d)", x$d_hat, x$t_obs, x$n_obs)
    }
    
    wellPanel(
      tags$div(
        style = paste0("background:", bg, "; padding:14px; border-radius:8px;"),
        HTML(sprintf(
          "<div style='font-size:1.2em'><b>Estimated power at n = %d</b></div>
           <div style='font-size:2em; line-height:1.2; margin-top:4px'><b>%0.1f%%</b> — %s</div>
           <div style='margin-top:6px'><small>%s; α = %.2f (two-sided)</small></div>",
          x$n_obs, 100*pow, label, src_label, x$alpha
        ))
      )
    )
  })
  
  # Link card: mapping and interpretation (guarded for NA)
  output$link_card <- renderUI({
    x <- rv()
    if (!isTRUE(x$ok)) {
      return(
        wellPanel(HTML(
          "<div><b>Effect link</b></div>
         <div style='margin-top:4px'>t(n) = d × √n — enter t and n to see the mapping.</div>"
        ))
      )
    }
    
    if (isTRUE(input$use_sesoi)) {
      # SESOI ON: d is fixed at chosen SESOI
      msg <- sprintf(
        "<div><b>Effect link</b></div>
       <div style='margin-top:4px'>Curve assumes your SESOI: d = %.2f (fixed). Changing n moves the dot; the curve stays the same.</div>
       <div style='margin-top:4px'>t(n) = d × √n; at n = %d, implied t = %.3f.</div>",
        x$d_eff, x$n_obs, x$d_eff * sqrt(x$n_obs)
      )
    } else {
      # SESOI OFF: d-hat is recomputed from current t and n
      msg <- sprintf(
        "<div><b>Effect link</b></div>
       <div style='margin-top:4px'>Curve uses the observed effect d̂ = t/√n computed from your current inputs. If you change t or n, d̂ (and the curve) update.</div>
       <div style='margin-top:4px'>Current: d̂ = %.3f (t = %.3f, n = %d).</div>",
        x$d_hat, x$t_obs, x$n_obs
      )
    }
    
    wellPanel(HTML(msg))
  })
  
  # Plot: power vs n at fixed d_eff; SESOI shows required n for 80% power
  output$power_plot <- renderPlot({
    x <- rv()
    validate(need(isTRUE(x$ok), "Enter a numeric t and n ≥ 3 to see the power curve."))
    
    n_seq <- seq(5, 200, by = 1)
    pow <- vapply(n_seq, function(nn) power_one_sample(x$d_eff, nn, x$alpha), numeric(1))
    df  <- data.frame(n = n_seq, power = pow)
    
    pow_now <- power_one_sample(x$d_eff, x$n_obs, x$alpha)
    dot_col <- if (pow_now < 0.5) "#805e75" else if (pow_now < 0.8) "#8e7446" else "#3b6558"
    
    n_req <- if (isTRUE(input$use_sesoi)) {
      ceiling(stats::power.t.test(
        delta = x$d_eff, sd = 1, sig.level = x$alpha,
        type = "one.sample", alternative = "two.sided",
        power = 0.80
      )$n)
    } else NA_integer_
    
    x_breaks <- unique(sort(c(pretty(df$n), x$n_obs,
                              if (!is.na(n_req) && n_req <= max(n_seq)) n_req else NULL)))
    
    title_txt <- if (isTRUE(input$use_sesoi)) {
      sprintf("Power vs n (one-sample; SESOI d = %.2f, α = %.2f)", x$d_eff, x$alpha)
    } else {
      sprintf("Power vs n (one-sample; observed d̂ = %.2f, α = %.2f)", x$d_hat, x$alpha)
    }
    
    p <- ggplot(df, aes(n, power)) +
      # make all geoms white on dark bg
      geom_line(linewidth = 1, color = "white") +
      geom_hline(yintercept = 0.80, linetype = 2, color = "white") +
      geom_vline(xintercept = x$n_obs, linetype = 3, color = "white") +
      geom_point(
        data = data.frame(n = x$n_obs, power = pow_now),
        aes(n, power), size = 6, shape = 16, color = dot_col
      ) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_x_continuous(breaks = x_breaks) +
      labs(x = "Sample size (n)", y = "Power", title = title_txt) +
      theme(
        # transparent image bg (so page shows through)
        plot.background  = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA),
        
        # all text to white
        text       = element_text(color = "white"),
        axis.title = element_text(size = 16, face = "bold", color = "white"),
        axis.text  = element_text(size = 14, color = "white"),
        plot.title = element_text(size = 18, face = "bold", color = "white"),
        
        # grid/axes/ticks to white (use a softer white for grids)
        panel.grid.major = element_line(linewidth = 0.4, color = scales::alpha("white", 0.55)),
        panel.grid.minor = element_line(linewidth = 0.25, color = scales::alpha("white", 0.25)),
        axis.ticks       = element_line(color = "white")
      )
    
    if (!is.na(n_req)) {
      if (n_req <= max(n_seq)) {
        p <- p +
          geom_vline(xintercept = n_req, linetype = "dotted", linewidth = 0.9, color = "white") +
          annotate("label", x = n_req, y = 0.05,
                   label = sprintf("n ≈ %d @ 0.80 power", n_req),
                   size = 4.5, label.size = 0, fill = NA, color = ORANGE)
      } else {
        p <- p +
          annotate("label", x = max(n_seq) - 5, y = 0.05,
                   label = sprintf("n ≈ %d needed (off-plot) @ 0.80 power", n_req),
                   size = 4.5, label.size = 0, fill = NA, color = ORANGE)
      }
    }
    
    p
  }, bg = "transparent")
  
  
  
  # Reference tables (compact, no striping)
  output$rule_table <- renderTable({
    data.frame(
      `Effect Size`   = c("Large", "Small", "Small", "Large"),
      `Sample Size`   = c("Small", "Large", "Small", "Large"),
      `Likely Outcome`= c(
        "May still detect effect",
        "Likely to detect even small effect",
        "Low power — may miss real effect",
        "High chance of detecting effect"
      ),
      check.names = FALSE
    )
  }, bordered = TRUE, striped = FALSE, spacing = "xs")
  
  output$summary_table <- renderTable({
    data.frame(
      Factor = c("Effect Size", "Sample Size", "Power"),
      `What It Affects` = c(
        "Practical importance of the finding",
        "Precision and power of the test",
        "Ability to detect true effects"
      ),
      check.names = FALSE
    )
  }, bordered = TRUE, striped = FALSE, spacing = "xs")
}

# --- Run ------------------------------------------------------------------
shinyApp(ui, server)
