library(shiny)
library(reactable)
library(magrittr)
library(Tplyr)
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)

## data loading ----

adsl <- readRDS(here::here('data', 'adsl.rds'))
adas <- readRDS(here::here('data', 'adas.rds'))

## work from breakout 3 prob. 5 ----

### basic table ----
# This table code is to support the primary efficacy table for CDISC Pilot table 14-3.01
t <- tplyr_table(adas, TRTP, where=EFFFL == "Y" & ITTFL == "Y" & PARAMCD == "ACTOT" & ANL01FL == "Y") %>%
  set_pop_data(adsl) %>%
  set_pop_treat_var(TRT01P) %>%
  set_pop_where(EFFFL == "Y" & ITTFL == "Y") %>%
  set_distinct_by(USUBJID) %>%
  set_desc_layer_formats(
    'n' = f_str('xx', n),
    'Mean (SD)' = f_str('xx.x (xx.xx)', mean, sd),
    'Median (Range)' = f_str('xx.x (xxx;xx)', median, min, max)
  ) %>%
  add_layer(
    group_desc(AVAL, where= AVISITN ==  0, by = "Baseline")
  ) %>%
  add_layer(
    group_desc(AVAL, where= AVISITN == 24, by = "Week 24")
  ) %>%
  add_layer(
    group_desc(CHG,  where= AVISITN == 24, by = "Change from Baseline")
  )

sum_data <- t %>%
  build(metadata = TRUE) %>%
  apply_row_masks() %>%
  select(row_id, starts_with('row_label'),
         var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`)

### efficacy mockup ----
#### actual results ----
# I don't need the full model code for this example so just mock it up.
# But if you want to see it, it's available here:
# https://github.com/RConsortium/submissions-pilot1/blob/694a207aca7e419513ffe16f6f5873526da1bdcb/R/eff_models.R#L17
model_portion <- tibble::tribble(
  ~"row_id",  ~"row_label1",                       ~"var1_Xanomeline Low Dose", ~"var1_Xanomeline High Dose",
  "x4_1",    "p-value(Dose Response) [1][2]",      "",                          "0.245",
  "x4_3",    "p-value(Xan - Placebo) [1][3]",	    "0.569",    	               "0.233",
  "x4_4",    "   Diff of LS Means (SE)",           "-0.5 (0.82)",               "-1.0 (0.84)",
  "x4_5",    "   95% CI",                          "(-2.1;1.1)",                "(-2.7;0.7)",
  "x4_7",    "p-value(Xan High - Xan Low) [1][3]", "",                          "0.520",
  "x4_8",    "   Diff of LS Means (SE)",           "",                          "-0.5 (0.84)",
  "x4_9",    "   95% CI",                          "",                          "(-2.2;1.1)"
)

results <- bind_rows(sum_data, model_portion) %>%
  mutate(
    across(where(is.character), ~ replace_na(., ""))
  )

#### fixup metadata for efficacy 'results' ----
meta <- tplyr_meta(
  names = quos(TRTP, AVISITN, AVISIT, PARAMCD, PARAM, AVAL),
  filters = quos(EFFFL == "Y", ITTFL == "Y", ANL01FL == "Y")
)

# Xan High / Placebo contrast
meta_xhp <- meta |>
  add_filters(quos(TRTP %in% c("Xanomeline High Dose", "Placebo")))

# Xan Low / Placbo Contrast
meta_xlp <- meta |>
  add_filters(quos(TRTP %in% c("Xanomeline Low Dose", "Placebo")))

# Xan High / Xan Low Contrast
meta_xlh <- meta |>
  add_filters(quos(TRTP %in% c("Xanomeline High Dose", "Xanomeline Low Dose")))

## Problem 6 - Create a metadata dataframe and bind it to the Tplyr table's metadata
# The metadata dataframe is pre-prepared for you
# Use append metadata to bind it to the Tplyr tables metadata
# View the updated metadata using get_metadata()
eff_meta <- tibble::tribble(
  ~"row_id",  ~"row_label1",                       ~"var1_Xanomeline Low Dose", ~"var1_Xanomeline High Dose",
  "x4_1",    "p-value(Dose Response) [1][2]",      NULL,                        meta,
  "x4_3",    "p-value(Xan - Placebo) [1][3]",	     meta_xlp,    	              meta_xhp,
  "x4_4",    "   Diff of LS Means (SE)",           meta_xlp,                    meta_xhp,
  "x4_5",    "   95% CI",                          meta_xlp,                    meta_xhp,
  "x4_7",    "p-value(Xan High - Xan Low) [1][3]", NULL,                        meta_xlh,
  "x4_8",    "   Diff of LS Means (SE)",           NULL,                        meta_xlh,
  "x4_9",    "   95% CI",                          NULL,                        meta_xlh
)


t <- append_metadata(t, eff_meta)

# shiny code ----

ui <- fillPage(
  reactableOutput("demoTab"),
  reactableOutput("demoList")
)

server <- function(input, output) {

  r_row <- reactive({
    i <- input$row$index

    req(rlang::is_scalar_integerish(i))

    results |>
      slice(i) |>
      pull(row_id)
  })

  r_col <- reactive({
    input$col$column
  })

  output$demoTab <- renderReactable(
    reactable(
      select(results, -row_id, -starts_with("ord")),
      sortable = FALSE,
      onClick = JS("function(rowInfo, colInfo) {
                      if (window.Shiny) {
                        Shiny.setInputValue('row', { index: rowInfo.index + 1 })
                        Shiny.setInputValue('col', { column: colInfo.id })
                        }
                    }"),
      height = 600,
      defaultPageSize = 21,
      columns = list(
        row_label1 = colDef(name = ""),
        row_label2 = colDef(name = ""),
        var1_Placebo = colDef(name = "Placebo"),
        `var1_Xanomeline Low Dose` = colDef(name = "Xano Low"),
        `var1_Xanomeline High Dose` = colDef(name = "Xano High")
      )
    )
  )

  sub_data <- reactive({
    row_id <- req(r_row())
    col_id <- req(r_col())

    get_meta_subset(x = t, row_id = row_id, column = col_id)
  })

  output$demoList <- renderReactable({
    req(sub_data()) |>
      reactable(
        data = _,
        sortable = FALSE,
        height = 450,
        defaultPageSize = 11,
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
