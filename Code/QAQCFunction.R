qa_qc_heights_final <- function(df) {
  library(dplyr)
  
  classify_increment <- function(x) {
    if (is.na(x)) return(NA)
    if (x > 0.4) return("Pos")
    if (x > 0) return("Pos_Stag")
    if (x < -0.4) return("Neg")
    if (x < 0) return("Neg_Stag")
    return("Zero")
  }
  
  df <- df %>%
    group_by(LindseyID) %>%
    mutate(
      Growth_prev = (Height - lag(Height)) / (Year - lag(Year)),
      Growth_next = (lead(Height) - Height) / (lead(Year) - Year),
      GrowthClass_prev = sapply(Growth_prev, classify_increment),
      GrowthClass_next = sapply(Growth_next, classify_increment),
      PercentDead_prev = lag(Percent_Dead),
      PercentDead_next = lead(Percent_Dead),
      Year_prev = lag(Year),
      Year_next = lead(Year),
      Flag = NA_character_,
      Diagnostic = NA_character_
    )
  
  df <- df %>% rowwise() %>%
    mutate(
      Flag = {
        flag_val <- NA_character_
        
        if (!is.na(Year_prev) && !is.na(Year_next)) {
          inc1 <- GrowthClass_prev
          inc2 <- GrowthClass_next
          pd_change <- Percent_Dead - PercentDead_prev
          
          Diagnostic <- paste(inc1, inc2, sep="-")
          
          # --- Primary rules: slope-based anomalies ---
          if (!is.na(inc1) && !is.na(inc2)) {
            if (inc1 == "Pos" && inc2 == "Neg") {
              flag_val <- "Anomalous Incline"
            } else if (inc1 == "Neg" && inc2 == "Pos" &&
                       (is.na(pd_change) || pd_change < 10)) {
              flag_val <- "Anomalous Decline"
            }
          }
          
          # --- Secondary rules: apply ONLY if slope flag already exists ---
          if (!is.na(flag_val) && !is.na(Percent_Dead)) {
            if (!is.na(Growth_prev) && Percent_Dead > 50 && Growth_prev > 0.5) {
              flag_val <- paste(flag_val, "(Unrealistic Jump: Majority Dead)")
            }
            if (Percent_Dead == 100 && !is.na(Growth_next) && Growth_next > 0) {
              flag_val <- paste(flag_val, "(Unrealistic Jump: Dead Tree)")
            }
          }
        }
        
        flag_val
      },
      Diagnostic = ifelse(is.na(Diagnostic),
                          paste(GrowthClass_prev, GrowthClass_next, sep="-"),
                          Diagnostic)
    ) %>% ungroup()
  
  return(df)
}
