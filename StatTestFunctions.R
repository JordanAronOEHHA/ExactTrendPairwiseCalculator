# package needed for C-A exact trend test
library(CATTexact)

# Main function to call for calculating p-values for both pairwise and trend tests
# Calls respective helper functions for pairwise and trend tests
# @param incidence_vec: vector of tumor incidences for each dose group
# @param n_vec: vector of total number of animals for each dose group
# @param dose_vec: vector of dose levels corresponding to each group
CalculatePvals <- function(incidence_vec, n_vec, dose_vec){
    # Calculates and prints pairwise p-values for each dose compared to control
    PairwiseHelper(incidence_vec, n_vec, dose_vec)

    # Calculates and prints the C-A exact trend test p-value
    TrendHelper(incidence_vec, n_vec, dose_vec)
}

# Helper function to calculate the C-A exact trend test p-value using the CATTexact package
# @param incidence_vec: vector of tumor incidences for each dose group
# @param n_vec: vector of total number of animals for each dose group
# @param dose_vec: vector of dose levels corresponding to each group
CalculateTrendPval <- function(incidence_vec, n_vec, dose_vec, tail = "upper"){

    #library only returns right tail
    #need to calculate opposite and return right tail (thus getting left tail)
    if (tail == "lower"){
        return(catt_exact(dose_vec,n_vec,n_vec - incidence_vec)$exact.pvalue)
    }
    return(catt_exact(dose_vec,n_vec,incidence_vec)$exact.pvalue)
}

# Helper function to calculate and print the C-A exact trend test p-value
# @param incidence_vec: vector of tumor incidences for each dose group
# @param n_vec: vector of total number of animals for each dose group
# @param dose_vec: vector of dose levels corresponding to each group
TrendHelper <- function(incidence_vec, n_vec, dose_vec) {

    # Calculate the C-A exact trend test p-value using the CATTexact package
    trend_pval <-CalculateTrendPval(incidence_vec, n_vec, dose_vec)
    cat("C-A exact trend test: ")

    #Branching logic to determine how to report the p-value
    if (trend_pval < 0.001) {
        cat("P-value of ", trend_pval, "is <.001. Report <.001")
    } else if (trend_pval < 0.01) {
        cat("P-value of ", round(trend_pval,3), "is <.01 and >.001. Report <.01")
    } else if (trend_pval < 0.05) {
        cat("P-value of ", round(trend_pval,3), "is <.05 and >.01. Report <.05")
    } else {
        cat("P-value of ", round(trend_pval,3), "is >.05. Report NS")
    }
}

# Helper function to calculate pairwise p-values for each dose compared to control
# @param incidence_vec: vector of tumor incidences for each dose group
# @param n_vec: vector of total number of animals for each dose group
CalculatePairwisePval <- function(incidence_vec, n_vec){
    ndoses <- length(incidence_vec)

    # Initialize a vector of length #doses to store the p-values for each dose compared to control, first element is emply on purpose 
    pval_vec <- numeric(ndoses)
    for (index in 2:ndoses){

        # Construct a 2x2 contingency table for the current dose compared to control
        # Needs counts of both tumor and no tumor for control and dose groups, so need to subtract incidence from total n to get no tumor counts
        working_pairwise_table <- data.frame( 
            "No Tumor" = c(n_vec[1] - incidence_vec[1], n_vec[index] - incidence_vec[index]), 
            "Tumor" = c(incidence_vec[1],incidence_vec[index]), 
            row.names = c("Control", "Dose"), 
            stringsAsFactors = FALSE
            )

        # Calculate the Fisher's exact test p-value for the current dose compared to control and store it in the pval_vec
        pval_vec[index] <- fisher.test(working_pairwise_table, alternative = "greater")$p.value
    }
    return(pval_vec)
}

# Helper function to print pairwise p-values for each dose compared to control, calls CalculatePairwisePval to get the p-values 
# @param incidence_vec: vector of tumor incidences for each dose group
# @param n_vec: vector of total number of animals for each dose group
# @param dose_vec: vector of dose levels corresponding to each group
PairwiseHelper <- function(incidence_vec, n_vec, dose_mgkg){
    ndoses <- length(incidence_vec)

    # Calculates pairwise p-values for each dose compared to control and stores them in a vector using helper function
    pval_vec <- CalculatePairwisePval(incidence_vec, n_vec)

    # Branching logic to determine how to report the p-values for each dose compared to control
    # Iterates through all pairwise p-values
    for (index in 2:ndoses){
        cat("Fisher paiwrise comparison of dose ", dose_mgkg[index], "mg/kg to control: ")
        if (pval_vec[index] < 0.001) {
            cat("P-value of ", pval_vec[index], "is <.001. Report <.001 \n")
        } else if (pval_vec[index] < 0.01) {
            cat("P-value of ", round(pval_vec[index],3), "is <.01 and >.001. Report <.01 \n")
        } else if (pval_vec[index] < 0.05) {
            cat("P-value of ", round(pval_vec[index],3), "is <.05 and >.01. Report <.05 \n")
        } else {
            cat("P-value of ", round(pval_vec[index],3), "is >.05. Report NS \n")
        }

    }
}