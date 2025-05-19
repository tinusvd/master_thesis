# Repository for Master's Thesis: Evaluating Causal Dominance Hypotheses in Multilevel VAR(1) Models

This README provides guidance for reproducing the results presented in the Master's thesis titled: **"How to Evaluate Causal Dominance Hypotheses in Multilevel Vector Autoregressive Models,"** by M.L.G. van Dam, supervised by Dr. Rebecca M. Kuiper. This thesis is part of the Methodology and Statistics for the Behavioural, Biomedical, and Social Sciences program at Utrecht University (UU).

The repository contains all necessary scripts, tables, figures, and instructions required for reproducing the results and conducting your own simulation studies.

## Software and Dependencies

For detailed software versions and package dependencies, refer to `./data_archive/requirements.csv`.

## Hardware

The data were generated on a MacBook Air with Apple M3 CPU. Software (and versions) are presented in the `requirements.csv` file.
The simulations were conducted on a server with Ampere Altra Q80-30 CPus (80 cores) and 128 GB or memory (RRAM). 


## Ethical Approval and Data Management

This study has received ethical approval from the Ethical Review Board of the Faculty of Social and Behavioural Sciences at Utrecht University (UU), under case number **24-1988**. Approval was granted based on submitted research protocols, addressing ethical considerations, data management practices, and privacy issues in compliance with the General Data Protection Regulation (GDPR). Approval is valid through **12 May 2025**.

### Privacy and Consent
- No empirical data was collected for this simulation study. Hence, informed consent procedures were not applicable.
- The empirical example provided in this repository consists of generated data.


## Permission and Access

* **Archive Access**: Freely available on [GitHub](https://github.com/tinsuvd/master_thesis).
* **Responsible Researcher**: M.L.G. van Dam.
* **Archive Availability**: Freely accessible online indefinitely.
* **Contact Information**: [m.l.g.vandam@uu.nl](mailto:m.l.g.vandam@uu.nl)

---

## Folder Structure Overview

* **Data\_archive**:

  * Contains R scripts to reproduce the simulation study results.

* **Empirical\_example**:

  * Contains R scripts and data for conducting analyses with multilevel VAR(1) models.
  * Includes evaluation of hypotheses using the Generalized Order-Restricted Information Criterion Approximation (GORICA) at between- and within-subject levels.

* **Supplementary\_materials**:

  * Includes additional methodological explanations, tables, and figures supporting the thesis.

## Study Design

This thesis evaluates the performance of the **Generalized Order-Restricted Information Criterion Approximation (GORICA)**—an extension of the AIC for testing informative hypotheses—in multilevel bivariate first-order vector autoregressive (VAR(1)) models.

The simulation systematically varies:

* Number of subjects (**N**)
* Number of measurement occasions (**T**)
* Population parameter values
* Hypothesis sets:

  * Two sets of causal dominance hypotheses
  * "About-equality" hypotheses specifying ranges for parameter differences

In total, 56 unique conditions were assessed, each replicated with 500 generated datasets. Hypotheses were tested independently at the between- and within-subject levels, with results stored and processed separately for each level. Each level has dedicated R scripts.


- **Simulations**: Contains scripts for:
  - Installing required packages
  - Creating directories
  - Generating multilevel bivariate VAR(1) data
  - Running simulations in parallel

- **Post_processing**: Includes scripts for processing simulation results after data analysis.

- **Meta_files**: Contains essential meta-information files used during data generation and post-processing.

- **Figures_tables**: Contains all figures and tables (in HTML format) from the thesis and supplementary materials.

## How to Run the Simulations

### Step 1: Review Simulation Conditions

- Load `./data_archive/meta_conditions.csv` (semicolon-delimited) into R to view all simulation conditions.

### Step 2: Set Up the Simulation Environment

Navitage tot the main folder and open `master_thesis.Rproj'. This ensures all R scripts with source code will open the correct file paths.

Then, navigate to the `simulations` folder and open `execute_process.R`. This fully annotated script guides you step-by-step:

1. **Load required packages**:
   - Source the file: `01_initialization.R` (located in `./data_archive/simulations/scripts`).

2. **Create directories**:
   - Source the file: `02_make_directories.R`.

3. **Generate simulation data**:
   - Source the file: `03_create_data.R`.
   - By default, data for all conditions will be generated. To generate only selected conditions, adjust line `177` in `03_create_data.R`.
   - ⚠️ **Note**: Data generation for all conditions takes approximately **60 minutes**.
   - Seed values and stationarity checks are logged in:  
     `data_archive/simulations/conditions_data/logs`.

### Step 3: Configure Parallel Computation

- Check available cores using the `DetectCores` function (line `18` in `execute_process.R`).
- Keep at least 2 cores free to ensure system stability.
- Update line `21` in `execute_process.R` with the value that you have to subtract from your total numbers of cores to keep 2 cores free.  
  *(Example: If you have 8 cores, enter `2`.)*

### Step 4: Run Simulations

- Run the final line in `execute_process.R` to start parallel processing.
- ⚠️ **Warning**: Computational time can be extremely long:
  - With a typical 8-core desktop or laptop, full simulations take around **2-3 months**.
  - To limit simulations and reduce runtime, adjust condition indices in  
    `./data_archive/simulations/scripts/02_make_directories.R` (lines `36-37`).

## Post-processing Simulation Results

After completing the simulations, analyze the results and generate the figures and tables by using the scripts located in the `Post_processing` folder.

We recommend that users first process the between-subject level results, as these require significantly less computational time. Additionally, the between-subject results are already included in this repository, whereas the within-subject results are not.

### Step-by-Step Guide for Between-Subject Level

Navigate to the `post_processing` folder and open the script `execute_between_level.R`. Follow the steps below:

1. **Load simulation results (optional)**:

   * Uncomment **line 10** to load the `final_conditions` object. This object includes the between-subject simulation results used in the thesis. Perform this step if you did not generate simulation data yourself and wish to directly run the provided post-processing scripts.

⚠️ **Note:** The following steps (2-6) are only required if you are **not** using the pre-provided data from this repository.

2. **Combine simulation results**:

   * Execute **line 14** to aggregate between-subject results from all datasets. This operation takes approximately **30-45 minutes**.
   * The resulting file is saved to:
     `./post_processing/intermediate_output/between_level/combined/combined_results.csv`.

3. **Prepare benchmark evaluation**:

   * Execute **line 18** to source the benchmark candidate function. After sourcing, run the subsequent lines to identify benchmark candidates based on pre-specified population values (do not alter these values).

4. **Generate GORICA objects**:

   * Execute **lines 28-29** to run the function `load_data_gorica`. This step prepares necessary inputs for benchmark evaluation.

5. **Benchmark evaluation**:

   * Execute **lines 33-45** to perform benchmark evaluations and combine the results. This process typically requires **20-30 minutes**.
   * By default, each benchmark performs 5,000 iterations. Adjust the iteration number in the benchmark script (`./data_archive/post_processing/source_functions/FUNCTION_APPLY_BENCH.R`) to reduce computational time, if needed.

6. **Calculate boundary-adjusted values**:

   * Execute **line 48** to create boundary-adjusted values essential for calculating boundary-adjusted True Hypothesis Rates (THR).
   * Save the results by running **lines 54-57**.
   * ⚠️ **Important:** Verify that the object `output_combined` contains the correct file path. This object is automatically created after executing line 14. If the path is incorrect, adjust the object manually or replace it directly in the `write_csv` function.

7. **Generate summary statistics**:

   * Execute **line 61** to generate summaries required for tables and plots.

8. **Create tables**:

   * Execute **line 65** to source the table-generation functions and create tables.
   * Tables are available as `.ltx` objects in your R environment and saved as `.html` files in:
     `./data_archive/figures_tables/tables/between_level/`.
   * HTML tables are pre-included in this repository.
   * Accessing tables in R provides LaTeX code for the tables.

9. **Create figures**:

   * Execute **line 70** to source the plot-generation functions and produce THR and boundary-adjusted THR plots. These figures correspond to the ones presented in the thesis.
   * For best visualization, maximize the height and set the width to **9.42 inches** in the RStudio plots pane.
   * All thesis figures are already available in:
     `./data_archive/figures_tables/plots/between_level/`.

10. **Supplementary material plots**:

    * Execute the final line of the script to generate supplementary figures.
    * These figures are already included in:
      `./data_archive/figures_tables/plots/supplementary_material/`.

---

### Within-Subject Level Post-processing

The steps for processing within-subject results follow the same structure using the `execute_within_level.R` script. However, please note:

⚠️ **Important considerations for within-subject level:**

* Processing time is substantially longer. Generating the combined results (`final_conditions_wl`) takes at least **1.5 hours**.
* Creating hypothesis summaries requires at least **3.5 GB of RAM**.

---

Following these steps ensures accurate reproduction of the results presented in the thesis and allows flexibility for extending or adapting the analyses.
