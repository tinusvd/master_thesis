# Reproducing Simulation Results for Evaluating Causal Dominance Hypotheses in Multilevel VAR(1) Models

This README provides guidance for reproducing the simulation results presented in the thesis titled: **"How to Evaluate Causal Dominance Hypotheses in Multilevel Vector Autoregressive Models."** All necessary scripts, tables, and figures used in the thesis are included, along with instructions for researchers conducting their own simulation studies.

## Folder Structure Overview

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

⚠️ **Note:** The following steps (2-7) are only required if you are **not** using the pre-provided data from this repository.

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
   * HTML tables are pre-included in this repository; accessing tables in R provides LaTeX code.

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
