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

Navigate to the `simulations` folder and open `execute_process.R`. This fully annotated script guides you step-by-step:

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

## Post-processing

After simulations complete, analyze results and generate figures/tables using scripts in the `Post_processing` folder.

We recommend users first execute the between level script, as the within level takes significantly longer to process. Moreover, the between level reults are available on Github, but the within level results are not.



---

Following these steps ensures accurate replication of thesis results and provides flexibility to extend or adapt the study as needed.
