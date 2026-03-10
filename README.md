## Project overview

This project investigates spatial learning and navigation in common shrews (*Sorex araneus*) across seasons and captivity conditions. Three behavioural tasks were used: a **spatial learning maze**, a **visual associative learning** task, and a **path integration** task.

In the spatial learning task, shrews navigated a multiple T-maze (50×50 cm, 13 decision points) to locate a food reward, performing ten consecutive trials. In the visual associative learning and path integration tasks, shrews moved freely in a square sand arena (114×114 cm, 100 wells) to locate a food item marked with a visual cue. Straightness of the path towards the food (*straightness_to_food*) indexes visual associative learning; straightness of the return path (*straightness_back*) indexes path integration. Animals were tested across summer, winter, and spring, and included both wild-caught and captive individuals.

## Data

You can find the data associated with this project at this link: https://osf.io/qa96m/overview
The OSF repository contains two folders. Download them and place them as follows:

```
The maze data folder                         → shrew-learning/maze/data/
The visual/path integration data folder      → shrew-learning/visual-cue_path-integration/data/
```

See repository structure for details.

The raw data in the repository is the following:

```
├── maze/
│   ├── data/
│   │   ├── raw/                          - track_*.csv, path_*.csv, mask_*.png, average_*.png
│   │   ├── processed/                    - empty, files will be created by running the code
│   │   └── results/                      - all_track.csv, other files will be created by running the code
└── visual-cue_path-integration/
    ├── data/
    │   ├── raw/                          - tracking.csv, coords.csv, trial_door.csv
    │   ├── processed/                    - empty, files will be created by running the code
    │   └── results/     
```


## Repository structure

```
shrew-learning/
├── shrew-learning.Rproj
├── renv.lock
├── .venv/
├── .gitignore
├── README.qmd
├── requirements.txt
├── utils.R
├── maze/
│   ├── data/
│   │   ├── raw/
│   │   ├── processed/
│   │   └── results/
│   └── scripts/
│       ├── maze_calculations.qmd 
│       ├── maze_tidy.R 
│       └── maze_models.R 
└── visual-cue_path-integration/
    ├── data/
    │   ├── raw/
    │   ├── processed/
    └── scripts/
        ├── data_cleaning.R   
        └── models.R 
```


## Requirements

- **R** ≥ 4.3
- **Python** ≥ 3.10
- R packages are managed with [`renv`](https://rstudio.github.io/renv/). The full list is recorded in `renv.lock`.
- Python packages are listed in `requirements.txt`.
- Python is called from within `maze_calculations.qmd` via `reticulate`. The virtual environment must be created at `.venv/` in the project root (see Setup below).


## Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/cecibaldoni/shrew-learning
   cd shrew-learning
   ```

2. Open `shrew-learning.Rproj` in RStudio. This sets the working directory to the project root automatically.

3. Restore R packages:
   ```r
   renv::restore()
   ```

4. We use a virtual environment to keep Python libraries contained. In the **Terminal** tab in RStudio (at the project root), run: 

   ```bash

   # Create the virtual environment
   python3 -m venv .venv

   # Upgrade pip and install requirements
   ./.venv/bin/python -m pip install --upgrade pip
   ./.venv/bin/python -m pip install -r requirements.txt
   
   ```


## How to run

Run scripts in the following order. Each script depends on the outputs of the previous one!

**Maze (spatial learning):**

1. `maze/scripts/maze_calculations.qmd` — processes raw tracking data, computes path metrics, writes `maze_results.csv`, `deviation_report.csv`, and `all_track.csv` to `maze/data/results/`. It uses `here::here(".venv")` to locate the environment automatically.
In RStudio, open the `.qmd` file and either run each chunck of code by clicking the small green arrow on the top right of the chunk, or by clicking "Run" on top of the Source panel and then `Run all Chunck Below`
2. `maze/scripts/maze_tidy.R` — loads CSVs, tidies data, computes speed, saves `maze.rds`, `deviations.rds`, `deviation_counts.rds`
3. `maze/scripts/maze_models.R` — loads RDS files, fits Bayesian models, produces plots

**Visual associative learning and path integration:**

4. `visual-cue_path-integration/scripts/data_cleaning.R` — processes raw tracking data, computes trajectory metrics and speed, builds door memory dataset, saves `visual_assoc_data.rds`, `path_integration_data.rds`, `door_result.rds`
5. `visual-cue_path-integration/scripts/models.R` — loads RDS files, fits Bayesian models, produces plots

