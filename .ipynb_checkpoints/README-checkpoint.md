# cosy-analysis

Accessing the Environment
	1.	Open the Vertex AI Workbench instance (trials instance 2):
https://console.cloud.google.com/vertex-ai/workbench/instances?project=cnz-data-warehouse-d66eb552a5
	2.	Once the instance is active, click “Open JupyterLab”.

Folder Structure
cosy-analysis/
data/
data/input/    – raw input datasets
data/output/   – model outputs and results
data/scratch/  – temporary or intermediate files
scripts/    – all R scripts (main + numbered components)
graphs/     – generated figures and visualisations
tables/     – output tables for reports and papers

The main entry point is scripts/main.R, which orchestrates the full run.
Other scripts are numbered to indicate execution order. Note: scripts named  “cosy (2…)” currently runs first.

Quick Start
From a terminal in JupyterLab:
	1.	Mount the GCS bucket (before starting R):
gcsfuse –implicit-dirs 
–rename-dir-limit=100 
–max-conns-per-host=100 
cnz-oe-extract-57d7be9d0a “/home/jupyter/gcs”
	2.	Run the analysis in R:
cd cosy-analysis
R
source(“scripts/main.R”)

Notes
	•	Ensure the bucket is mounted at /home/jupyter/gcs before running the scripts.
	•	Outputs are written under data/output, and figures/tables under graphs and tables respectively.
