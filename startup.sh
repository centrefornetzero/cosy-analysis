# ----- Mount GCS bucket (idempotent) -----
/usr/bin/gcsfuse --implicit-dirs \
    --rename-dir-limit=100 \
    --max-conns-per-host=100 \
    cnz-oe-extract-57d7be9d0a /home/jupyter/gcs


# switch to more recent version of R
conda init
conda activate r-4.4

