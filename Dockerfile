# Reproducible runtime for this research package.
# Fija una base estable; ajusta versiones si tu análisis requiere otras.
FROM mambaorg/micromamba:1.5.8-jammy

USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    make build-essential ca-certificates curl git \
    libgeos-dev libproj-dev libgdal-dev libudunits2-dev \
    libxml2-dev libssl-dev libfontconfig1-dev libfreetype6-dev \
    libharfbuzz-dev libfribidi-dev libpng-dev libtiff5-dev libjpeg-dev \
    r-base r-base-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /work
COPY environment/ /work/environment/

RUN micromamba install -y -n base -c conda-forge python=3.11 pip && \
    micromamba run -n base pip install -r environment/requirements.txt || true

RUN Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    Rscript -e "renv::restore(lockfile='environment/renv.lock', prompt=FALSE)" || true

COPY . /work/

# Sandbox profiles usados en producción: biostats-python, sdm-r
# Si necesitas paridad exacta con E2B, replicar el Dockerfile del profile.

CMD ["make", "reproduce"]
