# installing base image
FROM jacobmur/shiny_base:1.0.0

# set the workdir
ENV HOME=/home/app
WORKDIR $HOME

# setting python path env
ENV PYTHONPATH="/home/app"

# creating file structure
RUN mkdir -p $HOME/scripts

# scripts
COPY --chown=app: scripts/app.R $HOME/scripts/app.R

CMD ["Rscript", "./scripts/app.R"]
