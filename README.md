# Documentation
This project contains the utility to prepare the data into trainable images for stylegan3.

`deepl_backend`, `deepl_downloader`, `image_cutter` are ocaml projects. I have used an opam switch for each of them, but a single one might work aswell.
Once all dependencies are setup, each project can be run from the `deepl_tools/` directory using `dune exec <deepl_backend | deepl_downloader | image_cutter>`, depending on which project is run.

### deepl_backend
This is the project running the primary web application running the demo. It needs to be started together with `model_service`. The default address will be `http://localhost:9000`.
The project is using ocaml with the dream web framework. The frontend is html together with htmx.

### model_service
Used in conjunction with `deepl_backend`. This is a python project, so a virtual env is recommended. 
The model needs to be downloaded, since it's too big for github. The link can be found in `MODEL_DOWNLOAD_LINK.md`. Place the file in the root of the `model_service` directory and name it `deepl_model.pkl` ❗
Once setup and all the requirements are installed, it can be run with `python3 app.py`.

This project exposes a single endpoint accepting a seed number and returns a generated image.

### deepl_downloader
This is a simple app made to download the files from the `links_swiss10_dataset.csv`

### image_cutter
After all images have been downloaded to a directory, they can be split into smaller sized images for further processing. This requires `ImageMagick` to be installed and available. 
For this project `ImageMagick` was built from source combined with the modules for .tif and .png support.

After the images have been cut into smaller pieces they have been converted to .png in a non-destructive way.
Prior to running the stylegan3 for training the .png files are downsized using the `dataset_tool.py` out of the stylegan3 project.
