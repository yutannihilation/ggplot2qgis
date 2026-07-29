# QGIS helper scripts

Development tools, not part of the package: they answer "does QGIS read
this project, and does it draw what the plot drew?" without opening the
GUI. They need a QGIS installation with its Python bindings (QGIS 4.2 is
what they were written against).

Run them directly:

```sh
tools/qgis/render_project.py project.qgs out.png
```

`qgis.core` only imports in the interpreter QGIS ships with, so each
script calls `qgis_env.relaunch_if_needed()` before importing it: under a
plain `python3` the script re-executes itself under QGIS's Python with
the environment that needs (on macOS the bundled interpreter, its
standard library and its PROJ database live in three different places
inside the .app). Where QGIS is packaged for the system Python, the
import works and nothing is relaunched.

Set `QGIS_APP=/Applications/QGIS-x.y.app` to choose an install; the
default is the newest one in /Applications.

For a one-off script without that preamble, `qgis-python` runs it under
the same environment:

```sh
tools/qgis/qgis-python probe.py project.qgs
```

## render_project.py

Renders a `.qgs` to a PNG at a given size and resolution, and lists each
layer with its validity and renderer — so a project that reads but draws
nothing is visible. `--measure` reports the bounding box of what was
drawn, in pixels and millimeters.

Comparing a written project against the plot it came from:

```sh
# The R side: write the project and render the same plot to a PNG.
Rscript -e 'write_qgs(map, "map.qgs"); png("r.png", 1200, 800, res = 96); print(map); dev.off()'

# The QGIS side, at the same size and resolution.
tools/qgis/render_project.py map.qgs qgis.png --size 1200x800 --dpi 96
```

Absolute symbol sizes (marker size in mm, line widths) can be checked
exactly this way: render one feature on a known extent and measure it.

```sh
tools/qgis/render_project.py one.qgs one.png \
    --size 800x800 --extent=-80,34,-78,36 --measure
#> drawn: 22x22 px (5.82x5.82 mm at 96 dpi)
```

## marker_shapes.py

Draws QGIS's SimpleMarker shapes at a known size and measures each one,
which is how the pch → marker table in `R/marker.R` was derived: QGIS sizes
a marker by its width, except `equilateral_triangle`, which is inscribed
in the circle of that diameter.

```sh
tools/qgis/marker_shapes.py --png shapes.png
```
