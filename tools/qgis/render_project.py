#!/usr/bin/env python3
"""Render a QGIS project to a PNG, the way QGIS itself would draw it.

    tools/qgis/render_project.py project.qgs out.png

This is how a written .qgs gets checked against the plot it came from: the
project renders headlessly, so the result can be compared with an R device
of the same size and resolution (--measure prints the drawn extent in
pixels and millimeters, which is what pinned the symbol sizes down for the
tmap symbol constants).

Options:
  --size WxH        output pixels (default 1200x800)
  --dpi N           output resolution (default 96, R's png() default)
  --extent x0,y0,x1,y1
                    map extent in project CRS units (default: every
                    layer's extent combined, with a 5% margin)
  --layers A,B      render only these layers, topmost first
  --measure         report the bounding box of everything drawn. It counts
                    every non-background pixel, so antialiasing widens it
                    by about a pixel per side; compare against an R
                    measurement taken the same way.
  --quiet           only print errors

Every layer of the project is reported with its validity and renderer, so
a project that reads but draws nothing is visible in the output.
"""

import argparse
import os
import sys

import qgis_env

# Re-runs this script under QGIS's own Python when the bindings are not
# importable here; must happen before the qgis imports below.
qgis_env.relaunch_if_needed()

from qgis.core import (  # noqa: E402
    QgsApplication,
    QgsCoordinateTransform,
    QgsMapRendererParallelJob,
    QgsMapSettings,
    QgsProject,
    QgsRectangle,
)
from qgis.PyQt.QtCore import QEventLoop, QSize  # noqa: E402
from qgis.PyQt.QtGui import QColor  # noqa: E402

BACKGROUND = QColor(255, 255, 255)


def parse_args(argv):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("project")
    p.add_argument("output")
    p.add_argument("--size", default="1200x800")
    p.add_argument("--dpi", type=float, default=96.0)
    p.add_argument("--extent")
    p.add_argument("--layers")
    p.add_argument("--measure", action="store_true")
    p.add_argument("--quiet", action="store_true")
    args = p.parse_args(argv)
    width, _, height = args.size.partition("x")
    args.width, args.height = int(width), int(height)
    return args


def combined_extent(layers, project):
    """Every layer's extent in the project CRS, with a small margin."""
    extent = QgsRectangle()
    for layer in layers:
        rect = layer.extent()
        if layer.crs() != project.crs():
            rect = QgsCoordinateTransform(
                layer.crs(), project.crs(), project
            ).transformBoundingBox(rect)
        extent.combineExtentWith(rect)
    extent.scale(1.05)
    return extent


def ink_bounds(image):
    """Bounding box of the non-background pixels, as (width, height) px."""
    xs, ys = [], []
    background = BACKGROUND.rgb()
    for y in range(image.height()):
        for x in range(image.width()):
            if image.pixel(x, y) != background:
                xs.append(x)
                ys.append(y)
    if not xs:
        return None
    return max(xs) - min(xs) + 1, max(ys) - min(ys) + 1


def main(argv):
    args = parse_args(argv)
    QgsApplication.setPrefixPath(os.environ["QGIS_PREFIX_PATH"], True)
    app = QgsApplication([], False)
    app.initQgis()

    project = QgsProject.instance()
    if not project.read(args.project):
        print(f"cannot read {args.project}", file=sys.stderr)
        return 1

    # layerOrder() is the drawing order the project itself defines.
    layers = project.layerTreeRoot().layerOrder()
    if args.layers:
        wanted = [name.strip() for name in args.layers.split(",")]
        layers = [layer for layer in layers if layer.name() in wanted]
    if not layers:
        print("the project has no layers to render", file=sys.stderr)
        return 1
    if not args.quiet:
        for layer in layers:
            renderer = getattr(layer, "renderer", lambda: None)()
            print(
                f"  {layer.name()}: valid={layer.isValid()} "
                f"crs={layer.crs().authid()} "
                f"renderer={type(renderer).__name__}"
            )

    settings = QgsMapSettings()
    settings.setLayers(layers)
    settings.setBackgroundColor(BACKGROUND)
    settings.setOutputSize(QSize(args.width, args.height))
    settings.setOutputDpi(args.dpi)
    settings.setDestinationCrs(project.crs())
    if args.extent:
        settings.setExtent(QgsRectangle(*[float(v) for v in args.extent.split(",")]))
    else:
        settings.setExtent(combined_extent(layers, project))

    job = QgsMapRendererParallelJob(settings)
    loop = QEventLoop()
    job.finished.connect(loop.quit)
    job.start()
    loop.exec()
    image = job.renderedImage()
    if not image.save(args.output):
        print(f"cannot write {args.output}", file=sys.stderr)
        return 1

    if args.measure:
        bounds = ink_bounds(image)
        if bounds is None:
            print("nothing was drawn")
        else:
            width, height = bounds
            mm = 25.4 / args.dpi
            print(
                f"drawn: {width}x{height} px "
                f"({width * mm:.2f}x{height * mm:.2f} mm at {args.dpi:g} dpi)"
            )
    if not args.quiet:
        print(f"wrote {args.output}")
    app.exitQgis()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
