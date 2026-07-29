#!/usr/bin/env python3
"""Measure (and optionally draw) QGIS's SimpleMarker shapes.

    tools/qgis/marker_shapes.py --png shapes.png

QGIS sizes a marker by its width — except the ones inscribed in the size
circle, `equilateral_triangle` above all, which comes out sqrt(3)/2 as
wide. Converting another toolkit's symbol size into a QGIS marker size
needs that ratio per shape, and this prints it:

    circle                 10.00 x 10.00 mm   width/size 1.000
    equilateral_triangle    8.65 x  7.55 mm   width/size 0.865

Options:
  --shapes A,B      shape names (default: the ones R's pch map onto)
  --size N          marker size in millimeters (default 10)
  --png PATH        also draw the shapes side by side, for a visual check
                    that the outline really matches (QGIS's asterisk_fill,
                    for instance, is a filled star, not R's thin pch 8)
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
    QgsMarkerSymbol,
    QgsRenderContext,
    QgsSimpleMarkerSymbolLayer,
    QgsUnitTypes,
)
from qgis.PyQt.QtCore import QPointF  # noqa: E402
from qgis.PyQt.QtGui import QColor, QImage, QPainter  # noqa: E402

DEFAULT_SHAPES = "circle,square,diamond,equilateral_triangle,cross,cross2"
PPMM = 20  # pixels per millimeter of the measuring canvas


def parse_args(argv):
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("--shapes", default=DEFAULT_SHAPES)
    p.add_argument("--size", type=float, default=10.0)
    p.add_argument("--png")
    return p.parse_args(argv)


def marker_layer(name, size):
    shape, ok = QgsSimpleMarkerSymbolLayer.decodeShape(name)
    if not ok:
        raise SystemExit(f"unknown marker shape: {name}")
    layer = QgsSimpleMarkerSymbolLayer(shape, size)
    layer.setSizeUnit(QgsUnitTypes.RenderUnit.RenderMillimeters)
    return layer


def render(name, size, canvas_px, at, painter_setup):
    """Draws one marker and returns its ink bounding box in pixels."""
    image = QImage(canvas_px, canvas_px, QImage.Format.Format_ARGB32)
    image.fill(QColor(255, 255, 255))
    painter = QPainter(image)
    context = QgsRenderContext.fromQPainter(painter)
    context.setScaleFactor(PPMM)
    layer = marker_layer(name, size)
    painter_setup(layer)
    symbol = QgsMarkerSymbol([layer])
    symbol.startRender(context)
    symbol.renderPoint(QPointF(at, at), None, context)
    symbol.stopRender(context)
    painter.end()

    xs, ys = [], []
    for y in range(image.height()):
        for x in range(image.width()):
            if QColor(image.pixel(x, y)).red() < 128:
                xs.append(x)
                ys.append(y)
    if not xs:
        return None
    return max(xs) - min(xs) + 1, max(ys) - min(ys) + 1


def solid_black(layer):
    layer.setColor(QColor(0, 0, 0))
    layer.setStrokeColor(QColor(0, 0, 0))
    layer.setStrokeWidth(0.0)


def draw_sheet(shapes, size, path):
    cell = int(size * PPMM * 1.6)
    image = QImage(cell * len(shapes), cell, QImage.Format.Format_ARGB32)
    image.fill(QColor(255, 255, 255))
    painter = QPainter(image)
    context = QgsRenderContext.fromQPainter(painter)
    context.setScaleFactor(PPMM)
    for i, name in enumerate(shapes):
        layer = marker_layer(name, size)
        layer.setColor(QColor(180, 180, 255))
        layer.setStrokeColor(QColor(0, 0, 0))
        layer.setStrokeWidth(0.3)
        symbol = QgsMarkerSymbol([layer])
        symbol.startRender(context)
        symbol.renderPoint(QPointF(cell * i + cell / 2, cell / 2), None, context)
        symbol.stopRender(context)
    painter.end()
    image.save(path)


def main(argv):
    args = parse_args(argv)
    QgsApplication.setPrefixPath(os.environ["QGIS_PREFIX_PATH"], True)
    app = QgsApplication([], False)
    app.initQgis()

    shapes = [name.strip() for name in args.shapes.split(",")]
    canvas = int(args.size * PPMM * 3)
    for name in shapes:
        bounds = render(name, args.size, canvas, canvas / 2, solid_black)
        if bounds is None:
            print(f"{name:22s} nothing drawn")
            continue
        width, height = (v / PPMM for v in bounds)
        print(
            f"{name:22s} {width:5.2f} x {height:5.2f} mm   "
            f"width/size {width / args.size:.3f}"
        )
    if args.png:
        draw_sheet(shapes, args.size, args.png)
        print(f"wrote {args.png}")
    app.exitQgis()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
