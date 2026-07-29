#!/usr/bin/env python3
"""Finding QGIS's Python, and re-running a script under it.

`qgis.core` only imports in the interpreter QGIS ships with, which on
macOS lives inside the .app together with its own standard library and
PROJ database — three paths that a headless run has to set itself, and
that fail unhelpfully ("No module named 'encodings'", "Cannot find
proj.db") when one of them is wrong.

A script that calls relaunch_if_needed() before importing qgis therefore
runs directly, under whatever python3 is on PATH:

    #!/usr/bin/env python3
    import qgis_env

    qgis_env.relaunch_if_needed()

    from qgis.core import QgsApplication

On a Linux box whose QGIS is packaged for the system python, the import
already works and nothing is relaunched.

This file also works as the interpreter for a one-off script that has no
such preamble:

    tools/qgis/qgis_env.py probe.py project.qgs
"""

import glob
import os
import sys

# Set on the child so a bundled interpreter that still cannot import qgis
# reports it instead of re-executing forever.
RELAUNCHED = "QGIS_ENV_RELAUNCHED"


def qgis_environment(app=None):
    """The interpreter and environment to run a QGIS script with.

    Returns (python, env). `app` is a QGIS application directory; the
    default is $QGIS_APP, or the newest /Applications/QGIS*.app.
    """
    env = dict(os.environ)
    if sys.platform != "darwin":
        # A distro-packaged QGIS puts its bindings on the system python.
        return sys.executable, env

    app = app or env.get("QGIS_APP") or newest_app()
    contents = os.path.join(app, "Contents")
    python = newest(os.path.join(contents, "MacOS", "python3.*"))
    site = newest(os.path.join(contents, "Resources", "python3.*", "site-packages"))
    if python is None or site is None:
        raise SystemExit(f"no bundled python found in {app}")
    env["QGIS_PREFIX_PATH"] = os.path.join(contents, "MacOS")
    env["PYTHONHOME"] = os.path.join(contents, "Frameworks")
    env["PYTHONPATH"] = os.pathsep.join(
        [site] + ([env["PYTHONPATH"]] if env.get("PYTHONPATH") else [])
    )
    env["PROJ_LIB"] = os.path.join(contents, "Resources", "qgis", "proj")
    return python, env


def newest(pattern):
    """The last match of a glob in sorted order, i.e. the newest version."""
    matches = sorted(glob.glob(pattern))
    return matches[-1] if matches else None


def newest_app():
    app = newest("/Applications/QGIS*.app")
    if app is None:
        raise SystemExit(
            "no QGIS application found; "
            "set QGIS_APP=/Applications/QGIS-x.y.app"
        )
    return app


def relaunch_if_needed():
    """Re-runs the calling script under QGIS's Python unless it is already."""
    try:
        import qgis.core  # noqa: F401
    except ImportError:
        pass
    else:
        os.environ.setdefault("QGIS_PREFIX_PATH", "/usr")
        return

    if os.environ.get(RELAUNCHED):
        raise SystemExit(
            "QGIS's own python cannot import qgis.core either; "
            f"check the install at {os.environ.get('QGIS_APP', '(default)')}"
        )
    python, env = qgis_environment()
    env[RELAUNCHED] = "1"
    script = os.path.abspath(sys.argv[0])
    os.execve(python, [python, script, *sys.argv[1:]], env)


def main(argv):
    if not argv:
        raise SystemExit(f"usage: {sys.argv[0]} SCRIPT [ARGS...]")
    python, env = qgis_environment()
    env[RELAUNCHED] = "1"
    os.execve(python, [python, *argv], env)


if __name__ == "__main__":
    main(sys.argv[1:])
