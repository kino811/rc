import base64
import os
import sys


pyqterminal_dir = os.path.join(
    os.path.dirname(os.path.dirname(__file__)),
    "emacs-application-framework",
    "app",
    "pyqterminal",
)
if pyqterminal_dir not in sys.path:
    sys.path.insert(0, pyqterminal_dir)

import eaf_pyqterm_buffer as upstream
from eaf_pyqterm_term import TerminalScreen


if not hasattr(TerminalScreen, "_copy_with_absolute_coordinates"):
    TerminalScreen._copy_with_absolute_coordinates = TerminalScreen._copy


def _copy_with_buffer_coordinates(self, start, end):
    """Translate alternate-screen absolute rows back to buffer rows."""
    if self.is_buffer:
        start = (start[0], start[1] - self.base)
        end = (end[0], end[1] - self.base)
    return self._copy_with_absolute_coordinates(start, end)


TerminalScreen._copy = _copy_with_buffer_coordinates


class AppBuffer(upstream.AppBuffer):
    def send_text_base64(self):
        text = base64.b64decode(self.current_event_string).decode("utf-8")
        self.term.backend.send(text)
