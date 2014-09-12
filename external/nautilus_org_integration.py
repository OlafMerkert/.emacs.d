from gi.repository import Nautilus, GObject
import subprocess
import os.path as path
import urllib

homedir = path.expanduser("~")


def clean_uri(uri):
    # remove double slash after file protocol
    if uri.startswith("file://"):
        uri = "file:" + uri[7:]
    # replace homedir with ~
    uri = uri.replace(homedir, "~", 1)
    return uri


class ColumnExtension(GObject.GObject, Nautilus.MenuProvider):
    def __init__(self):
        pass

    def org_capture(self, menu, file):
        name = file.get_name()
        uri = clean_uri(file.get_uri())
        subprocess.call(
            ["/usr/bin/emacsclient",
             "/".join(["org-protocol:/",
                       "capture:/",
                       urllib.quote_plus(uri),
                       urllib.quote_plus(name)])])

    def get_file_items(self, window, files):
        if len(files) != 1:
            return
        file = files[0]
        item = Nautilus.MenuItem(
            name="org_capture::org_menu",
            label="Org Capture ...",
            tip="Send file to org-capture"
        )
        item.connect('activate', self.org_capture, file)
        return [item]
