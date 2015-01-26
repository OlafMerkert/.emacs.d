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


def uri_quote(uri):
    return urllib.quote(uri, "%")


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
                       uri_quote(uri),
                       name])])

    def org_store_link(self, menu, file):
        name = file.get_name()
        uri = clean_uri(file.get_uri())
        subprocess.call(
            ["/usr/bin/emacsclient",
             "/".join(["org-protocol:/",
                       "store-link:/",
                       uri_quote(uri),
                       name])])

    def get_file_items(self, window, files):
        if len(files) != 1:
            return
        file = files[0]
        item_capture = Nautilus.MenuItem(
            name="org_capture::org_menu",
            label="Org Capture ...",
            tip="Send file to org-capture"
        )
        item_store_link = Nautilus.MenuItem(
            name="org_store_link::org_menu",
            label="Org Store Link ...",
            tip="Store file as org-link"
        )
        item_capture.connect('activate', self.org_capture, file)
        item_store_link.connect('activate', self.org_store_link, file)
        return [item_capture, item_store_link]
