import subprocess
import sys

# pip install modules to avoid "ModuleNotFoundError: No module found named '...'"
def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

install("pandas"),install("elasticsearch"),install("bs4"),install("git"),install("requests")