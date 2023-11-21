import sys
import shutil
import datetime
from cobol_converter.service.cobol_retriever_function import list_cobol_files
from cobol_converter.service.cobol_conversion_service import cobol_conversion
from cobol_converter.config import cfg


class Modes:
    MODE_OVERWRITE = "overwrite"
    MODE_CLEAR = "clear"
    MODEL_ONLY_NEW = "only_new"


def clear_files():
    for f in cfg.conversion_python_dir.glob("*"):
        shutil.rmtree(f)


def copy_target_folder():
    iso_timestamp = datetime.datetime.now().isoformat().replace(":", ".")
    if len(list(cfg.conversion_python_dir.glob("*"))) == 0:
        return
    shutil.copytree(
        cfg.conversion_python_dir, cfg.conversion_python_dir.parent / iso_timestamp
    )


if __name__ == "__main__":
    mode = Modes.MODE_OVERWRITE
    if len(sys.argv) > 1:
        mode = sys.argv[1]
    exclusion_file_names = []
    match mode:
        case Modes.MODE_CLEAR:
            copy_target_folder()
            clear_files()
        case Modes.MODEL_ONLY_NEW:
            exclusion_file_names = [
                f.stem for f in cfg.conversion_python_dir.iterdir() if f.is_dir()
            ]
    cobol_conversion(list_cobol_files(exclusion_file_names))
