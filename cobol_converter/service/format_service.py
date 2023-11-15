from pathlib import Path
from black import format_str
from black import Mode


def format_file(file: Path) -> str:
    assert file is not None
    text = file.read_text()
    formatted = format_str(text, mode=Mode())
    file.write_text(formatted, encoding="utf-8")
    return formatted


if __name__ == "__main__":
    from cobol_converter.config import cfg
    from cobol_converter.log_factory import logger

    sample_file = cfg.project_root / "cobol_converter/config.py"
    assert sample_file.exists()
    res = format_file(sample_file)
    logger.info(res)
