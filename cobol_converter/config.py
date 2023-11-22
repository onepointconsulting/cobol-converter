from pathlib import Path
import os
import json

from dotenv import load_dotenv


from cobol_converter.log_factory import logger

load_dotenv()


def create_if_not_exists(directory: Path):
    if not directory.exists():
        directory.mkdir(parents=True)
    assert directory.exists()


class Config:
    oai_config_list = json.loads(os.getenv("OAI_CONFIG_LIST"))
    assert oai_config_list is not None
    project_root = Path(os.getenv("PROJECT_ROOT"))
    assert project_root.exists(), "Cannot find project root"
    source_code_dir: Path = project_root / os.getenv("SOURCE_CODE_DIR")
    create_if_not_exists(source_code_dir)
    conversion_python_dir = Path(os.getenv("CONVERSION_PYTHON_DIR"))
    create_if_not_exists(conversion_python_dir)
    code_execution_dir = Path(os.getenv("CODE_EXECUTION_DIR"))
    assistent_id = os.getenv("ASSISTENT_ID")
    temperature = int(os.getenv("TEMPERATURE"))
    seed = int(os.getenv("SEED"))
    max_tries = int(os.getenv("MAX_TRIES"))
    test_process_timeout = int(os.getenv("TEST_PROCESS_TIMEOUT"))


cfg = Config()

if __name__ == "__main__":
    logger.info("Configuration Info:")
    help(cfg)
    logger.info("test_process_timeout: %d", cfg.test_process_timeout)
