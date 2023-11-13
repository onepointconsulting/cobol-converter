import autogen

from cobol_converter.log_factory import logger
from cobol_converter.config import cfg

config_list = autogen.config_list_from_json(
    "OAI_CONFIG_LIST",
    filter_dict={
        "model": ["gpt-4", cfg.oai_config_list[0]["model"]],
    },
)

llm_config = {"config_list": config_list}

if __name__ == "__main__":
    logger.info(llm_config)
