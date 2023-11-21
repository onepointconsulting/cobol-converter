from autogen import AssistantAgent, UserProxyAgent
from cobol_converter.autogen_config import llm_config

from cobol_converter.service.agent_setup import AgentType, UserProxyType
from cobol_converter.service.terminators import terminate_lambda
from cobol_converter.toml_support import prompts
from cobol_converter.config import cfg


def python_code_rest_generator() -> AssistantAgent:
    return AssistantAgent(
        name=AgentType.AGENT_REST_INTERFACE_GENERATOR,
        system_message=prompts["agents"]["code_rest"]["system_message"],
        llm_config=llm_config,
        is_termination_msg=terminate_lambda,
    )


def user_proxy_rest_factory() -> UserProxyAgent:
    user_proxy_agent = UserProxyAgent(
        name=UserProxyType.REST_PROXY,
        human_input_mode="NEVER",
        max_consecutive_auto_reply=10,
        is_termination_msg=terminate_lambda,
        code_execution_config=False,
        system_message=prompts["agents"]["rest_user_proxy"]["system_message"],
    )
    return user_proxy_agent


def create_rest_factory_chat(
    user_proxy: UserProxyAgent, assistant: AssistantAgent, python_code: str
):
    user_proxy_message = (prompts["agents"]["rest_user_proxy"]["message"]).format(
        python_code=python_code
    )
    user_proxy.initiate_chat(
        assistant,
        message=user_proxy_message,
    )
