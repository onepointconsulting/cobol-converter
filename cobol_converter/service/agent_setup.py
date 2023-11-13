from autogen import AssistantAgent, UserProxyAgent, GroupChat, GroupChatManager

from cobol_converter.autogen_config import llm_config
from cobol_converter.service.cobol_retriever_function import list_cobol_files
from cobol_converter.autogen_config import config_list
from cobol_converter.config import cfg
from cobol_converter.log_factory import logger


llm_config = {
    "cache_seed": cfg.seed,  # seed for caching and reproducibility
    "config_list": config_list,  # a list of OpenAI API configurations
    "temperature": cfg.temperature,  # temperature for sampling
}


def cobol_convert_agent_factory() -> AssistantAgent:
    assistant = AssistantAgent(
        name="Python_Coder",
        system_message="""You are a helpful AI assistant.
You convert Cobol code int Python code""",
        llm_config=llm_config,
    )
    return assistant


def python_test_agent_factory() -> AssistantAgent:
    assistant = AssistantAgent(
        name="Unit_Tester",
        system_message="""You are a helpful AI assistant.
You create unit tests based on nthe unittest library for Python code in the conversation.""",
        llm_config=llm_config,
        is_termination_msg=lambda x: True
    )
    return assistant



def user_proxy_factory() -> UserProxyAgent:
    user_proxy_agent = UserProxyAgent(
        name="user_proxy",
        human_input_mode="NEVER",
        max_consecutive_auto_reply=10,
        is_termination_msg=lambda x: x.get("content", "")
        .rstrip()
        .endswith("TERMINATE"),
        code_execution_config=False,
    )
    return user_proxy_agent


def create_group_chat_manager(
    user_proxy: UserProxyAgent,
    coding_assistant: AssistantAgent,
    testing_assistant: AssistantAgent,
) -> GroupChatManager:
    groupchat = GroupChat(
        agents=[user_proxy, coding_assistant, testing_assistant],
        messages=[],
        max_round=12,
    )
    manager = GroupChatManager(groupchat=groupchat, llm_config=llm_config)
    return manager


user_proxy = user_proxy_factory()
manager = create_group_chat_manager(
    user_proxy, cobol_convert_agent_factory(), python_test_agent_factory()
)


if __name__ == "__main__":
    assert manager is not None
    for cobol_file in list_cobol_files():
        assert cobol_file is not None
        user_proxy.initiate_chat(
            manager,
            message=f"""Please convert the following code to Python and write unit tests for it: \n\n{cobol_file.read_text()}""",
        )
        for i, (key, value) in enumerate(user_proxy.chat_messages.items()):
            output_file = cfg.conversion_python_dir/f"{cobol_file.name}_message_{i}.txt"
            logger.info(output_file)
            output_file.write_text(value[1]['content'])