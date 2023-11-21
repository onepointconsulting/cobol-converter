def terminate_lambda(x: str):
    return x.get("content", "").rstrip().find("TERMINATE") > -1
