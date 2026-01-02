def foo():
    if True:
        print("Hello")
        print("Broken Indent") ; This line is wrongly indented (2 spaces instead of 0 or 4)

def bar():
    return "clean"
