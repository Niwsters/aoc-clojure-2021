line = "{([(<{}[<>[]}>{[]{[(<()>"

def opens():
    return "([{<"

def closed():
    return ")]}>"

def is_open(c):
    return c in opens()

def is_closed(c):
    return c in closed()

def pairs():
    return dict(zip(opens(), closed()))

def process():
    opens = []
    for c in line:
        expected_closed = None
        if len(opens) > 0:
            expected_closed = pairs()[opens[-1]]

        if is_open(c):
            opens = opens + [c]

        if is_closed(c):
            if c == expected_closed:
                opens.pop()
            else:
                raise Exception("Expected " + expected_closed + " but got " + c)
    print(opens)

process()
