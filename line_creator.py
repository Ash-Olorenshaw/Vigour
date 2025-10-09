
def generate_program_lines(program_text : str) -> list[str]:
    program_lines = []

    current_line : str = ""
    inside_string : str = ''
    for i in range(len(program_text)):
        c = program_text[i]
        if (c == '\n' or c == '|') and not inside_string:
            if (i == len(program_text) - 1 or program_text[i + 1] != '\\') and (i == 0 or program_text[i - 1] != '\\'):
                program_lines.append(current_line)
                current_line = ""
        elif  i == len(program_text) - 1:
            current_line += c
            program_lines.append(current_line)
        elif c == '"': 
            if inside_string == c:
                inside_string = ''
            elif not inside_string:
                inside_string = c
            current_line += c
        elif c == "'": 
            if inside_string == c:
                inside_string = ""
            elif not inside_string:
                inside_string = c
            current_line += c
        else:
            current_line += c

    return program_lines

