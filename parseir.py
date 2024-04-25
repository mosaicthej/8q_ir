#! /usr/bin/python

import re
import sys

class BasicBlock:
    def __init__(self, label, statements):
        self.label = label
        self.statements = statements

    def __repr__(self):
        return f"BasicBlock(label='{self.label}', statements={self.statements})"

class Statement:
    pass

class Move(Statement):
    def __init__(self, dest, src):
        self.dest = dest
        self.src = src

    def __repr__(self):
        return f"Move(dest={self.dest}, src={self.src})"

class Jump(Statement):
    def __init__(self, target):
        self.target = target

    def __repr__(self):
        return f"Jump(target='{self.target}')"

class CJump(Statement):
    def __init__(self, condition, true_label, false_label):
        self.condition = condition
        self.true_label = true_label
        self.false_label = false_label

    def __repr__(self):
        return f"CJump(condition={self.condition}, true_label='{self.true_label}', false_label='{self.false_label}')"

def parse_statement(line):
    if "MOVE" in line:
        # Simplified parsing, real implementation might need more sophisticated parsing logic
        parts = re.match(r'MOVE\(temp "([^"]+)", ([^(]+)\((.*)\)\)', line)
        if parts:
            return Move(parts.group(1), parts.group(3))
        else:
            print(f"Unrecognized MOVE statement: {line}")
    elif "JUMP" in line:
        target = re.search(r'JUMP\(label "([^"]+)"\)', line)
        if target:
            return Jump(target.group(1))
        else: print(f"Unrecognized JUMP statement: {line}")
    elif "CJUMP" in line:
        parts = re.match(r'CJUMP\(temp "([^"]+)", label "([^"]+)", label "([^"]+)"\)', line)
        if parts: return CJump(parts.group(1), parts.group(2), parts.group(3))
        else: print(f"Unrecognized CJUMP statement: {line}")
    else:
        return Statement()  # Generic fallback for unhandled types

def parse_statements(block_text):
    statements = []
    lines = block_text.strip().split(';')
    for line in lines:
        if line.strip():
            statements.append(parse_statement(line.strip()))
    return statements

def parse_basic_blocks(ir_code):
    blocks = re.findall(r'\(label "([^"]+)", \[\s*([\s\S]*?)\s*\]\);', ir_code)
    basic_blocks = []
    
    for label, block_text in blocks:
        statements = parse_statements(block_text)
        basic_blocks.append(BasicBlock(label, statements))
    
    return basic_blocks

def read_ir_code(filename):
    with open(filename, 'r') as file:
        return file.read()

# Main execution
if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]
    ir_code = read_ir_code(filename)
    basic_blocks = parse_basic_blocks(ir_code)
    for block in basic_blocks:
        print(block)

