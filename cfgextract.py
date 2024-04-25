#! /usr/bin/python
import re

class BasicBlock:
    def __init__(self, label, statements):
        self.label = label
        self.statements = statements

    def __repr__(self):
        return f"BasicBlock(label='{self.label}', statements={self.statements})"

def parse_statements(block_text):
    # Parses the statements inside a block
    statements = []
    lines = block_text.strip().split(';')
    for line in lines:
        if line.strip():
            statements.append(line.strip())
    return statements

def parse_basic_blocks(ir_code):
    # Regex to find basic blocks
    blocks = re.findall(r'\(label "([^"]+)", \[\s*([\s\S]*?)\s*\]\);', ir_code)
    basic_blocks = []
    
    for label, block_text in blocks:
        statements = parse_statements(block_text)
        basic_blocks.append(BasicBlock(label, statements))
    
    return basic_blocks

# Example IR code
ir_code = """
  (label "_f_try_preamble", [
    MOVE(temp "ZERO", BINOP(temp "ZERO", OR, temp "ZERO"));
    JUMP(label "_f_try_if_0_header")
  ]);
  (label "_f_try_if_0_header", [
    MOVE(temp "_t_try_if_0_test",
      ROP(temp "_t_try_arg_c", EQ, temp "N"));
    CJUMP(temp "_t_try_if_0_test",
      label "_f_try_if_0_then",
      label "_f_try_if_0_else")
  ]);
"""

# Parse the IR code
basic_blocks = parse_basic_blocks(ir_code)
for block in basic_blocks:
    print(block)

