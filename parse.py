#!/usr/bin/env python

def value(input):
    return input

def make(what):
    def out(ignore):
        return what
    return out

left_paren = '%lparen'
right_paren = '%rparen'
left_brace = '%lbrace'
right_brace = '%rbrace'
left_bracket = '%lbracket'
right_bracket = '%rbracket'
semicolon = '%semi'
dot = '%dot'

import re
regex_table = {re.compile(r'\d+'): value, # Digits
               re.compile(r'\w+'): value, # Identifiers
               re.compile(r'\+'): value,
               re.compile(r'-'): value,
               re.compile(r'\.'): make(dot),
               re.compile(r'"[^"]*"'): value,
               re.compile(r';'): make(semicolon),
               re.compile(r'\('): make(left_paren),
               re.compile(r'\)'): make(right_paren),
               re.compile(r'\{'): make(left_brace),
               re.compile(r'\}'): make(right_brace),
               re.compile(r'\['): make(left_bracket),
               re.compile(r'\]'): make(right_bracket),
               re.compile(r'\s+'): make(None),
               }
# Returns a stream of tokens
def lex(input):
    out = []

    while len(input) > 0:
        def get():
            for regex in regex_table.keys():
                out = regex.match(input)
                if out:
                    return (regex_table[regex](out.group(0)), out.group(0))
            raise Exception('Could not lex "%s"' % input)
        maybe, direct = get()
        input = input[len(direct):]
        # Ignore any regex that returns None
        if maybe:
            out.append(maybe)

    return out

class Tree():
    def __init__(self, kind, parent):
        self.children = []
        self.kind = kind
        self.parent = parent

    def add(self, child):
        self.children.append(child)

    def __str__(self):
        strings = ' '.join([str(s) for s in self.children])
        if self.kind == '%top':
            return strings
        if self.kind == Tree.parens:
            return '(%parens ' + strings + ')'
        if self.kind == Tree.brackets:
            return '(%brackets ' + strings + ')'
        if self.kind == Tree.braces:
            return '(%braces ' + strings + ')'

Tree.top = '%top'
Tree.parens = '%parens'
Tree.brackets = '%brackets'
Tree.braces = '%braces'

# Returns an s-expression
def read(input):
    tokens = lex(input)
    top = Tree(Tree.top, None)
    current = top
    for token in tokens:
        if token == left_paren:
            last = current
            current = Tree(Tree.parens, last)
            last.add(current)
        elif token == right_paren:
            if current.kind != Tree.parens:
                raise Exception('Mismatched parens')
            current = current.parent
        elif token == left_brace:
            last = current
            current = Tree(Tree.braces, last)
            last.add(current)
        elif token == right_brace:
            if current.kind != Tree.braces:
                raise Exception('Mismatched braces')
            current = current.parent
        elif token == left_bracket:
            last = current
            current = Tree(Tree.brackets, last)
            last.add(current)
        elif token == right_bracket:
            if current.kind != Tree.brackets:
                raise Exception('Mismatched brackets')
            current = current.parent
        else:
            current.add(token)
    if current != top:
        raise Exception('Unbalanced %s' % current.kind)
    return top

assert(lex("1 2 a 3 ( )") == ['1', '2', 'a', '3', left_paren, right_paren])

def parse(input):
    pass

grammar = """
<class> = public class <identifier> { <expression>* }
<expression> = <macro>
             | <operator>
             | 
"""

print read('1')
print read('(1)')
print read('(1 {2 + 8})')
print read("""class public X{
    function public static void main(String args[]){
      System.out.println("hello world");
    }
}
""")
