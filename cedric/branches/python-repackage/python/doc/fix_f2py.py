

import re

class Fix_f2py(object):

    def __init__(self):
        self.rxheader = re.compile(r'auto-generated with f2py')
        self.rxfunctions = re.compile(r'Functions:')
        self.rxcommonblocks = re.compile(r'COMMON blocks:')

    def __call__(self, app, what, name, obj, options, lines):
        if what != 'module' or not self.rxheader.search(lines[0]):
            return
        i = 0
        handler = (lambda l: l)
        while i < len(lines):
            l = lines[i]
            if self.rxfunctions.search(l):
                lines.insert(i, "\n")
                i += 1
                handler = self._in_functions
            elif self.rxcommonblocks.search(l):
                lines.insert(i, "\n")
                i += 1
                handler = self._in_blocks
            else:
                lines[i] = handler(l)
            i += 1

    def _in_functions(self, line):
        return " * " + line[2:]

    def _in_blocks(self, line):
        if line.startswith('  '):
            return " * " + line[2:]
        if line.startswith('.'):
            return "\n"
        return line


def setup(app):
    app.connect('autodoc-process-docstring', Fix_f2py())


