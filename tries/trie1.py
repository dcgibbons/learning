#!/usr/bin/env python3
class trie(object):
    def __init__(self, string):
        self._create(string)
        pass
    def _create(self, strings):
        if not isinstance(strings, list):
            strings = [strings]
        self.root = {"children":dict()}
        for s in strings:
            n = len(s)
            v = self.root
            j = 0
            while j < n and self._child(v, s[j]) != None:
                v = self._child(v, s[j])
                j += 1
            while j < n:
                u = {"children":dict()}
                v["children"][s[j]] = u
                v = u
                j += 1
            v["children"]["$"] = None

    def _child(self, node, s):
        return node["children"].get(s, None)

    def __repr__(self):
        return str(self.root)

    def _strings(self, node):
        """ build all substrings from a given node """
        t = []
        for k in node["children"].keys():
            if k != "$":
               substrs = self._strings(node["children"][k])
               if len(substrs) > 0:
                   for substr in substrs:
                    t.append(k + substr)
               else:
                    t.append(k)
        return t

    def matches(self, s):
        n = len(s)
        v = self.root
        j = 0
        prefix = ""
        while j < n and self._child(v, s[j]) != None:
            prefix += s[j]
            v = self._child(v, s[j])
            j += 1
        if v is None:
            return None
        else:
            strs = self._strings(v)
            t = []
            for str in strs:
                t.append(prefix + str)
            return t

if __name__ == '__main__':
    t = trie(["hello", "pie", "pi", "hell", "hellboy", "spam", "ham", "pizza"])
    print("matches('hell')=%r" % (t.matches("hell")))
    print("matches('pi'=%r" % (t.matches("pi")))

