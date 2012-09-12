#! /usr/bin/env python
#
# Code Kata Two - Karate Chop
# http://codekata.pragprog.com/2007/01/kata_two_karate.html
#
# Chad Gibbons
# dcgibbons@gmail.com
# August 19, 2012
#

import unittest

def chop(key, l):
    lower = 0
    upper = len(l) - 1
    
    while (upper >= lower):
        mid = (lower + upper) / 2
        if (key < l[mid]):
            upper = mid - 1
        elif (key > l[mid]):
            lower = mid + 1
        else:
            return mid
    return -1


def chop2(key, l):
    def chopit(key, l, lower, upper):
        if (upper < lower):
            return -1
        else:
            mid = (lower + upper) / 2
            if (key < l[mid]):
                return chopit(key, l, lower, mid - 1)
            elif (key > l[mid]):
                return chopit(key, l, mid + 1, upper)
            else:
                return mid
    return chopit(key, l, 0, len(l) - 1)


def chop3(key, l):
  subset = l

  while (len(subset) > 0):
    upper = len(subset) - 1
    mid = upper / 2
    print "mid=%d subset=%r" % (mid, subset)
    if (key < subset[mid]):
      subset = subset[:mid]
      print "key < mid, subset=%r" % subset
    elif (key > subset[mid]):
      subset = subset[mid + 1:]
      print "key > mid, subset=%r" % subset
    else:
      return mid
  else:
    return -1


class TestKarateChop(unittest.TestCase):
    def template(self, fn):
        self.assertEqual(-1, fn(3, []))
        self.assertEqual(-1, fn(3, [1]))
        self.assertEqual(0,  fn(1, [1]))

        self.assertEqual(0,  fn(1, [1, 3, 5]))
        self.assertEqual(1,  fn(3, [1, 3, 5]))
        self.assertEqual(2,  fn(5, [1, 3, 5]))
        self.assertEqual(-1, fn(0, [1, 3, 5]))
        self.assertEqual(-1, fn(2, [1, 3, 5]))
        self.assertEqual(-1, fn(4, [1, 3, 5]))
        self.assertEqual(-1, fn(6, [1, 3, 5]))

        self.assertEqual(0,  fn(1, [1, 3, 5, 7]))
        self.assertEqual(1,  fn(3, [1, 3, 5, 7]))
        self.assertEqual(2,  fn(5, [1, 3, 5, 7]))
        self.assertEqual(3,  fn(7, [1, 3, 5, 7]))
        self.assertEqual(-1, fn(0, [1, 3, 5, 7]))
        self.assertEqual(-1, fn(2, [1, 3, 5, 7]))
        self.assertEqual(-1, fn(4, [1, 3, 5, 7]))
        self.assertEqual(-1, fn(6, [1, 3, 5, 7]))
        self.assertEqual(-1, fn(8, [1, 3, 5, 7]))

    def test_chop(self):
        self.template(chop)

    def test_chop2(self):
        self.template(chop2)

    def test_chop3(self):
      self.template(chop3)


if __name__ == '__main__':
    unittest.main()
