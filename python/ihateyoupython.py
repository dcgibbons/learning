abc = "hello, world!"
for c in abc:
   xyz = "I want a cookie, or maybe a %s" % c

print "abc is %s" % xyz # whoops, I meant to use abc here...
