L = []
for i in range(7):
    L.append(2 ** i)
print "L =", L
X = 5
print "found? ", (2 ** X) in L
