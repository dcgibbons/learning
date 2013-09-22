D={'spam':'slime','ham':'yummy','eggs':'cholesterol','cheeze':'fat'}
keys=D.keys()
keys.sort()
for x in keys: print "%s=%s" % (x, D[x])
