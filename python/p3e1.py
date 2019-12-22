S="hello, world"
n=0
L=[]
for x in S: 
    print "%c=0x%02X" % (x, ord(x)),
    n += ord(x)
    L.append(ord(x))

print "n=0x%04X" % (n)
print L
print map(ord,S)
    
