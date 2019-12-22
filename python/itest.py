L = [1,2,4,8,16,32,64]
X = 5

found = i = 0
while not found and i < len(L):
    if 2 ** X == L[i]:
        found = 1
	print 'found!! at '
    else:
        i = i+1

if found:
    print 'at index', i
else:
    print X, 'not found'
