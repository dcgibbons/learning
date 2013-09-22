D = {
    'already' : (lambda: 2 + 2),
    'got' : (lambda: 2 * 4),
    'one' : (lambda: 2 ** 6)
}

key='one'
print D[key]()
