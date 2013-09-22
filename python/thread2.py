import thread, time

def counter(myId, count):
    for i in range(count):
        print '[%s] => %s' % (myId, i)

for i in range(10):
    thread.start_new(counter, (i, 3))

time.sleep(4)
print 'main thread exiting'
