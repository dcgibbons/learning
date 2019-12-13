class Foo(object):
  def __init__(self):
    self.state = "IDLE"

def monkeypatch(target):
  def patcher(func):
    try:
      _oldfunc = getattr(target, func.__name__)
    except AttributeError:
      setattr(target, func.__name__, func)
      return func
    setattr(target, func.__name__, func)
    return func
  return patcher

@monkeypatch(Foo)
def __setattr__(other, name, value):
  print "mySetAttr: name=%s value=%r" % (name, value)
  if name == "state":
    print "changing state to %r" % value
 

foo = Foo()
foo.state = 'EATING'
print "state=%s" % foo.state
foo.state = 'RUNNING'
print "state=%s" % foo.state
foo.state = 'PUKING'
print "state=%s" % foo.state
