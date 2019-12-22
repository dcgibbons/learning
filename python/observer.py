import zope.interface

class Observable(zope.interface.Interface):
  def attachAttributeObserver(self, name, observer):
    pass

  def detachAttributeObserver(self, name, observer):
    pass

  def notifyAttributeChange(self, name, oldValue, newValue):
    pass

class ObservableMixin(object):
  zope.interface.implements(Observable)

  def __init__(self):
    super(ObservableMixin, self).__init__()
    self._observers = {}

  def attachPropertyObserver(self, name, observer):
    if not callable(observer):
      raise ValueError("observer must be callable")

    if not self._observers.has_key(name):
      self._observers[name] = []

    if observer not in self._observers[name]:
      self._observers[name].append(observer)

  def detachPropertyObserver(self, name, observer):
    try:
      self._observers[name].remove(observer)
    except KeyError, ValueError:
      pass

  def notifyAttributeChange(self, name, oldValue, newValue):
    # don't bother notifying if we don't have an _observers attribute
    # yet (during construction) 
    if hasattr(self, '_observers') and self._observers.has_key(name):
      for observer in self._observers[name]:
        observer(observer, name, oldValue, newValue)

  def __setattr__(self, name, newValue):
    oldValue = getattr(self, name, None)
    super(ObservableMixin, self).__setattr__(name, newValue)
    self.notifyAttributeChange(name, oldValue, newValue)

# create a do nothing class that tests to make sure MRO works right
# when we're throwing the ObservableMixin into the mix (hah)
class Something(object):
  def __init__(self):
    super(Something, self).__init__()
    self.something = "ya buddy!"

# create our class that we want to be Observable
class Foo(Something, ObservableMixin):
  def __init__(self):
    self.state = "IDLE"
    super(Foo, self).__init__()
    self.state = "MAYBE IDLE"

# use a stand-alone property observer
def observer(observer, name, oldValue, newValue):
  print "observer notified of property change: name=%s oldValue=%r newValue=%r" % (name, oldValue, newValue)

class MyTesterClass(object):
  def __init__(self):
    self.foo = Foo()
    self.foo.attachPropertyObserver('state', self._stateChangeListener)
    self.foo.attachPropertyObserver('something', observer)

  def _stateChangeListener(self, observer, name, oldValue, newValue):
    print "state was changed from %r to %r" % (oldValue, newValue)

  def run(self):
    print "does Foo implement Observable? %s" % Observable.implementedBy(Foo)
    print "does self.foo provided Observable? %s" % Observable.providedBy(self.foo)

    self.foo.state = "RUNNING"
    self.foo.something = "hmm"
    self.foo.state = "STILL RUNNING"
    self.foo.state = "maybe RUNNING?"
    self.foo.state = "ALL DONE!"
    self.foo.something = "hooray!"

MyTesterClass().run()
