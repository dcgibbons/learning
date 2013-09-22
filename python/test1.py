def track_state(func):
  def _inner(instance, state):
    func(instance, state)
    print "state was changed to: %s", state
  return _inner

class Task(object):
  def __init__(self):
    self.state = "IDLE"

  def doTask(self):
    self.state = "RUNNING"
    print "I'm running!"
    self.state = "DONE"

class CallableTask(object):
  def __init__(self, theTask):
    self.task = theTask

  def __call__(self):
    print 'running task'
    self.task.doTask()
    print 'task complete'


task = CallableTask(Task())()
