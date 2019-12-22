from zope.interface import Attribute, Interface

class ITask(Interface):
  name = Attribute("""name of the task""")
  state = Attribute("""current state of the task""")

  def doTask(self):
    """Run the task."""


from zope.component import provideAdapter

provideAdapter(CallableTaskAdapter, adapts=Task, provides=ITask)
