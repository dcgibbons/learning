from twisted.web import xmlrpc, server

class Example(xmlrpc.XMLRPC):
  """An example object to be published."""

  def xmlrpc_echo(self, x):
    """
      Return all passed args.
    """
    return x

  def xmlrpc_add(self, a, b):
    """
      Return sum of arguments.
    """
    return a + b

  def xmlrpc_fault(self):
    """
      Raise a fault indicating that the procedure shoudl not be usd.
    """
    raise xmlrpc.Fault(123, "The fault procedure is faulty.")

if __name__ == '__main__':
  from twisted.internet import reactor
  r = Example()
  reactor.listenTCP(7080, server.Site(r))
  reactor.run()
