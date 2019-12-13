import re
from twisted.internet import reactor
from twisted.internet.protocol import Protocol, ClientFactory
from twisted.web import xmlrpc, server

class ICBProtocol(Protocol):
  def __init__(self, nick):
    self._nick = nick

    self._buffer = []
    self._length = 0

    self._dispatchTable = {
      'a': self._handleLogin,
      'b': self._handleOpen,
      'c': self._handlePersonal,
      'd': self._handleStatus,
      'e': self._handleError,
      'f': self._handleImportant,
      'g': self._handleExit,
      'h': self._handleCommand,
      'i': self._handleCommandOut,
      'j': self._handleProtocol,
      'k': self._handleBeep,
      'l': self._handlePing,
      'm': self._handlePong,
      'n': self._handleNoOp,
    }

  def _handleLogin(self, packetData):
    print 'Login Accepted!'

  def _handleOpen(self, packetData):
    nick = packetData[0]
    msg = packetData[1]
    print "<%s> %s" % (nick, msg)

  def _handlePersonal(self, packetData):
    nick = packetData[0]
    msg = packetData[1]
    print "<*%s*> %s" % (nick, msg)

  def _handleStatus(self, packetData):
    print "[=%s=] %s" % (packetData[0], packetData[1])

  def _handleError(self, packetData):
    print "[=Error=] %s" % packetData[0]

  def _handleImportant(self, packetData):
    print "[=%s=] %s" % (packetData[0], packetData[1])

  def _handleExit(self, packetData):
    print "Server forced us to disconnect"

  def _handleCommand(self, packetData):
    pass

  def _handleCommandOut(self, packetData):
    cmd = packetData[0][0:2]
    if cmd == 'gh':
      print "Group     ## S  Moderator    "
    elif cmd == 'wg':
      msg = "Group : " + packetData[1]
      if len(packetData) > 2:
        msg += " " + packetData[2]
      print msg
    elif cmd == 'wh':
      print "   Nickname      Idle      Sign-on  Account"
    elif cmd == 'wl':
      # TODO: parse who listing
      pass
    elif cmd == 'co':
      print ''.join(packetData[1:])
    else:
      print ''.join(packetData)

  def _handleProtocol(self, packetData):
    print "Protocol Level : %d" % int(packetData[0])
    print "Server Name    : %s" % packetData[1]
    print "Server Desc    : %s" % packetData[2]

    loginPacket = 'a' + \
                  self._nick + '\001' +\
                  self._nick + '\001' +\
                  '\001' +\
                  'login\001' +\
                  '\001'
    self.transport.write(chr(len(loginPacket)) + loginPacket)

  def _handleBeep(self, packetData):
    pass

  def _handlePing(self, packetData):
    pass

  def _handlePong(self, packetData):
    pass

  def _handleNoOp(self, packetData):
    pass

  def connectionMade(self):
    print "Connection made."

  def dataReceived(self, data):
    if self._length == 0:
      self._length = ord(data[0])
      data = data[1:]

    self._buffer.extend(data)

    while len(self._buffer) > 0 and len(self._buffer) >= self._length:
      # extract the packet type from the first character of the buffer
      packetType = self._buffer[0]

      # convert the remaining packet to a string
      packetData = ''.join(self._buffer[1:self._length - 1])

      # split the string by the \x01 separator
      packetData = re.split('[\x00\x01]', packetData)

      # remove the extract portion of the buffer
      self._buffer = self._buffer[self._length:]
      self._length = 0

      # if there is any data left in the buffer, seed
      # the next packet
      if len(self._buffer) > 0:
        self._length = ord(self._buffer[0])
        self._buffer = self._buffer[1:]

      # dispatch to the correct protocol handler function based
      # upon packet type
      if self._dispatchTable.has_key(packetType):
        func = self._dispatchTable[packetType]
        func(packetData)
      else:
        print "Invalid packet type: %s" % packetType

class ICBClientFactory(ClientFactory):
  def __init__(self, nick):
    self.nick = nick

  def startedConnecting(self, connector):
    print 'Started to connect. Nick:', self.nick

  def buildProtocol(self, addr):
    print 'Connected.'
    return ICBProtocol(self.nick)
  
  def clientConnectionLost(self, connector, reason):
    print 'Lost connection. Reason: ', reason

  def clientConnectionFailed(self, connector, reason):
    print 'connection failed. Reason: ', reason


connections = {}

class ICBProxy(xmlrpc.XMLRPC):
  def xmlrpc_connect(self, serverHost, serverPort, nick):
    connections[nick] = ICBClientFactory(nick)
    reactor.connectTCP(serverHost, serverPort, connections[nick])

if __name__ == '__main__':
  r = ICBProxy()
  reactor.listenTCP(7080, server.Site(r))
  reactor.run()

