from Interface import Base

class Hello(Base):
    """ The Hello interface provides greetings. """

    def hello(self, name):
        """ Say hello to the name """

Class HelloComponent:
    __implements__ = Hello

    def hello(self, name):
        return "hello %s!" % name
