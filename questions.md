<link href="/Users/hjort/Code/configs/markdown.css" rel="stylesheet"></link>

* When we try to connect to server when username is taken, should we reply with
  `already_connected` or something else?
* It says that chat rooms should live forever. Is this a must?
* Should the server implement any security? Like making sure that the client
  asking to post for a certain PID is actually that client (so that a client
  doesn't send in) someone else's PID.
* When we send the Reason back the GUI we get an exception (the GUI can't parse
  it). So we decided to just write "Server couldn't be reached". Is that an
  okay solution?
