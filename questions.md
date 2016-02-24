<link href="/Users/hjort/Code/configs/markdown.css" rel="stylesheet"></link>

* When we try to connect to server when username is taken, should we reply with
  `already_connected` or something else?
* It says that chat rooms should live forever. Is this a must?

Answer: Use more concurrency! Chat rooms are not dependent on each other, so they should not have to wait for each other. Make every room a process and let that process send messages to all its clients.

* Should the server implement any security? Like making sure that the client
  asking to post for a certain PID is actually that client (so that a client
  doesn't send in someone else's PID).
* When we send the Reason back the GUI we get an exception (the GUI can't parse
  it). So we decided to just write "Server couldn't be reached". Is that an
  okay solution?
* How should we handle not being connected to a server when issuing commands
  like join or leave?
* When a channel sends out a message to everyone in a channel, should the server
  use the clients' genservers? This means that the server has to wait for a
  response from all clients, and this feels like a weird behaviour for a server.
* Can we spawn a new process for each client when sending a message?
* Let channel keep track of users, but also let users keep track of their channels, so we know when to disconnect. Is this reasonable?
