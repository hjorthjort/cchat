% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   nick: the chosen nickname for the client
%   gui: the name (or Pid) of the GUI process.
%   server: the server which the client is currently connected to
-record(client_state, { nick, gui, server }).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%   users: user records of all users on the server
%   channels: channel records of all channels on the server
-record(server_state, { name, users=[], channels=[] }).

% This record defines a user on the server side.
% It contains the following fields:
%   pid: the pid (or name) of the user process
%   nick: the user's nickname used for messages
%   channels: the channels the user is connected to
-record(user, { pid, nick, channels=[] }).

% This record defines a channel on the server side.
% It contains the following fields:
%   users: a list of user records for all the users currently in the channel
-record(channel_state, { atom, name, users=[] }).
