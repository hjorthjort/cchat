% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   nick: the chosen nickname for the client
%   gui: the name (or Pid) of the GUI process.
%   server: the server which the client is currently connected to
%   channels: a list of channel names for all the channels the user is in
-record(client_state, { nick, gui, server, channels=[] }).

% This record defines a channel on the client side.
% It contains the following fields:
%   name: the name of the channel (starts with '#')
%   pid: the pid to the channel process
-record(channel, { name, pid }).

% This record defines the structure of the server process.
% Add whatever other fields you need.
%   name: the name of the server
%   users: user records of all users on the server
-record(server_state, { name, users=[] }).

% This record defines a user on the server side.
% It contains the following fields:
%   pid: the pid (or name) of the user process
%   nick: the user's nickname used for messages
-record(user, { pid, nick }).

% This record defines a channel on the server side.
% It contains the following fields:
%   name: the name of the channel
%   users: a list of user records for all the users currently in the channel
-record(channel_state, { name, users=[] }).
