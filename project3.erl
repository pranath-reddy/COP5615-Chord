%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-3
%% Chord Protocol Simulation

%% Usage:
%% c(project3).
%% project3:main(numNodes, numRequests).
%% Ex: project3:main(1000, 10).
%% Restart shell after each execution

-module(project3).
-export([main/2, actor/6]).
-export([counter/1,increment/1,value/1]).
-import(string, [substr/3, to_lower/1, right/3]).

% Counter to track hops
increment(Counter) ->
  Counter ! increment.
value(Counter) ->
  Counter ! {self(),value},
  receive
    {Counter,Value} ->
      Value
  end.
counter(Val) ->
  receive
    increment ->
      counter(Val + 1);
    {From,value} ->
      From ! {self(),Val},
      counter(Val)
  end.

% Generate SHA256 hash
% To get the identifier for lookup
hash_function(ID, M) ->
  % Args:
  % ID: ID to be hashed
  % M: No of bits

  S = "actor" ++ integer_to_list(ID),
  <<X:256, _/binary>> = crypto:hash(sha256, S),
  Hash = to_lower(right(integer_to_list(X,16), 64, $0)),
  SubHash = substr(Hash, 1, M),
  SubHash.

% Spawns the required number of actors
% Ring Formation
spawner(0, _N, _R, _Req, _M, _Counter) -> done;
spawner(ID, N, R, Req, M, Counter) ->
  % Args:
  % ID: Peer/actor ID
  % N: Total number of nodes
  % R: numRequests-Number of requests per node
  % Req: Number of requests per node
  % M: bits identifier
  % Counter: Hops counter
  PID = spawn(project3, actor, [ID, R, Req, N, M, Counter]),
  register(list_to_atom("actor" ++ integer_to_list(ID)), PID),
  spawner(ID-1, N, R, Req, M, Counter).

main(N, R) ->
  % Args:
  % N: numNodes-Total number of nodes
  % R: numRequests-Number of requests per node
  io:fwrite("numNodes: ~p\n", [N]),
  io:fwrite("numRequests: ~p\n", [R]),
  % Set an M large enough to avoid collisions
  M = round(math:ceil(math:log2(N))),
  io:fwrite("M: ~p\n", [M]),
  % Initiate the hops counter
  Counter = spawn(project3, counter, [0]),
  % Form the logical ring
  spawner(N, N, R, R, M, Counter),
  server().

% Sends the query to the first node to start the simulation
server() ->
  ID = whereis(list_to_atom("actor" ++ integer_to_list(1))),
  ID ! {start}.

% Get successor to fill finger table
find_successor(NID, M) ->
  % Args:
  % NID: Input ID
  % M: bits identifier
  NR = round(math:pow(2,M)),
  if
    NID >= NR ->
      NewID = NID - NR + 1,
      PNID = whereis(list_to_atom("actor" ++ integer_to_list(NewID)));
    NID < NR ->
      NewID = round(NID rem round(math:pow(2,M))),
      PNID = whereis(list_to_atom("actor" ++ integer_to_list(NewID)))
  end,
  if
    PNID == undefined ->
      find_successor(NID+1, M);
    true ->
      NewID
  end.

% Get Finger Table of a node
finger_table(_ID, 0, _M) -> [];
finger_table(ID, N, M) ->
  % Args:
  % ID: Input ID to build finger table
  % N: Total number of nodes
  % M: bits identifier
  NID = round((ID + round(math:pow(2,N-1))) rem round(math:pow(2,M))),
  [find_successor(NID, M) | finger_table(ID, N-1, M)].

% Lookup the given key
% Get the target node from finger table
find_successor_lookup(K_rand, FT, 0, Tgt) ->
  I = lists:nth(1, FT),
  if
    (Tgt == I) and (K_rand < I) ->
      lists:nth(length(FT), FT);
    true ->
      Tgt
  end;
find_successor_lookup(K_rand, FT, L, Tgt) ->
  % Args:
  % K_rand: Query key
  % FT: Finger Table
  % L: Number of entries to iterate/ Size of finger table
  % Tgt: target
  I1 = lists:nth((length(FT) - L + 1), FT),
  if
    length(FT) == L ->
      if
        (Tgt < I1) ->
          find_successor_lookup(K_rand, FT, L-1, I1);
        true ->
          find_successor_lookup(K_rand, FT, L-1, Tgt)
      end;
    true ->
      I2 = lists:nth((length(FT) - L), FT),
      if
        (K_rand < I1) and (K_rand >= I2) ->
          find_successor_lookup(K_rand, FT, L-1, I2);
        (K_rand > I1) and (K_rand > I2) and (I2 < I1) and (Tgt < I2)->
          find_successor_lookup(K_rand, FT, L-1, I1);
        (K_rand < I1) and (K_rand < I2) and (I2 > I1)->
          find_successor_lookup(K_rand, FT, L-1, I2);
        true ->
          find_successor_lookup(K_rand, FT, L-1, Tgt)
      end
  end.

% Participant in the ring
actor(ID, 0, Req, N, M, Counter) ->
  % Print stats upon completion
  if
    ID == N ->
      Count = value(Counter),
      io:fwrite("Total Number of Hops: ~p\n", [Count]),
      AvgCount = Count / (N*Req),
      io:fwrite("Average Number of Hops: ~p\n", [AvgCount]),
      io:fwrite("Done!\n"),
      erlang:halt();
    ID < N ->
      NPID = whereis(list_to_atom("actor" ++ integer_to_list(ID+1))),
      NPID ! {start}
  end,
  % Node connection (Hop)
  receive
    {hop, K_rand, OID} ->
      M = round(math:ceil(math:log2(N))),
      FT = lists:reverse(finger_table(ID, M, M)),
      Target_Node = find_successor_lookup(K_rand, FT, length(FT), ID),
      Check = lists:member(K_rand, FT),
      Target_Hash = hash_function(Target_Node, M),
      Identifier = hash_function(ID, M),
      if
        (Target_Hash == Identifier) or (Check == true)->
          PID = whereis(list_to_atom("actor" ++ integer_to_list(OID))),
          PID ! {done, OID},
          actor(ID, 0, Req, N, M, Counter);
        true ->
          Target_PID = whereis(list_to_atom("actor" ++ integer_to_list(Target_Node))),
          Target_PID ! {hop, K_rand, OID},
          increment(Counter),
          actor(ID, 0, Req, N, M, Counter)
      end
    end;
actor(ID, R, Req, N, M, Counter) ->
  % Args:
  % ID: Peer/actor ID
  % N: Total number of nodes
  % R: numRequests-Number of requests per node
  % Req: Number of requests per node
  % M: bits identifier
  % Counter: Hops counter
  receive
    % Initiate a lookup query
    {start} ->
      M = round(math:ceil(math:log2(N))),
      FT = lists:reverse(finger_table(ID, M, M)),
      % Generate Random Query
      K_rand = rand:uniform(round(math:pow(2,M))),
      Target_Node = find_successor_lookup(K_rand, FT, length(FT), ID),
      OID = ID,
      Check = lists:member(K_rand, FT),
      Target_Hash = hash_function(Target_Node, M),
      Identifier = hash_function(ID, M),
      if
        (Target_Hash == Identifier) or (Check == true)->
          PID = whereis(list_to_atom("actor" ++ integer_to_list(OID))),
          PID ! {done, OID},
          actor(ID, R, Req, N, M, Counter);
        true ->
          Target_PID = whereis(list_to_atom("actor" ++ integer_to_list(Target_Node))),
          Target_PID ! {hop, K_rand, OID},
          increment(Counter),
          actor(ID, R, Req, N, M, Counter)
      end;

    % Node connection (Hop)
    {hop, K_rand, OID} ->
      M = round(math:ceil(math:log2(N))),
      FT = lists:reverse(finger_table(ID, M, M)),
      Target_Node = find_successor_lookup(K_rand, FT, length(FT), ID),
      Check = lists:member(K_rand, FT),
      Target_Hash = hash_function(Target_Node, M),
      Identifier = hash_function(ID, M),
      if
        (Target_Hash == Identifier) or (Check == true)->
          PID = whereis(list_to_atom("actor" ++ integer_to_list(OID))),
          PID ! {done, OID},
          actor(ID, R, Req, N, M, Counter);
        true ->
          Target_PID = whereis(list_to_atom("actor" ++ integer_to_list(Target_Node))),
          Target_PID ! {hop, K_rand, OID},
          increment(Counter),
          actor(ID, R, Req, N, M, Counter)
      end;

    % Send message to the peer that initiated the query
    {done, OID} ->
      increment(Counter),
      PID = whereis(list_to_atom("actor" ++ integer_to_list(OID))),
      PID ! {start},
      actor(OID, R-1, Req, N, M, Counter)
  end.
