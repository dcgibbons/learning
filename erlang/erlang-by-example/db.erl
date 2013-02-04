%
% Exercise 4 - Database Handling Using Lists
%
% Write a module db.erl that creates a database and is able to store, retrieve 
% and delete elements in it. The function destroy/1 will delete the database. 
% Considering that Erlang has garbage collection, you do not need to do 
% anything. Had the db module however stored everything on file, you would 
% delete the file. We are including the destroy function so as to make the 
% interface consistent. You may not use the lists library module, and have to 
% implement all the recursive functions yourself.
%
% Hint: Use lists and tuples your main data structures. When testing your 
% program, remember that Erlang variables are single assignment.
%
% Interface:
% db:new() ⇒ DbRef.
% db:destroy(DbRef) ⇒ ok.
% db:write(Key, Element, DbRef) ⇒ NewDbRef. db:delete(Key, DbRef) ⇒ NewDbRef.
% db:read(Key, DbRef) ⇒{ok, Element} | {error, instance}. db:match(Element, i 
% DbRef) ⇒ [Key1, ..., KeyN].
%
% Chad Gibbons
% dcgibbons@gmail.com
% August 10, 2012
%

-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
  [].

destroy(_) ->
  ok.

write(Key, Element, DbRef) ->
  [{Key,Element}|DbRef].

delete(Key, DbRef) ->
  delete(Key, DbRef, []).
delete(Key, [H|T], NewDbRef) ->
  case H of 
    {Key, _} ->
      delete(Key, T, NewDbRef);
    _ ->
      delete(Key, T, [H|NewDbRef])
  end;
delete(_, [], NewDbRef) ->
  NewDbRef.

read(Key, [H|T]) ->
  case H of
    {Key, Element} ->
      {ok, Element};
    _ ->
      read(Key, T)
  end;
read(_, []) ->
  {error, instance}.

match(Element, DbRef) ->
  match(Element, DbRef, []).
match(Element, [H|T], Matches) ->
  case H of 
    {Key, Element} ->
      match(Element, T, [Key|Matches]);
    _ ->
      match(Element, T, Matches)
  end;
match(_, [], Matches) ->
  Matches.
