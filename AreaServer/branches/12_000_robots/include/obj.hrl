%% @type dict(). A dictionary from the dict module. 
%% @type obj(). #obj{id = string, type = atom(), properties = dict()}
 
-record(obj, {id, type, parents=[], properties}).

