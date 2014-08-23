
%%
%% simple erlang terminal emulator  
%% 

-module(erl_command).

-export([test/0,run/2,inputloop/2,main/1]).


%% for test ==================================
test()->
    %% get input
    {_,[Input]} = io:fread("cmd:","~s"),
    Child = spawn(?MODULE,run,[Input,self()]),
    InPutChild = spawn(?MODULE,inputloop,[Child,self()]),
    mainloop(InPutChild),
    test().

%% ==========================

-spec main(Cmd::string()) -> ok.

main(Cmd)->
   Child = spawn(?MODULE,run,[Cmd,self()]),
   InPutChild = spawn(?MODULE,inputloop,[Child,self()]),
   mainloop(InPutChild). 

-spec run(Cmd::string(),Main::pid()) -> ok.

run(Cmd,Main)->
    Port = open_port({spawn,Cmd},[exit_status,in,out,{line,1024}]),
    run_loop(Port,Main).

-spec run_loop(P::pid(),Main::pid()) -> ok.
run_loop(P,Main)->
    receive
       {P, {data, {eol,Data}}} ->
            Exit = false,
            io:format("~s ~n",[Data]);
       {P, {data, Data}} ->
            Exit = false,
            io:format("~p ~n",[Data]);
       {P, {exit_status, Code}}->
            Exit = true, 
            io:format("exit ~p ~n",[Code]);
       {input,Data}->
            Exit = false,
            port_command(P,Data++"\n");
        {exit}->
            port_close(P),
            Exit = true;
        Other->
            Exit = false,
            io:format("rec ~p ~n",[Other])
    end,
    if    
        Exit->   
           Main ! {exit},
           ok;
        true->
          run_loop(P,Main)  
    end.

-spec mainloop(InputPid::pid()) -> ok.

mainloop(InputPid)->
    receive
       {exit}->
            Exit = true,
            erlang:exit(InputPid,kill);
        _Other->
            Exit = false
    end,
    if    
        Exit->
           ok;
        true->
           mainloop(InputPid) 
    end.

-spec inputloop(ProcPid::pid(),Main::pid()) -> true.

inputloop(ProcPid,Main)->
    {_,[Input]} = io:fread(">>","~s"),
    if 
        Input =:= "byebye"->
            case is_process_alive(ProcPid) of
                false->
                    Main ! {exit};
                _->
                    ProcPid ! {exit}
            end,
            exit(normal);
        true->
             ProcPid ! {input,Input},
             inputloop(ProcPid,Main)
    end.
