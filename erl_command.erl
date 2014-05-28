%%
%%Copyright (c) 2014 threestone.cn and chunbai.com
%%
%%www.threestone.cn 
%%www.chunbai.com
%%email:zhanglei0321@gmail.com

%%Permission is hereby granted, free of charge, to any person obtaining a copy
%%of this software and associated documentation files (the "Software"), to deal
%%in the Software without restriction, including without limitation the rights
%%to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%copies of the Software, and to permit persons to whom the Software is
%%furnished to do so, subject to the following conditions:

%%The above copyright notice and this permission notice shall be included in
%%all copies or substantial portions of the Software.

%%THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%THE SOFTWARE.


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
