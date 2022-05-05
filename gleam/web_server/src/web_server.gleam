import gleam/io
import gleam/bit_builder.{BitBuilder}
import gleam/http/elli
import gleam/http/request.{Request}
import gleam/http/response.{Response}

fn service(_req: Request(BitString)) -> Response(BitBuilder) {
  let body = bit_builder.from_string("Hello, my server!")
  response.new(200)
  |> response.set_body(body)
}

// This code couldn't compile because of building errors on `gleam_otp`.
// I'm totally beginner of Erlang, I couldn't debug anymore.
// The error was as below.
// build/dev/erlang/gleam_otp/gleam_otp_external.erl:17:14: can't find include lib "gleam_otp/include/gleam@otp@process_Sender.hrl"
// %   17| -include_lib("gleam_otp/include/gleam@otp@process_Sender.hrl").
// %     |              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:18:14: can't find include lib "gleam_otp/include/gleam@otp@process_Exit.hrl"
// %   18| -include_lib("gleam_otp/include/gleam@otp@process_Exit.hrl").
// %     |              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:19:14: can't find include lib "gleam_otp/include/gleam@otp@process_PortDown.hrl"
// %   19| -include_lib("gleam_otp/include/gleam@otp@process_PortDown.hrl").
// %     |              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:20:14: can't find include lib "gleam_otp/include/gleam@otp@process_ProcessDown.hrl"
// %   20| -include_lib("gleam_otp/include/gleam@otp@process_ProcessDown.hrl").
// %     |              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:21:14: can't find include lib "gleam_otp/include/gleam@otp@process_StatusInfo.hrl"
// %   21| -include_lib("gleam_otp/include/gleam@otp@process_StatusInfo.hrl").
// %     |              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:122:54: record process_down undefined
// %  122|             transform_msg(Receiving, {process, Ref}, #process_down{pid = Pid, reason = Reason});
// %     |                                                      ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:125:51: record port_down undefined
// %  125|             transform_msg(Receiving, {port, Ref}, #port_down{port = Port, reason = Reason});
// %     |                                                   ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:128:44: record exit undefined
// %  128|             transform_msg(Receiving, exit, #exit{pid = Pid, reason = Reason});
// %     |                                            ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:197:14: record sender undefined
// %  197|     Sender = #sender{pid = Pid, prepare = {some, Prepare}},
// %     |              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:210:5: record status_info undefined
// %  210|     #status_info{mode = Mode, parent = Parent, debug_state = Debug,
// %     |     ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:213:16: variable 'Mode' is unbound
// %  213|         get(), Mode, Parent, Debug,
// %     |                ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:213:22: variable 'Parent' is unbound
// %  213|         get(), Mode, Parent, Debug,
// %     |                      ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:213:30: variable 'Debug' is unbound
// %  213|         get(), Mode, Parent, Debug,
// %     |                              ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:215:29: variable 'Mode' is unbound
// %  215|          {data, [{'Status', Mode}, {'Parent', Parent}, {'State', State}]}]
// %     |                             ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:215:47: variable 'Parent' is unbound
// %  215|          {data, [{'Status', Mode}, {'Parent', Parent}, {'State', State}]}]
// %     |                                               ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:215:66: variable 'State' is unbound
// %  215|          {data, [{'Status', Mode}, {'Parent', Parent}, {'State', State}]}]
// %     |                                                                  ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:217:31: variable 'Mod' is unbound
// %  217|     {status, self(), {module, Mod}, Data}.
// %     |                               ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:121:32: Warning: variable 'Pid' is unused
// %  121|         {'DOWN', Ref, process, Pid, Reason} when is_map_key({process, Ref}, Receiving) ->
// %     |                                ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:121:37: Warning: variable 'Reason' is unused
// %  121|         {'DOWN', Ref, process, Pid, Reason} when is_map_key({process, Ref}, Receiving) ->
// %     |                                     ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:124:29: Warning: variable 'Port' is unused
// %  124|         {'DOWN', Ref, port, Port, Reason} when is_map_key({port, Ref}, Receiving) ->
// %     |                             ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:124:35: Warning: variable 'Reason' is unused
// %  124|         {'DOWN', Ref, port, Port, Reason} when is_map_key({port, Ref}, Receiving) ->
// %     |                                   ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:127:18: Warning: variable 'Pid' is unused
// %  127|         {'EXIT', Pid, Reason} when is_map_key(exit, Receiving) ->
// %     |                  ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:127:23: Warning: variable 'Reason' is unused
// %  127|         {'EXIT', Pid, Reason} when is_map_key(exit, Receiving) ->
// %     |                       ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:195:13: Warning: variable 'Pid' is unused
// %  195| system_msg({Pid, Ref}, Tag) ->
// %     |             ^

// build/dev/erlang/gleam_otp/gleam_otp_external.erl:196:5: Warning: variable 'Prepare' is unused
// %  196|     Prepare = fun(X) -> system_reply(Tag, Ref, X) end,
// %     |     ^
pub fn main() {
  elli.become(service, on_port: 3000)
}
