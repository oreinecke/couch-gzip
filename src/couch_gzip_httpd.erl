-module(couch_gzip_httpd).

-export([handle_req/1]).

-include_lib("couch/include/couch_db.hrl").

handle_req(Req) ->
  try
    gzip(Req)
  catch
    _:_-> identity(Req)
  end.

gzip(#httpd{mochi_req=MochiReq, method=Method}=Req) ->

    ["gzip"] = MochiReq:accepted_encodings(["gzip"]),

    {ok, Result} = httpc:request(
      list_to_atom(string:to_lower(atom_to_list(Method))),
      list_to_tuple([
        "http://"
          ++ couch_config:get("httpd", "bind_address") ++ ":"
          ++ couch_config:get("httpd", "port") ++ path(Req),
        [ {atom_to_list(Field), Value} ||
          {Field, Value} <- mochiweb_headers:to_list(MochiReq:get(headers)), is_atom(Field) ]
      ] ++ case Method of
        'POST' -> [ "", couch_httpd:body(Req) ];
        'GET' -> [] end
      ), [], []
    ),

    { {_,200,_}, Headers, Body} = Result,

    undefined = proplists:get_value("content-encoding", Headers),

    true = compressible_type(proplists:get_value("content-type", Headers)),

    couch_httpd:send_response(
      Req, 200,
      Headers ++ [{"Content-encoding", "gzip"}],
      zlib:gzip(Body)
    ).

identity(#httpd{mochi_req=MochiReq}=Req) ->

  MochiReq1 = mochiweb_request:new(
    MochiReq:get(socket),
    MochiReq:get(method),
    path(Req),
    MochiReq:get(version),
    MochiReq:get(headers)
  ),

  MochiReq1:cleanup(),

  #httpd{
    default_fun = DefaultFun,
    url_handlers = UrlHandlers,
    db_url_handlers = DbUrlHandlers,
    design_url_handlers = DesignUrlHandlers
  } = Req,

  couch_httpd:handle_request_int(
    MochiReq1, DefaultFun, UrlHandlers,
    DbUrlHandlers, DesignUrlHandlers
  ).

path(#httpd{ mochi_req = MochiReq, path_parts = [ Handler | _ ] }) ->
  MochiReq:get(raw_path) -- ("/" ++ binary_to_list(Handler)).

compressible_type(MimeType) ->
  TypeExpList = re:split(
    couch_config:get("attachments", "compressible_types", ""),
    "\\s*,\\s*", [{return, list}]
  ),
  lists:any( fun(TypeExp) ->
    Regexp = ["^\\s*", re:replace(TypeExp, "\\*", ".*"),
    "(?:\\s*;.*?)?\\s*", $$],
    re:run(MimeType, Regexp, [caseless]) =/= nomatch
  end, [T || T <- TypeExpList, T /= []] ).
